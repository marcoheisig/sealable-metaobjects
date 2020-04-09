(in-package #:sealable-metaobjects)

(defclass sealable-generic-function (sealable-metaobject-mixin generic-function)
  ((%sealed-domains
    :initform '()
    :type list
    :accessor sealed-domains))
  (:default-initargs
   :method-class (find-class 'potentially-sealable-method))
  (:metaclass funcallable-standard-class))

;;; Check that the supplied domain is sane.
(defmethod seal-domain
    ((sgf sealable-generic-function)
     (domain t))
  (seal-domain sgf (ensure-domain domain)))

(defmethod seal-domain :around
    ((sgf sealable-generic-function)
     (domain domain))
  ;; Ensure that we don't seal any domain more than once.
  (unless (find domain (sealed-domains sgf) :test #'domain-equal)
    (call-next-method sgf domain)))

;;; Ensure that the generic function is sealed, and that the newly sealed
;;; domain is disjoint from other domains.
(defmethod seal-domain :before
    ((sgf sealable-generic-function)
     (domain domain))
  ;; Ensure that the length of the domain matches the number of mandatory
  ;; arguments of the generic function.
  (unless (= (domain-arity domain)
             (length (generic-function-argument-precedence-order sgf)))
    (error "~@<Cannot seal the domain ~S with arity ~R ~
               of the generic function ~S with arity ~R.~@:>"
           (mapcar #'specializer-type (domain-specializers domain))
           (domain-arity domain)
           (generic-function-name sgf)
           (length (generic-function-argument-precedence-order sgf))))
  ;; Attempt to seal the supplied generic function.
  (seal-generic-function sgf)
  ;; Ensure that the domain does not intersect any existing sealed domains.
  (dolist (existing-domain (sealed-domains sgf))
    (when (domain-intersectionp domain existing-domain)
      (error "~@<Cannot seal the domain ~S of the generic function ~S, ~
               because it intersects with the existing domain ~S.~@:>"
             (mapcar #'specializer-type domain)
             sgf
             (mapcar #'specializer-type existing-domain)))))

;;; Add a new sealed domain.
(defmethod seal-domain
    ((sgf sealable-generic-function)
     (domain domain))
  (dolist (method (generic-function-methods sgf))
    (when (domain-intersectionp (method-domain method) domain)
      (unless (domain-subsetp (method-domain method) domain)
        (error "~@<The method ~S with specializers ~S is only partially ~
                   within the sealed domain ~S.~:@>"
               method
               (mapcar #'specializer-type (method-specializers method))
               (mapcar #'specializer-type (domain-specializers domain))))
      (seal-method method)))
  (push domain (sealed-domains sgf)))

;;; Skip the call to add-method if the list of specializers is equal to
;;; that of an existing, sealed method.
(defmethod add-method :around
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (method (generic-function-methods sgf))
    (when (and (method-sealed-p method)
               (equal (method-specializers psm)
                      (method-specializers method)))
      (return-from add-method psm)))
  (call-next-method))

;;; Ensure that the method to be added is disjoint from all sealed domains.
(defmethod add-method :before
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (domain (sealed-domains sgf))
    (when (domain-intersectionp domain (method-specializers psm))
      (error "~@<Cannot add the method ~S with specializers ~S to ~
                 the sealed generic function ~S, because it intersects ~
                 with the existing sealed domain ~S.~:@>"
             psm (method-specializers psm) sgf (mapcar #'specializer-type domain)))))

;;; Ensure that the method to be removed is disjoint from all sealed domains.
(defmethod remove-method :before
    ((sgf sealable-generic-function)
     (psm potentially-sealable-method))
  (dolist (domain (sealed-domains sgf))
    (when (domain-intersectionp domain (method-domain psm))
      (error "~@<Cannot remove the method ~S with specializers ~S from ~
                 the sealed generic function ~S, because it intersects ~
                 with the existing sealed domain ~S.~:@>"
             psm (method-specializers psm)
             sgf (mapcar #'specializer-type (domain-specializers domain))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Derived Classes

(defclass sealable-standard-generic-function
    (standard-generic-function sealable-generic-function)
  ()
  (:default-initargs
   :method-class (find-class 'potentially-sealable-standard-method))
  (:metaclass funcallable-standard-class))
