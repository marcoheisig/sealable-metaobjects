(in-package #:sealable-metaobjects)

(defclass domain ()
  ((%specializers
    :initform (required-argument :specializers)
    :initarg :specializers
    :reader domain-specializers)
   (%arity
    :initform (required-argument :arity)
    :initarg :arity
    :reader domain-arity)))

(defmethod print-object ((domain domain) stream)
  (print-unreadable-object (domain stream :type t)
    (format stream "~{~S~^ ~}"
            (mapcar #'specializer-type (domain-specializers domain)))))

(defun make-domain (specializers &aux (arity (list-length specializers)))
  (dolist (specializer specializers)
    (check-type specializer specializer))
  (make-instance 'domain
    :specializers specializers
    :arity arity))

(defmethod ensure-domain ((domain domain))
  domain)

(defmethod ensure-domain ((sequence sequence))
  (make-domain
   (map 'list #'ensure-specializer sequence)))

(defmethod method-domain ((method method))
  (make-domain (method-specializers method)))

(defmethod domain-equal
    ((domain-1 domain)
     (domain-2 domain))
  (and (= (domain-arity domain-1)
          (domain-arity domain-2))
       (every #'eq
              (domain-specializers domain-1)
              (domain-specializers domain-2))))

(defmethod domain-intersectionp
    ((domain-1 domain)
     (domain-2 domain))
  (assert (= (domain-arity domain-1)
             (domain-arity domain-2)))
  (every #'specializer-intersectionp
         (domain-specializers domain-1)
         (domain-specializers domain-2)))

(defmethod domain-subsetp
    ((domain-1 domain)
     (domain-2 domain))
  (assert (= (domain-arity domain-1)
             (domain-arity domain-2)))
  (every #'specializer-subsetp
         (domain-specializers domain-1)
         (domain-specializers domain-2)))
