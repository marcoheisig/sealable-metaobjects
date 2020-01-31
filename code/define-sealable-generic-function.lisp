(in-package #:sealable-metaobjects)

(defmacro define-sealable-generic-function
    (function-name lambda-list &body options-and-methods)
  (let ((options (remove-if (starts-with :method) options-and-methods))
        (methods (remove-if-not (starts-with :method) options-and-methods))
        (required-args
          (loop for elt in lambda-list
                until (member elt lambda-list-keywords)
                collect elt))
        specializer-profile)
    (let ((match (find-if (starts-with :argument-precedence-order) options)))
      (cond ((not match)
             (push `(:argument-precedence-order ,@required-args) options)
             (setf specializer-profile
                   (make-list (length required-args) :initial-element 't)))
            (t
             (setf options (remove match options))
             (push `(:argument-precedence-order
                     ,@(rest match)
                     ,@(set-difference required-args (rest match)))
                   options)
             (setf specializer-profile
                   (mapcar
                    (lambda (arg)
                      (and (member arg (rest match)) t))
                    required-args)))))
    (unless (member :generic-function-class options :key #'first)
      (push `(:generic-function-class sealable-standard-generic-function) options))
    `(progn
       (defgeneric ,function-name ,lambda-list ,@options)
       (setf (generic-function-specializer-profile #',function-name)
             ',specializer-profile)
       ,@(loop for (nil . defmethod-body) in methods
               collect `(defmethod ,function-name ,@defmethod-body)))))
