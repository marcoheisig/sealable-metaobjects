(in-package #:sealable-metaobjects)

(defun required-argument (name)
  (error "Required argument: ~S" name))

(defun starts-with (item)
  (lambda (sequence)
    (typecase sequence
      (list (eql (first sequence) item))
      (sequence (eql (elt sequence 0) item))
      (otherwise nil))))

(defun type-specifier-and (&rest type-specifiers)
  (let ((relevant (remove t type-specifiers)))
    (cond ((null relevant) t)
          ((null (cdr relevant)) (first relevant))
          (t `(and ,@relevant)))))

(defun type-specifier-or (&rest type-specifiers)
  (let ((relevant (remove nil type-specifiers)))
    (cond ((null relevant) nil)
          ((null (cdr relevant)) (first relevant))
          (t `(or ,@relevant)))))

(defun type-specifier-not (type-specifier)
  (cond ((eql type-specifier t) nil)
        ((eql type-specifier nil) t)
        (t `(not ,type-specifier))))

(defparameter *standard-metaobjects*
  (list (find-class 'standard-object)
        (find-class 'standard-class)
        (find-class 'standard-generic-function)
        (find-class 'standard-method)
        (find-class 'built-in-class)))
