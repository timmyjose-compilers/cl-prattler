(defpackage #:cl-prattler/ast
  (:use #:common-lisp)
  (:export #:ast
           #:number-expression
           #:unary-expression
           #:binary-expression
           #:value
           #:operator
           #:expr
           #:left
           #:right))

(in-package #:cl-prattler/ast)

(defclass ast () ())

(defclass expression (ast) ())

(defclass number-expression (expression)
  ((value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod print-object ((expr number-expression) stream)
  (print-unreadable-object (expr stream)
    (with-standard-io-syntax
      (with-accessors ((value value)) expr
        (format stream "(literal ~a)" value)))))

(defclass unary-expression (expression)
  ((operator
    :initarg :operator
    :accessor operator)
   (expr
    :initarg :expr
    :accessor expr)))

(defmethod print-object ((expr unary-expression) stream)
  (print-unreadable-object (expr stream)
    (with-standard-io-syntax
      (with-accessors ((op operator)
                       (expr expr))
          expr
        (format stream "(~a ~a)" op expr)))))

(defclass binary-expression (expression)
  ((operator
    :initarg :operator
    :accessor operator)
   (left
    :initarg :left
    :accessor left)
   (right
    :initarg :right
    :accessor right)))

(defmethod print-object ((expr binary-expression) stream)
  (print-unreadable-object (expr stream)
    (with-standard-io-syntax
      (with-accessors ((op operator)
                       (left left)
                       (right right))
          expr
        (format stream "(~a ~a ~a)" op left right)))))

