(defpackage #:cl-prattler/parser
  (:use #:common-lisp
        #:cl-prattler/tokenizer)
  (:export #:parser
           #:parse))

(in-package #:cl-prattler/parser)

(defclass token () ())

(defgeneric nud (token)
  (:documentation "the null denotation function"))

(defgeneric led (token)
  (:documentation "the left denotation function"))

(defclass operator (token)
  ((lbp
    :initarg :lbp
    :initform 0
    :accessor lbp)))

(defclass literal (operator) ())

(defclass operator-add (operator) ())

(defclass operator-sub (operator) ())

(defclass operator-sub (operator) () ())

(defclass operator-mul (operator) ())

(defclass operator-mul (operator) ())

(defclass operator-div (operator) ())

(defclass parser ()
  ((tokenizer
    :initarg :tokenizer
    :accessor tokenizer)))

(defgeneric expression (parser)
  (:documentation "the Pratt parser"))

(defgeneric parse (parser)
  (:documentation "parse the given expression"))

(defmethod expression ((parser parser) &optional (rbp 0))
  ())

(defmethod parse ((parser parser))
  (print "TBD"))
