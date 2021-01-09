(defpackage #:cl-prattler/parser
  (:use #:common-lisp
        #:cl-prattler/tokenizer)
  (:export #:parser
           #:parse))

(in-package #:cl-prattler/parser)

(defclass token ()
  ((lbp
    :initarg :lbp
    :initform 0
    :accessor lbp)))

(defgeneric nud (token parser)
  (:documentation "the null denotation function"))

(defgeneric led (token left parser)
  (:documentation "the left denotation function"))

(defclass literal (token)
  ((value
    :initarg :value
    :initform 0
    :accessor value)))

(defmethod nud ((literal literal) parser)
  (declare (ignore parser))
  (value literal))

(defclass token-left-paren (token) ())

(defclass token-right-paren (token) ())

(defclass token-add (token) ())

(defmethod nud ((token-add token-add) parser)
  (with-accessors ((lbp lbp)) token-add
    (expression parser (* lbp 10))))

(defmethod led ((token-add token-add) left parser)
  (with-accessors ((lbp lbp)) token-add
    (+ left (expression parser lbp))))

(defmethod print-object ((token-add token-add) stream)
  (print-unreadable-object (token-add stream :type t :identity t)
    (with-standard-io-syntax
      (with-accessors ((lbp lbp)) token-add
        (format stream "~a" lbp)))))

(defclass token-sub (token) ())

(defclass token-sub (token) ())

(defclass token-mul (token) ())

(defclass token-mul (token) ())

(defclass token-div (token) ())

(defclass token-pow (token) ())

(defclass token-end (token) ())

(defclass parser ()
  ((tokenizer
    :initarg :tokenizer
    :accessor tokenizer)))

(defgeneric next-token (parser)
  (:documentation "request the tokenizer for the next token"))

(defgeneric expression (parser &optional rbp)
  (:documentation "the Pratt parser"))

(defgeneric parse (parser)
  (:documentation "parse the given expression"))

(defun preprocess (token)
  (let ((kind (cl-prattler/tokenizer:token-kind token))
        (spelling (cl-prattler/tokenizer:token-spelling token)))
    (case kind
      (:number (make-instance 'literal :value spelling))
      (:left-paren (make-instance 'token-left-paren :lbp 0))
      (:right-paren (make-instance 'token-right-paren :lbp 0))
      (:plus (make-instance 'token-add :lbp 10))
      (:minus (make-instance 'token-sub :lbp 10))
      (:asterisk (make-instance 'token-mul :lbp 20))
      (:slash (make-instance 'token-div) :lbp 20)
      (:caret (make-instance 'token-pow :lbp 30))
      (:eof (make-instance 'token-end :lbp 0))
      (t (error "unknown token kind encountered: ~a" kind)))))

(defmethod next-token ((parser parser))
  (with-accessors ((tokenizer tokenizer)) parser 
    (cl-prattler/tokenizer:tokenize tokenizer)))

(defparameter *token* nil)

(defmethod expression ((parser parser) &optional (rbp 0))
  (let* ((tok *token*)
         (left (nud tok parser)))
    (setf *token* (preprocess (next-token parser)))
    (loop
      while (< rbp (lbp *token*))
      do (progn
           (setf tok *token*)
           (setf *token* (preprocess (next-token parser)))
           (setf left (led tok left parser))))
    left))

(defmethod parse ((parser parser))
  (setf *token* (preprocess (next-token parser)))
  (expression parser))
