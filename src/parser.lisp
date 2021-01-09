(defpackage #:cl-prattler/parser
  (:use #:common-lisp
        #:cl-prattler/tokenizer
        #:cl-prattler/ast)
  (:export #:parser
           #:parse))

(in-package #:cl-prattler/parser)

(defclass token ()
  ((lbp
    :initarg :lbp
    :initform 0
    :accessor lbp)))

(defmethod print-object ((token token) stream)
  (print-unreadable-object (token stream :type t :identity t)
    (with-standard-io-syntax
      (with-accessors ((lbp lbp)) token
        (format stream "~d" lbp)))))

(defgeneric nud (token parser)
  (:documentation "the null denotation function"))

(defgeneric led (token left parser)
  (:documentation "the left denotation function"))

(defclass literal (token)
  ((value
    :initarg :value
    :initform nil
    :accessor value)))

(defmethod nud ((literal literal) parser)
  (declare (ignore parser))
  (make-instance 'cl-prattler/ast:number-expression
                 :value (value literal)))

(defclass token-left-paren (token) ())

(defmethod nud ((token-left-paren token-left-paren) parser)
  (with-accessors ((lbp lbp)) token-left-paren
    (let ((expr (expression parser lbp)))
      (match parser 'token-right-paren)
      expr)))

(defclass token-right-paren (token) ())

(defclass token-add (token) ())

(defmethod nud ((token-add token-add) parser)
  (with-accessors ((lbp lbp)) token-add
    (let ((expr (expression parser (* lbp 10))))
      (make-instance 'cl-prattler/ast:unary-expression
                     :operator "pos"
                     :expr expr))))

(defmethod led ((token-add token-add) left parser)
  (with-accessors ((lbp lbp)) token-add
    (let ((right (expression parser lbp)))
      (make-instance 'cl-prattler/ast:binary-expression
                     :operator "add"
                     :left left
                     :right right))))

(defclass token-sub (token) ())

(defmethod nud ((token-sub token-sub) parser)
  (with-accessors ((lbp lbp)) token-sub
    (let ((expr (expression parser (* lbp 10))))
      (make-instance 'cl-prattler/ast:unary-expression
                     :operator "neg"
                     :expr expr))))

(defmethod led ((token-sub token-sub) left parser)
  (with-accessors ((lbp lbp)) token-sub
    (let ((right (expression parser lbp)))
      (make-instance 'cl-prattler/ast:binary-expression
                     :operator "sub"
                     :left left
                     :right right))))

(defclass token-mul (token) ())

(defmethod led ((token-mul token-mul) left parser)
  (with-accessors ((lbp lbp)) token-mul
    (let ((right (expression parser lbp)))
      (make-instance 'cl-prattler/ast:binary-expression
                     :operator "mul"
                     :left left
                     :right right))))

(defclass token-div (token) ())

(defmethod led ((token-div token-div) left parser)
  (with-accessors ((lbp lbp)) token-div
    (let ((right (expression parser lbp)))
      (make-instance 'cl-prattler/ast:binary-expression
                     :operator "div"
                     :left left
                     :right right))))

(defclass token-pow (token) ())

(defmethod led ((token-pow token-pow) left parser)
  (with-accessors ((lbp lbp)) token-pow
    (let ((right (expression parser (1- lbp))))
      (make-instance 'cl-prattler/ast:binary-expression
                     :operator "pow"
                     :left left
                     :right right))))

(defclass token-end (token) ())

(defclass parser ()
  ((tokenizer
    :initarg :tokenizer
    :accessor tokenizer)))

(defgeneric match (parser token-type)
  (:documentation "advance the token stream if the current token kind matches the expected kind"))

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
      (:slash (make-instance 'token-div :lbp 20))
      (:caret (make-instance 'token-pow :lbp 30))
      (:eof (make-instance 'token-end :lbp 0))
      (t (error "unknown token kind encountered: ~a" kind)))))

(defmethod match ((parser parser) token-type)
  (if (not (typep *token* token-type))
      (error "expected to match token of kind ~a, got token of kind ~a~%" token-type (type-of *token*))
      (setf *token* (preprocess (next-token parser)))))

(defmethod next-token ((parser parser))
  (with-accessors ((tokenizer tokenizer)) parser 
    (cl-prattler/tokenizer:tokenize tokenizer)))

(defparameter *token* nil)

(defmethod expression ((parser parser) &optional (rbp 0))
  (let ((left nil)
        (tok nil))
    (setf tok *token*)
    (setf *token* (preprocess (next-token parser)))
    (setf left (nud tok parser))
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
