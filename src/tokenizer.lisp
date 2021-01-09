(defpackage #:cl-prattler/tokenizer
  (:use #:common-lisp)
  (:export #:tokenizer
           #:tokenize
           #:token-kind
           #:token-spelling))

(in-package #:cl-prattler/tokenizer)

(defun token-type-p (thing)
  (or
   (string= thing "number")
   (string= thing "eof")
   (string= thing "+")
   (string= thing "-")
   (string= thing "*")
   (string= thing "/")
   (string= thing "^")
   (string= thing "(")
   (string= thing ")")))

(deftype token-type (&optional type)
  (declare (ignore type))
  `(satisfies token-type-p))

(defclass token ()
  ((kind
    :initarg :token-kind
    :accessor token-kind)
   (spelling
    :initarg :token-spelling
    :accessor token-spelling)))

(defmethod initialize-instance :after ((tok token) &rest rest)
  (declare (ignore rest))
  (with-accessors ((kind token-kind)) tok
    (assert (typep kind 'token-type))))

(defmethod print-object ((obj token) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-standard-io-syntax
      (with-accessors ((kind token-kind) (spelling token-spelling)) obj
        (format stream "<:~a,~a>" kind spelling)))))

(defparameter *current-spelling* nil)

(defclass tokenizer ()
  ((input
    :initarg :input
    :accessor input)
   (current-char
    :initarg :current-char
    :accessor current-char)))

(defmethod initialize-instance :after ((tokenizer tokenizer) &rest rest)
  (declare (ignore rest))
  (with-accessors ((input input)
                   (current-char current-char))
      tokenizer
    (setf current-char (char input 0))))

(defgeneric skip-it (tokenizer)
  (:documentation "unconditionally sklp the current character"))

(defgeneric eat (tokenizer c)
  (:documentation "consume the current character if it matches c"))

(defgeneric eat-it (tokenizer)
  (:documentation "unconditionally consume the current character"))

(defgeneric read-number (tokenizer)
  (:documentation "parse the input into an integer or real number"))

(defgeneric tokenize (tokenizer)
  (:documentation "retrieve the next token from the stream"))

(defmethod skip-it ((tokenizer tokenizer))
  (with-accessors ((input input)
                   (current-char current-char))
      tokenizer
    (if (> (length input) 1)
        (progn
          (setf input (subseq input 1))
          (setf current-char (char input 0)))
        (setf current-char #\Nul))))

(defmethod eat ((tokenizer tokenizer) c)
  (with-accessors ((input input)
                   (current-char current-char))
      tokenizer
    (if (char= c current-char)
        (progn
          (vector-push-extend c *current-spelling*)
          (if (> (length input) 1)
              (progn
                (setf input (subseq input 1))
                (setf current-char (char input 0)))
              (setf current-char #\Nul))))))

(defmethod eat-it ((tokenizer tokenizer))
  (with-accessors ((input input)
                   (current-char current-char))
      tokenizer
    (vector-push-extend current-char *current-spelling*)
    (if (> (length input) 1)
        (progn
          (setf input (subseq input 1))
          (setf current-char (char input 0)))
        (setf current-char #\Nul))))

(defun whitespacep (c)
  (or
   (char= c #\Space)
   (char= c #\Newline)
   (char= c #\Tab)))

(defmethod skip-whitespace ((tokenizer tokenizer))
  (with-accessors ((current-char current-char)) tokenizer
    (loop
      while (whitespacep current-char)
      do (skip-it tokenizer))))

(defun parse-number (spelling)
  (with-input-from-string (stream spelling)
    (read stream nil nil)))

(defmethod read-number ((tokenizer tokenizer))
  (with-accessors ((current-char current-char)) tokenizer
    (loop
      while (or (digit-char-p current-char)
                (char= current-char #\.))
      do (eat-it tokenizer))
    (make-instance 'token :token-kind "number" :token-spelling (parse-number *current-spelling*))))

(defmethod tokenize ((tokenizer tokenizer))
  (setf *current-spelling* (make-array 0 :element-type 'character
                                         :fill-pointer 0
                                         :adjustable t))
  (let ((current-token nil))
    (with-accessors ((current-char current-char)) tokenizer
      (skip-whitespace tokenizer)
      (cond
        ((char= current-char #\Nul)
         (setf current-token (make-instance 'token :token-kind "eof" :token-spelling "eof")))
        ((char= current-char #\()
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\( :token-spelling #\())))
        ((char= current-char #\))
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\) :token-spelling #\)))))
        ((digit-char-p current-char)
         (setf current-token (read-number tokenizer)))
        ((char= current-char #\+)
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\+ :token-spelling #\+))))
        ((char= current-char #\-)
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\- :token-spelling #\-))))
        ((char= current-char #\-)
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\- :token-spelling #\-))))
        ((char= current-char #\*)
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\* :token-spelling #\*))))
        ((char= current-char #\/)
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\/ :token-spelling #\/))))
        ((char= current-char #\^)
         (progn
           (eat-it tokenizer)
           (setf current-token (make-instance 'token :token-kind #\^ :token-spelling #\^))))
        (t (error "unexpected character ~a while tokenizing input" current-char)))
      current-token)))
