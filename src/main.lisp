(defpackage #:cl-prattler/main
  (:use #:common-lisp
        #:cl-prattler/tokenizer
        #:cl-prattler/parser)
  (:export #:main))

(in-package #:cl-prattler/main)

(defun main ()
  (let* ((input (read-line *query-io* nil nil))
         (tokenizer (make-instance 'cl-prattler/tokenizer:tokenizer :input input))
         (parser (make-instance 'cl-prattler/parser:parser :tokenizer tokenizer)))
    (cl-prattler/parser:parse parser)))
