(defsystem #:cl-prattler
  :class :package-inferred-system
  :description "a simple parser for arithmetic expressions using Pratt parsing"
  :version "0.0.1"
  :author "Timmy Jose<zoltan.jose@gmail.com>"
  :pathname "src"
  :depends-on ("cl-prattler/tokenizer"
               "cl-prattler/parser"
               "cl-prattler/main"))
