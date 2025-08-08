;;;; Vibe coding in Common Lisp

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :cl-ppcre)
  (load "openai.lisp"))

(defpackage :vibe
  (:use :cl :openai)
  (:export
   :vibe))

(in-package :vibe)


(defun make-prompt (name args text)
  (format nil "Write a Common Lisp function called ~A with argument list ~A to return ~A. Just return the defun expression with no other text."
          name
          args
          text))

(defun strip-markdown-code-fences (text)
  "If TEXT contains a markdown code block, remove the fences and return the contents.
Supports both ``` and ~~~ fences, with or without language specifiers."
  (cl-ppcre:regex-replace
   ;; Pattern: capture content between ```...``` or ~~~...~~~
   ;; (?s) makes '.' match newlines
   "(?s)^[ \t]*(```|~~~)[^\n]*\n(.*?)\n[ \t]*\\1[ \t]*$"
   text
   "\\2"))


(defmacro vibe (name args text)
  `(let ((result (openai:completions
                   (make-prompt (format nil "~a" ',name)
                                (format nil "~a" ',args)
                                ,text))))
     (format t "~&[defvibe debug] openai:completions returned:~%~a~%" result)
     (eval (read-from-string (strip-markdown-code-fences result)))))
