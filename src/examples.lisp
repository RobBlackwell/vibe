;;;; Examples of using vibe

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "vibe.lisp"))

(defpackage :vibe-user
  (:use :cl :vibe))

(in-package :vibe-user)

;;;

(vibe double (x) "Multiply x by two")

(double 10)

;;;

(vibe factorial (x) "The factorial of x")

(factorial 5)

;;;

(vibe foo (x y) "Multiply x by y")

(foo 10 5)

;;;

(vibe my-median (x) "The median value in the list x")

(my-median '(1 2 3 4 5))

;;;

(vibe my-reverse (x) "Reverse the list x")

(my-reverse '(5 4 3 2 1))

;;;

(vibe circle-area (radius) "Compute the area of a circle given its radius")

(circle-area 5)

;;;

(vibe is-prime (x) "Returns true if x is a prime number")

(is-prime 23)

(is-prime 100)
