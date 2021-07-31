#lang info

(define version "0.2")
(define collection "sturgis")
(define description "Implementation for Sturgis")
(define authors '("jesse@serverracket.com"))
(define deps '("base"
               "typed-racket-lib"
               "syntax-classes-lib"
               "txexpr"
               "http-easy"))
(define build-deps '("rackunit-lib"
                     "rackunit-typed"))
