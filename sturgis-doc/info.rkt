#lang info

(define collection "sturgis")

(define build-deps '("scribble-lib"
                     "sturgis-lib"
                     "racket-doc"))

(define deps '("base"))

(define update-implies '("sturgis-lib"))

(define pkg-desc "Documentation for Sturgis")

(define scribblings '(("sturgis.scrbl")))

(define authors '("jesse@serverracket.com"))
