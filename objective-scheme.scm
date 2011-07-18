; "objective-scheme.scm" implementation of MOP for scheme -*-scheme-*-

; This file is an attempt to create an OOP system in scheme using the Metaobject
; Protocol based on
;   "Putting Metaclasses to Work" by Ira Forman and Scott Danforth, 1999.

;;; Some simple assertion code.
(define-macro (assert p)
  (if (eq? p #f) (error "ASSERTION ERROR!") #t))

;;; Definition 1: Dictionary
;;; First we create a dictionary to store slots.
(define (obj-make-dictionary)
  (make-table init: 'nil)) ; LAMb: probably want to replace nil!

(define (obj-make-data-table)
  (define (no-such-iv obj . args)
    (error "INVALID INSTANCE VARIABLE: " msg))
  (make-table init: 'no-such-iv))

(define (obj-make-method-table)
  (make-table init: 'nil))

(define (obj-dict-copy d)
  (table-copy d))

(define (obj-dict? d)
  (table? d))

(define (obj-dict-contains? d k)
  (if (eq? (table-ref d k) 'nil) #f #t))

(define (obj-dict-get d k)
  (table-ref d k))

(define (obj-dict-set! d k v)
  (table-set! d k v)
  d)

(define (obj-dict-key-list d)
  (map (lambda (x) (car x)) (table->list d)))

(define (obj-dict-merge! d1 d2)
  (table-merge! d1 d2 #f))

(define (obj-dict-recursive-merge! d1 d2)
  (table-for-each
   (lambda (k v2)
     (let ((v1 (obj-dict-get d1 k)))
       (if (eq? v1 'nil)
           (obj-dict-set! d1 k (if (obj-dict? v2) (obj-dict-copy v2) v2))
           (if (and (obj-dict? v1) (obj-dict? v2))
               (obj-dict-recursive-merge! v1 v2)))))
   d2)
  d1)

;;; Define Class and Object which require a loop to define.
;;; Class is a subclass of Object. Class is an instance of class.
;;; Object is an instance of Class.
;;; We require Class and Object to exist at the root of the MOP.
(define (obj-init)
  )

(define Class (obj-init))
(define Object (car (Class 'get-parents)))

