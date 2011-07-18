; "objective-scheme.scm" implementation of MOP for scheme -*-scheme-*-

;;; This file is an attempt to create an OOP system in scheme using the Metaobject
;;; Protocol based on
;;;   "Putting Metaclasses to Work" by Ira Forman and Scott Danforth, 1999.

;;; Some simple assertion code.
(define (assert p msg)
  (if p #t (error "ASSERTION ERROR: " msg)))
;  (if (and (boolean? p) (not p)) (error "ASSERTION ERROR: " msg) #t))

;;; Definition 1: Dictionary
;;; First we create a dictionary to store slots.
(define (obj-make-dictionary)
  (make-table init: 'nil))

(define (obj-dict-print d)
  (print "root")
  (newline)
  (obj-dict-print-recursive d 1)
  (newline))

(define (obj-dict-print-recursive d n)
  (define (tab n)
    (if (> n 0)
        (begin
          (print "  ")
          (tab (- n 1)))))
  (print "{")
  (newline)
  (table-for-each
   (lambda (k v)
     (if (table? v)
         (begin
           (tab n)
           (print k)
           (newline)
           (tab n)
           (obj-dict-print-recursive v (+ n 1)))
         (begin
           (tab n)
           (print k " = " v ", ")))
     (newline))
   d)
  (tab (- n 1))
  (print "}"))

(define (obj-undefined-method obj . args)
  (error "UNDEFINED METHOD: " obj))

(define (obj-make-method-table)
  (make-table init: obj-undefined-method))

(define (obj-dict-length d)
  (table-length d))

(define (obj-dict-copy d)
  (table-copy d))

(define (obj-dict-deep-copy d)
  (obj-dict-recursive-merge! (obj-make-dictionary) d))

(define (obj-dict? d)
  (table? d))

(define (obj-dict-contains? d k)
  (if (eq? (table-ref d k) 'nil) #f #t))
  
(define (obj-method-table-contains? d k)
  (if (eq? (table-ref d k) obj-undefined-method) #f #t))
  
(define (obj-dict-get d k)
  (table-ref d k))

(define (obj-dict-set! d k v)
  (table-set! d k v)
  d)

(define (obj-dict-init list)
  (let ((dictionary (obj-make-dictionary)))
    (for-each
     (lambda (elem)
       (obj-dict-set! dictionary (car elem) (cdr elem)))
     list)
    dictionary))

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

;;; Define Class and Object which requires a self-reference to create.
;;; Class is a subclass of Object. Class is an instance of class.
;;; Object is an instance of Class.
;;; We require Class and Object to exist at the root of the MOP.
(define (obj-make-empty-class-dictionary)
  (obj-dict-init (list
                  (cons 'ivdefs (obj-make-dictionary))
                  (cons 'mdefs (obj-make-dictionary))
                  (cons 'parents '())
                  (cons 'ivs (obj-make-dictionary))
                  (cons 'mtab (obj-make-dictionary)))))

(define (obj-find-mtab-method mtab mro msg)
  (if (null? mro)
      obj-undefined-method
      (let* ((class-mtab (obj-dict-get mtab (car mro)))
             (method (obj-dict-get class-mtab msg)))
        (if (not (eq? method obj-undefined-method))
            method
            (obj-find-mtab-method mtab (cdr mro) msg)))))

(define (obj-mro class)
  (let ((parents (obj-dict-get (class 'get-class-dict) 'parents)))
    (if (eq? (length parents) 0)
        (list class)
        (list Class (car parents)))))
        
; int i, j
; list ri
; for (i=0; i < lhs.length; i++)
;   for (j=0; j < rhs.length; j++)
;     if (lhs.i == rhs.j) { // found merge point
;       for(r=0; r < j; r++)
;         if (rhs.r not_elem lhs)
;           ri = append(ri, rhs.r)
;       return conservative-merge(append(lhs[0, i-1], ri, lhs[i,lhs.length-1]),
;                                 rhs[j+1, rhs.length-1])
;     }
; return append(lhs, rhs)

(define (list-merge list k obj)
  (cond ((null? list) obj)
        ((<= k 0) (append obj list))
        (else (cons (car list) (list-merge (cdr list) (- k 1) obj)))))

; lhs and rhs are lists.
(define (conservative-merge lhs rhs)
  (call/cc
    (lambda (return)
      (do ((i 0 (+ i 1)))
          ((>= i (length lhs)))
        (do ((j 0 (+ j 1)))
            ((>= j (length rhs)))
          (if (eq? (list-ref lhs i) (list-ref rhs j)) ; found a merge point
            (let ((right-insertions '()))
              (do ((r 0 (+ r 1)))
                  ((>= r j))
                (let ((r-value (list-ref rhs r)))
                  (if (not (memq r-value lhs))
                    (set! right-insertions (append right-insertions (list r-value))))))
              (let ((left-merge (list-merge lhs (+ i 0) right-insertions))
                    (right-merge (list-tail rhs (+ j 1))))
                  (return (conservative-merge left-merge right-merge)))))))
      (append lhs rhs))))

(define (first-index-of list x i)
  (define (first-index-of-count l x i c)
    (cond ((null? l) -1)
          ((<= i 0) (first-index-of-start l x c))
          (else (first-index-of-count (cdr l) x (- i 1) (+ c 1)))))
  (define (first-index-of-start l x c)
    (cond ((null? l) -1)
          ((eq? (car l) x) c)
          (else (first-index-of-start (cdr l) x (+ c 1)))))
  (first-index-of-count list x i 0))
  
(define (last-index-of list x i)
  (let ((reverse-index (first-index-of (reverse list) x (- (length list) i))))
    (if (eq? reverse-index -1)
      -1
;      (- (length list) reverse-index 1))))
      reverse-index

(define (is-disagreement-serious l r)
  #t)

(define (no-serious-order-disagreements lhs rhs)
  (call/cc
    (lambda (return)
      (do ((i 0 (+ i 1)))
          ((>= i (length lhs)))
        (let ((r (first-index-of rhs (list-ref lhs i) 0)))
              (if (>= r 0)
                (do ((j (+ i 1) (+ j 1)))
                    ((>= j (length lhs)))
                  (if (>= (last-index-of rhs (list-ref lhs j) 0))
                    (if (is-disagreement-serious (list-ref lhs i) (list-ref lhs j))
                      (return #f)))))))
      #t)))

;; LAMb: finish me!
(define (obj-mro-foo class)
  (let ((result (list class)))
    (for-each (lambda (cls)
            (let ((parent-mro (obj-mro-foo cls)))
              (if (no-serious-order-disagreements result parent-mro)
                  (set! result (conservative-merge result parent-mro))
                  '(#f))))
          (class 'get-parents))
    result))

(define (obj-make-dispatcher root-dict class-dict)
  (letrec ((dispatcher
            (lambda (msg . args)
              (case msg
                ('get-class-dict class-dict)
                ('get-root-dict root-dict)
                (else
                 (let*
                     ((class (obj-dict-get root-dict 'class))
                      (parent-class-dict (class 'get-class-dict))
                      (mtab (obj-dict-get parent-class-dict 'mtab))
                      (mro (obj-mro class))
                      (method (obj-find-mtab-method mtab mro msg)))
                   (apply method dispatcher args)))))))
    dispatcher))

(define (obj-get-class obj)
  (obj-dict-get (obj 'get-root-dict) 'class))

(define (OBJ-EMPTY-METHOD obj . args)
  (begin
    (display "obj-empty-method")
    obj))

(define (OBJ-EMPTY-PREDICATE obj . args)
  #f)

(define (obj-init-object-ivdefs object)
  (let ((object-ivdefs (obj-dict-get (object 'get-class-dict) 'ivdefs))
        (object-ivs (obj-dict-get (object 'get-class-dict) 'ivs)))
    (obj-dict-set! object-ivdefs object (obj-make-dictionary))
    (obj-dict-set! object-ivs object (obj-make-dictionary))))

(define (obj-init-object-methods class Object)
  (let
      ((mdefs
        (obj-dict-get (obj-dict-get (class 'get-class-dict) 'mdefs) Object))
       (mtab
        (obj-dict-get (obj-dict-get (class 'get-class-dict) 'mtab) Object))
       (get-iv
        (lambda (obj var)
          (obj-dict-get (obj 'get-class-dict) var)))
       (set-iv!
        (lambda (obj var value)
          (if (eq? obj Object)
              (error "ATTEMPTED TO SET UNDEFINED CLASS INSTANCE VARIABLE!")
              (obj-dict-set! (obj 'get-class-dict) var value))))
       (print-obj
        (lambda (obj)
          (obj-dict-print (obj 'get-root-dict))))
       )
    (if (eq? mdefs 'nil)
        (begin
          (set! mdefs (obj-make-method-table))
          (obj-dict-set!
           (obj-dict-get (class 'get-class-dict) 'mdefs)
           Object
           mdefs)))
    (if (eq? mtab 'nil)
        (begin
          (set! mtab (obj-make-method-table))
          (obj-dict-set!
           (obj-dict-get (class 'get-class-dict) 'mtab)
           Object
           mtab)))
    (obj-dict-set! mdefs 'initialize OBJ-EMPTY-METHOD)
    (obj-dict-set! mdefs 'get-iv get-iv)
    (obj-dict-set! mdefs 'set-iv! set-iv!)
    (obj-dict-set! mdefs 'print print-obj)
    (obj-dict-merge! mtab mdefs)
    ))

(define (obj-make-instance obj)   ;{"class"=X} <= X.Class."ivs"
  (let*
      ((root-dict
        (obj-dict-merge!
         (obj-dict-init (list (cons 'class obj)))
         (obj-dict-get (obj 'get-class-dict) 'ivs))) ;LAMb: will this work? What is the "class" key in this case?
       (class-dict
        (obj-dict-get root-dict obj)))
     (obj-make-dispatcher root-dict class-dict)))

(define (obj-method-table-contains-mro? d k mro)
  (if (null? mro)
        #f
        (let ((c (obj-dict-get d (car mro))))
          (if (eqv? c 'nil)
            (error "Illegal MRO list; could not find " c)
            (if (obj-method-table-contains? c k)
              #t
              (obj-method-table-contains-mro? d k (cdr mro)))))))

(define (obj-dict-contains-mro? d k mro)
  (if (null? mro)
        #f
        (let ((c (obj-dict-get d (car mro))))
          (if (eqv? c 'nil)
            (error "Illegal MRO list; could not find " c)
            (if (obj-dict-contains? c k)
              #t
              (obj-dict-contains-mro? d k (cdr mro)))))))

(define (obj-dict-for-each func d)
  (table-for-each
    (lambda (k v)
      (if (obj-dict? v)
        (obj-dict-for-each func v)
        (func k v)))
    d))

;;; Does not check if add is legal or not.
(define (obj-add-method obj class method-name method)
  (let* ((method-def (obj 'get-iv 'mdefs))
        (table-defs (obj-dict-get method-def class))
        (method-table (obj 'get-iv 'mtab))
        (table-methods (obj-dict-get method-table class)))
    (if (eq? table-defs 'nil)
          (begin (set! table-defs (obj-make-method-table))
                 (obj-dict-set! method-def class table-defs)))
    (if (eq? table-methods 'nil)
          (begin (set! table-methods (obj-make-method-table))
                 (obj-dict-set! method-table class table-methods)))
    (obj-dict-set! table-defs method-name method)
    (obj-dict-set! table-methods method-name method)))
    
(define (obj-resolve-method class method-name)
  (let ((method-pair (assq method-name (class 'get-supported-methods))))
    (if method-pair (cdr method-pair) obj-undefined-method)))

(define (obj-init-class-methods Class)
  (let
      ((mdefs
        (obj-dict-get (obj-dict-get (Class 'get-class-dict) 'mdefs) Class))
       (get-parents
        (lambda (obj)
          (obj-dict-get (obj 'get-class-dict) 'parents)))
       (print-obj
        (lambda (obj)
          (obj-dict-print (obj 'get-root-dict))))
       (make-instance-with-init
        (lambda (obj)
          (let ((instance (obj-make-instance obj)))
            (instance 'initialize))))
       (initialize-class
        (lambda (obj parent-list)
          (assert (> (length parent-list) 0) "Has no parents!")
          #t)) ; LAMb: FINISH ME!
       (is-subclass-of?
        (lambda (obj another-class)
          (or (eq? obj another-class)
              (list? (member another-class (obj 'get-parents))))))
       (is-descendant-of?
        (lambda (obj another-class)
          (define (check-parent-descendants parents)
            (cond ((null? parents) #f)
                  (((car parents) 'is-descendant-of? another-class) #t)
                  (check-parent-descendants (cdr parents))))
          (or (obj 'is-subclass-of? another-class)
              (check-parent-descendants (obj 'get-parents)))))
       (is-parent-of?
        (lambda (obj another-class)
          (another-class 'is-subclass-of? obj)))
       (is-ancestor-of?
        (lambda (obj another-class)
          (another-class 'is-descendant-of? obj)))
       (ready-class
        (lambda (obj)
          obj)) ; do nothing, its a hook
       (defines-method?
        (lambda (obj method)
            (obj-method-table-contains-mro? (obj 'get-iv 'mdefs) method (obj-mro obj))))
       (supports-method?
        (lambda (obj method)
          (obj-method-table-contains-mro? (obj 'get-iv 'mtab) method (obj-mro obj))))
       (introduces-method?
        (lambda (obj method)
          (obj-method-table-contains? (obj-dict-get (obj 'get-iv 'mdefs) obj) method)))
       (supports-iv?
        (lambda (obj iv)
          (obj-dict-contains-mro? (obj 'get-iv 'ivs) iv (obj-mro obj))))
       (get-supported-methods ;; LAMb: this has to be in MRO order! But its not...
        (lambda (obj)
          (let ((methods '()))
            (obj-dict-for-each
              (lambda (k v)
                (set! methods (cons (cons k v) methods)))
              (obj 'get-iv 'mtab))
            methods)))
       (resolve-terminal-old
        (lambda (obj intro-class method-name)
          (let ((method (assq method-name (obj 'get-supported-methods))))
            (display method)
          )))
       (resolve-terminal
        (lambda (obj intro-class method-name)
          (let ((method-table (obj-dict-get (obj 'get-iv 'mtab) intro-class)))
            (if (eq? 'nil method-table)
              obj-undefined-method
              (obj-dict-get method-table method-name)))))
       (dispatch
        (lambda (obj method . args)
          (display "dispatch!")))
       (put-rd-stub ;; LAMb: fix me!
        (lambda (obj intro-class method-name)
          (let* ((method-table (obj-dict-get (obj 'get-iv 'mtab) intro-class))
                 (method (if (eq? 'nil method-table)
                          obj-undefined-method
                          (obj-dict-get method-table method-name)))
                 (redispatcher
                  (lambda (obj . args)
                    (obj 'dispatch method args)))
                )
            (if (eq? 'nil method-table)
              #f
              (begin
                (display (table->list method-table))
                (display method-name)
                (display method)
                (display redispatcher)
                (obj-dict-set! method-table method-name redispatcher)
                )))))
       (add-method
        (lambda (obj class method-name method)
          (let* ((supported-methods (class 'get-supported-methods))
                (mro (obj-mro obj)))
            (assert (not (assq method-name supported-methods)) "Attempted to add-method that is already supported!")
            (assert (memq class mro) "Attempted to add-method for class that does not exist in MRO!")
            (obj-add-method obj class method-name method)
          )))
       (override-method
        (lambda (obj class method-name method)
          (let* ((supported-methods (class 'get-supported-methods)))
            (assert (assq method-name supported-methods) "Attempted to override-method that does not exist!")
            (obj-add-method obj class method-name method)
          )))
       (resolve-method
        (lambda (obj method-name)
          (obj-resolve-method obj method-name)))
      )
    (obj-dict-set! mdefs 'make-instance make-instance-with-init)
    (obj-dict-set! mdefs 'initialize-class initialize-class)
    (obj-dict-set! mdefs 'ready-class ready-class)
    (obj-dict-set! mdefs 'get-parents get-parents)
    (obj-dict-set! mdefs 'get-mro obj-mro)
    (obj-dict-set! mdefs 'is-subclass-of? is-subclass-of?)
    (obj-dict-set! mdefs 'is-descendant-of? is-descendant-of?)
    (obj-dict-set! mdefs 'is-parent-of? is-parent-of?)
    (obj-dict-set! mdefs 'is-ancestor-of? is-ancestor-of?)
    (obj-dict-set! mdefs 'supports-iv? supports-iv?)
    (obj-dict-set! mdefs 'defines-method? defines-method?)
    (obj-dict-set! mdefs 'supports-method? supports-method?)
    (obj-dict-set! mdefs 'introduces-method? introduces-method?)
    (obj-dict-set! mdefs 'get-supported-methods get-supported-methods)
    (obj-dict-set! mdefs 'add-method add-method)
    (obj-dict-set! mdefs 'override-method override-method)
    (obj-dict-set! mdefs 'resolve-method resolve-method)
    (obj-dict-set! mdefs 'resolve-terminal resolve-terminal)
    (obj-dict-set! mdefs 'dispatch dispatch)
    (obj-dict-set! mdefs 'put-rd-stub put-rd-stub)
    ))

(define (obj-init)
  (let* ((Class-Prototype
          (obj-make-dictionary))
         (Method-Defs
          (obj-make-dictionary))
         (Class-Dictionary
          (obj-dict-init (list
                          (cons 'ivdefs   Class-Prototype)
                          (cons 'ivs      Class-Prototype)
                          (cons 'parents  '())
                          (cons 'mdefs    Method-Defs)
                          (cons 'mtab     Method-Defs))))
         (Root-Dictionary
          (obj-make-dictionary))
         (Class
          (obj-make-dispatcher Root-Dictionary Class-Dictionary)))
    (obj-dict-set! Class-Prototype Class (obj-make-empty-class-dictionary))
    (obj-dict-set! Method-Defs Class (obj-make-method-table))
    (obj-dict-set! Root-Dictionary 'class Class)
    (obj-dict-set! Root-Dictionary Class Class-Dictionary)
    (obj-init-class-methods Class)
    (let
        ((Object (obj-make-instance Class)))
      (obj-dict-set! Class-Dictionary 'parents (list Object))
      (obj-dict-set! Class-Prototype Object (obj-make-dictionary))
      (obj-init-object-ivdefs Object)
      (obj-init-object-methods Class Object)
      (obj-init-object-methods Object Object))
    Class))

(define Class (obj-init))
(define Object (car (Class 'get-parents)))

(define lhs '(P Q R S Y))      
(define rhs '(U V R W X Y T Z))
(define lhx (append lhs '(X))) 

