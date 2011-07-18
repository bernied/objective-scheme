; "test-objective-scheme.scm" implementation of MOP for scheme -*-scheme-*-

;;; This file implements the test cases for objective-scheme.

;(load "testeez")
;(load "objective-scheme")

;(define Class (obj-init))
;(define Object (car (Class 'get-parents)))


(testeez 
  "Class Creation Test"
  
;  (test-define  "Class value"
;    Class (obj-init))
    
  (test-define  "root-dict"
    root-dict (Class 'get-root-dict))

  (test-define  "class-dict"
    class-dict (Class 'get-class-dict))

  (test/equal   "Class contains parents list"
    (list? (Class 'get-parents)) #t)

  (test/equal   "parents"
    (length (Class 'get-parents)) 1)
  
  (test/equal   "'class = Class"
    (obj-get-class Class) Class)
    
  (test/equal   "root-dict is dictionary"
    (obj-dict? root-dict) #t)
    
  (test/equal   "root-dict contains class-dict"
    (obj-dict-get root-dict Class) (Class 'get-class-dict))
    
  (test/equal   "class-dict is dictionary"
    (obj-dict? class-dict) #t)
    
  (test/equal   "class-dict contains ivdefs dictionary"
    (obj-dict? (obj-dict-get class-dict 'ivdefs)) #t)
    
  (test/equal   "class-dict contains mdefs dictionary"
    (obj-dict? (obj-dict-get class-dict 'mdefs)) #t)
  
  (test/equal   "class-dict contains ivs dictionary"
    (obj-dict? (obj-dict-get class-dict 'ivs)) #t)

  (test/equal   "class-dict contains mtab dictionary"
    (obj-dict? (obj-dict-get class-dict 'mtab)) #t)
  
  (test-define  "ivdefs"
    ivdefs (obj-dict-get class-dict 'ivdefs))
  
  (test/equal   "ivdefs contains Class dictionary"
    (obj-dict? (obj-dict-get ivdefs Class)) #t)

  (test-define  "mdefs"
    mdefs (obj-dict-get class-dict 'mdefs))
  
  (test/equal   "mdefs contains Class dictionary"
    (obj-dict? (obj-dict-get mdefs Class)) #t)
    
  (test-define  "ivs"
    ivs (obj-dict-get class-dict 'ivs))
  
  (test/equal   "ivs contains Class dictionary"
    (obj-dict? (obj-dict-get ivs Class)) #t)

  (test-define  "mtab"
    mtab (obj-dict-get class-dict 'mtab))
  
  (test/equal   "mtab contains Class dictionary"
    (obj-dict? (obj-dict-get mtab Class)) #t)
    
  (test-define  "ivdefs-class"
    ivdefs-class (obj-dict-get ivdefs Class))
    
  (test/equal   "ivdefs-class contains ivdefs dictionary"
    (obj-dict? (obj-dict-get ivdefs-class 'ivdefs)) #t)

  (test/equal   "ivdefs-class contains mdefs dictionary"
    (obj-dict? (obj-dict-get ivdefs-class 'mdefs)) #t)

  (test/equal   "ivdefs-class contains ivs dictionary"
    (obj-dict? (obj-dict-get ivdefs-class 'ivs)) #t)

  (test/equal   "ivdefs-class contains mtab dictionary"
    (obj-dict? (obj-dict-get ivdefs-class 'mtab)) #t)

  (test/equal   "ivdefs-class contains parents list"
    (list? (obj-dict-get ivdefs-class 'parents)) #t)

  (test/equal   "ivdefs-class contains empty parents"
    (length (obj-dict-get ivdefs-class 'parents)) 0)

  (test-define  "mdefs-class"
    mdefs-class (obj-dict-get mdefs Class))
  
  (test/equal   "mdefs-class contains 'make-instance"
    (procedure? (obj-dict-get mdefs-class 'make-instance)) #t)
    
  (test-define  "ivs-class"
    ivs-class (obj-dict-get ivs Class))
    
  (test/equal   "ivs-class contains ivdefs dictionary"
    (obj-dict? (obj-dict-get ivs-class 'ivdefs)) #t)

  (test/equal   "ivs-class contains mdefs dictionary"
    (obj-dict? (obj-dict-get ivs-class 'mdefs)) #t)

  (test/equal   "ivs-class contains ivs dictionary"
    (obj-dict? (obj-dict-get ivs-class 'ivs)) #t)

  (test/equal   "ivs-class contains mtab dictionary"
    (obj-dict? (obj-dict-get ivs-class 'mtab)) #t)

  (test/equal   "ivs-class contains parents list"
    (list? (obj-dict-get ivs-class 'parents)) #t)

  (test/equal   "ivs-class contains empty parents"
    (length (obj-dict-get ivs-class 'parents)) 0)

  (test-define  "mtab-class"
    mtab-class (obj-dict-get mtab Class))
  
  (test/equal   "mtab-class contains 'make-instance"
    (procedure? (obj-dict-get mtab-class 'make-instance)) #t)
)

(testeez 
  "Object Creation Test"
  
;  (test-define  "Class value"
;    Class (obj-init))
    
  (test-define  "Object value"
    Object (car (Class 'get-parents)))
    
  (test-define  "root-dict"
    root-dict (Object 'get-root-dict))

  (test-define  "class-dict"
    class-dict (Object 'get-class-dict))

  (test/equal   "Class contains parents list"
    (list? (Object 'get-parents)) #t)

  (test/equal   "parents"
    (length (Object 'get-parents)) 0)
  
  (test/equal   "'class = Class"
    (obj-get-class Object) Class)
    
  (test/equal   "root-dict is dictionary"
    (obj-dict? root-dict) #t)
    
  (test/equal   "root-dict contains class-dict"
    (obj-dict-get root-dict Class) (Object 'get-class-dict))
    
  (test/equal   "class-dict is dictionary"
    (obj-dict? class-dict) #t)
    
  (test/equal   "class-dict contains ivdefs dictionary"
    (obj-dict? (obj-dict-get class-dict 'ivdefs)) #t)
    
  (test/equal   "class-dict contains mdefs dictionary"
    (obj-dict? (obj-dict-get class-dict 'mdefs)) #t)
  
  (test/equal   "class-dict contains ivs dictionary"
    (obj-dict? (obj-dict-get class-dict 'ivs)) #t)

  (test/equal   "class-dict contains mtab dictionary"
    (obj-dict? (obj-dict-get class-dict 'mtab)) #t)
  
  (test-define  "ivdefs"
    ivdefs (obj-dict-get class-dict 'ivdefs))
      
  (test/equal   "ivdefs length is 0"
    (obj-dict-length ivdefs) 0)

  (test-define  "mdefs"
    mdefs (obj-dict-get class-dict 'mdefs))
      
  (test/equal   "mdefs length is 0"
    (obj-dict-length mdefs) 0)

  (test-define  "ivs"
    ivs (obj-dict-get class-dict 'ivs))
      
  (test/equal   "ivs length is 0"
    (obj-dict-length ivs) 0)

  (test-define  "mtab"
    mtab (obj-dict-get class-dict 'mtab))
      
  (test/equal   "mtab length is 0"
    (obj-dict-length mtab) 0)

  (test-define  "Create instance of Object"
    obj (Object 'make-instance))

  (test/equal   "make-instance"
    (procedure? obj) #t)
    
  (test/equal   "obj class is Object"
    (obj-get-class obj) Object)
)


;;; Need test cases to make sure data and method tables are in the right places!