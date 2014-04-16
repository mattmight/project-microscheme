#lang racket

; *** Input language ***

; <prog> ::= <defs> <exp>

; <defs> ::= <def> <defs>
;         |  

; <def> ::= (define (<var> <formals>) <exp>)

; <formals> ::= <var> <formals>
;            |  

; <exp> ::= <integer>
;        |  <var>
;        |  #t | #f
;        |  (lambda (<formals>) <exp>)
;        |  (let ([<var> <exp>] ...) <exp>)
;        |  (if <exp> <exp> <exp>)
;        |  (set! <var> <exp>)
;        |  (begin <exp> ...)
;        |  (<prim> <exp> ...)
;        |  (<exp> <exp> ...)
;        |  (call/cc <exp>)


; <prim> ::= +  |  -  |  *  |  = 
;         |  void

(define (prim? exp)
  (case exp
    [(+ - * = void) #t]
    [else      #f]))

      

(define (map-exp-bu f exp)
  
  (define (g x) (map-exp-bu f x))
  
  (match exp
    [(? symbol?)       (f exp)]
    [(? boolean?)      (f exp)]
    [(? integer?)      (f exp)]
    
    [`(lambda ,formals ,exp)
     (f `(lambda ,formals ,(g exp)))]
    
    [`(let ,(list `(,vars ,args) ...) ,exp)
     (f `(let ,(map list vars (map g args)) ,(g exp)))]
    
    [`(if ,cond ,cons ,alt)
     (f `(if ,(g cond) ,(g cons) ,(g alt)))]
             
    [`(set! ,var ,exp)
     (f `(set! ,var ,(g exp)))]
    
    [`(call/cc ,exp)
     (f `(call/cc ,(g exp)))]
    
    [`(,(and op (? prim?)) . ,args)
     (f `(,op ,@(map g args)))]
    
    [`(,fun . ,args)
     (f `(,(g fun) ,@(map g args)))]))


; *** Desugared language ***

; <prog> ::= <defs> <exp>

; <defs> ::= <def> <defs>
;         |  

; <def> ::= (define (<var> <formals>) <exp>)

; <formals> ::= <var> <formals>
;            |  

; <exp> ::= <integer>
;        |  <var>
;        |  #t | #f
;        |  (lambda (<formals>) <exp>)
;        |  (if <exp> <exp> <exp>)
;        |  (set! <var> <exp>)
;        |  (call/cc <exp>)
;        |  (<exp> <exp> ...)
;        |  (<prim> <exp> ...)


(define (desugar-locally exp)
  (match exp
    [`(let ,(list `(,vars ,args) ...) ,exp)
     `((lambda ,vars ,exp) ,@args)]
    
    [`(begin)
     `(void)]
    
    [`(begin ,exp)
     exp]
    
    [`(begin ,exp . ,exps)
     (define $t (gensym '$t))
     `(let ([,$t ,exp]) (begin ,@exps))]
    
    [else exp]))
   

(define (desugar-everywhere exp)
  (map-exp-bu desugar-locally exp))

(define (desugar-program prog)
  (match prog
    [(cons `(define (,f . ,formals) ,exp) rest)
     (cons `(define (,f . ,formals) ,(desugar exp)) 
           (desugar-program rest))]
    
    [(cons `(define ,v ,exp) rest)
     (cons `(define ,v ,(desugar exp))
           (desugar-program rest))]
    
    [(list exp)
     (list (desugar exp))]))



(define (iterate/fix f)
  (λ (z)
    (define next (f z))
    (if (equal? next z)
        z
        ((iterate/fix f) next))))

(define desugar (iterate/fix desugar-everywhere))
         


; *** A-Normalized language ***

; <prog> ::= <defs> <exp>

; <defs> ::= <def> <defs>
;         |  

; <def> ::= (define <var> <exp>)

; <formals> ::= <var> <formals>
;            |  

; <aexp> ::= <integer>
;         |  <var>
;         |  #t | #f
;         |  (lambda (<formals>) <exp>)
;         |  (<prim> <aexp> ...)

; <cexp> ::= |  (set! <var> <aexp>)
;            |  (call/cc <aexp>)
;            |  (<aexp> <aexp> ...)
;            |  (if <aexp> <exp> <exp>)

; <exp> ::= <cexp>
;        |  <aexp>
;        |  (let ([<var> <exp>]) <exp>)


(define (atomic? exp)
  (match exp
    [(? integer?)        #t]
    [(? boolean?)        #t]
    [(? symbol?)         #t]
    [else                #f]))


;; Expression normalization:
(define (normalize-term exp) (normalize exp (λ (x) x)))

(define (normalize exp k)
  (match exp
    [`(lambda ,params ,body)   
      (k `(lambda ,params ,(normalize-term body)))]
    
    [`(let () ,exp)
      (normalize exp k)]

    [`(let ([,x ,exp1] . ,clause) ,exp2) 
      (normalize exp1 (λ (aexp1) 
       `(let ([,x ,aexp1])
         ,(normalize `(let (,@clause) ,exp2) k))))]
    
    [`(if ,exp1 ,exp2 ,exp3)    
      (normalize-name exp1 (λ (t) 
       (k `(if ,t ,(normalize-term exp2) 
                  ,(normalize-term exp3)))))]
    
    [`(set! ,v ,exp)
      (normalize-name exp (λ (t)
       `(let ([,(gensym '_) (set! ,v ,t)])
          ,(k '(void)))))]
    
    [`(,f . ,e*) 
      (normalize-name f (λ (t) 
       (normalize-name* e* (λ (t*)
        (k `(,t . ,t*))))))]
    
    [(? atomic?)            
     (k exp)]))

(define (normalize-name exp k)
  (normalize exp (λ (aexp) 
    (if (atomic? aexp) (k aexp) 
        (let ([t (gensym)]) 
         `(let ([,t ,aexp]) ,(k t)))))))

(define (normalize-name* exp* k)
  (if (null? exp*)
      (k '())
      (normalize-name (car exp*) (λ (t) 
       (normalize-name* (cdr exp*) (λ (t*) 
        (k `(,t . ,t*))))))))


;; Top-level normalization:
(define (normalize-define def)
  (match def
    [`(define (,f . ,params) ,body)
     `(define ,f ,(normalize-term `(lambda ,params ,body)))]
    
    [`(define ,v ,exp)
     `(begin ,@(flatten-top (normalize-term exp) v))]))
                    

(define (flatten-top exp v)
  (match exp
    [`(let ([,x ,cexp]) ,exp)
     (cons `(define ,x ,cexp) 
            (flatten-top exp v))]
    
    [else
     `((define ,v ,exp))]))


(define (normalize-program decs)
  (flatten-begins (normalize-program* decs)))

(define (normalize-program* decs)
  (match decs
    ['() 
     (error "empty program")]
    
    [(cons `(define . ,_) rest)
     (cons (normalize-define (car decs))
           (normalize-program rest))]
    
    [(list exp)
     (list (normalize-term exp))]))
  
  


(define (flatten-begins prog)
  
  (match prog
    [(cons (and def `(define . ,_)) rest)
     (cons def (flatten-begins rest))]
    
    [(cons `(begin . ,defs) rest)
     (append defs (flatten-begins rest))]
    
    [(list exp)
     (list exp)]))
     


; It's possible to turn a program into a single expression
; of "lets and sets":

; (define v1 exp1)

; (define v2 exp2)
; ...
; body

; =>

; (let ([v1 #f] [v2 #f] ...) 
;  (begin
;    (set! v1 exp1)
;    (set! v2 exp2)
;    ...
;    body))


(define (program->exp prog)
  (error "maybe you want to define this one"))



(define (read-all)
  (define next (read))
  (if (eof-object? next)
      '()
      (cons next (read-all))))


(define prog (read-all))

(define exp (program->exp prog))

(define desugared-exp (desugar exp))

(define anf-exp (normalize-term desugared-exp))

; Hook into (modified) anf-cesk.rkt here.


