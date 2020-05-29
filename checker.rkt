#lang racket

;;;;;;;;;;;;;;;
#| EXPANDER |#
;;;;;;;;;;;;;;;

(define (expand-expr expr)
  (match expr
    [`(let ([,id ,val])
        ,body)
     `(,(expand-expr body))]
    [`(let ([,ids ,vals] ...)
        ,body)
      `(let ([,(car ids) ,(car vals)])
         ,(expand-expr `(let ,(map list (cdr ids) (cdr vals))
                          ,body)))]
    [`(lambda (,id)
         ,body)
      `(lambda (,id)
          ,(expand-expr body))]
    [`(lambda (,ids ...)
         ,body)
      `(lambda (,(car ids))
          ,(expand-expr `(lambda ,(cdr ids)
                            ,body)))]
    [`(,f ,x)
      `(,(expand-expr f) ,(expand-expr x))]
    [`(,f ,x . ,xs)
      (expand-expr `((,f ,x) . ,xs))]
    [_ expr]))

;;;;;;;;;;;;;;;;;;
#| TYPE-CHECKER |#
;;;;;;;;;;;;;;;;;;

(struct type () #:transparent)
(struct int type () #:transparent)
(struct bool type () #:transparent)
(struct fun type (from to) #:transparent)

(define (external-form->type texpr)
  (match texpr
    ['int (int)]
    ['bool (bool)]
    [`(-> ,in ,out) (fun (external-form->type in)
                         (external-form->type out))]))

(define (type->external-form ty)
  (match ty
    [(int) 'int]
    [(bool) 'bool]
    [(fun arg-type result-type)
     (list (type->external-form arg-type)
           '->
           (type->external-form result-type))]))

(define (check tenv expr ty)
  (displayln (format "check ~a ~a" expr ty))
  (newline)
  (match expr 
    [(? type?)
     (define t1 expr)
     (define t2 ty)
     (cond
       [(equal? t1 t2) t1]
       [else (error (format "~a does not check against ~a" t1 t2))])]

    [`(lambda (,x) ,e) 
      (cond
        [(fun? ty) 
         (match-define (fun t1 t2) ty)
         (check (hash-set tenv x t1) e t2)
         (fun t1 t2)]
        [else (error (format "function ~a does not check against non-function ~a" expr ty))])]

    [_ (check tenv (synth tenv expr) ty)]))

(define (synth tenv expr)
  (displayln (format "synth ~a" expr))
  (newline)
  (match expr
    [(? number?) (int)]
    [(? boolean?) (bool)]
    [(? symbol?) (hash-ref tenv expr)]
    [`(: ,e ,t) (check tenv e (external-form->type t))]
    [`(,e1 ,e2) (match-define (fun t1 t2) (synth tenv e1))
                (check tenv e2 t1)
                t2]))
