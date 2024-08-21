#lang racket


(define expr-eval
  (lambda (expr env)
    (cond
      [(number? expr) expr]
      [(string? expr) expr]
      [(list? expr) (let ([op (car expr)] [opr (cdr expr)])
                      (cond
                        [(equal? op 'PLUS) (+ (expr-eval (first opr) env) (expr-eval (second opr) env))]
                        [(equal? op 'MUL) (* (expr-eval (first opr) env) (expr-eval (second opr) env))]
                        [(equal? op 'FUNC) (let ([param (first opr)] [body (second opr)])
                                               (lambda (arg) (expr-eval body (lambda (id)
                                                                                  (if (equal? id param) 
                                                                                    arg
                                                                                    (env id))))))]
                        [else ((expr-eval op env) (expr-eval (first opr) env))]))]
      [(symbol? expr) (env expr)]
      [else (error "invalid syntax")])))


(expr-eval '((FUNC v (PLUS v 1)) ONE) (lambda (id) 
                                        (case id
                                             ['ONE 1]))) 
