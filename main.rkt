;huzeyfe bahadiroglu
;2018400114
;compiling: yes
;complete: yes

#lang racket

(provide (all-defined-out))

; 10 points
(define := (lambda (var value) (list var value)))
; 10 points
(define --agmntd (lambda args (if (list? args) (append '(let) args) (append '(let) (list args)))))
(define -- (lambda args (append '(let) (list args))))
; 10 points
(define @ (lambda (bindings expr) (append bindings expr)))
; 20 points
(define split_at_delim (lambda (delim args) (foldr
                                             (lambda (currElem next)
                                               (cond
                                                 [ (eqv? currElem delim) (cons '() next) ]
                                                 [else ( cons (cons currElem (car next) ) (cdr next))]))
                                             '(()) args)))
; 30 points

(define parse_expr (lambda (expr) (cond
                                    [(and (member 'quote expr ) (= 2 (length expr) ) ) expr]
                                    [(and (number? (car expr) ) (= 1 (length expr) ) ) (car expr)]
                                    [(and (symbol? (car expr) ) (= 1 (length expr) ) ) (car expr)]
                                    [(= 1 (length expr)) (parse_expr (car expr))] ; paranhteses handling
                                    [else (cond
                                            [ (member '+ expr)  (append '(+) (map parse_expr (splitExpr expr)))]                                  
                                            [ (member '* expr) (append '(*) (map parse_expr (splitExpr expr)))]                                  
                                            [ (member '@ expr)  (@ (car(map parse_expr (splitExpr expr))) (cdr(map parse_expr (splitExpr expr))) )]                                              
                                            [ (member '-- expr) (--agmntd (map
                                                                     (lambda (args)
                                                                       (:= (cadr(car(remove ':= args)))
                                                                           (if (number? (cadr (remove ':= args))) (cadr (remove ':= args)) (cadr(cadr (remove ':= args))))))
                                                                     (if (eq? 'quote (caar(parse_expr(splitExpr expr)))) (list(parse_expr(splitExpr expr))) (parse_expr(splitExpr expr)))))]                                          
                                            [else expr ])])))


(define splitExpr (lambda (expr) (cond
                                   [ (member '+ expr)  (split_at_delim '+ expr)]                                  
                                   [ (member '* expr) (split_at_delim '* expr)]                                  
                                   [ (member '@ expr)  (split_at_delim '@ expr)]
                                   [ (member '-- expr) (remove '() (split_at_delim '-- expr))]
                                   [ (member ':= expr)  (split_at_delim ':= expr)]
                                   [else expr])))


; 20 points
(define eval_expr (lambda (expr) (eval (parse_expr expr))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

