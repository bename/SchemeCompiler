(load "pc.scm")
(load "pattern-matcher.scm")
 



;;;;;;;;;;;;;;;;;;;;;;
(print-graph #f) ; display circular structures
(print-gensym #f) ; print gensym as g1234
(case-sensitive #f) ; ditto
(print-brackets #f) ; do not use brackets when pretty-printing

(revert-interaction-semantics) ; allow builtins to be redefined

;;; fix bug in optimizer
(#%$sputprop 'append '*flags* 122)
(#%$sputprop 'append! '*flags* 34)
(#%$sputprop 'list* '*flags* 1250)
(#%$sputprop 'cons* '*flags* 1250)

;;; And just for good luck :-)
(define with (lambda (s f) (apply f s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(define *reserved-words*
  '(and begin cond define do else if lambda
    let let* letrec or quasiquote unquote 
    unquote-splicing quote set!))

(define check-sym? (lambda (sym ls)
    (if (null? ls) #t
      (if (equal? sym (car ls)) #f
         (check-sym? sym (cdr ls)))))) 
                 
(define is-applic? (lambda (exp)
    (or (and (list? exp) (not (null? exp)) (list? (car exp)))
      (and (list? exp) (not (null? exp)) (symbol? (car exp)) 
        (check-sym? (car exp)  *reserved-words*)))))    

(define list-length (lambda(ls x)
      (if (null? ls) x
        (list-length (cdr ls) (+ x 1)))))   

(define r-def? (lambda (exp)
     (and (list? exp) (equal? (list-length exp 0) 3)
          (symbol? (car exp)) 
          (equal? (symbol->string (car exp)) "define")
       
         (not (list? (cadr exp))))))   


(define quote? (lambda(ls)
            (and (list? ls) (not (null? exp)) (symbol? (car ls)) 
            (equal? (symbol->string (car ls)) "quote"))))     

(define if? (lambda (exp) 
       (and (list? exp) (equal? (list-length exp 0) 4) 
            (symbol? (car exp)) (equal? "if" (symbol->string (car exp))))))       

(define lambda? (lambda(exp)
            (and (list? exp)(> (list-length exp 0) 2)  (symbol? (car exp)) 
                 (equal? (symbol->string (car exp)) "lambda") 
                 (list? (cadr exp)))))       

(define lambda-args? (lambda(exp)
    (and (list? exp) (> (list-length exp 0) 2) (symbol? (car exp))  
        (equal? (symbol->string (car exp)) "lambda")  
         (symbol? (cadr exp)))))                    
(define if-void? (lambda (exp) 
       (and (list? exp) (equal? (list-length exp 0) 3) 
            (symbol? (car exp)) (equal? "if" (symbol->string (car exp))))))       
(define void?  (lambda (exp)
     (and (string? exp) (equal? "#<void>"     exp))))             
(define myseq (lambda (ls )
    
     (if (= (list-length ls 0) 1)
       (cons  (parse (car ls)) '())
      (cons (append '(seq) 
        (cons (map (lambda(x) (parse x)) ls) '())) '()))))
   
(define define-lambda? (lambda(exp)
       (and (list? exp)  (> (list-length exp 0) 2)(symbol? (car exp)) 
            (equal? "define" (symbol->string (car exp)))
          (list? (cadr exp)) )))
(define define-opt? (lambda(exp)
       (and (list? exp) (> (list-length exp 0) 2) (symbol? (car exp)) 
            (equal? "define" (symbol->string (car exp)))
           (not (list? (cadr exp))) (not (symbol? (cadr exp)))
           (check-opt (cadr exp)))))           
(define check-opt (lambda (ls)
     (if (null? ls) #f
      (if (and (pair? ls) (not (pair? (cdr ls) ) )  ) #t    
        (check-opt (cdr ls))))))


                    
(define opt->list (lambda (ls)
     (if (and (pair? ls) (not (pair? (cdr ls) ) )  ) (cons (car ls) '())
       (append (cons (car ls) '()) (opt->list (cdr ls))))))  

(define get-last (lambda (ls)
   (if (and (pair? ls) 
            (not (pair? (cdr ls) ) )  ) (cdr ls)
       (get-last (cdr ls)))))

(define lambda-opt? (lambda(exp) 
  (and (list? exp) (> (list-length exp 0) 2) (symbol? (car exp)) (equal? "lambda" (symbol->string (car exp))) (not (list? (cadr exp) ))
     (not (symbol? (cadr exp))) (check-opt (cadr exp)) )))  

(define is-begin? (lambda (exp)
    (and (list? exp) (not (null? exp)) 
         (equal? (car exp) 'begin)))) 
(define variable? (lambda (x)
     (and (symbol? x) ( check-sym? x  *reserved-words*))))  

(define cond? (lambda (exp) 
       (and (list? exp)  
            (symbol? (car exp)) (equal? "cond" (symbol->string (car exp)))))) 

(define let? (lambda (exp) 
       (and (list? exp)  
            (symbol? (car exp)) (equal? "let" (symbol->string (car exp))))))

(define let*? (lambda (exp) 
       (and (list? exp)  
            (symbol? (car exp)) (equal? "let*" (symbol->string (car exp))))))

(define letrec? (lambda (exp) 
       (and (list? exp)  
            (symbol? (car exp)) (equal? "letrec" (symbol->string (car exp))))))

(define and? (lambda (exp) 
       (and (list? exp)  
            (symbol? (car exp)) (equal? "and" (symbol->string (car exp))))))

(define or? (lambda (exp) 
       (and (list? exp) (not (null? exp))  
            (symbol? (car exp)) (equal? "or" (symbol->string (car exp))))))

(define Ym
  (lambda fs
    (let ((ms (map
		(lambda (fi)
		  (lambda ms
		    (apply fi (map (lambda (mi)
				     (lambda args
				       (apply (apply mi ms) args))) ms))))
		fs)))
      (apply (car ms) ms))))


(define expand-let
      (lambda (expr)
         (letrec((iter-let (lambda (expr  con          )         
        (if (null? expr) 
            (con (list) (list))    
                (iter-let (cdr expr) (lambda (parm const)
                (if ((lambda (e)(and (pair? (cdr e)) (pair? (cdr e)))) (car expr))
                   (con `(,(caar expr) ,@parm) `(,@(cdar expr) ,@const))
                    )))))))
           (iter-let (cadr expr)    (lambda (b g) ((lambda(x) x )  `((lambda ,b ,@(cddr expr)) ,@g)))      )) )     )


     

(define expand-or (lambda (ls)
    (if (and (= (list-length ls 0 ) 1) (equal? (car ls) 'or))
        #f
      (if (and (= (list-length ls 0 ) 2) (equal? (car ls) 'or))
         (cadr ls)
        (help-or (cdr ls) 'vor)  )))) 
              


(define help-or (lambda (ls   sym)
    (if (null? (cdr ls)) `(let ,(cons (list sym (car ls)) '())
                            (if ,sym ,sym #f))
      `(let ,(cons (list sym (car ls)) '()) 
         (if ,sym ,sym ,(help-or (cdr ls) sym))))))   
          
         

(define expand-and
  (lambda(expr) 
    (if (null? (cdr expr)) #t
     (letrec((iter-and (lambda (expr simple)
        (if 
          (not(pair? (cddr expr))) (simple (cadr expr))
              
              
               (iter-and `(and ,@(cddr expr))
                             (lambda (ans)(simple `(if ,   (cadr expr) ,ans   #f))) 
                      )))))
  (iter-and expr (lambda  (x) x))))))





(define beginnify
  (lambda (s)
    (cond ((null? s) (if #f #f))
      
	  ((null? (cdr s)) (car s))
	  (else `(begin ,@s)))))



(define expand-cond
      (lambda (expr)
        
        (if (null? (cdr expr)) `(quasiquote exeption)
        
        (letrec((iter-cond (lambda (expr simple)                   
         (cond 
          
              ((and (eq? 'else (caadr expr) )(pair? (cdadr expr))(null? (cddr expr)))
                 (simple (beginnify (cdadr expr))))
          
                
          
              ((null? (cddr expr))
               
               (cond (not (pair? (cdadr expr)))
                   (simple `(let ((one ,(caadr expr)))
                            (if one one)))
                 (else 
                   (simple `(if ,(caadr expr) ,(beginnify (cdadr expr))))
                   )))
           

           
              (else 
                (iter-cond `(cond ,@(cddr expr)) (lambda (ans)
                           (if (null? (cddadr expr))
                               
                             (simple `(if  ,(caadr expr) ,(beginnify (cdadr expr)) ,ans))
                               
                              (simple `(let ((one ,(caadr expr))(two (lambda () ,ans)))
                                         
                                  (if (not one) (two) one)))
                           
                                   ))))   )     )))
          (iter-cond expr (lambda  (x) x))))))

(define expand-qq
  (lambda (e)
    (cond ((unquote? e) (cadr e))
	  ((unquote-splicing? e)
	   (error 'expand-qq "unquote-splicing here makes no sense!"))
	  ((pair? e)
	   (let ((a (car e))
		 (b (cdr e)))
	     (cond ((unquote-splicing? a) `(append ,(cadr a) ,(expand-qq b)))
		   ((unquote-splicing? b) `(cons ,(expand-qq a) ,(cadr b)))
		   (else `(cons ,(expand-qq a) ,(expand-qq b))))))
	  ((vector? e) `(list->vector ,(expand-qq (vector->list e))))
	  ((or (null? e) (symbol? e)) `',e)
	  (else e))))


(define ^quote?
  (lambda (tag)
    (lambda (e)
      (and (pair? e)
	   (eq? (car e) tag)
	   (pair? (cdr e))
	   (null? (cddr e))))))

(define unquote? (^quote? 'unquote))
(define unquote-splicing? (^quote? 'unquote-splicing))




(print-gensym #f)

(define expand-letrec
  (lambda (letrec-expr)
    (with letrec-expr
      (lambda (_letrec ribs . exprs)
	(let* ((fs (map car ribs))
	       (lambda-exprs (map cdr ribs))
	       (nu (gensym))
	       (nu+fs `(,nu ,@fs))
	       (body-f `(lambda ,nu+fs ,@exprs))
	       (hofs
		(map (lambda (lambda-expr) `(lambda ,nu+fs ,@lambda-expr))
		  lambda-exprs)))
	  `(Ym ,body-f ,@hofs))))))    
        

(define parse
  (let ((run
	 (compose-patterns
	  
    (pattern-rule
	   (? 'c null?)
	   (lambda (c) `(const ,c)))
 
    (pattern-rule
	   (? 'c boolean?)
	   (lambda (c) `(const ,c)))
           (pattern-rule
	   (? 'c number?)
	   (lambda (c) `(const ,c)))  
         (pattern-rule
	   (? 'c lambda-opt?)
	   (lambda (c) (append `(lambda-opt ,(opt->list (cadr c)) 
                 ,(get-last (cadr c))) (myseq (cddr c)))))                  
           (pattern-rule
	   (? 'c is-begin?)
	      (lambda (c) (car (myseq (cdr c)))))   
          (pattern-rule
	   (? 'c define-opt?)
	   (lambda (c) `(define ,(parse (caadr c)) 
                 ,(parse `(lambda ,(cdadr  c) ,(car (cddr c)))))))
          
                
          (pattern-rule
	   (? 'c is-applic?)
	   (lambda (c) `(applic ,(parse (car c)) 
                   ,(map (lambda(x) (parse x)) (cdr c)))))
            
    
               
           (pattern-rule
	   (? 'c define-lambda?)
	   (lambda (c) `(define ,(parse (caadr c)) 
                 ,(parse `(lambda ,(cdadr  c) ,(car (cddr c)))))))
            (pattern-rule
	   (? 'c char?)
	   (lambda (c) `(const ,c)))
           (pattern-rule
	   (? 'c void?)
	   (lambda (c) `(const ,(string->symbol c)))) 
           (pattern-rule
	   (? 'c string?)
	   (lambda (c) `(const ,c)))
           (pattern-rule
	   (? 'c variable?) 
	   (lambda (c) `(var ,c)))
  	      (pattern-rule
	   (? 'c quote?)
	   (lambda (c) `(const ,(cadr c))))
           (pattern-rule
	   (? 'c r-def?)
	   (lambda (c) 
          `(define ,(parse  (cadr c)) ,(parse (caddr c)))))
         
          (pattern-rule
	   (? 'c if?)
	   (lambda (c) `(if3 ,(parse (cadr c)) ,(parse (caddr c)) 
                   ,(parse (cadddr c))  )))
          (pattern-rule
	   (? 'c if-void?)
	   (lambda (c) `(if3 ,(parse (cadr c)) ,(parse (caddr c)) 
             ,(parse "#<void>"))  ))
            (pattern-rule
	   (? 'c lambda?)
	   (lambda (c) (append `(lambda-simple ,(cadr c)) 
                  (myseq (cddr c)))))                     
	  (pattern-rule
	   (? 'c lambda-args?)
	   (lambda (c) (append `(lambda-variadic ,(cadr c)) 
                   (myseq (cddr c)))))                     
	            
           (pattern-rule
	   (? 'c lambda-opt?)
	   (lambda (c) (append `(lambda-variadic ,(cadr c)) 
                   (myseq (cddr c)))))  
    
           (pattern-rule
	   (? 'c cond?)
	   (lambda (c) (parse (expand-cond c))))
    
            (pattern-rule
	   (? 'c let?)
	   (lambda (c) (parse (expand-let   c))))
             
           (pattern-rule
	   (? 'c letrec?)
	   (lambda (c) (parse (expand-letrec   c))))
     
           
            (pattern-rule
	   `(let* () ,(? 'expr) . ,(? 'exprs list?))
	   (lambda (expr exprs)
	     (parse (beginnify (cons expr exprs)))))   
    
           (pattern-rule
	   `(let* ((,(? 'var variable?) ,(? 'val)) . ,(? 'rest)) . ,(? 'exprs))
	   (lambda (var val rest exprs)
	     (parse `(let ((,var ,val))
		       (let* ,rest . ,exprs)))))
    

            (pattern-rule
	   (? 'c and?)
	   (lambda (c) (parse (expand-and c))))
    
     (pattern-rule
	   (? 'c or?)
	   (lambda (c) (parse (expand-or c))))
    
            (pattern-rule 
                `(,'quasiquote ,(? 'c )) 
                      (lambda (c) (parse (expand-qq c))))

 (pattern-rule
	   (? 'c list?)
	   (lambda (c) `(const ,c)))
    	 
 (pattern-rule
	   (? 'c pair?)
	   (lambda (c) `(const ,c)))
        
	  ;; add more rules here
	  )))
    (lambda (e . z)
      (if (null? z)
      (run e
	   (lambda ()
	     (error 'parse
    (format "I can't recognize this: ~s" e))))
        (parse  (parseq z `(begin ,e  )))  
          
          
          
          ))))

(define parseq (lambda (z curr) 
     (if (null? z)  curr
    (parseq (cdr z) (append curr (cons (car z) '()))))))    


  (define isplusOn? (lambda (ls)
     (and (list? ls) (not (null? ls)) 
          (equal? (car ls) '+) (> (list-length ls 0) 2)
          (only-num? (cdr ls)))))


(define only-num? (lambda (ls)
    (if (null? ls) #t
      (if (not (number? (car ls))) #f
         (only-num? (cdr ls)))))) 


(define sumup (lambda (ls curr)
      (if (null? ls) curr
       (if (not (number? (car ls))) (sumup (cdr ls) curr)
         (sumup (cdr ls) (+ curr (car ls)))))))  


(define is-there-vars? (lambda (ls)
     (if (null? ls) #f
      (if (symbol? (car ls)) #t
        (is-there-vars? (cdr ls))))))  

(define plusToKefel (lambda (ls sym curr)
  (if (null? ls) (if (= curr 1) sym `(* ,sym ,curr))
     (if (equal? sym (car ls)) 
         (plusToKefel (cdr ls) sym (+ curr 1))
      (plusToKefel (cdr ls) sym curr)))))


(define allButN (lambda (ls curr)
     (if (null? ls) curr
       (if (not (number? (car ls)))
          (allButN (cdr ls) (append curr (cons (car ls) '())))
           (allButN (cdr ls) curr)))))


(define in-list? (lambda (exp ls)
    (if (null? ls) #f
     (if (equal? exp (car ls)) #t
      (in-list? exp (cdr ls))))))


(define pinor (lambda (exp ls curr)
     (if (null? ls) -1
     (if (and (not (list? (car ls))) (equal? exp (car ls))) curr 
     (if (and (list? (car ls)) 
               (equal? (in-list? exp (car ls)) #t)) curr
       (pinor exp (cdr ls) (+ curr 1)))))))


(define minor (lambda (exp ls curr)
       (if (null? ls) -1
        (if (and (not (list? ls)) (equal? exp ls)) curr
        (if (equal? (car ls) exp) curr
          (minor exp (cdr ls) (+ curr 1)))))))  


(define get-list-index (lambda (ls index)
  (if (= index 0) (car ls)
      (get-list-index (cdr ls) (- index 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define lambda-simple? (lambda (ls)
      (and (list? ls) (not (null? ls)) 
           (equal? (car ls) 'lambda-simple))))                   
(define unparse-constant (lambda (ls)
      (cadr ls)))                      
(define un-parse-seq (lambda (ls)
        (map (lambda (x) (unparse x)) (cadr ls))))               

(define const? (lambda (ls)
    (and (list? ls) (= (list-length ls 0) 2)
       (equal? (car ls) 'const))))
(define seq? (lambda (ls)
          (and (list? ls) (= (list-length ls 0) 2)
       (equal? (car ls) 'seq) 
          (list? (cadr ls))     )))
(define define? (lambda (ls)
   (and (list? ls) (= (list-length ls 0) 3) 
      (equal? (car ls) 'define )
        (var? (cadr ls)))))
(define unparse-define (lambda (ls)
    (append `(define) (un-seq (cdr ls)))))                     
(define unparse (lambda (ls)
    (if (seq? ls) (un-parse-seq ls )
     (if (or (const? ls) (var? ls)) (unparse-constant ls)
      (if (lambda-simple? ls) (unparse-simple (cddr ls) `(lambda ,(cadr ls)))
       (if (if3? ls) (unparse-if3 ls '())
       (if (define? ls) (unparse-define ls) 
        (if (app? ls) (unparse-app ls)   ))))))))   
(define is-void? (lambda (ls)
    (and (const? ls) (symbol? (cadr ls)) 
         (equal? (symbol->string (cadr ls)) "#<void>")))) 
                                           
(define un-seq (lambda (ls)
      (map (lambda (x) (unparse x)) ls)))   
(define if3? (lambda (ls)
   (and (list? ls) 
      (equal? (car ls) 'if3))))
(define unparse-if3 (lambda (ls curr)
  (if  (null? ls) curr
  (if (is-void? (car ls)) curr
   (if (equal? (car ls) 'if3) (unparse-if3 (cdr ls) 
                                (append curr `(if)))
     (unparse-if3 (cdr ls) (append curr `(,(unparse (car ls)))
                             )))))))  
    

(define unparse-simple (lambda (ls curr)
     (if (null? ls) curr
      (if (seq? (car ls))
        (unparse-simple (cdr ls) (append curr (unparse (car ls))))
       (unparse-simple (cdr ls) 
         (append curr (cons (unparse (car ls)) '())))))))   

(define app? (lambda (ls)
    (and (list? ls) (> (list-length ls 0) 1) 
         (equal? (car ls) 'applic))))
(define unparse-app (lambda (ls)
 (append `() `(,(unparse (cadr ls))) (un-seq (caddr ls)))))                     
(define var? (lambda (ls)
      (and (list? ls) (= (list-length ls 0) 2) 
           (equal? (car ls) 'var))))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;sdfdsf
(define is-lambda-simple? (lambda(exp)
            (and (list? (car exp))(> (list-length (car exp) 0) 2)  (symbol? (caar exp)) 
                 (equal? (symbol->string (caar exp)) "lambda-simple") 
                 (list? (cadar exp)))))       
(define is-boolean? (lambda (ls)
     (boolean? (car ls))))                
(define is-variable? (lambda (ls)
    (and (list? (car ls)) (= (list-length (car ls) 0) 2)
         (equal? (caar ls) 'var))))

(define is-const? (lambda (ls)
    (and  (list? (car ls)) (= (list-length (car ls) 0) 2)
       (equal? (caar ls) 'const))))  
    
(define is-seq? (lambda (ls)
      (and (list? (car ls)) (not (null? (car ls)))
           (equal? (caar ls) 'seq))))

(define do-seq (lambda (ls env)
    (map (lambda (x)
         (pe-help (cons x env))) ls)))   


(define myseq2 (lambda (ls env )
    
     (if (= (list-length ls 0) 1)
       (cons  (pe-help (cons (car ls) env)) '())
      (cons (append '(seq) 
        (cons (map (lambda(x) 
      (pe-help (cons x env))) ls) '())) '()))))



(define is-if? (lambda (ls)
     (and (list? (car ls)) (not (null? (car ls)))
       (equal? (caar ls) 'if3))))   

(define is-application? (lambda (ls)   
 (and (list? (car ls)) (not (null? (car ls)))
       (equal? (caar ls) 'applic))))   

(define is-tc-applic? (lambda (ls)   
 (and (list? (car ls)) (not (null? (car ls)))
       (equal? (caar ls) 'tc-applic))))   

(define is-define? (lambda (ls)
    (and (list? (car ls)) (not (null? (car ls)))
       (equal? (caar ls) 'define))))   


(define is-lambda-variadic? (lambda (ls)
     (and (list? (car ls)) (not (null? (car ls)))
       (equal? (caar ls) 'lambda-variadic))))   

(define is-lambda-opt? (lambda (ls)
     (and (list? (car ls)) (not (null? (car ls)))
       (equal? (caar ls) 'lambda-opt))))   


(define ispvar? (lambda (ls)
  (and (list? (car ls)) (not (null? (car ls)))
     (equal? (caar ls) 'pvar))))  

(define isfvar? (lambda (ls)
  (and (list? (car ls)) (not (null? (car ls)))
     (equal? (caar ls) 'fvar))))

(define isbvar? (lambda (ls)
  (and (list? (car ls)) (not (null? (car ls)))
     (equal? (caar ls) 'bvar))))  

(define is-bpfvar? (lambda (ls)
   (or (ispvar? ls) (isbvar? ls) (isfvar? ls))))                  

(define last (lambda (ls)
    (if (null? (cdr ls)) (car ls)
      (last (cdr ls)))))  
(define do-seq-tc (lambda (ls tc)
    (map (lambda (x)
         (tc-help (cons x tc))) ls)))   

(define do-seq-s (lambda (ls tc curr)
    
      (if (null? (cdr ls)) 
        (append curr (cons (tc-help (cons (car ls) #t)) '()))  
     (do-seq-s  (cdr ls) tc   
       (append curr (cons (tc-help (cons (car ls) #f)) '()))))))  



 (define pe-help 
  (let ((run
	 (compose-patterns
	  (pattern-rule
	   (?  'c is-define?)
	   (lambda (c) (append `(define) 
                  (do-seq  (cdar c) (cdr c)))))
	 
    
          (pattern-rule
	   (?  'c is-const?)
	   (lambda (c) (car c)))
	 
    
    (pattern-rule
	   (?  'c is-application?)
	   (lambda (c) 
          `(applic ,(pe-help (cons (cadar c) (cdr c))) ,(do-seq 
                (caddar c) (cdr c)))))   
	                      
           (pattern-rule
	   (?  'c is-tc-applic?)
	   (lambda (c) 
          `(tc-applic ,(pe-help (cons (cadar c) (cdr c))) ,(do-seq 
                (caddar c) (cdr c)))))   
	
 
    (pattern-rule
	   (?  'c is-if?)
	   (lambda (c) (append `(if3) (do-seq (cdar c) (cdr c)))))
	 
    
             (pattern-rule
	   (?  'c is-seq?)
	   (lambda (c) `(seq ,(do-seq (cadar c) (cdr c)))))
	 
    ;;;;;;;;;;;;;;;;;;   
    
    (pattern-rule
	   (? 'c is-variable?) 
	   (lambda (c) (if  (= (pinor (unparse (car c)) (cdr c) 0) -1)
              `(fvar ,(unparse (car c)))
            (if (= (pinor (unparse (car c)) (cdr c) 0) 0)
              `(pvar ,(unparse (car c)) ,(minor (unparse (car c)) 
                                 (get-list-index (cdr c) 0) 0))
              `(bvar ,(unparse (car c)) ,(- (pinor (unparse (car c)) (cdr c) 0) 1) 
                 ,(minor (unparse (car c)) 
         (get-list-index (cdr c) (pinor (unparse (car c)) (cdr c) 0)) 0))))))   
                           
            (pattern-rule
	   (? 'c is-lambda-simple?)
	   (lambda (c ) 
      (append `(lambda-simple ,(cadar c)) 
                  (do-seq  
                    (cddar c) (cons (cadar c) (cdr c))))))                      
	    	 
            (pattern-rule
	   (? 'c is-lambda-variadic?)
	   (lambda (c ) 
      (append `(lambda-variadic ,(cadar c)) 
                  (do-seq  
                    (cddar c) (cons (cadar c) (cdr c))))))                      


    
    (pattern-rule
	   (? 'c is-lambda-opt?)
	   (lambda (c ) 
      (append `(lambda-opt ,(cadar c) ,(caddar c)) 
                  (do-seq  
                    (cdddar c) (cons (append (cadar c) 
                                      (cons  (caddar c) '())) 
                                 (cdr c))))))                      


	  ;; add more rules here
	  )))
    (lambda (e )
      (run e 
	   (lambda ()
	     (error 'parse
    (format "I can't recognize this: ~s" e)))))))


(define  pe->lex-pe (lambda (exp)
      (pe-help (cons exp '()))))                

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   

(define tc-help 
  (let ((run
	 (compose-patterns
	  (pattern-rule
	   (?  'c is-define?)
	   (lambda (c) (append `(define) 
                  (do-seq-tc  (cdar c) (cdr c)))))
	 
    
          (pattern-rule
	   (?  'c is-const?)
	   (lambda (c) (car c)))
	 (pattern-rule
	   (?  'c is-application?)
	   (lambda (c) 
          (if (equal? (cdr c) #t) 
          `(tc-applic ,(tc-help (cons (cadar c) #f)) ,(do-seq-tc 
                (caddar c) #f))
            `(applic ,(tc-help (cons (cadar c) #f)) ,(do-seq-tc 
             (caddar c) #f)))))
                       
    (pattern-rule
	   (?  'c is-if?)
	    
            (lambda (c) 
              
              (append `(if3 ,(cadar c)) (do-seq-tc (cddar c) (cdr c)))))
                  
	 
    
             (pattern-rule
	   (?  'c is-seq?)
	   (lambda (c) 
           `(seq ,(do-seq-s (cadar c) (cdr c) '() ))))
	 
    ;;;;;;;;;;;;;;;;;;   
    (pattern-rule
	   (? 'c is-bpfvar?) 
	   (lambda (c) 
             (car c)))
    
    (pattern-rule
	   (? 'c is-variable?) 
	   (lambda (c) 
             (car c)))
                           
            (pattern-rule
	   (? 'c is-lambda-simple?)
	   (lambda (c ) 
      (append `(lambda-simple ,(cadar c)) 
                  (do-seq-tc  
                    (cddar c) #t))))                      
	    	 
             (pattern-rule
	   (? 'c is-lambda-variadic?)
	   (lambda (c ) 
      (append `(lambda-variadic ,(cadar c) ) 
                  (do-seq-tc  
                    (cddar c) #t))))                      

  	 
                    (pattern-rule
	   (? 'c is-lambda-opt?)
	   (lambda (c ) 
      (append `(lambda-opt ,(cadar c) ,(caddar c) ) 
                  (do-seq-tc  
                    (cdddar c) #t))))                      
	 


	  ;; add more rules here
	  )))
    (lambda (e )
      (run e 
	   (lambda ()
	     (error 'parse
    (format "I can't recognize this: ~s" e)))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;

(define annotate-tc (lambda (exp)
   (tc-help (cons exp #f)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ^^label
(lambda (name)
(let ((n 0))
(lambda ()
(set! n (+ n 1))
(string-append name 
(number->string n))))))
(define ^label-not-bool (^^label "NOTBOOL"))
(define ^label-if3else (^^label "Lif3else"))
(define ^label-if3exit (^^label "Lif3exit"))
(define nl "\n")
(define ^lambda-label (^^label "lambdacl"))
(define ^lambda-exit (^^label "lambdaexit"))
(define ^zero (^^label "EXPANDZERO"))
(define ^makeclone (^^label "MAKECLONE"))
(define ^applic-return (^^label "appret"))
(define ^exl (^^label "EXLOOP"))
(define ^onecl (^^label "EXLOOPDONE"))
(define ^endinnercl (^^label "ENDINNERCL"))
(define ^loopin (^^label "ONELOOP"))
(define ^loopr15 (^^label "LOOPR15"))
(define ^loopendr15 (^^label "LOOPENDR15"))
(define ^looptail (^^label "LOOPTAIL"))
(define ^endtail (^^label "ENDTAIL"))
(define ^oneld (^^label "ONELOOPDONE"))
(define ^endex (^^label  "DONEEX"))
(define ^envloop (^^label "CHECKLOOP"))
(define ^envend  (^^label "CHECKEND"))
(define expandclosure (lambda ()
    (let ((zero (^zero)))
     (let ((one (^onecl)))
     (let ((exl (^exl)))
      (let ((onel (^loopin))) 
       (let ((onedone (^oneld)))
        (let ((done (^endex))) 
         (let ((envc (^envloop)))
          (let ((envend (^envend))) 
          (string-append   
       "MOV(R15,FPARG(0));"
       "CMP(R15,IMM(0));" nl 
       "JUMP_EQ(" zero ");" nl
         "CMP(R15,5);" nl
      "JUMP_EQ(" one ");" nl
        
      "MOV(R9,IMM(0));"
       envc ":" nl
        "CMP(INDD(R15,R9),0);" nl
         "JUMP_EQ(" envend ");" nl
          "ADD(R9,IMM(1));" nl
           "JUMP(" envc ");" nl
            envend ":" nl
            
            "PUSH(R9);" nl      
            nl "CALL(MALLOC);" nl 
     
            "DROP(1);" nl
      "MOV(R1,R0);"  nl
       "MOV(R2,IMM(0));" nl
     "MOV(R3,IMM(1));" nl
     "MOV(R4,FPARG(0));" nl
      exl ":" nl  
      "CMP(R3,R9);" nl
      "JUMP_EQ(" one  ");" nl
      "MOV(INDD(R1,R3),INDD(R4,R2));" nl
     "
ADD(R3,IMM(1));" nl
     "
ADD(R2,IMM(1));" nl
     "JUMP(" exl ");" nl
      one ":" nl
  "MOV(R3,FPARG(1));" nl
     "PUSH(R3);" nl
     "
CALL(MALLOC);" nl
     "
DROP(1);" nl
     "
MOV(R2,IMM(2));" nl
     "
ADD(R3,IMM(2));" nl
     "
MOV(R4,IMM(0));" nl
  
   onel ":"   nl   "CMP(R2,R3);" nl
      "JUMP_EQ (" onedone ");" nl
  " MOV(INDD(R0,R4),FPARG(R2));" nl
     "
ADD(R2,IMM(1));" nl
     "
ADD(R4,IMM(1));" nl
     "JUMP(" onel ");" nl
      onedone ":" nl
      "MOV(INDD(R1,0),R0);" nl
         "
MOV(R0,R1);" nl
        "JUMP(" done ");" nl
         zero ":" nl
      
    
     " MOV(R0,5);
" nl
           
      done ":" nl )))))))))))
           
      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;      


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;      





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define list->cisc (lambda (exp ls lsf curr)
      (if (null? exp) curr
        (if (not (pair? exp))
            (string-append curr "PUSH(R0);" nl (car (code-gen-help (pe->lex-pe (parse exp)) ls lsf)) nl "PUSH(R0);" nl
              "CALL(MAKE_SOB_PAIR);" nl "DROP(2);" nl )
         (if (and (pair? exp) (not (pair? (car exp)))) 
            (list->cisc (cdr exp) ls lsf (string-append curr "PUSH(R0);" nl  (car (code-gen-help (pe->lex-pe (parse (car exp))) ls lsf)) nl "PUSH(R0);" nl
              "CALL(MAKE_SOB_PAIR);" nl "DROP(2);" nl ))
             
        (if (and (pair? exp) (pair? (car exp)) (not (number? (caar ls))) (not (char? (caar ls))) (not (boolean? (caar ls)))
                (not (string? (caar ls))))  
           (list->cisc (cdr exp) ls lsf (string-append curr "PUSH(R0);" nl  (car (code-gen-help  (car exp) ls lsf)) nl "PUSH(R0);" nl
              "CALL(MAKE_SOB_PAIR);" nl "DROP(2);" nl ))
             
             
             ))))))

(define string-length (lambda (exp)
     (list-length (string->list exp) 0)))                  
(define code-gen-const (lambda (e ls lsf)
   (with e
     (lambda (const val)
      
     (if (and (symbol? val) (equal? (symbol->string val) "#<void>")) 
        (list (string-append "MOV(R0,6);" nl) ls) 
      (if  (symbol? val)   
       (list (string-append "MOV(R0," (number->string (findadd val ls))
                              ");" nl) ls)      
         
          (if (boolean? val) 
          (if (equal? val #t)
          (list (string-append "MOV(R0,1);" nl) ls)
          (list (string-append "MOV(R0,3);" nl) ls))    
         (if (and (number? val) 
                  (equal? (oneorzero ls val) 1)  ) 
                 
         (list (string-append "PUSH(IMM(" (number->string val) "));" nl
                         "CALL(MAKE_SOB_INTEGER);" 
           nl "DROP(1);" nl) (makeitzero ls ls val))
          (if (null? val)
              (list (string-append "MOV(R0,5);" nl) ls)      
           (if (number? val)
         (list (string-append "MOV(R0," (number->string (findadd val ls))
                              ");" nl) ls)      
          
         (if (pair? val)
               (list (list->cisc (cdr (switchlist val '())) ls lsf (string-append "PUSH(5);" nl 
 (car (code-gen-help (parse (car  (switchlist val '()))) ls lsf)) "PUSH(R0);" nl
           "CALL(MAKE_SOB_PAIR);" nl "DROP(2);" nl)) ls )                              
                 
           
         (if (and (char? val) 
                  (equal? (oneorzero ls val) 1)  ) 
         
         (list   (string-append 
              "PUSH("   (number->string (char->integer val))
             ");" nl "CALL(MAKE_SOB_CHAR);" nl "DROP(1);" nl) ls) 
          (if (char? val) 
            (list (string-append "MOV(R0," (number->string (findadd val ls))
                              ");" nl) ls)      
         
            (if (string? val)
                   (list (string-append "MOV(R0," (number->string (findadd val ls))
                              ");" nl) ls)      
         
           
                       
                                              
             
             
             ))))))))))))))

(define numberorcons (lambda (val)
     (or (number? val) (char? val) (boolean? val) (string? val) (void? val) (null? val))))                  

                       
  (define const->string (lambda (var ls)
       (if (numberorcons var) 
           (code-gen-const (parse var) ls))))
         
               
  


                         
(define addconst (lambda (exp ls )
   (if (equal? (findadd exp ls) #f) 
   (append ls 
     (cons (list exp (findnextadress ls)) '()))
       #f ))) 
               
(define switchlist (lambda (ls curr)
    (if (null? ls) curr                
     (switchlist (cdr ls) (cons (car ls) curr)))))



(define findadd (lambda (const ls )
          (if (null? ls) #f
          (if (equal? const (caar ls)) (cadar ls)
            (findadd const (cdr ls))))))  
    

(define code-gen (lambda (exp)
      (code-gen-help exp 
       (count exp '((#t 1) (#f 3) (() 5) (\x23;<void> 6)  ))
     
     (setadd exp 
       (findnextadress (count exp '((#t 1) (#f 3) (() 5) (\x23;<void> 6)  )))) 
        ))) 


(define printfvar (lambda (ls curr)
    (if (null? ls)  curr
     (printfvar (cdr ls)
     (string-append curr "PUSH(1);" nl
      "CALL(MALLOC);" nl "DROP(1);" nl))))) 



       

(define countfvar (lambda (exp ls )
      (if (null? exp) ls
      (if (and (fvar? exp)
            (equal? #f (findadd (cadr exp) ls)))
       (append ls 
      (cons (list (cadr exp) 0) '()))    
          
      (if (and (fvar? (car exp) )
            (equal? #f (findadd (cadar exp) ls)))
             (countfvar (cdr exp) (append ls 
      (cons (list (cadar exp) 0) '())))
        (if (list? (car exp))
         (countfvar (cdr exp) (countfvar (car exp) ls))
         (countfvar (cdr exp) ls)))))))   


(define setadd (lambda (exp start)
   (initfvar '() (map car (cdr (countfvar  exp '((% 0)))) ) 
     start))) 


(define initfvar (lambda (start ls add)
  (if (null? ls) start
   (initfvar (append start 
      (cons (list (car ls)  add) '())) (cdr ls) (+ 1 add)))))                   
                   

(define pvar? (lambda (exp)
    (and (list? exp) (equal? (list-length exp 0) 3) 
         (equal? (car exp) 'pvar))))

(define bvar? (lambda (exp)
    (and (list? exp) (equal? (list-length exp 0) 4) 
         (equal? (car exp) 'bvar))))

(define code-gen-bvar (lambda (exp ls)
  (let ((index (caddr exp)))
   (let ((min (cadddr exp)))
    (list (string-append "MOV(R0,FPARG(0));" nl
         "MOV(R0,INDD(R0," (number->string index) "));" nl
          "MOV(R0,INDD(R0," (number->string min) "));" nl) ls)))))  
                        

                             

       
(define code-gen-pvar (lambda (exp ls)
     (let ((min (+ 2 (caddr exp))))
      (list (string-append "MOV(R0,FPARG (" (number->string min)
              "));" nl) ls))))



                        




           
(define countlist2 (lambda (ls curr)
  (if (null? ls) curr
    (countlist2 (cdr ls) (+ curr 1)))))  



                     
(define closure? (lambda (exp)
       (and (list? exp) (not (null? exp)) (equal? (car exp) 'closure))))            
                              

(define countbefore (lambda (exp ls)
      (if (null? exp) ls 
        (countbefore (cdr exp) (count (parse (car exp)) ls)))))  
                  
 (define countlist (lambda (exp ls)
       (if (null? exp) ls
        (if (and (pair? exp) (not (pair? (cdr exp))))
     (count (parse (cdr exp)) (count (parse (car exp)) (append ls 
      (cons (list  exp (findnextadress ls) 1) '())))) 
   
           (if (pair? (car exp)) 
         (countlist (cdr exp) 
       (count (parse (cdar exp)) (count (parse (caar exp)) (append ls 
      (cons (list (car exp) (findnextadress ls) 1) '()) 
         (cons (list exp (+ (findnextadress ls) 3) 1) '())
        ))))    
       
     (countlist (cdr exp) (count (parse (car exp)) (append ls 
      (cons (list exp (findnextadress ls) 1) '())))))))))  
              

(define printsconsts (lambda (ls curr)
      (if (null? ls) curr
      (if (char? (caar ls))
      (printsconsts (cdr ls) (string-append curr "PUSH(IMM(" (number->string (char->integer (caar ls))) "));" nl
            "CALL(MAKE_SOB_CHAR);" nl "DROP(1);" nl ))
           
       (if (number? (caar ls))
         (printsconsts (cdr ls) (string-append curr "PUSH(IMM(" (number->string (caar ls)) "));" nl
            "CALL(MAKE_SOB_INTEGER);" nl "DROP(1);" nl ))
        (if (and (boolean? (caar ls)) (equal? (caar ls) #t))
           (printsconsts (cdr ls)  (string-append curr "PUSH(1);" nl "CALL(MAKE_SOB_BOOL);" nl "DROP(1);" nl))
            
        (if (and (boolean? (caar ls)) (equal? (caar ls) #f))
           (printsconsts (cdr ls) (string-append curr "PUSH(0);" nl "CALL(MAKE_SOB_BOOL);" nl "DROP(1);" nl))
         (if (null? (caar ls))
             (printsconsts (cdr ls) (string-append curr "PUSH(MAKE_SOB_NIL);" nl))))))))))
                
            
                       
     
  (define count (lambda (exp ls )
      (if (null? exp) ls
      (if (and (const? exp) (symbol? (cadr exp)) 
           (equal? #f (findadd (symbol->string (cadr exp)) ls)))
        (count (cdr exp) (append (append ls 
       (cons (list (symbol->string (cadr exp)) 
               (findnextadress ls) 0) '())) 
        (cons (list (cadr exp) (findnextadress (append ls 
       (cons (list (symbol->string (cadr exp)) 
               (findnextadress ls) 0) '()))
                                 ) 0) '())))                   
      
      (if (and (const? exp) (pair? (cadr exp)))
        (countlist (cadr exp) (countbefore (cadr exp) ls)) 
          
       (if (and (const? exp) (equal? #f (findadd (cadr exp) ls))) 
          (append ls 
      (cons (list (cadr exp) (findnextadress ls) 0) '()))
       (if (and (list? exp) (list? (car exp)) 
               (const? (car exp)) 
               (equal? #f (findadd (cadar exp) ls)))
        (count (cdr exp) (count (car exp) (countbefore (car exp) ls)))
       (if  (list? (car exp)) 
                 
                (count (cdr exp) (count (car exp) ls))   
          
          (count (cdr exp) ls)))))))))        


;;;;;;;;;;;;;;;;;;;;;;;;;
                  
(define code-gen-define (lambda (exp ls lsf)
 
  (if (equal? (cadadr exp) (car (car lsf)))
     (list (string-append (car (code-gen-help (caddr exp) ls lsf))
      nl  "MOV(R1,R0);" nl
       "MOV(R0," (number->string (cadar lsf)) ");" nl
        "MOV(INDD(R0,0),R1);" nl) ls lsf)
    (code-gen-define exp ls (cdr lsf)))))   

(define countfvar (lambda (exp ls )
      (if (null? exp) ls
      (if (and (fvar? exp)
            (equal? #f (findadd (cadr exp) ls)))
       (append ls 
      (cons (list (cadr exp) 0) '()))    
          
      (if (and (fvar? (car exp) )
            (equal? #f (findadd (cadar exp) ls)))
             (countfvar (cdr exp) (append ls 
      (cons (list (cadar exp) 0) '())))
        (if (list? (car exp))
         (countfvar (cdr exp) (countfvar (car exp) ls))
         (countfvar (cdr exp) ls)))))))   

                      
(define nextfvar (lambda (ls)
    (if (null? (cdr ls))
       (+ 3 (cadar ls))
      (nextfvar (cdr ls)))))  
           
          
          
          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define makeitzero (lambda (ls temp val)
    (if (and (equal? (caar temp) val) (not (null? (cdr temp))))
     (append (untilthen ls val '()) 
       (cons (list (caar temp) (cadar temp) 0) '())
        (cdr temp) )
    (if (and (equal? (caar temp) val)  (null? (cdr temp)))
     (append (untilthen ls val '()) 
       (cons (list (caar temp) (cadar temp) 0) '())
         )
 
        (makeitzero ls (cdr temp) val)))))  
 



(define oneorzero (lambda ( ls val)
  (if (null? ls) #f
    (if (equal? (caar ls) val)
      (caddar ls)
     (oneorzero (cdr ls) val))))) 

 (define untilthen (lambda (ls val curr)
        (if (equal? (caar ls) val) curr
          (untilthen (cdr ls) val 
            (append curr (cons (car ls) '()))))))  

(define compile-scheme-file (lambda (input-file output-file)
	(let ((input (file->sexprs input-file))
		  (output  output-file))
	  (display (string-append	(prologue) 
		(car (code-gen (pe->lex-pe (parse input))))
		(epilogue) output))
	   output)))

(define prologue 
	(lambda()
		(string-append	"#include <stdio.h>" nl 
		"#include <stdlib.h>" nl   
	     "#include <string.h>" nl
	"#define DO_SHOW 1" nl 
	"#include \"cisc.h\"" nl nl
         "int main()" nl
	"{" nl 
  "START_MACHINE;" nl
      
	 
	 "JUMP(CONTINUE);" nl nl 
	 "#include \"char.lib\"" nl
	 "#include \"io.lib\"" nl 
	 "#include \"math.lib\""   nl 
     "#include \"string.lib\"" nl
"#include \"system.lib\"" nl  
 "#include \"scheme.lib\"" nl nl						
	 
 "CONTINUE:" nl nl )))


(define epilogue
	(lambda ()
		(string-append  nl 	"END_PROGRAM:" nl nl
                  
		
		 
		 "STOP_MACHINE;" nl nl
		 "return 0;" nl
		"}")))      
(define file->sexprs
  (lambda (filename)
    (let ((input (open-input-file filename)))
      (letrec ((run
                (lambda ()
                  (let ((e (read input)))
                    (if (eof-object? e)
                        (begin (close-input-port input)
                               '())
                        (cons e (run)))))))
        (run)))))





(define fvar? (lambda (exp)
      (and (list? exp) (equal? (list-length exp 0) 2)
         (equal? (car exp) 'fvar))))  


;;;;;;;;;;;;;;;;;;;;;



(define code-gen-help (lambda ( exp ls lsf )
         
        (if (and (list? exp) (not (null? exp))
              (equal? (car exp) 'define))
          (code-gen-define exp ls lsf)  
         (if (fvar? exp)
           (code-gen-fvar exp lsf) 
        (if  (const? exp)
              (code-gen-const exp ls lsf  ) 
        (if (equal?  (car exp) 'tc-applic)
           (code-gen-applic-tail exp ls lsf "") 
        (if (equal?  (car exp) 'applic)
          (code-gen-applic exp ls lsf "") 
        (if (bvar? exp)
          (code-gen-bvar exp ls)  
        (if (pvar? exp)
          (code-gen-pvar exp ls)  
         (if (if3? exp)
           (code-gen-if3 exp ls lsf)
         (if (lambda-simple? exp)
           (code-gen-lambda exp ls lsf )  
        (if (and (list? exp) (not (null? exp)) 
                 (equal? (car exp) 'seq))
            (code-gen-seq (cadr exp) ls lsf "" )            
            
            
            ))))))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-if3
(lambda (e ls lsf)
 (let ((test (code-gen-help (cadr e) ls lsf)))
  (let ((do-else (^label-if3else)))
  (let ((if-exit (^label-if3exit)))  
 (list (string-append (car test) nl
   "CMP (R0,3);" nl "JUMP_EQ(" do-else ");" nl
   (car (code-gen-help (caddr e) ls lsf))
   "JUMP("  if-exit  ");" nl
   do-else ":" nl
    (car (code-gen-help (cadddr e) ls lsf))   nl 
   if-exit ":" nl) ls lsf))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     (define findnextadress (lambda (ls)
     (if (null? (cdr ls))
       (if (string? (caar ls)) 
           (+ 2 (string-length (caar ls))  (cadar ls) )
        
      (if (closure? (caar ls)) (+ 3 (cadar ls))
       (if (pair? (caar ls))
          (+ 3 (cadar ls)) 
       (if (or (boolean? (caar ls)) (number? (caar ls)) 
               (char? (caar
                        ls))) (+ 2 (cadar ls))
        (if (or (null? (caar ls)) 
             (and   (symbol? (caar ls))     
               (equal? (symbol->string (caar ls)) "#<void>"))
               
                )
                (+ (cadar ls) 1)
           (if  (symbol? (caar ls))
              (+ 2 (cadar ls)) 
            
            ))))))
        (findnextadress (cdr ls))))) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-lambda  (lambda (exp ls lsf)
  (with exp
     (lambda (lambda  par body)
        
       (let ((bodystring (code-gen-help body ls lsf)))
        (let ((label-lambda (^lambda-label)))
         (let ((label-exit (^lambda-exit)))
            
        (list (string-append    nl
        (expandclosure)  "PUSH( LABEL (" label-lambda "));" nl
          "PUSH(R0);" nl "CALL(MAKE_SOB_CLOSURE);" nl
           "DROP(2);" nl      
             "JUMP(" label-exit ");" nl 
              label-lambda ":"  nl
                (car bodystring)  nl "POP(FP);" nl "RETURN;" nl    
               label-exit ":" nl) ls lsf))))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define code-gen-seq (lambda (exp ls lsf curr)
        (if (null? exp) (list curr ls lsf)
       (code-gen-seq (cdr exp) ls lsf
       (string-append curr 
         (car (code-gen-help (car exp) ls lsf)))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ^dontail (^^label "DONTTAIL"))
(define ^dontret (^^label "DONTRET"))

(define pusha (lambda ()
   (string-append  nl "PUSH(R1);" nl "PUSH(R2);" nl "PUSH(R3);" nl)))             
(define popa (lambda ()
           (string-append "POP(R3);" nl "POP(R2);" nl "POP(R1);" nl )))     

(define code-gen-applic-tail (lambda (exp ls lsf curr)
  (let ((lp (^looptail)))
   (let ((ep (^endtail))) 
   (let ((dont (^dontail)))
    (let ((dontret (^dontret))) 
  ;;;;;;;;;; 
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     
    
      (if (lambda-simple? (cadr exp))
     (let ((numpar (countlist2 (caddr exp) 0)))
    (list (string-append  (car (code-gen-lambda-tail (cadr exp) ls lsf)) nl
     "MOV(R1,R0);" nl 
                nl
         "MOV(R10,FPARG(1));" nl "MOV(R6,IMM(1));"
                  
             "ADD(R6,R10);" nl "MOV(R5,"  (number->string numpar) ");" 
      nl "ADD(R5,IMM(4));" nl "MOV(R7,IMM(-3));" nl       nl
     nl "ADD(R10," (number->string numpar) ");" nl
     "ADD(R10,IMM(8));" nl
      "SUB(R10," (number->string numpar) ");" nl
        "SUB(R10,4);" nl    
     (seqpar (switchlist (caddr exp) '()) ls lsf curr)
            "PUSH(" (number->string numpar)  ");"  nl
        "PUSH(INDD(R1,1));" nl 
        "CMP(FPARG(0),0);" nl
          "JUMP_EQ(" dont ");" nl  
            
            "PUSH(FPARG(-1));" nl 
       "PUSH(FPARG(-2));" nl      
       nl lP ":" nl
        "CMP(R5,IMM(0));" nl "JUMP_EQ(" ep ");" nl
       "MOV(FPARG(R6),FPARG(R7));" nl
        "SUB(R6,IMM(1));" nl 
        "SUB(R7,IMM(1));" nl
         "SUB(R5,IMM(1));" nl
         "JUMP(" lp ");" nl 
            ep ":" nl
          
            "DROP(R10);" nl  
           "MOV(FP,SP);" nl            
            
            
            
       "ADD(R13,IMM(1));" nl "ADD(R15,IMM(1));" nl
            "JUMPA(INDD(R1,2));" nl
           
           dont ":"
           "PUSH( LABEL(" dontret "));" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl
            "JUMPA(INDD(R1,2));" nl
            dontret ":" nl
                        
           
            ) ls))

    
    
    
     (if (equal?  (caadr exp) 'lambda-opt)
     (let ((numpar (countlist2 (caddr exp) 0)))
    (list (string-append (car (code-gen-lambda-opt (cadr exp) ls lsf)) nl
     "MOV(R1,R0);" nl           nl
         "MOV(R10,FPARG(1));" nl "MOV(R6,IMM(1));"
                  
             "ADD(R6,R10);" nl "MOV(R5,"  (number->string numpar) ");" 
      nl "ADD(R5,IMM(4));" nl "MOV(R7,IMM(-3));" nl       nl
     nl "ADD(R10," (number->string numpar) ");" nl
     "ADD(R10,IMM(8));" nl
      "SUB(R10," (number->string numpar) ");" nl
        "SUB(R10,4);" nl    
     (seqpar (switchlist (caddr exp) '()) ls lsf curr)
            "PUSH(" (number->string numpar)  ");"  nl
      "PUSH(INDD(R1,1));" nl 
         "CMP(FPARG(0),0);" nl
          "JUMP_EQ(" dont ");" nl  
      
        "PUSH(FPARG(-1));" nl 
       "PUSH(FPARG(-2));" nl      
       nl lP ":" nl
        "CMP(R5,IMM(0));" nl "JUMP_EQ(" ep ");" nl
       "MOV(FPARG(R6),FPARG(R7));" nl
        "SUB(R6,IMM(1));" nl 
        "SUB(R7,IMM(1));" nl
         "SUB(R5,IMM(1));" nl
         "JUMP(" lp ");" nl 
            ep ":" nl
          
            "DROP(R10);" nl  
           "MOV(FP,SP);" nl            
            
            
            
       "ADD(R13,IMM(1));" nl "ADD(R15,IMM(1));" nl
            "JUMPA(INDD(R1,2));" nl
           
            dont ":"
           "PUSH( LABEL(" dontret "));" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl
            "JUMPA(INDD(R1,2));" nl
            dontret ":" nl
     
           
            ) ls))  
     
   
     
     
     
     
     ;;;;;;;;;;;;;;;;;
   (if (equal?  (caadr exp) 'lambda-variadic)
     (let ((numpar (countlist2 (caddr exp) 0)))
    (list (string-append (car (code-gen-lambda-var (cadr exp) ls lsf)) nl
     "MOV(R1,R0);" nl           nl
         "MOV(R10,FPARG(1));" nl "MOV(R6,IMM(1));"
                  
             "ADD(R6,R10);" nl "MOV(R5,"  (number->string numpar) ");" 
      nl "ADD(R5,IMM(4));" nl "MOV(R7,IMM(-3));" nl       nl
     nl "ADD(R10," (number->string numpar) ");" nl
     "ADD(R10,IMM(8));" nl
      "SUB(R10," (number->string numpar) ");" nl
        "SUB(R10,4);" nl    
     (seqpar (switchlist (caddr exp) '()) ls lsf curr)
            "PUSH(" (number->string numpar)  ");"  nl
      "PUSH(INDD(R1,1));" nl 
       "CMP(FPARG(0),0);" nl
      "JUMP_EQ(" dont ");" nl  
             
            
            "PUSH(FPARG(-1));" nl 
       "PUSH(FPARG(-2));" nl      
       nl lP ":" nl
        "CMP(R5,IMM(0));" nl "JUMP_EQ(" ep ");" nl
       "MOV(FPARG(R6),FPARG(R7));" nl
        "SUB(R6,IMM(1));" nl 
        "SUB(R7,IMM(1));" nl
         "SUB(R5,IMM(1));" nl
         "JUMP(" lp ");" nl 
            ep ":" nl
          
            "DROP(R10);" nl  
           "MOV(FP,SP);" nl            
            
            
            
       "ADD(R13,IMM(1));" nl "ADD(R15,IMM(1));" nl
            "JUMPA(INDD(R1,2));" nl
           
             dont ":"
           "PUSH( LABEL(" dontret "));" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl
            "JUMPA(INDD(R1,2));" nl
            dontret ":" nl
     
           
            ) ls))  
     
     
     
     
     
  ;;;;;;;;;;   
     
   
   
     (if (pvar? (cadr exp))
     (let ((numpar (countlist2 (caddr exp) 0)))
    (list (string-append  nl "MOV(R4,IMM(2));" nl
        "ADD(R4," (number->string (caddr (cadr exp))) ");" nl       
    
   "MOV (R1,FPARG(R4));" 
            nl
         "MOV(R10,FPARG(1));" nl "MOV(R6,IMM(1));"
                  
             "ADD(R6,R10);" nl "MOV(R5,"  (number->string numpar) ");" 
      nl "ADD(R5,IMM(4));" nl "MOV(R7,IMM(-3));" nl       nl
     nl "ADD(R10," (number->string numpar) ");" nl
     "ADD(R10,IMM(8));" nl
      "SUB(R10," (number->string numpar) ");" nl
        "SUB(R10,4);" nl    
     (seqpar (switchlist (caddr exp) '()) ls lsf curr)
            "PUSH(" (number->string numpar)  ");"  nl
      "PUSH(INDD(R1,1));" nl
       "CMP(FPARG(0),0);" nl
      "JUMP_EQ(" dont ");" nl  
       
        "PUSH(FPARG(-1));" nl 
       "PUSH(FPARG(-2));" nl      
       nl lP ":" nl
        "CMP(R5,IMM(0));" nl "JUMP_EQ(" ep ");" nl
       "MOV(FPARG(R6),FPARG(R7));" nl
        "SUB(R6,IMM(1));" nl 
        "SUB(R7,IMM(1));" nl
         "SUB(R5,IMM(1));" nl
         "JUMP(" lp ");" nl 
            ep ":" nl
          
            "DROP(R10);" nl  
           "MOV(FP,SP);" nl            
            
            
            
       "ADD(R13,IMM(1));" nl "ADD(R15,IMM(1));" nl
            "JUMPA(INDD(R1,2));" nl
            dont ":"
           "PUSH( LABEL(" dontret "));" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl
            "JUMPA(INDD(R1,2));" nl
            dontret ":" nl
   
           
            ) ls))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
        (if (pvar? (cadr exp))
     (let ((numpar (countlist2 (caddr exp) 0)))
    (list (string-append  nl "MOV(R4,IMM(2));" nl
        "ADD(R4," (number->string (caddr (cadr exp))) ");" nl       
    
   "MOV (R1,FPARG(R4));" 
            nl
         "MOV(R10,FPARG(1));" nl "MOV(R6,IMM(1));"
                  
             "ADD(R6,R10);" nl "MOV(R5,"  (number->string numpar) ");" 
      nl "ADD(R5,IMM(4));" nl "MOV(R7,IMM(-3));" nl       nl
     nl "ADD(R10," (number->string numpar) ");" nl
     "ADD(R10,IMM(8));" nl
      "SUB(R10," (number->string numpar) ");" nl
        "SUB(R10,4);" nl    
     (seqpar (switchlist (caddr exp) '()) ls lsf curr)
            "PUSH(" (number->string numpar)  ");"  nl
      "PUSH(INDD(R1,1));" nl
       "CMP(FPARG(0),0);" nl
      "JUMP_EQ(" dont ");" nl  
       
        "PUSH(FPARG(-1));" nl 
       "PUSH(FPARG(-2));" nl      
       nl lP ":" nl
        "CMP(R5,IMM(0));" nl "JUMP_EQ(" ep ");" nl
       "MOV(FPARG(R6),FPARG(R7));" nl
        "SUB(R6,IMM(1));" nl 
        "SUB(R7,IMM(1));" nl
         "SUB(R5,IMM(1));" nl
         "JUMP(" lp ");" nl 
            ep ":" nl
          
            "DROP(R10);" nl  
           "MOV(FP,SP);" nl            
            
            
            
       "ADD(R13,IMM(1));" nl "ADD(R15,IMM(1));" nl
            "JUMPA(INDD(R1,2));" nl
            dont ":"
           "PUSH( LABEL(" dontret "));" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl
            "JUMPA(INDD(R1,2));" nl
            dontret ":" nl
   
           
            ) ls))

      
         
         

;;;;;;;;;;;;;;;;;;;;;

         (if (fvar? (cadr exp))
     (let ((numpar (countlist2 (caddr exp) 0)))
    (list (string-append  nl "MOV(R4,IMM(2));" nl
       
            
        
            "MOV(R4,IMM(2));" nl        
           
         "MOV(R10,FPARG(1));" nl "MOV(R6,IMM(1));"
                  
             "ADD(R6,R10);" nl "MOV(R5,"  (number->string numpar) ");" 
      nl "ADD(R5,IMM(4));" nl "MOV(R7,IMM(-3));" nl       nl
     nl "ADD(R10," (number->string numpar) ");" nl
     "ADD(R10,IMM(8));" nl
      "SUB(R10," (number->string numpar) ");" nl
        "SUB(R10,4);" nl    
     (seqpar (switchlist (caddr exp) '()) ls lsf curr)
            "PUSH(" (number->string numpar)  ");"  nl
      (car (code-gen-help (cadr exp) ls lsf)) nl
       "MOV(R1,R0);" nl         
  
       "PUSH(INDD(R1,1));" nl 
      "CMP(FPARG(0),0);" nl
      "JUMP_EQ(" dont ");" nl  
     
        "PUSH(FPARG(-1));" nl 
       "PUSH(FPARG(-2));" nl      
       nl lP ":" nl
        "CMP(R5,IMM(0));" nl "JUMP_EQ(" ep ");" nl
       "MOV(FPARG(R6),FPARG(R7));" nl
        "SUB(R6,IMM(1));" nl 
        "SUB(R7,IMM(1));" nl
         "SUB(R5,IMM(1));" nl
         "JUMP(" lp ");" nl 
            ep ":" nl
          
            "DROP(R10);" nl  
           "MOV(FP,SP);" nl            
            
            
            
       "ADD(R13,IMM(1));" nl "ADD(R15,IMM(1));" nl
              (car (code-gen-help (cadr exp) ls lsf)) nl
              "MOV(R1,R0);" nl         
  
            "JUMPA(INDD(R1,2));" nl
           
          dont ":"
           "PUSH( LABEL(" dontret "));" nl
            "PUSH(FP);" nl
            "MOV(FP,SP);" nl
               (car (code-gen-help (cadr exp) ls lsf)) nl
               "MOV(R1,R0);" nl         
  
            "JUMPA(INDD(R1,2));" nl
            dontret ":" nl

            ) ls))
   
   
   
      ))))))))))))      


    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-applic (lambda (exp ls lsf curr)
  (let ((ret (^applic-return)))
   (let ((numpar (countlist2 (caddr exp) 0))) 

;;;;;;;;;;;;;;;;;;;;;;;;
     
 (if (equal? (caadr exp ) 'lambda-opt)
     (list (string-append    nl 
                 (car (code-gen-lambda-opt (cadr exp) ls lsf)) nl 
        "MOV (R1,R0);"

             (seqpar (switchlist (caddr exp) '()) ls lsf curr) nl
    "PUSH(" (number->string numpar)  ");" nl
     "PUSH(INDD(R1,1));" nl 
     "ADD(R15,IMM(1));" nl
      "PUSH(LABEL(" ret "));" nl 
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl   
     "JUMPA(INDD(R1,2));" nl ret ":" nl "SUB(R15,R13);" nl
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));" 
          "MOV(R8,STARG(0));" nl
           "ADD(R8,IMM(2));" nl
            "DROP(R8);" nl   
    
          
          
             nl) ls)
         
               
     
     
     
     
     
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
    
    
    
    
    
    
    
     ;;;;;;;;;;;;;;;
     
  (if (equal? (caadr exp ) 'lambda-variadic)
     (list (string-append (pusha) (car (code-gen-lambda-var (cadr exp) ls lsf)) nl 
    "MOV (R1,R0);"
      nl (seqpar (switchlist (caddr exp) '()) ls lsf curr) nl
    "PUSH(" (number->string numpar)  ");" nl
     "PUSH(INDD(R1,1));" nl 
     "ADD(R15,IMM(1));" nl
      "PUSH(LABEL(" ret "));" nl 
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl   
     "JUMPA(INDD(R1,2));" nl ret ":" nl "SUB(R15,R13);" nl
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));"  
            "DROP(3);" nl   
            (popa)
           
           
           
             nl) ls)
    
     
     ;;;;;;;;;;;;;;;;;;;;;;;;;
     
     
     
     
     
  (if (lambda-simple? (cadr exp)) 
   (list (string-append (pusha) (car (code-gen-lambda (cadr exp) ls lsf)) nl 
    "MOV (R1,R0);"
      nl (seqpar (switchlist (caddr exp) '()) ls lsf curr) nl
    "PUSH(" (number->string numpar)  ");" nl
    "PUSH(INDD(R1,1));" nl 
     "ADD(R15,IMM(1));" nl
      "PUSH(LABEL(" ret "));" nl 
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl   
     "JUMPA(INDD(R1,2));" nl ret ":" nl "SUB(R15,R13);" nl
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));" 
           "MOV(R8,STARG(0));" nl
           "ADD(R8,IMM(2));" nl
            "DROP(R8);" nl   
           (popa)
        
           nl) ls)
    (if (pvar? (cadr exp))
       (list (string-append  (pusha)  nl        
    
    nl (seqpar (switchlist (caddr exp) '()) ls lsf  curr) nl
    "PUSH(" (number->string numpar)  ");" nl
   "MOV(R4,IMM(2));" nl
        "ADD(R4," (number->string (caddr (cadr exp))) ");" nl
        "MOV (R1,FPARG(R4));"        
     "PUSH(INDD(R1,1));" nl 
     "ADD(R15,IMM(1));" nl
      "PUSH(LABEL(" ret "));" nl 
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl   
     "JUMPA(INDD(R1,2));" nl ret ":" nl "SUB(R15,R13);" nl
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));"    
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));"  
          "MOV(R8,STARG(0));" nl
           "ADD(R8,IMM(2));" nl
            "DROP(R8);" nl   
            (popa)       
              
               nl) ls)
    (if (bvar? (cadr exp))
       (list (string-append (pusha)  nl "MOV(R4,IMM(2));" nl
       
       "MOV(R4,IMM(2));" nl         
  nl (seqpar (switchlist (caddr exp) '()) ls lsf curr) nl
    "PUSH(" (number->string numpar)  ");" nl
     "MOV(R4," (number->string (caddr (cadr exp))) ");" nl       
      "MOV(R5," (number->string (cadddr (cadr exp))) ");" nl
    
     "MOV (R1,FPARG(0));" nl
     "MOV (R1,INDD(R1,R4));" nl
    "MOV (R1,INDD(R1,R5));" nl
    
    "PUSH(INDD(R1,1));" nl 
     "ADD(R15,IMM(1));" nl
      "PUSH(LABEL(" ret "));" nl 
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl   
     "JUMPA(INDD(R1,2));" nl ret ":" nl "SUB(R15,R13);" nl
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));" 
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));"  
          "MOV(R8,STARG(0));" nl
           "ADD(R8,IMM(2));" nl
            "DROP(R8);" nl   
         (popa)
                    
               nl) ls)
                    
 ;;;;;;;;;;;;;;;;;;;;;;;;
        
     (if (fvar? (cadr exp))
       (list (string-append  (pusha) nl "MOV(R4,IMM(2));" nl
                  (car (code-gen-help (cadr exp) ls lsf)) nl
        "MOV(R1,R0);" nl
              
              
            
               "MOV(R4,IMM(2));" nl         
  nl (seqpar (switchlist (caddr exp) '()) ls lsf curr) nl
    "PUSH(" (number->string numpar)  ");" nl
    
    
      "PUSH(INDD(R1,1));" nl 
     "ADD(R15,IMM(1));" nl
      "PUSH(LABEL(" ret "));" nl 
       "PUSH(FP);" nl
       "MOV(FP,SP);" nl   
       
       "JUMPA(INDD(R1,2));" nl ret ":" nl "SUB(R15,R13);" nl
          "MOV(R13,IMM(0));";  nl
         "SUB(R15,IMM(1));"  
          "MOV(R8,STARG(0));" nl
           "ADD(R8,IMM(2));" nl
            "DROP(R8);" nl   
            (popa)     
                 
               nl) ls)
       
        
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
   
 
 
     ))))))))))     


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define seqpar (lambda (ls1 ls2 lsf curr)
    (if (null? ls1)   curr 
     (seqpar (cdr ls1) ls2 lsf 
      (string-append curr (car  (code-gen-help  (car ls1) ls2 lsf))
        "PUSH(R0);" nl)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define code-gen-lambda-tail  (lambda (exp ls lsf)
  (with exp
     (lambda (lambda  par body)
        
       (let ((bodystring (code-gen-help body ls lsf)))
        (let ((label-lambda (^lambda-label)))
         (let ((label-exit (^lambda-exit)))
           
        (list (string-append   
           nl
         
         (expandclosure) "PUSH( LABEL (" label-lambda "));" nl
          "PUSH(R0);" nl "CALL(MAKE_SOB_CLOSURE);" nl
           "DROP(2);" nl      
             "JUMP(" label-exit ");" nl 
              label-lambda ":" nl   nl
                (car bodystring) nl  nl 
                "POP(FP);" nl "RETURN;" nl    
               label-exit ":" nl ) ls lsf)))))))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define ^loopvar (^^label "LOOPVAR"))
(define ^loopvarend (^^label "LOOPVAREND"))
(define ^loopvarnull (^^label "LOOPVARNULL"))
(define ^dovar (^^label "DOVAR"))
(define ^varzero (^^label "VARZERO"))
(define code-gen-lambda-var  (lambda (exp ls lsf)
  (with exp
     (lambda (lambda  z body)
       (let ((zero (^varzero)))
       (let ((dov (^dovar))) 
       (let ((null (^loopvarnull))) 
       (let ((bodystring (code-gen-help body ls lsf)))
        (let ((label-lambda (^lambda-label)))
         (let ((label-exit (^lambda-exit)))
         (let ((varl (^loopvar)))
          (let ((vare (^loopvarend)))
        (list (string-append    nl
        (expandclosure)  "PUSH( LABEL (" label-lambda "));" nl
          "PUSH(R0);" nl "CALL(MAKE_SOB_CLOSURE);" nl
           "DROP(2);" nl      
             "JUMP(" label-exit ");" nl 
              label-lambda ":" nl 
             "MOV(R2,FPARG(1));" nl
              "CMP(R2,IMM(0));"
               "JUMP_NE(" dov ");" nl 
                "PUSH(5);" nl
                 "MOV(FPARG(-3),FPARG(-2));" nl
                 "MOV(FPARG(-2),FPARG(-1));" nl
                 "MOV(FPARG(-1),FPARG(0));" nl
                  "MOV(FPARG(0),IMM(1));" nl
                 "MOV(FPARG(1),5);" nl
                 "MOV(FP,SP);" nl
                "JUMP(" zero ");" nl
               dov ":"
                "MOV(R6,R2);" nl "SUB(R6,1);" nl
              "MOV(R5,R2);" nl "ADD(R5,1);"  nl "ADD(R2,1);" nl "MOV(R3,1);"
                nl 
            
             "MOV(R3,FPARG(R2));" nl 
             "PUSH(5);" nl
             "PUSH(R3);" nl
              "CALL(MAKE_SOB_PAIR);" nl
              "DROP(2);" nl 
              "SUB(R2,IMM(1));" nl  
              varl ":" nl
              "CMP(R2,IMM(1));" nl
              "JUMP_EQ(" vare ");" nl
               "MOV(R4,R0);" nl
              "MOV(R3,FPARG(R2));" nl
               "PUSH(R4);" nl
               "PUSH(R3);" nl
                "CALL(MAKE_SOB_PAIR);" nl
                "DROP(2);" nl
                "SUB(R2,1);" nl 
                "JUMP(" varl ");" nl
                vare ":" nl
                "MOV(FPARG(R5),R0);" nl
                "SUB(R5,1);" nl
                 "MOV(FPARG(R5),1);" nl
                 "SUB(R5,1)" nl
                 "MOV(FPARG(R5),FPARG(0));" nl
                 "SUB(R5,1)" nl
                 "MOV(FPARG(R5),FPARG(-1));" nl
                 "SUB(R5,1)" nl
                  "MOV(FPARG(R5),FPARG(-2));" nl
                 "SUB(R5,1)" nl
                  "DROP(R6);"
                   "MOV(FP,SP);"
                 
                                    
                zero ":"  
                
                (car bodystring)  nl "POP(FP);" nl "RETURN;" nl    
               label-exit ":" nl) ls lsf))))))))))))) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
            


(define ^loopopt (^^label "LOOOPT"))
(define ^loopoptend (^^label "LOOPOPTEND"))
(define ^loopinopt (^^label "LOOINOPT"))
(define ^loopinoptend (^^label "LOOPINOPTEND"))
(define ^loopnul (^^label "LOOPNUL"))
(define ^loopnulend (^^label "NULLEND"))
(define ^loopnulin (^^label "NULLIN"))
(define ^didlist (^^label "DONELIST"))
(define code-gen-lambda-opt  (lambda (exp ls lsf)
  (with exp
     (lambda (lambda  par s  body)
       (let ((donelist (^didlist))) 
       (let ((innull (^loopnulin)))
       (let ((loopn (^loopnul)))
        (let ((loopnend (^loopnulend))) 
       (let ((bodystring (code-gen-help body ls lsf)))
        (let ((label-lambda (^lambda-label)))
         (let ((label-exit (^lambda-exit)))
         (let ((varl (^loopopt)))
          (let ((vare (^loopoptend)))
           (let ((numpar (list-length par 0)))
            (let ((loopin (^loopinopt)))
            (let ((loopinend (^loopinoptend)))  
             (list (string-append    nl
         (expandclosure)  "PUSH( LABEL (" label-lambda "));" nl
          "PUSH(R0);" nl "CALL(MAKE_SOB_CLOSURE);" nl
           "DROP(2);" nl      
             "JUMP(" label-exit ");" nl 
              label-lambda ":" nl 
             "MOV(R2,FPARG(1));" nl
             "CMP(R2," (number->string numpar) ");" nl
              "JUMP_EQ(" loopn ");" nl
                     
                    
                     "MOV(R6,R2);" nl 
                     "SUB(R6,1);" nl
              "MOV(R5,R2);" nl "ADD(R5,1);"  nl "ADD(R2,1);" nl 
                     "MOV(R3,1);"
              "MOV(R7,IMM("   (number->string numpar) "));" nl
               "MOV(R8,R7);" nl
               "MOV(R10,R7);" nl
                "ADD(R10,IMM(1));" nl   
              
                    
               "MOV(R9,FPARG(1));" nl
               "SUB(R9,R7);" nl 
               "SUB(R9,IMM(1));" 
                     nl     
                "ADD(R7,IMM(1));"      
                    nl 
                 
             "MOV(R3,FPARG(R2));" nl 
             "PUSH(5);" nl
             "PUSH(R3);" nl
              "CALL(MAKE_SOB_PAIR);" nl
              "DROP(2);" nl 
              "SUB(R2,IMM(1));" nl  
              varl ":" nl
              "CMP(R2,R7);" nl
              "JUMP_EQ(" vare ");" nl
               "MOV(R4,R0);" nl
              "MOV(R3,FPARG(R2));" nl
               "PUSH(R4);" nl
               "PUSH(R3);" nl
                "CALL(MAKE_SOB_PAIR);" nl
                "DROP(2);" nl
                "SUB(R2,1);" nl 
                "JUMP(" varl ");" nl
                vare ":" nl
               " MOV(R2,FPARG(1));" nl
                 "ADD(R2,IMM(1));" nl    
                "MOV(FPARG(R2),R0);" nl
                 "SUB(R2,1);" nl     
               
                "MOV(R6,1);" nl
                "ADD(R6," (number->string numpar) ");" nl
                     
                 loopin ":" nl
                "CMP(R8,IMM(0));"     
                "JUMP_EQ(" loopinend ");" nl
                 "MOV(FPARG(R2),FPARG(R6));" nl
                 "SUB(R2,1);" nl
                 "SUB(R6,1);" nl
                 "SUB(R8,1);" nl
                 "JUMP(" loopin ");" nl
                 loopinend ":" nl
                  "MOV(FPARG(R2),R10);" nl
                   "SUB(R2,1);" nl
                   "MOV(FPARG(R2),FPARG(0));" nl
                    "SUB(R2,1);" nl
                   "MOV(FPARG(R2),FPARG(-1));" nl
                    "SUB(R2,1);" nl
                    "MOV(FPARG(R2),FPARG(-2));" nl
                               
                   
                   
                     "DROP(R9);"
                   "MOV(FP,SP);"
                   "JUMP(" donelist ");" nl            
                     loopn ":"    
                    "PUSH(5);"
                    "MOV(R6," (number->string numpar) ");" nl
                    "MOV(R7,R6);" nl
                     "ADD(R7,IMM(1));" nl
                    
                   
                     "MOV(FPARG(-3),FPARG(-2));" nl
                       "MOV(FPARG(-2),FPARG(-1));" nl
                      "MOV(FPARG(-1),FPARG(0));" nl
                      "MOV(FPARG(0),R7);" nl
                      "MOV(R7,IMM(1));" nl
                       "MOV(R8,IMM(2));" nl
 
                     innull ":"
                     "CMP(R6,IMM(0));"
                     "JUMP_EQ(" loopnend ");" nl
                     "MOV(FPARG(R7),FPARG(R8));" nl
                     "ADD(R8,IMM(1));" nl
                     "ADD(R7,IMM(1));" nl   
                     "SUB(R6,IMM(1));" nl
                     "JUMP(" innull ");" nl
                      loopnend ":" nl
                      "MOV(FPARG(R7),5);" nl
                      "MOV(FP,SP);" nl                     

                  donelist ":" nl
 
                (car bodystring)  nl "POP(FP);" nl "RETURN;" nl    
               label-exit ":" nl) ls lsf))))))))))))))))) 

;;;;;;;;;;;;;;;;;;;;;;

(define code-gen-string (lambda (ls curr p)
  (if (null? ls)
   (list (string-append curr "PUSH(" (number->string p) ");" nl   "CALL(MAKE_SOB_STRING);" nl
       "DROP(" (number->string (+ 1  p))  ");" nl) 0)
   (code-gen-string (cdr ls)  
     (string-append curr  
       "PUSH('" (list->string (list (car ls))) "');" nl)
     (+ p 1))
      )))     
      


(define printsconsts1 (lambda (ls curr ls2 lsf)
      (if (null? ls) curr
       (if (string? (caar ls))
          (printsconsts1 (cdr ls) (string-append curr
              (car (code-gen-string (string->list (caar ls)) "" 0)))                      
                                    ls2 lsf)
           
          
       (if (char? (caar ls))
        (printsconsts1 (cdr ls) (string-append curr "PUSH(IMM(" (number->string (char->integer (caar ls))) "));" nl
            "CALL(MAKE_SOB_CHAR);" nl "DROP(1);" nl ) ls2 lsf)
        (if (pair? (caar ls))   
         (printsconsts1 (cdr ls) (string-append curr (car (code-gen-help (parse (caar ls)) ls2 lsf))) ls2 lsf)
        
       (if (number? (caar ls))
         (printsconsts1 (cdr ls) (string-append curr "PUSH(IMM(" (number->string (caar ls)) "));" nl
            "CALL(MAKE_SOB_INTEGER);" nl "DROP(1);" nl ) ls2 lsf)
        (if (and (boolean? (caar ls)) (equal? (caar ls) #t))
           (printsconsts1 (cdr ls)  (string-append curr "PUSH(1);" nl "CALL(MAKE_SOB_BOOL);" nl "DROP(1);" nl) ls2 lsf)
            
        (if (and (boolean? (caar ls)) (equal? (caar ls) #f))
           (printsconsts1 (cdr ls) (string-append curr "PUSH(0);" nl "CALL(MAKE_SOB_BOOL);" nl "DROP(1);" nl) ls2 lsf)
         (if (null? (caar ls))
             (printsconsts1 (cdr ls) (string-append curr "CALL(MAKE_SOB_NIL);" nl) ls2 lsf)
          (if (and (symbol? (caar ls) ) 
                   (equal? (symbol->string (caar ls)) "#<void>"))  
              (printsconsts1 (cdr ls) (string-append curr "CALL(MAKE_SOB_VOID);" nl) ls2 lsf)
           (if (symbol? (caar ls))
              (printsconsts1 (cdr ls) 
                (string-append curr "PUSH(2);" nl "CALL(MALLOC);" nl
                    "DROP(2);" nl "MOV(INDD(R0,0),T_SYMBOL);" nl
                    "MOV(INDD(R0,1)," (number->string 
           (findadd (symbol->string (caar ls)) ls2)) ");" nl)
                                        ls2 lsf)  
              ))))))))))))
 
;;;;;;;;;;;;;;do list and other consts!!!

(define print-code-gen (lambda (exp)
     (string-append (printsconsts1 (count (annotate-tc (pe->lex-pe  exp)) '((#t 1) (#f 3) (() 5) (\x23;<void> 6)  )) "" (count (annotate-tc (pe->lex-pe  exp)) '((#t 1) (#f 3) (() 5) (\x23;<void> 6) ))
                (setadd (annotate-tc (pe->lex-pe exp)) 
       (findnextadress (count (annotate-tc (pe->lex-pe exp)) 
                      '((#t 1) (#f 3) (() 5) (\x23;<void> 6)   ))))      )
      (printfvar  (setadd (annotate-tc (pe->lex-pe exp)) 
       (findnextadress (count (annotate-tc (pe->lex-pe exp)) 
                         '((#t 1) (#f 3) (() 5) (\x23;<void> 6)  )))) "")
       (printproc (count (annotate-tc (pe->lex-pe exp)) 
                         '((#t 1) (#f 3) (() 5) (\x23;<void> 6) ))
         (setadd (annotate-tc (pe->lex-pe exp)) 
       (findnextadress (count (annotate-tc (pe->lex-pe exp)) 
                         '((#t 1) (#f 3) (() 5) (\x23;<void> 6)  )))) 
        
        (setadd (annotate-tc (pe->lex-pe exp)) 
       (findnextadress (count (annotate-tc (pe->lex-pe exp)) 
                         '((#t 1) (#f 3) (() 5) (\x23;<void> 6)  )))) "" )
 ;    (printcar (count (annotate-tc (pe->lex-pe exp)) 
  ;                       '((#t 1) (#f 3) (() 5) (\x23;<void> 6) ))
   ;      (setadd (annotate-tc (pe->lex-pe exp)) 
    ;   (findnextadress (count (annotate-tc (pe->lex-pe exp)) 
     ;                    '((#t 1) (#f 3) (() 5)  )))) )
       
       (car (code-gen  (annotate-tc (pe->lex-pe  exp)))))))


(define code-gen-fvar (lambda (exp ls) 
  (if (equal? (cadr exp) (caar ls))
     (list (string-append "MOV(R0," (number->string (cadar ls))
             ");" nl "MOV(R0,INDD(R0,0));" nl) ls)
   (code-gen-fvar exp (cdr ls)))))   

(define find-fvar (lambda (exp ls) 
  (if (equal? (cadr exp) (caar ls))
     (list (string-append "MOV(R0," (number->string (cadar ls))
             ");" nl) ls)
   (find-fvar exp (cdr ls)))))   

;;;;;;;;;;;;;;;;;;;;;;;;;;




(define printproc (lambda (ls lsf lstemp curr )
    (if (null? lsf) curr
      (if (equal? (caar lsf) '+)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printplus ls lstemp)))
          
     (if (equal? (caar lsf) '-)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printminus ls lstemp)))         
    (if (equal? (caar lsf) '*)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printmul ls lstemp)))        
     (if (equal? (caar lsf) '<)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printlesser ls lstemp)))         
     (if (equal? (caar lsf) '>)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printbigger ls lstemp)))        
     (if (equal? (caar lsf) 'integer->char)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printintchar ls lstemp)))
     (if (equal? (caar lsf) 'char->integer)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printcharint ls lstemp)))
     (if (equal? (caar lsf) '=)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printshave ls lstemp)))
     (if (equal? (caar lsf) 'boolean?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printisbool ls lstemp)))
     (if (equal? (caar lsf) 'integer?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printisint ls lstemp)))
     (if (equal? (caar lsf) 'make-vector)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printmakevector ls lstemp)))
     (if (equal? (caar lsf) 'vector-length)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printvectorlength ls lstemp)))
     (if (equal? (caar lsf) 'vector?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printvector ls lstemp)))
     (if (equal? (caar lsf) '/)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printdiv ls lstemp)))
     (if (equal? (caar lsf) 'vector-ref)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printvectorref ls lstemp)))
     (if (equal? (caar lsf) 'make-string)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printmakestring ls lstemp)))
     (if (equal? (caar lsf) 'string-length)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printstringlength ls lstemp)))
     (if (equal? (caar lsf) 'string-ref)  
         (printproc ls (cdr lsf) lstemp  (string-append curr (printstringref ls lstemp)))
     
     (if (equal? (caar lsf) 'cons)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printcons ls lstemp)))
     (if (equal? (caar lsf) 'cdr)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printcdr ls lstemp)))
     (if (equal? (caar lsf) 'car)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printcar ls lstemp)))
     (if (equal? (caar lsf) 'char?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printischar ls lstemp)))
         
     (if (equal? (caar lsf) 'pair?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printispair ls lstemp)))
     (if (equal? (caar lsf) 'number?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printisnum ls lstemp)))
     (if (equal? (caar lsf) 'string?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printisstring ls lstemp)))
     (if (equal? (caar lsf) 'null?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printisnull ls lstemp)))
     (if (equal? (caar lsf) 'number?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printisnum ls lstemp)))
     (if (equal? (caar lsf) 'remainder)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printremainder ls lstemp)))
     (if (equal? (caar lsf) 'zero?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printzero ls lstemp)))
     (if (equal? (caar lsf) 'set-car!)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printsetcar ls lstemp)))
     (if (equal? (caar lsf) 'set-cdr!)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printsetcdr ls lstemp)))
     (if (equal? (caar lsf) 'symbol?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printissymbol ls lstemp)))
     (if (equal? (caar lsf) 'procedure?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printprocedure ls lstemp)))
     (if (equal? (caar lsf) 'apply)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printapply ls lstemp)))
     (if (equal? (caar lsf) 'eq?)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printeq ls lstemp)))
     (if (equal? (caar lsf) 'apply)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printapply ls lstemp)))
     (if (equal? (caar lsf) 'string->symbol)
         (printproc ls (cdr lsf) lstemp  (string-append curr (printstrtosym ls lstemp)))
         
         
        (printproc ls (cdr lsf) lstemp curr) 
         
         ))))))))))))))))))))))))))))))))))))))))
      
;;;;;;;;;;;;;;;;;;;;;;;;;1
(define ^plus (^^label "PLUS"))
(define ^plusend (^^label "PLUSEND"))
(define printplus (lambda ( ls lsf  )                   
(let ((plus (^plus)))
(let ((end  (^plusend)))  
(string-append 
"PUSH(LABEL(" plus "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
 "MOV(R1,R0);" nl
  (car (find-fvar '(fvar +) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl
plus ":" 
(pusha)  
"MOV(R2,FPARG(1));" nl
"ADD(R2,IMM(2));" nl
"MOV(R0,IMM(0));" nl
"MOV(R1,IMM(2));" nl
"LOOPLUS:" nl
"CMP(R1,R2);" nl
"JUMP_EQ(EXITPLUS);" nl
"MOV(R3,FPARG(R1));" nl
"MOV(R3,INDD(R3,1));" nl  
"ADD(R0,R3);" nl
"ADD(R1,IMM(1));" nl
"JUMP(LOOPLUS);" nl
"EXITPLUS:" nl
"PUSH(R0);" nl
"CALL(MAKE_SOB_INTEGER);" nl
"DROP(1);" nl 
  (popa)
"POP(FP);" nl
"RETURN; " nl
end ":"
  
 
  )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
(define ^minus (^^label "MINUS"))
(define ^minend (^^label "MINUSEND"))
(define printminus (lambda (ls  lsf )                   
(let ((minus (^minus)))
 (let ((mine (^minend))) 
(string-append 
"PUSH(LABEL(" minus "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar -) lsf))
  "MOV(INDD(R0,0),R1);" nl                   
"JUMP(" mine ");"    
  
 minus ":" 
 (pusha)
"MOV(R1,FPARG(1));" nl
"SUB(R1,IMM(1));" nl
"MOV(R0,FPARG(2));" nl
"MOV(R0,INDD(R0,1));" nl
"MOV(R3,IMM(3));" nl
"LOOPMINUS:" nl
"CMP(R1,IMM(0));" nl
"JUMP_EQ(EXITMINUS);"nl
"MOV(R4,FPARG(R3));" nl
"MOV(R4,INDD(R4,1));" nl  
"SUB(R0,R4);"nl
"SUB(R1,IMM(1));" nl
"ADD(R3,IMM(1));" nl  
"JUMP(LOOPMINUS);"nl
"EXITMINUS:"nl
"PUSH(R0);" nl
"CALL(MAKE_SOB_INTEGER);" nl
"DROP(1);" nl 
  (popa)
"POP(FP);"nl
"RETURN;" nl
mine ":" 
 
  )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
(define findfvar (lambda (fvar lsf)
      (if (equal? fvar (caar lsf))
          (cadar lsf)
        (findvar fvar (cdr lsf)))))  

(define ^mul (^^label "MULT"))
(define ^mulend (^^label "MULEND"))
(define printmul (lambda ( ls lsf  )                   
(let ((mul (^mul)))
(let ((end  (^mulend)))  
(string-append 
"PUSH(LABEL(" mul "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar *) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl
mul ":"  
  (pusha)
"MOV(R2,FPARG(1));" nl
"ADD(R2,IMM(2));" nl
"MOV(R0,IMM(1));" nl
"MOV(R1,IMM(2));" nl
"LOOPMUL:" nl
"CMP(R1,R2);" nl
"JUMP_EQ(EXITMUL);" nl
"MOV(R3,FPARG(R1));" nl
"MOV(R3,INDD(R3,1));" nl  
"MUL(R0,R3);" nl
"ADD(R1,IMM(1));" nl
"JUMP(LOOPMUL);" nl
"EXITMUL:" nl
"PUSH(R0);" nl
"CALL(MAKE_SOB_INTEGER);" nl
"DROP(1);" nl 
  (popa)
"POP(FP);" nl
"RETURN; " nl
end ":"
  
 
  )))))


;;;;;;;;;;;;;;;;;;;;;4

(define ^lesserend (^^label "LESSEREND"))
(define ^lesser (^^label "LESSER"))
(define printlesser (lambda (ls lsf)                   
(let ((lesser (^lesser)))
(string-append 
  
(let ((lesser (^lesser)))
(let ((end  (^lesserend)))  
(string-append 
"PUSH(LABEL(" lesser "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar <) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl

lesser ":" 
  (pusha)
"MOV(R2,FPARG(1));"nl
"ADD(R2,IMM(2));"nl
"MOV(R1,IMM(2));"nl
"MOV(R0,FPARG(R1));"nl
"MOV(R0,INDD(R0,1));" nl
"ADD(R1,IMM(1));"nl
"LOOBIGGER:" nl
"CMP(R1,R2);" nl
"JUMP_EQ(EXITBIGGERT);" nl
"MOV(R4,FPARG(R1));" nl
"MOV(R4,INDD(R4,1));" nl  
"CMP(R0,R4);" nl
"JUMP_GE(EXITBIGGER);" nl
"MOV(R0,FPARG(R1));" nl
 "MOV(R0,INDD(R0,1));" nl 
"ADD(R1,IMM(1));" nl
"JUMP(LOOBIGGER);" nl
"EXITBIGGERT: " nl
"MOV(R0,1);" nl
   (popa)
"POP(FP);" nl
"RETURN; " nl
"EXITBIGGER:"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
 end ":" )))))))



;;;;;;;;;;;;;;;;;;;;;;5
(define ^biggerend (^^label "BIGGEREND"))
(define ^bigger (^^label "BIGGER"))
(define printbigger (lambda (ls lsf)                   
(let ((bigger (^bigger)))
(string-append 
  
(let ((bigger (^bigger)))
(let ((end  (^biggerend)))  
(string-append 
"PUSH(LABEL(" bigger "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar >) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl
  
  bigger ":" 
(pusha)
"MOV(R2,FPARG(1));"nl
"ADD(R2,IMM(2));"nl
"MOV(R1,IMM(2));"nl
"MOV(R0,FPARG(R1));" nl
"MOV(R0,INDD(R0,1));" nl  
"ADD(R1,IMM(1));"nl
"LOOLESSER:"nl
"CMP(R1,R2);"nl
"JUMP_EQ(EXITLESSERT);"nl

 "MOV(R4,FPARG(R1));" nl
 "MOV(R4,INDD(R4,1));" nl 
  "CMP(R0,R4);"nl
"JUMP_LE(EXITLESSER);"nl
"MOV(R0,FPARG(R1));"nl
"MOV(R0,INDD(R0,1));" nl  
"ADD(R1,IMM(1));"nl
"JUMP(LOOLESSER);"nl
"EXITLESSERT: "nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);" nl
 "RETURN; "nl
"EXITLESSER:"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);" nl
"RETURN;"nl
 end ":"
 
  )))))))
  
  
  ;;;;;;;;;;;;;;;;;;;;;6


(define ^intcharend (^^label "INTCHAREND"))
(define ^intchar (^^label "INTCHAR"))
(define printintchar (lambda (ls lsf)                   
(let ((intchar (^intchar)))
(string-append 
(let ((intchar (^intchar)))
(let ((end  (^intcharend)))  
(string-append 
"PUSH(LABEL(" intchar "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar integer->char) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl
intchar ":"
  (pusha)
"PUSH(INDD(FPARG(2),1));"nl
"CALL(MAKE_SOB_CHAR);"nl
"DROP(1);"nl
  (popa)
"POP(FP);" nl
"RETURN;"nl
  end ":"
  )))))))


;;;;;;;;;;;;;;;;;;;;;7

(define ^charintend (^^label "CHARINTEND"))
(define ^charint (^^label "CHARINT"))
(define printcharint (lambda (ls lsf)                   
(let ((charint (^charint)))
(let ((end  (^charintend)))  
(string-append 
"PUSH(LABEL(" charint "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar char->integer) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl
charint ":" 
  (pusha)
"PUSH(INDD(FPARG(2),1));" nl
"CALL(MAKE_SOB_INTEGER);" nl
"DROP(1);" nl
  (popa)
"POP(FP);" nl
"RETURN;" nl
 end ":" 
  )))))

;;;;;;;;;;;;;;;;;;;;8
                     


(define ^shaveend (^^label "SHAVEEND"))
(define ^shave (^^label "SHAVE"))
(define printshave (lambda (ls lsf)                   
(let ((shave (^shave)))
 

(let ((end  (^shaveend)))  
(string-append 
"PUSH(LABEL(" shave "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar =) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl
shave ":" 
  (pusha)
"MOV(R2,FPARG(1));"nl
"ADD(R2,IMM(2));"nl
"MOV(R1,IMM(2));"nl
"MOV(R0,FPARG(R1));"nl
"MOV(R0,INDD(R0,1));" nl  
"ADD(R1,IMM(1));"nl
"LOOLSHAVE:"nl
"CMP(R1,R2);"nl
"JUMP_EQ(EXITSHAVE);"nl
"MOV(R4,FPARG(R1));" nl
 "MOV(R4,INDD(R4,1));" nl 
"CMP(R0,R4);"nl
"JUMP_NE(EXITSHAVEF);"nl
"MOV(R0,FPARG(R1));"nl
"MOV(R0,INDD(R0,1));" nl  
"ADD(R1,IMM(1));"nl
"JUMP(LOOLSHAVE);"nl
"EXITSHAVE: " nl
"MOV(R0,1);" nl
   (popa)
"POP(FP);" nl
"RETURN; " nl
"EXITSHAVEF:" nl
"MOV(R0,3);" nl
   (popa)
"POP(FP);" nl
"RETURN;" nl
   end ":"
  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;9
(define ^isboolend (^^label "ISBOOLEND"))
(define ^isbool (^^label "ISBOOL"))
(define printisbool (lambda (ls lsf)                   
(let ((isbool (^isbool)))

(let ((end  (^isboolend)))  
(string-append 
"PUSH(LABEL(" isbool "));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar boolean?) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(" end ");" nl 
isbool ":" 
  (pusha)
"PUSH(FPARG(2));" nl
"CALL(IS_SOB_BOOL);" nl
"DROP(1);"  nl
"CMP(R0,IMM(0));" nl
"JUMP_EQ(FALSEJ);" nl
"MOV(R0,1);" nl
  (popa)
"POP(FP);" nl
"RETURN;" nl
"FALSEJ:" nl
"MOV(R0,3);" nl
  (popa)
"POP(FP);" nl
"RETURN;" nl
  end ":"
  
  
  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;10

(define ^isintend (^^label "ISINTEND"))
(define ^isint (^^label "ISINT"))
(define printisint (lambda (ls lsf)                   
(let ((isint (^isint)))


(let ((end  (^isintend)))  
(string-append 
"PUSH(LABEL(" isint "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar integer?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
isint ":"
  (pusha)
"PUSH(FPARG(2));"nl
"CALL(IS_SOB_INTEGER);"nl
"DROP(1);"nl
"CMP(R0,IMM(0));"nl
"JUMP_EQ(FALSEJU);"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"FALSEJU:" nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;11

(define ^ischarend (^^label "ISCHAREND"))
(define ^ischar (^^label "ISCHAR"))
(define printischar (lambda (ls lsf)                   
(let ((ischar (^ischar)))
(let ((end  (^ischarend)))  
(string-append 
"PUSH(LABEL(" ischar "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar char?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
ischar ":" 
  (pusha)
"PUSH(FPARG(2));"nl
"CALL(IS_SOB_CHAR);"nl
"DROP(1);"nl
"CMP(R0,IMM(0));"nl
"JUMP_EQ(FALSEJC);"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"FALSEJC:"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":"
  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;12


(define ^isnullend (^^label "ISNULLEND"))
(define ^isnull (^^label "ISNULL"))
(define printisnull (lambda (ls lsf)                   
(let ((isnull (^isnull)))
 

(let ((end  (^isnullend)))  
(string-append 
"PUSH(LABEL(" isnull "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar null?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
isnull ":" 
  (pusha)
"PUSH(FPARG(2));"nl
"CALL(IS_SOB_NIL);"nl
"DROP(1);"nl
"CMP(R0,IMM(0));"nl
"JUMP_EQ(FALSEJN);"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"FALSEJN:"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":"
  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;13
(define ^isnumend (^^label "ISNUMEND"))
(define ^isnum (^^label "ISNUM"))
(define printisnum (lambda (ls lsf)                   
 
(let ((isnum (^isnum)))
(let ((end  (^isnumend)))  
(string-append 
"PUSH(LABEL(" isnum "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar number?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
isnum ":" 
  (pusha)
"PUSH(FPARG(2));"nl
"CALL(IS_SOB_INTEGER);"nl
"DROP(1);"nl
"CMP(R0,IMM(0));"nl
"JUMP_EQ(FALSEJMNU);"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"FALSEJMNU:"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;14

(define ^makevectorend (^^label "MAKEVECTOREND"))
(define ^makevector (^^label "MAKEVECTOR"))
(define printmakevector (lambda (ls lsf)                   
(let ((makevector (^makevector)))
 
(let ((makevector (^makevector)))
(let ((end  (^makevectorend)))  
(string-append 
"PUSH(LABEL(" makevector "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar make-vector) lsf))
"MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
makevector ":" 
  (pusha)
"MOV(R3,IMM(0));"nl
"MOV(R1,(FPARG(2)));"nl 
"MOV(R2,FPARG(3)); "nl
"MOV(R1,INDD(R1,1)); "nl
"LOOP_VECTOR:"nl
"CMP(R3,R1);"nl
"JUMP_EQ(END_VECTOR);"nl
"PUSH(R2);"nl
"ADD(R3,IMM(1));"nl
"JUMP(LOOP_VECTOR);"nl
"END_VECTOR:"nl
"PUSH(R1);"nl
"CALL(MAKE_SOB_VECTOR);"nl
"DROP(R1); "nl
"DROP(1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
   end ":"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;15


(define ^makestringend (^^label "MAKESTRINGEND"))
(define ^makestring (^^label "MAKESTRING"))
(define printmakestring (lambda (ls lsf)                   
(let ((makestring (^makestring)))

(let ((makestring (^makestring)))
(let ((end  (^makestringend)))  
(string-append 
"PUSH(LABEL(" makestring "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar make-string) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
makestring ":" 
  (pusha)
"MOV(R3,IMM(0));"nl
"MOV(R1,(FPARG(2)));"nl 
"MOV(R2,FPARG(3)); "nl
"MOV(R2,INDD(R2,1)); "nl
 "MOV(R1,INDD(R1,1));" nl 
"LOOP_STRING:"nl
"CMP(R3,R1);"nl
"JUMP_EQ(END_STRING);"nl
"PUSH(R2);"nl
"ADD(R3,IMM(1));"nl
"JUMP(LOOP_STRING);"nl
"END_STRING:"nl
"PUSH(R1);"nl
"CALL(MAKE_SOB_STRING);"nl
"DROP(R1);"nl
"DROP(1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
   end ":"))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;16

(define ^car (^^label "CAREND"))
(define ^carend (^^label "CAR"))
(define printcar (lambda (ls lsf)                   


(let ((car (^car)))
(let ((end  (^carend)))  
(string-append 
"PUSH(LABEL(" car "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar car) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
car ":" 
  (pusha)
"MOV(R0,FPARG(0));"nl
"MOV(R0,INDD(R0,1)); "nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
end ":"  )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;17

(define ^remainderend (^^label "REMAINDEREND"))
(define ^remainder (^^label "REMAINDER"))
(define printremainder (lambda (ls lsf)                   
(let ((remainder (^remainder)))
(let ((end  (^remainderend)))  
(string-append 
"PUSH(LABEL(" remainder "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar remainder) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
remainder ":"
  (pusha)
"MOV(R1,(FPARG(2)));"nl    
"MOV(R2,(FPARG(3)));"nl
"MOV(R1,INDD(R1, 1));"nl      
"MOV(R2,INDD(R2, 1));"nl   
"REM(R1,R2);"nl
"PUSH(R1);"nl
"CALL(MAKE_SOB_INTEGER);"nl     
"DROP(1);"nl  
  (popa)
"POP(FP);"nl           
"RETURN;"nl
  end ":")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;18


(define ^div (^^label "DIV"))
(define ^divend (^^label "DIVEND"))
(define printdiv (lambda (ls  lsf )                   
(let ((div (^div)))
(let ((divend (^divend))) 
(string-append 
"PUSH(LABEL(" div "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
"MOV(R1,R0);" nl
(car (find-fvar '(fvar /) lsf))
  "MOV(INDD(R0,0),R1);" nl                     
"JUMP(" divend ");"    
  
 div ":" 
 (pusha)
"MOV(R1,FPARG(1));" nl
"SUB(R1,IMM(1));" nl
"MOV(R0,FPARG(2));" nl
"MOV(R0,INDD(R0,1));" nl
"MOV(R3,IMM(3));" nl
"LOOPDIV:" nl
"CMP(R1,IMM(0));" nl
"JUMP_EQ(EXITDIV);"nl
"MOV(R4,FPARG(R3));" nl
"MOV(R4,INDD(R4,1));" nl  
"DIV(R0,R4);"nl
"SUB(R1,IMM(1));" nl
"ADD(R3,IMM(1));" nl  
"JUMP(LOOPDIV);"nl
"EXITDIV:"nl
"PUSH(R0);" nl
"CALL(MAKE_SOB_INTEGER);" nl
"DROP(1);" nl 
  (popa)
"POP(FP);"nl
"RETURN;" nl
divend ":" 
 
  )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;19

(define ^zeroend (^^label "ZEROEND"))
(define ^zero (^^label "ZERO"))
(define printzero (lambda (ls lsf)                   
(let ((zero (^zero)))
(let ((end  (^zeroend)))  
(string-append 
"PUSH(LABEL(" zero "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar zero?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
zero ":" 
   (pusha)
"PUSH(FPARG(0));"nl
"CALL(IS_SOB_INTEGER);"nl
"PUSH(R0);"nl
"CMP(INDD(R0,1),0);"nl
"JUMP_EQ(FJP);"nl
"DROP(1);"nl
"CMP(INDD(FPARG(0),1),0);"nl
"JUMP_EQ(TRUEC);"nl
"JUMP(FJP);"nl
"TRUEC:"nl
"MOV(R0,1);"nl
"DROP(1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"FJP:"nl
"MOV(R0,3);"nl
"DROP(1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
 end ":" )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;20

(define ^vectorend (^^label "VECTOREND"))
(define ^vector (^^label "VECTOR"))
(define printvector (lambda (ls lsf)                   
(let ((vector (^vector)))
(let ((end  (^vectorend)))  
(string-append 
"PUSH(LABEL(" vector "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar vector?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
  vector ":" 	
(pusha)			
"CMP(INDD(FPARG(2),0),335728);"nl
"JUMP_EQ(TRUEEVJP);"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"TRUEEVJP:"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;21

(define ^vectorrefend (^^label "VECTORREFEND"))
(define ^vectorref (^^label "VECTORREF"))
(define printvectorref (lambda (ls lsf)                   
(let ((vectorref (^vectorref)))
(let ((end  (^vectorrefend)))  
(string-append 
"PUSH(LABEL(" vectorref "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar vector-ref) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
vectorref ":" 
  (pusha)
"MOV(R0,(FPARG(2)));"nl 
"MOV(R2,(FPARG(3)));"nl 
"PUSH(INDD(R0,-11));"nl
"CALL(MAKE_SOB_INTEGER);"nl
"DROP(1);"nl
"END_VECTOR_REF:"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
   end ":")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;22

(define ^stringrefend (^^label "STRINGREFEND"))
(define ^stringref (^^label "STRINGREF"))
(define printstringref (lambda (ls lsf)                   
(let ((stringref (^stringref)))
(let ((end  (^stringrefend)))  
(string-append 
"PUSH(LABEL(" stringref "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar string-ref) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
stringref ":"
  (pusha)
"MOV(R0,(FPARG(2)));"nl 
"MOV(R2,(FPARG(3)));"nl 
"PUSH(INDD(R0,R2));"nl
"CALL(IS_SOB_CHAR);"nl
"CMP(R0,0);"nl
"JUMP_EQ(CHARCON);"nl	
"CALL(MAKE_SOB_INTEGER);"nl
"DROP(1);"nl
"JUMP(END_S_REF);"nl
"CHARCON:	"nl
"CALL(MAKE_SOB_CHAR);"nl
"DROP(1);"nl
"END_S_REF:"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl 
  end ":")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;23

(define ^stringlengthend (^^label "STRINGLENGTHEND"))
(define ^stringlength (^^label "STRINGLENGTH"))
(define printstringlength (lambda (ls lsf)                   
(let ((stringlength (^stringlength)))
(let ((end  (^stringlengthend)))  
(string-append 
"PUSH(LABEL(" stringlength "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar string-length) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
stringlength ":" 
		(pusha)
"MOV(R0,(FPARG(2)));"nl
"PUSH(INDD(R0,1));"nl
"CALL(MAKE_SOB_INTEGER);"nl
"DROP(1);"nl
"END_S_L:"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl 
  end ":")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;24

(define ^vectorlengthend (^^label "VECTORLENGTHEND"))
(define ^vectorlength (^^label "VECTORLENGTH"))
(define printvectorlength (lambda (ls lsf)                   
(let ((vectorlength (^vectorlength)))
(let ((end  (^vectorlengthend)))  
(string-append 
"PUSH(LABEL(" vectorlength "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar vector-length) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl  
vectorlength ":" 
  (pusha)
"MOV(R0,(FPARG(2)));"nl 
"MOV(R2,(FPARG(3)));"nl 
"PUSH(INDD(R0,1));"nl
"CALL(MAKE_SOB_INTEGER);"nl
"DROP(1);"nl
"END_VECTOR_LENGTH:"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
 end ":" )))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;25


(define ^consend (^^label "CONSEND"))
(define ^cons (^^label "CONS"))
(define printcons (lambda (ls lsf)                   
(let ((cons (^cons)))
(let ((end  (^consend)))  
(string-append 
"PUSH(LABEL(" cons "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar cons) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
cons ":"
  (pusha)
"PUSH(FPARG(3));"nl
"PUSH(FPARG(2));"nl
"CALL(MAKE_SOB_PAIR);"nl
"DROP(2);"nl
  (popa)
"POP(FP);"nl
"RETURN;"
  end ":"
  nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;26

(define ^str->sym (^^label "strsymstart"))
(define ^str->symend (^^label "STRSYMEND"))
(define printstrtosym (lambda (ls lsf)
(let ((str1 (^str->sym)))
  (let ((end (^str->symend)))  
  (string-append
   "PUSH(LABEL(" str1 "));" nl
   "PUSH(5);" nl
   "CALL(MAKE_SOB_CLOSURE);" nl
   "DROP(2);" nl
   "MOV (R1,R0);" nl
   (car (find-fvar '(fvar string->symbol) lsf))
   "MOV (INDD(R0,0),R1);" nl
   "JUMP(" end ");" nl
   str1 ":"
   (pusha)
   "MOV(R1,FPARG(2));" nl
  "PUSH(2);" nl
  "CALL(MALLOC);" nl
 " DROP(2);" nl
 "MOV(INDD(R0,0),T_SYMBOL);" nl
 "MOV(INDD(R0,1),R1);" nl
(popa)
"POP(FP);" nl
"RETURN;" nl
end ":" nl)))))


    



;;;;;;;;;;;;;;;;;;;;;;;

(Define ^carend (^^label "CAREND"))
(define ^car1 (^^label "CAR"))
(define printcar (lambda (ls lsf)                   
(let ((car1 (^car1)))
(let ((end  (^carend)))  
(string-append 
"PUSH(LABEL(" car1 "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar car) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
car1 ":"
  (pusha)
"MOV(R0,FPARG(2));"nl
"MOV(R0,INDD(R0,1)); "nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
end ":"  
  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;27


(define ^cdrend (^^label "CDREND"))
(define ^cdr1 (^^label "CDR"))
(define printcdr (lambda (ls lsf)                   
(let ((cdr1 (^cdr1)))
(let ((end  (^cdrend)))  
(string-append 
"PUSH(LABEL(" cdr1 "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar cdr) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
cdr1 ":" 
  (pusha)
"MOV(R0,FPARG(2));"nl
"MOV(R0,INDD(R0,2)); "nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
end ":"  
  )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;28

(define ^setcarend (^^label "SETCAREND"))
(define ^setcar (^^label "SETCAR"))
(define printsetcar (lambda (ls lsf)                   
(let ((setcar (^setcar)))
(let ((end  (^setcarend)))  
(string-append 
"PUSH(LABEL(" setcar "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar set-car!) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
setcar ":"
 (pusha)  
"MOV(INDD(FPARG(2),1),FPARG(3));"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":"
  nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;29

(define ^setcdrend (^^label "SETCDREND"))
(define ^setcdr (^^label "SETCDR"))
(define printsetcdr (lambda (ls lsf)                   
(let ((setcdr (^setcdr)))
(let ((end  (^setcdrend)))  
(string-append 
"PUSH(LABEL(" setcdr "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar set-cdr!) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
setcdr ":"  
  (pusha)
"MOV(INDD(FPARG(2),2),FPARG(3));"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":"
  nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;;30

(define ^ispairend (^^label "ISPAIREND"))
(define ^ispair (^^label "ISPAIR"))
(define printispair (lambda (ls lsf)                   
(let ((ispair (^ispair)))
(let ((end  (^ispairend)))  
(string-append 
"PUSH(LABEL(" ispair "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar pair?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl  
ispair ":" 
  (pusha)
"CMP(INDD(FPARG(2),0),885397);"nl
"JUMP_EQ(TRUEEPJP);"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"TRUEEPJP:"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
 end ":" )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;31

(define ^isstringend (^^label "ISSTRINGEND"))
(define ^isstring (^^label "ISSTRING"))
(define printisstring (lambda (ls lsf)                   
(let ((isstring (^isstring)))
(let ((end  (^ispairend)))  
(string-append 
"PUSH(LABEL(" isstring "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar string?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
isstring ":" 
  (pusha)
"CMP(INDD(FPARG(2),0),799345);"nl
"JUMP_EQ(TRUEESJP);"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"TRUEESJP:"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"
  end ":"  nl)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;32 
  
(define ^isprocend (^^label "ISPROCEND"))
(define ^isproc (^^label "ISPROC"))
(define printprocedure (lambda (ls lsf)                   
(let ((isproc (^isproc)))
(let ((end  (^isprocend)))  
(string-append 
"PUSH(LABEL(" isproc "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar procedure?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl 
isproc ":" 
  (pusha)
"CMP(INDD(FPARG(2),0),276405);"nl
"JUMP_EQ(TRUEEPRJP);"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"TRUEEPRJP:"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
  end ":")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;33

(define ^issymbolend (^^label "ISSYMBOLEND"))
(define ^issymbol (^^label "ISSYMBOL"))
(define printissymbol (lambda (ls lsf)                   
(let ((issymbol (^issymbol)))
(let ((end  (^issymbolend)))  
(string-append 
"PUSH(LABEL(" issymbol "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar symbol?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
issymbol ":" 
  (pusha)
"CMP(INDD(FPARG(2),0),368031);"nl
"JUMP_EQ(TRUEESYJP);"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl
"TRUEESYJP:"nl
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"
  end ":"  nl)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;33

(define ^eqend (^^label "EQEND"))
(define ^eq (^^label "EQ"))
(define printeq (lambda (ls lsf)                   
(let ((eq (^eq)))
(let ((end  (^eqend)))  
(string-append 
"PUSH(LABEL(" eq "));" nl
"PUSH(5);" nl
"CALL(MAKE_SOB_CLOSURE);" nl
"DROP(2);" nl
  "MOV(R1,R0);" nl
(car (find-fvar '(fvar eq?) lsf))
  "MOV(INDD(R0,0),R1);" nl
"JUMP(" end ");" nl
eq ":" 
   (pusha)
"MOV(R1,FPARG(2));"nl
"MOV(R2,FPARG(3));"nl
"CMP(INDD(R1,0),335728);"nl
"JUMP_EQ(VECTORSCMP);"nl
"CMP(INDD(R1,0),885397);"nl
"JUMP_EQ(PAIRSCMP);"nl
"CMP(INDD(R1,0),799345);"nl
"JUMP_EQ(STRINGSCMP); "nl
"JUMP(REGULCOMP); "nl
"VECTORSCMP:"nl
"CMP(INDD(R2,0),335728);"nl
"JUMP_EQ(COMPADD);  "nl
"JUMP(EQFALSECASE);"nl
"STRINGSCMP:"nl
"CMP(INDD(R2,0),885397);"nl
"JUMP_EQ(COMPADD);  "nl
"JUMP(EQFALSECASE);"nl
"PAIRSCMP:  "nl
"CMP(INDD(R2,0),799345);"nl
"JUMP_EQ(COMPADD);  "nl
"JUMP(EQFALSECASE);"nl
"COMPADD:"nl
"CMP(R1,R2);"nl
"JUMP_EQ(EQTRUECASE);"nl
"JUMP(EQFALSECASE);  "nl
"REGULCOMP:"nl
"CMP(INDD(R1,1),INDD(R2,1));"nl
"JUMP_EQ(EQTRUECASE);"nl  
"JUMP(EQFALSECASE);  "nl
"EQFALSECASE:"nl
"MOV(R0,3);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl  
"EQTRUECASE:"nl  
"MOV(R0,1);"nl
  (popa)
"POP(FP);"nl
"RETURN;"nl 
 end nl":" )))))



;;;;;;;;;;;;;;
(define printapply (lambda (ls lsf)
(string-append
      "PUSH(LABEL( APPLY));" nl
 "PUSH(5);" nl
 "CALL(MAKE_SOB_CLOSURE);" nl
 "DROP(2);" nl
 "MOV(R1,R0);" nl
  (car (find-fvar '(fvar apply) lsf))
  "MOV(INDD(R0,0),R1);" nl
 "JUMP(APPLYENDLAST);"                     
"APPLY:" nl
"

MOV(R0,FPARG(2));" nl
"
MOV(R2,FPARG(3));"nl
"
CMP(R2,5);"nl

"JUMP_EQ(APPLYEMPTY);"nl

"MOV(R3,INDD(R2,1));"nl
"
MOV(R4,INDD(R2,2));"nl
"
MOV(R5,IMM(1));"nl
"
APPLYLOOP:"nl
"
CMP(R4,5);
"nl
"JUMP_EQ(APPLYEND);"nl
"
PUSH(R3);"nl
"
ADD(R5,IMM(1));"nl
"
MOV(R3,INDD(R4,1));"nl
"
MOV(R4,INDD(R4,2));"nl
"
JUMP(APPLYLOOP);" nl
"
APPLYEND:"nl
"PUSH(R3);" nl
  "MOV(R4,IMM(-2));" nl
 "
SUB(R4,R5);" nl
 "
MOV(R7,IMM(-3));" nl
 "
LOOPAPPLYSWAP:" nl
 "
CMP(R7,R4);" nl
 "
JUMP_LE(APPLYSWAPDONE);" nl
 "
MOV(R6,FPARG(R4));" nl
 "
MOV(FPARG(R4),FPARG(R7));" nl
 "
MOV(FPARG(R7),R6);" nl
 "
ADD(R4,IMM(1));
" nl
  "SUB(R7,IMM(1));" nl
" 
JUMP(LOOPAPPLYSWAP); " nl
 "
APPLYSWAPDONE:" nl
 

  
  
"PUSH(R5);"nl
"
PUSH(INDD(R0,1));"nl
"
PUSH(FPARG(-1));"nl
"
PUSH(FPARG(-2));"nl
"
ADD(R5,IMM(4));"nl
"
MOV(R3,IMM(3));"nl
"
MOV(R4,IMM(-3));"nl
"
FIXAPPLY:
"nl
"CMP(R5,IMM(0));"nl
"
JUMP_EQ(FIXAPPLYEND);"nl
"
MOV(FPARG(R3),FPARG(R4));"nl
"
SUB(R3,IMM(1));"nl
"
SUB(R4,IMM(1));"nl
"
SUB(R5,IMM(1));"nl

"JUMP(FIXAPPLY);"nl
"

FIXAPPLYEND:"nl
"
DROP(6);"nl
"
MOV(FP,SP);"nl
"
JUMPA(INDD(R0,2));" nl
"
APPLYEMPTY:" nl
"
PUSH(0);"nl
"
PUSH(INDD(R0,1));"nl
"
PUSH(FPARG(-1));"nl
"
PUSH(FPARG(-2));"nl
"
MOV(FPARG(3),FPARG(-3));"nl
"
MOV(FPARG(2),FPARG(-4));"nl
"
MOV(FPARG(1),FPARG(-5));"nl

"MOV(FPARG(0),FPARG(-6));"nl
"
DROP(6);"nl
"
MOV(FP,SP);"nl
"
JUMPA(INDD(R0,2));
"nl
"APPLYENDLAST:" nl
)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





(define sexprs->file
  (lambda (filename str)
    (let ((output (open-output-file filename)))
           (begin   (write str output)
              (close-output-port output))    )))
	   

(define prologue 
	(lambda()
		(string-append	"#include <stdio.h>" nl 
		"#include <stdlib.h>" nl   
	     "#include <string.h>" nl
	"#define DO_SHOW 1" nl 
	"#include \"cisc.h\"" nl nl
         "int main()" nl
	"{" nl 
  "START_MACHINE;" nl
      
	 
	 "JUMP(CONTINUE);" nl nl 
	 "#include \"char.lib\"" nl
	 "#include \"io.lib\"" nl 
	 "#include \"math.lib\""   nl 
     "#include \"string.lib\"" nl
"#include \"system.lib\"" nl  
 "#include \"scheme.lib\"" nl nl						
	 
 "CONTINUE:" nl nl )))


(define epilogue
	(lambda ()
		(string-append  nl "PUSH(R0);" nl  "CALL(WRITE_SOB);" nl "END_PROGRAM:" nl nl
                  
		
		 
		 "STOP_MACHINE;" nl nl
		 "return 0;" nl
		"}")))     
(define parse-and-analayze (lambda (sexpr) (map (lambda (sexp) (pe->lex-pe (parse sexp))) sexpr)))
 
(define printcodegenseq (lambda (ls curr)
       (if (null? ls) curr
        (printcodegenseq (cdr ls) (string-append curr (print-code-gen (parse (car ls))))))))

(define compile-scheme-file (lambda (input-file output-file)
        (let ((input (file->sexprs input-file))
                  (output (open-output-file output-file)))
          (display (string-append       (prologue)
              (printcodegenseq input "")
                  (epilogue)) output)
          (close-output-port output))))
 
(define file->sexprs
  (lambda (filename)
    (let ((input (open-input-file filename)))
      (letrec ((run
                (lambda ()
                  (let ((e (read input)))
                    (if (eof-object? e)
                        (begin (close-input-port  input)
                               '())
                        (cons e (run)))))))
        (run))))) 



