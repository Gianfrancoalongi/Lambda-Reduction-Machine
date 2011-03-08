
;; This code is "as is", and may be used freely for educational purposes.
;; 					Copyright. Gianfranco Alongi
;;
;; Gianfranco Alongi presents LISP lambda expression machine	20081225
;;
;; LAMBDA (haskell style) 	MY-REPRESENTATION
;; -----------------------	------------------------------------------
;; (\x . x )			(:l (:h x) (:b x))
;; (\x y z . x z ( x y ))	(:l (:h x y z) (:b x z ( x y )
;; (\x . x (\y . y ))		(:l (:h x) (:b x (:l (:h y) (:b y))))
;; (\x . )			(:l (:h x) (:b ) )
;; (\x . x x)			(:l (:h x) (:b x x ))
;;
;; How to use this piece of LISP:
;;
;; (0) Load file into clisp.
;;
;; (1) Construct a lambda expression using the ml operator
;;	Examples can be seen at end of file (standard combinators)
;;
;; (2)  Use either of the functions together with appropriate arguments
;; 	
;; 	>>>	LAMBDAEXPRESSION	(LAMBDAEXPRESSIONS)
;;	>>	LAMBDAEXPRESSION 	VARIABLE/LAMBDAEXPRESSION
;;	>>?	LAMBDAEXPRESSION
;;	>!>?	LAMBDAEXPRESSION	[VARIABLE/LAMBDAEXPRESSION]
;;	!!!	LAMBDAEXPRESSION
;;	
;;	[A] == A is optional
;;	
;;	>>> A (A_1...)  Automatically reduces A using the list 
;;
;;	>>  A B		Reduces A with the help of B, returns result.
;;
;;	>>? A 		Tries to perform a reduction on A, and returns
;;			the reduced result or the input unchanged.
;;
;;	>!>? A	[B]	Tries to reduce a (using a possible B) and 
;;			outputs the result in prettyprinted form
;;			also returns result.
;;
;;	!!! A		Prettyprints the given lambdaexpression A.
;;
;; (3)  Have fun! The example below shows the use of *twice* (predefined)
;; 	with the toy example  ( twice ident ident 1 )
;;
;; 		(>!>? *twice*)			<-- what you write
;; 		(\F X .  F ( F  X ))		<-- what lisp responds with (ignoring return value)
;;
;;		(>!>? (>!>? *twice*) *ident*)
;;		(\X . (\X .  X )((\X .  X ) X ))
;;
;;		(>!>? (>!>? (>!>? *twice*) *ident*) *ident*)
;;		((\X .  X )((\X .  X )(\X .  X )))
;;
;;		Here we must do a single internal reduction...
;;		This is done by calling >!>? without additional arguments
;;		As you can see here ------------------------------
;;		                                                 |
;;		                                                 v
;;		(>!>? (>!>? (>!>? (>!>? *twice*) *ident*) *ident*))
;;		((\X .  X )(\X .  X ))
;;
;;		Yet another internal reduction....
;;
;;		(>!>? (>!>? (>!>? (>!>? (>!>? *twice*) *ident*) *ident*)))
;;		(\X .  X )
;;
;;		Adding the final 1
;;		
;;		(>!>? (>!>? (>!>? (>!>? (>!>? (>!>? *twice*) *ident*) *ident*))) 1)
;;		1
;;		
;; (4) A cooler example using the automatized reduction and list of arguments
;;
;;	(eval (>>> *twice* `(,*twice* ,*inc* 0)))		;;<-- input to clisp prompt
;;
;;	(\X . (\F X .  F ( F  X ))((\F X .  F ( F  X )) X ))
;;	((\F X .  F ( F  X ))((\F X .  F ( F  X ))(\X . ( +  X  1 ))))
;;	(\X . ((\F X .  F ( F  X ))(\X . ( +  X  1 )))(((\F X .  F ( F  X ))(\X . ( +  X  1 ))) X ))
;;	(((\F X .  F ( F  X ))(\X . ( +  X  1 )))(((\F X .  F ( F  X ))(\X . ( +  X  1 ))) 0 ))
;;	((\X . (\X . ( +  X  1 ))((\X . ( +  X  1 )) X ))((\X . (\X . ( +  X  1 ))((\X . ( +  X  1 )) X )) 0 ))
;;	((\X . ( +  X  1 ))((\X . ( +  X  1 ))((\X . (\X . ( +  X  1 ))((\X . ( +  X  1 )) X )) 0 )))
;;	( + ((\X . ( +  X  1 ))((\X . (\X . ( +  X  1 ))((\X . ( +  X  1 )) X )) 0 )) 1 )
;;	( + ( + ((\X . (\X . ( +  X  1 ))((\X . ( +  X  1 )) X )) 0 ) 1 ) 1 )
;;	( + ( + ((\X . ( +  X  1 ))((\X . ( +  X  1 )) 0 )) 1 ) 1 )
;;	( + ( + ( + ((\X . ( +  X  1 )) 0 ) 1 ) 1 ) 1 )
;;	( + ( + ( + ( +  0  1 ) 1 ) 1 ) 1 )
;;	4
;;

(defun >>> (a &optional params)
  "Atuomatically reduce a given lambda expression [a] as far as possible 
  using the given parameters [params]. Output the intermediate result in 
  each step"
  (let* ((given (fixpoint 'simplify a)))
    (cond ((or (atom given)
	       ;; Reduced to single atom
	       (and (null params)
		     (equal given (>>? given)))
	       ;; Consumed all input and no 
	       ;; further reduction possible
	       (and (not (lambdap given))
		    (equal (>!>? given) given))
	       ;; Reduced to something not a lambda expr
	       ;; and no further reduction possible
	       )
	   ;; Return reduced ground form
	   given)
	
	   ((and (> (length given) 1)
		(not (lambdap given)))
	   ;;Reduction from left possible
	   ;;
	   (>>> (>!>? given) params))
	
	  ((null params)
	   ;;No more input parameter
	   ;;
	   (if (equalp given (>>? given))
	     ;;No more internal reduction possible
	     ;;Output and return
	     (>!>? given)
	     ;; More internal reduction possible
	     ;;
	     (>>> (>!>? given))))

	   ;; More input parameters
	   (t (if (lambdap given)
		;; Given a lambda expression
		;; consume input parameters
		(>>>  (>!>? given (car params)) (cdr params))
		;; Otherwise perform internal reduction
		;; until fixpoint
		(if (equalp given (>>? given))
		  given
		  (>>> (>!>? given) params))))) ))

(defun fixpoint (op elem)
  "Apply the operand <op> on the element <elem> until fixpoint
  for <op> is reached"
  (if (equal (funcall op elem) elem)
    elem
    (fixpoint op (funcall op elem))))

(defun >!>? (lambdaexpr &optional extra)
  "Perform reduction and output at the same time.
  Returns the result of the reduction."
  (let ((given (fixpoint 'simplify lambdaexpr)))
    (cond ((null extra)
	   (!!! (>>? given) )
	   (format t "~%")
	   (>>? given ))
	  (t (!!! (>> given extra)) 
	     (format t "~%")
	     (>> given extra)))))

(defun simplify (lambdaexpr) 
  "Simplify all expressions :: (e) ==> e"
  (cond ((atom lambdaexpr)
	 lambdaexpr)
	;;Done when it's just an atom
	;;
	((and (listp lambdaexpr)
	      (eq (length lambdaexpr) 1))
	      (car (mapcar 'simplify lambdaexpr)))
	;; Mapcar on the lambdaexpression
	(t (mapcar 'simplify lambdaexpr))))

(defun <$> (test elems pre)
  "Process list pairwise, test pairs with test,
  if succeed, return (pre elem1 elem2 rest) else nil"
 (if (and (listp elems)
	  (> (length elems) 1))
   (if (funcall test (car elems) (cadr elems))
     `(,pre ,(car elems) ,(cadr elems) ,(cddr elems))
     (<$> test (cdr elems) (append pre `(,(car elems))) ))
   nil))

(defun ml (head body) 
  "Creates a lambda expression using a head and a body."
 `(:l (:h ,@head) (:b ,@body)))

(defun x\y (x y l)
  "Substitute all occurences of x with y in all levels of l
  as long as element of l not is a lambdaexpr"
  (cond ((lambdap l) l)
	((atom l) 
	 (if (eq x l) y l))
	(t (mapcar (lambda (z) (x\y x y z)) l))))

(defun >>? (lambdaexpr)
  "Searches for two lambda expressions and performs a reduction 
  if possible; otherwise it returns the same expression"
  (if (atom lambdaexpr)
    lambdaexpr
    (let* ((x (<$> (lambda (x y) (lambdap x)) lambdaexpr ())))
      (if (eq x nil)
	(mapcar '>>? lambdaexpr)
	(let* ((pre (car x))
	       (elem1 (cadr x))
	       (elem2 (caddr x))
	       (post (cadddr x)))
	  (append pre `(,(>> elem1 elem2)) post))))))

(defun >> (lambL lambR)
  "Performs a beta reduction given a lambda expression and an expr reduces 
  (\x y z. y z) (\x.x) into (\y z. y z)"
  (if (> (arity lambL) 0)
      (let* ((vars (cdadr lambL))
	     (bound (first vars))
	     (newhead (cdr vars))
	     (newbody (x\y bound lambR (cdaddr lambL))))
	(cond ((eq newhead nil)
	       (fixpoint 'simplify newbody))

	      ;; If no more vaiables in lexprL can be bound
	      ;; then new lambda expression is everything contained 
	      ;; within the substituted body
	     (T (ml newhead newbody))))
     ;; Arity of left lambdaexpr is 0, should be > 0.
     (format t "Lexpr has arity == 0")))

(defun arity (lexpr) 
  "Returns the arity of the lambda expression, or nil"
  (if (lambdap lexpr)
    (length (cdadr lexpr)) nil))

(defun !!! (lexpr)
  "Outputs a lambda expression in human readable form"
  (cond ((atom lexpr)
	 ;;Output a single variable or parenthesis
	 (format t " ~a " lexpr))

	((lambdap lexpr)
	 ;;Output the lambda expression
	 (format t "(")
	 (print-head (cadr lexpr))
	 (!!! (caddr lexpr))
	 (format t ")"))

    	((headp lexpr)
	 ;;Output a head expr
	 (print-head lexpr))

	((bodyp lexpr)
	 ;;Output a body by outputting each (:b E E E)
	 ;;as "(" .... "(" .. ")" ..  ")"
	 (mapcar '!!! (cdr lexpr)))

	((groupp lexpr)
	 ;;Output each group ( x ( x y ) (z x y ))
	 ;;as "( x ( x y ) ( z x y ))"
	 (format t "(")
	 (mapcar '!!! lexpr)
	 (format t ")"))))

(defun print-head (head-expr)
  "Output a head expression in nice form. (:head X Y Z)"
  (format t "\\")
  (mapcar (lambda (x) (format t "~a " x)) (cdr head-expr))
  (format t ". "))

(defun markp (lexpr mark)
  (and (listp lexpr)
       (eq (car lexpr) mark)))

(defun lambdap (lexpr)
  (markp lexpr :l))

(defun headp (lexpr)
  (markp lexpr :h))

(defun bodyp (lexpr)
  (markp lexpr :b))

(defun groupp (lexpr)
  "Check that this is a group == a list of expressions 
  without a preceding :b :l :h"
  (and (listp lexpr)
       (not (member (car lexpr) `(:b :l :h)))))

;; Some standard combinators +++++++++++++++++++++++++++++++++++++++++++++

;; identity
(defvar *ident* (ml `(x) `(x)))

;; K combinator
(defvar *k* (ml `(x y) `(x)))

;; K* combinator
(defvar *ks* (ml `(x y) `(y)))

;; S combinator
(defvar *s* (ml `(x y z) `(x z ( x y ))))

;; twice
(defvar *twice* (ml `(f x) `(f ( f x) )))

;; inc
(defvar *inc* (ml `(x) `(+ x 1)))

;; Church numerals
(defvar *zero*	(ml `(f x) 	`(x)))
(defvar *succ*	(ml `(n f x)	`(f(n f x))))
(defvar *add*	(ml `(n m f x)	`(n f (m f x))))
(defvar *mult* 	(ml `(n m f)	`(n ( m f))))

;; Some test 
(defun tests ()
  (list 
	`(,(format t "Add 0 with 0~%")
	  ,(>>> *add* `(,*zero* ,*zero*))
	  ,(format t "~%"))

	`(,(format t "Multiply 0 with 0~%")
	  ,(>>> *mult* `(,*zero* ,*zero*))
	  ,(format t "~%"))

	`(,(format t "Add 0 with 1~%")
	  ,(>>> *add* `(,*zero* ,(>>> *succ* `(,*zero*))))
	  ,(format t "~%"))

	`(,(format t "Multuply 0 with 1~%")
	  ,(>>> *mult* `(,*zero* ,(>>> *succ* `(,*zero*))))
	  ,(format t "~%"))

	`(,(format t "Multiply 1 with 1~%")
	  ,(>>> *mult* `(,(>>> *succ* `(,*zero*)) ,(>>> *succ* `(,*zero*))))
	  ,(format t "~%"))))
