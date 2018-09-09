;;fast dumper (maclisp model) -*-lisp-*-

;; ** (c) copyright 1980 massachusetts institute of technology **
;;
;; read lispm;macros > in before trying to run this interpretively

;; (declare (cond ((status feature lispm))
;; 	       ((null (memq 'newio (status features)))
;; 		(break 'you-have-to-compile-this-with-qcompl t))
;; 	       ((null (get 'if-for-maclisp 'macro))
;; 		(load '(macros > dsk lispm))
;; 		(load '(defmac fasl dsk lispm2))
;; 		(load '(lmmac > dsk lispm2))
;; 		(macros t))))	;send over the rest of the macros in this file

;; (declare (fixnum (q-char-length notype)
;; 		 (q-char-chomp notype)))

;; (declare (special array-elements-per-q array-dim-mult array-types
;; 	array-type-shift array-displaced-bit array-leader-bit array-long-length-flag
;; 	%array-max-short-index-length))

;; (declare (special fasd-buffer-array fasd-file))

;; (declare (special fasd-table fasd-group-length fasl-table-parameters))

;; (declare (special %fasl-group-check
;;    %fasl-group-flag %fasl-group-length
;;    fasl-group-length-shift %fasl-group-type
;;   fasl-op-err fasl-op-index fasl-op-symbol fasl-op-package-symbol fasl-op-list
;;   fasl-op-temp-list fasl-op-fixed fasl-op-float
;;   fasl-op-array fasl-op-eval fasl-op-move
;;   fasl-op-frame fasl-op-array-push fasl-op-storein-symbol-value
;;   fasl-op-storein-function-cell fasl-op-storein-property-cell
;;   fasl-op-storein-array-leader
;;   fasl-op-fetch-symbol-value fasl-op-fetch-function-cell
;;   fasl-op-fetch-property-cell fasl-op-apply fasl-op-end-of-whack
;;   fasl-op-end-of-file fasl-op-soak fasl-op-function-header fasl-op-function-end
;;   fasl-op-make-micro-code-entry fasl-op-save-entry-point fasl-op-micro-code-symbol
;;   fasl-op-micro-to-micro-link fasl-op-misc-entry fasl-op-quote-pointer fasl-op-s-v-cell
;;   fasl-op-funcell fasl-op-const-page fasl-op-set-parameter
;;   fasl-op-initialize-array fasl-op-unused fasl-op-unused1
;;     fasl-op-unused2 fasl-op-unused3 fasl-op-unused4
;;   fasl-op-unused5 fasl-op-unused6
;;   fasl-op-string fasl-op-eval1
;;   fasl-nil fasl-evaled-value fasl-tem1 fasl-tem2 fasl-tem3
;;     fasl-symbol-head-area
;;     fasl-symbol-string-area fasl-obarray-pointer fasl-array-area
;;     fasl-frame-area fasl-list-area fasl-temp-list-area
;;     fasl-micro-code-exit-area
;;   fasl-table-working-offset ))

;; (declare (fixnum (fasd-table-enter notype notype))
;; 	 (notype (fasd-start-group notype fixnum fixnum)
;; 		 (fasd-fixed fixnum)
;; 		 (fasd-initialize-array fixnum notype)
;; 		 (fasd-index fixnum)
;; 		 (fasd-eval fixnum)
;; 		 (fasd-nibble fixnum)))

(defun fasd-start-group (flag length type)
  (prog (out-len)
	(setq fasd-group-length length)
        (setq out-len (lsh (cond ((>= length 377) 377)
                                 (t length))
                           (- fasl-group-length-shift)))
	(fasd-nibble (+ %fasl-group-check
			(+ (cond (flag %fasl-group-flag) (t 0))
			   (+ out-len
			      type))))
	(and (>= length 377)
	     (fasd-nibble length))
	(return nil)))

(defun fasd-symbol (sym)
  (cond ((get sym 'magic-package-flag)
	 (fasd-package-symbol (get sym 'magic-package-flag)))
	(t (fasd-write-symbol sym fasl-op-symbol))))

(defun fasd-string (string) (fasd-write-symbol string fasl-op-string))

(defun fasd-write-symbol (sym group-type)
  (prog (fasd-group-length chlist c0 c1)
	(declare (fixnum c0 c1))
	(setq chlist (exploden sym))
	(fasd-start-group nil (// (1+ (q-char-length chlist)) 2) group-type)
 l	(cond ((null chlist)
		(return nil)))
;	(setq c0 (car chlist))
;	(setq c1 (cond ((cdr chlist) (cadr chlist))
;				     (t 200)))
;	(cond ((and (> c0 7)
;		    (< c0 16))
;		(setq c0 (+ 200 c0))))
;	(cond ((and (> c1 7)
;		    (< c1 16))
;		(setq c1 (+ 200 c1))))
	(setq c0 (q-char-chomp chlist))
	(setq c1 (cond ((null (setq chlist (q-char-advance chlist)))
			  200)
		       (t (q-char-chomp chlist))))
	(fasd-nibble (+ c0 (lsh c1 8)))
	(setq chlist (q-char-advance chlist))
	(go l)))

;; for a:b:c, we are given the list (a b c).
(defun fasd-package-symbol (list)
    (fasd-start-group nil 1 fasl-op-package-symbol)
    (fasd-nibble (length list))
    (do l list (cdr l) (null l)
      (fasd-string (car l))
      (fasd-table-enter 'list (car l))))

(defun fasd-constant (s-exp)
  (prog (fasd-group-length tem bsize dotp)
	(cond ((setq tem (fasd-table-search 'list s-exp))
		(fasd-start-group nil 1 fasl-op-index)
		(fasd-nibble tem)
		(return tem))
	      ((fixp s-exp) (fasd-fixed s-exp) (go x))
	      ((floatp s-exp) (fasd-float s-exp) (go x))
	      ((atom s-exp) (fasd-symbol s-exp) (go x))
	      ((eq (car s-exp) '**package**)
	       (fasd-package-symbol (cdr s-exp)) (go x))
	      ((eq (car s-exp) '**string**)
		(fasd-string (cadr s-exp)) (go x))
	      ((eq (car s-exp) '**execution-context-eval**)
	        (fasd-eval1 (cdr s-exp))))
	(setq bsize (length-term-by-atom s-exp))
	(setq tem s-exp)
	(cond ((cdr (last-term-by-atom s-exp))
		(setq bsize (1+ bsize))
		(setq dotp t)
		(setq tem (undotify s-exp))))
	(fasd-start-group dotp 1 fasl-op-list)
	(fasd-nibble bsize)
  l	(cond ((null tem) (go x)))
	(fasd-constant (car tem))
	(setq tem (cdr tem))
	(go l)
  x	(return (fasd-table-enter 'list s-exp))
))

(defun fasd-fixed (n)
 (prog (fasd-group-length nmag nlength)
	(setq nmag (abs n)
	      nlength (// (+ (haulong nmag) 15.) 16.))
	(cond ((> (haulong nmag) 64.)
	       (barf n 'bignum-too-long-for-fasd-fixed 'warn)))  ;until new byte spec.
	(fasd-start-group (< n 0) nlength fasl-op-fixed)
	(do ((pos (* 20 (1- nlength)) (- pos 20))
	     (c nlength (1- c)))
	    ((zerop c))
	    (fasd-nibble (logldb (+ (lsh pos 6) 20) nmag)))))

(defun fasd-float (n)
 (declare (flonum n))
 (prog (fasd-group-length exp mantissa)
        (setq mantissa (logand (lsh n 0) 777777777)
	      exp (lsh n -27.))
	(cond ((minusp n)
	       (setq exp (logand (1- (- exp)) 377)
		     mantissa (+ 1_28. mantissa))
	       ;; this is to take care of the -1/2 case which is different in 10
	       (cond ((= mantissa 3_28.)
		      (setq exp (1- exp) mantissa 1_28.)))))
	(cond ((not (zerop n))
	       (setq exp (+ exp 1600))))		;conversion from excess 200 to 2000
	(fasd-start-group nil 3 fasl-op-float)
	(fasd-nibble exp)
	(fasd-nibble (lsh mantissa -12.))
	(fasd-nibble (logand (lsh mantissa 3) 177777))))

(defun fasd-micro-code-symbol (sym)
 (prog (fasd-group-length tem)
	(fasd-start-group nil 1 fasl-op-micro-code-symbol)
	(break obsolete t)))

(defun fasd-misc-entry (sym)
  (prog (fasd-group-length tem)
	(fasd-start-group nil 1 fasl-op-micro-code-symbol)
	(cond ((null (setq tem (get sym 'qlval)))
		(barf sym 'undefined-misc-entry 'barf)))
	(fasd-nibble (- tem 200))))	;area starts with misc-entry 200

(defun fasd-quote-pointer (s-exp)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-quote-pointer)
	(fasd-constant s-exp)))

(defun fasd-s-v-cell (sym)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-s-v-cell)
	(fasd-constant sym)))

(defun fasd-funcell (sym)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-funcell)
	(fasd-constant sym)))

(defun fasd-const-page (const-page-index)
  (prog (fasd-group-length)
	(fasd-start-group nil 1 fasl-op-const-page)
	(fasd-nibble const-page-index)))

(defun fasd-micro-to-micro-link (sym)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-micro-to-micro-link)
	(fasd-constant sym)))

(defun fasd-function-header (fctn-name)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-function-header)
	(fasd-constant fctn-name)
	(fasd-constant '0)))

(defun fasd-save-entry-point (fctn-name)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-save-entry-point)
	(return (fasd-table-enter 'entry-point fctn-name))))

(defun fasd-make-micro-code-entry (fctn-name argdesc-atom entry-fasl-index)
  (prog (fasd-group-length)
	(fasd-start-group nil 1 fasl-op-make-micro-code-entry)
	(fasd-constant fctn-name)
	(fasd-constant argdesc-atom)
	(fasd-nibble entry-fasl-index)
	(return (fasd-table-enter 'uentry-index fctn-name)) ))

(defun fasd-function-end nil
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-function-end)))

(defun fasd-end-whack nil
  (prog ()			;starting new whack so let fasd-group-length get
				;set to 0
	(fasd-start-group nil 0 fasl-op-end-of-whack)
	(fasd-table-initialize)))

(defun fasd-end-of-file nil
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-end-of-file)))

(defun fasd-end-file nil
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-end-of-file)))

(defun fasd-set-parameter (param val)
  (prog (fasd-group-length c-val)
	(cond ((null (setq c-val (assq param fasd-table)))
		(barf param 'unknown-fasl-parameter 'barf)))
	(cond ((equal val (cdr c-val))(return nil)))
	(fasd-start-group nil 0 fasl-op-set-parameter)
	(fasd-constant param)
	(fasd-constant val)
))

(defun fasd-storein-array-leader (array subscr value)
   (prog (fasd-group-length)
	(fasd-start-group nil 3 fasl-op-storein-array-leader)
	(fasd-nibble array)
	(fasd-nibble subscr)
	(fasd-nibble value)
	(return 0)))

(defun fasd-storein-function-cell (sym idx)	;idx an fasd-table index that has
   (prog (fasd-group-length)			;stuff desired to store.
	(fasd-start-group nil 1 fasl-op-storein-function-cell)
	(fasd-nibble idx)
	(fasd-constant sym)
	(return 0)))

(defun fasd-storein-symbol-value (sym idx)
   (prog (fasd-group-length)
	(fasd-start-group nil 1 fasl-op-storein-symbol-value)
	(fasd-nibble idx)
	(fasd-constant sym)
	(return 0)))

(defun fasd-storein-property-cell (sym idx)
   (prog (fasd-group-length)
	(fasd-start-group nil 1 fasl-op-storein-property-cell)
	(fasd-nibble idx)
	(fasd-constant sym)
	(return 0)))

(defun fasd-initialize-array (idx init)
   (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-initialize-array)
	(fasd-index idx)
	(fasd-constant (length init))
   l	(cond ((null init) (return 0)))
	(fasd-constant (car init))
	(setq init (cdr init))
	(go l)))

(defun fasd-index (idx)
  (fasd-start-group nil 1 fasl-op-index)
  (fasd-nibble idx))

;(defun fasd-mesa-fef (storage-length max-exit-vector-usage max-ip-pdl-usage
;			 fctn-name fast-option-q)
;  (prog (fasd-group-length)
;	(fasd-start-group nil 3 fasl-op-mesa-fef)
;	(fasd-nibble storage-length)
;	(fasd-nibble max-exit-vector-usage)
;	(fasd-nibble max-ip-pdl-usage)
;	(fasd-constant fctn-name)
;	(fasd-constant fast-option-q)))
;
;(defun fasd-mesa-instruction (wd)
;  (prog (fasd-group-length)
;	(fasd-start-group nil 1 fasl-op-mesa-instruction)
;	(fasd-nibble wd)))
;
;(defun fasd-mesa-funcell-plugin (sym arg-q)
;  (prog (fasd-group-length)
;	(fasd-start-group arg-q 0 fasl-op-mesa-funcell-plugin)
;	(fasd-constant sym)
;	(cond (arg-q (fasd-constant arg-q))) ))
;
;(defun fasd-mesa-s-v-cell-plugin (sym)
;  (prog (fasd-group-length)
;	(fasd-start-group nil 0 fasl-op-mesa-s-v-cell-plugin)
;	(fasd-constant sym)))
;
;(defun fasd-mesa-quote-plugin (s-exp)
;  (prog (fasd-group-length)
;	(fasd-start-group nil 0 fasl-op-mesa-quote-plugin)
;	(fasd-constant s-exp)))
;
;(defun fasd-mesa-const-page-plugin (const-page-index)
;  (prog (fasd-group-length)
;	(fasd-start-group nil 1 fasl-op-mesa-const-page-plugin)
;	(fasd-nibble const-page-index)))
;
;(defun fasd-mesa-function-end nil
;  (prog (fasd-group-length)
;	(fasd-start-group nil 0 fasl-op-mesa-function-end)))

(defun fasd-eval (idx)
  (prog (fasd-group-length)
	(fasd-start-group nil 1 fasl-op-eval)
	(fasd-nibble idx)
	(return fasl-evaled-value)))

(defun fasd-eval1 (sexp)
  (prog (fasd-group-length)
	(fasd-start-group nil 0 fasl-op-eval1)
	(fasd-constant sexp)
	(return (fasd-table-enter 'evaled-value sexp))))
;--

(defun fasd-store-value-in-function-cell (sym val)
	(fasd-storein-function-cell sym (fasd-constant val)))

(defun fasd-make-array (&rest n)
  (cond ((or (< n 5) (> n 6))
	 (error '|wrong number of arguments to fasd-make-array| n)))
  (let ((fasd-group-length 0))
    (fasd-start-group (> n 5) 0 fasl-op-array)
    (fasd-constant (arg 1))
    (fasd-constant (arg 2))
    (fasd-constant (arg 3))
    (fasd-constant (arg 4))
    (fasd-constant (arg 5))
    ;; index offset
    (fasd-constant nil)
    (and (> n 5)
	 (fasd-constant (arg 6)))
    (fasd-table-enter 'array-pointer (gensym))))

(defun undotify (x)
	(cond ((or (atom x) (null (cdr x))) x)
	      ((atom (cdr x)) (list (car x) (cdr x)))
	      (t (cons (car x) (undotify (cdr x))))))

(defun fasd-table-enter (type data)
  (prog nil
	(nconc fasd-table (list (cons type data)))
	(return (1- (length fasd-table)))))

(defun fasd-table-length () (length fasd-table))

(defun fasd-table-set (type data)
 (prog (tem)
	(setq tem fasd-table)
  l	(cond ((null tem) (barf type 'bad-fasd-parameter 'barf))
	      ((eq (caar tem) type)
		 (rplacd (car tem) data)
		 (return nil)))
	(setq tem (cdr tem))
	(go l)))

(defun fasd-table-lookup (data) (fasd-table-search 'list data))

(defun fasd-table-search (type data)
 (prog (c tem)
       (and (eq type 'list)
	    (numberp data)
	    (return nil))
	(setq c 0)
	(setq tem fasd-table)
  l	(cond ((null tem) (return nil))
	      ((and (eq (caar tem) type)
		    (eq (cdar tem) data))
		(return c)))
	(setq c (1+ c))
	(setq tem (cdr tem))
	(go l)))

(defun fasd-initialize nil
	(fasd-table-initialize))

(defun fasd-table-initialize nil
  (prog (tem)
	(setq fasd-group-length 0)
	(setq fasd-table nil)
	(setq tem (reverse fasl-table-parameters))
  l1	(cond ((not (= (length tem) fasl-table-working-offset))
		(setq tem (cons 'unused tem))
		(go l1)))
  l	(cond ((null tem) (go x)))
	(setq fasd-table (cons (list (car tem))
			       fasd-table))
	(setq tem (cdr tem))
	(go l)
  x	(fasd-table-set 'fasl-symbol-head-area 'nrsym) ;set things up like
						;initialize-fasl-table does at fasl time
	(fasd-table-set 'fasl-symbol-string-area 'p-n-string)
	(fasd-table-set 'fasl-array-area 'user-array-area)
	(fasd-table-set 'fasl-frame-area 'macro-compiled-program)
	(fasd-table-set 'fasl-list-area 'user-initial-list-area)
	(fasd-table-set 'fasl-temp-list-area 'fasl-temp-area)
	(fasd-table-set 'fasl-micro-code-exit-area 'micro-code-exit-area)
	(return t)))

;dump a group to evaluate a given form and return its value.
;if optimize is set, setq and defun are handled specially,
;in a way appropriate for the top level of fasdump or qc-file.
(defun fasd-form (form optimize)
   (cond ((or (memq form '(t nil))
	      (and (not (atom form))
		   (memq (car form) '(**package** **string**)))
	      (numberp form))
	  (fasd-constant form))
	 ((atom form) (fasd-random-form form))
	 ((eq (car form) 'quote)
	  (fasd-constant (cadr form)))
	 ((not optimize)
	  (fasd-random-form form))
	 ((eq (car form) 'setq)
	  (fasd-setq form))
         ((eq (car form) 'declare)
          (mapc (function fasd-declaration) (cdr form)))
	 (t (fasd-random-form form))))

(defun fasd-declaration (dcl)
    (and (memq (car dcl) '(special unspecial))
         (fasd-form dcl nil)))

;dump something to eval some random form (which is the argument).
(defun fasd-random-form (frm)
    (fasd-eval (fasd-constant frm)))

;this is an old name for the same thing as fasd-random-form.
(defun fasdump-eval (lst)
  (prog (idx)
	(setq idx (fasd-constant lst))
	(return (fasd-eval idx))))

(defun fasd-setq (form) (fasdump-setq (cdr form)))

(defun fasdump-setq (pair-list)
  (prog (idx)
   l	(cond ((null pair-list) (return nil))
	      ((not (atom (car pair-list)))
		(barf (car pair-list) 'fasdump-setq 'data)
		(go e))
	      (t (setq idx (fasd-form (cadr pair-list) nil))))
	(fasd-storein-symbol-value (car pair-list) idx)
  e	(setq pair-list (cddr pair-list))
	(go l)))

;(defun fasd-nibble (x) (print x))

(defun fasd-nibble (x)
  (setq x (logand 177777 x))
  (let ((tem 0))
    (declare (fixnum tem))
    (store (arraycall fixnum fasd-buffer-array 0)
	   (cond ((minusp (setq tem (arraycall fixnum fasd-buffer-array 0)))	;first halfword
		  x)
		 (t (out fasd-file	;second halfword
			 (lsh (+ (lsh tem 16.) x) 4))
		    -1))))
  nil)

(defun fasd-close (final-name)
  (and (plusp (arraycall fixnum fasd-buffer-array 0))
       (fasd-nibble 0))		;force
  (and final-name (renamef fasd-file final-name))
  (close fasd-file))

(defun fasd-open (file)
  (setq file (mergef '((* *) _qcmp_ output) file))
  (setq fasd-file (open file '(out fixnum block)))
  (or (boundp 'fasd-buffer-array)
      (setq fasd-buffer-array (*array nil 'fixnum 1)))	;to avoid number consing
  (store (arraycall fixnum fasd-buffer-array 0) -1)	;reset buffered back halfword
  (fasd-nibble 143150)					;magic
  (fasd-nibble 71660)					;more magic - sixbit/qfasl/
  t)

(defun fasdump-array (name area array-type dimlist displaced-p leader initialization)
  (prog (idx)
	(cond ((equal dimlist '(**)) (setq dimlist (list (length initialization)))))
	(setq idx (fasd-make-array area array-type dimlist displaced-p leader))
	(cond ((atom name) (fasd-storein-function-cell name idx))
	      ((and (eq (car name) 'value-cell)
		    (atom (cadr name)))
		(fasd-storein-symbol-value (cadr name) idx))
	      (t (barf name 'bad-array-name 'warn)))
	(cond (initialization (fasd-initialize-array idx initialization)))
	))
