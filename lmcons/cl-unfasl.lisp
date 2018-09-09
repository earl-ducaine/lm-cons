 ;;;QFASL FILE DISASSEMBLER  -*-LISP-*-
;	** (c) Copyright 1980 Massachusetts Institute of Technology **
 ;FOR BEST RESULTS, RUN THIS INSIDE A CC


(in-package :si)

;; (DECLARE (FIXNUM (LOGLDB-FROM-FIXNUM FIXNUM FIXNUM)
;; 		 (LOGDPB-INTO-FIXNUM FIXNUM FIXNUM FIXNUM)))

;; (DECLARE (FIXNUM (UNFASL-NEXT-NIBBLE)
;; 		 (UNFASL-NIBBLE)
;; 		 (ENTER-UNFASL-TABLE NOTYPE)))

;SYMBOLS FROM QCOM

;; (DECLARE (SPECIAL %%ARRAY-TYPE-FIELD %%ARRAY-LEADER-BIT %%ARRAY-DISPLACED-BIT
;; 		  %%ARRAY-FLAG-BIT %%ARRAY-NUMBER-DIMENSIONS %%ARRAY-LONG-LENGTH-FLAG
;; 		  %%ARRAY-INDEX-LENGTH-IF-SHORT ARRAY-TYPES ARRAY-ELEMENTS-PER-Q
;; 		  DTP-TRAP DTP-NULL DTP-FREE
;; 		  DTP-SYMBOL DTP-FIX DTP-EXTENDED-NUMBER
;; 		  DTP-GC-FORWARD DTP-EXTERNAL-VALUE-CELL-POINTER DTP-ONE-Q-FORWARD DTP-FORWARD
;; 		  DTP-LOCATIVE
;; 		  DTP-LIST
;; 		  DTP-U-ENTRY
;; 		  DTP-FEF-POINTER DTP-ARRAY-POINTER DTP-ARRAY-HEADER
;; 		  DTP-STACK-GROUP DTP-CLOSURE
;; 		  CDR-NIL CDR-NEXT AREA-LIST
;; ))

;FASLOAD TYPE SYMBOLS FROM QCOM OR QDEFS OR SOMEPLACE.

;; (DECLARE (SPECIAL LENGTH-OF-FASL-TABLE FASL-TABLE-WORKING-OFFSET FASL-OPS
;; 		  FASL-SYMBOL-HEAD-AREA FASL-SYMBOL-STRING-AREA
;; 		  FASL-OBARRAY-POINTER FASL-ARRAY-AREA FASL-FRAME-AREA
;; 		  FASL-LIST-AREA FASL-TEMP-LIST-AREA
;; 		  FASL-MICRO-CODE-EXIT-AREA FASL-RETURN-FLAG
;; 		  FASL-GROUP-FLAG FASL-GROUP-BITS FASL-GROUP-TYPE FASL-GROUP-LENGTH
;; 		  %FASL-GROUP-CHECK %FASL-GROUP-FLAG %%FASL-GROUP-LENGTH
;; 		  %FASL-GROUP-TYPE LENGTH-OF-ATOM-HEAD
;; 		  %ARRAY-LEADER-LENGTH %%ARRAY-INDEX-LENGTH-IF-SHORT
;; ))

;; Suitably big so no lossages
(defparameter length-of-fasl-table 4000)

;; fixnum boolean functions -- use CLs?
;; (defmacro logand (x)
;;   `(boole ,@(cons 1 (cdr x))))

;; (defun logior macro (x)
;;   (cons 'boole (cons 7 (cdr x))))

;; (defun logxor macro (x)
;;   (cons 'boole (cons 6 (cdr x))))

;; use CLs version of these?
;; fixnum logldb with constant 1st arg, in-line
;; (defun logldb* macro (x)
;;   ((lambda (p s w)
;;     (list 'boole 1 (1- (lsh 1 s))
;; 	(list 'lsh w (- p))))
;;    (lsh (cadr x) -6)
;;    (boole 1 (cadr x) 77)
;;    (caddr x)))

;; ;fixnum logdpb with constant 2nd arg, in-line
;; (defun logdpb* macro (x)
;;   ((lambda (p s d w)
;;     ((lambda (m)
;; 	(list 'boole 7 (list 'boole 1 (boole 6 -1 (lsh m p)) w)
;; 		       (list 'lsh (list 'boole 1 m d) p)))
;;      (1- (lsh 1 s))))
;;    (lsh (caddr x) -6)
;;    (boole 1 (caddr x) 77)
;;    (cadr x)
;;    (cadddr x)))

;; in this file, only fixnum versions of logldb, logdpb used.
;; (defun logldb macro (x)
;;   (rplaca x 'logldb-from-fixnum))

;; (defun logdpb macro (x)	;not true anymore.  most are logdpb* s anyway
;;  (rplaca x 'logdpb-into-fixnum))


(defparameter *source-directory* (asdf:system-source-directory :lm-cons))

(defprop logldb-from-fixnum (util1 fasl dsk lispm) autoload)

;; (declare (special fasl-table fasl-table-fill-pointer
;; 		  unfasl-indentation
;; 		  unfasl-group-dispatch unfasl-group-dispatch-fast unfasl-group-dispatch-size))

;; (declare (special unfasl-file unfasl-word))

;; so don't get screwed if reload it
(makunbound 'unfasl-group-dispatch)

(export 'unfasl :si)

;; User calls this
(defun unfasl fexpr (filespec)
       (or (boundp 'unfasl-group-dispatch)
	   (initialize-unfasl-environment))
       (setq filespec (mergef filespec '|dsk:lispm;* qfasl|))
       (setq unfasl-word nil)
       (setq unfasl-file (open filespec '(in block fixnum)))
       (or (and (= (unfasl-nibble) 143150)		;check magic id
		(= (unfasl-nibble) 71660))
	   (error '|not a qfasl file| unfasl-file))
       (do ((^r t)
	    (outfiles (list (open (mergef '|* unfasl| filespec) '(out block)))))
	   ()
	 (unfasl-top-level)
	 (close (car outfiles)))
       (close unfasl-file))

(defun unfasl-nibble ()
  (cond ((null unfasl-word)
	 ((lambda (tem)
	      (declare (fixnum tem))
	      (setq unfasl-word (logldb-from-fixnum 0420 tem))
	      (logldb-from-fixnum 2420 tem))
	  (in unfasl-file)))
	(t (prog2 nil unfasl-word (setq unfasl-word nil)))))

(defun ar-1 macro (x)	;just to save typing (only used for fasl-table)
  (list 'arraycall t (cadr x) (caddr x)))

(defun as-1 macro (x)	;just to save typing
  (list 'store (list 'arraycall t (caddr x) (cadddr x)) (cadr x)))

(defun unfasl-top-level nil
  (prog nil
   l	(cond ((eq (unfasl-whack) 'eof) (return t)))
	(go l)))

(defun unfasl-whack nil
  (prog (fasl-table fasl-return-flag unfasl-indentation)
	(setq unfasl-indentation 0)
;	(setq fasl-table (make-array 'fasl-table-area
;					   'art-q-list
;					   (list length-of-fasl-table)
;					   nil
;					   (list fasl-table-working-offset)))
;							;leader for filling
	(setq fasl-table (*array nil t length-of-fasl-table))
		;contents is a number which is a lisp machine q.
	(setq fasl-table-fill-pointer fasl-table-working-offset)
	(initialize-unfasl-table)
;	(fasl-set-mesa-exit-base)
  l	(unfasl-group)
	(cond (fasl-return-flag
;		(adjust-array-size fasl-table 0)
		(*rearray fasl-table)
		(return fasl-return-flag)))
	(go l)
))

(defun initialize-unfasl-table nil
	(as-1 'nr-sym fasl-table fasl-symbol-head-area)
	(as-1 'p-n-string fasl-table fasl-symbol-string-area)
;	(as-1 obarray fasl-table fasl-obarray-pointer)
	(as-1 'user-array-area fasl-table fasl-array-area)
	(as-1 'macro-compiled-program fasl-table fasl-frame-area)
	(as-1 'user-initial-list-area fasl-table fasl-list-area)
	(as-1 'fasl-temp-area fasl-table fasl-temp-list-area)
	(as-1 'micro-code-exit-area fasl-table fasl-micro-code-exit-area)
)

(defun unfasl-group nil
  (prog (fasl-group-flag fasl-group-bits fasl-group-type fasl-group-length)
	(setq fasl-group-bits (unfasl-nibble))
	(cond ((= 0 (logand fasl-group-bits %fasl-group-check))
		(print 'fasl-group-nibble-without-check-bit) (print fasl-group-bits)))
	(setq fasl-group-flag (not (= 0 (logand fasl-group-bits
						  %fasl-group-flag))))
	(setq fasl-group-length (logldb %%fasl-group-length fasl-group-bits))
	(and (= fasl-group-length 377)
	     (setq fasl-group-length (unfasl-nibble)))
	(setq fasl-group-type (logand fasl-group-bits %fasl-group-type))
	(or (< fasl-group-type unfasl-group-dispatch-size)
	    (error '|erroneous fasl group type| fasl-group-type 'fail-act))
	(unfasl-terpri)
	(prin1 (nth fasl-group-type fasl-ops))
	(return
	 (prog2 nil
	  (cond	(nouuo
		 (funcall (arraycall t unfasl-group-dispatch fasl-group-type)))
		(t
		 (subrcall nil (arraycall t unfasl-group-dispatch-fast fasl-group-type))))
	  (cond ((not (zerop fasl-group-length))
		 (terpri) (princ '|fasl-group-count wrong; |)
		 (prin1 fasl-group-length) (princ '| nibbles left over|)
		 (terpri)))
	  ))))

(defun unfasl-terpri nil
  (terpri)
  (do i unfasl-indentation (1- i) (not (> i 0))
    (tyo 40)))

(defun unfasl-next-nibble nil
	(cond ((= 0 fasl-group-length) (error 'fasl-group-overflow nil 'fail-act))
	      (t (setq fasl-group-length (1- fasl-group-length))
		 (unfasl-nibble))))

(defun unfasl-next-nibble-pr nil
  ((lambda (nibble)
     (princ '| [|)
     (prin1 nibble)
     (princ '|]|)
     nibble)
   (unfasl-next-nibble)))

(defun unfasl-next-value nil
  ((lambda (unfasl-indentation)
	(ar-1 fasl-table (unfasl-group)))
   (+ 3 unfasl-indentation)))

(defun enter-unfasl-table (v)
  (cond ((not (< fasl-table-fill-pointer length-of-fasl-table))
	 (error '|fasl table overflow| v 'fail-act))
	(t
	 (as-1 v fasl-table fasl-table-fill-pointer)
	 (princ '|  --> |) (prin1 fasl-table-fill-pointer)
	 (prog2 nil fasl-table-fill-pointer
		    (setq fasl-table-fill-pointer (1+ fasl-table-fill-pointer))))))

;--fasl ops

(defun unfasl-op-err nil
  (princ '| not handled|)
  (cond ((not (zerop fasl-group-length))
	 (princ '| - following nibbles: |)
	 (do i fasl-group-length (1- i) (= i 0)
	     (unfasl-next-nibble-pr)))))

(defun unfasl-op-index nil
  (prog (tem)
    (setq tem (unfasl-next-nibble-pr))
    (tyo 40)
    (tyo 173)
    (princ (ar-1 fasl-table tem))
    (tyo 175)
    (return tem)))

(defun unfasl-op-noop nil
  t)

(defun unfasl-op-string nil
  (unfasl-op-symbol1 t))

(defun unfasl-op-symbol nil
  (and fasl-group-flag (princ '| uninterned|))
  (unfasl-op-symbol1 nil))

(defun unfasl-op-symbol1 (string-flag)
  (prog (tem lst)
	(and string-flag (setq lst (list 42)))
   lp	(cond ((= 0 fasl-group-length) (go x)))
	(setq tem (unfasl-next-nibble))
	;; tem contains two 8-bit lisp machine characters.  200 is a null character.
	(setq lst (cons (logand tem 177) lst))
	(cond ((= (setq tem (lsh tem -8)) 200)
		(go x)))
	(setq lst (cons (logand tem 177) lst))
	(go lp)
   x	(tyo 40)
	(and string-flag
	     (setq lst (cons 42 lst)))
	(mapc (function tyo)
	      (setq lst (nreverse lst)))
        (return (enter-unfasl-table (maknam lst)))))

(defun unfasl-op-package-symbol ()
  (prog (len lst)
	(setq len (1- (unfasl-next-nibble)))
	(do i 0 (1+ i) (not (< i len))
	  (setq lst (cons (unfasl-next-value) lst))	;will print out with double quotes,
	  (tyo 72)					;  too bad.
	  )						;72 is a colon.
	(return
	 (enter-unfasl-table				;is this reasonable?
	  (cons '**package**
	        (nreverse (cons (unfasl-next-value) lst)))))))
		
(defun unfasl-op-list nil
  (unfasl-op-list1 (ar-1 fasl-table fasl-list-area)))

(defun unfasl-op-list1 (area)
  (declare (fixnum list-length))
  (prog (list-length)
	(setq list-length (unfasl-next-nibble-pr))
	(princ '| area=|)
	(prin1 area)
	(and fasl-group-flag (princ '| (dotify)|))
  l	(cond ((= 0 list-length)
		(go x)))
        (unfasl-next-value)
	(setq list-length (1- list-length))
	(go l)
  x	(return (enter-unfasl-table 'list))
))

(defun unfasl-op-temp-list nil
  (unfasl-op-list1 (ar-1 fasl-table fasl-temp-list-area)))

;(defun unfasl-op-fixed nil
;  (prog (ans)
;	(declare (fixnum ans))
;	(setq ans 0)
;  l	(cond ((= 0 fasl-group-length)
;		(go x)))
;	(setq ans (+ (lsh ans 20) (unfasl-next-nibble)))
;	(go l)
;  x	(cond (fasl-group-flag (setq ans (minus ans))))
;        (tyo 40)
;	(prin1 ans)
;	(return (enter-unfasl-table ans))))

;generate a fixnum (or bignum) value.
(defun unfasl-op-fixed nil
  (do ((pos (* (1- fasl-group-length) 20) (- pos 20))
       (c fasl-group-length (1- c))
       (ans 0))
      ((zerop c) (cond (fasl-group-flag (setq ans (minus ans))))
		 (tyo 40)
		 (prin1 ans)
		 (enter-unfasl-table ans))
    (declare (fixnum pos c))
    (setq ans (logdpb (unfasl-next-nibble) (+ (lsh pos 6) 20) ans))))

(defun unfasl-op-array nil
 ((lambda (flag)
   (unfasl-next-value)
   (princ '| =area|)
   (unfasl-next-value)
   (princ '| =type|)
   (unfasl-next-value)
   (princ '| =dimlist|)
   (unfasl-next-value)
   (princ '| =displaced-p|)
   (unfasl-next-value)
   (princ '| =leader|)
   (unfasl-next-value)
   (princ '| =index-offset|)
   (cond (flag
	  (unfasl-next-value)
	  (princ '| =named-structure|)))
   (enter-unfasl-table 'array))
  fasl-group-flag))

(defun unfasl-op-move nil
 (prog (from to)
	(setq from (unfasl-next-nibble-pr))
	(setq to (unfasl-next-nibble-pr))
	(cond ((= to 177777) (return (enter-unfasl-table (ar-1 fasl-table from))))
	      (t (as-1 (ar-1 fasl-table from) fasl-table to)
		 (return to)))))

(defun unfasl-op-frame nil
  (declare (fixnum q-count unboxed-count tem))
  (prog (q-count unboxed-count tem)
	(setq q-count (unfasl-next-nibble))
	(setq unboxed-count (unfasl-next-nibble))
	(setq fasl-group-length (unfasl-next-nibble))
	(princ '| q-count=|) (prin1 q-count)
	(princ '| unboxed-count=|) (prin1 unboxed-count)
	(princ '| group-length=|) (prin1 fasl-group-length)
   l1	(cond ((= 0 q-count) (go l2)))
	(unfasl-next-value)
	(setq tem (unfasl-next-nibble))
	(princ '| cdrc=|) (prin1 (lsh tem -6))
	(or (= 0 (logand 1 (lsh tem -5))) (princ '| flagb|))
	(or (= 0 (logand 20 tem)) (princ '| e-v-c-p|))
	(or (= 0 (logand 400 tem)) (princ '| locative|))
	(or (= 0 (setq tem (logand tem 17)))
	    (progn (princ '| offset=|) (prin1 tem)))
	(setq q-count (1- q-count))
	(go l1)
  l2
  l3	(cond ((= 0 unboxed-count) (go l4)))
        (unfasl-terpri)
	(princ '|   unboxed |)
	(prin1 (logior (unfasl-next-nibble) (lsh (unfasl-next-nibble) 20)))
	(setq unboxed-count (1- unboxed-count))
	(go l3)
  l4	(return (enter-unfasl-table 'fef))
))

(defun unfasl-op-array-push nil
  (unfasl-next-value)
  (unfasl-next-value))

(defun unfasl-op-file-property-list ()
  (unfasl-next-value))

(defun unfasl-op-storein-symbol-value nil
  (unfasl-op-index)
  (unfasl-next-value))

(defun unfasl-op-storein-function-cell nil
  (unfasl-op-index)
  (unfasl-next-value))

(defun unfasl-op-storein-property-cell nil
  (unfasl-op-index)
  (unfasl-next-value))

(defun unfasl-op-storein-array-leader nil
  (princ '| array|) (unfasl-op-index)
  (princ '| subscr|) (unfasl-op-index)
  (princ '| value|) (unfasl-op-index))

(defun unfasl-op-fetch-symbol-value nil
  (enter-unfasl-table (unfasl-next-value)))

(defun unfasl-op-fetch-function-cell nil
  (enter-unfasl-table (unfasl-next-value)))

(defun unfasl-op-fetch-property-cell nil
  (enter-unfasl-table (unfasl-next-value)))

(defun unfasl-op-end-of-whack nil
  (setq fasl-return-flag 'end-of-whack)
  0)

(defun unfasl-op-end-of-file nil
  (setq fasl-return-flag 'eof)
  0)

(defun unfasl-op-soak nil
  (prog (count)
	(setq count (unfasl-next-nibble-pr))
 l	(cond ((= 0 count) (return (unfasl-group))))
	(unfasl-next-value)
	(setq count (1- count))
	(go l)))

(defun unfasl-op-function-header nil		;what?  copied direct from qfasl, though
  (prog (fctn f-sxh)
	(setq fctn (unfasl-next-value))
	(setq f-sxh (unfasl-next-value))
	(return 0)))

(defun unfasl-op-function-end nil
	0)

(defun unfasl-op-set-parameter nil
  (prog (from to)
	(setq to (unfasl-next-value)) (princ '| =to|)
	;(setq from (unfasl-group)) (princ '| =from|)
	(setq from (unfasl-next-value)) (princ '| =from|)
	(return 0)))

(defun unfasl-op-initialize-array nil
  (prog (arrayp num)
	(setq arrayp (unfasl-next-value))
	(setq num (unfasl-next-value))	;# of vals to initialize
	(do idx 0 (1+ idx) (= idx num)
	  (unfasl-next-value))
	(return 0)))

(defun unfasl-op-initialize-numeric-array nil
  (prog (arrayp num)
	(setq arrayp (unfasl-next-value))
	(setq num (unfasl-next-value))	;# of vals to initialize
	(setq fasl-group-length num)
	(do idx 0 (1+ idx) (= idx num)
	  (unfasl-next-nibble))
	(return 0)))

(defun unfasl-op-make-micro-code-entry nil
  (prog nil
	(princ '| fctn|) (unfasl-next-value)
	(princ '| argdesc|) (unfasl-next-value)
	(princ '| entry-index|) (unfasl-next-nibble-pr)
	(cond ((not fasl-group-flag) (princ '| -> function-cell|)))
	(return (enter-unfasl-table 0))))

(defun unfasl-op-save-entry-point nil
  (prog nil
	(return (enter-unfasl-table 0))))

;unfasl-op-micro-code-symbol  wins as much as it can anyway

(defun unfasl-op-micro-to-micro-link nil
	(unfasl-next-value)
	0)

(defun unfasl-op-quote-pointer nil
	(unfasl-next-value)
	0)

(defun unfasl-op-s-v-cell nil
	(unfasl-next-value)
	0)

(defun unfasl-op-funcell nil
	(unfasl-next-value)
	0)

(defun unfasl-op-const-page nil
	(unfasl-next-nibble-pr)
	0)

(defparameter *lispm-dir*
  (cl-fad:merge-pathnames-as-directory #P"/home/rett/dev/common-lisp/lm-cons/"
				       (make-pathname :directory '(:relative "lispm"))))




(defun readfile (file)
  (with-open-file (stream file)
    (loop for expression = (read stream nil :eof)
       collect expression
       while (not (eql expression :eof)))))


(defun readfile (file)
  (with-open-file (stream file)
    (loop for expression = (read stream nil :eof)
       collect expression
       while (not (eql expression :eof)))))


(defun initialize-unfasl-environment ()
  (when (not (boundp 'fasl-ops))
    (format t "reading in qdefs")
    ;; (readfile '(qdefs > dsk lispm))
    (load (cl-fad:merge-pathnames-as-file *lispm-dir* #p"cl-qdefs.lisp"))
    (terpri))
  (setq unfasl-group-dispatch-size (length fasl-ops))
  (setq unfasl-group-dispatch (*array nil t unfasl-group-dispatch-size))
  (setq unfasl-group-dispatch-fast (*array nil t unfasl-group-dispatch-size))
					;(fillarray unfasl-group-dispatch fasl-ops)
  (do ((i 0 (1+ i))
       (l fasl-ops (cdr l)))
      ((null l))
    (store (arraycall t unfasl-group-dispatch i)
	   (implode (cons 'u (cons 'n (explode (car l)))))))
  (do ((i 0 (1+ i))
       (tem))
      ((= i unfasl-group-dispatch-size))
    (cond ((setq tem (get (arraycall t unfasl-group-dispatch i) 'subr))
	   (store (arraycall t unfasl-group-dispatch-fast i) tem))
	  (t
	   (store (arraycall t unfasl-group-dispatch-fast i) (get 'unfasl-op-err 'subr))
	   (or (get (arraycall t unfasl-group-dispatch i) 'expr)
	       (store (arraycall t unfasl-group-dispatch i) 'unfasl-op-err))))))
