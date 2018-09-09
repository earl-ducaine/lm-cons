; -*-LISP-*-

;	** (c) copyright 1980 massachusetts institute of technology **


(in-package :si)

(defun ttyprint (x)
  (progn
    (let ((^r ^w))
      (declare (special ^r ^w))
      (print x))))

;; functions for hand-testing things
;; (defun tml nil (mslap 'mesa-code-area ms-prog 'cold))


;; ulap-mode can be cold, qfasl
(defun ulap (ulap-output-area fctn ulap-mode)
  (declare (ignore ulap-output-area fctn ulap-mode))
  ;; seems like this should be obsolete
  (error "ulap is currently nop")
  ;; (prog (symtab adr tem t1 fctn-name progsa fasl-fasl-table-entry-index)
  ;;    (cond ((eq ulap-mode 'cold)
  ;; 	    (ulap-micro-entry-setup)))
  ;;    (setq t1 fctn)
  ;;    (setq adr 0)
  ;;    p1	(cond ((null t1) (go p2)))
  ;;    (u-pass1 (car t1))
  ;;    (setq t1 (cdr t1))
  ;;    (go p1)
  ;;    p2	(cond ((eq ulap-mode 'qfasl)
  ;; 	       (fasd-function-header fctn-name)
  ;; 	       (fasd-set-parameter 'fasl-pdp10-inst-area ulap-output-area)
  ;; 	       (setq fasl-fasl-table-entry-index (fasd-save-entry-point fctn-name))))
  ;;    (setq t1 fctn)
  ;;    (setq adr 0)
  ;;    p3	(cond ((null t1) (go p4)))
  ;;    (u-pass2 (car t1))
  ;;    (setq t1 (cdr t1))
  ;;    (go p3)
  ;;    p4	(cond ((eq ulap-mode 'qfasl)
  ;; 	       (fasd-make-micro-code-entry
  ;; 		fctn-name
  ;; 		(ulap-get-argdesc-atom fctn-name)
  ;; 		fasl-fasl-table-entry-index)
  ;; 	       (fasd-function-end))
  ;; 	      ((null ulap-debug)
  ;; 	       (ulap-micro-entry-finalize
  ;; 		fctn-name
  ;; 		(list 'cold-load-relocate-by-mem (list ulap-output-area
  ;; 						       progsa)))))
  ;;    (return nil))
  )


;; Obsolete?
(defparameter *mc-prog* nil)
(defparameter *lap-debug* nil)


;; seems this should be obsolete
(defun tul ()
  (ulap 'micro-compiled-program mc-prog 'cold))

(defun tl (mode)
  (cond ((eq mode 'qfasl)
	 (fasd-initialize)
	 (setq *lap-debug* nil)))
  (qlapp qcmp-output mode))

;; #-lispm
;; (cond ((null (getl 'special '(fexpr fsubr)))
;;        (defun lispm-special fexpr (l)
;; 	      (mapcar (function (lambda (x) (putprop x t 'special)))
;; 		      l))
;;        ))


;; #-lispm (cond ((null (getl 'special '(fexpr fsubr)))
;; 	       (setf  (get 'special  fexpr (l)
;; 			   (mapcar (function (lambda (x) (putprop x t 'special)))
;; 				   l)))))

(defun special-list (x) (eval (cons 'special (symeval x))))

;--q--
;; q fctn specials
(defun loadup-finalize ()
   (mapc (function special-list) system-constant-lists)
   (mapc (function special-list) system-variable-lists))

;;; The documentation that used to be here has been moved to
;;; lmdoc;fasld >

;; also in qmisc
(defun get-alternate (x)
   (prog (y)
    l	(cond ((null x)(return (reverse y))))
	(setq y (cons (car x) y))
	(setq x (cddr x))
	(go l)))

(defparameter *fasl-group-fields*
  (get-alternate *fasl-group-field-values*))

(defun assign-alternate (x)
   (prog nil
    l	(cond ((null x)(return nil)))
	(set (car x) (cadr x))
	(setq x (cddr x))
	(go l)))

(assign-alternate *fasl-group-field-values*)

(defun assign-values (input-list &optional (shift 0) (init 0) (delta 1))
  (prog ()
   lp	(cond ((null input-list)(return init)))
   (set (car input-list) (lsh init shift))
   (setq input-list (cdr input-list))
   (setq init (+ init delta))
   (go lp)))


(assign-values *fasl-ops* 0)

(assign-values
 *fasl-table-parameters* 0)

(defparameter *fasl-constants*
  '(fasl-table-working-offset length-of-fasl-table))

(defparameter *fasl-constant-lists*
  '(fasl-constants
    fasl-group-fields
    fasl-ops
    fasl-table-parameters))

(defparameter *fasl-table-working-offset* 40)

(cond ((> (length *fasl-table-parameters*) *fasl-table-working-offset*)
	;; (ioc v)
	(error "fasl-table-parameter-overflow")))

;; People call this you know, don't go randomly deleting it!
(defun fasl-assign-variable-values ()
  ;; I guess what this used to do is done at top level in this file
 ())
