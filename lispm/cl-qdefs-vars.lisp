; -*-LISP-*-

;	** (c) copyright 1980 massachusetts institute of technology **

(in-package :si)

;; Elements in q-corresponding-variable-list are symbols whose values
;; in maclisp are lists all of whose members are system contants.
;; these system constants have maclisp values and are made to have the
;; identical values in lisp machine lisp.
(defparameter *q-corresponding-variable-lists*
  '(;; rtb-rtb-bits rtb-rts-bits rtb-rto-ops
    ;; rtb-misc rtm-ops readtable-%%-bits
    adi-fields
    adi-kinds
    adi-storing-options
    area-list
    arg-desc-fields
    array-fields
    array-leader-fields
    array-miscs
    array-types
    chaos-buffer-leader-qs
    chaos-hardware-symbols
    disk-hardware-symbols
    disk-rq-hwds
    disk-rq-leader-qs
    fasl-constant-lists
    fasl-constants
    fasl-group-fields
    fasl-ops
    fasl-table-parameters
    fef-arg-syntax
    fef-des-dt
    fef-functional
    fef-init-option
    fef-name-present
    fef-quote-status
    fef-specialness
    fefh-constants
    fefhi-fields
    fefhi-indexes
    hardware-memory-sizes
    header-fields
    header-types
    instance-descriptor-offsets
    linear-pdl-fields
    linear-pdl-qs
    m-error-substatus-fields
    m-flags-fields
    micro-stack-fields
    misc-q-variables
    numeric-arg-desc-fields
    page-hash-table-fields
    q-cdr-codes
    q-data-types
    q-fields
    q-header-types
    q-lisp-constants
    q-region-bits
    reg-pdl-leader-qs
    scratch-pad-variables
    sg-inst-dispatches
    sg-state-fields
    sg-states
    special-pdl-leader-qs
    stack-group-head-leader-qs
    system-communication-area-qs
    system-constant-lists
    system-variable-lists
    unibus-channel-qs))

;; Elements in system-constant-lists are symbols whose maclisp and
;; lisp machine values are lists of symbols which should get
;; system-constant property for the compiler.  normally should be very
;; close to q-corresponding-variables-lists
(defparameter *system-constant-lists*
  '(;; array-miscs
    ;; rtb-misc rtm-ops
    ;; readtable-%%-bits
    ;; rtb-rtb-bits
    ;; rtb-rts-bits rtb-rto-ops
    adi-fields
    adi-kinds
    adi-storing-options
    area-list
    arg-desc-fields
    array-fields
    array-leader-fields
    array-miscs
    array-types
    at
    be
    chaos-buffer-leader-qs
    chaos-hardware-symbols
    disk-hardware-symbols
    disk-rq-hwds
    disk-rq-leader-qs
    fasl-constant-lists
    fasl-constants
    fasl-group-fields
    fasl-ops
    fasl-table-parameters
    fef-arg-syntax
    fef-des-dt
    fef-functional
    fef-init-option
    fef-name-present
    fef-quote-status
    fef-specialness
    fefh-constants
    fefhi-fields
    fefhi-indexes
    flushed
    hardware-memory-sizes
    ;; not header-types
    header-fields
    instance-descriptor-offsets
    linear-pdl-fields
    linear-pdl-qs
    look
    m-error-substatus-fields
    m-flags-fields
    micro-stack-fields
    numeric-arg-desc-fields
    of
    page-hash-table-fields
    q-cdr-codes
    q-data-types
    q-fields
    q-header-types
    q-lisp-constants
    q-region-bits
    reg-pdl-leader-qs
    scratch-pad-parameters
    scratch-pad-pointers
    scratch-pad-temps
    scratch-pad-variables
    sg-inst-dispatches
    sg-state-fields
    sg-states
    should
    someday
    special-pdl-leader-qs
    stack-group-head-leader-qs
    sublists
    system-communication-area-qs
    system-constant-lists
    system-variable-lists
    these
    things
    unibus-channel-qs))

;; Like above but get declared special rather than system-constant
(defparameter *system-variable-lists*
  '(a-memory-location-names
    m-memory-location-names
    io-stream-names
    lisp-variables
    misc-q-variables))

(defparameter *io-stream-names*
  '(standard-input
    standard-output
    error-output
    query-io
    terminal-io
    trace-output))

;; these get declared special, and get their maclisp values shipped
;; over
(defparameter  *misc-q-variables*
  '(before-cold-initialization-list
    cold-initialization-list
    for-cadr
    once-only-initialization-list
    prin1
    system-constant-lists
    system-initialization-list
    system-variable-lists
    warm-initialization-list))

;; These get declared special, but don't get sent over. They get
;; initialized some other way, e.g. from a load-time-setq in some
;; compile list, or from special code in cold, or by lisp-reinitialize
;; when the machine is first started.
(defparameter *lisp-variables*
  '(%initially-disable-trapping
    *nopoint
    *rset
    *
    +
    -
    base
    defun
    evalhook
    fasload
    ibase
    lisp-crash-list
    local-declarations
    package
    prinlength
    prinlevel
    readtable
    rubout-handler
    scheduler-stack-group
    stream-input-operations
    stream-output-operations
    user-id))

;; These get declared system-constant (which is similar to special)
;; and get their maclisp values shipped over.
(defparameter *q-lisp-constants*
  '(%fef-header-length
    %lp-call-block-length
    %lp-initial-local-block-offset
    a-memory-counter-block-names
    a-memory-virtual-address
    area-list
    array-bits-per-element
    array-elements-per-q
    io-space-virtual-address
    lambda-list-keywords
    length-of-atom-head
    page-size
    q-data-types
    size-of-area-arrays
    size-of-ob-tbl
    size-of-page-table
    unibus-virtual-address))

(defparameter *hardware-memory-sizes*
  '(size-of-hardware-a-memory
    size-of-hardware-control-memory
    size-of-hardware-dispatch-memory
    size-of-hardware-level-1-map
    size-of-hardware-level-2-map
    size-of-hardware-m-memory
    size-of-hardware-micro-stack
    size-of-hardware-pdl-buffer
    size-of-hardware-unibus-map))

(defparameter *lambda-list-keywords*
  '(&aux
    &dt-atom
    &dt-dontcare
    &dt-fixnum
    &dt-frame
    &dt-list
    &dt-number
    &dt-symbol
    &eval
    &function-cell
    &functional
    &local
    &optional
    &quote
    &quote-dontcare
    &rest
    &special))

;; Don't put function around the symbols in here -- that means if you
;; redefine the function the microcode does not get the new
;; definition, which is not what you normally want. Saying function
;; makes it a couple microseconds faster to call it. Not all of these
;; data are actually used; check the microcode if you want to know.
(defparameter *support-vector-contents*
  '((quote print)
    (quote fexpr)
    (quote expr)
    (quote apply-lambda)
    (quote equal)
    (quote package)
    (quote expt-hard)))

;; Contents of constants page
(defparameter *constants-page* '(nil t 0 1 2))

(defparameter *scratch-pad-variables*
  '(scratch-pad-parameter-offset
   scratch-pad-parameters
   scratch-pad-pointers
   scratch-pad-temp-offset
   scratch-pad-temps))

(defparameter *scratch-pad-pointers*
  '(current-stack-group
    error-handler-stack-group
    initial-stack-group
    initial-top-level-function
    last-array-element-accessed))

(defparameter *scratch-pad-parameter-offset* 20)

(cond ((> (length *scratch-pad-pointers*)
	  *scratch-pad-parameter-offset*)
	(barf 'barf 'scrach-pad-parameter-offset 'barf)))


(defparameter *scratch-pad-parameters*
  '(active-micro-code-entries
    bind-cons-area last-array-accessed-type
    cdr-atom-mode car-atom-mode
    error-trap-in-progress default-cons-area
    invoke-mode invisible-mode
    last-array-accessed-index))

(defparameter *scratch-pad-temp-offset* 20)

(cond ((> (length *scratch-pad-parameters*)
	  *scratch-pad-temp-offset*)
       (barf 'barf 'scratch-pad-temp-offset 'barf)))

(defparameter *scratch-pad-temps*
    '(last-instruction temp-trap-code
      local-block-offset
      scratch-/#-args-loaded
      specials-in-last-block-slow-entered
      temp-pc))

(defvar ^r)
(defvar ^w)

;; no initial initializations
(defvar cold-initialization-list nil)
(defvar  before-cold-initialization-list nil)
(defvar  warm-initialization-list nil)
(defvar  once-only-initialization-list nil)
(defvar  system-initialization-list nil)


(defvar  fasl-table)
(defvar fasl-group-length)
(defvar fasl-group-flag)
(defvar fasl-return-flag)

(defparameter *fasl-group-field-values*
  '(%fasl-group-check 100000
    %fasl-group-flag 40000
    %fasl-group-length 37700
    fasl-group-length-shift -6
    %fasl-group-type 77
    %%fasl-group-check 2001
    %%fasl-group-flag 1701
    %%fasl-group-length 0610
    %%fasl-group-type 0006))


(defparameter *fasl-ops*
  '(fasl-op-apply
    fasl-op-array
    fasl-op-array-push
    fasl-op-const-page
    fasl-op-end-of-file
    fasl-op-end-of-whack
    fasl-op-err
    fasl-op-eval
    fasl-op-eval1
    fasl-op-fetch-function-cell
    fasl-op-fetch-property-cell
    fasl-op-fetch-symbol-value
    fasl-op-file-property-list
    fasl-op-fixed
    fasl-op-float
    fasl-op-frame
    fasl-op-funcell
    fasl-op-function-end
    fasl-op-function-header
    fasl-op-index
    fasl-op-initialize-array
    fasl-op-initialize-numeric-array
    fasl-op-list
    fasl-op-make-micro-code-entry
    fasl-op-micro-code-symbol
    fasl-op-micro-to-micro-link
    fasl-op-misc-entry
    fasl-op-move
    fasl-op-noop
    fasl-op-package-symbol
    fasl-op-quote-pointer
    fasl-op-rel-file
    fasl-op-remote-variable
    fasl-op-s-v-cell
    fasl-op-save-entry-point
    fasl-op-set-parameter
    fasl-op-soak
    fasl-op-storein-array-leader
    fasl-op-storein-function-cell
    fasl-op-storein-property-cell
    fasl-op-storein-symbol-value
    fasl-op-string
    fasl-op-symbol
    fasl-op-temp-list
    fasl-op-unused
    fasl-op-unused1
    fasl-op-unused2
    fasl-op-unused3
    fasl-op-unused4
    fasl-op-unused5
    fasl-op-unused6
    fasl-op-unused7))


(defparameter *fasl-table-parameters*
  '(fasl-array-area
    fasl-evaled-value
    fasl-frame-area
    fasl-list-area
    fasl-micro-code-exit-area
    fasl-nil
    fasl-obarray-pointer
    fasl-symbol-head-area
    fasl-symbol-string-area
    fasl-tem1
    fasl-tem2
    fasl-tem3
    fasl-temp-list-area
    fasl-unused
    fasl-unused2
    fasl-unused3
    fasl-unused4
    fasl-unused5))
