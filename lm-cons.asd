

(asdf:defsystem :lm-cons
  :depends-on (alexandria
	       uiop
	       bordeaux-threads
	       cl-aa
	       cl-aa-misc
	       cl-fad
	       cl-ppcre
	       cl-store
	       cl-vectors
	       trivial-features
	       clx
	       trivial-dump-core
	       placeholder-si)
  :license ""
  :author ""
  :description ""
  :components
  ((:file "package")
   (:module lispm
	    :depends-on (:package)
	    :components
	    ((:file "cl-qcdefs")
	     (:file "cl-qdefs-vars")
	     (:file "cl-qdefs")
	     (:file "cl-fasd")))
   (:module lmcons
   	    :depends-on (:utils)
   	    :components
	    ((:file "cl-unfasl")))))
