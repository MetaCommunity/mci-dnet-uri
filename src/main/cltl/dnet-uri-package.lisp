;; dnet-uri-package.lisp

(in-package #:cl-user)

(defpackage #:info.metacommunity.cltl.dnet.uri
  (:nicknames #:dnet.uri)
  (:use #+NIL #:info.metacommunity.cltl.utils #:cl)
  (:export
   #:uri-table
   #:uri-hash-table
   #:make-uri-table
   #:uri-get
   #:uri-put
   #:uri-delete
   #:uri-equal
   ))
