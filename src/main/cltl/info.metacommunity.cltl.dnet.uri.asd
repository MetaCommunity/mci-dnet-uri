;; info.metacommunity.cltl.dnet.uri.asd		-*-lisp-*-

(in-package #:cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpackage #:dnet-system
    (:use #:asdf #:cl)))

(in-package #:dnet-system)


(defsystem #:info.metacommunity.cltl.dnet.uri
  :description 
  "URI caching for MetaCommunity.info Data Net"
  :version "1.0"
  :homepage "https://github.com/MetaCommunity/mci-dnet-uri"
  :license "https://github.com/MetaCommunity/mci-dnet-uri/blob/master/LICENSE"
  ;; :depends-on (#:info.metacommunity.cltl.utils)
  :components 
  ((:file "dnet-uri-package")
   (:file "uri-table"
          :depends-on ("dnet-uri-package"))
   ))
