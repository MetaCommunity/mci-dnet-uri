;; uri-table.lisp

(in-package #:dnet.uri)

(defgeneric uri-table-storage (table))
(defgeneric (setf uri-table-storage) (new-value table))

(defclass uri-table ()
  ;; protocol class
  ((storage
   :initarg :storage
   :accessor uri-table-storage
   )))

(defclass uri-hash-table (uri-table)
  ;; uri-table implementation nr. 1 
  ;;
  ;; Characteristics:
  ;; * 'storage' uses an equal hash-table (alternate: consider ctrie)
  ;;
  ;; * URIs will not be destructured
  ;;
  ;; * may be suitable for reference onto URIs as per XMLNS, in which
  ;;   a URI string is referenced for its exact syntax, without any 
  ;;   structural analysis of URI contents
  ;;
  ;; * may not be suitable for applications requiring information
  ;;   computed from URI structures
  ((storage
   :type hash-table
   :initform (make-hash-table :test 'equal))))

;; protocol implementation - defaulting to uri-hash-table

;; TO DO: If appending any additional URI table implementations,
;; consider implementing a model for user selection of protocol
;; implementation at compile time. This should serve to avoid
;; procedures for method dispatching, though it likewise may
;; interfere with portability and/or extensibility of the system.

(defun make-uri-table ()
  (declare (values uri-table))
  (values (make-instance 'uri-hash-table)))

(defun uri-get (uri table)
  (declare (type string uri)
           (type uri-hash-table table)
           (values t boolean))
  (gethash uri (uri-table-storage table)))

(defun uri-put (object uri table)
  ;; NOTE that this implementation does not specify any exact syntax
  ;; for OBJECT. Essentially, this implementation represents an
  ;; initial prototype. This prototype is not specialized for any
  ;; specific application.
  ;;
  ;; Neither does this implementation utilize CLOS generic
  ;; functions for its functional protocol. In its initial protoype,
  ;; this implementation provies exacty one type of URI-TABLE
  ;; implementation and therefore. Method dispatching, insofar as the 
  ;; URI-TABLE, would be unnecessary in this prototype.
  (declare (type string uri)
           (type uri-hash-table table)
           (values t))
  (setf (gethash uri (uri-table-storage table))
        object))

(defun (setf uri-get) (new-value uri table)
  (declare (type string uri)
           (type uri-hash-table table)
           (values t))
  (uri-put new-value uri table))


(defun uri-delete (uri table)
  (declare (type string uri)
           (type uri-hash-table table)
           (values boolean))
  (remhash uri (uri-table-storage table)))

#| Trivial instance tests


(defvar *t* (make-uri-table))

(setf (uri-get "http://www.example.com/" *t*) '#:example)

(uri-get  "http://www.example.com/" *t*)
;; => #:EXAMPLE, T

(uri-get  "http://two.example.com/" *t*)
;; => NIL, NIL

(uri-get  "http://WWW.example.com/" *t*)
;; => NIL, NIL
;; ^ NOTE that this implementation does not canonicalize the URI
;;   for any single URI scheme

(uri-delete  "http://www.example.com/" *t*)
;; => T

(uri-get  "http://www.example.com/" *t*)
;; => NIL, NIL

|#
