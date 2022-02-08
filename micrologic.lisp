(in-package cl-user)

(ql-require "fset")

(defpackage micrologic
  (:use cl))

(in-package micrologic)

(defstruct lvar
  id)

(defun lvar (id)
  (make-lvar :id id))

(defmethod fset:compare ((a lvar) (b lvar))
  (fset:compare-slots a b 'id))

(defun make-substitution-map ()
  (fset:empty-map))

;; TODO: check for cycles
(defun add-substitution (smap lvar value)
  (when smap
    (fset:with smap lvar value)))

(defgeneric walk (u smap))

(defmethod walk ((lvar lvar) smap)
  (multiple-value-bind (val foundp) (fset:lookup smap lvar)
    (if foundp
        (walk val smap)
        lvar)))

(defmethod walk (u smap)
  u)

(defun unify (u v smap)
  (let ((u (walk u smap))
        (v (walk v smap)))
    (cond ((fset:equal? u v) smap)
          ((lvar-p u) (add-substitution smap u v))
          ((lvar-p v) (add-substitution smap v u))
          (t (unify-terms u v smap)))))

(defgeneric unify-terms (u v smap))

(defmethod unify-terms (u v smap)
  nil)

;;; Lazy Streams

(defstruct lazy-stream)

(defstruct (empty-stream (:include lazy-stream)))

(defstruct (mature-stream (:include lazy-stream))
  head
  next)

(defstruct (immature-stream (:include lazy-stream))
  thunk)

(defgeneric merge-streams (a b))

(defgeneric mapcat-stream (stream function))

(defgeneric realize-stream-head (stream))

(defmethod merge-streams ((a empty-stream) (b lazy-stream))
  b)

(defmethod mapcat-stream ((s empty-stream) (f function))
  s)

(defparameter +empty-stream+
  (make-empty-stream))

(defmethod realize-stream-head ((s empty-stream))
  s)

(defmethod merge-streams ((a mature-stream) (b lazy-stream))
  (make-mature-stream :head (mature-stream-head a)
                      :next (merge-streams (mature-stream-next a)
                                           b)))

(defmethod mapcat-stream ((s mature-stream) (f function))
  (merge-streams (funcall f (mature-stream-head s))
                 (mapcat-stream (mature-stream-next s) f)))

(defmethod realize-stream-head ((s mature-stream))
  s)

(defun make-stream (s)
  (make-mature-stream :head s
                      :next +empty-stream+))

(defmethod merge-streams ((a immature-stream) (b lazy-stream))
  (make-immature-stream
   :thunk (lambda ()
            (merge-streams b
                           (funcall (immature-stream-thunk a))))))

(defmethod mapcat-stream ((s immature-stream) (f function))
  (make-immature-stream
   :thunk (lambda ()
            (mapcat-stream (funcall (immature-stream-thunk s))
                           f))))

(defmethod realize-stream-head ((s immature-stream))
  (realize-stream-head (funcall (immature-stream-thunk s))))
