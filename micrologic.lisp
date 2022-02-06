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
