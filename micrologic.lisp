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

(defun add-substitution (smap lvar value)
  (when smap
    (fset:with smap lvar value)))

(defgeneric walk (u smap))

(defmethod walk ((lvar lvar) smap)
  (multiple-value-bind (val foundp) (fset:lookup smap lvar)
    (if foundp
        (walk val smap)
        lvar)))

(defmethod walk ((u t) smap)
  u)

(defmethod walk ((u null) smap)
  nil)
