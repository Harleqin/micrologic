(in-package cl-user)

(defpackage micrologic
  (:use cl))

(in-package micrologic)

(defstruct lvar
  id)

(defun lvar (id)
  (make-lvar :id id))

(defun make-substitution-map ()
  (make-array 0))

(defun smap (&rest id-val-pairs)
  (let* ((max-id (loop :for (id val) :on id-val-pairs :by #'cddr
                       :maximize id))
         (smap (make-array (1+ max-id))))
    (loop :for (id val) :on id-val-pairs :by #'cddr
          :do (setf (aref smap id) val))
    smap))

(defun add-substitution (smap lvar value)
  (let ((new-smap (make-array (max (lvar-id lvar) (length smap)))))
    (loop :for i :from 0 :below (length smap)
          :do (setf (aref new-smap i) (aref smap i)))
    (setf (aref new-smap (lvar-id lvar)) value)
    new-smap))

(defgeneric walk (u smap))

(defmethod walk ((lvar lvar) smap)
  (let ((id (lvar-id lvar)))
    (if (array-in-bounds-p smap id)
        (walk (aref smap id) smap)
        lvar)))

(defmethod walk ((u t) smap)
  u)

(defmethod walk ((u null) smap)
  nil)
