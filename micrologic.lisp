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

;;; Goals

(defstruct state
  (s-map (make-substitution-map))
  (next-id 0))

(defun with-s-map (state s-map)
  (make-state :s-map s-map
              :next-id (state-next-id state)))

(defun with-next-id (state next-id)
  (make-state :s-map (state-s-map state)
              :next-id next-id))

(defun === (u v)
  (lambda (state)
    (let ((new-s-map (unify u v (state-s-map state))))
      (if new-s-map
          (make-stream (with-s-map state new-s-map))
          +empty-stream+))))

(defun call-fresh (goal-constructor)
  (lambda (state)
    (let ((goal (funcall goal-constructor (lvar (state-next-id state)))))
      (funcall goal (with-next-id state (1+ (state-next-id state)))))))

(defun ldisj (goal-1 goal-2)
  (lambda (state)
    (merge-streams (funcall goal-1 state)
                   (funcall goal-2 state))))

(defun lconj (goal-1 goal-2)
  (lambda (state)
    (mapcat-stream (funcall goal-1 state)
                   goal-2)))

;;; Sugar

(defmacro delay-goal (goal)
  (let ((state (gensym "STATE")))
    `(lambda (,state)
       (make-immature-stream :thunk (lambda ()
                                      (funcall ,goal ,state))))))

(defmacro ldisj+ (goal &rest more-goals)
  (if more-goals
      `(ldisj (delay-goal ,goal)
              (ldisj+ ,@more-goals))
      `(delay-goal ,goal)))

(defmacro lconj+ (goal &rest more-goals)
  (if more-goals
      `(lconj (delay-goal ,goal)
              (lconj+ ,@more-goals))
      `(delay-goal ,goal)))

;;; Reification

(defun reify-name (n)
  (intern (format nil "_.~a" n) '#:keyword))

(defun reify-s (v s-map)
  (reify-s* (walk v s-map) s-map))

(defgeneric reify-s* (v s-map))

(defmethod reify-s* (v s-map)
  s-map)

(defmethod reify-s* ((v lvar) s-map)
  (add-substitution s-map
                    v
                    (reify-name (fset:size s-map))))

(defun deep-walk (v s-map)
  (deep-walk* (walk v s-map) s-map))

(defgeneric deep-walk* (v s-map))

(defmethod deep-walk* (v s-map)
  v)

(defun reify-state-first-var (state)
  (let ((v (deep-walk (lvar 0) (state-s-map state))))
    (deep-walk v (reify-s v (make-substitution-map)))))

;;; Programmer interface

(defmacro conde (&body clauses)
  `(ldisj+ ,@(mapcar (lambda (clause)
                       `(lconj+ ,@clause))
                     clauses)))

(defmacro fresh (vars &body clauses)
  (if (endp vars)
      `(lconj+ ,@clauses)
      `(call-fresh (lambda (,(first vars))
                     (fresh ,(rest vars)
                            ,@clauses)))))
