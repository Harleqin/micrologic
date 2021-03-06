(in-package cl-user)

(defpackage micrologic
  (:use arrows cl))

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

;; A lazy stream is
;; - nil,
;; - a cons whose cdr is a lazy stream, or
;; - a thunk that returns a lazy stream.

;; This is not memoizing, so if you read such a stream again, its thunks will
;; all be re-evaluated.  In the use case here, streams seem mostly single-use,
;; so it should be OK…

(defgeneric merge-streams (a b))

(defgeneric mapcat-stream (stream function))

(defgeneric realize-stream-head (stream))

(defmethod merge-streams ((a null) b)
  b)

(defmethod mapcat-stream ((s null) (f function))
  s)

(defmethod realize-stream-head ((s null))
  s)

(defmethod merge-streams ((a cons) b)
  (cons (car a)
        (merge-streams (cdr a) b)))

(defmethod mapcat-stream ((s cons) (f function))
  (merge-streams (funcall f (car s))
                 (mapcat-stream (cdr s) f)))

(defmethod realize-stream-head ((s cons))
  s)

(defmethod merge-streams ((a function) b)
  (lambda ()
    (merge-streams b
                   (funcall a))))

(defmethod mapcat-stream ((s function) (f function))
  (lambda ()
    (mapcat-stream (funcall s)
                   f)))

(defmethod realize-stream-head ((s function))
  (realize-stream-head (funcall s)))

;;; State and Goals

(defstruct state
  (s-map (make-substitution-map))
  (next-id 0))

(defun with-s-map (state s-map)
  (make-state :s-map s-map
              :next-id (state-next-id state)))

(defun with-next-id (state next-id)
  (make-state :s-map (state-s-map state)
              :next-id next-id))

(defparameter +empty-state+
  (make-state))

;; A goal is a function from a state to a stream of solutions.  A goal
;; constructor is a function that returns a goal.

(defun === (u v)
  (lambda (state)
    (let ((new-s-map (unify u v (state-s-map state))))
      (if new-s-map
          (list (with-s-map state new-s-map))
          nil))))

(defun call-fresh (goal-constructor)
  (lambda (state)
    (let ((goal (funcall goal-constructor
                         (lvar (state-next-id state)))))
      (funcall goal
               (with-next-id state
                 (1+ (state-next-id state)))))))

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
       (lambda ()
         (funcall ,goal ,state)))))

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

(defun call-empty-state (goal)
  (funcall goal +empty-state+))

(defmacro run* (vars &body goals)
  `(->> (fresh ,vars ,@goals)
        call-empty-state
        (map-stream #'reify-state-first-var)))

(defgeneric map-stream (f stream))

(defmethod map-stream (f (stream null))
  nil)

(defmethod map-stream (f (stream cons))
  (lambda ()
    (cons (funcall f (car stream))
          (map-stream f (cdr stream)))))

(defmethod map-stream (f (stream function))
  (lambda ()
    (map-stream f (realize-stream-head stream))))

(defgeneric stream-take (n stream))

(defmethod stream-take ((n (eql 0)) stream)
  nil)

(defmethod stream-take (n (stream null))
  nil)

(defmethod stream-take (n (stream cons))
  (when (plusp n)
    (cons (car stream)
          (stream-take (1- n) (cdr stream)))))

(defmethod stream-take (n (stream function))
  (when (plusp n)
    (stream-take n (realize-stream-head stream))))

(defmacro run (n vars &body clauses)
  `(stream-take ,n (run* ,vars ,@clauses)))

;;; Extending to collections

;; Conses

(defmethod unify-terms ((u cons) (v cons) smap)
  (->> smap
       (unify (car u) (car v))
       (unify (cdr u) (cdr v))))

(defmethod reify-s* ((v cons) smap)
  (->> smap
       (reify-s (car v))
       (reify-s (cdr v))))

(defmethod deep-walk* ((v cons) smap)
  (cons (deep-walk (car v) smap)
        (deep-walk (cdr v) smap)))

(defun conso (car cdr out)
  (=== (cons car cdr) out))

(defun caro (car out)
  (fresh (cdr)
    (conso car cdr out)))

(defun cdro (cdr out)
  (fresh (car)
    (conso car cdr out)))

(defun emptyo (out)
  (=== '() out))

(defun appendo (l0 l1 out)
  (conde
    ((emptyo l0) (=== l1 out))
    ((fresh (car cdr rec)
       (conso car cdr l0)
       (conso car rec out)
       (appendo cdr l1 rec)))))
