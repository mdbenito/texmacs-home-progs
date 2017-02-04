(use-modules (generic generic-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; To-Do tag:

(tm-define (get-arg-of-tag t tag)
  (with r (tm-ref t 0)
    (or
     (and (tree-is? t tag) (tree? r) (tree->string r))
     "")))

(tm-menu (focus-taglist-menu tag)
  (:synopsis "A list of menu entries to jump to occurrences of @tag")
  (for (x (select (buffer-tree) `(:* ,tag)))
    ((eval (get-arg-of-tag x tag))
     (when (tree->path x) (tree-go-to x 0 :end)))))

(tm-menu (focus-tag-extra-icons t)
  (:require (tree-is? t 'todo))
  (mini #t
    (inert ("Jump to:" (noop)))
    (=> (balloon (eval (get-arg-of-tag t 'todo)) "Click to jump")
        (dynamic (focus-taglist-menu 'todo)))))

(tm-menu (focus-extra-menu t)
  (:require (tree-is? t 'todo))
  ---
  (=> "Jump to..."
      (dynamic (focus-taglist-menu 'todo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A list of labels in the focus bar

(tm-define (get-filtered-labels)
  (list-filter
   (list-sort (map (lambda (x) (get-arg-of-tag x 'label))
                   (select (buffer-tree) `(:* label)))
              string<?)
   (lambda (s) (or (< (string-length s) 4)
                   (string<> (string-take s 4) "bib-")))))

(tm-menu (focus-label-menu t)
  (:synopsis "A list of menu entries which insert labels")
  (for (s (get-filtered-labels))
    ((eval s) (when (tree-in? t '(reference eqref))
                (tree-set! t `(,(tree-label t) ,s))))))

(tm-menu (focus-tag-icons t)
  (:require (tree-in? t '(reference eqref)))
  (with cur (get-arg-of-tag t (tree-label t))
    (mini #t
      (=> (balloon (eval 
                    (if (string-null? cur) "Available labels..." cur))
                   "Pick one to insert")
          (dynamic (focus-label-menu t))))))

(tm-menu (focus-tag-menu t)
  (:require (tree-in? t '(reference eqref)))
  (=> "Insert label..."
      (dynamic (focus-label-menu t))))
