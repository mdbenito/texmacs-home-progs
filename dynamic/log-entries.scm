(texmacs-module (dynamic log-entries)
  (:use (dynamic dynamic-drd)))

(define-group log-entry-tag
  folded-entry unfolded-entry)
  
(define-fold folded-entry unfolded-entry)

(define (log-entry-select e)
  (with (dd yy mm) e
    (with l (select (buffer-tree) 
                    `((:or ,@(group-resolve 'log-entry-tag))))
      (with sel (lambda (t) 
                  (and (>= (tree-arity t) 4)
                       (== dd (tree->string (tree-ref t 1)))
                       (== yy (tree->string (tree-ref t 2)))
                       (== mm (tree->string (tree-ref t 3)))))
      (list-filter l sel)))))
 
(tm-define (jump-to-entry t)
  (:secure #t)
  (with l (log-entry-select (string-split (tree->string t) #\.))
    (if (nnull? l) (mouse-unfold (tree-ref (car l) 0)))))

