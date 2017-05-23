(define (rename-label old-labels new-labels)
  ;Rename labels, all its references and eqrefs.
  ;@old-labels, @new-labels are lists of strings.
  ;The n-th position in @old-labels is substituted
  ;by the n-th position in @new-labels.
  (define (replace-link link-list old-link new-link)
    (when (> (length link-list) 0)
      (with t (car link-list)
        (if (== (tree-ref t 0) (string->tree old-link))
            (begin 
              (tree-set! t 0 new-link)
              (display* "replacing " old-link "\n"))))
      (replace-link (cdr link-list) old-link new-link)))
  (with tree-labels (select (buffer-tree)
                            '(:* (:or label reference eqref)))
    (map (cut replace-link tree-labels <> <>)
       old-labels new-labels)))

(define (blank-to-dash . old-labels)
  (define (dash-labels blank-labels)
    (map (lambda (s) (string-replace s " " "-"))
               blank-labels))
  (if (null? old-labels)
      (let* ((l (list-remove-duplicates
               (map (lambda (t) (tree-ref t 0))
                    (select (buffer-tree) 
                            '(:* (:or label reference eqref))))))
             (_old-labels (map tree->stree l))
             (new-labels (dash-labels _old-labels)))
        (rename-label _old-labels new-labels))
      (with new-labels (dash-labels old-labels)
        (rename-label old-labels new-labels))))
