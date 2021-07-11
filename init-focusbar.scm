;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some tools for the focus bar (should commit to SVN)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (generic generic-menu))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auxiliary stuff

(define (replace-and-activate pos s)
  "Replace subtree @pos of the focus-tree with string @s and activate the tag"
  (let* ((t (focus-tree))
         (inactive? (tree-is? t :up 'inactive)))
  (with-focus-after t
    (tree-remove! t pos 1)
    (tree-insert! t pos (list s))
    (cond (inactive? (tree-go-to t pos) (activate))
           ((not (tree-accessible-child? t pos))
            (with ac (tree-accessible-children t)
              (when (nnull? ac)
                (tree-go-to (car ac) :start))))))))

(tm-define (focus-tag-string t)
  (or (and-with r (tm-ref t 0) (texmacs->verbatim r))
      ""))

(tm-define (focus-tag-string t)
  (:require (tree-in? t (group-resolve 'figure-tag)))
  (or (and-with r (tm-ref t 1) (texmacs->verbatim r))
      ""))

(tm-menu (focus-taglist-menu tag)
  (:synopsis "A list of menu entries to jump to occurrences of @tag")
  (for (x (select (buffer-tree) `(:* ,tag)))
    ((eval (focus-tag-string x))
     (when (tree->path x) (tree-go-to x 0 :end)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; "To-Do" tags.

(define-group todo-tag todo fixme askme)
(define-group variant-tag (todo-tag))
(define-group can-jump-tags (todo-tag)) ;(figure-tag)

(tm-define (focus-tag-name l) (:require (in? l '(todo))) "To do")
(tm-define (focus-tag-name l) (:require (in? l '(fixme))) "Fix me")
(tm-define (focus-tag-name l) (:require (in? l '(askme))) "Ask me")

(tm-menu (focus-tag-extra-icons t)
  (:require (tree-in? t (can-jump-tags-list)))
  (mini #t
    (group "Jump to:")
    (=> (balloon (eval (focus-tag-string t)) "Click to jump")
        (dynamic (focus-taglist-menu (tree-label t))))))

(tm-menu (focus-tag-menu t)
  (:require (tree-in? t (can-jump-tags-list)))
  (=> "Jump to…"
      (dynamic (focus-taglist-menu (tree-label t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A list of all labels in the document for the focus bar

(tm-define (get-filtered-labels)
  (list-filter
   (list-sort (map (lambda (x) (focus-tag-string x))
                   (select (buffer-tree) `(:* label)))
              string<?)
   (lambda (s) (or (< (string-length s) 4)
                   (string<> (string-take s 4) "bib-")))))

(tm-define (replace-other-label s)
  (:argument s "Label")
  (:proposals s (get-filtered-labels))
  (replace-and-activate 0 s))

(tm-menu (focus-label-menu)
  (:synopsis "A list of menu entries which insert labels")
  (with cur (tree->string (tm-ref (focus-tree) 0))
    (for (s (get-filtered-labels))
      ((check (eval s) "*" (== s cur)) (replace-and-activate 0 s)))
    ("Other" (interactive replace-other-label))))

(tm-menu (focus-hidden-icons t)
  (:require (tree-in? t '(reference-tag-list)))
  (with cur (focus-tag-string t)
    (mini #t
      (group "Id:")
      (=> (balloon (eval 
                    (if (string-null? cur) "Available labels..." cur))
                   "Pick one to insert")
          (link focus-label-menu)))))

(tm-menu (focus-tag-menu t)
  (:require (tree-in? t '(reference-tag-list)))
  (=> "Insert label..."
      (link focus-label-menu)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Focus bar icons and menus for citations

(tm-define (focus-tag-name l)
  (:require (in? l '(cite)))
  "Citation")

(tm-define (focus-tag-name l)
  (:require (in? l '(cite-detail)))
  "Detailed citation")

(tm-define (focus-tag-name l)
  (:require (in? l '(nocite)))
  "Invisible citation")

(tm-menu (focus-citations-menu* pos cur)
  (:synopsis "A list of menu entries which insert citations")
  (with l (citekey-list (current-bib-file #t) "")
    (for (s l)
      ((check (eval s) "*" (== s cur)) (replace-and-activate pos s)))
    ("Other" (interactive (lambda (ss) (replace-and-activate pos ss))
               (list "Citation key:" "string" l)))))

(tm-menu (focus-citations-menu pos)
  (with cur (tree->string (tm-ref (focus-tree) pos))
    (=> (balloon (eval 
                  (if (string-null? cur) "Insert..." cur))
                 "Pick key to insert")
        (dynamic (focus-citations-menu* pos cur)))))

(tm-menu (focus-hidden-icons t)
  (:require (tree-in? t '(cite nocite)))
  (mini #t
    (for (pos (.. 0 (tree-arity t)))
      (group "Key:")
      (dynamic (focus-citations-menu pos)))))

(tm-menu (focus-hidden-icons t)
  (:require (tree-in? t '(cite-detail)))
  (mini #t
    (group "Key:")
    (dynamic (focus-citations-menu 0))
    (dynamic (string-input-icon t 1))))

(tm-menu (focus-tag-menu t)
  (:require (tree-in? t (citation-tag-list)))
  (for (pos (.. 0 (tree-arity t)))
    (dynamic (focus-citations-menu pos))))

;;;;;;;;;;;;;; TEMP for presentations (04.2014)

(tm-menu (standard-focus-menu t)
  (:require (and (full-screen?)
                 (member "beamer" 
                         (tree->stree (get-style-tree)))))
  (dynamic (focus-slides-menu t)))
