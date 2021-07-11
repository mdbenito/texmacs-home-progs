;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Other stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 regex))

(define (toggle-line-comment* line)
  (with matches (string-match "^( *)(;+)(.*)$" line)
    (if matches
        (string-append (match:substring matches 1)
                       (match:substring matches 3))
        (with matches (string-match "^( *)(.*)$" line)
          (string-append ";"
                         (match:substring matches 1)
                         (match:substring matches 2))))))

(tm-define (toggle-line-comment)
  (:require in-scheme?)
  (let* ((t (cursor-tree))
         (s (toggle-line-comment* (tm->string t))))
    (display s)))

(tm-define (toggle-line-comment)
  (display "Not implemented for this mode"))

(tm-define (toggle-line-comment)
  (:require in-scheme?)
  (let* ((t (cursor-tree))
         (s (toggle-line-comment* (tm->string t))))
    (tree-assign t (tree s))))

(tm-define (toggle-comment)
  (display "Not implemented for this mode"))

(tm-define (toggle-comment)
  (:require in-scheme?)
  (if (selection-active?)
      (comment-selection)
      (toggle-line-comment)))

(define (toggle-tree-comment t)
  (cond ((string? t) (toggle-line-comment* t))
        ((tree? t) (map (compose toggle-line-comment* tree->string) 
                        (tree-children t)))
        (else (display "WTF?") t)))

(tm-define (comment-selection)
  (when (selection-active-any?)
    (with selection (selection-tree)
      (with commented (toggle-tree-comment selection)
        (clipboard-cut "comment")
        (insert `(document ,@commented))))))

(kbd-map
  (:require in-scheme?)
  ("C-;" (toggle-comment)))

;(define (test x y) (== (toggle-line-comment (toggle-line-comment x)) y))
;(map test '("" ";" " ;a" ";;;a") '("" ";" " ;a" ";a"))