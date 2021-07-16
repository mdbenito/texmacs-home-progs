;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Some editing facilities for code (scheme and a few others)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (ice-9 regex))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggling of comments for lines and selections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (in-prefix-comment-prog?)
  ; FIXME: probably more
  (or (in-scheme?) (in-python?) (in-cpp?)))

(tm-define (prog-to-prefix) "??")
(tm-define (prog-to-prefix) (:mode in-cpp?) "//")
(tm-define (prog-to-prefix) (:mode in-scilab?) "//")
(tm-define (prog-to-prefix) (:mode in-mathemagix?) "//")
(tm-define (prog-to-prefix) (:mode in-fortran?) "!")
(tm-define (prog-to-prefix) (:mode in-python?) "#")
(tm-define (prog-to-prefix) (:mode in-scheme?) ";")
(tm-define (prog-to-prefix) (:mode in-shell?) "#")

(define (toggle-line-comment* line comment-prefix)
  "Assumes @commment-prefix to be a single char"
  (let* ((pattern (string-append "^( *)" "(" comment-prefix "+ )" "(.*)$"))
         (matches (string-match pattern line)))
    (if matches
        (string-append (match:substring matches 1)
                       (match:substring matches 3))
        (with matches (string-match "^( *)(.*)$" line)
          (string-append comment-prefix " "
                         (match:substring matches 1)
                         (match:substring matches 2))))))

(tm-define (toggle-tree-comment t)
  (display "Not implemented for this language"))

(tm-define (toggle-tree-comment t)
  (:mode in-prefix-comment-prog?)
  (cond ((string? t) (toggle-line-comment* t (prog-to-prefix)))
        ((tree? t) 
         (map (compose (cut toggle-line-comment* <> (prog-to-prefix))
                       tree->string)
              (tree-children t)))
        (else (display "WTF?") t)))

(define (selection-going-up?)
  (path-less? (cursor-path) (selection-get-end)))

(define (ensure-proper-selection)
  "Ensures that the selection is right to comment whole lines"
  (if (selection-going-up?)
      (begin
        (go-to-column 0)
        (selection-set-start))
      (begin
        (go-end-line)
        (selection-set-end))))

(tm-define (toggle-selection-comment)
  (display "Not implemented for this language"))

; TODO: try to keep as much of the selection as possible after insert
; selection-get-start and -end are invalid after the insertion and need
; some finagling
(tm-define (toggle-selection-comment)
  (:mode in-prefix-comment-prog?)
  (when (selection-active-any?)
    (ensure-proper-selection)
    (let* ((selection (selection-tree))
           (commented (toggle-tree-comment selection)))
      (clipboard-cut "nowhere")
      (insert `(document ,@commented)))))

(tm-define (toggle-line-comment)
  (display "Not implemented for this language"))

(tm-define (toggle-line-comment)
  (:mode in-prefix-comment-prog?)
  (let* ((t (cursor-tree))
         (s (toggle-line-comment* (tm->string t) (prog-to-prefix))))
    (tree-assign t (tree s))))

(tm-define (toggle-comment)
  (display "Not implemented for this language"))

(tm-define (toggle-comment)
  (:mode in-prog?)
  (if (selection-active?)
      (toggle-selection-comment)
      (toggle-line-comment)))

;(define (test x y) (== (toggle-line-comment (toggle-line-comment x)) y))
;(map test '("" ";" " ;a" ";;;a") '("" ";" " ;a" ";a"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy, cut and duplicate whole lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (kbd-copy)
  (:mode in-prog?)
  (:require (not (selection-active-any?)))
  (select-line)
  (clipboard-copy-export "scheme" "primary"))

(tm-define (kbd-cut)
  (:mode in-prog?)
  (:require (not (selection-active-any?)))
  (select-line)
  (clipboard-cut-export "scheme" "primary")
  (key-press "delete"))


(tm-define (duplicate-code)
  (display "Not implemented for this language"))

(tm-define (duplicate-code)
  (:mode in-prog?)
  (let* ((t (cursor-tree))
         (p (tree-up t))
         (row (tree-index t)))
    (if (selection-active?)
        (tree-insert p row (list (tree-copy (selection-tree))))
        (tree-insert p row (list (tree-copy t))))))

(define (program-move-line t offset)
  (let* ((col (program-column-number))
         (p (tree-up t))
         (row (tree-index t))
         (above (tree-ref p (+ row offset)))
         (tmp (tree-copy above)))
    (tree-set above (tree-copy t))
    (tree-set t tmp)
    (program-go-to (+ row offset) col)))

(define (program-move-rows-up from to)
  (let* ((p (tree-up (cursor-tree)))
         (offset -1)
         (dest (+ from offset))
         (maxrows (tree-arity p))
         (save (tree-copy (tree-ref p dest))))
    (when (and (> dest 0) (< dest maxrows))
      (tree-remove p (+ from offset) 1)
      (tree-insert p (min (max 0 to) maxrows) (list save)))))

(define (program-move-rows-down from to)
  (let* ((p (tree-up (cursor-tree)))
         (offset 1)
         (dest (+ to offset))
         (maxrows (tree-arity p)))
    (when (and (> dest 0) (< dest maxrows))
      (with save (tree-copy (tree-ref p dest))
        (tree-remove p dest 1)
        (tree-insert p from (list save))))))

(define (program-path-row-update p offset)
  "Updates path @p to be @offset rows up or down"
  (let* ((rev (reverse p))
         (col (car rev))
         (row (cadr rev))
         (max-rows (tree-arity (tree-up (cursor-tree)))))
  (reverse (cons col (cons (min (max 0 (+ row offset)) max-rows) (cddr rev))))))

(tm-define (program-move dir)
  (with action (if (== dir :up) program-move-rows-up program-move-rows-down)
    (if (selection-active?)
        (let ((from (selection-get-start))
              (to (selection-get-end))
              (offset (if (== dir :up) -1 1)))
          (action (cADr from) (cADr to))
          ; keep the selection:
          (selection-set (program-path-row-update from offset)
                         (program-path-row-update to offset)))
        (action (program-row-number) (program-row-number)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (prog-go-to-line n)
  (:interactive #t)
  (:argument n "line number")
  (go-to-line (string->number n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(lazy-keyboard-force #t)

(kbd-map
  (:mode in-prog?)
  ("C-;" (toggle-comment))
  ("C-d" (duplicate-code))
  ("C-left" (traverse-left))
  ("C-right" (traverse-right))
  ("C-up" (program-move :up))
  ("C-down" (program-move :down))
  ("A-0" (prog-go-to-line 1))
  ("A-g" (interactive prog-go-to-line "Go to line")))

; FIXME: this doesn't always work as expected
(tm-define (kbd-variant t forwards?)
  (:mode in-prog-scheme?)
  (:require (string-null? (cursor-word)))
  (program-indent (not forwards?)))