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

(tm-define (duplicate-line)
  (:mode in-prog?)
  (:require (not (selection-active-any?)))
  (select-line)
  (clipboard-cut "nowhere")
  (insert `(document ,(tm->string (tm-ref (clipboard-get "nowhere") 1))
                     ,(tm->string (tm-ref (clipboard-get "nowhere") 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-prog?)
  ("C-;" (toggle-comment))
  ("C-d" (duplicate-line)))
