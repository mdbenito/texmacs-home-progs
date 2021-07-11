(use-modules (math math-kbd))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Caligraphic letters
;; Type the same uppercase letter thrice to make it calligraphic.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-one-kbd-map family numkeys key)
  (with s (symbol->string key)
    (list (string-concatenate (list-intersperse (make-list numkeys s) " "))
          (string-concatenate (list "<" family "-" s ">")))))

(define keys '(A B C D E F G H I J K L M N O P Q R S T U V W X Y Z))

(define (build-kbd-maps)
  `(kbd-map
    (:mode in-math?)
    ; rev 11681 changed the default behaviour for two caps typed in math:
    ; "A A" is <bbb-A> only in-math-non-hybrid? see math-kbd.scm
    ; So we just enforce it here again.
    ,@(map (cut build-one-kbd-map "bbb" 2 <>) keys)
    ,@(map (cut build-one-kbd-map "cal" 3 <>) keys)
    ,@(map (cut build-one-kbd-map "frak" 4 <>) keys)))

(eval (build-kbd-maps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
 (:mode in-math?)
 ("$" (make 'text))
 ("return" (if (or (inside? 'hybrid) (inside? 'label) 
                   (inside? 'phantom) (in-prog?))
               (kbd-return)
               (go-right)))
  ("/ tab tab" (insert "<backslash>"))
  ("0 tab" (insert "<emptyset>"))
  ("- | tab tab" (insert "<perp>"))
  ("| _ tab" (insert "<lfloor>"))
  ("_ | tab" (insert "<rfloor>"))
  ("g r s" (make 'grs)) ; requires extra-math.ts
  ("g r a" (make 'gra)) ; requires extra-math.ts
  ("i d x tab" (insert '(big-around "<int>" (concat "<mathd>" "x")))))

(kbd-map
 ("D e f tab" (make 'definition))
 ("L e m tab" (make 'lemma))
 ("P r o tab" (make 'proposition))
 ("T h e tab" (make 'theorem))
 ("C o r tab" (make 'corollary))
 ("P r f tab" (make 'proof))
 ("P a r tab" (make 'paragraph))
 ("A l g tab" (make 'algorithm))
 ("E x tab" (make 'exercise))
 ("P b tab" (make 'problem))
 ("a ) tab" (make 'enumerate-alpha) (make 'item))
 ("1 ) tab" (make 'enumerate) (make 'item))
 ("* ) tab" (make 'itemize-dot) (make 'item))
 ("* ) tab tab" (make-switch-list 'unroll 'itemize))
 ("M-A-i" (make-session "scheme" "default"))
 ("M-A-p" (make-session "python" "default"))
 ("M-A-t" (toggle-visible-header))
 ("M-A-right" (cursor-history-forward))
 ("M-A-left" (cursor-history-backward))
 ("C-left" (traverse-left))
 ("C-right" (traverse-right))
 ("C-up" (traverse-up))
 ("C-down" (traverse-down))
 ("C-home" (traverse-first))
 ("C-end" (traverse-last))
 ("C-A-left" (structured-exit-left))
 ("C-A-right" (structured-exit-right))
 ("S-M-up" (buffer-set-part-mode :preamble))
 ("M-S-down" (buffer-set-part-mode :all)))

(kbd-map
  (:mode in-session?)
  ("M-return" (session-split)))

(kbd-map
  (:mode in-text?)
  (:require (in? "log" (get-style-list)))
  ("C-f e" (make 'folded-entry)))

(kbd-map
  (:profile gnome)
  ("A-tab" (variant-circulate (focus-tree) #t)))

(tm-define (kbd-enter t shift?)
  (:require (and (in-text?) (tree-is? t 'document)))
  (if shift? (insert '(next-line)) (insert-return)))

(define (current-buffer-url-to-pdf)
  (with u (current-buffer-url)
    (url-glue (url-append (url-head u) (url-basename u)) ".pdf")))

(kbd-map
  ("macos e" (print-to-file (current-buffer-url-to-pdf))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Beamer mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define waiting-for-idle? #f)

(tm-define (keyboard-press key time)
  (:require (full-screen?))
  (when (not (in? key '("F9" "F10" "F11" "F12"))) 
    (set-boolean-preference "draw cursor" #t)
    (when (not waiting-for-idle?) ; avoid queueing a new command per keystroke
      (set! waiting-for-idle? #t)
      (delayed
        (:idle 3000)
        (set! waiting-for-idle? #f)
        (if (full-screen?)
            (set-boolean-preference "draw cursor" #f)))))
  (key-press key))

; Don't jump around the slides while in presentation mode:
; (won't work: requires loading of beamer stuff first!!)
(import-from (dynamic fold-kbd))
(kbd-map
  (:require (and (full-screen?) (in-screens?)))
  ("F9" (noop))
  ("F12" (noop)))