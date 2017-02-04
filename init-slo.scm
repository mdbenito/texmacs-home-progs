;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Cosas para Spektraltheorie Linearer Operatoren (14 Jun 2011)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (utils library tree))

; Decide si una referencia ha de actualizarse o no, según el número de la
; etiqueta que se le pase.
(define (inc-ctr _num _from)
  (cond
    ((and (number? _num) (number? _from) (>= _num _from))    
     (+ _num 1))
    (else _num)))


; El formato de las referencias es "schCC.NN-LL", donde:
; CC es el número de capítulo, NN el de sección, LL la letra de subsección
(define (inc-schref _str _from)
  ; quita los tres primeros caracteres, busca punto y guión
  (let* ((chunk (string-drop _str 3))
         (dot (string-index chunk #\.))
         (dash (string-index chunk #\-)))
    ; Seguro que hay una manera más corta de hacer esto, pero me da pereza pensarla
    (string-append "sch" 
     (cond
      ; Un punto (marca de capítulo) en la cadena, pero sin guión (marca de una letra):
      ((and (!= dot #f) (== dash #f))
       (string-append (string-take chunk (+ 1 dot))
                      (number->string (inc-ctr (string->number (string-drop chunk (+ 1 dot))) _from)) ))
      ; Un punto y un guión en la cadena
      ((and (!= dot #f) (!= dash #f)) 
       (string-append (string-take chunk (+ 1 dot))
                      ; Venga va, esto da asco. Mejor sería no precalcular dash 
                       (number->string (inc-ctr (string->number (string-take (string-drop chunk (+ 1 dot)) (- dash (+ 1 dot))) _from)))
                       (string-drop chunk dash) ))
      ; Sin punto, pero con guión
      ((and (== dot #f) (!= dash #f)) 
       (string-append (number->string (inc-ctr (string->number (string-take chunk dash)) _from)) 
                      (string-drop chunk dash)))
      ; Sin punto ni guión
      (else 
       (number->string (inc-ctr (string->number chunk) _from)))))))


; @_tree es un árbol correspondiente a una etiqueta </reference>
; @_num es el número a partir del cual deberán incrementarse las referencias
; Esto cambia el primer hijo del nodo que se le pasa como argumento, así
; que ha de ser una etiqueta de referencia o casca.
(define (mod-schref _tree _from)
  (let ((_str (tree->string (tree-ref _tree 0))))
    (cond 
      ((== "sch" (string-take _str 3))
       (tree-set _tree 0 (string->tree (inc-schref _str _from)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;; Permite insertar nuevos párrafos numerados y con etiqueta automática y que
;; las referencias apunten donde deben. De momento hay que llamarlo a mano,
;; pero más adelante debería ser parte de la macro \schnitt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Incrementa las referencias a etiquetas automáticas en el documento.
(tm-define (update-schrefs _from)
  (let* ((refs (select (buffer-tree) '(:* reference)))) ; refs es una lista de trees
    (for-each (lambda (item) (mod-schref item _from)) refs)))

; Incrementa las etiquetas que no son automáticas en el documento.
(tm-define (update-schlabels _from)
  (let* ((labels (select (buffer-tree) '(:* label)))) ; labels es una lista de trees
    (for-each (lambda (item) (mod-schref item _from)) labels)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keyboard mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(kbd-map
  (:mode in-math?)
  ("+ + +" (make-script #t #t) (insert "<ast>") (go-right) (key-press "enter"))
  ("i e tab" (insert '(big-around "<int>" " d E")))
  ("i e x tab" (insert '(big-around "<int>" (concat " d E" (rsub "x"))))))

(kbd-map
  ("S c h tab" (make 'schnitt)))


