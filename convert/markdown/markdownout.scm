;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Markdown-stree to markdown-document or markdown-snippet converter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert markdown markdownout)
  (:use (convert tools output)))

(define (hugo-extensions?)
  (== (get-preference "texmacs->markdown:hugo-extensions") "#t"))

(define (keep x)
  (cons (car x) (map serialize-markdown (cdr x))))

(define (skip x)
  (string-concatenate (map serialize-markdown (cdr x))))

(define (md-document x)
  (apply string-append
         (map line-break-after
              (map line-break-after
                   (map serialize-markdown (cdr x))))))

(define (md-concat x)
  (apply string-append 
         (map serialize-markdown (cdr x))))

(define (prefix-header n)
  (if (== 1 n)
      "#"
      (string-append "#" (prefix-header (- n 1)))))

(define (line-break-after s)
  (string-append s "\n"))

(define (md-header n)
  (lambda (x)
    (with res (apply string-append 
                     `(,(prefix-header n)
                         " "
                         ,@(map serialize-markdown (cdr x))))
      (if (<= n 4)
          (line-break-after res)
          (string-append res " ")))))

(define (math->latex t)
 "Converts the TeXmacs tree @t into internal LaTeX representation"
 (with options '(("texmacs->latex:replace-style" . "on")
                 ("texmacs->latex:expand-macros" . "on")
                 ("texmacs->latex:expand-user-macros" . "off")
                 ("texmacs->latex:indirect-bib" . "off")
                 ("texmacs->latex:encoding" . "utf8")
                 ("texmacs->latex:use-macros" . "off"))
 (texmacs->latex t options)))

(define (md-environment x)
  (string-append 
   (md-style 
    `(strong 
      ,(translate 
        (string-capitalize (symbol->string (car x))))))
   ": "
   (string-concatenate (map serialize-markdown (cdr x)))))

(define (md-math* t)
  ; TODO
  t)

(define (md-math t)
 "Takes a tree @t, and returns a valid MathJax-compatible LaTeX string"
 (with ltx (math->latex t)
   (serialize-latex (md-math* ltx))))

(define (md-list x)
  (let* ((c (cond ((== (car x) 'itemize) "* ")
                ((== (car x) 'enumerate) "1. ")
                ((== (car x) 'enumerate-alpha) "a. ")
                (else "* ")))
         (transform
          (lambda (a)
            (if (and (list>0? a)
                     (== (car a) 'concat)
                     (== (cadr a) '(item)))
                `(concat ,c ,@(cddr a))
                `(concat "  " ,a)))))
    (with doc (cAr x)
      (serialize-markdown 
       `(document ,@(map transform (cdr doc)))))))

(define (md-quotation x)
  (let ((add-prefix (lambda (a) `(concat "> " ,a)))
        (doc (cAr x)))
    (with prefixed-children (map add-prefix (cdr doc))
      (serialize-markdown
       `(document ,@prefixed-children)))))
        
(define (style-text style)
 (cond ((== style 'strong) "**")
       ((== style 'em) "*")
       ((== style 'tt) "`")
       (else "")))

(define (md-style x)
  (with st (style-text (car x))
    (string-concatenate 
     `(,st ,@(map serialize-markdown (cdr x)) ,st))))

(define (md-cite x)
  "Convert to hugo-cites.
   Multiple cites are not supported"
  (with cite-key (cadr x)
    (string-append "{{< cite " cite-key " >}}")))

(define (md-cite-detail x)
  (with detail (cAr x)
      (string-append (md-cite (cDr x)) " (" detail ")")))

(define (md-hlink x)
  (with payload (cdr x)
    (string-append "[" (car payload) "]" "(" (cadr payload) ")")))    

(define (md-figure x)
  "Hugo {{< figure >}} shortcode"
  (if (hugo-extensions?)
      (with payload (cdr x)
        (string-concatenate 
         `("{{< figure src=\"" ,(car payload) 
           "\" title=\"" ,@(map serialize-markdown (cdr payload)) "\" >}}")))
      ""))

; TODO: option for exporting or not cites
(define serialize-hash (make-ahash-table))
(map (lambda (l) (apply (cut ahash-set! serialize-hash <> <>) l)) 
     (list (list 'strong md-style)
           (list 'em md-style)
           (list 'tt md-style)
           (list 'document md-document)
           (list 'quotation md-quotation)
           (list 'theorem md-environment)
           (list 'proposition md-environment)
           (list 'corollary md-environment)
           (list 'lemma md-environment)
           (list 'proof md-environment)
           (list 'math md-math)
           (list 'equation md-math)
           (list 'equation* md-math)
           (list 'concat md-concat)
           (list 'itemize md-list)
           (list 'enumerate md-list)
           (list 'enumerate-alpha md-list)
           (list 'h1 (md-header 1))
           (list 'h2 (md-header 2))
           (list 'h3 (md-header 3))
           (list 'h4 (md-header 4))
           (list 'cite md-cite)
           (list 'cite-detail md-cite-detail)
           (list 'figure md-figure)
           (list 'hlink md-hlink)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define (serialize-markdown x)
  (cond ((null? x) "")
        ((string? x) x)
        ((symbol? (car x))
         (with fun 
              (ahash-ref serialize-hash (car x))
            (if (!= fun #f)
                (fun x)
                (begin
                  (display* "Skipped " (car x) "\n")
                  (skip x)))))
        (else
         (apply string-append 
                (cons (serialize-markdown (car x))
                      (map serialize-markdown (cdr x)))))))