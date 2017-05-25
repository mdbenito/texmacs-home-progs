;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Initialization of the autocomplete feature for scheme sessions.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (prog scheme-autocomplete))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Autocomplete for glue functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define file1 
  (url-concretize "~/Devel/TeXmacs/svn/src/src/Scheme/Glue/build-glue-server.scm"))
(define file2 
  (url-concretize "~/Devel/TeXmacs/svn/src/src/Scheme/Glue/build-glue-editor.scm"))
(define file3
  (url-concretize "~/Devel/TeXmacs/svn/src/src/Scheme/Glue/build-glue-basic.scm"))

(define-public (output-copyright arg) '())

(define (parse-def l)
  (cond ((null? l) '())
        ((not (list? (car l))) (parse-def (cdr l)))
        ((and (symbol? (caar l)) (defined? (caar l)))
         (cons (caar l) (parse-def (cdr l))))
        (else (parse-def (cdr l)))))

(define-macro build
  (lambda l
    (with sl (parse-def l)
      (scheme-completions-add-list (map symbol->string sl)))))

(define (parse-glue port)
  (with ev (eval (read port))
    (if (eof-object? ev) '()
      (parse-glue port))))

(delayed (:idle 2000) (call-with-input-file file1 parse-glue))
(delayed (:idle 3000) (call-with-input-file file2 parse-glue))
(delayed (:idle 4000) (call-with-input-file file3 parse-glue))
;(delayed (:idle 5000) (scheme-completions-rebuild))

