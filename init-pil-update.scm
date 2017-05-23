(use-modules (bibtex bib-complete))

(tm-define (pil-update)
  (with u (current-bib-file #f)
    (unless (url-none? u)
      (let ((bibpath (url-concretize u))
            (path "/Users/ana/Dropbox/TeXmacs")
            (script "capitalize_months.py"))
        (system (string-append path "/" script " \"" bibpath "\""))
        (update-document "all")))))

(menu-bind document-update-menu
  (former)
  ---
  ("Pil update" (pil-update)))
