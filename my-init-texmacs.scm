(use-modules (dynamic session-edit)
             (convert markdown init-markdown))
(set-session-multiline-input "python" "default" #t)
(set-session-multiline-input "scheme" "default" #t)

(load "init-joris-magic.scm")
(load "init-perso.scm")

