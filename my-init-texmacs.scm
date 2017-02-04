(load "init-kbd.scm")
(load "init-slo.scm")
(load "init-joris-magic.scm")
(load "init-autocomplete.scm")
(load "init-focusbar.scm")

(use-modules (dynamic session-edit))
(set-session-multiline-input "python" "default" #t)
(set-session-multiline-input "scheme" "default" #t)
