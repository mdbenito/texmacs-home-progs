;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : itemize-trees.scm
;; DESCRIPTION : Heuristically transform trees into lists of items
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The contents of this file are preliminary and simple. Things TO-DO are:
;;  - Operate on trees and not on selections.
;;  - process comma separated lists.
;;  - implement nested lists.
;;  - provide some options and interface for configuration.
;;  -
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (text itemize-trees)
  (:use (text std-text-edit)))

(define (clean-leading s)
  (cond ((string-null? s) s)
        ((== (string-take s 1) " ") (clean-leading (string-drop s 1)))
        ((== (string-take s 1) "-") (clean-leading (string-drop s 1)))
        (else s)))

; TODO: read leading whitespace to indent new sublists
(define (par->item p)
   (if (string? p) (set! p (clean-leading p)))
  `(document (concat (item) ,p)))

(define (pars->items st)
  (if (func? st 'document)
      (map par->item (cdr st))
      '() ; TODO: do something here?
      ))

(define (selection-make-tmlist type)
  "Convert all paragraphs in the selection to a tmlist of type @type"
  (with st (tree->stree (selection-tree))
    (insert `(,type (document ,@(pars->items st))))))

(define (remove-tmlist st type)
  "Rewrite the scheme tree @st without the symbol @type and any (item)s" 
  (cond ((or (null? st) (nlist? st)) st)
        ((== (car st) type) (car (remove-tmlist (cdr st) type)))
        ((== (car st) '(item)) (remove-tmlist (cdr st) type))
        (else (cons (remove-tmlist (car st) type)
                    (remove-tmlist (cdr st) type)))))

(define (selection-remove-tmlist type)
  (with st (tree->stree (selection-tree))
    (if (== (car st) type)
        (insert (remove-tmlist st type))
        (noop))))

(tm-define (make-tmlist l)
  (:require (selection-active-any?))
    (selection-make-tmlist l))

(menu-bind tools-selections-menu
   (former)
   (if (selection-active-any?)
       ("Itemize" (selection-make-tmlist 'itemize))
       ("Deitemize" (selection-remove-tmlist 'itemize))))

