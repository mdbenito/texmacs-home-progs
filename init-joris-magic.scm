;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               Joris' Magic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(tm-define binary-relations
  '("=" "<assign>" "<ne>" "<neq>" "<longequal>" "<less>" "<gtr>" "<le>" "<leq>"
    "<prec>" "<preceq>" "<ll>" "<lleq>" "<subset>" "<subseteq>"
    "<sqsubset>" "<sqsubseteq>" "<in>" "<ni>" "<of>"
    "<ge>" "<geq>" "<succ>" "<succeq>"
    "<gg>" "<ggeq>" "<supset>" "<supseteq>" "<sqsupset>" "<sqsupseteq>"
    "<equiv>" "<nequiv>" "<sim>" "<simeq>" "<asymp>" "<approx>" "<cong>"
    "<subsetsim>" "<supsetsim>" "<doteq>" "<propto>" "<varpropto>"
    "<perp>" "<bowtie>" "<Join>" "<smile>" "<frown>" "<signchange>"
    "<parallel>" "<shortparallel>" "<nparallel>" "<nshortparallel>"
    "<shortmid>" "<nshortmid>" "<nmid>" "<divides>" "<ndivides>"

    "<approxeq>" "<backsim>" "<backsimeq>" "<Bumpeq>" "<bumpeq>" "<circeq>"
    "<curlyeqprec>" "<curlyeqsucc>" "<Doteq>" "<doteqdot>" "<eqcirc>"
    "<eqslantgtr>" "<eqslantless>" "<fallingdotseq>" "<geqq>" "<geqslant>"
    "<ggg>" "<gggtr>" "<gnapprox>" "<gneq>" "<gneqq>" "<gnsim>" "<gtrapprox>"
    "<gtrdot>" "<gtreqdot>" "<gtreqless>" "<gtreqqless>" "<gtrless>"
    "<gtrsim>" "<gvertneqq>" "<leqq>" "<leqslant>" "<lessapprox>"
    "<lessdot>" "<lesseqdot>" "<lesseqgtr>" "<lesseqqgtr>" "<lessgtr>"
    "<lesssim>" "<lll>" "<llless>" "<lnapprox>" "<lneq>" "<lneqq>"
    "<lnsim>" "<lvertneqq>" "<napprox>" "<ngeq>" "<ngeqq>" "<ngeqslant>"
    "<ngtr>" "<nleq>" "<nleqq>" "<nleqslant>" "<nless>" "<nprec>" "<npreceq>"
    "<nsim>" "<nsimeq>""<ncong>" "<nasymp>" "<nsubset>" "<nsupset>"
    "<nsqsubset>" "<nsqsupset>" "<nsqsubseteq>" "<nsqsupseteq>"
    "<nsubseteq>" "<nsucc>" "<nsucceq>"
    "<nsupseteq>" "<nsupseteqq>" "<precapprox>" "<preccurlyeq>"
    "<npreccurlyeq>" "<precnapprox>" "<precneqq>"
    "<precsim>" "<precnsim>" "<risingdoteq>" "<Subset>"
    "<subseteqq>" "<subsetneq>" "<subsetneqq>" "<succapprox>"
    "<succcurlyeq>" "<nsucccurlyeq>" "<succnapprox>" "<succneqq>"
    "<succsim>" "<succnsim>" "<Supset>" "<supseteqq>"
    "<supsetneq>" "<supsetneqq>"
    "<thickapprox>" "<thicksim>" "<varsubsetneq>" "<varsubsetneqq>"
    "<varsupsetneq>" "<varsupsetneqq>" "<llleq>" "<gggeq>"
    "<subsetplus>" "<supsetplus>"

    "<vartriangleleft>" "<vartriangleright>"
    "<triangleleft>" "<triangleright>"
    "<trianglelefteq>" "<trianglerighteq>" "<trianglelefteqslant>"
    "<trianglerighteqslant>" "<blacktriangleleft>" "<blacktriangleright>"
    "<ntriangleleft>" "<ntriangleright>"
    "<ntrianglelefteq>" "<ntrianglerighteq>"
    "<ntrianglelefteqslant>" "<ntrianglerighteqslant>"

    "<precprec>" "<precpreceq>" "<precprecprec>" "<precprecpreceq>"
    "<succsucc>" "<succsucceq>" "<succsuccsucc>" "<succsuccsucceq>"
    "<nprecprec>" "<nprecpreceq>" "<nprecprecprec>" "<nprecprecpreceq>"
    "<nsuccsucc>" "<nsuccsucceq>" "<nsuccsuccsucc>" "<nsuccsuccsucceq>"
    "<asympasymp>" "<nasympasymp>" "<simsim>" "<nsimsim>" "<nin>" "<nni>"
    "<notin>" "<notni>" "<precdot>" "<preceqdot>"
    "<dotsucc>" "<dotsucceq>"))

(tm-define (binary-relation? t)
  (in? t binary-relations))

(tm-define (eqnarray->equation)
  (with-innermost t 'eqnarray*
    (with l (select t '(document tformat table row cell 0))
      (tree-set! t `(equation* (document ,(apply tmconcat l))))
      (tree-go-to t 0 :end))))

(tm-define (atom-decompose t)
  (if (tree-atomic? t)
      (tmstring->list (tree->string t))
      (list t)))

(tm-define (concat-decompose t)
  (cond ((tree-atomic? t) (atom-decompose t))
        ((tree-is? t 'concat)
         (apply append (map atom-decompose (tree-children t))))
        (else (list t))))

(tm-define (make-eqn-row-sub l)
  (list "" (car l) (apply tmconcat (cdr l))))

(tm-define (finalize-row l)
  `(row (cell ,(car l)) (cell ,(cadr l)) (cell ,(caddr l))))

(tm-define (equation->eqnarray)
  (with-innermost t 'equation*
    (let* ((l1 (concat-decompose (tree-ref t 0 0)))
           (l2 (list-scatter l1 binary-relation? #t)))
      (when (>= (length l2) 2)
        (let* ((l3 (cons (list (apply tmconcat (car l2))
                               (caadr l2)
                               (apply tmconcat (cdadr l2)))
                         (map make-eqn-row-sub (cddr l2))))
               (l4 (map finalize-row l3))
               (r `(eqnarray* (document (tformat (table ,@l4))))))
          (tree-set! t r)
          (tree-go-to t 0 0 0 :last :last 0 :end))))))

(kbd-map
  (:require (inside? 'eqnarray*))
  ("C-&" (eqnarray->equation)))

(kbd-map
  (:require (inside? 'equation*))
  ("C-&" (equation->eqnarray)))

