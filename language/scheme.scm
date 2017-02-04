
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : scheme.scm
;; DESCRIPTION : syntax of the scheme language
;; COPYRIGHT   : (C) 2012 Miguel de Benito Delgado
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Only the mini-scheme grammar is remotely usable, though it's mostly a test.
;; The scheme2 grammar is incomplete and doesn't work at all. Mainly because
;; of unhandled whitespace.
;;
;; Notes:
;;  - Remember that the top level of any definition has an implicit 'or
;;  - Remember that or'ing is done in order, first matched exits the rule:
;;    this might be used to optmize, but is mostly important for the design of
;;    the rules.
;;  - The packrat parser is supposed to run linear on inputlength * numrules,
;;    keeping them low should be important
;;  - I want akeyword like (:selectable disallow), which exists, but intended
;;    for whitespace. Tell packrat_select not to extend the box over whitespace
;;  - See other comments in scheme-grammar.tm for interaction with the indexer.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (language scheme))

(define-language scheme-operators
  (:synopsis "Operators and keywords for the formal syntax of scheme")

  (define Spc~ 
    (* (or " " "\t" "\n")))

  (define Spc
    (Spc~ Comment)
    Spc~)

  (define Space~ 
    (+ (or " " "\t" "\n")))

  (define Space
    (Space~ Comment)
    Space~)

  (define BlockCommentStart
    (Spc "#!"))

  (define BlockCommentEnd
    ("!#" Spc "\n"))  ; how do I say "begin of line"??

  (define BlockComment
    (:highlight comment)
    (BlockCommentStart (* (or AnyCharButNL "\n")) BlockCommentEnd))
 
  (define Comment
    (:highlight comment)
    (Spc ";" (* AnyCharButNL) "\n"))

  (define AnyCharButNL   ; Temporary
    (or Letter Digit " " "\t"))

  (define Open
    ("(" Spc))

  (define Close
    (Spc ")"))

  (define OpenVector
    ("#(" Spc))

  (define CloseVector
    (Spc ")"))

  (define DotPair
    (:operator infix)
    (Space "." Space))

  (define QuoteMark
    (:highlight keyword)
    "'")

  (define BackQuoteMark
    (:highlight keyword)
    "`")

  (define UnquoteMark
    (:highlight keyword)
    ",")

  (define UnquoteSplicingMark
    (:highlight keyword)
    ",@")

  (define KeywordQuote
    (:highlight keyword)
    "quote")

  (define KeywordQuasiquote
    (:highlight keyword)
    "quasiquote")

  (define KeywordBegin
    (:highlight keyword)
    "begin")

  (define KeywordLet
    (:highlight keyword)
    "let")

  (define KeywordCond
    (:highlight keyword)
    "cond")

  (define KeywordLetSyntax
    (:highlight keyword)
    "let-syntax")

  (define KeywordLetRecSyntax
    (:highlight keyword)
    "letrec-syntax")

  (define KeywordDefine
    (:highlight keyword)
    "define")

  (define KeywordDefinePublic
    (:highlight keyword)
    "define-public")
 
  (define KeywordDefineSyntax
    (:highlight keyword)
    "define-syntax")

  (define KeywordSyntaxRules
    (:highlight keyword)
    "syntax-rules")

  (define KeywordLambda
    (:highlight keyword)
    "lambda")

  (define KeywordIf
    (:highlight keyword)
    "if")

  (define KeywordSet
    (:highlight keyword)
    "set!"))

(define-language scheme-grammar
  (:synopsis "Formal definition of scheme grammar")

  ;;;;;;;;;;;;;;; Programs
 
  (define Main
    (Spc (* Form) Spc))

  (define Form
    ((or Definition Expression) Spc))

  ;;;;;;;;;;;;;;; Definitions  

  (define Definition
    VariableDefinition
    SyntaxDefinition
    (Open KeywordBegin Spc (* Definition) Close)
    (Open KeywordLetSyntax Spc Open (* SyntaxBinding) Close
          Spc (* Definition) Close)
    (Open KeywordLetRecSyntax Spc Open (* SyntaxBinding) Close
          Spc (* Definition) Close))
  ;  DerivedDefinition)
  ;  "Derived definitions are syntactic extensions that expand into some form
  ;  of definition"

  (define VariableDefinition
    (Open KeywordDefine Spc DefinedVariable Spc Expression Close)
    (Open KeywordDefine Spc 
          Open DefinedVariable (* (Spc DefinedArgument)) Close Spc
          DefineBody Close)
    (Open KeywordDefine Spc 
          Open DefinedVariable (* (Spc DefinedArgument))
               DotPair DefinedArgument Close Spc
          DefineBody Close))

  (define Variable
    (:highlight variable_identifier)
    Identifier)

  (define DefinedVariable 
    (:highlight variable_function)
    Identifier)

  (define DefineBody
    ((* Definition) Spc (+ Expression)))

  (define SyntaxDefinition
    (Open KeywordDefineSyntax Spc Keyword Spc TransformerExpression Close))

  (define SyntaxBinding
    (Open Keyword Spc TransformerExpression Close))

  (define Keyword
    (:highlight variable_function)
    Identifier)

  (define TransformerExpression
    (Open KeywordSyntaxRules Spc 
          Open (* Identifier Spc) Close Spc (* SyntaxRule Spc) Close))

  (define SyntaxRule
    (Open Pattern Spc Template Close))

  (define Pattern
    PatternIdentifier
    (Open (* Pattern Spc) Close)
    (Open (* Pattern Spc) Pattern DotPair Pattern Close)
    (Open (* Pattern Spc) Pattern Ellipsis Close)
    (OpenVector (* Pattern Spc) CloseVector)
    (OpenVector (* Pattern Spc) Pattern Ellipsis CloseVector)
    PatternDatum)

  (define PatternDatum
    String
    Character
    Boolean
    Number)

  (define Template
    PatternIdentifier
    (Open (* TemplateElement Spc) Close)
    (Open (* TemplateElement Spc) TemplateElement DotPair Template Close)
    (OpenVector (* TemplateElement Spc) Close)
    TemplateDatum)

  (define TemplateElement
    Template
    Template Ellipsis Spc)

  (define TemplateDatum
    PatternDatum)

  (define PatternIdentifier
    Identifier)
    ;(except DotsIdentifier Identifier)) ; FIXME: What's the syntax?

  ;;;;;;;;;;;;;;; Expressions  

  (define Expression
    Constant
    Variable
    (Open (or KeywordQuote "'") Datum Close)
    (Open KeywordLambda Spc Formals Spc Body Close)
    (Open KeywordIf Spc Expression Spc Expression Spc Expression Close)
    (Open KeywordIf Spc Expression Spc Expression Spc Close)
    (Open KeywordSet Spc Variable Spc Expression Close)
    Application
    (Open KeywordBegin Spc (* (Definition Spc)) Spc (* (Expression Spc)) Close)
    (Open KeywordLetSyntax Spc 
          Open (* SyntaxBinding Spc) Close (* (Definition Spc)) Close)
    (Open KeywordLetRecSyntax Spc Open (* (SyntaxBinding Spc)) Close
          (* (Definition Spc)) Close))
    ;DerivedExpression)
    ; "Derived expressions include and, begin, case, cond, delay, do, let,
    ;  let*, letrrec or and quasiquote expressions plus syntactic extensions
    ;  that expand into some form of expression"

  (define Constant
    Boolean Number Character String)

  (define Formals
    Variable
    (Open (* Variable Spc) Close)
    (Open (* Variable Spc) Variable DotPair Variable Close))

  (define Application
    (Open Expression Spc (* (Expression Spc)) Close))

  ;;;;;;;;;;;;;;; Identifiers
  
  (define DotsIdentifier "...")
  (define PlusIdentifier "+")
  (define MinusIdentifier "-")

  (define Identifier ; a subset: unicode > 127 is also valid and many others
    (:highlight variable_identifier)
    (:atomic)
    (Initial (* Subsequent) PlusIdentifier MinusIdentifier DotsIdentifier))

  (define Initial
    (or Letter "!" "$" "%" "&" "*" "+" "-" "." "/" ":" "<" "=" ">" "?" "@" 
               "^" "_" "~" ))

  (define Subsequent (or Initial Digit "." "+" "-"))

  (define Letter (or (- "a" "z") (- "A" "Z")))

  (define Digit (- "0" "9"))

  ;;;;;;;;;;;;;;; Data

  (define Datum
    Boolean
    Number
    Character
    String
    Symbol
    List
    Vector)

  (define Boolean
    (:highlight constant_type)
    (:atomic)
    (or "#t" "#f"))

  (define Number  ; WRONG: missing radixes (bases)
    (:highlight constant_number)
    (:atomic)
    ((+ (- "0" "9")) (or "" ("." (+ (- "0" "9"))))))

  (define Character
    (:highlight constant_number)
    (:atomic)
    (or "#\\newline" "#\\space" ("#\\" (or Letter Digit)))) ; last is WRONG (need "any")

  (define String
    (:highlight constant_string)
    (:atomic)
    ("\"" StringCharacter "\""))

  (define StringCharacter
    (or "\\\"" "\\\\" )) ;FIXME what's the syntax(except "\"" :any)))

  (define Symbol
    Identifier)

  (define List
    (Open (* Datum Spc) Close)
    (Open (* Datum Spc) Datum DotPair Datum Close)
    Abbreviation)

  (define Abbreviation
    (QuoteMark Datum)
    (BackQuoteMark Datum)
    (UnquoteMark Datum)
    (UnquoteSplicingMark Datum))

  (define Vector
    (OpenVector (* Datum Spc) CloseVector)))

(define-language scheme2
  (:synopsis "Syntax for the scheme programming language")
  (inherit scheme-operators)
  (inherit scheme-grammar))

(define-language mini-scheme
  (:synopsis "Toy syntax")
  
  (define Space~ (+ (or " " "\n" "\t")))
  (define Spc~   (* (or " " "\n" "\t")))
  (define Space  (Spc~ Comment) Space~)
  (define Spc    (Spc~ Comment) Spc~)
  
  (define Open ("(" Spc))
  (define Close (Spc ")"))
  (define DotPair (:operator infix) (Spc "." Spc))
  (define Comment (:highlight comment) (:atomic)
    (Spc~ ";" (* (or Letter Digit Other " " "\t" )) "\n"))
  
  (define KeywordBegin (:highlight keyword) "begin")
  (define KeywordDefine (:highlight declare_function) "define")
  (define KeywordDefinePublic (:highlight declare_function)
          "define-public")
  (define KeywordLet (:highlight keyword) "let")
  (define KeywordCond (:highlight keyword) "cond")
  (define KeywordLambda (:highlight declare_function) "lambda")
  (define KeywordIf (:highlight keyword) "if")
  (define KeywordSet (:highlight keyword) "set!")
  
  (define Main (Spc (+ Form) Spc))
  (define Form ((or Definition Expression) Spc))
  
  (define Definition
   VariableDefinition
   (Open KeywordBegin Spc (* (Definition Spc)) Close))
  
  (define VariableDefinition
    (Open KeywordDefine Space DefinedVariable Spc Expression Close)
    (Open KeywordDefine Spc 
          Open DefinedVariable (* (Space DefinedArgument)) Close Spc
          DefineBody Close)
    (Open KeywordDefine Spc 
          Open DefinedVariable (* (Space DefinedArgument))
               DotPair DefinedArgument Close Spc
          DefineBody Close))
  
  (define DefineBody ((* (Definition Spc)) Spc (+ (Expression Spc))))

  (define Variable        (:highlight variable_identifier) Identifier)
  (define DefinedVariable (:highlight declare_identifier)  Identifier)
  (define DefinedArgument (:highlight declare_function)    Identifier)
  
  ;;;;;;;;;;;;;;; Expressions  

  (define Expression
    (Open (or (KeywordQuote Spc) "'") Datum Close)
    (Open KeywordLambda Spc Formals Spc Body Close)
    (Open KeywordIf Spc Expression Spc Expression Spc Expression Close)
    (Open KeywordIf Spc Expression Spc Expression Spc Close)
    (Open KeywordSet Spc Variable Spc Expression Close)
    (Open KeywordBegin Spc BeginBody Close)
    Constant
    Variable
    Application)
    ;DerivedExpression)
    ; "Derived expressions include and, begin, case, cond, delay, do,
    ;  let, let*, letrrec or and quasiquote expressions plus syntactic
    ;  extensions that expand into some form of expression"
    
  (define BeginBody ((* (Definition Spc)) Spc (* (Expression Spc))))

  (define Formals
    DefinedArgument
    (Open (* DefinedArgument Spc) Close)
    (Open (* DefinedArgument Spc) DefinedArgument DotPair DefinedArgument
          Close))

  (define Application (Open Expression Spc (* (Expression Spc)) Close))
  (define Constant    (or Boolean Number Character String))
  
  ;;;;;;;;;;;;;;; Identifiers
  
  (define Identifier (:atomic) 
     ((or Letter Other) (* (or Letter Digit Other))))
  
  (define DotsIdentifier "...")
  (define PlusIdentifier "+")
  (define MinusIdentifier "-")
  
  (define Boolean (:highlight constant_type)   (:atomic) (or "#t" "#f"))
  (define Number  (:highlight constant_number) (:atomic)
    (or (+ Digit) ((+ Digit) "." (+ Digit))))      ; doesn't work.
  
  (define String  (:highlight constant_string) (:atomic)
    ("\"" (* (or Letter Digit Other " " "\t" )) "\""))

  (define Letter  (or (- "a" "z") (- "A" "Z")))
  (define Digit   (- "0" "9"))
  (define Other   "!" "$" "%" "&" "*" "+" "-" "." "/" ":" "<" "=" ">"
                  "?" "@" "^" "_" "~" "{" "}" "[" "]" ))
