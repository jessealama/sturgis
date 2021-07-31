#lang racket/base

(provide xpath)

(require racket/list
         racket/function
         syntax/parse
         (for-syntax syntax/parse
                     syntax/parse/class/paren-shape
                     racket/base)
         "parameters.rkt"
         "types.rkt"
         "equality.rkt"
         (prefix-in axis: "axes.rkt"))

(module+ test
  (require rackunit
           (prefix-in xml: xml)
           "convert.rkt"))

; -> element-node?
(define (root)
  (define (find-it n)
    (define p (node-parent n))
    (cond [(eq? #f p) n]
          [else (find-it p)]))
  (define node (current-node))
  (cond [(eq? #f node)
         (error "Current node not set (or empty)")]
        [else
         (find-it node)]))

; -> (listof node?)
(define (enumerate-nodes)
  (define n (current-node))
  (when (eq? #f n)
    (error "current node not set!"))
  (case (current-axis)
    ['ancestor (axis:ancestor n)]
    ['ancestor-or-self (axis:ancestor-or-self n)]
    ['attribute (axis:attribute n)]
    ['child (axis:child n)]
    ['descendant (axis:descendant n)]
    ['descendant-or-self (axis:descendant-or-self n)]
    ['following (axis:following n)]
    ['following-sibling (axis:following-sibling n)]
    ['namespace (axis:namespace n)]
    ['parent (axis:parent n)]
    ['preceding (axis:preceding n)]
    ['preceding-sibling (axis:preceding-sibling n)]
    ['self (axis:self n)]))

(define (reset-axis)
  (current-axis 'child))

; string? -> void
(define (element [name #f])
  (define pred (cond [(string? name)
                      (lambda (n)
                        (and (element-node? n)
                             (string=? name (element-node-name n))))]
                [else
                 element-node?]))
  (filter pred (enumerate-nodes)))

(define (nodes-with-name name)
  (define pred (lambda (n)
                 (cond [(element-node? n)
                        (string=? name (element-node-name n))]
                       [(attribute-node? n)
                        (string=? name (attribute-node-name n))]
                       [else #f])))
  (filter pred (enumerate-nodes)))

; string? -> (xdm-node? -> boolean?)
(define (element-has-name? name)
  (lambda (n)
    (and (element-node? n)
         (string=? name (element-node-name n)))))

(define (text)
  (filter text-node? (enumerate-nodes)))

; string? -> (listof attribute-node?)
(define (attribute name)
  (define n (current-node))
  (cond [(and (attribute-node? n)
              (string=? name (attribute-node-name n)))
         (list n)]
        [(element-node? n)
         (define attrs (element-node-attributes n))
         (define a (findf (lambda (attr)
                            (string=? name (attribute-node-name attr)))
                          (element-node-attributes n)))
         (cond [(eq? #f a)
                (list)]
               [else
                (list a)])]
        [else (list)]))

#|

Examples we should handle:

(xpath "A")
(xpath / "A")
(xpath "A" [1])
(xpath // "A")
(xpath // (= #:id (following * #:id)))

|#

(define (take/safe lst n)
  (with-handlers ([exn:fail:contract? (lambda (e) (list))])
    (take lst n)))

(define (drop/safe lst n)
  (with-handlers ([exn:fail:contract? (lambda (e) (list))])
    (drop lst n)))

(define (flatten items)
  (cond [(null? items)
         (list)]
        [(list? (car items))
         (append (flatten (car items))
                 (flatten (cdr items)))]
        [else
         (cons (car items)
               (flatten (cdr items)))]))

(define-syntax (xpath-predicates stx)
  (syntax-parse stx
    ;; multiple predicates
    [(_ p1 p ...+)
     #'(conjoin (xpath-predicates p1)
                (xpath-predicates p ...))]
    ;; atomic cases
    [(_ (~parens (~datum =) x y))
     #'(lambda (n)
         (parameterize ([current-node n])
           (xdm-equal? (xpath/top x)
                       (xpath/top y))))]))

(begin-for-syntax
  (define-syntax-class symbol (pattern ({~literal quote} test:id))))

(define-syntax (xpath/top stx)
  (syntax-parse stx
    ;; normal axes cases
    [(_ (~parens (~datum ancestor) a ...))
     #'(parameterize ([current-axis 'ancestor])
         (xpath/top a ...))]
    [(_ (~parens (~datum ancestor-or-self) a ...))
     #'(parameterize ([current-axis 'ancestor-or-self])
         (xpath/top a ...))]
    [(_ (~parens (~datum attribute) a ...))
     #'(parameterize ([current-axis 'attribute])
         (xpath/top a ...))]
    [(_ (~parens (~datum child) a ...))
     #'(parameterize ([current-axis 'child])
         (xpath/top a ...))]
    [(_ (~parens (~datum descendant) a ...))
     #'(parameterize ([current-axis 'descendant])
         (xpath/top a ...))]
    [(_ (~parens (~datum descendant-or-self) a ...))
     #'(parameterize ([current-axis 'descendant-or-self])
         (xpath/top a ...))]
    [(_ (~parens (~datum following) a ...))
     #'(parameterize ([current-axis 'following])
         (xpath/top a ...))]
    [(_ (~parens (~datum following-sibling) a ...))
     #'(parameterize ([current-axis 'following-sibling])
         (xpath/top a ...))]
    [(_ (~parens (~datum namespace) a ...))
     #'(parameterize ([current-axis 'namespace])
         (xpath/top a ...))]
    [(_ (~parens (~datum parent) a ...))
     #'(parameterize ([current-axis 'parent])
         (xpath/top a ...))]
    [(_ (~parens (~datum preceding) a ...))
     #'(parameterize ([current-axis 'preceding])
         (xpath/top a ...))]
    [(_ (~parens (~datum preceding-sibling) a ...))
     #'(parameterize ([current-axis 'preceding-sibling])
         (xpath/top a ...))]
    [(_ (~parens (~datum self) a ...))
     #'(parameterize ([current-axis 'self])
         (xpath/top a ...))]

    [(_ (~datum /) a ...)
     #'(parameterize ([current-node (root)]
                      [current-axis 'child])
         (xpath/top a ...))]
    [(_ (~datum //) a ...)
     #'(parameterize ([current-node (root)]
                      [current-axis 'descendant])
         (xpath/top a ...))]
    [(_ (~datum *) a ...)
     #'(xpath/top (element) a ...)]
    [(_ attr:keyword a ...+)
     (with-syntax [(attr/string (keyword->string (syntax->datum #'attr)))]
       #'(parameterize ([current-axis 'attribute])
           (xpath/top (attribute attr/string) a ...)))]

    ;; terminal cases
    [(_ (~datum *))
     #'(element)]
    [(_ test:symbol)
     ; looks like (quote thing)
     (with-syntax ([n (symbol->string (syntax->datum (cadr (syntax-e #'test))))])
       #'(nodes-with-name n))]
    [(_ attr:keyword)
     (with-syntax [(a (keyword->string (syntax->datum #'attr)))]
       #'(parameterize ([current-axis 'attribute])
           (nodes-with-name a)))]
    [(_ a [~brackets pos:exact-nonnegative-integer])
     #'(xpath/top a [(= (position) pos)])]
    [(_ [~brackets test ...+])
     #'(filter (xpath-predicates test ...)
               (enumerate-nodes))]
    [(_ a (~datum /) b)
     #'(let ([nodes (xpath/top a)])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)])
            (parameterize ([current-node n]
                           [current-position (add1 i)])
              (xpath/top b)))))]

    ;; step cases
    [(_ a (~datum /) b ...+)
     #'(let ([nodes (xpath/top a)])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)])
            (parameterize ([current-node n]
                           [current-position (add1 i)])
              (xpath/top b ...)))))]
    [(_ a (~datum //) b ...+)
     #'(let ([nodes (xpath/top a)])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)])
            (parameterize ([current-node n]
                           [current-position (add1 i)]
                           [current-axis 'descendant])
              (xpath/top b ...)))))]

    ;; predicate case
    [(_ a [~brackets pos:exact-nonnegative-integer] b ...+)
     #'(xpath/top a [(= (position) pos)] b ...)]
    [(_ a [~brackets test ...+])
     #'(let ([nodes (xpath/top a)])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)]
                     #:when ((xpath-predicates test ...) n))
            n)))]
    [(_ a [~brackets test ...+] (~datum /) b ...+)
     #'(let ([nodes (xpath/top a [test ...])])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)])
            (parameterize ([current-node n]
                           [current-position (add1 i)])
              (xpath/top b ...)))))]
    [(_ a [~brackets test ...+] (~datum //) b ...+)
     #'(let ([nodes (xpath/top a [test ...])])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)])
            (parameterize ([current-node n]
                           [current-position (add1 i)]
                           [current-axis 'descendant])
              (xpath/top b ...)))))]
    [(_ a [~brackets test ...+] b ...+)
     #'(let ([nodes (xpath/top a)])
         (flatten
          (for/list ([n nodes]
                     [i (length nodes)]
                     #:when ((xpath-predicates test ...) n))
            (parameterize ([current-node n]
                           [current-position (add1 i)])
              (xpath/top b ...)))))]

    ;; basic data
    [(_ n:nat)
     #'n]
    [(_ s:string)
     #'s]

    ;; functions
    [(_ (~parens (~datum element)))
     #'(element)]
    [(_ (~parens (~datum position)))
     #'(current-position)]
    [(_ (~parens (~datum text)))
     #'(text)]))

(define-syntax (xpath stx)
  (syntax-parse stx
    [(_)
     (error "At least one step is needed")]

    ;; error cases for named axes
    [(_ (~parens (~datum ancestor)))
     (error "Step missing after ancestor axis")]
    [(_ (~parens (~datum ancestor-or-self)))
     (error "Step missing after ancestor-or-self axis")]
    [(_ (~parens (~datum attribute)))
     (error "Step missing after attribute axis")]
    [(_ (~parens (~datum child)))
     (error "Step missing after child axis")]
    [(_ (~parens (~datum descendant)))
     (error "Step missing after descendant axis")]
    [(_ (~parens (~datum descendant-or-self)))
     (error "Step missing after descendant-or-self axis")]
    [(_ (~parens (~datum following)))
     (error "Step missing after following axis")]
    [(_ (~parens (~datum following-sibling)))
     (error "Step missing after following-sibling axis")]
    [(_ (~parens (~datum namespace)))
     (error "Step missing after namespace axis")]
    [(_ (~parens (~datum parent)))
     (error "Step missing after parent axis")]
    [(_ (~parens (~datum preceding)))
     (error "Step missing after preceding axis")]
    [(_ (~parens (~datum preceding-sibling)))
     (error "Step missing after preceding-sibling axis")]
    [(_ (~parens (~datum self)))
     (error "Step missing after self axis")]

    ;; incomplete
    [(_ (~datum //))
     (error "Step missing after //")]

    ;; let's roll
    [(_ a ...)
     #'(atomize (xpath/top a ...))]))

(module+ test
  (define test-doc/string #<<DOC
<A>
  <B id="foo">
    <A/>
  </B>
  <C id="foo"><A id="bar"/></C>
</A>
DOC
    )
  (define test-doc/xml (xml:read-xml/document (open-input-string test-doc/string)))
  (define test-doc/xdm (xml->xdm test-doc/xml))
  (parameterize ([current-node test-doc/xdm])
    (check-equal? (length (xpath 'A))
                  1)
    (check-equal? (length (xpath 'A / 'B))
                  1)
    (check-equal? (length (xpath 'A // 'A))
                  2)
    (check-equal? (length (xpath 'A / 'Z))
                  0)
    (check-equal? (length (xpath / 'A))
                  1)
    (check-equal? (length (xpath 'A [1]))
                  1)
    (check-equal? (length (xpath 'A [2]))
                  0)
    (check-equal? (length (xpath // 'A))
                  3)
    (check-equal? (length (xpath // * [(= #:id (following * / #:id))]))
                  1)
    (check-equal? (length (xpath // * [(= #:id (preceding * / #:id))]))
                  1)
    (check-equal? (length (xpath // * [(= #:id (self * / #:id))]))
                  3)
    (check-equal? (length (xpath / 'A / 'C [(= #:id "foo")] / 'A))
                  1)
    (check-equal? (length (xpath / 'A / 'B / #:id))
                  1)
    (check-equal? (length (xpath // * [#:id]))
                  3)))
