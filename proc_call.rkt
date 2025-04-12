#lang at-exp slideshow
(require pict/balloon2
         racket/class
         racket/draw
         (only-in "common.rhm"))

(provide make-stack
         call-example-picts
         overflow-example-picts
         guarded-stack
         code-on-stack-picts
         example-rop-stack
         example-rop2-stack
         decrypt-stack)

(define (diagram-box label w h
                     #:position [superimpose cc-superimpose]
                     #:color color)
  (define b 
   (frame (colorize (filled-rectangle (* w gap-size) (* h gap-size)) color)
          #:line-width 2))
  (refocus (superimpose b
                        (inset (if (string? label) (t label) label) (/ gap-size 2)))
           b))

(define CPU-background-color "PaleTurquoise")
(define CPU-light-background-color "lightcyan")
(define CPU-color "yellowgreen")

(define (shifted *-find dx dy)
  (lambda (p sp)
    (define-values (x y) (*-find p sp))
    (values (+ x dx) (+ y dy))))

(define para*
  (make-keyword-procedure
   (lambda (kws vals . args)
     (keyword-apply para kws vals #:fill? #f args))))

(define (scale-color c s)
  (cond
   [(string? c) (scale-color (make-object color% c) s)]
   [(list? c) (scale-color (make-color (car c) (cadr c) (caddr c)) s)]
   [else (make-color (min 255 (floor (* s (send c red))))
                     (min 255 (floor (* s (send c green))))
                     (min 255 (floor (* s (send c blue)))))]))

(define (darker c) (scale-color c #e0.9))
(define (lighter c) (scale-color c #e1.1))

(define source-code-color "lightyellow")
(define machine-code-color "pink")
(define goto-color (list 255 200 150))

(define fframe-inset (/ gap-size 2))

(define (fframe p
                #:color [color source-code-color]
                #:name [name #f])
  (define q (inset p fframe-inset))
  (define prog
    (frame (cc-superimpose (colorize (filled-rectangle (pict-width q) (pict-height q))
                                     color)
                           q)))
  (cond
   [name (vr-append (fframe (if (pict? name)
                                name
                                (scale (tt name) 0.8))
                            #:color (lighter color)) prog)]
   [else prog]))

(define (it-para* . s) (parameterize ([current-main-font (cons 'italic (current-main-font))])
                         (apply para* s)))

(define (stack-element h #:color [color "lightblue"])
  (frame (colorize (filled-rectangle (* gap-size 12)
                                     (* gap-size h))
                   color)
         #:line-width 1))

(define frame-pointer-color "purple")

(define stack-slot (stack-element 1.7))
(define stack-frame (stack-element 4))

(define stack-bottom (it-para* "stack ``bottom''"))
(define stack-top (it-para* "stack ``top''"))

(define rsp (hc-append (tt "%rsp") (let ([p (tt " ")])
                                     (pin-arrow-line (/ gap-size 2)
                                                     p 
                                                     p lc-find
                                                     p rc-find))))
(define rbp (hc-append (tt "%rbp") (let ([p (tt "       ")])
                                     (pin-arrow-line (/ gap-size 2)
                                                     p 
                                                     p lc-find
                                                     p rc-find))))

(define (make-stack popped
                    #:stack-base-size [stack-base-size 11]
                    #:shadow [shadow 0]
                    #:concrete? [concrete? #f]
                    #:stack-addresses? [stack-addresses? concrete?]
                    #:desc? [desc? (not concrete?)]
                    #:recents [recents null]
                    #:bytes? [bytes? #f]
                    #:rbp [rbp-pos #f]
                    #:old-rsp [old-rsp-pos #f]
                    #:links [links null]
                    #:early-val [early-val "0xB5A9"]
                    #:mid-val [mid-val "0x789ABC"]) 
  (define stack-vals (if bytes?
                         (list 
                          "0x0" "0x0" "0xFA" "OxDE"
                          "0x0" "0x0" "0x0" "0x0"
                          "0x7" "0xFF" "0xFD" "0xE0")
                         (list "0x1" early-val
                               mid-val
                               "0x2" "0x3" "0xFADE"
                               "0x7FFFFE0")))
  (define recents-len (length recents))
  (vc-append
   stack-bottom
   (vc-append
    -1
    (let ([p (apply vc-append -1 (for/list ([i (in-range (- stack-base-size popped))]
                                            [v (in-cycle stack-vals)])
                                   (define (annotate v c)
                                     (if (vector? v)
                                         (let ([h (pict-height c)])
                                           (refocus (hb-append c
                                                               (blank 2)
                                                               (hc-append
                                                                (vr-append
                                                                 (hline 10 0)
                                                                 (vline 0 (* (vector-ref v 1) h))
                                                                 (hline 10 0))
                                                                (blank 2)
                                                                (tt (vector-ref v 0))))
                                                    c))
                                         c))
                                   (cond
                                    [(or concrete? (pair? recents))
                                     (define recent? (i . >= . (- stack-base-size popped recents-len)))
                                     (refocus 
                                      (hb-append
                                       (/ gap-size 2)
                                       (annotate
                                        (and recent?
                                             (list-ref recents (- stack-base-size popped i 1)))
                                        (cc-superimpose stack-slot
                                                        ((if (or recent? concrete?) values ghost)
                                                         (if recent?
                                                             (let ([s (list-ref recents (- stack-base-size popped i 1))])
                                                               (cond
                                                                 [(vector? s) (if (pict? (vector-ref s 2))
                                                                                  (vector-ref s 2)
                                                                                  (tt (vector-ref s 2)))]
                                                                 [(pict? s) s]
                                                                 [else (tt s)]))
                                                             (tt v)))))
                                       ((if stack-addresses? values ghost)
                                        (inset (tt (format "0x~a"
                                                           (string-upcase (format "~x" (- #x8000000 (* (add1 i) (if bytes? 1 8)))))))
                                               0 0 0 -5)))
                                      stack-slot)]
                                    [else stack-slot])))])
      (define main-stack
        (add-links
         (refocus (ht-append (* 2 gap-size)
                             p
                             ((if desc? values ghost)
                              (vl-append
                               (* (pict-height stack-slot) 4)
                               (colorize
                                (let ([q (inset (vc-append (t "increasing")
                                                           (t "addresses")
                                                           (t " ")
                                                           (t "pop")
                                                           (t "moves up"))
                                                gap-size 0)])
                                  (pin-arrow-line (/ gap-size 2)
                                                  q
                                                  q lb-find
                                                  q lt-find))
                                "forestgreen")
                               (colorize
                                (let ([q (inset (vc-append (t "push") (t "grows down"))
                                                gap-size 0)])
                                  (pin-arrow-line (/ gap-size 2)
                                                  q
                                                  q lt-find
                                                  q lb-find))
                                "red"))))
                  p)
         links))
      (refocus (hb-append (inset 
                           (cond
                             [rbp-pos
                              (vr-append (* (- stack-base-size (+ rbp-pos popped) 1)
                                            (sub1 (pict-height stack-slot)))
                                         (colorize rbp frame-pointer-color)
                                         rsp)]
                             [old-rsp-pos
                              (vr-append (* (- stack-base-size (+ old-rsp-pos popped) 1)
                                            (sub1 (pict-height stack-slot)))
                                         (cellophane (hbl-append (t "old ") rsp) 0.3)
                                         rsp)]
                             [else
                              rsp])
                           0 0 0 (* -1/3 (pict-height rsp)))
                          main-stack)
               main-stack))
    (ct-superimpose
     (clip (inset (frame (colorize
                         (filled-rectangle (pict-width stack-slot)
                                           (* (+ 2 popped) (sub1 (pict-height stack-slot))))
                         "beige")
                         #:line-width 1)
                  1 1 1 -2))
     (let ([top stack-top])
       (cond
        [(or (pair? shadow) (positive? shadow))
         (apply vc-append -1
                (append
                 (for/list ([i shadow])
                   (cc-superimpose (colorize (if (pict? i)
                                                 i
                                                 (tt (if (string? i)
                                                         i
                                                         (list-ref stack-vals
                                                                   (modulo (+ i (- stack-base-size popped))
                                                                           (length stack-vals))))))
                                             "gray")
                                   (ghost stack-slot)))
                 (list top)))]
        [else top]))))))

(define (add-links p l)
  (for/fold ([p p]) ([tf (in-list l)])
    (pin-arrow-line (/ gap-size 2)
                    p
                    p (shifted lt-find 0 (* (+ (car tf) 0.5) (sub1 (pict-height stack-slot))))
                    p (shifted lt-find 0 (* (+ (cadr tf) 0.9) (sub1 (pict-height stack-slot))))
                    #:start-angle pi
                    #:end-angle 0
                    #:color frame-pointer-color)))

(define (pre #:scale [s 1] #:copy? [copy? #f] #:indent [indent 0] . l)
  (let ([p
         (apply vl-append
                (current-line-sep)
                (let ([line->pict
                       (lambda (line)
                         (if (null? line)
                             (tt " ")
                             (let ([ln (apply htl-append (reverse line))])
                               (if (zero? indent)
                                   ln
                                   (htl-append (tt (make-string indent #\space)) ln)))))])
                  (let loop ([l l] [line null] [any? #f])
                    (cond
                      [(empty? l) 
                       (if (null? line)
                           null
                           (list (line->pict line)))]
                      [(equal? (car l) "\n")
                       (if any?
                           (cons (line->pict line)
                                 (loop (cdr l) null #t))
                           (loop (cdr l) line #f))]
                      [else (loop (cdr l) 
                                  (cons (if (pict? (car l))
                                            (car l)
                                            (tt (car l)))
                                        line)
                                  #t)]))))])
    (scale
     p
     s)))

(define main-c
  (fframe
   @pre{
   int main() {
     ....
     f();
     ....
     return 0;
   }
   }))

(define main-asm
  (fframe
   #:color machine-code-color
   @pre{
                  ....
     0x400457:  callq  0x400560 <f>
     0x40045c:  xor    %eax,%eax
                  ....
   }))
           
(define f-c
  (fframe
   @pre{
   void f() {
     char name[10];
     gets(....);
   }
   }))

(define f-asm
  (fframe
   #:color machine-code-color
   @pre{
     0x400560:  sub    $0x18,%rsp
     0x400564:  ....
     0x400570:  callq  0x310 <gets>
     0x400575:  add    $0x18,%rsp
     0x400579:  retq
   }))

(define gets-asm
  (fframe
   #:color machine-code-color
   @pre{
     0x310:  ...
     0x350:  retq
   }))

(define (call-example step in-asm asm-line
                      #:stack? [stack? #t]
                      #:gets? [gets? #t]
                      #:local? [local? #f]
                      #:note [note #f] 
                      #:val0 [val0 ""]
                      #:val1 [val1 ""]
                      #:val2 [val2 #f]
                      #:code-fade [code-fade values])
  (define %rip
    (tt (case step
          [(0) "0x400457"]
          [(1) "0x400560"]
          [(2) "0x400564"]
          [(3) "0x400570"]
          [(4) "   0x310"]
          [(4.5) "   0x350"]
          [(5) "0x400575"]
          [(6) "0x400579"]
          [(7) "0x40045c"])))
  (define sz 0.75)
  (define p
    (ht-append
     (* 2 gap-size)
     (scale
      (vl-append
       (* 2 gap-size)
       (code-fade (ht-append (* 2 gap-size) main-c main-asm))
       (rc-superimpose
        (lc-superimpose
         (launder (ghost main-asm))
         (code-fade
          ((if stack? values ghost)
           (diagram-box
            (table 2
                   (list (t "register") (t "value")
                         (tt "%rip") %rip)
                   (cons lbl-superimpose rbl-superimpose) cbl-superimpose
                   gap-size (current-line-sep))
            14 4
            #:color CPU-color))))
        ((if gets? values ghost) (code-fade
                                  (inset (refocus (vl-append
                                                   (current-line-sep)
                                                   (tt "gets")
                                                   gets-asm)
                                                  gets-asm)
                                         0 (- (* 2 gap-size)) (- (* 2 gap-size)) 0))))
       (code-fade (ht-append (* 2 gap-size) f-c f-asm)))
      sz)
     ((if stack? values ghost)
      (make-stack (case step
                    [(0 7) 7]
                    [(1 6) 6]
                    [(2 3 5) 3]
                    [(4 4.5) 2])
                  #:concrete? #f #:desc? #f
                  #:shadow (case step
                             [(5) '("0x400575")]
                             [(7) '("0x40045c")]
                             [(6) (if (memq note '(stack-attack stack-attack+w^x))
                                      (let ([a (tt "mov $0x1,%eax")]
                                            [b (tt "syscall")])
                                        (list (scale (lt-superimpose b (ghost a)) 0.8)
                                              (scale a 0.8)))
                                      0)]
                             [else 0])
                  #:recents (case step
                              [(1 6) (list (or val2 "0x40045c"))]
                              [(2 3 5) (list "" (if local? (vector "name" 2 val0) "") val1 (or val2 "0x40045c"))]
                              [(4 4.5) (list "0x400575" "" (if local? (vector "name" 2 val0) "") val1 (or val2 "0x40045c"))]
                              [else null])))))
  (define p2
    (if in-asm
        (pin-arrow-line (/ gap-size 2)
                        p
                        %rip rc-find
                        #:start-angle 0
                        in-asm (shifted lt-find 0 (* sz (+ (current-line-sep) (pict-height (tt " "))) (+ 0.5 asm-line)))
                        #:end-angle 0
                        #:color "purple"
                        #:line-width 3
                        #:alpha (code-fade 1.0))
        p))
  (cond
    [(eq? note 'unlucky)
     (pin-balloon p2
                  lc-find val1
                  #:spike 'e
                  #:color "gold"
                  (vl-append
                   (current-line-sep)
                   (para* "Unlucky case: overflow")
                   (para* "without a crash")))]
    [(eq? note 'return)
     (pin-balloon p2
                  lc-find val2
                  #:spike 'e
                  #:dx 64
                  #:color "gold"
                  (vl-append
                   (current-line-sep)
                   (para* "Large enough overflow can")
                   (para* "change return address!")))]
    [(eq? note 'attack)
     (pin-balloon p2
                  lc-find val2
                  #:spike 'ne
                  #:dx 64
                  #:dy -64
                  #:color "gold"
                  (vl-append
                   (current-line-sep)
                   (para* "Hopefully crashes... but")
                   (para* "could jump to unexpected code")))]
    [(memq note '(stack-attack stack-attack+w^x))
     (pin-balloon p2
                  lc-find val2
                  #:spike 'ne
                  #:dx 64
                  #:dy -64
                  #:color "gold"
                  (let ([p (vl-append
                            (current-line-sep)
                            (para* "Could even jump to buffer content")
                            (para* "crafted to hold new instructions"))])
                    (if (eq? note 'stack-attack+w^x)
                        (pin-balloon p
                                     cb-find p
                                     #:spike 'n
                                     #:color "lightgreen"
                                     (para* "Blocked by page protection"))                        
                        p)))]
    [else p2]))

(define (call-example* #:addr? [addr? #t]
                       #:note [note #f]
                       #:code-fade [code-fade values]
                       . args)
  (apply call-example args
         #:local? #t
         #:note note
         #:val0 (tt "\"12345678\"")
         #:val1 (tt "\"90123456\"")
         #:val2 (tt (if addr?
                        (if (memq note '(stack-attack stack-attack+w^x))
                            "0x7FFFF810"
                            "0x30393837")
                        "\"7890\\0\""))
         #:code-fade code-fade))

(define (call-example-picts)
  (list
   (call-example 0 #f #f #:stack? #f #:gets? #f)
   (call-example 0 #f #f #:stack? #f)
   (call-example 0 main-asm 1)
   (call-example 1 f-asm 0)
   (call-example 2 f-asm 1)
   (call-example 3 f-asm 2)
   (call-example 4 gets-asm 0)
   (call-example 4.5 gets-asm 1)
   (call-example 5 f-asm 3)
   (call-example 6 f-asm 4)
   (call-example 7 main-asm 2)))
   
(define (overflow-example-picts)
  (list
   (call-example 1 f-asm 0)
   (call-example 2 f-asm 1 #:local? #t)
   (call-example 4 gets-asm 0 #:local? #t)
   (call-example 4 gets-asm 0 #:local? #t #:val0 (tt "\"MSD\\0\""))
   (call-example 4 gets-asm 0 #:local? #t #:val0 (tt "\"Rumpelst\"") #:val1 (tt "\"iltskin\\0\""))
   (call-example 4 gets-asm 0 #:local? #t #:val0 (tt "\"Rumpelst\"") #:val1 (tt "\"iltskin\\0\"") #:note 'unlucky)
   (call-example* 4 gets-asm 0 #:addr? #f)
   (call-example* 4 gets-asm 0 #:addr? #f #:note 'return)
   (call-example* 4 gets-asm 0)
   (call-example* 5 f-asm 3)
   (call-example* 6 f-asm 4)
   (call-example* 6 f-asm 4 #:note 'attack)
   (call-example* 6 f-asm 4 #:note 'stack-attack)))

(define (guarded-stack)
  (make-stack 3
              #:concrete? #f #:desc? #f
              #:shadow 0
              #:recents (list "0x400575" "" (vector "name" 2 "") "" (it "canary") "0x40045c")))

(define (code-on-stack-picts)
  (define (code-fade p)
    (if (pict? p)
        (cellophane p 0.25)
        0.25))
  (list
   (call-example* 6 f-asm 4 #:note 'stack-attack #:code-fade code-fade)
   (call-example* 6 f-asm 4 #:note 'stack-attack+w^x #:code-fade code-fade)))

(define (example-rop-stack #:ret1 [ret1 (tt "0x0100003eb0")]
                           #:ret2 [ret2 (tt "0x0100003ecb")])
  (define val0 "aaaaaaaa")
  (define val1 "aa")
  (make-stack 2
              #:old-rsp 6
              #:concrete? #f
              #:desc? #f
              #:recents (list "" (vector "name" 2 val0) val1
                              ret1
                              ret2)))

(define (example-rop2-stack #:rets [rets (list (tt "??????"))]
                            #:ret2 [ret2 (tt "0x0100003ecb")])
  (define val0 "/bin/bas")
  (define val1 "h\\0")
  (make-stack 2
              #:stack-base-size (+ 10 (length rets))
              #:old-rsp (+ 5 (length rets))
              #:concrete? #f
              #:desc? #f
              #:recents (append (list "" (vector "name" 2 val0) val1)
                                rets
                                (list ret2))))

(define (decrypt-stack #:secret [secret? #t]
                       #:returned [returned? #f]
                       #:buffer [buffer? #f])
  (define val0 (if secret? "super" ""))
  (define val1 (if secret? "Secret" ""))
  (make-stack (if returned? 5 2)
              #:shadow (if returned? (list val1 val0) 0)
              #:stack-base-size 10
              #:old-rsp (and (not returned?) 5)
              #:concrete? #f
              #:desc? #f
              #:recents (if returned?
                            null
                            (list "" (vector (if buffer? "buffer" "private_key") 2 val0) val1
                                  "...."))))

(module+ main
  (slide (decrypt-stack))
  (slide (decrypt-stack #:returned #t))
  #;
  (for-each slide (code-on-stack-picts))
  #;
  (slide (guarded-stack))
  #;
  (for-each slide (append (call-example-picts)
                          (overflow-example-picts))))
