#lang racket
(require racket/gui)

(provide make
         add)

(define (make)
  (define t (new text%))
  (send t change-style (make-object style-delta% 'change-size 20))
  t)

(define (add f note)
  (define c (new editor-canvas% [parent f]))
  (send c set-editor note)  
  void)


