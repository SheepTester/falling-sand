#lang racket/gui

(require "./particle.rkt")

(define frame (new frame% [label "Falling sand"]))

(define btn-panel (new horizontal-panel% [parent frame]))
(define current-particle #f)
(define current-particle-btn #f)
; Both create buttons for each particle and also select the first
; particle by default.
((car (map (lambda (part)
             (define (on-click btn event)
               (when current-particle-btn
                 (send current-particle-btn enable #t))
               (set! current-particle part)
               (set! current-particle-btn btn)
               (send btn enable #f))
             (define button
               (new button%
                  [parent btn-panel]
                  [label (particle-name part)]
                  [callback on-click]))
             (lambda () (on-click button #f)))
           particle-types)))

(define canvas
  (new
   (class canvas%
     (define/override (on-event event)
       (display event))
     (super-new))
   [parent frame]))

(send frame show #t)