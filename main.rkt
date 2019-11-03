#lang racket/gui

(require "./particle.rkt")
(require "./sim-mutable-pairs.rkt")

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

(define particle-size 5)
(define canvas
  (new
   (class canvas%
     ; When a mouse event occurs:
     (define/override (on-event event)
       (when (or (send event button-down? 'left)
                 (send event dragging?))
         (define x (quotient (send event get-x) particle-size))
         (define y (quotient (send event get-y) particle-size))
         (set! sim ((car sim) 'set x y current-particle))
         (define dc (send canvas get-dc))
         (send dc set-brush (particle-colour current-particle) 'solid)
         (send dc draw-rectangle
               (* x particle-size)
               (* y particle-size)
               particle-size
               particle-size)))
     (super-new))
   [parent frame]
   [paint-callback
    (lambda (canvas dc)
      (define width (cadr sim))
      (define height (caddr sim))
      (send dc set-brush
            (particle-colour (assoc 'empty particle-types)) 'solid)
      (send dc draw-rectangle
            0 0 (* width particle-size) (* height particle-size))
      (define (iter i particles)
        (cond ((not (null? particles))
               (define part
                 (if (pair? particles)
                     (car particles)
                     (mcar particles)))
               (when (not (equal? (particle-id part) 'empty))
                 (send dc set-brush (particle-colour part) 'solid)
                 (send dc draw-rectangle
                       (* (remainder i width) particle-size)
                       (* (quotient i width) particle-size)
                       particle-size
                       particle-size))
               (iter (+ i 1)
                     (if (pair? particles)
                         (cdr particles)
                         (mcdr particles))))))
      (iter 0 (cadddr sim)))]))

(define sim #f)
(define (reset width height)
  (set! sim (start-sim width height
                       (assoc 'empty particle-types)
                       (assoc 'metal particle-types)))
  (send canvas min-client-width (* width particle-size))
  (send canvas min-client-height (* height particle-size))
  (define dc (send canvas get-dc))
  (send dc clear)
  (send dc set-pen "white" 1 'transparent)
  (send canvas on-paint))
(reset 40 60)

(new timer%
     [notify-callback
      (lambda ()
        (define width (cadr sim))
        (define height (caddr sim))
        (for ([i (in-range 1000)])
          (set! sim ((car sim)
                     'sim
                     (exact-floor (* (random) width))
                     (exact-floor (* (random) height)))))
        (send canvas on-paint))]
     [interval 40]
     [just-once? #f])

(send frame show #t)
