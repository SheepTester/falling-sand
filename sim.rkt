#lang racket

(require "./particle.rkt")

; Starts a simulation with the given width and height,
; creating a Scratch-style one-dimensional list to store
; the particles. The default and border particle should be a particle,
; not a particle ID. The list is initialized with the default particle
; and references to outside the map returns the border particle.
(define (start-sim width height default-part border-part)
  ; The length of the map list.
  (define length (* width height))
  
  (define (create-map-iter cells-left map)
    (if (<= cells-left 0) map
        (create-map-iter (- cells-left) (cons default-part map))))

  ; Returns a callback lambda that can be called to either
  ; set a particle or simulate a particle:
  ; (step 'set 1 2 (assoc 'water particle-types))
  ; (step 'sim 1 2)
  (define (next-step curr-map)
    (lambda (instruct x y [to-part #f])
      ; Recreates the map but checks each position with the given
      ; list to see if a particle should be replaced.
      ; (Is there an iterative way to do this?)
      (define (recreate-map replace old-map i)
        (cond ((null? old-map) '())
              (else
               (define replacement (assoc i replace))
               (cons (if replacement
                         replacement
                         (car old-map))
                     (recreate-map replace (cdr old-map) (+ i 1))))))
      (cond ((equal? instruct 'set)
             (next-step (recreate-map
                         (list (list (+ x (* y height)) to-part))
                         curr-map
                         0)))
            ((equal? instruct 'sim)
             (next-step
              (recreate-map
               (map (lambda (replacement)
                      (cons (+ x (caar replacement)
                               (* (+ y (cdar replacement))
                                  height))
                            (assoc (cadr replacement)
                                   particle-types)))
                    ((particle-fn (list-ref
                                   curr-map
                                   (+ x (* y height))))
                     (lambda (offset-x offset-y)
                       (define px (+ x offset-x))
                       (define py (+ y offset-y))
                       (if (or (< px 0) (< py 0)
                               (>= px width) (>= py height))
                           (particle-id border-part)
                           (particle-id (list-ref
                                         curr-map
                                         (+ px (* py height))))))))
               curr-map
               0)))
            (else
             (error "What kind of instruction is " instruct "???")))))

  (next-step (create-map-iter length '())))

(provide start-sim)