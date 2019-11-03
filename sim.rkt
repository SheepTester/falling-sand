#lang racket

(require "./particle.rkt")

; Starts a simulation with the given width and height,
; creating a Scratch-style one-dimensional list to store
; the particles. The default and border particle should be a particle,
; not a particle ID. The list is initialized with the default particle
; and references to outside the map returns the border particle.
(define (start-sim width height default-part border-part)
  (define (create-map-iter cells-left map)
    (if (<= cells-left 0) map
        (create-map-iter (- cells-left 1) (cons default-part map))))

  ; Returns a callback lambda that can be called to either
  ; set a particle or simulate a particle:
  ; (step 'set 1 2 (assoc 'water particle-types))
  ; (step 'sim 1 2)
  (define (next-step curr-map)
    (define (step instruct x y [to-part #f])
      ; Recreates the map but checks each position with the given
      ; list to see if a particle should be replaced.
      ; (Is there an iterative way to do this?)
      ; `replace` is a list of pairs of indices and replacement
      ; particles.
      (define (recreate-map replace old-map i)
        (cond ((null? old-map) '())
              (else
               (define replacement (assoc i replace))
               (cons (if replacement
                         (cdr replacement)
                         (car old-map))
                     (recreate-map replace (cdr old-map) (+ i 1))))))
      (cond ((or (< x 0) (< y 0) (>= x width) (>= y height))
             (error "Hmm! Quite suspish! " x " " y))
            ((equal? instruct 'set)
             (next-step (recreate-map
                         (list (cons (+ x (* y width)) to-part))
                         curr-map
                         0)))
            ((equal? instruct 'sim)
             (next-step
              (recreate-map
               (map (lambda (replacement)
                      (define px (+ x (caar replacement)))
                      (define py (+ y (cdar replacement)))
                      (if (or (< px 0) (< py 0)
                              (>= px width) (>= py height))
                          (cons -1 #f)
                          (cons (+ px (* py width))
                                (assoc (cadr replacement)
                                       particle-types))))
                    ((particle-fn (list-ref
                                   curr-map
                                   (+ x (* y width))))
                     (lambda (offset-x offset-y)
                       (define px (+ x offset-x))
                       (define py (+ y offset-y))
                       (if (or (< px 0) (< py 0)
                               (>= px width) (>= py height))
                           (particle-id border-part)
                           (particle-id (list-ref
                                         curr-map
                                         (+ px (* py width))))))))
               curr-map
               0)))
            (else
             (error "What kind of instruction is " instruct "???"))))
    (list step width height curr-map))

  (next-step (create-map-iter (* width height) '())))

(provide start-sim)
