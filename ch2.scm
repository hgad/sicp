(module debug (newlines title println id put get put-coercion get-coercion
               reset-proc-table reset-coercion-table install-proc install-proc2)
  (import scheme)
  (import (only chicken sub1))

  (define (newlines n)
    (if (not (zero? n)) (let () (newline) (newlines (sub1 n)))))

  (define (title str)
    (display (string-append str ":")) (newline))

  (define (println obj)
    (display obj) (newline))

  (define (id x) x)

  (define-syntax define-syntax-rule
    (syntax-rules ()
      ((define-syntax-rule (name . pattern) template)
      (define-syntax name
        (syntax-rules ()
          ((name . pattern) template))))))

  (define-syntax-rule (put-table table key1 key2 proc)
    (let ((pair (assoc (cons key1 key2) table)))
      (if pair
        (set-cdr! pair proc)
        (set! table (cons (cons (cons key1 key2) proc) table)))))

  (define-syntax-rule (get-table table key1 key2)
    (let ((pair (assoc (cons key1 key2) table)))
      (if pair
        (cdr pair)
        #f)))

  (define-syntax-rule (install-proc proc)
    (define (proc arg)
      ((get 'proc (type-tag arg)) (contents arg))))

  (define-syntax-rule (install-proc2 proc)
    (define (proc arg1 arg2)
      ((get 'proc (list (type-tag arg1) (type-tag arg2))) (contents arg1)
                                                          (contents arg2))))

  (define proc-table '())

  (define coercion-table '())

  (define (put op type proc)
    (put-table proc-table op type proc))

  (define (get op type)
    (get-table proc-table op type))

  (define (put-coercion type1 type2 proc)
    (put-table coercion-table type1 type2 proc))

  (define (get-coercion type1 type2)
    (get-table coercion-table type1 type2))

  (define (reset-proc-table) (set! proc-table '()))

  (define (reset-coercion-table) (set! coercion-table '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.1                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.1 (make-rat numer denom)
  (import scheme debug)
  (import (only chicken gcd))
  (title "ex2.1")

  (define (make-rat n d)
    (let* ((g (gcd n d)) (num (/ n g)) (den (/ d g)))
      (if (negative? den)
        (cons (- num) (- den))
        (cons num den))))

  (define (numer rat) (car rat))

  (define (denom rat) (cdr rat))

  (println (make-rat 6 12))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.2                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.2 (make-point x-coord y-coord)
  (import scheme debug)
  (title "ex2.2")

  (define (make-point x y) (cons x y))
  (define (x-coord p) (car p))
  (define (y-coord p) (cdr p))

  (define (make-seg p1 p2) (cons p1 p2))
  (define (seg-start s) (car s))
  (define (seg-end s) (cdr s))

  (define (average x y) (/ (+ x y) 2))
  (define (midpoint-segment s)
    (let ((start (seg-start s)) (end (seg-end s)))
      (make-point
        (average (x-coord start) (x-coord end))
        (average (y-coord start) (y-coord end)))))

  (define (print-point p)
    (display "(")
    (display (x-coord p))
    (display ", ")
    (display (y-coord p))
    (display ")")
    (newline))

  (print-point
    (midpoint-segment
      (make-seg (make-point 0.0 0.0)
                (make-point 3.0 3.0)))) ; (1.5,1.5)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.3                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.3 ()
  (import scheme debug)
  (import (only ex2.2 make-point x-coord y-coord))
  (title "ex2.3")

  ; ll = lower-left point & ur = upper-right point
  (define (make-rect ll ur) (cons ll ur))
  (define (rect-ll r) (car r))
  (define (rect-ur r) (cdr r))

  (define (rect-perimeter r)
    (let ((ll (rect-ll r)) (ur (rect-ur r)))
    (* 2 (+ (- (y-coord ur) (y-coord ll))
            (- (x-coord ur) (x-coord ll))))))

  (define (rect-area r)
    (let ((ll (rect-ll r)) (ur (rect-ur r)))
    (* (- (y-coord ur) (y-coord ll))
      (- (x-coord ur) (x-coord ll)))))

  (let ((rect (make-rect (make-point 0.0 0.0) (make-point 3.0 3.0))))
    (println (rect-perimeter rect))
    (println (rect-area rect))
    (newline))

  ; different representation for rect. same perimeter and area procedures will
  ; work with both representations.
  (define (make-rect ll ur) (cons ur ll))
  (define (rect-ll r) (cdr r))
  (define (rect-ur r) (car r))

  (let ((rect (make-rect (make-point 0.0 0.0) (make-point 3.0 3.0))))
    (println (rect-perimeter rect))
    (println (rect-area rect))
    (newline)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.4                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.4 ()
  (import scheme debug)
  (title "ex2.4")

  (define (cons-lambda x y)
    (lambda (m) (m x y)))

  (define (car-lambda z)
    (z (lambda (p q) p)))

  ; (car (cons x y))
  ; (car (lambda (m) (m x y)))
  ; ((lambda (m) (m x y)) (lambda (p q) p))
  ; ((lambda (p q) p) x y)
  ; x

  (define (cdr-lambda z)
    (z (lambda (p q) q)))

  ; (cdr (cons x y))
  ; (cdr (lambda (m) (m x y)))
  ; ((lambda (m) (m x y)) (lambda (p q) q))
  ; ((lambda (p q) q) x y)
  ; y

  (println (car-lambda (cons-lambda 1 2))) ; 1
  (println (cdr-lambda (cons-lambda 1 2))) ; 2

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.5                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.5 ()
  (import scheme debug)
  (import (only chicken add1))
  (title "ex2.5")

  ; Here's a proof by construction:
  (define (cons-expt x y) (* (expt 2 x) (expt 3 y)))

  (define (car-expt z)
    (define (car-iter a result)
      (if (odd? a)
        result
        (car-iter (/ a 2) (add1 result))))
    (car-iter z 0))

  (define (cdr-expt z)
    (define (cdr-iter a result)
      (if (not (= (remainder a 3) 0))
        result
        (cdr-iter (/ a 3) (add1 result))))
    (cdr-iter z 0))

  ; Now we verify the pair laws:

  (println (car-expt (cons-expt 4 5)))  ; 4
  (println (cdr-expt (cons-expt 4 5))) ; 5
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.6                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.6 ()
  (import scheme debug)
  (import (only chicken add1))
  (title "ex2.6")

  (define church-zero (lambda (f) (lambda (x) x)))

  (define (church-add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x)))))

  ; (church-add-1 church-zero)
  ; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
  ; (lambda (f) (lambda (x) (f ((lambda (x) x) x))))
  ; (lambda (f) (lambda (x) (f x)))

  (define church-one (lambda (f) (lambda (x) (f x))))
  (define church-two (lambda (f) (lambda (x) (f (f x)))))

  (define (church-add m n)
    (lambda (f)
      (lambda (x)
        ((m f) ((n f) x)))))

  (define (church->scheme church-numeral)
    ((church-numeral add1) 0))

  (println (church->scheme church-zero))                         ; 0
  (println (church->scheme church-one))                          ; 1
  (println (church->scheme church-two))                          ; 2
  (println (church->scheme (church-add  church-one church-two))) ; 3
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.7                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.7 (make-interval interval-lb interval-ub)
  (import scheme debug)
  (title "ex2.7")

  (define (make-interval a b) (cons a b))
  (define (interval-lb i) (car i))
  (define (interval-ub i) (cdr i))

  (let ((interval (make-interval 2.8 3.2)))
    (println (interval-lb interval))
    (println (interval-ub interval)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.8                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The lower bound of the difference interval will be the difference between the
; lower bound of the first interval and the upper bound of the second interval,
; while the upper bound of the difference will be the difference between the
; upper bound of the first interval and the lower bound of the second interval.

(module ex2.8 ()
  (import scheme debug ex2.7)
  (title "ex2.8")

  (define (sub-interval x y)
    (make-interval (- (interval-lb x) (interval-ub y))
                   (- (interval-ub x) (interval-lb y))))

  (let ((i1 (make-interval 2.8 3.2)) (i2 (make-interval 5.9 6.1)))
    (println (sub-interval i2 i1))
    (println (sub-interval i1 i2)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.9                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Given intervals i1 = (a1, b1) of width w1 = (b1 - a1) / 2,
;             and i2 = (a2, b2) of width w2 = (b2 - a2) / 2.
; The sum of i1 & i2 is i3 = (a1 + a2, b1 + b2)
;   with width w3 = (b1 + b2 - a1 - a2) / 2
;                 = (b1 - a1) / 2 + (b2 - a2) / 2
;                 = w1 + w2.
; The difference is i4 = i1 - i2 = (a1 - b2, b1 - a2)
;        with width w4 = (b1 - a2 - a1 + b2) / 2
;                      = (b1 - a1) / 2 + (b2 - a2) / 2
;                      = w1 + w2 as well.
;
; The width of the product is not a necessarily a function of the width of the
; two intervals. Here's a counter example:
;
; Let i1 = (-10, -5) with width = 0.25 and i2 = (0.5, 1) of width = 2.5,
; then i5 = i1 * i2 = (-10, -2.5) with width = 3.75,
;  and i6 = i1 / i2 = (-20, -5)   with width = 7.5
;
; It's clear that neither of the two results can be put in the form of a
; function of w1 and w2.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.10                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.10 (mul-interval div-interval)
  (import scheme debug ex2.7)
  (import (only chicken error))
  (title "ex2.10")

  (define (mul-interval x y)
    (let ((p1 (* (interval-lb x) (interval-lb y)))
          (p2 (* (interval-lb x) (interval-ub y)))
          (p3 (* (interval-ub x) (interval-lb y)))
          (p4 (* (interval-ub x) (interval-ub y))))
      (make-interval (min p1 p2 p3 p4)
                    (max p1 p2 p3 p4))))

  (define (div-interval x y)
    (let ((yrlb (/ 1.0 (interval-ub y)))  ; y reciprocal lower bound
          (yrup (/ 1.0 (interval-lb y)))) ; y reciprocal upper bound
      (if (and (<= yrlb 0) (>= yrup 0))
        (error "Second argument's reciprocal interval includes zero.")
        (mul-interval x (make-interval yrlb yrup)))))

  (let ((i1 (make-interval 2.8 3.2)) (i2 (make-interval 5.9 6.1)))
    (println (mul-interval i2 i1))
    (println (div-interval i2 i1)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.11                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.11 ()
  (import scheme debug ex2.7)
  (title "ex2.11")

  (define (mul-interval x y)
    (let ((xlb (interval-lb x)) (ylb (interval-lb y))
          (xub (interval-ub x)) (yub (interval-ub y)))
      (cond
        ((and (>= xlb 0) (>= ylb 0))
        (make-interval (* xlb ylb) (* xub yub)))
        ((and (<= xub 0) (<= yub 0))
        (make-interval (* xub yub) (* xlb ylb)))
        ((and (<= xlb 0) (>= xub 0) (>= ylb 0))
        (make-interval (* xlb yub) (* xub yub)))
        ((and (<= ylb 0) (>= yub 0) (>= xlb 0))
        (make-interval (* xub ylb) (* xub yub)))
        ((and (<= xub 0) (>= ylb 0))
        (make-interval (* xlb yub) (* xub yub)))
        ((and (>= xlb 0) (<= yub 0))
        (make-interval (* xub ylb) (* xub yub)))
        ((and (<= xlb 0) (>= xub 0) (<= yub 0))
        (make-interval (* xub (min xlb ylb)) (* xlb ylb)))
        ((and (<= xub 0) (<= ylb 0) (>= yub 0))
        (make-interval (* yub (min xlb ylb)) (* xlb ylb)))
        ((and (<= xlb 0) (>= xub 0) (<= ylb 0) (>= yub 0))
        (make-interval (min (* xlb yub) (* xub ylb))
                       (max (* xlb ylb) (* xub yub)))))))

  (let ((i1 (make-interval 2.8 3.2))
        (i2 (make-interval 5.9 6.1))
        (i3 (make-interval -5.2 -4.8))
        (i4 (make-interval -7.1 -6.9))
        (i5 (make-interval -0.1 0.1)))
    (println (mul-interval i1 i2))
    (println (mul-interval i2 i1))
    (println (mul-interval i1 i3))
    (println (mul-interval i3 i1))
    (println (mul-interval i1 i4))
    (println (mul-interval i4 i1))
    (println (mul-interval i1 i5))
    (println (mul-interval i5 i1))
    (println (mul-interval i2 i3))
    (println (mul-interval i3 i2))
    (println (mul-interval i2 i4))
    (println (mul-interval i4 i2))
    (println (mul-interval i2 i5))
    (println (mul-interval i5 i2))
    (println (mul-interval i3 i4))
    (println (mul-interval i4 i3))
    (println (mul-interval i3 i5))
    (println (mul-interval i5 i3))
    (println (mul-interval i4 i5))
    (println (mul-interval i5 i4)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.12                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.12 (make-center-percent interval-center interval-width)
  (import scheme debug ex2.7)
  (title "ex2.12")

  (define (make-center-percent c p)
    (let ((w (* c p)))
      (make-interval (- c w) (+ c w))))

  (define (interval-center i)
    (/ (+ (interval-lb i) (interval-ub i)) 2))

  (define (interval-width i)
    (/ (- (interval-ub i) (interval-lb i)) 2))

  (define (interval-percent i)
    (let ((w (interval-width i)) (c (interval-center i)))
      (/ w c)))

  (let ((interval (make-center-percent 5.0 0.2)))
    (println interval)
    (println (interval-center interval))
    (println (interval-width interval))
    (println (interval-percent interval)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.13 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Trying an example like 2 +/- 5% = (1.9, 2.1)
;                        5 +/- 1% = (4.95, 5.05)
;                        Product  = (9.405, 10.605)
;                        Tolerance = 5.999% almost = 5% + 1%
;
; So in this case, the product tolerance percentage would be approximately equal
; to the sum of the tolerance percentage of the two input intervals.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.14                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.14 ()
  (import scheme debug ex2.7 ex2.10 ex2.12)
  (title "ex2.14")

  (define (add-interval x y)
    (make-interval (+ (interval-lb x) (interval-lb y))
                   (+ (interval-ub x) (interval-ub y))))

  (define (par1 r1 r2)
    (div-interval (mul-interval r1 r2)
                  (add-interval r1 r2)))

  (define (par2 r1 r2)
    (let ((one (make-interval 1 1)))
      (div-interval one
                    (add-interval (div-interval one r1)
                                  (div-interval one r2)))))

  (define a-interval (make-center-percent 10 0.01))
  (define b-interval (make-center-percent 5 0.01))

  (println (par1 a-interval b-interval)) ; 3.334666800013335 +/- 2.9992002399280064%
  (println (par2 a-interval b-interval)) ; 3.333333333333333 +/- 0.9999999999999966%

  (println (div-interval a-interval a-interval))  ; 1.0002000200020003 +/- 1.999800019997991%
  (println (div-interval a-interval b-interval)) ; 2.0004000400040005 +/- 1.999800019997991%

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.15                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; She's right. Because every instance of the uncertain number will contribute to
; the tolerance percentage of the result. So formulas where every uncertain
; number is not repeated will result in less output tolerance percentage than
; those where uncertain numbers are repreated.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.16                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Because every interval arithmetic operation contributes differently to the
; result's tolerance percentage. So different formulas, despite being
; algebraically equivalent, will have different output tolerance percentage and
; thus will result in different intervals. It might be doable to devise an
; interval arithmetic package that doesn't behave like this if we precede
; interval caluclation of formulas with symbolic algebraic simplification to put
; every algebraic formula in a certain normal form before performing interval
; arithmetic.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.17                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.17 ()
  (import scheme debug)
  (title "ex2.17")

  (define (last-pair l)
    (define (last-pair-iter iter-l result)
      (if (null? iter-l)
        result
        (last-pair-iter (cdr iter-l) iter-l)))
    (last-pair-iter l l))

  (println (last-pair '(1 2 3 4)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.18                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.18 ()
  (import scheme debug)
  (title "ex2.18")

  (define (list-reverse l)
    (define (list-reverse-iter iter-l result)
      (if (null? iter-l)
        result
        (list-reverse-iter (cdr iter-l) (cons (car iter-l) result))))
    (list-reverse-iter l '()))

  (println (list-reverse (list 1 4 9 16 25)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.19                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.19 ()
  (import scheme debug)
  (title "ex2.19")

  (define us-coins (list 50 25 10 5 1))
  (define uk-coins (list 100 50 20 10 5 2 1 0.5))

  (define (cc amount coin-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coin-values)) 0)
          (else
          (+ (cc amount
                  (except-first-denomination coin-values))
              (cc (- amount
                    (first-denomination coin-values))
                  coin-values)))))

  (define (first-denomination coin-values) (car coin-values))

  (define (except-first-denomination coin-values) (cdr coin-values))

  (define (no-more? coin-values) (null? coin-values))

  ; Changing the order of coin-values does not affect the value of cc because the
  ; algorithm does not make any assumptions as to the order of coin values.
  (println (cc 11 us-coins))
  (println (cc 11 uk-coins))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.20                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.20 ()
  (import scheme debug)
  (title "ex2.20")

  (define (same-parity x . y)
    (define (parity-correct? n)
      (if (even? x) (even? n) (odd? n)))
    (define (same-parity-iter w result)
      (if (null? w)
        result
        (same-parity-iter (cdr w)
          (if (parity-correct? (car w))
            (append result (list (car w)))
            result))))
    (same-parity-iter (cons x y) '()))

  (println (same-parity 2 4 5 6 7 9 11 12))
  (println (same-parity 3 4 5 6 7 9 11 12))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.21                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.21 (square)
  (import scheme debug)
  (title "ex2.21")

  (define (square x) (* x x))

  (define (square-list items)
    (if (null? items)
        '()
        (cons (square (car items)) (square-list (cdr items)))))

  (println (square-list '(1 2 3 4)))

  (define (square-list items)
    (map square items))

  (println (square-list '(1 2 3 4)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.22                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Because he's pushing the results into the output list in the reverse order.
; Reversing the arguments to cons wouldn't work because it will break the
; conventional list structure, so Scheme will recognize the result as pairs of
; pairs or pairs etc. and not as a list.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.23                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.23 ()
  (import scheme debug)
  (title "ex2.23")

  (define (foreach proc l)
    (cond
      ((null? l) #t)
      (else (proc (car l))
            (foreach proc (cdr l)))))

  (foreach println '(1 2 3 4))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.24                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Interpreter result: (1 (2 (3 4)))
;
; Box-and-pointer representation:
;
;   -------     -------
;  | * | *-|-->| * | / |
;   -|-----     -|-----
;    v           v
;   ---         -------     -------
;  | 1 |       | * | *-|-->| * | / |
;   ---         -|-----     -|-----
;                v           v
;               ---         -------     -------
;              | 2 |       | * | *-|-->| * | / |
;               ---         -|-----     -|-----
;                            v           v
;                           ---         ---
;                          | 3 |       | 4 |
;                           ---         ---
;
; Tree representation:
;
;    (1 (2 (3 4)))
;         /\
;        /  \
;       /    \
;      1    (2 (3 4))
;              /\
;             /  \
;            /    \
;           2    (3 4)
;                 /\
;                /  \
;               /    \
;              3      4


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.25                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.25 ()
  (import scheme debug)
  (title "ex2.25")

  (println (cadr (caddr '(1 3 (5 7) 9))))

  (println (caar '((7))))

  (println (cadadr (cadadr (cadadr '(1 (2 (3 (4 (5 (6 7))))))))))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ex2.26                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.26 ()
  (import scheme debug)
  (title "ex2.26")

  (define x '(1 2 3))
  (define y '(4 5 6))

  (println (append x y)) ; (1 2 3 4 5 6)

  (println (cons x y))   ; ((1 2 3) 4 5 6)

  (println (list x y))   ; ((1 2 3) (4 5 6))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.27                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.27 ()
  (import scheme debug)
  (title "ex2.27")

  (define (deep-list-reverse l)
    (define (list-reverse-iter iter-l result)
      (cond
        ((null? iter-l) result)
        ((not (pair? iter-l)) iter-l)
        (else (list-reverse-iter (cdr iter-l)
                                (cons (list-reverse-iter (car iter-l) '())
                                      result)))))
    (list-reverse-iter l '()))

  (println (deep-list-reverse (list (list 1 2) (list 3 4))))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.28                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.28 (fringe)
  (import scheme debug)
  (title "ex2.28")

  (define (fringe l)
    (cond
      ((null? l) '())
      ((not (pair? l)) (list l))
      (else (append (fringe (car l))
                    (fringe (cdr l))))))

  (println (fringe '(1 2 (3 4) 5 6)))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.29                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.29 (left-branch right-branch)
  (import scheme debug)
  (title "ex2.29")

  (define (make-mobile left right)
    (list left right))

  (define (make-branch len structure)
    (list len structure))

  ; Part (a):

  (define (left-branch mobile) (car mobile))
  (define (right-branch mobile) (cadr mobile))

  (define (branch-length branch) (car branch))
  (define (branch-structure branch) (cadr branch))

  (define m (make-mobile (make-branch 1 (make-mobile (make-branch 4 5)
                                                     (make-branch 5 4)))
                         (make-branch 3 3)))
  (println (left-branch m))
  (println (right-branch m))
  (println (branch-length (left-branch m)))
  (println (branch-structure (right-branch m)))

  ; Part (b):

  (define (structure-is-weight? structure)
    (not (pair? structure)))

  (define (branch-weight branch)
    (let ((structure (branch-structure branch)))
      (if (structure-is-weight? structure)
        structure
        (total-weight structure))))

  (define (total-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))))

  (println (total-weight m))

  ; Part (c):

  (define (branch-balanced? branch)
    (let ((structure (branch-structure branch)))
      (if (structure-is-weight? structure)
        #t
        (mobile-balanced? structure))))

  (define (mobile-balanced? mobile)
    (define (torque branch)
      (* (branch-length branch)
         (branch-weight branch)))

    (let ((left (left-branch mobile))
          (right (right-branch mobile)))
      (and (= (torque left)
              (torque right))
          (branch-balanced? left)
          (branch-balanced? right))))

  (println (mobile-balanced? m))

  ; Part (d):
  ;
  ; All what needs to change are the right-branch and branch-structure selectors:

  (define (make-mobile left right)
    (cons left right))

  (define (make-branch length structure)
    (cons length structure))

  (define (right-branch mobile) (cdr mobile))
  (define (branch-structure branch) (cdr branch))

  (define n (make-mobile (make-branch 1 (make-mobile (make-branch 2 3)
                                                     (make-branch 4 5)))
                         (make-branch 2 13)))

  (println (left-branch n))
  (println (right-branch n))
  (println (branch-length (left-branch n)))
  (println (branch-structure (left-branch n)))
  (println (total-weight n))
  (println (mobile-balanced? n))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.30                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module ex2.30 ()
  (import scheme debug)
  (import (only ex2.21 square))
  (title "ex2.30")

  ; Directly:
  (define (square-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree (car tree))
                      (square-tree (cdr tree))))))

  (println (square-tree '((1 2) (3 4))))

  ; Using map:
  (define (square-tree tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (square-tree sub-tree)
               (square sub-tree)))
         tree))

  (println (square-tree '((1 2) (3 4))))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.31                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.31 ()
  (import scheme debug)
  (import (only ex2.21 square))
  (title "ex2.31")

  (define (tree-map proc tree)
    (map (lambda (sub-tree)
           (if (pair? sub-tree)
               (tree-map proc sub-tree)
               (proc sub-tree)))
         tree))

  (println (tree-map square '((1 2) (3 4))))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.32                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.32 ()
  (import scheme debug)
  (title "ex2.32")

  (define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
          (append rest (map (lambda (x) (cons (car s) x)) rest)))))

  (println (subsets '(1 2 3 4)))

  (newline))

; It works because:
;  1) If the list is '() it returns '()
;  2) If the list has one element a -> rest = '() -> the return value is
;     '((), (a)).
;  3) If the list has two elemens (a, b) -> rest = '((), (a)) -> the return
;     value is computed by cons'ing b to every element of rest and appending the
;     result to rest -> '((), (a), (b), (b, a))
;
;  and so on and so forth, thus constructing the powerset of the input set.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.33                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.33 (accumulate)
  (import scheme debug)
  (import (only chicken add1))
  (import (only ex2.21 square))
  (title "ex2.33")

  (define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

  (define (map-acc p sequence)
    (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

  (define (append-acc seq1 seq2)
    (accumulate cons seq2 seq1))

  (define (length-acc sequence)
    (accumulate (lambda (x y) (add1 y)) 0 sequence))

  (println (map-acc square '(1 2 3 4 5 6)))
  (println (append-acc '(1 2 3) '(4 5 6)))
  (println (length-acc '(1 2 3 4 5 6)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.34                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.34 ()
  (import scheme debug)
  (import (only ex2.33 accumulate))
  (title "ex2.34")

  (define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms)
                  (+ this-coeff (* x higher-terms)))
                0
                coefficient-sequence))

  (println (horner-eval 4 '(1 2 3 4)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.35                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.35 ()
  (import scheme debug)
  (import (only ex2.28 fringe))
  (import (only ex2.33 accumulate))
  (title "ex2.35")

  (define (count-leaves t)
    (accumulate (lambda (x y) (+ (length x) y)) 0 (map fringe t)))

  (println (count-leaves '((1 2) (3 4) 5 6)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.36                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.36 (accumulate-n)
  (import scheme debug)
  (import (only ex2.33 accumulate))
  (title "ex2.36")

  (define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs)))))

  (println (accumulate-n + 0 (list (list 1 2 3) (list 4 5 6))))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.37                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.37 ()
  (import scheme debug)
  (import (only ex2.33 accumulate))
  (import (only ex2.36 accumulate-n))
  (title "ex2.37")

  (define (matrix-*-vector m v)
    (map (lambda (row) (accumulate + 0 (map * row v))) m))

  (define (transpose mat)
    (accumulate-n cons '() mat))

  (define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
      (map (lambda (row) (matrix-*-vector cols row)) m)))

  (println (matrix-*-vector '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '(4 3 2 1)))
  ; (20 49 70)

  (println (transpose '((1 2 3 4) (4 5 6 6) (6 7 8 9))))
  ; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))

  (println (matrix-*-matrix '((1 2 3 4) (4 5 6 6) (6 7 8 9)) '((8 7) (6 5) (4 3) (2 1))))
  ; ((40 30) (98 77) (140 110))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.38                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.38 ()
  (import scheme debug)
  (import (only chicken foldr foldl))
  (title "ex2.38")

  (println (foldr / 1 (list 1 2 3)))      ; 3/2
  (println (foldl / 1 (list 1 2 3)))       ; 1/6
  (println (foldr list '() (list 1 2 3))) ; (1 (2 (3 ())))
  (println (foldl list '() (list 1 2 3)))  ; (((() 1) 2) 3)

  ; op must be commutitive for fold-left and fold-riht to produce the same
  ; results for any sequence.
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.39                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.39 ()
  (import scheme debug)
  (import (only chicken foldr foldl))
  (title "ex2.39")

  (define (reverse-fold-right sequence)
    (foldr (lambda (x y) (append y (list x))) '() sequence))

  (define (reverse-fold-left sequence)
    (foldl (lambda (x y) (cons y x)) '() sequence))

  (println (reverse-fold-right '(1 2 3 4)))
  (println (reverse-fold-left '(1 2 3 4)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.40                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.40 (filter enumerate-interval flatmap unique-pairs)
  (import scheme debug)
  (import (only ex2.21 square))
  (import (only ex2.33 accumulate))
  (title "ex2.40")

  (define (filter pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst)) (cons (car lst) (filter pred (cdr lst))))
      (else (filter pred (cdr lst)))))

  (define (enumerate-interval low high)
    (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

  (define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

  (define (unique-pairs n)
    (flatmap
      (lambda (i)
        (map (lambda (j) (list i j))
            (enumerate-interval 1 (- i 1))))
      (enumerate-interval 1 n)))

  (define (divides? a b)
    (= (remainder b a) 0))

  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

  (define (smallest-divisor n)
    (find-divisor n 2))

  (define (prime? n)
    (= n (smallest-divisor n)))

  (define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair))))

  (define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

  (define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (unique-pairs n))))

  (println (prime-sum-pairs 10))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.41                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.41 ()
  (import scheme debug)
  (import (only chicken sub1))
  (import (only ex2.40 filter enumerate-interval flatmap unique-pairs))
  (title "ex2.41")

  (define (unique-triples n)
    (flatmap (lambda (pair)
              (map (lambda (value) (append pair (list value)))
                    (enumerate-interval 1 (sub1 (cadr pair))))) (unique-pairs n)))

  (define (s-sum-triples n s)
    (filter (lambda (triple) (= (+ (car triple) (cadr triple) (caddr triple)) s))
            (unique-triples n)))

  (println (s-sum-triples 10 12))
  ; ((5 4 3) (6 4 2) (6 5 1) (7 3 2) (7 4 1) (8 3 1) (9 2 1))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.42                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.42 ()
  (import scheme debug)
  (import (only ex2.33 accumulate))
  (import (only ex2.40 filter enumerate-interval flatmap unique-pairs))
  (title "ex2.42")

  (define (remove pred lst)
    (filter (lambda (x) (not (pred x))) lst))

  (define (queens board-size)
    (define (make-pos row col) (list row col))
    (define (row position) (car position))
    (define (col position) (cadr position))
    (define (rowEqual? pos1 pos2) (= (row pos1) (row pos2)))
    (define (colEqual? pos1 pos2) (= (col pos1) (col pos2)))
    (define (posEqual? pos1 pos2) (and (rowEqual? pos1 pos2)
                                      (colEqual? pos1 pos2)))
    (define empty-board '())


    (define (diagonals pos)
      (let* ((curr-row (row pos))
            (curr-col (col pos))
            (diffs (map (lambda (x) (- x curr-row))
                        (enumerate-interval 1 board-size))))
        (filter (lambda (x) (and (not (posEqual? x pos))
                                (<= (row x) board-size)
                                (<= (col x) board-size)))
                (flatmap
                  (lambda (diff)
                    (list (make-pos (+ curr-row diff) (+ curr-col diff))
                          (make-pos (+ curr-row diff) (- curr-col diff))))
                  diffs))))

    (define (safe? column positions)
      (let* ((curr-position
              (car (filter (lambda (pos) (colEqual? pos (make-pos 1 column)))
                            positions)))
            (prev-positions
              (remove (lambda (pos) (posEqual? pos curr-position)) positions))
            (prev-diagonals (flatmap diagonals prev-positions)))

        (define (positionsGood? pred? l)
          (accumulate (lambda (x y) (and x y)) #t
            (map (lambda (pos) (not (pred? pos curr-position))) l)))

        (and (positionsGood? rowEqual? prev-positions)
            (positionsGood? posEqual? prev-diagonals))))

    (define (adjoin-position new-row k rest-of-queens)
      (cons (make-pos new-row k) rest-of-queens))

    (define (queen-cols k)
      (if (= k 0)
          (list empty-board)
          (filter
            (lambda (positions) (safe? k positions))
            (flatmap
              (lambda (rest-of-queens)
                (map (lambda (new-row) (adjoin-position new-row k rest-of-queens))
                    (enumerate-interval 1 board-size)))
              (queen-cols (- k 1))))))
    (queen-cols board-size))

  ;(println (queens 8))
  ; (((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (5 2) (1 1))
  ;  ((5 8) (2 7) (4 6) (7 5) (3 4) (8 3) (6 2) (1 1))
  ;  ((3 8) (5 7) (2 6) (8 5) (6 4) (4 3) (7 2) (1 1))
  ;  ((3 8) (6 7) (4 6) (2 5) (8 4) (5 3) (7 2) (1 1))
  ;  ((5 8) (7 7) (1 6) (3 5) (8 4) (6 3) (4 2) (2 1))
  ;  ((4 8) (6 7) (8 6) (3 5) (1 4) (7 3) (5 2) (2 1))
  ;  ((3 8) (6 7) (8 6) (1 5) (4 4) (7 3) (5 2) (2 1))
  ;  ((5 8) (3 7) (8 6) (4 5) (7 4) (1 3) (6 2) (2 1))
  ;  ((5 8) (7 7) (4 6) (1 5) (3 4) (8 3) (6 2) (2 1))
  ;  ((4 8) (1 7) (5 6) (8 5) (6 4) (3 3) (7 2) (2 1))
  ;  ((3 8) (6 7) (4 6) (1 5) (8 4) (5 3) (7 2) (2 1))
  ;  ((4 8) (7 7) (5 6) (3 5) (1 4) (6 3) (8 2) (2 1))
  ;  ((6 8) (4 7) (2 6) (8 5) (5 4) (7 3) (1 2) (3 1))
  ;  ((6 8) (4 7) (7 6) (1 5) (8 4) (2 3) (5 2) (3 1))
  ;  ((1 8) (7 7) (4 6) (6 5) (8 4) (2 3) (5 2) (3 1))
  ;  ((6 8) (8 7) (2 6) (4 5) (1 4) (7 3) (5 2) (3 1))
  ;  ((6 8) (2 7) (7 6) (1 5) (4 4) (8 3) (5 2) (3 1))
  ;  ((4 8) (7 7) (1 6) (8 5) (5 4) (2 3) (6 2) (3 1))
  ;  ((5 8) (8 7) (4 6) (1 5) (7 4) (2 3) (6 2) (3 1))
  ;  ((4 8) (8 7) (1 6) (5 5) (7 4) (2 3) (6 2) (3 1))
  ;  ((2 8) (7 7) (5 6) (8 5) (1 4) (4 3) (6 2) (3 1))
  ;  ((1 8) (7 7) (5 6) (8 5) (2 4) (4 3) (6 2) (3 1))
  ;  ((2 8) (5 7) (7 6) (4 5) (1 4) (8 3) (6 2) (3 1))
  ;  ((4 8) (2 7) (7 6) (5 5) (1 4) (8 3) (6 2) (3 1))
  ;  ((5 8) (7 7) (1 6) (4 5) (2 4) (8 3) (6 2) (3 1))
  ;  ((6 8) (4 7) (1 6) (5 5) (8 4) (2 3) (7 2) (3 1))
  ;  ((5 8) (1 7) (4 6) (6 5) (8 4) (2 3) (7 2) (3 1))
  ;  ((5 8) (2 7) (6 6) (1 5) (7 4) (4 3) (8 2) (3 1))
  ;  ((6 8) (3 7) (7 6) (2 5) (8 4) (5 3) (1 2) (4 1))
  ;  ((2 8) (7 7) (3 6) (6 5) (8 4) (5 3) (1 2) (4 1))
  ;  ((7 8) (3 7) (1 6) (6 5) (8 4) (5 3) (2 2) (4 1))
  ;  ((5 8) (1 7) (8 6) (6 5) (3 4) (7 3) (2 2) (4 1))
  ;  ((1 8) (5 7) (8 6) (6 5) (3 4) (7 3) (2 2) (4 1))
  ;  ((3 8) (6 7) (8 6) (1 5) (5 4) (7 3) (2 2) (4 1))
  ;  ((6 8) (3 7) (1 6) (7 5) (5 4) (8 3) (2 2) (4 1))
  ;  ((7 8) (5 7) (3 6) (1 5) (6 4) (8 3) (2 2) (4 1))
  ;  ((7 8) (3 7) (8 6) (2 5) (5 4) (1 3) (6 2) (4 1))
  ;  ((5 8) (3 7) (1 6) (7 5) (2 4) (8 3) (6 2) (4 1))
  ;  ((2 8) (5 7) (7 6) (1 5) (3 4) (8 3) (6 2) (4 1))
  ;  ((3 8) (6 7) (2 6) (5 5) (8 4) (1 3) (7 2) (4 1))
  ;  ((6 8) (1 7) (5 6) (2 5) (8 4) (3 3) (7 2) (4 1))
  ;  ((8 8) (3 7) (1 6) (6 5) (2 4) (5 3) (7 2) (4 1))
  ;  ((2 8) (8 7) (6 6) (1 5) (3 4) (5 3) (7 2) (4 1))
  ;  ((5 8) (7 7) (2 6) (6 5) (3 4) (1 3) (8 2) (4 1))
  ;  ((3 8) (6 7) (2 6) (7 5) (5 4) (1 3) (8 2) (4 1))
  ;  ((6 8) (2 7) (7 6) (1 5) (3 4) (5 3) (8 2) (4 1))
  ;  ((3 8) (7 7) (2 6) (8 5) (6 4) (4 3) (1 2) (5 1))
  ;  ((6 8) (3 7) (7 6) (2 5) (4 4) (8 3) (1 2) (5 1))
  ;  ((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (1 2) (5 1))
  ;  ((7 8) (1 7) (3 6) (8 5) (6 4) (4 3) (2 2) (5 1))
  ;  ((1 8) (6 7) (8 6) (3 5) (7 4) (4 3) (2 2) (5 1))
  ;  ((3 8) (8 7) (4 6) (7 5) (1 4) (6 3) (2 2) (5 1))
  ;  ((6 8) (3 7) (7 6) (4 5) (1 4) (8 3) (2 2) (5 1))
  ;  ((7 8) (4 7) (2 6) (8 5) (6 4) (1 3) (3 2) (5 1))
  ;  ((4 8) (6 7) (8 6) (2 5) (7 4) (1 3) (3 2) (5 1))
  ;  ((2 8) (6 7) (1 6) (7 5) (4 4) (8 3) (3 2) (5 1))
  ;  ((2 8) (4 7) (6 6) (8 5) (3 4) (1 3) (7 2) (5 1))
  ;  ((3 8) (6 7) (8 6) (2 5) (4 4) (1 3) (7 2) (5 1))
  ;  ((6 8) (3 7) (1 6) (8 5) (4 4) (2 3) (7 2) (5 1))
  ;  ((8 8) (4 7) (1 6) (3 5) (6 4) (2 3) (7 2) (5 1))
  ;  ((4 8) (8 7) (1 6) (3 5) (6 4) (2 3) (7 2) (5 1))
  ;  ((2 8) (6 7) (8 6) (3 5) (1 4) (4 3) (7 2) (5 1))
  ;  ((7 8) (2 7) (6 6) (3 5) (1 4) (4 3) (8 2) (5 1))
  ;  ((3 8) (6 7) (2 6) (7 5) (1 4) (4 3) (8 2) (5 1))
  ;  ((4 8) (7 7) (3 6) (8 5) (2 4) (5 3) (1 2) (6 1))
  ;  ((4 8) (8 7) (5 6) (3 5) (1 4) (7 3) (2 2) (6 1))
  ;  ((3 8) (5 7) (8 6) (4 5) (1 4) (7 3) (2 2) (6 1))
  ;  ((4 8) (2 7) (8 6) (5 5) (7 4) (1 3) (3 2) (6 1))
  ;  ((5 8) (7 7) (2 6) (4 5) (8 4) (1 3) (3 2) (6 1))
  ;  ((7 8) (4 7) (2 6) (5 5) (8 4) (1 3) (3 2) (6 1))
  ;  ((8 8) (2 7) (4 6) (1 5) (7 4) (5 3) (3 2) (6 1))
  ;  ((7 8) (2 7) (4 6) (1 5) (8 4) (5 3) (3 2) (6 1))
  ;  ((5 8) (1 7) (8 6) (4 5) (2 4) (7 3) (3 2) (6 1))
  ;  ((4 8) (1 7) (5 6) (8 5) (2 4) (7 3) (3 2) (6 1))
  ;  ((5 8) (2 7) (8 6) (1 5) (4 4) (7 3) (3 2) (6 1))
  ;  ((3 8) (7 7) (2 6) (8 5) (5 4) (1 3) (4 2) (6 1))
  ;  ((3 8) (1 7) (7 6) (5 5) (8 4) (2 3) (4 2) (6 1))
  ;  ((8 8) (2 7) (5 6) (3 5) (1 4) (7 3) (4 2) (6 1))
  ;  ((3 8) (5 7) (2 6) (8 5) (1 4) (7 3) (4 2) (6 1))
  ;  ((3 8) (5 7) (7 6) (1 5) (4 4) (2 3) (8 2) (6 1))
  ;  ((5 8) (2 7) (4 6) (6 5) (8 4) (3 3) (1 2) (7 1))
  ;  ((6 8) (3 7) (5 6) (8 5) (1 4) (4 3) (2 2) (7 1))
  ;  ((5 8) (8 7) (4 6) (1 5) (3 4) (6 3) (2 2) (7 1))
  ;  ((4 8) (2 7) (5 6) (8 5) (6 4) (1 3) (3 2) (7 1))
  ;  ((4 8) (6 7) (1 6) (5 5) (2 4) (8 3) (3 2) (7 1))
  ;  ((6 8) (3 7) (1 6) (8 5) (5 4) (2 3) (4 2) (7 1))
  ;  ((5 8) (3 7) (1 6) (6 5) (8 4) (2 3) (4 2) (7 1))
  ;  ((4 8) (2 7) (8 6) (6 5) (1 4) (3 3) (5 2) (7 1))
  ;  ((6 8) (3 7) (5 6) (7 5) (1 4) (4 3) (2 2) (8 1))
  ;  ((6 8) (4 7) (7 6) (1 5) (3 4) (5 3) (2 2) (8 1))
  ;  ((4 8) (7 7) (5 6) (2 5) (6 4) (1 3) (3 2) (8 1))
  ;  ((5 8) (7 7) (2 6) (6 5) (3 4) (1 3) (4 2) (8 1)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.43 (hesitant about this answer)                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Switching the lambdas causes the recursive call to queen-cols to be called
; board-size * k times as opposed to k times before switching, so assuming
; ex2.42 solves the problem in T time, switching lambdas will solve it in
; board-size * T.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.44                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.44 (beside below)
  (import scheme debug)
  (title "ex2.44")

  (define (beside painter1 painter2)
    '())

  (define (below painter1 painter2)
    '())

  (define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
          (below painter (beside smaller smaller))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.45                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.45 ()
  (import scheme debug)
  (title "ex2.45")

  (define (split combiner1 combiner2)
    (lambda (painter n)
      (if (= n 0)
          painter
          (let ((smaller (split painter (- n 1))))
            (combiner1 painter (combiner2 smaller smaller)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.46                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.46 (make-vect xcor-vect ycor-vect add-vect sub-vect scale-vect)
  (import scheme debug)
  (title "ex2.46")

  (define (make-vect x y) (cons x y))
  (define (xcor-vect v) (car v))
  (define (ycor-vect v) (cdr v))

  (define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
              (+ (ycor-vect v1) (ycor-vect v2))))

  (define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2))
              (- (ycor-vect v1) (ycor-vect v2))))

  (define (scale-vect s v)
    (make-vect (* s (xcor-vect v))
              (* s (ycor-vect v)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.47                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.47 (make-frame edge1-frame edge2-frame origin-frame)
  (import scheme debug)
  (title "ex2.47")

  (define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))

  (define (origin-frame f) (car f))
  (define (edge1-frame f) (cadr f))
  (define (edge2-frame f) (caddr f))

  (define (make-frame origin edge1 edge2)
    (cons origin (cons edge1 edge2)))

  (define (origin-frame f) (car f))
  (define (edge1-frame f) (cadr f))
  (define (edge2-frame f) (cddr f)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.48                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.48 (make-segment start-segment end-segment)
  (import scheme debug)
  (title "ex2.48")

  (define (make-segment v1 v2) (cons v1 v2))
  (define (start-segment s) (car s))
  (define (end-segment s) (cdr s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.49                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.49 (frame-coord-map)
  (import scheme debug)
  (import (only ex2.46 xcor-vect ycor-vect add-vect scale-vect))
  (import (only ex2.47 edge1-frame edge2-frame origin-frame))
  (import (only ex2.48 make-segment start-segment end-segment))
  (title "ex2.49")

  (define (draw-line p1 p2)
    '())

  (define (frame-coord-map frame)
    (lambda (v)
      (add-vect
        (origin-frame frame)
        (add-vect (scale-vect (xcor-vect v)
                              (edge1-frame frame))
                  (scale-vect (ycor-vect v)
                              (edge2-frame frame))))))

  (define (segments->painter segment-list)
    (lambda (frame)
      (for-each
        (lambda (segment)
          (draw-line
            ((frame-coord-map frame) (start-segment segment))
            ((frame-coord-map frame) (end-segment segment))))
        segment-list)))

  ; Part (a):

  (define (outline f)
    (let* ((ll (origin-frame f))
          (lr (add-vect ll (edge1-frame f)))
          (ul (add-vect ll (edge2-frame f)))
          (ur (add-vect lr (edge2-frame f))))
      (segments->painter
        (list (make-segment ll lr)
              (make-segment ll ul)
              (make-segment lr ur)
              (make-segment ul ur)))))

  ; Part (b):

  (define (outline f)
    (let* ((ll (origin-frame f))
          (lr (add-vect ll (edge1-frame f)))
          (ul (add-vect ll (edge2-frame f)))
          (ur (add-vect lr (edge2-frame f))))
      (segments->painter
        (list (make-segment ll ur)
              (make-segment lr ul)))))

  ; Part (c):

  (define (outline f)
    (let* ((ll (origin-frame f))
          (lr (add-vect ll (edge1-frame f)))
          (ul (add-vect ll (edge2-frame f)))
          (ur (add-vect lr (edge2-frame f)))
          (lm (scale-vect 0.5 (add-vect ll ul)))
          (rm (scale-vect 0.5 (add-vect lr ur)))
          (bm (scale-vect 0.5 (add-vect ll lr)))
          (tm (scale-vect 0.5 (add-vect ul ur))))
      (segments->painter
        (list (make-segment lm ur)
              (make-segment ul rm)
              (make-segment rm bm)
              (make-segment bm lm)))))

  ; Part (d):
  ;
  ; I'll skip this one!
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.50                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.50 (flip-horiz rotate180 rotate270 transform-painter)
  (import scheme debug)
  (import (only ex2.46 make-vect sub-vect))
  (import (only ex2.47 make-frame))
  (import (only ex2.49 frame-coord-map))
  (title "ex2.50")

  (define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
      (let ((m (frame-coord-map frame)))
        (let ((new-origin (m origin)))
          (painter
          (make-frame new-origin
                      (sub-vect (m corner1) new-origin)
                      (sub-vect (m corner2) new-origin)))))))

  (define (flip-horiz painter)
    (transform-painter painter
                      (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0)))

  (define (rotate180 painter)
    (transform-painter painter
                      (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0)))

  (define (rotate270 painter)
    (transform-painter painter
                      (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.51                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.51 ()
  (import scheme debug)
  (import (only ex2.46 make-vect))
  (import (only ex2.50 flip-horiz rotate180 rotate270 transform-painter))
  (title "ex2.51")

  (define (rotate90 painter1)
    '())

  (define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
      (let ((paint-bottom
            (transform-painter painter1
                                (make-vect 0.0 0.0)
                                (make-vect 1.0 0.0)
                                split-point))
            (paint-top
            (transform-painter painter2
                                split-point
                                (make-vect 1.0 0.5)
                                (make-vect 0.0 1.0))))
        (lambda (frame)
          (paint-bottom frame)
          (paint-top frame)))))

  (define (beside painter1 painter2)
    (let ((split-point (make-vect 0.5 0.0)))
      (let ((paint-left
            (transform-painter painter1
                                (make-vect 0.0 0.0)
                                split-point
                                (make-vect 0.0 1.0)))
            (paint-right
            (transform-painter painter2
                                split-point
                                (make-vect 1.0 0.0)
                                (make-vect 0.5 1.0))))
        (lambda (frame)
          (paint-left frame)
          (paint-right frame)))))

  (define (below painter1 painter2)
    (rotate90 (beside (rotate270 painter1) (rotate270 painter2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.52                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.52 ()
  (import scheme debug)
  (import (only ex2.44 beside below))
  (import (only ex2.46 make-vect))
  (import (only ex2.50 flip-horiz rotate180 transform-painter))
  (title "ex2.52")

  ; Part (a):
  ;
  ; I'll skip this one too.

  ; Part (b):

  (define (right-split painter n)
    '())

  (define (up-split painter n)
    '())

  (define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
          (let ((top-left up)
                (bottom-right right)
                (corner (corner-split painter (- n 1))))
            (beside (below painter top-left)
                    (below bottom-right corner))))))

  ; Part (c):

  (define (identity painter) painter)

  (define (flip-vert painter)
    (transform-painter painter
                      (make-vect 0.0 1.0)   ; new origin
                      (make-vect 1.0 1.0)   ; new end of edge1
                      (make-vect 0.0 0.0))) ; new end of edge2

  (define (square-of-four tl tr bl br)
    (lambda (painter)
      (let ((top (beside (tl painter) (tr painter)))
            (bottom (beside (bl painter) (br painter))))
        (below bottom top))))

  (define (square-limit painter n)
    (let ((combine4 (square-of-four identity flip-horiz
                                    flip-vert rotate180)))
      (combine4 (corner-split painter n))))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.53                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.53 ()
  (import scheme debug)
  (title "ex2.53")

  (println (list 'a 'b 'c))                         ; (a b c)
  (println (list (list 'george)))                   ; ((george))
  (println (cdr '((x1 x2) (y1 y2))))                ; ((y1 y2))
  (println (cadr '((x1 x2) (y1 y2))))               ; (y1 y2)
  (println (pair? (car '(a short list))))           ; #f
  (println (memq 'red '((red shoes) (blue socks)))) ; #f
  (println (memq 'red '(red shoes blue socks)))     ; (red shoes blue socks)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.54                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.54 ()
  (import scheme debug)
  (title "ex2.54")

  (define (equal-book? l1 l2)
    (if (not (or (null? l1) (null? l2)))
      (let ((a1 (car l1)) (d1 (cdr l1))
            (a2 (car l2)) (d2 (cdr l2)))
        (cond
          ((and (symbol? a1) (symbol? a2)) (and (eq? a1 a2) (equal-book? d1 d2)))
          ((and (number? a1) (number? a2)) (and (= a1 a2) (equal-book? d1 d2)))
          (else (and (equal-book? a1 a2) (equal-book? d1 d2)))))
      (and (null? l1) (null? l2))))

  (println (equal-book? '(1 a 4.5) '(1 a 4.5)))
  (println (equal-book? '(1 b 4.5) '(1 a 4.5)))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.55                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; ''abracadabr is syntactic sugar for (quote (quote abracadabra)). Taking the
; car of that gives: quote.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.56                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.56 (deriv =number?)
  (import scheme debug)
  (import (only chicken error))
  (title "ex2.56")

  (define (variable? x) (symbol? x))

  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

  (define (make-sum a1 a2) (list '+ a1 a2))

  (define (make-product m1 m2) (list '* m1 m2))

  (define (sum? x)
    (and (pair? x) (eq? (car x) '+)))

  (define (addend s) (cadr s))

  (define (augend s) (caddr s))

  (define (product? x)
    (and (pair? x) (eq? (car x) '*)))

  (define (multiplier p) (cadr p))

  (define (multiplicand p) (caddr p))

  (define (=number? ex num)
    (and (number? ex) (= ex num)))

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2))))

  (define (exponentiation? x)
    (and (pair? x) (eq? (car x) '**)))

  (define (base e) (cadr e))

  (define (exponent e) (caddr e))

  (define (make-exponentiation b e)
    (cond ((=number? e 0) 1)
          ((=number? e 1) b)
          ((and (number? b) (number? e)) (expt b e))
          (else (list '** b e))))

  (define (paren-expr? ex) (and (pair? ex) (= (length ex) 1)))

  (define (deriv ex var)
    (cond ((number? ex) 0)
          ((variable? ex)
          (if (same-variable? ex var) 1 0))
          ((sum? ex)
          (make-sum (deriv (addend ex) var)
                    (deriv (augend ex) var)))
          ((product? ex)
          (make-sum
            (make-product (multiplier ex)
                          (deriv (multiplicand ex) var))
            (make-product (deriv (multiplier ex) var)
                          (multiplicand ex))))
          ((exponentiation? ex)
          (make-product
            (make-product
              (exponent ex)
              (make-exponentiation (base ex)
                                    (make-sum (exponent ex) -1)))
            (deriv (base ex) var)))
          ((paren-expr? ex) (deriv (car ex) var))
          (else
          (error "unknown expression type -- DERIV" ex))))

  (println (deriv '(+ (** x 3) (+ x y)) 'x))
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.57                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.57 ()
  (import scheme debug)
  (title "ex2.57")

  (define (augend s)
    (if (= (length s) 3)
      (caddr s)
      (cons '+ (cddr s))))

  (define (multiplicand s)
    (if (= (length s) 3)
      (caddr s)
      (cons '* (cddr s)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.58                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.58 ()
  (import scheme debug)
  (import (only chicken foldr))
  (import (only ex2.40 filter))
  (import (only ex2.56 =number?))
  (title "ex2.58")

  ; Part (a):

  (define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list a1 '+ a2))))

  (define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))))

  (define (sum? x)
    (and (pair? x) (eq? (cadr x) '+)))

  (define (addend s) (car s))

  (define (augend s) (caddr s))

  (define (product? x)
    (and (pair? x) (eq? (cadr x) '*)))

  (define (multiplier p) (car p))

  (define (multiplicand p) (caddr p))

  ; Part (b):

  (define (join l sym)
    (if (= (length l) 1)
      (if (sum? (car l)) l (car l))
      (append (list (car l) sym)
              (join (cdr l) sym))))

  (define (make-sum . as)
    (let ((numbers-sum (foldr + 0 (filter (lambda (a) (number? a)) as)))
          (non-numbers (filter (lambda (a) (not (number? a))) as)))
      (cond ((zero? (length non-numbers)) numbers-sum)
            ((zero? numbers-sum) (join non-numbers '+))
            (else (append (join non-numbers '+) (list '+ numbers-sum))))))

  (define (make-product . ms)
    (let ((numbers-product (foldr * 1 (filter (lambda (m) (number? m)) ms)))
          (non-numbers (filter (lambda (m) (not (number? m))) ms)))
      (cond ((zero? (length non-numbers)) numbers-product)
            ((zero? numbers-product) 0)
            ((= numbers-product 1) (join non-numbers '+))
            (else (append (list numbers-product '*) (join non-numbers '*))))))

  (define (sum? x) (and (pair? x) (memq '+ x)))

  (define (take-while pred l)
    (cond
      ((null? l) '())
      ((pred (car l)) (cons (car l) (take-while pred (cdr l))))
      (else '())))

  (define (addend s)
    (take-while (lambda (x) (not (eq? x '+))) s))

  (define (augend s) (cdr (memq '+ s)))

  (define (product? x)
    (and (pair? x) (memq '* x) (not (sum? x))))

  (define (multiplier p) (car p))

  (define (multiplicand p) (cddr p))

  (define (paren-expr? ex) (and (pair? ex) (= (length ex) 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.59                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.59 ()
  (import scheme debug)
  (title "ex2.59")

  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

  (define (union-set set1 set2)
    (cond
      ((null? set1) set2)
      ((element-of-set? (car set1) set2) (union-set (cdr set1) set2))
      (else (cons (car set1) (union-set (cdr set1) set2))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.60                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.60 (element-of-set?)
  (import scheme debug)
  (title "ex2.60")

  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

  (define (adjoin-set x set) (cons x set))

  (define (intersection-set set1 set2)
    (cond ((or (null? set1) (null? set2)) '())
          ((element-of-set? (car set1) set2)
          (cons (car set1)
                (intersection-set (cdr set1) set2)))
          (else (intersection-set (cdr set1) set2))))

  (define (union-set set1 set2) (append set1 set2)))

; element-of-set?: theta(n) (here n is the number of elements in the list
;                            representation of the set, which is generally
;                            speaking, larger than the number of set elements).
; adjoin-set: theta(1)
; intersection-set: theta(n^2) (n is as described before).
; union-set: theta(1) (assuming append is theta(1) at that level of abstraction,
;                      but in general, append is theta(n)).
;
; Another aspect to take in account is that a set in this representation in
; general takes a lot more space than a set in the non-duplicate representation.
;
; This representation can be used in applications where elements are added to
; sets and sets are merged a lot more often than sets being inspected for
; certain elements or set intersections are computed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.61                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.61 ()
  (import scheme debug)
  (title "ex2.61")

  (define (adjoin-set x set)
    (cond
      ((= x (car set)) set)
      ((< x (car set)) (cons x set))
      (else (cons (car set) (adjoin-set x (cdr set)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.62                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.62 ()
  (import scheme debug)
  (title "ex2.62")

  (define (union-set set1 set2)
    (cond
      ((null? set1) set2)
      ((null? set2) set1)
      (else
        (let ((x (car set1)) (y (car set2)))
          (cond
            ((= x y) (cons x (union-set (cdr set1) (cdr set2))))
            ((< x y) (cons x (union-set (cdr set1) set2)))
            (else (cons y (union-set set1 (cdr set2))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.63                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part (a):
;
; The two procedures produce the same results. tree->list-1 converts the left
; branch first and then appends to it the conversion of the right branch.
; tree->list-2 converts the right branch first and then keeps cons'ing the left
; branch right-to-left depth-first.
;
; For all trees, both functions will produce the following result:
;   (1 3 5 7 9 11)
;
; Part (b):
;
; They both grow in theta(n) number of steps (hesitant).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.64                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part (a):
;
; The first n elements of elts are divided into three parts, of sizes left-size
; (equals to floor((n-1)/2), 1 and right-size (equals to ((n-1) - left-size)).
; The first left-size elements are used to create the left-result, whose car is
; the left-tree and cdr is the non-left-elts. The next element is going to be
; this-entry. The next right-size elements are used to create the right-result,
; whose car is the right-tree and cdr is the remaining-elts. Finally, the tree
; is constructed from this-entry, left-tree and right-tree and cons'ed to the
; remaining-elmts.
;
;                            5
;                           / \
;                          /   \
;                         1     9
;                          \   / \
;                           3 7   11
;
; Part (b); theta(n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.65                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.65 ()
  (import scheme debug)
  (title "ex2.65")

  (define (tree->list-2 t)
    '())

  (define (list->tree l)
    '())

  (define (union-set set1 set2)
    (define (union-set-list l1 l2)
      (cond
        ((null? l1) l2)
        ((null? l2) l1)
        (else
          (let ((x (car l1)) (y (car l2)))
            (cond
              ((= x y) (cons x (union-set-list (cdr l1) (cdr l2))))
              ((< x y) (cons x (union-set-list (cdr l1) l2)))
              (else (cons y (union-set-list l1 (cdr l2)))))))))
    (list->tree (union-set-list (tree->list-2 set1) (tree->list-2 set2))))

  (define (intersection-set set1 set2)
    (define (intersection-set-list l1 l2)
      (if (or (null? l1) (null? l2))
          '()
          (let ((x1 (car l1)) (x2 (car l2)))
            (cond ((= x1 x2) (cons x1 (intersection-set-list (cdr l1) (cdr l2))))
                  ((< x1 x2) (intersection-set-list (cdr l1) l2))
                  (else (intersection-set-list l1 (cdr l2)))))))
    (list->tree (intersection-set-list (tree->list-2 set1) (tree->list-2 set2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.66                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.66 ()
  (import scheme debug)
  (import (only ex2.29 left-branch right-branch))
  (title "ex2.66")

  (define (key entry) '())
  (define (entry records) '())

  (define (lookup given-key set-of-records)
    (cond ((null? set-of-records) #f)
          ((= given-key (key (entry set-of-records))) (entry set-of-records))
          ((< given-key (key (entry set-of-records)))
           (lookup given-key (left-branch set-of-records)))
          ((> given-key (key (entry set-of-records)))
           (lookup given-key (right-branch set-of-records))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.67                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.67 (sample-tree right-branch left-branch symbols leaf?
                make-code-tree make-leaf weight)
  (import scheme debug)
  (import (only chicken error))
  (title "ex2.67")

  (define (make-leaf symbol weight)
    (list 'leaf symbol weight))

  (define (leaf? object)
    (eq? (car object) 'leaf))

  (define (symbol-leaf x) (cadr x))

  (define (weight-leaf x) (caddr x))

  (define (make-code-tree left right)
    (list left
          right
          (append (symbols left) (symbols right))
          (+ (weight left) (weight right))))

  (define (left-branch tree) (car tree))

  (define (right-branch tree) (cadr tree))

  (define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree))
        (caddr tree)))

  (define (weight tree)
    (if (leaf? tree)
        (weight-leaf tree)
        (cadddr tree)))

  (define (decode bits tree)
    (define (decode-1 bits current-branch)
      (if (null? bits)
          '()
          (let ((next-branch
                 (choose-branch (car bits) current-branch)))
            (if (leaf? next-branch)
                (cons (symbol-leaf next-branch)
                      (decode-1 (cdr bits) tree))
                (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))

  (define sample-tree
    (make-code-tree (make-leaf 'A 4)
                    (make-code-tree
                      (make-leaf 'B 2)
                      (make-code-tree (make-leaf 'D 1)
                                      (make-leaf 'C 1)))))

  (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

  (println (decode sample-message sample-tree)) ; (A D A B B C A)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.68                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.68 (encode)
  (import scheme debug)
  (import (only chicken error))
  (import (only ex2.60 element-of-set?))
  (import (only ex2.67 sample-tree right-branch left-branch symbols leaf?))
  (title "ex2.68")

  (define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

  (define (encode-symbol symbol tree)
    (let ((left (left-branch tree)) (right (right-branch tree)))
      (cond
        ((leaf? tree) '())
        ((element-of-set? symbol (symbols left))
          (cons 0 (encode-symbol symbol left)))
        ((element-of-set? symbol (symbols right))
          (cons 1 (encode-symbol symbol right)))
        (else (error "bad symbol - ENCODE-SYMBOL" symbol)))))

  (println (encode '(A D A B B C A) sample-tree)) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.69                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.69 (generate-huffman-tree)
  (import scheme debug)
  (import (only ex2.67 make-code-tree make-leaf weight))
  (title "ex2.69")

  (define (adjoin-set x set)
    (cond ((null? set) (list x))
          ((< (weight x) (weight (car set))) (cons x set))
          (else (cons (car set)
                      (adjoin-set x (cdr set))))))

  (define (make-leaf-set pairs)
    (if (null? pairs)
        '()
        (let ((pair (car pairs)))
          (adjoin-set (make-leaf (car pair)    ; symbol
                                 (cadr pair))  ; frequency
                      (make-leaf-set (cdr pairs))))))

  (define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

  (define (successive-merge set)
    (if (= (length set) 1)
      (car set)
      (successive-merge (adjoin-set (make-code-tree (car set) (cadr set))
                                    (cddr set)))))

  (println (generate-huffman-tree '((a 4) (b 2) (c 1) (d 1))))
  ; ((leaf a 4) ((leaf b 2) ((leaf d 1) (leaf c 1) (d c) 2) (b d c) 4) (a b d c) 8)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.70                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.70 ()
  (import scheme debug)
  (import (only ex2.68 encode))
  (import (only ex2.69 generate-huffman-tree))
  (title "ex2.70")

  (define huffman-tree (generate-huffman-tree
    '((a 2) (na 16) (boom 1) (Sha 3) (Get 2) (yip 9) (job 2) (Wah 1))))

  (define message '(Get a job
                    Sha na na na na na na na na
                    Get a job
                    Sha na na na na na na na na
                    Wah yip yip yip yip yip yip yip yip yip
                    Sha boom))

  (println (encode message huffman-tree))

  ; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1
  ;  0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1
  ;  0 1 1 0 1 1)

  ; The huffman-encoded message needs 84 bits. Using fixed-length codes, each
  ; symbol of the 8 symbols will be encoded in 3 bits. We have 36 symbols in the
  ; message, so the minimum number of bits needed is 3 * 36 = 108.
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.71                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Since 2^n < 2^(n+1) for all integers n > 0, then 2^n + 2^(n+1) < 2*2^(n+1)
; i.e. 2^n + 2^(n+1) < 2^(n+2). This means that the sum of weights of every two
; consequtive symbols is always going to be less than that of the next symbol in
; sequence. This means that trees will always grow to the right.
;
; n = 5:
;                   (a b c d e) 31
;                        /\
;                       /  \
;                      /    \
;                    e 16  (a b c d) 15
;                            /\
;                           /  \
;                          /    \
;                        d 8   (a b c) 7
;                                /\
;                               /  \
;                              /    \
;                            c 4   (a b) 3
;                                    /\
;                                   /  \
;                                  /    \
;                                b 2    a 1
;
;
; n = 10:
;               (a b c d e f g h i j) 1023
;                        /\
;                       /  \
;                      /    \
;                   j 512  (a b c d e f g h i) 511
;                            /\
;                           /  \
;                          /    \
;                       i 256  (a b c d e f g h) 255
;                                /\
;                               /  \
;                              /    \
;                           h 128  (a b c d e f g) 127
;                                    /\
;                                   /  \
;                                  /    \
;                                g 64  (a b c d e f) 63
;                                        /\
;                                       /  \
;                                      /    \
;                                    f 32  (a b c d e) 31
;                                            /\
;                                           /  \
;                                          /    \
;                                        e 16  (a b c d) 15
;                                                /\
;                                               /  \
;                                              /    \
;                                            d 8   (a b c) 7
;                                                    /\
;                                                   /  \
;                                                  /    \
;                                                c 4   (a b) 3
;                                                        /\
;                                                       /  \
;                                                      /    \
;                                                     b 2   a 1
;
; Most frequent symbol: 1-bit
; Least frequent symbol: (n-1)-bits


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.72                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Order of growth of encode for a message consisting solely of n instances of
; the most frequent symbol is theta(n).
;
; Order of growth of encode for a message consisting solely of n instances of
; the least frequent symbol is theta(n^2).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.73                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.73 (attach-tag type-tag contents)
  (import scheme debug)
  (import (only ex2.56 =number?))
  (title "ex2.73")
  (reset-proc-table)

  ; Part (a):
  ;
  ; What happened is that now the derivation function for every expression type is
  ; stored in an operation table which is accessed with two keys:
  ;   1) The operation name (always 'deriv),
  ;   2) The expression type tag being the expression symbol (e.g. +).
  ;
  ; We couldn't assimilate number? and same-variable? predicates because there is
  ; no expression symbol to act as key in the operation table for numbers and
  ; variables.
  ;
  ; Part (b):

  (define (attach-tag tag ex) (cons tag ex))
  (define (type-tag ex) (car ex))
  (define (contents ex) (cdr ex))

  (define (deriv expression var)
    ((get 'deriv (type-tag expression)) (contents expression) var))

  (define (install-sum-deriv-rule)
    ;; internal procedures
    (define (make-sum-internal a1 a2)
      (cond ((=number? a1 0) a2)
            ((=number? a2 0) a1)
            ((and (number? a1) (number? a2)) (+ a1 a2))
            (else (list a1 a2))))

    (define (addend operands) (car operands))
    (define (augend operands) (cadr operands))

    (define (sum-deriv operands var)
      (make-sum (deriv (addend operands) var)
                (deriv (augend operands) var)))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag '+ x))
    (put 'deriv '+ sum-deriv)
    (put 'make-sum '+ (lambda (x y) (tag (make-sum-internal x y))))

    'done)

  (define (install-product-deriv-rule)
    ;; internal procedures
    (define (make-product-internal m1 m2)
      (cond ((or (=number? m1 0) (=number? m2 0)) 0)
            ((=number? m1 1) m2)
            ((=number? m2 1) m1)
            ((and (number? m1) (number? m2)) (* m1 m2))
            (else (list m1 m2))))

    (define (multiplier operands) (car operands))
    (define (multiplicand operands) (cadr operands))

    (define (product-deriv operands var)
      (make-sum (make-product (multiplier operands)
                              (deriv (multiplicand operands) var))
                (make-product (deriv (multiplier operands) var)
                              (multiplicand operands))))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag '* x))
    (put 'deriv '* product-deriv)
    (put 'make-product '* (lambda (x y) (tag (make-product-internal x y))))

    'done)

  (define (make-sum x y)
    ((get 'make-sum '+) x y))

  (define (make-product x y)
    ((get 'make-product '*) x y))

  ; Part (c):

  (define (install-exponentiation-deriv-rule)
    ;; internal procedures
    (define (make-exponentiation-internal b e)
      (cond ((=number? e 0) 1)
            ((=number? e 1) b)
            ((and (number? b) (number? e)) (expt b e))
            (else (list b e))))

    (define (base e) (car e))
    (define (exponent e) (cadr e))

    (define (exponentiation-deriv operands var)
      (make-product (exponent operands)
                    (make-exponentiation (base operands)
                                         (make-sum (exponent operands) -1))))

    ;; interface to the rest of the system
    (define (tag x) (attach-tag '** x))
    (put 'deriv '** exponentiation-deriv)
    (put 'make-exponentiation '** (lambda (b e) (tag (make-exponentiation-internal b e))))

    'done)

  (define (make-exponentiation b e)
    ((get 'make-exponentiation '**) b e))

  (install-sum-deriv-rule)
  (install-product-deriv-rule)
  (install-exponentiation-deriv-rule)
  (println (deriv (make-exponentiation 'x 3) 'x))

  ; Part (d):
  ;
  ; Just switch the arguments to the 'put' procedure too.
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.74                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.74 ()
  (import scheme debug)
  (title "ex2.74")
  (reset-proc-table)

  (define (record-division record) '())
  (define (file-division file) '())

  ; Part (a):

  (define (get-record name file)
    ((get 'get-record (file-division file)) name file))

  ; each division personnel file must have type information specifying which
  ; division this file belongs to, such that (file-division file) should be able
  ; to return the file's division.
  ;
  ; Part (b):

  (define (get-salary record)
    ((get 'get-salary (record-division record)) record))

  ; Part (c):

  (define (find-employee-record name files)
    (if (null? files)
      #f
      (or (get-record name (car files))
          (find-employee-record name (cdr files)))))

  ; Part (d):
  ;
  ; When a new company comes in, they have to install their own get-record and
  ; get-salary (and possibly other) procedures in the operations table by calling
  ; 'put' on every procedure passing it the division (company) name as a type tag.
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.75                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.75 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex2.75")

  (define (make-from-mag-ang r a)
    (define (dispatch op)
      (cond ((eq? op 'real-part) (* r (cos a)))
            ((eq? op 'imag-part) (* r (sin a)))
            ((eq? op 'magnitude) r)
            ((eq? op 'angle) a)
            (else
            (error "Unknown op -- MAKE-FROM-MAG_ANG" op))))
    dispatch))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.76                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Explicit Dispatch:
;   New Type:      Create operations for the new type and add a clause for the
;                  new type and for every operation under that type in the
;                  dispatch manager.
;   New Operation: Add a clause for the new operation under that type in the
;                  dispatch manager.
;
; Data-Directed:
;   New Type:      Create operations for the new type and create an installation
;                  procedure to install the new type operations in the
;                  operations table.
;   New Operation: Modify the installation procedure by adding a new 'put' call
;                  to install the new procedure for this type.
;
; Message-Passing:
;   New Type:      Create operations for the new type and add them to the type's
;                  dispatch procedure, which is usually defined in the type's
;                  constructor.
;   New Operation: Add the new procedure to the type's dispatch procedure.
;
; I'd use message-passing for systems where types are added all the time,
; because it involves nothing more that implementing the type's constructor
; procedure, as opposed to having to go through the hassle of creating an
; installation procedure for every new type as is the case with the
; data-directed style.
;
; For systems where new procedures are added all the time, I'd go with the
; data-directed style because the process is as simple as installing new
; procedures using a 'put' as opposed to having to modify types' dispatch
; procedures by adding new clauses all the time for the new procedures.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.77                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (magnitude z) is going to evaluate (apply-generic 'magnitude z). This will
; strip off the type tag from z (in this case, it's (complex)) and use it as a
; key in the table along with the operation symbol 'magnitude. This will return
; the generic magnitude operation which works on both polar and rectangular
; complex numbers. Calling this function will evaluate (apply-generic 'magnitude
; (contents z)). (contents z) is the list (rectangular (3 . 4)), so
; apply-generic with access the operations table using the operation symbol
; 'magnitude and the type tag (rectangular). This will return the rectangular
; magnitude calculation procedure which returns the square root of the sum of
; squares of its arguments thus returning 5.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.78                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.78 (type-tag contents attach-tag)
  (import scheme debug)
  (title "ex2.78")

  (define (type-tag obj)
    (if (number? obj)
      'scheme-number
      (car obj)))

  (define (contents obj)
    (if (number? obj)
      obj
      (cdr obj)))

  (define (attach-tag tag obj)
    (if (eq? tag 'scheme-number)
      obj
      (cons tag obj))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.79                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.79 ()
  (import scheme debug)
  (import (only ex2.78 type-tag contents))
  (title "ex2.79")
  (reset-proc-table)

  (define (numer rat) (car rat))
  (define (denom rat) (cdr rat))

  (define (install-equ?)
    (put 'equ? '(scheme-number scheme-number)
      (lambda (n1 n2) (= n1 n2)))

    (put 'equ? '(rational rational)
      (lambda (n1 n2) (and (= (numer n1) (numer n2))
                           (= (denom n1) (denom n2)))))

    (put 'equ? '(complex complex)
      (lambda (n1 n2) (and (= (real-part n1) (real-part n2))
                           (= (imag-part n1) (imag-part n2))))))

  (install-equ?)
  (define (equ? n1 n2)
    ((get 'equ? (list (type-tag n1) (type-tag n2))) (contents n1) (contents n2)))

  (println (equ? 1 2))
  (println (equ? 1 1))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.80                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.80 ()
  (import scheme debug)
  (import (only ex2.78 attach-tag type-tag contents))
  (title "ex2.80")
  (reset-proc-table)

  (define (install-rat)
    (define (numer x) (car x))
    (define (tag x) (attach-tag 'rational x))
    (put 'make-rat 'rational (lambda (n d) (tag (cons n d))))
    (put '=zero? 'rational (lambda (n) (zero? (numer n)))))

  (define (make-rat n d)
    ((get 'make-rat 'rational) n d))

  (define (install-=zero?)
    (put '=zero? 'scheme-number (lambda (n) (zero? n)))
    (put '=zero? 'complex
      (lambda (n) (and (zero? (real-part n)) (zero? (imag-part n))))))

  (install-proc =zero?)

  (install-rat)
  (install-=zero?)
  (println (=zero? 2))
  (println (=zero? 0))
  (println (=zero? (make-rat 3 6)))
  (println (=zero? (make-rat 0 5)))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.81                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.81 ()
  (import scheme debug)
  (import (only chicken error))
  (import (only ex2.78 attach-tag type-tag contents))
  (title "ex2.81")
  (reset-proc-table)
  (reset-coercion-table)

  (define (scheme-number->scheme-number n) n)
  (define (complex->complex z) z)

  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'complex 'complex complex->complex)

  ; Part (a):

  (define (tag x) (attach-tag '** x))
  (define (exponential x y) (apply-generic 'exponential x y))

  (put 'exponential '(scheme-number scheme-number)
      (lambda (x y) (tag (expt x y)))) ; using primitive expt

  ; Calling exp with two complex numbers in this case will never return
  ; (apply-generic will go into infinite recursion).
  ;
  ; Part (b):
  ;
  ; No, he's not. apply-generic is correct as-is. If the two arguments have the
  ; same type, and that type exists in the table for this operation, proc will not
  ; be false and will be applied to the contents of the arguments. If the type
  ; does not exists in the table for this operation, apply-generic rightly give an
  ; error.
  ;
  ; Part (c):

  (define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
      (let ((proc (get op type-tags)))
        (if proc
            (apply proc (map contents args))
            (if (and (= (length args) 2)
                    (not (= (car type-tags) (cadr type-tags))))
                (let ((type1 (car type-tags))
                      (type2 (cadr type-tags))
                      (a1 (car args))
                      (a2 (cadr args)))
                  (let ((t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2
                          (apply-generic op (t1->t2 a1) a2))
                          (t2->t1
                          (apply-generic op a1 (t2->t1 a2)))
                          (else
                          (error "No method for these types"
                                  (list op type-tags))))))
                (error "No method for these types"
                      (list op type-tags))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.82                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.82 ()
  (import scheme debug)
  (import (only chicken foldr error))
  (import (only ex2.78 type-tag contents))
  (title "ex2.82")
  (reset-proc-table)
  (reset-coercion-table)

  (define (apply-generic op . args)
    (define (coerce-args-to-argn args argn)
      (let* ((argn-type (type-tag argn))
            (new-args
              (map (lambda (arg)
                      (if (= (type-tag arg) argn-type)
                        arg
                        (let ((coerce (get-coercion (type-tag arg) argn-type)))
                          (if coerce (coerce arg) #f))))
                    args)))
          (if (foldr (lambda (x y) (and x y)) #t new-args) new-args #f)))

    (define (try-coercion op all-args remaining-args)
      (let ((new-args (coerce-args-to-argn all-args (car remaining-args))))
        (cond
          ((null? remaining-args) (error "No method for these types"
                                        (list op (map type-tag remaining-args))))
          (new-args
            (let ((proc (get op (map type-tag new-args))))
              (and proc (apply proc (map contents new-args)))))
          (else (try-coercion op all-args (cdr remaining-args))))))

    (let* ((type-tags (map type-tag args))
          (proc (get op type-tags)))
        (if proc
          (apply proc (map contents args))
          (try-coercion op args args))))

  ; An example where the above solution will still fail is when the type hierarchy
  ; is a lattice and the least upper bound of all argument types is not equal to
  ; any of the argument types. For this case to work, all arguments have to be
  ; coerced to the least upper bound type. For example, assume the function max()
  ; is defined only for rational and complex numbers but not for scheme numbers.
  ; Calling this function with two scheme number will not work even though scheme
  ; numbers are coercible to rational numbers.
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.83                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.83 (make-rat make-rect make-pol make-complex
                install-raise raise type-tag contents)
  (import scheme debug)
  (import (only ex2.1 numer denom))
  (title "ex2.83")
  (reset-proc-table)

  (define (type-tag obj)
    (if (number? obj)
      (if (exact? obj) 'integer 'real)
      (car obj)))

  (define (contents obj)
    (if (number? obj)
      obj
      (cdr obj)))

  (define (make-rat n d)
    (cons 'rational (let ((g (gcd n d))) (cons (/ n g) (/ d g)))))

  (define (make-rect x y)
    (cons 'rect (cons x y)))

  (define (make-pol x y)
    (cons 'pol (cons x y)))

  (define (make-complex n)
    (cons 'complex n))

  (define (install-raise)
    (define (raise-integer obj)
      (make-rat obj 1))

    (define (raise-rat obj)
      (exact->inexact (/ (numer obj) (denom obj))))

    (define (raise-real obj)
      (make-complex (make-rect obj 0)))

    (put 'raise 'integer raise-integer)
    (put 'raise 'rational raise-rat)
    (put 'raise 'real raise-real))

  (install-raise)
  (install-proc raise)

  (println (raise 2)) ; (2 . 1)
  (println (raise (make-rat 1 2))) ; 0.5
  (println (raise 5.4)) ; (5.4 . 0)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.84                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.84 (install-add install-complex real imag mag ang higher-type?
                get-highest-type successive-raise)
  (import scheme debug)
  (import (only chicken foldr error))
  (import (only ex2.1 numer denom))
  (import (only ex2.83 type-tag contents make-rat make-rect
                       make-complex install-raise raise))
  (title "ex2.84")
  (reset-proc-table)
  (install-raise)

  (define (install-complex)
    (define (square x) (* x x))

    (put 'real 'rect (lambda (x) (car x)))
    (put 'real 'pol (lambda (x) (* (car x) (cos (cdr x)))))
    (put 'imag 'rect (lambda (x) (cdr x)))
    (put 'imag 'pol (lambda (x) (* (car x) (sin (cdr x)))))
    (put 'mag 'rect (lambda (x) (sqrt (+ (square (car x)) (square (cdr x))))))
    (put 'mag 'pol (lambda (x) (car x)))
    (put 'ang 'rect (lambda (x) (atan (/ (cdr x) (car x)))))
    (put 'ang 'pol (lambda (x) (cdr x))))

  (install-complex)
  (install-proc real)
  (install-proc imag)
  (install-proc mag)
  (install-proc ang)

  (define (install-add)
    (put 'add '(integer integer) (lambda (x y) (+ x y)))
    (put 'add '(rational rational) (lambda (x y) (make-rat (+ (* (numer x) (denom y))
                                                              (* (numer y) (denom x)))
                                                           (* (denom x) (denom y)))))
    (put 'add '(real real) (lambda (x y) (+ x y)))
    (put 'add '(complex complex) (lambda (x y) (make-complex
                                                 (make-rect (+ (real x) (real y))
                                                            (+ (imag x) (imag y)))))))
  (install-add)
  (install-proc2 add)

  (define (higher-type? high low)
    (let ((type-hierarchy '(integer rational real complex)))
      (< (length (memq high type-hierarchy))
          (length (memq low type-hierarchy)))))

  (define (get-highest-type arg-types)
    (foldr (lambda (t1 t2) (if (higher-type? t1 t2) t1 t2))
                'integer arg-types))

  (define (successive-raise arg type)
    (if (eq? (type-tag arg) type)
      arg
      (successive-raise (raise arg) type)))

  (define (apply-generic op . args)
    (let* ((type-tags (map type-tag args))
           (highest-type (get-highest-type type-tags))
           (new-args (map (lambda (arg) (successive-raise arg highest-type)) args))
           (proc (get op (map type-tag new-args))))
        (if proc
          (apply proc (map contents new-args))
          (error "No method for these types" (list op type-tags)))))

  (println (apply-generic 'add 1 (make-rat 3 6))) ; (rational 3 . 2)
  (println (apply-generic 'add 4 5.4)) ; 9.4
  (println (apply-generic 'add 2 (make-complex (make-rect 5 4)))) ; (complex rect 7.0 . 4)
  (println (apply-generic 'add (make-complex (make-rect 5 4)) (make-rat 2 5))) ; (complex rect 5.4 . 4)
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.85                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.85 (install-project install-equ? apply-generic add)
  (import scheme debug)
  (import (only chicken foldr error))
  (import (only ex2.1 numer denom))
  (import (only ex2.83 type-tag contents make-rat make-rect
                       make-complex install-raise raise))
  (import (only ex2.84 install-complex real imag install-add higher-type?
                       get-highest-type successive-raise))
  (title "ex2.85")
  (reset-proc-table)
  (install-raise)
  (install-complex)
  (install-add)

  (define (install-project)
    (put 'project 'rational (lambda (x) (numer x)))
    (put 'project 'real (lambda (x) (make-rat (inexact->exact (round x)) 1)))
    (put 'project 'complex (lambda (x) (real x))))

  (install-project)
  (install-proc project)

  (define (install-equ?)
    (put 'equ? '(integer integer) (lambda (x y) (= x y)))
    (put 'equ? '(rational rational) (lambda (x y) (and (equ? (numer x) (numer y))
                                                       (equ? (denom x) (denom y)))))
    (put 'equ? '(real real) (lambda (x y) (= x y)))
    (put 'equ? '(complex complex) (lambda (x y) (and (equ? (real x) (real y))
                                                     (equ? (imag x) (imag y))))))

  (install-equ?)

  (define (drop x)
    (if (eq? (type-tag x) 'integer)
      x
      (let ((projection (project x)))
        (if (equ? (raise projection) x) (drop projection) x))))

  (define (apply-generic op . args)
    (let* ((type-tags (map type-tag args))
           (highest-type (get-highest-type type-tags))
           (new-args (map (lambda (arg) (successive-raise arg highest-type)) args))
           (proc (get op (map type-tag new-args))))
        (if proc
          (let ((result (apply proc (map contents new-args))))
            (if (or (number? result) (pair? result))
              (drop result)
              result))
          (error "No method for these types" (list op type-tags)))))

  (define (add x y)
    (apply-generic 'add x y))

  (define (equ? x y)
    (apply-generic 'equ? x y))

  (println (add 1 (make-rat 2 1))) ; 3
  (println (add 4 5.0)) ; 9
  (println (add (make-complex (make-rect 5 0)) (make-rat 2 5))) ; 5.4
  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex2.86                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex2.86 ()
  (import scheme debug)
  (import (only ex2.1 numer denom))
  (import (only ex2.83 type-tag contents make-rat make-rect
                       make-pol make-complex install-raise))
  (import (only ex2.85 install-project install-equ? apply-generic))
  (title "ex2.86")
  (reset-proc-table)
  (install-raise)
  (install-project)
  (install-equ?)

  (define (install-complex)
    (define (square x) (mul x x))

    (put 'square-root 'integer (lambda (x) (sqrt x)))
    (put 'square-root 'rational (lambda (x) (sqrt (div (numer x) (denom x)))))
    (put 'square-root 'real (lambda (x) (sqrt x)))

    (put 'sine 'integer (lambda (x) (sin x)))
    (put 'sine 'rational (lambda (x) (sin (div (numer x) (denom x)))))
    (put 'sine 'real (lambda (x) (sin x)))

    (put 'cosine 'integer (lambda (x) (cos x)))
    (put 'cosine 'rational (lambda (x) (cos (div (numer x) (denom x)))))
    (put 'cosine 'real (lambda (x) (cos x)))

    (put 'arctan 'integer (lambda (x) (atan x)))
    (put 'arctan 'rational (lambda (x) (atan (div (numer x) (denom x)))))
    (put 'arctan 'real (lambda (x) (atan x)))

    (put 'real 'rect (lambda (x) (car x)))
    (put 'real 'pol (lambda (x) (mul (car x) (cosine (cdr x)))))
    (put 'imag 'rect (lambda (x) (cdr x)))
    (put 'imag 'pol (lambda (x) (mul (car x) (sine (cdr x)))))
    (put 'mag 'rect (lambda (x) (square-root (add (square (car x)) (square (cdr x))))))
    (put 'mag 'pol (lambda (x) (car x)))
    (put 'ang 'rect (lambda (x) (arctan (div (cdr x) (car x)))))
    (put 'ang 'pol (lambda (x) (cdr x))))

  (install-complex)
  (install-proc square-root)
  (install-proc sine)
  (install-proc cosine)
  (install-proc arctan)
  (install-proc real)
  (install-proc imag)
  (install-proc mag)
  (install-proc ang)

  (define (install-add)
    (put 'add '(integer integer) (lambda (x y) (+ x y)))
    (put 'add '(rational rational) (lambda (x y) (make-rat (add (mul (numer x) (denom y))
                                                                (mul (numer y) (denom x)))
                                                           (mul (denom x) (denom y)))))
    (put 'add '(real real) (lambda (x y) (+ x y)))
    (put 'add '(complex complex) (lambda (x y) (make-complex
                                                 (make-rect (add (real x) (real y))
                                                            (add (imag x) (imag y)))))))

  (define (install-sub)
    (put 'sub '(integer integer) (lambda (x y) (- x y)))
    (put 'sub '(rational rational) (lambda (x y) (make-rat (sub (mul (numer x) (denom y))
                                                                (mul (numer y) (denom x)))
                                                           (mul (denom x) (denom y)))))
    (put 'sub '(real real) (lambda (x y) (- x y)))
    (put 'sub '(complex complex) (lambda (x y) (make-complex
                                                 (make-rect (sub (real x) (real y))
                                                            (sub (imag x) (imag y)))))))

  (define (install-mul)
    (put 'mul '(integer integer) (lambda (x y) (* x y)))
    (put 'mul '(rational rational) (lambda (x y)
                                     (make-rat (mul (numer x) (numer y))
                                               (mul (denom x) (denom y)))))
    (put 'mul '(real real) (lambda (x y) (* x y)))
    (put 'mul '(complex complex) (lambda (x y)
                                   (make-complex
                                     (make-pol (mul (mag x) (mag y))
                                               (add (ang x) (ang y)))))))

  (define (install-div)
    (put 'div '(integer integer) (lambda (x y) (/ x y)))
    (put 'div '(rational rational) (lambda (x y)
                                     (make-rat (mul (numer x) (denom y))
                                               (mul (denom x) (numer y)))))
    (put 'div '(real real) (lambda (x y) (/ x y)))
    (put 'div '(complex complex) (lambda (x y)
                                   (make-complex
                                     (make-pol (div (mag x) (mag y))
                                               (sub (ang x) (ang y)))))))

  (install-add)
  (install-sub)
  (install-mul)
  (install-div)

  (define (add x y)
    (apply-generic 'add x y))

  (define (sub x y)
    (apply-generic 'sub x y))

  (define (mul x y)
    (apply-generic 'mul x y))

  (define (div x y)
    (apply-generic 'div x y))

  (println (mul (make-complex (make-rect (make-rat 3 4) 4)) (make-rat 2 5))) ; (complex pol 1.62788205960997 . 1.3854483767992)
  (println (add (make-complex (make-rect 3 4)) (make-rat 2 5))) ; (complex rect 3.4 . 4)
  (newline))


