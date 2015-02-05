;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.1                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

10                                ; 10
(+ 5 3 4)                         ; 12
(- 9 1)                           ; 8
(/ 6 2)                           ; 3
(+ (* 2 4) (- 4 6))               ; 16
(define a 3)                      ; a
(define b (+ a 1))                ; b
(+ a b (* a b))                   ; 19
(= a b)                           ; #f
(if (and (> b a) (< b (* a b)))
    b
    a)                            ; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))                  ; 16
(+ 2 (if (> b a) b a))            ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                       ; 16


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.2                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.3                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sos-max-two a b c)
  (define (sos x y) (+ (square x) (square y)))
  (cond
  ((= (min a b c) a) (sos b c))
  ((= (min a b c) b) (sos a c))
  ((= (min a b c) c) (sos a b))))

(sos-max-two 3 2 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.4                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; If b is +ve, the operator evaluates to +, otherwise, it evaluates to -,
; effectively adding the absolute value of b to a.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.5                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; If the interpreter uses applicative-order evaluation, the combination
; (test 0 (p)) will hang trying to evaluate the infinitely-recursive (p)
; combination. If the interpreter uses normal-order evluation, the combination
; will evaluate to 0 because the second argument to test is not going to be
; needed to evaluate the body of test.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.6                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The program will run forever because scheme uses applicative-order evaluation,
; so, evaluating new-if implies evaluating all its operands before substitution
; regardless of whether the condition evaluates to true or false. The problem is
; that one of new-if operands is actually a recursive call to sqrt-iter which in
; turn will invoke new-if again, and so on and so forth forever.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.7                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; good-enough? is not suitable for very small numbers because the difference
; between x and the square of the guess would fall behind 0.001 before the
; procedure arrives at the correct square root of x. The procedure will fail for
; very large numbers if they do not fit in machine registers because at some
; point, they'll be truncated and as a result, good-enough? would return a value
; less than 0.001 before guess is really close to the square root of x.

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt-book x)
  (sqrt-iter 1.0 x))

; examples:
; small numbers:
(square (sqrt-book 0.000001)) ; 9.772285838805523e-4 ≈ 1e-3 which is clearly
                              ; wrong.

; large numbers: With 64-bit precision, numbers that would cause such issues are
;                too big that sqrt-book will never finish.
;
; New sqrt:

(define (sqrt-new x)
  (define (average x y)
    (/ (+ x y) 2))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (good-enough? guess old-guess)
    (< (/ (abs (- guess old-guess)) guess) 0.001))
  (define (sqrt-iter guess old-guess x)
    (if (good-enough? guess old-guess)
      guess
      (sqrt-iter (improve guess x) guess x)))
  (sqrt-iter 1.0 10.0 x))

(square (sqrt-new 0.000001)) ; 1.0000003066033492e-6 ≈ 1e-6 which is correct.

; as seen, sqrt-new works better for small numbers. For large numbers, the
; precision problem still persists, so their should be no improvement.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.8                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube-root x)
  (define (improve guess x)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))
  (define (good-enough? guess old-guess)
    (< (/ (abs (- guess old-guess)) guess) 0.0001))
  (define (cube-root-iter guess old-guess x)
    (if (good-enough? guess old-guess)
      guess
      (cube-root-iter (improve guess x) guess x)))
  (cube-root-iter 1.0 10.0 x))

(cube-root 27.0)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.9                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
;
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
;
; first is is recursive and second one is iterative.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.10                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (define (A x y)
;   (cond ((= y 0) 0)
;         ((= x 0) (* 2 y))
;         ((= y 1) 2)
;         (else (A (- x 1)
;                  (A x (- y 1))))))
;
; (A 1 10) = 1024
;
; (A 2 4) = 65536
;
; (A 3 3) = 65536
;
; (define (f n) (A 0 n)) = 2n
;
; (define (g n) (A 1 n)) = 2^n
;
; (define (h n) (A 2 n)) = 2^^n (i.e. 2^2^2^... n-times)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.11                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (f1-recursive n)
  (if (< n 3)
    n
    (+ (f1-recursive (- n 1))
       (* 2 (f1-recursive (- n 2)))
       (* 3 (f1-recursive (- n 3))))))

(define (f1-iterative n)
  (define (f1-iter a b c n)
    (if (= n 0)
      a
      (f1-iter b c (+ c (* 2 b) (* 3 a)) (- n 1))))
  (f1-iter 0 1 2 n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.12                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; the question is not very clear, so I created a function to return the n'th row
; of the pascal triangle as a list, starting at 0 = (1).

(define (pascal n)
  (if (= n 0)
    (list 1)
    (let ((pn-1 (pascal (- n 1))))
      (map + (cons 0 pn-1) (append pn-1 (list 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.13 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Prove that: fib(n) = round(phi^n / sqrt(5))
;
; By induction:
;
; Base case: n = 0
;   fib(0) = 0
;   round(phi^0 / sqrt(5)) = round(0.44) = 0
;
; Hypothesis: fib(n) = round(phi^n / sqrt(5))
;
; Prove that: fib(n+1) = round(phi^(n+1) / sqrt(5))
;
; Proof:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.14                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (count-change 11)
;  |
;  `-> (cc 11 5)
;       |-> (cc -39 5) -> 0
;       `-> (cc 11 4)
;            |-> (cc -14 4) -> 0
;            `-> (cc 11 3)
;                 |-> (cc 1 3)
;                 |    |-> (cc -9 3) -> 0
;                 |    `-> (cc 1 2)
;                 |         |-> (cc -4 2) -> 0
;                 |         `-> (cc 1 1)
;                 |              |-> (cc 0 1) -> 1
;                 |              `-> (cc 1 0) -> 0
;                 `-> (cc 11 2)
;                      |-> (cc 6 2)
;                      |    |-> (cc 1 2)
;                      |    |    |-> (cc -4 2) -> 0
;                      |    |    |-> (cc 1 1)
;                      |    |         |-> (cc 0 1) -> 1
;                      |    |         |-> (cc 1 0) -> 0
;                      |    `-> (cc 6 1)
;                      |         |-> (cc 5 1)
;                      |         |    |-> (cc 4 1)
;                      |         |    |    |-> (cc 3 1)
;                      |         |    |    |    |-> (cc 2 1)
;                      |         |    |    |    |    |-> (cc 1 1)
;                      |         |    |    |    |    |    |-> (cc 0 1) -> 1
;                      |         |    |    |    |    |    `-> (cc 1 0) -> 0
;                      |         |    |    |    |    `-> (cc 2 0) -> 0
;                      |         |    |    |    `-> (cc 3 0) -> 0
;                      |         |    |    `-> (cc 4 0) -> 0
;                      |         |    `-> (cc 5 0) -> 0
;                      |         `-> (cc 6 0) -> 0
;                      `-> (cc 11 1)
;                           |-> (cc 10 1)
;                           |    |-> (cc 9 1)
;                           |    |    |-> (cc 8 1)
;                           |    |    |    |-> (cc 7 1)
;                           |    |    |    |    |-> (cc 6 1)
;                           |    |    |    |    |    |-> (cc 5 1)
;                           |    |    |    |    |    |    |-> (cc 4 1)
;                           |    |    |    |    |    |    |    |-> (cc 3 1)
;                           |    |    |    |    |    |    |    |    |-> (cc 2 1)
;                           |    |    |    |    |    |    |    |    |    |-> (cc 1 1)
;                           |    |    |    |    |    |    |    |    |    |    |-> (cc 0 1) -> 1
;                           |    |    |    |    |    |    |    |    |    |    `-> (cc 1 0) -> 0
;                           |    |    |    |    |    |    |    |    |    `-> (cc 2 0) -> 0
;                           |    |    |    |    |    |    |    |    `-> (cc 3 0) -> 0
;                           |    |    |    |    |    |    |    `-> (cc 4 0) -> 0
;                           |    |    |    |    |    |    `-> (cc 5 0) -> 0
;                           |    |    |    |    |    `-> (cc 6 0) -> 0
;                           |    |    |    |    `-> (cc 7 0) -> 0
;                           |    |    |    `-> (cc 8 0) -> 0
;                           |    |    `-> (cc 9 0) -> 0
;                           |    `-> (cc 10 0) -> 0
;                           `-> (cc 11 0) -> 0
;
; Steps = theta(n) ; hesitant about this answer
; Space = theta(n)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.15                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; a) 5 times.
; b) Steps = theta(log3(a))
;    Space = theta(log3(a))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.16                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-expt b n)
  (define (fast-expt-iter a b n)
    (cond ((= n 0) a)
      ((even? n) (fast-expt-iter (* a (square b)) b (/ n 2)))
      (else (fast-expt-iter (* a b) b (- n 1)))))
  (fast-expt-iter 1 b n))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.17                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-times-recursive a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (cond ((= b 0) 0)
        ((even? b) (double (fast-times-recursive a (halve b))))
        (else (+ a (fast-times-recursive a (- b 1))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.18                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fast-times-iterative a b)
  (define (double x) (+ x x))
  (define (halve x) (/ x 2))
  (define (fast-times-iter a b c)
    (cond ((= b 1) (+ a c))
      ((even? b) (fast-times-iter (double a) (halve b) c))
      (else (fast-times-iter a (- b 1) (+ c a)))))
  (fast-times-iter a b 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.19                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Tpq: a <- bq + aq + ap
;      b <- bp + aq
;
; Tp'q': a <- (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;        b <- (bp + aq)p + (bq + aq + ap)q
;
; Tp'q': a <- b(q^2 + 2pq) + a(q^2 + 2pq) + a(p^2 + q^2)
;        b <- b(p^2 + q^2) + a(q^2 + 2pq)
;
; Therefore, p' = p^2 + q^2
;            q' = q^2 + 2pq

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q)  (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.20 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Normal-order evaluation:
;
;  (gcd 206 40)
; -> (if (= 40 0)
;      206
;      (if (= (remainder 206 40) 0)
;        40
;        (if (= ((remainder 40 (remainder 206 40)) 0)
;          (remainder 206 40)
;          (gcd (remainder 40 (remainder 206 40))
;               (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
;        )
;      )
;    )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.21                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 199)   ; 199
(smallest-divisor 1999)  ; 1999
(smallest-divisor 19999) ; 7


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.22                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (let () (report-prime (- (runtime) start-time)) #t) #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes-odd minimum count)
  (if (> count 0)
    (if (not (timed-prime-test minimum))
        (search-for-primes minimum count)
        (search-for-primes minimum (-1+ count)))))

(define (search-for-primes minimum count)
  (if (even? minimum)
      (search-for-primes-odd (1+ minimum) count)
      (search-for-primes-odd (+ minimum 2) count)))

(search-for-primes 10000000 3)
;  10000019 *** 1.0000000000000009e-2
;  10000079 *** 0.
;  10000103 *** 1.0000000000000009e-2
(search-for-primes 100000000 3)
;  100000007 *** .01999999999999999
;  100000037 *** 2.0000000000000018e-2
;  100000039 *** 1.9999999999999962e-2
(search-for-primes 1000000000 3)
;  1000000007 *** .07
;  1000000009 *** .07
;  1000000021 *** .06
(search-for-primes 10000000000 3)
;  10000000019 *** .20999999999999996
;  10000000033 *** .21000000000000008
;  10000000061 *** .20999999999999996

; I pumped the numbers a bit because on modern machines, the example numbers all
; give 0 runtime. Numbers are close to what's expected. The results are
; approximately sqrt(10) multiples of each other. Of course there are other
; factors affecting the program runtime, but it still approximately reflect the
; number of steps taken by the algorithm.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.23                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (define (next n)
    (if (= n 2) 3 (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))
;
(search-for-primes 10000000 3)
;  10000019 *** 1.0000000000000009e-2
;  10000079 *** 1.0000000000000009e-2
;  10000103 *** 0.
(search-for-primes 100000000 3)
;  100000007 *** .01999999999999999
;  100000037 *** 1.0000000000000009e-2
;  100000039 *** 2.0000000000000018e-2
(search-for-primes 1000000000 3)
;  1000000007 *** .03999999999999998
;  1000000009 *** .03999999999999998
;  1000000021 *** .03999999999999998
(search-for-primes 10000000000 3)
;  10000000019 *** .13
;  10000000033 *** .13
;  10000000061 *** .13

; It's clear from the large numbers that the runtime has decreased, but not
; exactly by a factor of 2 (more like 1.54). This is because halving the time
; spent in 'smallest-divisor' doesn't mean halving the full runtime of the
; program. 'report-prime' for example consumes the same amount of time as
; before.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.24                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 300)
      (let () (report-prime (- (runtime) start-time)) #t) #f))

(search-for-primes 10000000 3)
;  10000019 *** .03
;  10000079 *** .02999999999999997
;  10000103 *** 3.0000000000000027e-2
(search-for-primes 100000000 3)
;  100000007 *** .02999999999999997
;  100000037 *** 3.0000000000000027e-2
;  100000039 *** .02999999999999997
(search-for-primes 1000000000 3)
;  1000000007 *** 4.0000000000000036e-2
;  1000000009 *** .02999999999999997
;  1000000021 *** 4.0000000000000036e-2
(search-for-primes 10000000000 3)
;  10000000019 *** 4.0000000000000036e-2
;  10000000033 *** 3.9999999999999925e-2
;  10000000061 *** 4.0000000000000036e-2

; The runtime depends mostly on the number of times to try fast-prime?. The
; numbers above are for a number of times equals to 300. The interesting thing
; is that the starting value bares almost no significance in the resulting
; runtime. This is attributed to the fact that regardless of the starting value,
; the test is always tried 300 times only for each value, as opposed to a number
; of times proportional to n.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.25                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; No, because the original expmod does the remainder operation as it recurses,
; thus reducing the value of the exponent (which is the main factor in the
; algorithm run time). In her version though, the exponent is calculated first
; using very large numbers, taking extremely long run time, and then the
; remainder operation takes place.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.26                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Because (expmod base (/ exp 2) m) is now evaluated twice everytime expmod is
; called with an even number, thus cancelling out the runtime halving effect,
; and thus defeating the whole purpose of writing the algorithm this way in the
; first place.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.27                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-sub-n-pass-fermat-test? n)
  (all-sub-n-pass-fermat-test-iter? (-1+ n) n))
(define (all-sub-n-pass-fermat-test-iter? a n)
  (cond
    ((= a 1) #t)
    ((not (= a (expmod a n n))) #f)
    (else (all-sub-n-pass-fermat-test-iter? (-1+ a) n))))

(all-sub-n-pass-fermat-test? 561)  ; #t
(all-sub-n-pass-fermat-test? 1105) ; #t
(all-sub-n-pass-fermat-test? 1729) ; #t
(all-sub-n-pass-fermat-test? 2465) ; #t
(all-sub-n-pass-fermat-test? 2821) ; #t
(all-sub-n-pass-fermat-test? 6601) ; #t


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.28                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
          (let ((num (expmod base (/ exp 2) m)))
            (let ((rem (remainder (square num) m)))
              (if (and (not (or (= num 1) (= num (-1+ m)))) (= rem 1))
                0
                rem))))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod a (-1+ n) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(miller-rabin-test 31)   ; #t
(miller-rabin-test 59)   ; #t
(miller-rabin-test 60)   ; #f
(miller-rabin-test 561)  ; #f
(miller-rabin-test 1105) ; #f
(miller-rabin-test 1729) ; #f
(miller-rabin-test 2465) ; #f
(miller-rabin-test 2821) ; #f
(miller-rabin-test 6601) ; #f


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.29                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (let
        ((coefficient
          (cond
            ((or (= k 0) (= k n)) 1)
            ((even? k) 2)
            (else 4))))
        (* coefficient (f (+ a (* k h))))))
    (* (/ h 3) (sum term 0 1+ n))))

(simpson cube 0.0 1 100)  ; 0.24999999999999992
(simpson cube 0.0 1 1000) ; 0.2500000000000003

; The values are a lot more accurate than the method described earlier in the
; book.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.30                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.31                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part (a):

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (factorial n)
  (define (id a) a)
  (product id 1 1+ n))

(define (calc-pi n)
  (define (add-two a) (+ a 2))
  (* 8.0 n (/ (product square 4 add-two (- n 1))
              (product square 3 add-two n))))

; Part (b):

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.32                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part (a):

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

; Part (b):

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.33                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (filtered-accumulate combiner null-value predicate? term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (if (predicate? a)
                (combiner (term a) result)
                result))))
  (iter a null-value))

; Part (a):

(define (sum-square-primes a b)
  (define (square x) (* x x))
  (filtered-accumulate + 0 prime? square a 1+ b))

; Part (b):

(define (sum-relatively-prime n)
  (define (id a) a)
  (define (relatively-prime-to-n? a)
    (= (gcd a n) 1))
  (filtered-accumulate + 0 relatively-prime-to-n? id 1 1+ (-1+ n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.34                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; It will give an error because (f f) evaluates to (f 2) which in turn evalutes
; to (2 2). Since 2 is not a procedure, the interpreter will issue an error when
; trying to evaluate this last combination.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.35                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; By definition, the golden ratio is that value that satisfies phi^2 = phi + 1.
; Multiplying he transformation x -> 1 + 1/x by x on both sides yields the same
; equation.

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (phi-transformation x) (+ 1 (/ 1 x)))

(define phi (fixed-point phi-transformation 1.0))

phi


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.36                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fixed-point-display f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (x-transformation x) (/ (log 1000) (log x)))

(define x (fixed-point-display x-transformation 2.0))

(define (x-average-damping-transformation x) (/ (+ x (/ (log 1000) (log x))) 2))

(define x (fixed-point-display x-average-damping-transformation 2.0))

; Average damping takes a lot less steps.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.37                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Part (a):

(define (cont-frac n d k)
  (define (cont-frac-helper i n d k)
    (/ (n i) (if (= i k)
                 (d i)
                 (+ (d i) (cont-frac-helper (1+ i) n d k)))))
  (cont-frac-helper 1 n d k))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11) ; .6180555555555556

; k = 11 for 4-decimal places approximation.
;
; Part (b):

(define (cont-frac n d k)
  (define (cont-frac-iter n d i result)
    (cond
      ((= i 0) result)
      (else (cont-frac-iter n d (-1+ i) (/ (n i) (+ (d i) result))))))
  (cont-frac-iter n d (-1+ k) (/ (n k) (d k))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.38                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (n-e i) 1.0)

(define (d-e i)
  (cond
    ((< i 3) i)
    ((and (= (d-e (- i 1)) 1) (= (d-e (- i 2)) 1))
        (+ (d-e (- i 1)) (d-e (- i 2)) (d-e (- i 3))))
    (else 1)))

(define e
  (+ 2 (cont-frac n-e d-e 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.39                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (tan-cf x k)
  (define (n i) (if (= i 1) x (- (* x x))))
  (define (d i) (-1+ (* 2 i)))
  (cont-frac n d k))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.40                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.41                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (double f)
  (lambda (x) (f (f x))))

(((double (double double)) 1+) 5) ; 21

; This is equivalent to ((double double) ((double double) 1+)) which is
; equivalent to (quaruple (quadruple 1+)) which adds 16.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.42                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compose f g)
  (lambda (x) (f (g x))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.43                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repeated f n)
  (if (= n 1) f (compose f (repeated f (-1+ n)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.44                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx))) 3)))

(define (nsmooth f n)
  (lambda (x) (((repeated smooth n) f) x)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.45                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (nth-root n x)
  (fixed-point ((repeated average-damp (floor (/ (log n) (log 2))))
                  (lambda (y) (/ x (expt y (-1+ n)))))
               1.0))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex1.46                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (let ((next (improve guess)))
      (if (good-enough? guess next)
        guess
        (iter next))))
  iter)

(define (sqrt-book x)
  (define (improve guess)
    (average-damp (lambda (y) (/ 25 y))) guess)
  ((iterative-improve good-enough? improve) 1.0))

(define (fixed-point f guess)
  (define (good-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001))
  ((iterative-improve good-enough? f) guess))

(sqrt-book 25) ; 5.000000000053722

(fixed-point (average-damp (lambda (y) (/ 25 y))) 1.0) ; 5.000000000053722


