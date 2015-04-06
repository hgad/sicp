(module debug (newlines title println id put get put-coercion get-coercion
               reset-proc-table reset-coercion-table install-proc install-proc2)
  (import scheme)
  (import (only chicken sub1))

  (define (newlines n)
    (if (not (zero? n)) (let () (newline) (newlines (sub1 n)))))

  (define (title str)
    (newline)
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
;; ex3.1                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.1 ()
  (import scheme debug)
  (title "ex3.1")

  (define (make-accumulator sum)
    (lambda (value)
      (set! sum (+ sum value))
      sum))

  (define a (make-accumulator 5))
  (define b (make-accumulator 10))
  (println (a 10))
  (println (b 10))
  (println (a 10))
  (println (b 10)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.2                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.2 ()
  (import scheme debug)
  (import (only chicken add1))
  (title "ex3.2")

  (define (make-monitored f)
    (let ((count 0))
      (lambda (input)
        (define (how-many-calls?) count)
        (define (reset-count) (set! count 0) count)
        (cond
          ((and (symbol? input) (eq? input 'how-many-calls?)) (how-many-calls?))
          ((and (symbol? input) (eq? input 'reset-count)) (reset-count))
          (else (set! count (add1 count)) (f input))))))

  (define msqrt (make-monitored sqrt))
  (println (msqrt 10))
  (println (msqrt 'how-many-calls?))
  (println (msqrt 100))
  (println (msqrt 'how-many-calls?))
  (println (msqrt 'reset-count))
  (println (msqrt 'how-many-calls?)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.3                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.3 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.3")

  (define (make-account balance password)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (incorrect-password amount)
      "Incorrect password")

    (define (dispatch pass m)
      (if (and (symbol? pass) (eq? pass password))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        incorrect-password))
    dispatch)

  (define acc (make-account 100 'secret-password))
  (println ((acc 'secret-password 'withdraw) 40))
  (println ((acc 'some-other-password 'deposit) 50))
  (println ((acc 'secret-password 'deposit) 30)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.4                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.4 ()
  (import scheme debug)
  (import (only chicken sub1 error))
  (title "ex3.4")

  (define (make-account balance password)
    (define allowed-trials 7)
    (define trials allowed-trials)

    (define (reset-trials)
      (set! trials allowed-trials))

    (define (withdraw amount)
      (reset-trials)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                balance)
          "Insufficient funds"))

    (define (deposit amount)
      (reset-trials)
      (set! balance (+ balance amount))
      balance)

    (define (call-the-cops)
      "Dialing 911 ...")

    (define (incorrect-password amount)
      (set! trials (sub1 trials))
      (if (<= trials 0)
        (call-the-cops)
        "Incorrect password"))

    (define (dispatch pass m)
      (if (and (symbol? pass) (eq? pass password))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        incorrect-password))
    dispatch)

  (define acc (make-account 100 'secret-password))
  (println ((acc 'secret-password 'deposit) 40))
  (println ((acc 'some-other-password1 'withdraw) 50))
  (println ((acc 'some-other-password2 'withdraw) 50))
  (println ((acc 'some-other-password3 'withdraw) 50))
  (println ((acc 'secret-password 'withdraw) 50))

  (println ((acc 'some-other-password1 'withdraw) 50))
  (println ((acc 'some-other-password2 'withdraw) 50))
  (println ((acc 'some-other-password3 'withdraw) 50))
  (println ((acc 'some-other-password4 'withdraw) 50))
  (println ((acc 'some-other-password5 'withdraw) 50))
  (println ((acc 'some-other-password6 'withdraw) 50))
  (println ((acc 'some-other-password7 'withdraw) 50))
  (println ((acc 'some-other-password8 'withdraw) 50))

  (println ((acc 'secret-password 'deposit) 30)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.5                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.5 ()
  (import scheme extras debug)
  (title "ex3.5")

  (define (random-flonum value)
    (/ (random (inexact->exact (* value 1000000))) 1000000))

  (define (random-in-range low high)
    (let ((range (- high low)))
      (+ low (random-flonum range))))

  (define (monte-carlo trials experiment)
    (define (iter trials-remaining trials-passed)
      (cond ((= trials-remaining 0)
             (/ trials-passed trials))
            ((experiment)
             (iter (- trials-remaining 1) (+ trials-passed 1)))
            (else
             (iter (- trials-remaining 1) trials-passed))))
    (iter trials 0))

  (define (estimate-integral p x1 x2 y1 y2 trials)
    (* (monte-carlo trials p) (* (- x2 x1) (- y2 y1))))

  (define (square value)
    (* value value))

  (define (in-unit-circle)
    (let ((x (random-in-range -1.0 1.0))
          (y (random-in-range -1.0 1.0)))
      (<= (+ (square x) (square y)) 1.0)))

  (define pi-estimate
    (estimate-integral in-unit-circle -1.0 1.0 -1.0 1.0 10000))

  (println pi-estimate))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.6                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.6 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.6")

  (define (rand-factory seed-value)
    (define seed seed-value)

    (lambda (input)
      (define (generate)
        (set! seed (remainder (+ (* 223 seed) 189) 1000000))
        seed)

      (define (reset value)
        (set! seed value)
        seed)

      (cond
        ((and (symbol? input) (eq? input 'generate)) (generate))
        ((and (symbol? input) (eq? input 'reset)) reset)
        (else error "Invalid message -- RAND" input))))

  (define rand (rand-factory 1))

  ((rand 'reset) 50)
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (newline)
  ((rand 'reset) 50)
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate))
  (println (rand 'generate)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.7                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.7 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.7")

  (define (make-account balance password)
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                balance)
          "Insufficient funds"))

    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)

    (define (incorrect-password amount)
      "Incorrect password")

    (define (dispatch pass m)
      (if (and (symbol? pass) (eq? pass password))
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        incorrect-password))
    dispatch)

  (define (make-joint acc acc-passwd joint-acc-passwd)
    (define (incorrect-password amount)
      "Incorrect password")

    (lambda (password action)
      (if (and (symbol? password) (eq? password joint-acc-passwd))
        (acc acc-passwd action)
        incorrect-password)))

  (define peter-acc (make-account 100 'open-sesame))
  (define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

  (println ((peter-acc 'open-sesame 'withdraw) 40))
  (println ((paul-acc 'rosebud 'deposit) 50))
  (println ((peter-acc 'rosebud 'withdraw) 30))
  (println ((paul-acc 'open-sesame 'withdraw) 30))
  (println ((paul-acc 'rosebud 'withdraw) 50))
  (println ((peter-acc 'open-sesame 'deposit) 40)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.8                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.8 ()
  (import scheme debug)
  (title "ex3.8")

  (define (factory state)
    (lambda (value)
      (let ((old-state state))
        (set! state (if (zero? value) 0 1))
        old-state)))

  (define f (factory 0))
  (println (+ (f 0) (f 1)))

  (define g (factory 0))
  (println (+ (g 1) (g 0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.9                                                                      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Version 1:
;
;          +-------------------------------------------------------------------+
;          |                                                                   |
; global ->|                                                                   |
;  env     |                                                                   |
;          +-------------------------------------------------------------------+
;             ^           ^           ^           ^           ^           ^
;             |           |           |           |           |           |
;        +---------+ +---------+ +---------+ +---------+ +---------+ +---------+
;        |  n : 6  | |  n : 5  | |  n : 4  | |  n : 3  | |  n : 2  | |  n : 1  |
;        +---------+ +---------+ +---------+ +---------+ +---------+ +---------+
;        (* n        (* n        (* n        (* n        (* n             1
;         (factorial  (factorial  (factorial  (factorial  (factorial
;          (- n 1)))   (- n 1)))   (- n 1)))   (- n 1)))   (- n 1)))
;
;
; Version 2:
;
;          +----------------------------------------------------------------------------------------------------------------+
;          |                                                                                                                |
; global ->|                                                                                                                |
;  env     |                                                                                                                |
;          +----------------------------------------------------------------------------------------------------------------+
;             ^                ^                   ^                    ^                    ^                    ^  ^  ^
;             |                |                   |                    |                    |                    |  |  |
;             |                |                   |                    |                    |                    |  |  +---------+
;             |                |                   |                    |                    |                    |  |            |
;             |                |                   |                    |                    |                    |  +---------+  |
;             |                |                   |                    |                    |                    |            |  |
;        +---------+ +------------------+ +------------------+ +------------------+ +------------------+ +------------------+  |  |
;        |  n : 6  | |  product   : 1   | |  product   : 1   | |  product   : 2   | |  product   : 6   | |  product   : 24  |  |  |
;        +---------+ |  counter   : 1   | |  counter   : 2   | |  counter   : 3   | |  counter   : 4   | |  counter   : 5   |  |  |
;        (fact-iter  |  max-count : 6   | |  max-count : 6   | |  max-count : 6   | |  max-count : 6   | |  max-count : 6   |  |  |
;          1 1 n)    +------------------+ +------------------+ +------------------+ +------------------+ +------------------+  |  |
;                    (fact-iter           (fact-iter           (fact-iter           (fact-iter           (fact-iter            |  |
;                     (* counter product)  (* counter product)  (* counter product)  (* counter product)  (* counter product)  |  |
;                     (+ counter 1)        (+ counter 1)        (+ counter 1)        (+ counter 1)        (+ counter 1)        |  |
;                     max-count)           max-count)           max-count)           max-count)           max-count)           |  |
;                                                                                                                              |  |
;                              +-----------------------------------------------------------------------------------------------+  |
;                              |                                                                                                  |
;                              |                   +------------------------------------------------------------------------------+
;                              |                   |
;                    +------------------+ +------------------+
;                    |  product   : 120 | |  product   : 720 |
;                    |  counter   : 6   | |  counter   : 7   |
;                    |  max-count : 6   | |  max-count : 6   |
;                    +------------------+ +------------------+
;                    (fact-iter                 product
;                     (* counter product)
;                     (+ counter 1)
;                     max-count)
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.10                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Defining make-withdraw adds a symbol to the global environment and binds it to
; the body of make-withdraw (shown after desugaring):
;
;          +---------------------------------------+
;          |                                       |
; global ->| make-withdraw:--+                     |
;  env     |                 |                     |
;          +-----------------|---------------------+
;                            |       ^
;                            v       |
;                       +---------+  |
;                       |  * | *--|--+
;                       +--|------+
;                          |
;                          v
;    parameters: initial-amount
;          body: ((lambda (balance)
;                  (lambda (amount)
;                    (if (>= balance amount)
;                        (begin (set! balance (- balance amount))
;                              balance)
;                        "Insufficient funds"))) initial-amount)
;
;
; Calling make-withdraw with argument 100 executes the body of make-withdraw
; in an environment binding initial-amount to 100 whose enclosing environment
; is the global environment.
;
;
;          +-----------------------------------------------------------------+
;          |                                                                 |
; global ->| make-withdraw:---------------------------------------+          |
;  env     |                                                      |          |
;          +------------------------------------------------------|----------+
;                        ^                                        |       ^
;                        |                                        v       |
;            +----------------------+                        +---------+  |
;      E1 -->| initial-amount : 100 |                        |  * | *--|--+
;            +----------------------+                        +--|------+
; ((lambda (balance)                                            |
;   (lambda (amount)                                            v
;     (if (>= balance amount)                         parameters: initial-amount
;         (begin (set! balance (- balance amount))          body: ...
;               balance)
;         "Insufficient funds"))) initial-amount)
;
;
; Evaluating the first part of the combination creates a new un-named procedure
; with parameter balance whose environment is E1.
;
;          +-----------------------------------------------------------------+
;          |                                                                 |
; global ->| make-withdraw:---------------------------------------+          |
;  env     |                                                      |          |
;          +------------------------------------------------------|----------+
;                                      ^                          |       ^
;                                      |                          v       |
;                           +----------------------+         +---------+  |
;                     E1 -->| initial-amount : 100 |         |  * | *--|--+
;                           +----------------------+         +--|------+
;                                      ^                        |
;            +---------+               |                        v
;            |  * | *--|---------------+              parameters: initial-amount
;            +--|------+                                    body: ...
;               |
;               v
;     parameters: balance
;           body:
;             (lambda (amount)
;               (if (>= balance amount)
;                   (begin (set! balance (- balance amount))
;                         balance)
;                   "Insufficient funds"))
;
;
; Evaluating the second part of the combination just retrieves initial-amount
; from E1. Evaluating the combination applies the newly created un-named
; procedure to initial-amount, thus binding balance to the value of
; initial-amount in a new environment E2 (I'll omit E1 & the un-named procedure
; to make space for E2):
;
;
;          +-----------------------------------------------------------------+
;          |                                                                 |
; global ->| make-withdraw:---------------------------------------+          |
;  env     |                                                      |          |
;          +------------------------------------------------------|----------+
;                        ^                                        |       ^
;                        |                                        v       |
;            +--------------------+                          +---------+  |
;      E2 -->|   balance : 100    |                          |  * | *--|--+
;            +--------------------+                          +--|------+
;   (lambda (amount)                                            v
;     (if (>= balance amount)                         parameters: initial-amount
;         (begin (set! balance (- balance amount))          body: ...
;               balance)
;         "Insufficient funds"))
;
;
; define binds the result (which is a lambda expression) to a global env
; variable W2 (since define is called in the global environment):
;
;
;          +-----------------------------------------------------------------+
;          |                                                                 |
; global ->| make-withdraw:---------------------------------------+          |
;  env     |                                                      |          |
;          | W1:--+                                               |          |
;          |      |                                               |          |
;          +------|-----------------------------------------------|----------+
;                 |                    ^                          |     ^
;                 |                    |                          v     |
;                 |            +---------------+         +---------+    |
;                 |      E2 -->| balance : 100 |         |  * | *--|----+
;                 |            +---------------+         +--|------+
;                 |                    ^                    v
;                 v                    |              parameters: initial-amount
;            +---------+               |                    body: ...
;            |  * | *--|---------------+
;            +--|------+
;               |
;               v
;     parameters: amount
;           body:
;             (if (>= balance amount)
;                 (begin (set! balance (- balance amount))
;                        balance)
;                 "Insufficient funds")
;
;
; Calling (W1 50) evaluates the body of W1 in a new environment E3 that binds
; amount to 50 and whose enclosing environment is E2:
;
;
;          +---------------------------------------------------------------------------------+
;          |                                                                                 |
; global ->| make-withdraw:-------------------------------------------------------+          |
;  env     |                                                                      |          |
;          | W1:------------------+                                               |          |
;          |                      |                                               |          |
;          +----------------------|-----------------------------------------------|----------+
;                                 |                    ^                          |     ^
;                                 |                    |                          v     |
;                                 |            +---------------+         +---------+    |
;                                 |      E2 -->| balance : 100 |         |  * | *--|----+
;                                 |            +---------------+         +--|------+
;                                 |               ^      ^                  v
;                                 v               |      |        parameters: initial-amount
;                            +---------+          |      |              body: ...
;                            |  * | *--|----------+      |
;                            +--|------+                 |
;                               |                        |
;                               v                        |
;                     parameters: amount                 |
;                           body: ...                    |
;                                                        |
;            +--------------------+                      |
;      E3 -->|    amount : 50     |----------------------+
;            +--------------------+
;        (if (>= balance amount)
;            (begin (set! balance (- balance amount))
;                   balance)
;            "Insufficient funds")
;
;
; Since balance is indeed greater than amount, the set! gets executed and the
; new balance returned.
;
; Defining W2 follows the same steps and results in a new procedure which has
; the same body as that of W1 but point to a new environment E4 with a separate
; binding of balance:
;
;
;          +---------------------------------------------------------------------------------+
;          |                                                                                 |
; global ->| make-withdraw:-------------------------------------------------------+          |
;  env     |                                                                      |          |
;          | W1:------------------+                                               |          |
;          |                      |                                               |          |
;          | W2:----+             |                                               |          |
;          |        |             |                                               |          |
;          +--------|-------------|-----------------------------------------------|----------+
;            ^      |             |                    ^                          |     ^
;            |      |             |                    |                          v     |
;      +-----+      v             |            +---------------+         +---------+    |
;      |       +---------+        |      E2 -->| balance : 100 |         |  * | *--|----+
;      |       |  * | *  |        |            +---------------+         +--|------+
;      |       +--|---|--+        |               ^                         v
;      |          |   |           v               |               parameters: initial-amount
;+-------------+  |   |      +---------+          |                     body: ...
;| balance:100 |<-+   |      |  * | *--|----------+
;+-------------+      |      +--|------+
;      ^              |         |
;      |              v         v
;      E4             parameters: amount
;                           body:
;                             (if (>= balance amount)
;                                 (begin (set! balance (- balance amount))
;                                        balance)
;                                 "Insufficient funds")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.11                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (define acc (make-account 50)):
;
;          +--------------------------------------------------------------------+
;          |                                                                    |
; global ->| make-account:--+                                                   |
;  env     |                |                                                   |
;          | acc:-----------|-----------------+                                 |
;          |                |                 |                                 |
;          +----------------|-----------------|---------------------------------+
;                           |       ^         |                   ^
;                           v       |         |                   |
;                      +---------+  |         |       +-----------------------+
;                      |  * | *--|--+         |       | balance: 50           |
;                      +--|------+            |       | dispatch:---+         |
;                         |                   | E1 -->| withdraw:...|         |
;                         v                   |       | deposit:... |         |
;      parameters: balance                    |       +-------------|---------+
;            body:                            |                     |      ^
;              (define (withdraw amount) ...) |                     v      |
;              (define (deposit amount) ...)  |                +---------+ |
;              (define (dispatch m) ...)      +--------------->|  * | *--|-+
;                                                              +--|------+
;                                                                 v
;                                                         parameters: m
;                                                               body:
;                                                                 (cond ...)
; ((acc 'deposit) 40):
;
;          +--------------------------------------------------------------------+
;          |                                                                    |
; global ->| make-account:--+                                                   |
;  env     |                |                                                   |
;          | acc:-----------|-----------------+                                 |
;          |                |                 |                                 |
;          +----------------|-----------------|---------------------------------+
;                           |       ^         |                   ^
;                           v       |         |                   |
;                      +---------+  |         |       +-----------------------------------------------------------------+
;                      |  * | *--|--+         |       | balance: 50                                                     |
;                      +--|------+            |       | dispatch:---+                                                   |
;                         |                   | E1 -->| withdraw:...|                                                   |
;                         v                   |       | deposit:... |                                                   |
;      parameters: balance                    |       +-------------|---------------------------------------------------+
;            body:                            |                     |      ^            ^                       ^
;              (define (withdraw amount) ...) |                     v      |            |                       |
;              (define (deposit amount) ...)  |                +---------+ |    +-------------+         +------------+
;              (define (dispatch m) ...)      +--------------->|  * | *--|-+    | m: 'deposit |<-- E2   | amount: 40 |<-- E3
;                                                              +--|------+      +-------------+         +------------+
;                                                                 v               (cond ...)             (set! balance ...)
;                                                         parameters: m
;                                                               body:
;                                                                 (cond ...)
; ((acc 'withdraw) 60):
;
;          +--------------------------------------------------------------------+
;          |                                                                    |
; global ->| make-account:--+                                                   |
;  env     |                |                                                   |
;          | acc:-----------|-----------------+                                 |
;          |                |                 |                                 |
;          +----------------|-----------------|---------------------------------+
;                           |       ^         |                   ^
;                           v       |         |                   |
;                      +---------+  |         |       +-----------------------------------------------------------------+
;                      |  * | *--|--+         |       | balance: 50                                                     |
;                      +--|------+            |       | dispatch:---+                                                   |
;                         |                   | E1 -->| withdraw:...|                                                   |
;                         v                   |       | deposit:... |                                                   |
;      parameters: balance                    |       +-------------|---------------------------------------------------+
;            body:                            |                     |      ^            ^                       ^
;              (define (withdraw amount) ...) |                     v      |            |                       |
;              (define (deposit amount) ...)  |                +---------+ |    +--------------+         +------------+
;              (define (dispatch m) ...)      +--------------->|  * | *--|-+    | m: 'withdraw |<-- E4   | amount: 60 |<-- E5
;                                                              +--|------+      +--------------+         +------------+
;                                                                 v               (cond ...)             (if (>= balance amount)
;                                                         parameters: m                                    ...)
;                                                               body:
;                                                                 (cond ...)
;
; The local state for acc is acc's balance which is kept in E1.
;
; If we define a new account acc2, the local states are distinct because
; acc2's dispatch will be pointing to different environemnt:
;
;          +--------------------------------------------------------------------+
;          |                                                                    |
; global ->| make-account:--+                                                   |
;  env     |                |                                                   |
;          | acc:-----------|-----------------+                                 |<---------------+
;          |                |                 |                                 |                |
;          | acc2:----------|-----------------|---------------------------------|-+              |
;          |                |                 |                                 | |              |
;          +----------------|-----------------|---------------------------------+ |              |
;                           |       ^         |                   ^               |              |
;                           v       |         |                   |               |              |
;                      +---------+  |         |       +----------------------+    |  +------------------------+
;                      |  * | *--|--+         |       | balance: 50          |    |  | balance: 100           |
;                      +--|------+            |       | dispatch:---+        |    |  | dispatch:----+         |
;                         |                   | E1 -->| withdraw:...|        |    |  | withdraw:... |         |
;                         v                   |       | deposit:... |        |    |  | deposit:...  |         |
;      parameters: balance                    |       +-------------|--------+    |  +--------------|---------+
;            body:                            |                     |      ^      |                 |      ^
;              (define (withdraw amount) ...) |                     v      |      |                 |      |
;              (define (deposit amount) ...)  |                +---------+ |      |            +---------+ |
;              (define (dispatch m) ...)      +--------------->|  * | *--|-+      +----------->|  * | *--|-+
;                                                              +--|------+                     +--|------+
;                                                                 v                               |
;                                                         parameters: m <-------------------------+
;                                                               body:
;                                                                 (cond ...)
;
; The code for dispatch, withdraw and deposit can all be shared between
; acc and acc2.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.12                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; I draw a symbol inside the box as a shortcut for a pointer pointing to the
; symbol.
;
; response 1: (b)
; response 2: (b c d)
;
; (define x (list 'a 'b)):
;     +-------+    +-------+
; x-->| a | *-|--->| b | / |
;     +-------+    +-------+
;
; (define y (list 'c 'd)):
;     +-------+    +-------+
; y-->| c | *-|--->| d | / |
;     +-------+    +-------+
;
; (define z (append x y)):
;     +-------+    +-------+
; x-->| a | *-|--->| b | / |
;     +-------+    +-------+
;
;     +-------+    +-------+
; y-->| c | *-|--->| d | / |
;     +-------+    +-------+
;         ^
;         |
;         +-------------------+
;     +-------+    +-------+  |
; z-->| a | *-|--->| b | *-|--+
;     +-------+    +-------+
;
; x is now (a b), so (cdr x) returns (b):
;     +-------+    +-------+
; x-->| a | *-|--->| b | / |
;     +-------+    +-------+
;                      ^
;                      |
;                   (cdr x)
;
; (define w (append! x y)): This appends y to x and returns x, so w & x
; now point to the same list structure:
;
;         w
;         |
;         v
;     +-------+    +-------+
; x-->| a | *-|--->| b | *-|--+
;     +-------+    +-------+  |
;         +-------------------+
;         |
;         v
;     +-------+    +-------+
; y-->| c | *-|--->| d | / |
;     +-------+    +-------+
;
; This means that (cdr x) is (b c d):
;
;         w         (cdr x)
;         |            |
;         v            v
;     +-------+    +-------+
; x-->| a | *-|--->| b | *-|--+
;     +-------+    +-------+  |
;         +-------------------+
;         |
;         v
;     +-------+    +-------+
; y-->| c | *-|--->| d | / |
;     +-------+    +-------+


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.13                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;         +--------------------------------+
;         |                                |
;         v                                |
;     +-------+    +-------+    +-------+  |
; z-->| a | *-|--->| b | *-|--->| c | *-|--+
;     +-------+    +-------+    +-------+
;
; If we try to compute (last-pair z), we'll go into an infinite loop.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.14                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; mystery reverses a list.
;
; (define v (list 'a 'b 'c 'd)):
;     +-------+    +-------+    +-------+    +-------+
; v-->| a | *-|--->| b | *-|--->| c | *-|--->| d | / |
;     +-------+    +-------+    +-------+    +-------+
;
; (define w (mystery v)):
;                                                v
;                                                |
;                                                v
;     +-------+    +-------+    +-------+    +-------+
; w-->| d | *-|--->| c | *-|--->| b | *-|--->| a | / |
;     +-------+    +-------+    +-------+    +-------+
;
; v: (a)
; w: (d c b a)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.15                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;
;       +-------+
; z1 -->| * | * |
;       +-|---|-+
;         v   v
;       +-------+    +-------+
; x  -->| * | *-|--->| * | / |
;       +-|-----+    +-|-----+
;         v            v
;       +---+        +---+
;       | a |        | b |
;       +---+        +---+

;       +-------+    +-------+    +-------+
; z2 -->| * | *-|--->| * | *-|--->| * | / |
;       +-|-----+    +-|-----+    +-|-----+
;         |            v            v
;         |          +---+        +---+
;         |          | a |        | b |
;         |          +---+        +---+
;         |            ^            ^
;         |          +-|-----+    +-|-----+
;         +--------->| * | *-|--->| * | / |
;                    +-------+    +-------+
;
; (set-to-wow! z1):
;
;       +-------+
; z1 -->| * | * |
;       +-|---|-+
;         v   v
;       +-------+    +-------+
; x  -->| * | *-|--->| * | / |
;       +-|-----+    +-|-----+
;         v            v
;      +-----+       +---+
;      | wow |       | b |
;      +-----+       +---+
;
; (set-to-wow! z2):
;
;       +-------+    +-------+    +-------+
; z2 -->| * | *-|--->| * | *-|--->| * | / |
;       +-|-----+    +-|-----+    +-|-----+
;         |            v            v
;         |          +---+        +---+
;         |          | a |        | b |
;         |          +---+        +---+
;         |                         ^
;         |          +-------+    +-|-----+
;         +--------->| * | *-|--->| * | / |
;                    +-|-----+    +-------+
;                      v
;                   +-----+
;                   | wow |
;                   +-----+


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.16                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; return 3:
;
;       +-------+    +-------+    +-------+
; x1 -->| * | *-|--->| * | *-|--->| * | / |
;       +-|-----+    +-|-----+    +-|-----+
;         v            v            v
;       +---+        +---+        +---+
;       | a |        | b |        | c |
;       +---+        +---+        +---+
;
; return 4:
;
;       +-------+    +-------+
; x2 -->| * | *-|--->| * | / |
;       +-|-----+    +-|-----+
;         v            |
;       +---+---+      |
;       | * | / |<-----+
;       +-|-+---+
;         v
;       +---+
;       | a |
;       +---+
;
; return 7:
;
;       +-------+
; x3 -->| * | * |
;       +-|---|-+
;         v   v
;       +---+---+
;       | * | * |
;       +-|-+-|-+
;         v   v
;       +---+---+
;       | * | * |
;       +-|-+-|-+
;         v   v
;      +---+ +---+
;      | a | | b |
;      +---+ +---+
;
; never returns:
;
;           +---------------------------+
;           |                           |
;           v                           |
;       +-------+    +-------+    +-----|-+
; x4 -->| * | *-|--->| * | *-|--->| * | * |
;       +-|-----+    +-|-----+    +-|-----+
;         v            v            v
;       +---+        +---+        +---+
;       | a |        | b |        | c |
;       +---+        +---+        +---+

(module ex3.16 ()
  (import scheme debug)
  (title "ex3.16")

  (define (count-pairs x)
    (if (not (pair? x))
        0
        (+ (count-pairs (car x))
           (count-pairs (cdr x))
           1)))

  (define x1 '(a b c))

  (define temp2 '(a))
  (define x2 (list temp2 temp2))

  (define temp3 (cons 'a 'b))
  (define temp4 (cons temp3 temp3))
  (define x3 (cons temp4 temp4))

  (define x4 '(a b c))
  (set-cdr! (cddr x4) x4)

  (println x1)
  (println x2)
  (println x3)
  ; (println x4) ; will never return

  (println (count-pairs x1)) ; 3
  (println (count-pairs x2)) ; 4
  (println (count-pairs x3)) ; 7
  ; (println (count-pairs x4)) ; will never return
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.17                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.17 ()
  (import scheme debug)
  (title "ex3.17")

  (define (count-pairs x)
    (define (iter x visited-pairs)
      (if (not (pair? x))
          (list 0 visited-pairs)
          (let* ((visited-pair (memq x visited-pairs)))
            (if visited-pair
              (list 0 visited-pairs)
              (let* ((new-visited-pairs (cons x visited-pairs))
                     (car-iter-ret (iter (car x) new-visited-pairs))
                     (cdr-iter-ret (iter (cdr x) (cadr car-iter-ret))))
                (list (+ (car car-iter-ret) (car cdr-iter-ret) 1)
                      (cadr cdr-iter-ret)))))))
    (car (iter x '())))

  (define x1 '(a b c))

  (define temp2 '(a))
  (define x2 (list temp2 temp2))

  (define temp3 (cons 'a 'b))
  (define temp4 (cons temp3 temp3))
  (define x3 (cons temp4 temp4))

  (define x4 '(a b c))
  (set-cdr! (cddr x4) x4)

  (println (count-pairs x1)) ; 3
  (println (count-pairs x2)) ; 3
  (println (count-pairs x3)) ; 3
  (println (count-pairs x4)) ; 3
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.18                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.18 ()
  (import scheme debug)
  (title "ex3.18")

  (define (has-cycle? x)
    (define (iter x visited-pairs)
      (if (not (pair? x))
          (list #f visited-pairs)
          (let* ((visited-pair (memq x visited-pairs)))
            (if visited-pair
              (list #t visited-pairs)
              (let* ((new-visited-pairs (cons x visited-pairs))
                     (car-iter-ret (iter (car x) new-visited-pairs))
                     (cdr-iter-ret (iter (cdr x) new-visited-pairs)))
                (list (or (car car-iter-ret) (car cdr-iter-ret))
                      (cadr cdr-iter-ret)))))))
    (car (iter x '())))

  (define x1 '(a b c))

  (define temp2 '(a))
  (define x2 (list temp2 temp2))

  (define temp3 (cons 'a 'b))
  (define temp4 (cons temp3 temp3))
  (define x3 (cons temp4 temp4))

  (define x4 '(a b c))
  (set-cdr! (cddr x4) x4)

  (define x5 '(a b c))
  (set-cdr! (cddr x5) (cdr x5))

  (println (has-cycle? x1)) ; #f
  (println (has-cycle? x2)) ; #f
  (println (has-cycle? x3)) ; #f
  (println (has-cycle? x4)) ; #t
  (println (has-cycle? x5)) ; #t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.19                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.19 ()
  (import scheme debug)
  (title "ex3.19")

  (define (has-cycle? x)
    (define (iter tortoise1 hare1 tortoise2 hare2)
      (cond ((and (equal? tortoise1 "stopped")
                  (equal? tortoise2 "stopped")
                  (equal? hare1 "stopped")
                  (equal? hare2 "stopped")) #f)
            ((or (and (not (equal? tortoise1 "stopped")) (eq? tortoise1 hare1))
                 (and (not (equal? tortoise2 "stopped")) (eq? tortoise2 hare2))
                 (and (not (equal? tortoise1 "stopped")) (eq? tortoise1 hare2))
                 (and (not (equal? tortoise2 "stopped")) (eq? tortoise2 hare1))) #t)
            (else (let* ((new-tortoise1 (if (pair? tortoise1) (car tortoise1) "stopped"))
                         (new-hare1-temp (if (pair? hare1) (car hare1) "stopped"))
                         (new-hare1 (if (pair? new-hare1-temp) (car new-hare1-temp) "stopped"))
                         (new-tortoise2 (if (pair? tortoise2) (cdr tortoise2) "stopped"))
                         (new-hare2-temp (if (pair? hare2) (cdr hare2) "stopped"))
                         (new-hare2 (if (pair? new-hare2-temp) (cdr new-hare2-temp) "stopped")))
                    (iter new-tortoise1 new-hare1 new-tortoise2 new-hare2)))))
    (if (not (pair? x))
      #f
      (iter x (car x) x (cdr x))))

  (define x1 '(a b c))

  (define temp2 '(a))
  (define x2 (list temp2 temp2))

  (define temp3 (cons 'a 'b))
  (define temp4 (cons temp3 temp3))
  (define x3 (cons temp4 temp4))

  (define x4 '(a b c))
  (set-cdr! (cddr x4) x4)

  (define x5 '(a b c))
  (set-cdr! (cddr x5) (cdr x5))

  (println (has-cycle? x1)) ; #f
  (println (has-cycle? x2)) ; #f
  (println (has-cycle? x3)) ; #f
  (println (has-cycle? x4)) ; #t
  (println (has-cycle? x5)) ; #t
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.20                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (define x (cons 1 2))
; (define z (cons x x))
;
;          +--------------------------------------------------------------------------------------------------+
;          |                                                                                                  |
; global ->| cons:-------+                                                                                    |
;          | car:...     |                                                                                    |
;          | cdr:...     |                                                                                    |
;          | set-car!:...|                                                                                    |
;          | set-cdr!:...|                                                                                    |
;          | x:----------|-------------+                                                                      |
;          | z:----------|-------------|----------------------------------+                                   |
;          |             |             |                                  |                                   |
;          +-------------|-------------|----------------------------------|-----------------------------------+
;                   +----+  ^          |                  ^               |                      ^
;                   |       |          |                  |               |                      |
;                   v       |          |                  |               |                      |
;              +---------+  |          |      +-----------------------+   |         +------------------------+
;              |  * | *--|--+          |      | x: 1                  |   |         | x:-------------------+ |
;              +--|------+             |      | y: 2                  |   |         | y:-------------------+ |
;                 |                    | E1 ->| set-x!:...            |   |    E2 ->| set-x!:...           | |
;                 v                    |      | set-y!:...            |   |         | set-y!:...           | |
; parameters: x y                      |      | dispatch:---+         |   |         | dispatch:---+        | |
;       body:                          |      +-------------|---------+   |         +-------------|--------|-+
;         (define (set-x! amount) ...) |                    v      ^      |                       v      ^ |
;         (define (set-y! amount) ...) |               +---------+ |      |                  +---------+ | |
;         (define (dispatch m) ...)    +-------------->|  * | *--|-+      +----------------->|  * | *--|-+ |
;         dispatch                                     +--|------+                           +--|------+   |
;                                                         v     ^                               v          |
;                                               parameters: m   |                     parameters: m        |
;                                                     body:     +--------+                  body:          |
;                                                       (cond ...)       |                    (cond ...)   |
;                                                                        +---------------------------------+
;
; E1's dispatch procedure is essentially the pair (1 . 2).
;
; (set-car! (cdr z) 17):
;
; (cdr z) points at x (i.e. E1's dispatch procedure). So set-car! will
; basically set E1's x to 17. (car x) is then invoked to return the same
; value


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.21                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The Lisp printer only knows how to print pairs and lists, where lists are
; just pairs whose cdr is itself a pair (or nil). From the point of view of the
; Lisp printer, the queue looks like a list whose car is itself the list of
; elements in the queue and whose cdr is a list containing the last element of
; the queue.

(module ex3.21 (make-queue empty-queue? front-queue insert-queue! delete-queue!)
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.21")

  (define (front-ptr queue) (car queue))
  (define (rear-ptr queue) (cdr queue))
  (define (set-front-ptr! queue item) (set-car! queue item))
  (define (set-rear-ptr! queue item) (set-cdr! queue item))

  (define (empty-queue? queue) (null? (front-ptr queue)))
  (define (make-queue) (cons '() '()))

  (define (front-queue queue)
    (if (empty-queue? queue)
        (error "FRONT-QUEUE called with an empty queue" queue)
        (car (front-ptr queue))))

  (define (insert-queue! queue item)
    (let ((new-pair (cons item '())))
      (cond ((empty-queue? queue)
             (set-front-ptr! queue new-pair)
             (set-rear-ptr! queue new-pair)
             queue)
            (else
             (set-cdr! (rear-ptr queue) new-pair)
             (set-rear-ptr! queue new-pair)
             queue))))

  (define (delete-queue! queue)
    (cond ((empty-queue? queue)
           (error "DELETE-QUEUE! called with an empty queue" queue))
          (else
           (set-front-ptr! queue (cdr (front-ptr queue)))
           queue)))

  (define (print-queue queue)
    (display (front-ptr queue)))

  (define q1 (make-queue))
  (insert-queue! q1 'a)
  (insert-queue! q1 'b)

  (print-queue q1) ; (a b)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.22                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.22 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.22")

  (define (make-queue)
    (let ((front-pointer '())
          (rear-pointer '()))
      (define (front-ptr) front-pointer)
      (define (rear-ptr) rear-pointer)
      (define (set-front-ptr! item) (set! front-pointer item))
      (define (set-rear-ptr! item) (set! rear-pointer item))
      (define (empty-queue?) (null? (front-ptr)))
      (define (front-queue)
        (if (empty-queue?)
            (error "FRONT-QUEUE called with an empty queue")
            (car (front-ptr))))

      (define (insert-queue! queue item)
        (let ((new-pair (cons item '())))
          (cond ((empty-queue?)
                 (set-front-ptr! new-pair)
                 (set-rear-ptr! new-pair)
                 queue)
                (else
                 (set-cdr! (rear-ptr) new-pair)
                 (set-rear-ptr! new-pair)
                 queue))))

      (define (delete-queue! queue)
        (cond ((empty-queue?)
               (error "DELETE-QUEUE! called with an empty queue" queue))
              (else
               (set-front-ptr! (cdr (front-ptr)))
               queue)))

      (define (print-queue)
        (display (front-ptr)))

      (define (dispatch m)
        (cond
          ((eq? m 'empty-queue?) empty-queue?)
          ((eq? m 'front-queue) front-queue)
          ((eq? m 'insert-queue!) insert-queue!)
          ((eq? m 'delete-queue!) delete-queue!)
          ((eq? m 'print-queue) print-queue)))

      dispatch))

  (define (empty-queue? queue) ((queue 'empty-queue?)))
  (define (front-queue queue) ((queue 'front-queue)))
  (define (insert-queue! queue item) ((queue 'insert-queue!) queue item))
  (define (delete-queue! queue) ((queue 'delete-queue!) queue))
  (define (print-queue queue) ((queue 'print-queue)))

  (define q1 (make-queue))
  (insert-queue! q1 'a)
  (insert-queue! q1 'b)

  (print-queue q1) ; (a b)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.23                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.23 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.23")

  (define (make-node item)
    (let ((forward-pair (cons item '()))
          (backward-pair (cons item '())))
      (cons forward-pair backward-pair)))

  (define (forward-pair node) (car node))
  (define (backward-pair node) (cdr node))

  (define (node-value node) (car (forward-pair node)))
  (define (next-node node) (cdr (forward-pair node)))
  (define (prev-node node) (cdr (backward-pair node)))
  (define (last-node? node) (null? (next-node node)))
  (define (set-next-node! node next-node)
    (set-cdr! (forward-pair node) next-node))
  (define (set-prev-node! node prev-node)
    (set-cdr! (backward-pair node) prev-node))

  (define (make-deque) (cons '() '()))
  (define (front-node deque) (car deque))
  (define (rear-node deque) (cdr deque))
  (define (set-front-node! deque node) (set-car! deque node))
  (define (set-rear-node! deque node) (set-cdr! deque node))
  (define (empty-deque? deque) (null? (front-node deque)))

  (define (front-deque deque)
    (if (empty-deque?)
            (error "FRONT-DEQUE called with an empty deque")
            (node-value (front-node deque))))

  (define (rear-deque deque)
    (if (empty-deque?)
            (error "REAR-DEQUE called with an empty deque")
            (node-value (rear-node deque))))

  (define (front-insert-deque! deque item)
    (let ((new-node (make-node item)))
      (cond ((empty-deque? deque)
             (set-front-node! deque new-node)
             (set-rear-node! deque new-node)
             deque)
            (else
             (set-prev-node! (front-node deque) new-node)
             (set-next-node! new-node (front-node deque))
             (set-front-node! deque new-node)
             deque))))

  (define (rear-insert-deque! deque item)
    (let ((new-node (make-node item)))
      (cond ((empty-deque? deque)
             (set-front-node! deque new-node)
             (set-rear-node! deque new-node)
             deque)
            (else
             (set-prev-node! new-node (rear-node deque))
             (set-next-node! (rear-node deque) new-node)
             (set-rear-node! deque new-node)
             deque))))

  (define (front-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "FRONT-DELETE-QUEUE! called with an empty deque" deque))
          (else
           (set-prev-node! (next-node (front-node deque)) '())
           (set-front-node! deque (next-node (front-node deque)))
           deque)))

  (define (rear-delete-deque! deque)
    (cond ((empty-deque? deque)
           (error "REAR-DELETE-QUEUE! called with an empty deque" deque))
          (else
           (set-next-node! (prev-node (rear-node deque)) '())
           (set-rear-node! deque (prev-node (rear-node deque)))
           deque)))

  (define (print-deque deque)
    (define (print-node node)
      (display (node-value node)))

    (define (print-all-nodes node)
      (if (not (null? node))
        (begin
          (print-node node)
          (if (not (last-node? node))
              (display " "))
          (print-all-nodes (next-node node)))))

    (if (empty-deque? deque)
      (display "()")
      (begin
        (display "(")
        (print-all-nodes (front-node deque))
        (display ")"))))

  (define d1 (make-deque))
  (rear-insert-deque! d1 'a)
  (rear-insert-deque! d1 'b)
  (front-insert-deque! d1 'c)
  (front-insert-deque! d1 'd)
  (rear-delete-deque! d1)
  (front-delete-deque! d1)

  (print-deque d1) ; (c a)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.24                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.24 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.24")

  (define (make-table same-key?)
    (let ((local-table (list '*table*)))
      (define (assoc-same-key? key records)
        (cond ((null? records) #f)
              ((same-key? key (caar records)) (car records))
              (else (assoc-same-key? key (cdr records)))))

      (define (lookup key)
        (let ((record (assoc-same-key? key (cdr local-table))))
          (if record (cdr record) #f)))

      (define (insert! key value)
        (let ((record (assoc-same-key? key (cdr local-table))))
          (if record
              (begin (set-car! record key)
                     (set-cdr! record value))
              (set-cdr! local-table
                        (cons (cons key value)
                              (cdr local-table)))))
        'ok)

      (define (print)
        (display (cdr local-table)))

      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              ((eq? m 'print-proc) print)
              (else (error "Unknown operation -- TABLE" m))))

      dispatch))

  (define (lookup key table)
    ((table 'lookup-proc) key))

  (define (insert! key value table)
    ((table 'insert-proc!) key value))

  (define (print-table table)
    ((table 'print-proc)))

  (define (within-tolerance? k1 k2)
    (< (abs (- k1 k2)) 0.1))

  (define t1 (make-table within-tolerance?))
  (insert! 1 'a t1)
  (insert! 2 'b t1)
  (insert! 1.05 'c t1)

  (print-table t1)
  (newline)

  (println (lookup 1 t1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.25                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.25 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.25")

  (define (make-table)
    (let ((local-table (list '*table*)))
      (define (lookup keys)
        (let ((record (assoc keys (cdr local-table))))
          (if record (cdr record) #f)))

      (define (insert! keys value)
        (let ((record (assoc keys (cdr local-table))))
          (if record
              (begin (set-car! record keys)
                     (set-cdr! record value))
              (set-cdr! local-table
                        (cons (cons keys value)
                              (cdr local-table)))))
        'ok)

      (define (print)
        (display (cdr local-table)))

      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              ((eq? m 'print-proc) print)
              (else (error "Unknown operation -- TABLE" m))))

      dispatch))

  (define (lookup keys table)
    ((table 'lookup-proc) keys))

  (define (insert! keys value table)
    ((table 'insert-proc!) keys value))

  (define (print-table table)
    ((table 'print-proc)))

  (define t1 (make-table))
  (insert! '(1 2) 'a t1)
  (insert! '(3 4 5) 'b t1)
  (insert! '(1 2) 'c t1)

  (print-table t1)
  (newline)

  (println (lookup '(3 4 5) t1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.26                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.26 ()
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.26")

  (define (make-table key-less-than?)
    (define (make-tree key value left-branch right-branch)
      (list key value left-branch right-branch))

    (define (empty-tree? tree) (null? tree))
    (define (get-key tree) (car tree))
    (define (get-value tree) (cadr tree))
    (define (left-branch tree) (caddr tree))
    (define (right-branch tree) (cadddr tree))

    (define (set-key! key tree) (set-car! tree key))
    (define (set-value! value tree) (set-car! (cdr tree) value))
    (define (set-left-branch! left-branch tree) (set-car! (cddr tree) left-branch))
    (define (set-right-branch! right-branch tree) (set-car! (cdddr tree) right-branch))

    (define (tree-lookup key tree)
      (let ((tree-key (get-key tree)))
        (cond
          ((key-less-than? key tree-key)
           (if (empty-tree? (left-branch tree))
               #f
               (tree-lookup key (left-branch tree))))
          ((key-less-than? tree-key key)
           (if (empty-tree? (right-branch tree))
               #f
               (tree-lookup key (right-branch tree))))
          (else
           (get-value tree)))))

    (define (tree-insert! key value tree)
      (let ((tree-key (get-key tree)))
        (cond
          ((key-less-than? key tree-key)
           (if (empty-tree? (left-branch tree))
               (set-left-branch! (make-tree key value '() '()) tree)
               (tree-insert! key value (left-branch tree))))
          ((key-less-than? tree-key key)
           (if (empty-tree? (right-branch tree))
               (set-right-branch! (make-tree key value '() '()) tree)
               (tree-insert! key value (right-branch tree))))
          (else
           (set-key! key tree)
           (set-value! value tree)))))

    (define (tree-print tree)
      (if (not (empty-tree? (left-branch tree)))
          (tree-print (left-branch tree)))

      (display "(")
      (display (get-key tree))
      (display " ")
      (display (get-value tree))
      (display ")")

      (if (not (empty-tree? (right-branch tree)))
          (tree-print (right-branch tree))))

    (let ((local-table (list '*table*)))
      (define (empty-table?)
        (null? (cdr local-table)))

      (define (table-tree)
        (cadr local-table))

      (define (lookup key)
        (if (empty-table?)
            #f
            (tree-lookup key (table-tree))))

      (define (insert! key value)
        (if (empty-table?)
          (set-cdr! local-table (list (make-tree key value '() '())))
          (tree-insert! key value (table-tree))))

      (define (print)
        (display "(")
        (tree-print (table-tree))
        (display ")"))

      (define (dispatch m)
        (cond ((eq? m 'lookup-proc) lookup)
              ((eq? m 'insert-proc!) insert!)
              ((eq? m 'print-proc) print)
              (else (error "Unknown operation -- TABLE" m))))

      dispatch))

  (define (lookup key table)
    ((table 'lookup-proc) key))

  (define (insert! key value table)
    ((table 'insert-proc!) key value))

  (define (print-table table)
    ((table 'print-proc)))

  (define t1 (make-table <))
  (insert! 1 'a t1)
  (insert! 3 'c t1)
  (insert! 2 'b t1)

  (print-table t1)
  (newline)

  (println (lookup 1 t1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.27 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The environment diagram is obvious but ASCII art is tiresome, so I'll skip it.
;
; memo-fib computes the nth-fibonacci number in a number of steps proportional
; to n because no fibonacci number has to be computed twice as in the case of
; the exponential fibonacci number. Once a fibonacci number is computed, it is
; stored in the memoization table and retrieved when needed.
;
; This scheme will work if memo-fib is defined as (memoize fib) but it will be
; less efficient than the given definition. For example, computing (memo-fib 10)
; using the original definition would start by computing and storing (memo-fib 0)
; and (memo-fib 1). (memo-fib 2) will be computed by retrieving the values of
; (memo-fib 1) and (memo-fib 0) from the memoization table, and then (memo-fib 2)
; itself will be stored in the table for the computation of the next fibonacci
; number to use it. In the new scheme (memo-fib 10) will compute all the
; fibonacci numbers from 1 to 10 (more than once) and will only store the result
; of (fib 10).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.28                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.28 (make-agenda make-wire get-signal set-signal! add-action!
                inverter and-gate or-gate probe propagate clear-the-agenda!)
  (import scheme debug ex3.21)
  (import (only chicken error))
  (title "ex3.28")

  (define (make-time-segment time queue)
    (cons time queue))
  (define (segment-time s) (car s))
  (define (segment-queue s) (cdr s))

  (define (make-agenda) (list 0))
  (define the-agenda (make-agenda))
  (define (clear-the-agenda!) (set! the-agenda (make-agenda)))


  (define (current-time agenda) (car agenda))
  (define (set-current-time! agenda time)
    (set-car! agenda time))
  (define (segments agenda) (cdr agenda))
  (define (set-segments! agenda segments)
    (set-cdr! agenda segments))
  (define (first-segment agenda) (car (segments agenda)))
  (define (rest-segments agenda) (cdr (segments agenda)))

  (define (empty-agenda? agenda)
    (null? (segments agenda)))

  (define (add-to-agenda! time action agenda)
    (define (belongs-before? segments)
      (or (null? segments)
          (< time (segment-time (car segments)))))

    (define (make-new-time-segment time action)
      (let ((q (make-queue)))
        (insert-queue! q action)
        (make-time-segment time q)))

    (define (add-to-segments! segments)
      (if (= (segment-time (car segments)) time)
          (insert-queue! (segment-queue (car segments))
                         action)
          (let ((rest (cdr segments)))
            (if (belongs-before? rest)
                (set-cdr!
                 segments
                 (cons (make-new-time-segment time action)
                       (cdr segments)))
                (add-to-segments! rest)))))

    (let ((segments (segments agenda)))
      (if (belongs-before? segments)
          (set-segments!
           agenda
           (cons (make-new-time-segment time action)
                 segments))
          (add-to-segments! segments))))

  (define (remove-first-agenda-item! agenda)
    (let ((q (segment-queue (first-segment agenda))))
      (delete-queue! q)
      (if (empty-queue? q)
          (set-segments! agenda (rest-segments agenda)))))

  (define (first-agenda-item agenda)
    (if (empty-agenda? agenda)
        (error "Agenda is empty -- FIRST-AGENDA-ITEM")
        (let ((first-seg (first-segment agenda)))
          (set-current-time! agenda (segment-time first-seg))
          (front-queue (segment-queue first-seg)))))

  (define (after-delay delay-period action)
    (add-to-agenda! (+ delay-period (current-time the-agenda))
                    action
                    the-agenda))

  (define (propagate)
    (if (empty-agenda? the-agenda)
        'done
        (let ((first-item (first-agenda-item the-agenda)))
          (first-item)
          (remove-first-agenda-item! the-agenda)
          (propagate))))

  (define (call-each procedures)
    (if (null? procedures)
        'done
        (begin
          ((car procedures))
          (call-each (cdr procedures)))))

  (define (make-wire)
    (let ((signal-value 0) (action-procedures '()))
      (define (set-my-signal! new-value)
        (if (not (= signal-value new-value))
            (begin
              (set! signal-value new-value)
              (call-each action-procedures))
            'done))
      (define (accept-action-procedure! proc)
        (set! action-procedures (append action-procedures (list proc)))
        (proc))
      (define (dispatch m)
        (cond ((eq? m 'get-signal) signal-value)
              ((eq? m 'set-signal!) set-my-signal!)
              ((eq? m 'add-action!) accept-action-procedure!)
              (else (error "Unknown operation -- WIRE" m))))
      dispatch))

  (define (get-signal w) (w 'get-signal))
  (define (set-signal! w v) ((w 'set-signal!) v))
  (define (add-action! w a) ((w 'add-action!) a))

  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))

  (define (logical-and s1 s2)
    (let ((valid-signals '(0 1)))
      (if (and (memq s1 valid-signals) (memq s2 valid-signals))
        (if (or (= s1 0) (= s2 0)) 0 1)
        (error "Invalid signals" s1 s2))))

  (define (logical-or s1 s2)
    (let ((valid-signals '(0 1)))
      (if (and (memq s1 valid-signals) (memq s2 valid-signals))
        (if (or (= s1 1) (= s2 1)) 1 0)
        (error "Invalid signals" s1 s2))))

  (define (inverter input output)
    (define (invert-input)
      (let ((new-value (logical-not (get-signal input)))
            (inverter-delay 1))
        (after-delay inverter-delay
                     (lambda () (set-signal! output new-value)))))
    (add-action! input invert-input)
    'ok)

  (define (and-gate a1 a2 output)
    (define (and-action-procedure)
      (let ((new-value (logical-and (get-signal a1) (get-signal a2)))
            (and-gate-delay 2))
        (after-delay and-gate-delay
                     (lambda () (set-signal! output new-value)))))
    (add-action! a1 and-action-procedure)
    (add-action! a2 and-action-procedure)
    'ok)

  (define (or-gate a1 a2 output)
    (define (or-action-procedure)
      (let ((new-value (logical-or (get-signal a1) (get-signal a2)))
            (or-gate-delay 2))
        (after-delay or-gate-delay
                     (lambda () (set-signal! output new-value)))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok)

  (define (probe name wire)
    (add-action! wire
      (lambda ()
        (newline)
        (display "Wire = ")
        (display name)
        (display " Time = ")
        (display (current-time the-agenda))
        (display " Value = ")
        (display (get-signal wire)))))

  (define a (make-wire))
  (define b (make-wire))
  (define o (make-wire))
  (set-signal! a 1)
  (set-signal! b 0)
  (or-gate a b o)

  (probe 'o o)
  (propagate)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.29                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.29 ()
  (import scheme debug)
  (import (only chicken error))
  (import (only ex3.28 make-wire set-signal! inverter and-gate probe propagate
                       clear-the-agenda!))
  (title "ex3.29")
  (clear-the-agenda!)

  (define (or-gate a b output)
    (let ((i1 (make-wire)) (i2 (make-wire)) (io (make-wire)))
      (inverter a i1)
      (inverter b i2)
      (and-gate i1 i2 io)
      (inverter io output)
      'ok))

  (define a (make-wire))
  (define b (make-wire))
  (define o (make-wire))
  (set-signal! a 1)
  (set-signal! b 0)
  (or-gate a b o)

  (probe 'o o)
  (propagate)

  ; or-gate delay is equals to 2 inverter gate delays (for input and output
  ; inverters) + 1 and-gate delay.

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.30                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.30 ()
  (import scheme debug ex3.28)
  (import (only chicken error))
  (title "ex3.30")
  (clear-the-agenda!)

  (define (half-adder a b s c)
    (let ((d (make-wire)) (e (make-wire)))
      (or-gate a b d)
      (and-gate a b c)
      (inverter c e)
      (and-gate d e s)
      'ok))

  (define (full-adder a b c-in sum c-out)
    (let ((s (make-wire))
          (c1 (make-wire))
          (c2 (make-wire)))
      (half-adder b c-in s c1)
      (half-adder a s sum c2)
      (or-gate c1 c2 c-out)
      'ok))

  (define (ripple-carry-adder as bs c-in ss c-out)
    (if (or (not (= (length as) (length bs)))
            (not (= (length as) (length ss))))
      (error "Signals of different lengths" as bs ss)
      (if (= (length as) 1)
        (begin
          (full-adder (car as) (car bs) c-in (car ss) c-out)
          'ok)
        (let ((stage-carry (make-wire)))
          (full-adder (car as) (car bs) c-in (car ss) stage-carry)
          (ripple-carry-adder (cdr as) (cdr bs) stage-carry (cdr ss) c-out)))))

  (define a0 (make-wire))
  (define a1 (make-wire))
  (define a2 (make-wire))

  (define b0 (make-wire))
  (define b1 (make-wire))
  (define b2 (make-wire))

  (define s0 (make-wire))
  (define s1 (make-wire))
  (define s2 (make-wire))

  (define c-in  (make-wire))
  (define c-out (make-wire))

  ; a = 5
  (set-signal! a0 1)
  (set-signal! a1 0)
  (set-signal! a2 1)

  ; b = 6
  (set-signal! b0 0)
  (set-signal! b1 1)
  (set-signal! b2 1)

  ; c-in = 1
  (set-signal! c-in 1)

  (probe 's0 s0)
  (probe 's1 s1)
  (probe 's2 s2)

  (probe 'c-out c-out)

  (ripple-carry-adder (list a0 a1 a2) (list b0 b1 b2) c-in (list s0 s1 s2) c-out)

  (propagate)

  ; Wire = s0 Time = 9 Value = 0
  ; Wire = s1 Time = 17 Value = 0
  ; Wire = s2 Time = 22 Value = 1
  ; Wire = c-out Time = 8 Value = 1
  ;
  ; Therefore, the final output is 1100 (i.e. 12) for inputs 101 (5), 110 (6)
  ; and 1 (1) carry-in.

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.30                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; If accept-action-procedure! does not run the given proceduce after adding it,
; creating new circuitry will amount to only adding action procedures to wires
