(module debug (newlines title println id put get put-coercion get-coercion
               reset-proc-table reset-coercion-table install-proc install-proc2
               define-syntax-rule)
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
;; ex3.31                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; If accept-action-procedure! does not run the given proceduce after adding it,
; you will have to make sure you create the whole circuit before setting
; signals on any input lines, because if you set signals on input lines before
; creating some part of the circuit, the action procedures for these parts will
; never be executed, because they will never have been added to the agenda.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.32 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; With queue:
;   Wire = a Time = 0 Value = 1
;   Wire = b Time = 0 Value = 0
;   Wire = o Time = 0 Value = 0
;   Wire = o Time = 2 Value = 1
;   Wire = o Time = 2 Value = 0
;
; With stack:
;   Wire = a Time = 0 Value = 1
;   Wire = b Time = 0 Value = 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.33                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.33 (make-connector has-value? get-value set-value! forget-value!
                connect inform-about-value inform-about-no-value adder
                multiplier constant probe averager)
  (import scheme debug)
  (import (only chicken error))
  (title "ex3.33")

  (define (for-each-except exception procedure list)
    (define (loop items)
      (cond ((null? items) 'done)
            ((eq? (car items) exception) (loop (cdr items)))
            (else (procedure (car items))
                  (loop (cdr items)))))
    (loop list))

  (define (make-connector)
    (let ((value #f) (informant #f) (constraints '()))
      (define (set-my-value newval setter)
        (cond ((not (has-value? me))
               (set! value newval)
               (set! informant setter)
               (for-each-except setter
                                inform-about-value
                                constraints))
              ((not (= value newval))
               (error "Contradiction" (list value newval)))
              (else 'ignored)))

      (define (forget-my-value retractor)
        (if (eq? retractor informant)
            (begin (set! informant #f)
                   (for-each-except retractor
                                    inform-about-no-value
                                    constraints))
            'ignored))

      (define (connect new-constraint)
        (if (not (memq new-constraint constraints))
            (set! constraints 
                  (cons new-constraint constraints)))
        (if (has-value? me)
            (inform-about-value new-constraint))
        'done)

      (define (me request)
        (cond ((eq? request 'has-value?)
               (if informant #t #f))
              ((eq? request 'value) value)
              ((eq? request 'set-value!) set-my-value)
              ((eq? request 'forget) forget-my-value)
              ((eq? request 'connect) connect)
              (else (error "Unknown operation -- CONNECTOR"
                           request))))
      me))

  (define (has-value? connector)
    (connector 'has-value?))

  (define (get-value connector)
    (connector 'value))

  (define (set-value! connector new-value informant)
    ((connector 'set-value!) new-value informant))

  (define (forget-value! connector retractor)
    ((connector 'forget) retractor))

  (define (connect connector new-constraint)
    ((connector 'connect) new-constraint))

  (define (inform-about-value constraint)
    (constraint 'I-have-a-value))

  (define (inform-about-no-value constraint)
    (constraint 'I-lost-my-value))

  (define (adder a1 a2 sum)
    (define (process-new-value)
      (cond ((and (has-value? a1) (has-value? a2))
             (set-value! sum
                         (+ (get-value a1) (get-value a2))
                         me))
            ((and (has-value? a1) (has-value? sum))
             (set-value! a2
                         (- (get-value sum) (get-value a1))
                         me))
            ((and (has-value? a2) (has-value? sum))
             (set-value! a1
                         (- (get-value sum) (get-value a2))
                         me))))

    (define (process-forget-value)
      (forget-value! sum me)
      (forget-value! a1 me)
      (forget-value! a2 me)
      (process-new-value))

    (define (me request)
      (cond ((eq? request 'I-have-a-value)  
             (process-new-value))
            ((eq? request 'I-lost-my-value) 
             (process-forget-value))
            (else 
             (error "Unknown request -- ADDER" request))))

    (connect a1 me)
    (connect a2 me)
    (connect sum me)
    me)

  (define (multiplier m1 m2 product)
    (define (process-new-value)
      (cond ((or (and (has-value? m1) (= (get-value m1) 0))
                 (and (has-value? m2) (= (get-value m2) 0)))
             (set-value! product 0 me))
            ((and (has-value? m1) (has-value? m2))
             (set-value! product
                         (* (get-value m1) (get-value m2))
                         me))
            ((and (has-value? product) (has-value? m1))
             (set-value! m2
                         (/ (get-value product) (get-value m1))
                         me))
            ((and (has-value? product) (has-value? m2))
             (set-value! m1
                         (/ (get-value product) (get-value m2))
                         me))))

    (define (process-forget-value)
      (forget-value! product me)
      (forget-value! m1 me)
      (forget-value! m2 me)
      (process-new-value))

    (define (me request)
      (cond ((eq? request 'I-have-a-value)
             (process-new-value))
            ((eq? request 'I-lost-my-value)
             (process-forget-value))
            (else
             (error "Unknown request -- MULTIPLIER" request))))

    (connect m1 me)
    (connect m2 me)
    (connect product me)
    me)

  (define (constant value connector)
    (define (me request)
      (error "Unknown request -- CONSTANT" request))
    (connect connector me)
    (set-value! connector value me)
    me)

  (define (probe name connector)
    (define (print-probe value)
      (newline)
      (display "Probe: ")
      (display name)
      (display " = ")
      (display value))

    (define (process-new-value)
      (print-probe (get-value connector)))

    (define (process-forget-value)
      (print-probe "?"))

    (define (me request)
      (cond ((eq? request 'I-have-a-value)
             (process-new-value))
            ((eq? request 'I-lost-my-value)
             (process-forget-value))
            (else
             (error "Unknown request -- PROBE" request))))
    (connect connector me)
    me)

  (define (averager a b c)
    (let ((x (make-connector))
          (y (make-connector)))
      (multiplier x c y)
      (adder a b y)
      (constant 2 x)
      'ok))

  (define A (make-connector))
  (define B (make-connector))
  (define C (make-connector))
  (averager A B C)

  (probe "A" A)
  (probe "B" B)
  (probe "C" C)

  (set-value! A 5 'user)
  (set-value! B 7 'user)
  (forget-value! A 'user)
  (set-value! C 5 'user)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.34                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The problem with this is that the value of a cannot be deduced from the value
; of b. The multiplier assumes that two values are available in order to compute
; the third value, but when a has no value (or when the value of a is
; forgotten), the multiplier will have only one value, namely, the output value
; set by the user.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.35                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.35 ()
  (import scheme debug ex3.33)
  (import (only chicken error))
  (title "ex3.35")

  (define (squarer a b)
    (define (process-new-value)
      (cond
        ((has-value? b)
         (if (< (get-value b) 0)
             (error "square less than 0 -- SQUARER" (get-value b))
             (set-value! a (sqrt (get-value b)) me)))
        ((has-value? a)
         (set-value! b (* (get-value a) (get-value a)) me))))

    (define (process-forget-value)
      (forget-value! b me)
      (forget-value! a me)
      (process-new-value))

    (define (me request)
      (cond ((eq? request 'I-have-a-value)
             (process-new-value))
            ((eq? request 'I-lost-my-value)
             (process-forget-value))
            (else
             (error "Unknown request -- SQUARER" request))))

    (connect a me)
    (connect b me)
    me)

  (define A (make-connector))
  (define B (make-connector))
  (squarer A B)

  (probe "A" A)
  (probe "B" B)

  (set-value! A 5 'user)
  (forget-value! A 'user)
  (set-value! B 25 'user)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.36 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Skipping


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.37                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.37 ()
  (import scheme debug ex3.33)
  (title "ex3.37")

  (define (c+ x y)
    (let ((z (make-connector)))
      (adder x y z)
      z))

  (define (c- x y)
    (let ((z (make-connector)))
      (adder z y x)
      z))

  (define (c* x y)
    (let ((z (make-connector)))
      (multiplier x y z)
      z))

  (define (c/ x y)
    (let ((z (make-connector)))
      (multiplier z y x)
      z))

  (define (cv v)
    (let ((x (make-connector)))
      (constant v x)
      x))

  (define (celsius-fahrenheit-converter x)
    (c+ (c* (c/ (cv 9) (cv 5))
            x)
        (cv 32)))

  (define C (make-connector))
  (define F (celsius-fahrenheit-converter C))

  (probe "C" C)
  (probe "F" F)

  (set-value! C 25 'user)
  (forget-value! C 'user)
  (set-value! F 212 'user)

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.38 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; (a) Possible values: 45, 35, 50, 40.
; (b) Will skip the timing diagrams. Some other possible values if processes are
; allowed to interleave are: 55, 80, 90, 110, etc.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.39                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; 100, 101, 121


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.40                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Possible values: 100, 1000, 10000, 100000, 1000000.
; After serialization: Only 1000000.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.41                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; No. The worst that can happen is to read the balance while a deposit or
; withdrawal is being processed, but then, the operation is not yet complete, so
; it's okay to read the old value of the balance.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.42                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; It is a safe change because every account object still has its own serializer.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.43 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Skipping


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.44                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; No, Louis is not right. The transfer problem does not involve querying
; account balances before withdrawal, while the exchange problem has to query
; account balances to compute the difference.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.45                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; The program will go into a deadlock because account1 withdraw is already
; serialized using account 1 serializer, so it cannot be called inside another
; account 1 serializer. Similarly for account2 deposit.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.46 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Skipping.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.47                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.47 ()
  (import scheme debug)
  (import (only chicken add1 sub1))
  (title "ex3.47")

  (define (clear! cell)
    #t
  )

  (define (test-and-set! cell)
    #t
  )

  (define (make-mutex)
    #t
  )

  ; (a)

  (define (make-semaphore n)
    (let ((mutex (make-mutex))
          (l n))
      (define (the-semaphore m)
        (cond ((eq? m 'acquire)
               (if (= l 0)
                (mutex 'acquire)
                (set! l (sub1 l))))
              ((eq? m 'release)
               (if (= l n)
                 (mutex 'release)
                 (set! l (add1 l))))))
      the-semaphore))

  ; (b)

  (define (make-semaphore n)
    (let ((cell (list #f))
          (l n))
      (define (the-semaphore m)
        (cond ((eq? m 'acquire)
               (if (= l 0)
                   (if (test-and-set! cell)
                       (the-semaphore 'acquire))
                   (set! l (sub1 l))))
              ((eq? m 'release)
               (if (= l n)
                   (clear! cell)
                   (set! l (add1 l))))))
      the-semaphore))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.48 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Skipping.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.49 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Skipping.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.50                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.50 (stream-cons stream-car stream-cdr the-empty-stream stream-null?
                stream-enumerate-interval stream-ref stream-for-each stream-map
                stream-filter display-stream display-line add-streams
                scale-stream integers-starting-from naturals)
  (import scheme debug)
  (title "ex3.50")

  (define-syntax-rule (stream-cons a b)
    (cons a (delay b)))
  (define (stream-car stream) (car stream))
  (define (stream-cdr stream) (force (cdr stream)))

  (define the-empty-stream '())
  (define (stream-null? s) (null? s))

  (define (stream-enumerate-interval low high)
    (if (> low high)
        the-empty-stream
        (stream-cons
         low
         (stream-enumerate-interval (+ low 1) high))))

  (define (stream-ref s n)
    (if (= n 0)
        (stream-car s)
        (stream-ref (stream-cdr s) (- n 1))))

  (define (stream-for-each proc s)
    (if (stream-null? s)
        'done
        (begin (proc (stream-car s))
               (stream-for-each proc (stream-cdr s)))))

  (define (stream-map proc . argstreams)
    (if (stream-null? (car argstreams))
        the-empty-stream
        (stream-cons
         (apply proc (map stream-car argstreams))
         (apply stream-map
                (cons proc (map stream-cdr argstreams))))))

  (define (stream-filter pred stream)
    (cond ((stream-null? stream) the-empty-stream)
          ((pred (stream-car stream))
           (stream-cons (stream-car stream)
                        (stream-filter pred
                                       (stream-cdr stream))))
          (else (stream-filter pred (stream-cdr stream)))))

  (define (add-streams s1 s2)
    (stream-map + s1 s2))

  (define (scale-stream stream factor)
    (stream-map (lambda (x) (* x factor)) stream))

  (define (integers-starting-from n)
    (stream-cons n (integers-starting-from (+ n 1))))

  (define naturals (integers-starting-from 1))

  (define (display-stream s)
    (stream-for-each display-line s))

  (define (display-line x)
    (newline)
    (display x))

  (define result (stream-map + (stream-enumerate-interval 1 10)
                               (stream-enumerate-interval 11 20)))

  (println result)
  (println (cons (stream-car result) (force (stream-cdr result))))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.51                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.51 ()
  (import scheme debug ex3.50)
  (title "ex3.51")

  (define (show x)
    (display-line x)
    x)


  (define x (stream-map show (stream-enumerate-interval 0 10)))
  (stream-ref x 5)
  (println "")
  (stream-ref x 7)

  ; The result is:
  ;
  ; 0
  ; 1
  ; 2
  ; 3
  ; 4
  ; 5
  ;
  ; 6
  ; 7
  ;
  ; Which indicates that the built-in delay operation is memoized.

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.52                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.52 ()
  (import scheme debug ex3.50)
  (title "ex3.52")

  (define sum 0)
  (println sum) ; 0

  (define (accum x)
    (set! sum (+ x sum))
    sum)
  (println sum) ; 0

  (define seq (stream-map accum (stream-enumerate-interval 1 20)))
  ; The unevaluated seq is (1 3 6 10 15 21 28 36 45 55 66 78 91 105 120 136 153
  ;                         171 190 210)
  (println sum) ; 1

  (define y (stream-filter even? seq))
  ; The unevaluated y is (6 10 28 36 66 78 120 136 190 210)
  (println sum) ; 6

  (define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                           seq))
  ; The unevaluated z is (10 15 45 55 105 120 190 210)
  (println sum) ; 10

  (stream-ref y 7)
  ; This evaluates until index 7 of y (starting at 0)
  (println sum) ; 136

  (display-stream z)
  ; This evaluates all of z
  (println "\n")
  (println sum) ; 210

  ; If delay was not memoized, every stream function call will have to
  ; re-evaluate the full sequence and would thus contribute more to 'sum'.

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.53                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.53 ()
  (import scheme debug ex3.50)
  (title "ex3.53")

  (define s (stream-cons 1 (add-streams s s)))
  ; (1 2 4 8 16 ...)

  (println (stream-ref s 8)) ; 256

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.54                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.54 (mul-streams)
  (import scheme debug ex3.50)
  (title "ex3.54")

  (define (mul-streams s1 s2)
    (stream-map * s1 s2))

  (define factorials (stream-cons 1 (mul-streams factorials naturals)))

  (println (stream-ref factorials 6)) ; 720

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.55                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.55 (partial-sums)
  (import scheme debug ex3.50 ex3.54)
  (title "ex3.55")

  (define (partial-sums s)
    (define x (stream-cons 0 (add-streams x s)))
    (stream-cdr x))

  (println (stream-ref (partial-sums naturals) 0)) ; 1
  (println (stream-ref (partial-sums naturals) 1)) ; 3
  (println (stream-ref (partial-sums naturals) 2)) ; 6
  (println (stream-ref (partial-sums naturals) 3)) ; 10
  (println (stream-ref (partial-sums naturals) 4)) ; 15

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.56                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.56 ()
  (import scheme debug ex3.50)
  (title "ex3.56")

  (define (merge s1 s2)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (cond ((< s1car s2car)
                    (stream-cons s1car (merge (stream-cdr s1) s2)))
                   ((> s1car s2car)
                    (stream-cons s2car (merge s1 (stream-cdr s2))))
                   (else
                    (stream-cons s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))

  (define s (stream-cons 1 (merge (scale-stream s 2)
                                  (merge (scale-stream s 3)
                                         (scale-stream s 5)))))

  (println (stream-ref s 0))
  (println (stream-ref s 1))
  (println (stream-ref s 2))
  (println (stream-ref s 3))
  (println (stream-ref s 4))
  (println (stream-ref s 5))
  (println (stream-ref s 6))
  (println (stream-ref s 7))
  (println (stream-ref s 8))
  (println (stream-ref s 9))
  (println (stream-ref s 10))
  (println (stream-ref s 11))
  (println (stream-ref s 12))
  (println (stream-ref s 13))
  (println (stream-ref s 14))
  (println (stream-ref s 15))
  (println (stream-ref s 16))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.57                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Number of additions = n - 1. If delay was not memoized, the number of
; additions a(n) = a(n - 1) + a(n - 2) + 1 = fib(n) + 1.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.58 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.58 ()
  (import scheme debug ex3.50)
  (title "ex3.58")

  (define (expand num den radix)
    (stream-cons
      (quotient (* num radix) den)
      (expand (remainder (* num radix) den) den radix)))

  (println (stream-ref (expand 1 7 10) 0))
  (println (stream-ref (expand 1 7 10) 1))
  (println (stream-ref (expand 1 7 10) 2))
  (println (stream-ref (expand 1 7 10) 3))
  (println (stream-ref (expand 1 7 10) 4))
  (println (stream-ref (expand 1 7 10) 5))
  (println (stream-ref (expand 1 7 10) 6))

  (println "")

  (println (stream-ref (expand 3 8 10) 0))
  (println (stream-ref (expand 3 8 10) 1))
  (println (stream-ref (expand 3 8 10) 2))
  (println (stream-ref (expand 3 8 10) 3))
  (println (stream-ref (expand 3 8 10) 4))
  (println (stream-ref (expand 3 8 10) 5))
  (println (stream-ref (expand 3 8 10) 6))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.59                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.59 (cosine-series sine-series)
  (import scheme debug ex3.50 ex3.54)
  (title "ex3.59")

  ; (a)

  (define (div-streams s1 s2)
    (stream-map / s1 s2))

  (define (integrate-series s)
    (div-streams s naturals))

  (define test-stream (mul-streams naturals naturals))
  (println (stream-ref (integrate-series test-stream) 0)) ; 1
  (println (stream-ref (integrate-series test-stream) 1)) ; 2
  (println (stream-ref (integrate-series test-stream) 2)) ; 3
  (println (stream-ref (integrate-series test-stream) 3)) ; 4

  ; (b)

  (define cosine-series
    (stream-cons 1 (scale-stream (integrate-series sine-series) -1)))
  (define sine-series
    (stream-cons 0 (integrate-series cosine-series)))

  (println "")
  (println (stream-ref cosine-series 0)) ; 1
  (println (stream-ref cosine-series 1)) ; 0
  (println (stream-ref cosine-series 2)) ; -0.5
  (println (stream-ref cosine-series 3)) ; 0
  (println (stream-ref cosine-series 4)) ; 0.041667
  (println (stream-ref cosine-series 5)) ; 0

  (println "")
  (println (stream-ref sine-series 0)) ; 0
  (println (stream-ref sine-series 1)) ; 1
  (println (stream-ref sine-series 2)) ; 0
  (println (stream-ref sine-series 3)) ; -0.16667
  (println (stream-ref sine-series 4)) ; 0
  (println (stream-ref sine-series 5)) ; 0.008333

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.60                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.60 (mul-series)
  (import scheme debug ex3.50 ex3.54 ex3.59)
  (title "ex3.60")

  (define (mul-series s1 s2)
    (stream-cons (* (stream-car s1) (stream-car s2))
                 (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                 (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                              (stream-cons 0 (mul-series (stream-cdr s1)
                                                         (stream-cdr s2)))))))

  (define two-powers (stream-cons 1 (add-streams two-powers two-powers)))

  (define (one n)
    (define one-stream (mul-streams
                        (add-streams (mul-series cosine-series cosine-series)
                                      (mul-series sine-series sine-series))
                        two-powers))
    (if (< n 0)
        0
        (+ (stream-ref one-stream n) (one (- n 1)))))

  (println (one 20)) ; 1.0

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.61                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.61 (invert-unit-series)
  (import scheme debug ex3.50 ex3.59 ex3.60)
  (title "ex3.61")

  (define (invert-unit-series s)
    (define x (stream-cons (stream-car s)
                           (scale-stream (mul-series (stream-cdr s) x) -1)))
    x)

  (define cosine-inverse (invert-unit-series cosine-series))

  (println (stream-ref cosine-inverse 0))
  (println (stream-ref cosine-inverse 1))
  (println (stream-ref cosine-inverse 2))
  (println (stream-ref cosine-inverse 3))
  (println (stream-ref cosine-inverse 4))
  (println (stream-ref cosine-inverse 5))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.62                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.62 ()
  (import scheme debug ex3.50 ex3.59 ex3.60 ex3.61)
  (import (only chicken error))
  (title "ex3.62")

  (define (div-series s1 s2)
    (let ((c (stream-car s2)))
      (if (zero? c)
          (error "Zero denominator constant term -- DIV-SERIES")
          (let ((c-inverse (/ 1 c)))
            (scale-stream
              (mul-series s1 (invert-unit-series (scale-stream s2 c-inverse)))
              c-inverse)))))

  (define tan-series (div-series sine-series cosine-series))

  (println (stream-ref tan-series 0)) ; 0
  (println (stream-ref tan-series 1)) ; 1
  (println (stream-ref tan-series 2)) ; 0.0
  (println (stream-ref tan-series 3)) ; 0.33333
  (println (stream-ref tan-series 4)) ; 0.0
  (println (stream-ref tan-series 5)) ; 0.1333333

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.63 (incomplete)                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Every force in this new implementation will have to execute (sqrt-stream x)
; because it's a function call not just a variable, effectively disabling
; memorization.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.64                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.64 ()
  (import scheme debug ex3.50)
  (title "ex3.64")

  (define (stream-cadr s)
    (stream-car (stream-cdr s)))

  (define (stream-limit s t)
    (let ((elem1 (stream-car s))
          (elem2 (stream-cadr s)))
      (if (< (abs (- elem1 elem2)) t)
        elem2
        (stream-limit (stream-cdr s) t))))

  (define (average x y)
    (/ (+ x y) 2))

  (define (sqrt-improve guess x)
    (average guess (/ x guess)))

  (define (sqrt-stream x)
    (define guesses
      (stream-cons 1.0
                   (stream-map (lambda (guess)
                                 (sqrt-improve guess x))
                               guesses)))
    guesses)

  (define (sqrt-tolerance x tolerance)
    (stream-limit (sqrt-stream x) tolerance))

  (println (sqrt-tolerance 2 0.01)) ; 1.41421568627451

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.65                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.65 ()
  (import scheme debug ex3.50 ex3.55)
  (title "ex3.65")

  (define (square x) (* x x))

  (define (ln-2-summands n)
    (stream-cons (/ 1.0 n)
                 (stream-map - (ln-2-summands (+ n 1)))))

  (define ln-2-stream1
    (partial-sums (ln-2-summands 1)))

  (println (stream-ref ln-2-stream1 0))
  (println (stream-ref ln-2-stream1 1))
  (println (stream-ref ln-2-stream1 2))
  (println (stream-ref ln-2-stream1 3))
  (println (stream-ref ln-2-stream1 4))
  (println (stream-ref ln-2-stream1 5))
  (println (stream-ref ln-2-stream1 6))

  (define (euler-transform s)
    (let ((s0 (stream-ref s 0))           ; Sn-1
          (s1 (stream-ref s 1))           ; Sn
          (s2 (stream-ref s 2)))          ; Sn+1
      (stream-cons (- s2 (/ (square (- s2 s1))
                            (+ s0 (* -2 s1) s2)))
                   (euler-transform (stream-cdr s)))))

  (define ln-2-stream2 (euler-transform ln-2-stream1))

  (println "")
  (println (stream-ref ln-2-stream2 0))
  (println (stream-ref ln-2-stream2 1))
  (println (stream-ref ln-2-stream2 2))
  (println (stream-ref ln-2-stream2 3))
  (println (stream-ref ln-2-stream2 4))
  (println (stream-ref ln-2-stream2 5))
  (println (stream-ref ln-2-stream2 6))

  (define (make-tableau transform s)
    (stream-cons s
                 (make-tableau transform
                               (transform s))))

  (define (accelerated-sequence transform s)
    (stream-map stream-car
                (make-tableau transform s)))

  (define ln-2-stream3 (accelerated-sequence euler-transform ln-2-stream1))

  (println "")
  (println (stream-ref ln-2-stream3 0))
  (println (stream-ref ln-2-stream3 1))
  (println (stream-ref ln-2-stream3 2))
  (println (stream-ref ln-2-stream3 3))
  (println (stream-ref ln-2-stream3 4))
  (println (stream-ref ln-2-stream3 5))
  (println (stream-ref ln-2-stream3 6))

  ; ln-2-stream3 converges faster than ln-2-stream2 faster than ln-2-stream1

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.66                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.66 (pairs interleave)
  (import scheme debug ex3.50)
  (title "ex3.66")

  (define (interleave s1 s2)
    (if (stream-null? s1)
        s2
        (stream-cons (stream-car s1)
                     (interleave s2 (stream-cdr s1)))))

  (define (pairs s t)
    (stream-cons
      (list (stream-car s) (stream-car t))
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (pairs (stream-cdr s) (stream-cdr t)))))

  (define nat-pairs (pairs naturals naturals))

  (println (stream-ref nat-pairs 0))
  (println (stream-ref nat-pairs 1))
  (println (stream-ref nat-pairs 2))
  (println (stream-ref nat-pairs 3))
  (println (stream-ref nat-pairs 4))
  (println (stream-ref nat-pairs 5))
  (println (stream-ref nat-pairs 6))
  (println (stream-ref nat-pairs 7))
  (println (stream-ref nat-pairs 8))

  ; Pairs show up in the stream by interleaving every column with the first row.
  ; When a column is up, interleaving moves on to the next column and so on.
  ; (1, 100) would be preceded by 197 pairs (2*(n-2)+1).
  ; (99, 100) would be preceded by 100*99/2 = 4950 (size of triangle above and
  ; to the left of that pair) + (4950 - 100) (number of pairs in the first row
  ; after that pair) = 9800 pairs.
  ; (100, 100) would be preceded by 2*5000 - 100 = 9900 pairs.

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.67                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.67 ()
  (import scheme debug ex3.50)
  (title "ex3.67")

  (define (pairs s t)
    (define (iter index sum-indexes)
      (stream-cons
        (list (stream-ref s index) (stream-ref t (- sum-indexes index)))
        (iter
          (if (= index sum-indexes) 0 (+ index 1))
          (if (= index sum-indexes) (+ sum-indexes 1) sum-indexes))))
    (iter 0 0))

  (define nat-pairs (pairs naturals naturals))

  (println (stream-ref nat-pairs 0))
  (println (stream-ref nat-pairs 1))
  (println (stream-ref nat-pairs 2))
  (println (stream-ref nat-pairs 3))
  (println (stream-ref nat-pairs 4))
  (println (stream-ref nat-pairs 5))
  (println (stream-ref nat-pairs 6))
  (println (stream-ref nat-pairs 7))
  (println (stream-ref nat-pairs 8))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.68                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.68 ()
  (import scheme debug ex3.50)
  (import (only ex3.66 interleave))
  (title "ex3.68")

  (define (pairs s t)
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x))
                  t)
      (pairs (stream-cdr s) (stream-cdr t))))

  ; this implementation will loop forever trying to compute the first pair.

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.69                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.69 ()
  (import scheme debug ex3.50 ex3.66)
  (title "ex3.69")

  (define (triples s t u)
    (stream-cons
      (list (stream-car s) (stream-car t) (stream-car u))
      (interleave
        (stream-map (lambda (x) (cons (stream-car s) x))
                    (pairs (stream-cdr t) (stream-cdr u)))
        (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

  (define (square x) (* x x))

  (define pythagorean-triples
    (stream-filter (lambda (x) (eq? (+ (square (car x)) (square (cadr x)))
                                    (square (caddr x))))
                   (triples naturals naturals naturals)))

  (println (stream-ref pythagorean-triples 0))
  (println (stream-ref pythagorean-triples 1))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.70                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.70 (weighted-pairs)
  (import scheme debug ex3.50)
  (title "ex3.70")

  (define (merge-weighted s1 s2 weight)
    (cond ((stream-null? s1) s2)
          ((stream-null? s2) s1)
          (else
           (let ((s1car (stream-car s1))
                 (s2car (stream-car s2)))
             (cond ((< (weight s1car) (weight s2car))
                    (stream-cons s1car (merge-weighted (stream-cdr s1) s2 weight)))
                   ((> (weight s1car) (weight s2car))
                    (stream-cons s2car (merge-weighted s1 (stream-cdr s2) weight)))
                   (else
                    (stream-cons s1car
                                 (merge-weighted (stream-cdr s1)
                                                 (stream-cdr s2) weight))))))))

  (define (weighted-pairs s t weight)
    (stream-cons
      (list (stream-car s) (stream-car t))
      (merge-weighted
        (stream-map (lambda (x) (list (stream-car s) x))
                    (stream-cdr t))
        (weighted-pairs (stream-cdr s) (stream-cdr t) weight) weight)))

  ; (a)

  (define (order1-weight p)
    (+ (car p) (cadr p)))

  (define order1 (weighted-pairs naturals naturals order1-weight))

  (println (stream-ref order1 0))
  (println (stream-ref order1 1))
  (println (stream-ref order1 2))
  (println (stream-ref order1 3))
  (println (stream-ref order1 4))
  (println (stream-ref order1 5))
  (println (stream-ref order1 6))
  (println (stream-ref order1 7))
  (println (stream-ref order1 8))

  ; (b)

  (define (order2-weight p)
    (+ (* 2 (car p)) (* 3 (cadr p)) (* 5 (car p) (cadr p))))

  (define order2 (weighted-pairs naturals naturals order2-weight))

  (newline)
  (println (stream-ref order2 0))
  (println (stream-ref order2 1))
  (println (stream-ref order2 2))
  (println (stream-ref order2 3))
  (println (stream-ref order2 4))
  (println (stream-ref order2 5))
  (println (stream-ref order2 6))
  (println (stream-ref order2 7))
  (println (stream-ref order2 8))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.71                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.71 ()
  (import scheme debug ex3.50 ex3.70)
  (title "ex3.71")

  (define (cube x)
    (* x x x))

  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set)))))

  (define (adjoin-set x set) (cons x set))

  (define (ramanujan-weight p)
    (+ (cube (car p)) (cube (cadr p))))


  (define ramanujan-weights
    (stream-map ramanujan-weight
                (weighted-pairs naturals naturals ramanujan-weight)))

  (define (ramanujan-stream set-of-weights weights)
    (let ((current-weight (stream-car weights)))
      (cond ((stream-null? weights) the-empty-stream)
            ((element-of-set? current-weight set-of-weights)
              (stream-cons current-weight
                            (ramanujan-stream set-of-weights
                                          (stream-cdr weights))))
            (else (ramanujan-stream (adjoin-set current-weight set-of-weights)
                                    (stream-cdr weights))))))

  (define ramanujan-numbers (ramanujan-stream '() ramanujan-weights))

  (println (stream-ref ramanujan-numbers 0))
  (println (stream-ref ramanujan-numbers 1))
  (println (stream-ref ramanujan-numbers 2))
  (println (stream-ref ramanujan-numbers 3))
  (println (stream-ref ramanujan-numbers 4))
  (println (stream-ref ramanujan-numbers 5))

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.72                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.72 ()
  (import scheme debug ex3.50)
  (title "ex3.72")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.73                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.73 ()
  (import scheme debug ex3.50)
  (title "ex3.73")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.74                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.74 ()
  (import scheme debug ex3.50)
  (title "ex3.74")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.75                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.75 ()
  (import scheme debug ex3.50)
  (title "ex3.75")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.76                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.76 ()
  (import scheme debug ex3.50)
  (title "ex3.76")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.77                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.77 ()
  (import scheme debug ex3.50)
  (title "ex3.77")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.78                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.78 ()
  (import scheme debug ex3.50)
  (title "ex3.78")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.79                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.79 ()
  (import scheme debug ex3.50)
  (title "ex3.79")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.80                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.80 ()
  (import scheme debug ex3.50)
  (title "ex3.80")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.81                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.81 ()
  (import scheme debug ex3.50)
  (title "ex3.81")

  (newline))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex3.82                                                                     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module ex3.82 ()
  (import scheme debug ex3.50)
  (title "ex3.82")

  (newline))



