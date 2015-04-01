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
  (println (b 10))

  (newline))


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
  (println (msqrt 'how-many-calls?))

  (newline))


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
  (println ((acc 'secret-password 'deposit) 30))

  (newline))


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

  (println ((acc 'secret-password 'deposit) 30))

  (newline))


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

  (println pi-estimate)

  (newline))


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
  (println (rand 'generate))
  (newline))


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
  (println ((peter-acc 'open-sesame 'deposit) 40))

  (newline))


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
  (println (+ (g 1) (g 0)))

  (newline))


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
;
