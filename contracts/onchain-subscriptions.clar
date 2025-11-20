;; onchain-subscriptions.clar
;; On-chain subscription service (STX)
;; - Creators create plans: (price in microSTX, period in blocks)
;; - Users subscribe by paying first period; renew by paying again
;; - Creators withdraw accumulated earnings
;; - Subscribers can cancel; creators can deactivate plans
;; ------------------------------------------------------------

(define-constant ERR_NOT_CREATOR u100)
(define-constant ERR_PLAN_NOT_FOUND u101)
(define-constant ERR_PLAN_INACTIVE u102)
(define-constant ERR_INVALID_AMOUNT u103)
(define-constant ERR_NOT_SUBSCRIBER u104)
(define-constant ERR_ALREADY_SUBSCRIBED u105)
(define-constant ERR_NO_FUNDS u106)
(define-constant ERR_TRANSFER_FAIL u107)
(define-constant ERR_ONLY_CREATOR u108)

;; Simple IDs
(define-data-var next-plan-id uint u1)

;; Plans: id -> { creator: principal, name: (string-ascii 64), price: uint, period: uint, active: bool }
(define-map plans
  { plan-id: uint }
  {
    creator: principal,
    name: (string-ascii 64),
    price: uint,       ;; microSTX per period
    period: uint,      ;; billing period in blocks
    active: bool
  })

;; Subscriptions: { plan-id, subscriber } -> { start-block: uint, next-billing: uint, active: bool }
(define-map subscriptions
  { plan-id: uint, subscriber: principal }
  {
    start-block: uint,
    next-billing: uint,
    active: bool
  })

;; Creator earnings accrued in contract (microSTX)
(define-map creator-balances
  { who: principal } { balance: uint })

;; Map data helpers
(define-private (create-plan-data (owner principal) (name (string-ascii 64)) (price uint) (period uint) (active bool))
  ;; Input validation is done by the caller with asserts!
  { creator: owner,
    name: name,
    price: price,
    period: period,
    active: active })

(define-private (create-subscription-data (start-height uint) (next-billing-height uint) (active bool))
  ;; Input validation is done by the caller with asserts!
  { start-block: start-height,
    next-billing: next-billing-height,
    active: active })

;; Events
(define-private (ev-create-plan (id uint) (creator principal) (price uint) (period uint) (name (string-ascii 64)))
  (print { event: "plan-created", plan-id: id, creator: creator, price: price, period: period, name: name }))

(define-private (ev-update-plan (id uint) (creator principal) (price uint) (period uint) (active bool))
  (print { event: "plan-updated", plan-id: id, creator: creator, price: price, period: period, active: active }))

(define-private (ev-subscribe (plan-id uint) (subscriber principal) (next-billing uint))
  (print { event: "subscribed", plan-id: plan-id, subscriber: subscriber, next_billing: next-billing }))

(define-private (ev-renew (plan-id uint) (subscriber principal) (next-billing uint))
  (print { event: "renewed", plan-id: plan-id, subscriber: subscriber, next_billing: next-billing }))

(define-private (ev-cancel (plan-id uint) (subscriber principal))
  (print { event: "subscription-cancelled", plan-id: plan-id, subscriber: subscriber }))

(define-private (ev-withdraw (creator principal) (amount uint))
  (print { event: "withdrawn", creator: creator, amount: amount }))

;; -------------------------
;; Plan management (creator)
;; -------------------------
(define-public (create-plan (name (string-ascii 64)) (price uint) (period uint))
  (if (and (> price u0) (> period u0))
    (let ((id (var-get next-plan-id))
          (plan-data { creator: tx-sender, name: name, price: price, period: period, active: true }))
      (begin
        (var-set next-plan-id (+ id u1))
        (map-set plans { plan-id: id } plan-data)
        (print { event: "plan-created", plan-id: id, creator: tx-sender, price: price, period: period, name: name })
        (ok id)))
    (err ERR_INVALID_AMOUNT)))

(define-public (update-plan (plan-id uint) (price uint) (period uint) (active bool))
  (begin
    (asserts! (> plan-id u0) (err ERR_INVALID_AMOUNT))
    (asserts! (> price u0) (err ERR_INVALID_AMOUNT))
    (asserts! (> period u0) (err ERR_INVALID_AMOUNT))
    (let ((p? (map-get? plans { plan-id: plan-id })))
      (match p?
        p
        (begin
          (asserts! (is-eq tx-sender (get creator p)) (err ERR_ONLY_CREATOR))
          (let ((plan-data (create-plan-data (get creator p) (get name p) price period active)))
            (map-set plans { plan-id: plan-id } plan-data)
            (ev-update-plan plan-id (get creator p) price period active)
            (ok true)))
        (err ERR_PLAN_NOT_FOUND)))))

(define-public (deactivate-plan (plan-id uint))
  (begin
    (asserts! (> plan-id u0) (err ERR_INVALID_AMOUNT))
    (let ((p? (map-get? plans { plan-id: plan-id })))
      (match p?
        p
        (begin
          (asserts! (is-eq tx-sender (get creator p)) (err ERR_ONLY_CREATOR))
          (let ((plan-data (create-plan-data (get creator p) (get name p) (get price p) (get period p) false)))
            (map-set plans { plan-id: plan-id } plan-data)
            (ev-update-plan plan-id (get creator p) (get price p) (get period p) false)
            (ok true)))
        (err ERR_PLAN_NOT_FOUND)))))

;; -------------------------
;; Subscribe / Renew / Cancel
;; -------------------------
;; Subscriber must send the price with the call: contract pulls STX from tx-sender using stx-transfer? to contract.
;; We require the subscriber to transfer price to this contract in the same call.
(define-public (subscribe (plan-id uint))
  (begin
    (asserts! (> plan-id u0) (err ERR_INVALID_AMOUNT))
    (let ((p? (map-get? plans { plan-id: plan-id })))
      (match p?
        p
        (let ((creator (get creator p)) (price (get price p)) (period (get period p)) (active (get active p)))
          (begin
            (asserts! active (err ERR_PLAN_INACTIVE))
            (asserts! (> price u0) (err ERR_INVALID_AMOUNT))
            ;; collect payment into contract
            (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
            ;; update creator balance in contract
            (let ((oldb (default-to u0 (get balance (map-get? creator-balances { who: creator })))))
              (map-set creator-balances { who: creator } { balance: (+ oldb price) }))
            ;; ensure not already active subscription
            (let ((s? (map-get? subscriptions { plan-id: plan-id, subscriber: tx-sender })))
              (asserts! (is-none s?) (err ERR_ALREADY_SUBSCRIBED))
              (let ((start burn-block-height)
                    (next (+ burn-block-height period))
                    (sub-data (create-subscription-data start next true)))
                (map-set subscriptions { plan-id: plan-id, subscriber: tx-sender } sub-data)
                (ev-subscribe plan-id tx-sender next)
                (ok { plan: plan-id, next-billing: next })))))
        (err ERR_PLAN_NOT_FOUND)))))

(define-public (renew (plan-id uint))
  (begin 
    (asserts! (> plan-id u0) (err ERR_INVALID_AMOUNT))
    (let ((p? (map-get? plans { plan-id: plan-id })))
      (match p?
        p
        (let ((creator (get creator p)) (price (get price p)) (period (get period p)) (active (get active p)))
          (begin
            (asserts! active (err ERR_PLAN_INACTIVE))
            (asserts! (> price u0) (err ERR_INVALID_AMOUNT))
            (let ((s? (map-get? subscriptions { plan-id: plan-id, subscriber: tx-sender })))
              (asserts! (is-some s?) (err ERR_NOT_SUBSCRIBER))
              (let ((s (unwrap-panic s?)))
                (asserts! (get active s) (err ERR_NOT_SUBSCRIBER))
                ;; collect payment
                (try! (stx-transfer? price tx-sender (as-contract tx-sender)))
                ;; credit creator
                (let ((oldb (default-to u0 (get balance (map-get? creator-balances { who: creator })))))
                  (map-set creator-balances { who: creator } { balance: (+ oldb price) }))
                ;; compute next-billing: extend from max(existing-next, block-height)
                (let ((existing-next (get next-billing s))
                      (current-height burn-block-height)
                      (base (if (> existing-next current-height) existing-next current-height))
                      (new-next (+ base period)))
                  (let ((sub-data (create-subscription-data (get start-block s) new-next true)))
                    (map-set subscriptions { plan-id: plan-id, subscriber: tx-sender } sub-data))
                  (ev-renew plan-id tx-sender new-next)
                  (ok { plan: plan-id, next-billing: new-next }))))))
        (err ERR_PLAN_NOT_FOUND)))))

(define-public (cancel-subscription (plan-id uint))
  (begin
    (asserts! (> plan-id u0) (err ERR_INVALID_AMOUNT))
    (let ((s? (map-get? subscriptions { plan-id: plan-id, subscriber: tx-sender })))
      (match s?
        s
        (let ((sdata (unwrap-panic s?)))
          (asserts! (get active sdata) (err ERR_NOT_SUBSCRIBER))
          (let ((sub-data (create-subscription-data (get start-block sdata) (get next-billing sdata) false)))
            (map-set subscriptions { plan-id: plan-id, subscriber: tx-sender } sub-data))
          (ev-cancel plan-id tx-sender)
          (ok true))
        (err ERR_NOT_SUBSCRIBER)))))

;; -------------------------
;; Creator withdraw earnings
;; -------------------------
(define-public (withdraw-earnings)
  (let ((bal? (map-get? creator-balances { who: tx-sender })))
    (match bal?
      b
      (let ((amt (get balance b)))
        (asserts! (> amt u0) (err ERR_NO_FUNDS))
        ;; zero out before transfer
        (map-set creator-balances { who: tx-sender } { balance: u0 })
        (try! (as-contract (stx-transfer? amt tx-sender tx-sender)))
        (ev-withdraw tx-sender amt)
        (ok amt))
      (err ERR_NO_FUNDS))))

;; -------------------------
;; Read-only helpers
;; -------------------------
(define-read-only (get-plan (plan-id uint))
  (ok (map-get? plans { plan-id: plan-id })))

(define-read-only (get-subscription (plan-id uint) (subscriber principal))
  (ok (map-get? subscriptions { plan-id: plan-id, subscriber: subscriber })))

(define-read-only (get-creator-balance (who principal))
  (ok (default-to u0 (get balance (map-get? creator-balances { who: who })))))

(define-read-only (get-next-plan-id) (ok (var-get next-plan-id)))
