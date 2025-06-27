;; Retention Strategy Contract
;; Develops and manages customer retention strategies

;; Constants
(define-constant ERR-STRATEGY-NOT-FOUND (err u300))
(define-constant ERR-INVALID-PARAMETERS (err u301))
(define-constant ERR-NOT-STRATEGY-OWNER (err u302))
(define-constant ERR-STRATEGY-INACTIVE (err u303))

;; Data Variables
(define-data-var next-strategy-id uint u1)

;; Data Maps
(define-map strategies
  { strategy-id: uint }
  {
    owner: principal,
    name: (string-ascii 50),
    target-segment: (string-ascii 30),
    investment-required: uint,
    expected-roi: uint,
    active: bool,
    created-at: uint
  }
)

(define-map strategy-performance
  { strategy-id: uint }
  {
    customers-targeted: uint,
    customers-retained: uint,
    total-investment: uint,
    revenue-generated: uint,
    actual-roi: uint
  }
)

(define-map strategy-tokens
  { strategy-id: uint, holder: principal }
  { token-amount: uint }
)

;; Public Functions

;; Create new retention strategy
(define-public (create-strategy
  (name (string-ascii 50))
  (target-segment (string-ascii 30))
  (investment-required uint)
  (expected-roi uint))
  (let
    (
      (strategy-id (var-get next-strategy-id))
    )
    (asserts! (> investment-required u0) ERR-INVALID-PARAMETERS)
    (asserts! (> expected-roi u0) ERR-INVALID-PARAMETERS)

    (map-set strategies
      { strategy-id: strategy-id }
      {
        owner: tx-sender,
        name: name,
        target-segment: target-segment,
        investment-required: investment-required,
        expected-roi: expected-roi,
        active: true,
        created-at: block-height
      }
    )

    ;; Issue strategy tokens to creator
    (map-set strategy-tokens
      { strategy-id: strategy-id, holder: tx-sender }
      { token-amount: u1000 }
    )

    (var-set next-strategy-id (+ strategy-id u1))
    (ok strategy-id)
  )
)

;; Update strategy performance
(define-public (update-performance
  (strategy-id uint)
  (customers-targeted uint)
  (customers-retained uint)
  (total-investment uint)
  (revenue-generated uint))
  (let
    (
      (strategy (unwrap! (map-get? strategies { strategy-id: strategy-id }) ERR-STRATEGY-NOT-FOUND))
      (actual-roi (if (> total-investment u0) (/ (* revenue-generated u100) total-investment) u0))
    )
    (asserts! (is-eq tx-sender (get owner strategy)) ERR-NOT-STRATEGY-OWNER)
    (asserts! (get active strategy) ERR-STRATEGY-INACTIVE)

    (map-set strategy-performance
      { strategy-id: strategy-id }
      {
        customers-targeted: customers-targeted,
        customers-retained: customers-retained,
        total-investment: total-investment,
        revenue-generated: revenue-generated,
        actual-roi: actual-roi
      }
    )
    (ok true)
  )
)

;; Transfer strategy tokens
(define-public (transfer-tokens (strategy-id uint) (recipient principal) (amount uint))
  (let
    (
      (sender-balance (default-to u0 (get token-amount (map-get? strategy-tokens { strategy-id: strategy-id, holder: tx-sender }))))
      (recipient-balance (default-to u0 (get token-amount (map-get? strategy-tokens { strategy-id: strategy-id, holder: recipient }))))
    )
    (asserts! (>= sender-balance amount) ERR-INVALID-PARAMETERS)

    ;; Update sender balance
    (map-set strategy-tokens
      { strategy-id: strategy-id, holder: tx-sender }
      { token-amount: (- sender-balance amount) }
    )

    ;; Update recipient balance
    (map-set strategy-tokens
      { strategy-id: strategy-id, holder: recipient }
      { token-amount: (+ recipient-balance amount) }
    )
    (ok true)
  )
)

;; Deactivate strategy
(define-public (deactivate-strategy (strategy-id uint))
  (let
    (
      (strategy (unwrap! (map-get? strategies { strategy-id: strategy-id }) ERR-STRATEGY-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get owner strategy)) ERR-NOT-STRATEGY-OWNER)

    (map-set strategies
      { strategy-id: strategy-id }
      (merge strategy { active: false })
    )
    (ok true)
  )
)

;; Read-only Functions

;; Get strategy details
(define-read-only (get-strategy (strategy-id uint))
  (map-get? strategies { strategy-id: strategy-id })
)

;; Get strategy performance
(define-read-only (get-strategy-performance (strategy-id uint))
  (map-get? strategy-performance { strategy-id: strategy-id })
)

;; Get token balance
(define-read-only (get-token-balance (strategy-id uint) (holder principal))
  (default-to u0 (get token-amount (map-get? strategy-tokens { strategy-id: strategy-id, holder: holder })))
)

;; Check if strategy is active
(define-read-only (is-strategy-active (strategy-id uint))
  (match (map-get? strategies { strategy-id: strategy-id })
    strategy (get active strategy)
    false
  )
)
