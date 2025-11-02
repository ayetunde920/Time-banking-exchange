;; Time Banking Exchange - Time Credit Manager
;; Track time credits earned and spent, match service requests, verify completion,
;; manage member accounts, and facilitate exchanges

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-invalid-input (err u104))
(define-constant err-insufficient-credits (err u105))
(define-constant err-invalid-status (err u106))
(define-constant err-already-completed (err u107))

;; Data Variables
(define-data-var service-nonce uint u0)
(define-data-var exchange-nonce uint u0)

;; Data Maps

;; Member accounts
(define-map members
  { member: principal }
  {
    name: (string-utf8 200),
    credits-earned: uint,
    credits-spent: uint,
    credits-available: uint,
    joined-at: uint,
    is-active: bool,
    rating: uint,
    total-ratings: uint
  }
)

;; Service offerings
(define-map services
  { service-id: uint }
  {
    provider: principal,
    title: (string-utf8 200),
    description: (string-utf8 1000),
    category: (string-utf8 100),
    credits-per-hour: uint,
    is-available: bool,
    created-at: uint
  }
)

;; Service requests
(define-map service-requests
  { request-id: uint }
  {
    requester: principal,
    service-id: uint,
    hours-requested: uint,
    total-credits: uint,
    status: (string-ascii 20),
    created-at: uint,
    accepted-by: (optional principal),
    accepted-at: (optional uint)
  }
)

;; Exchange records
(define-map exchanges
  { exchange-id: uint }
  {
    provider: principal,
    receiver: principal,
    service-id: uint,
    hours: uint,
    credits: uint,
    completed-at: uint,
    verified-at: (optional uint),
    verified-by: (optional principal),
    rating: (optional uint),
    feedback: (optional (string-utf8 500))
  }
)

;; Member service offerings mapping
(define-map member-services
  { member: principal, service-id: uint }
  { exists: bool }
)

;; Service request tracking
(define-map request-tracking
  { request-id: uint, participant: principal }
  { role: (string-ascii 20) }
)

;; Exchange counts
(define-map member-exchange-count
  { member: principal }
  { count: uint }
)

;; Read-only functions

(define-read-only (get-member (member principal))
  (map-get? members { member: member })
)

(define-read-only (get-service (service-id uint))
  (map-get? services { service-id: service-id })
)

(define-read-only (get-service-request (request-id uint))
  (map-get? service-requests { request-id: request-id })
)

(define-read-only (get-exchange (exchange-id uint))
  (map-get? exchanges { exchange-id: exchange-id })
)

(define-read-only (get-member-balance (member principal))
  (default-to
    { credits-available: u0 }
    (map-get? members { member: member })
  )
)

(define-read-only (get-member-rating (member principal))
  (let
    (
      (member-data (unwrap! (map-get? members { member: member }) err-not-found))
    )
    (if (> (get total-ratings member-data) u0)
      (ok (/ (get rating member-data) (get total-ratings member-data)))
      (ok u0)
    )
  )
)

(define-read-only (calculate-credits (hours uint) (rate uint))
  (ok (* hours rate))
)

;; Public functions

;; Register as member
(define-public (register-member (name (string-utf8 200)))
  (let
    (
      (existing-member (map-get? members { member: tx-sender }))
    )
    (asserts! (is-none existing-member) err-already-exists)
    (asserts! (> (len name) u0) err-invalid-input)
    
    (ok (map-set members
      { member: tx-sender }
      {
        name: name,
        credits-earned: u0,
        credits-spent: u0,
        credits-available: u0,
        joined-at: block-height,
        is-active: true,
        rating: u0,
        total-ratings: u0
      }
    ))
  )
)

;; Offer service
(define-public (offer-service
  (title (string-utf8 200))
  (description (string-utf8 1000))
  (category (string-utf8 100))
  (credits-per-hour uint)
)
  (let
    (
      (member-data (unwrap! (map-get? members { member: tx-sender }) err-not-found))
      (service-id (var-get service-nonce))
    )
    (asserts! (get is-active member-data) err-unauthorized)
    (asserts! (> (len title) u0) err-invalid-input)
    (asserts! (> credits-per-hour u0) err-invalid-input)
    
    (map-set services
      { service-id: service-id }
      {
        provider: tx-sender,
        title: title,
        description: description,
        category: category,
        credits-per-hour: credits-per-hour,
        is-available: true,
        created-at: block-height
      }
    )
    
    (map-set member-services
      { member: tx-sender, service-id: service-id }
      { exists: true }
    )
    
    (var-set service-nonce (+ service-id u1))
    (ok service-id)
  )
)

;; Request service
(define-public (request-service (service-id uint) (hours-requested uint))
  (let
    (
      (member-data (unwrap! (map-get? members { member: tx-sender }) err-not-found))
      (service-data (unwrap! (map-get? services { service-id: service-id }) err-not-found))
      (request-id (var-get service-nonce))
      (total-credits (* hours-requested (get credits-per-hour service-data)))
    )
    (asserts! (get is-active member-data) err-unauthorized)
    (asserts! (get is-available service-data) err-invalid-status)
    (asserts! (> hours-requested u0) err-invalid-input)
    (asserts! (not (is-eq tx-sender (get provider service-data))) err-invalid-input)
    
    (map-set service-requests
      { request-id: request-id }
      {
        requester: tx-sender,
        service-id: service-id,
        hours-requested: hours-requested,
        total-credits: total-credits,
        status: "pending",
        created-at: block-height,
        accepted-by: none,
        accepted-at: none
      }
    )
    
    (map-set request-tracking
      { request-id: request-id, participant: tx-sender }
      { role: "requester" }
    )
    
    (var-set service-nonce (+ request-id u1))
    (ok request-id)
  )
)

;; Accept service request
(define-public (accept-service-request (request-id uint))
  (let
    (
      (request-data (unwrap! (map-get? service-requests { request-id: request-id }) err-not-found))
      (service-data (unwrap! (map-get? services { service-id: (get service-id request-data) }) err-not-found))
      (member-data (unwrap! (map-get? members { member: tx-sender }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get provider service-data)) err-unauthorized)
    (asserts! (is-eq (get status request-data) "pending") err-invalid-status)
    (asserts! (get is-active member-data) err-unauthorized)
    
    (map-set service-requests
      { request-id: request-id }
      (merge request-data {
        status: "accepted",
        accepted-by: (some tx-sender),
        accepted-at: (some block-height)
      })
    )
    
    (map-set request-tracking
      { request-id: request-id, participant: tx-sender }
      { role: "provider" }
    )
    
    (ok true)
  )
)

;; Complete exchange
(define-public (complete-exchange (request-id uint))
  (let
    (
      (request-data (unwrap! (map-get? service-requests { request-id: request-id }) err-not-found))
      (service-data (unwrap! (map-get? services { service-id: (get service-id request-data) }) err-not-found))
      (provider-data (unwrap! (map-get? members { member: (get provider service-data) }) err-not-found))
      (requester-data (unwrap! (map-get? members { member: (get requester request-data) }) err-not-found))
      (exchange-id (var-get exchange-nonce))
      (credits (get total-credits request-data))
    )
    (asserts! (is-eq tx-sender (get provider service-data)) err-unauthorized)
    (asserts! (is-eq (get status request-data) "accepted") err-invalid-status)
    
    ;; Update provider credits (earned)
    (map-set members
      { member: (get provider service-data) }
      (merge provider-data {
        credits-earned: (+ (get credits-earned provider-data) credits),
        credits-available: (+ (get credits-available provider-data) credits)
      })
    )
    
    ;; Update requester credits (spent)
    (map-set members
      { member: (get requester request-data) }
      (merge requester-data {
        credits-spent: (+ (get credits-spent requester-data) credits)
      })
    )
    
    ;; Record exchange
    (map-set exchanges
      { exchange-id: exchange-id }
      {
        provider: (get provider service-data),
        receiver: (get requester request-data),
        service-id: (get service-id request-data),
        hours: (get hours-requested request-data),
        credits: credits,
        completed-at: block-height,
        verified-at: none,
        verified-by: none,
        rating: none,
        feedback: none
      }
    )
    
    ;; Update request status
    (map-set service-requests
      { request-id: request-id }
      (merge request-data { status: "completed" })
    )
    
    (var-set exchange-nonce (+ exchange-id u1))
    (ok exchange-id)
  )
)

;; Verify and rate exchange
(define-public (verify-exchange
  (exchange-id uint)
  (rating uint)
  (feedback (string-utf8 500))
)
  (let
    (
      (exchange-data (unwrap! (map-get? exchanges { exchange-id: exchange-id }) err-not-found))
      (provider-data (unwrap! (map-get? members { member: (get provider exchange-data) }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get receiver exchange-data)) err-unauthorized)
    (asserts! (is-none (get verified-at exchange-data)) err-already-completed)
    (asserts! (<= rating u5) err-invalid-input)
    
    ;; Update exchange with verification
    (map-set exchanges
      { exchange-id: exchange-id }
      (merge exchange-data {
        verified-at: (some block-height),
        verified-by: (some tx-sender),
        rating: (some rating),
        feedback: (some feedback)
      })
    )
    
    ;; Update provider rating
    (map-set members
      { member: (get provider exchange-data) }
      (merge provider-data {
        rating: (+ (get rating provider-data) rating),
        total-ratings: (+ (get total-ratings provider-data) u1)
      })
    )
    
    (ok true)
  )
)

;; Spend credits
(define-public (spend-credits (amount uint))
  (let
    (
      (member-data (unwrap! (map-get? members { member: tx-sender }) err-not-found))
    )
    (asserts! (>= (get credits-available member-data) amount) err-insufficient-credits)
    
    (ok (map-set members
      { member: tx-sender }
      (merge member-data {
        credits-available: (- (get credits-available member-data) amount),
        credits-spent: (+ (get credits-spent member-data) amount)
      })
    ))
  )
)

;; Deactivate service
(define-public (deactivate-service (service-id uint))
  (let
    (
      (service-data (unwrap! (map-get? services { service-id: service-id }) err-not-found))
    )
    (asserts! (is-eq tx-sender (get provider service-data)) err-unauthorized)
    
    (ok (map-set services
      { service-id: service-id }
      (merge service-data { is-available: false })
    ))
  )
)


;; title: time-credit-manager
;; version:
;; summary:
;; description:

;; traits
;;

;; token definitions
;;

;; constants
;;

;; data vars
;;

;; data maps
;;

;; public functions
;;

;; read only functions
;;

;; private functions
;;

