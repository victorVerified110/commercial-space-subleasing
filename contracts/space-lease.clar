;; Commercial Space Subleasing Smart Contract
;; Provides office space listings, lease creation with availability constraints,
;; tenant screening approvals, and facility ticket management.

;; ----------------------------
;; constants / error codes
;; ----------------------------
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-SPACE-NOT-FOUND (err u101))
(define-constant ERR-SPACE-INACTIVE (err u102))
(define-constant ERR-NOT-OWNER (err u103))
(define-constant ERR-TENANT-NOT-APPROVED (err u104))
(define-constant ERR-DURATION-OUT-OF-BOUNDS (err u105))
(define-constant ERR-START-BEFORE-AVAILABLE (err u106))
(define-constant ERR-TICKET-NOT-FOUND (err u107))
(define-constant ERR-NOT-CURRENT-TENANT (err u108))
(define-constant ERR-INVALID-PARAMS (err u109))
(define-constant ERR-TRANSFER-FAILED (err u110))

;; ----------------------------
;; data vars (id counters)
;; ----------------------------
(define-data-var next-space-id uint u1)
(define-data-var next-lease-id uint u1)
(define-data-var next-ticket-id uint u1)

;; ----------------------------
;; data maps
;; ----------------------------
;; space metadata
(define-map spaces
  {space-id: uint}
  {
    owner: principal,
    name: (string-utf8 64),
    capacity: uint,
    price-per-block: uint,
    min-duration: uint,
    max-duration: uint,
    security-deposit: uint,
    active: bool
  }
)

;; per-space next available block height (sequential booking model)
(define-map space-next-available {space-id: uint} {next-available: uint})

;; approvals: space owner must approve tenant before lease
(define-map approvals {space-id: uint, tenant: principal} {approved: bool})

;; current active lease per space (single active lease model)
(define-map current-tenant
  {space-id: uint}
  {tenant: principal, start: uint, end: uint})

;; leases registry by id (for auditability)
(define-map leases
  {lease-id: uint}
  {
    space-id: uint,
    tenant: principal,
    start: uint,
    end: uint,
    rent: uint,
    deposit: uint
  })

;; facility tickets by id
(define-map tickets
  {ticket-id: uint}
  {
    space-id: uint,
    opened-by: principal,
    description: (string-utf8 200),
    status: uint
  }) ;; 0=open, 1=closed

;; ----------------------------
;; private helpers
;; ----------------------------
(define-private (is-some? (x (optional bool)))
  (is-some x)
)

(define-read-only (get-space (space-id uint))
  (map-get? spaces {space-id: space-id})
)

(define-read-only (get-next-available (space-id uint))
  (default-to u0 (get next-available (map-get? space-next-available {space-id: space-id})))
)

(define-read-only (get-approval (space-id uint) (tenant principal))
  (default-to false (get approved (map-get? approvals {space-id: space-id, tenant: tenant})))
)

(define-read-only (get-current-tenant (space-id uint))
  (map-get? current-tenant {space-id: space-id})
)

(define-read-only (get-ticket (ticket-id uint))
  (map-get? tickets {ticket-id: ticket-id})
)

(define-private (require-space-exists (space-id uint))
  (match (map-get? spaces {space-id: space-id})
    space (ok space)
    (err ERR-SPACE-NOT-FOUND)
  )
)

(define-private (only-space-owner (space-id uint))
  (let ((space (unwrap! (require-space-exists space-id) (err u999))))
    (if (is-eq tx-sender (get owner space))
        (ok true)
        ERR-NOT-OWNER))
)

(define-private (compute-rent (price-per-block uint) (duration uint))
  (* price-per-block duration)
)

(define-private (transfer-or-fail (amount uint) (recipient principal))
  (if (is-ok (stx-transfer? amount tx-sender recipient))
      (ok true)
      ERR-TRANSFER-FAILED))

;; ----------------------------
;; public functions
;; ----------------------------
(define-public (register-space (name (string-utf8 64)) (capacity uint) (price-per-block uint) (min-duration uint) (max-duration uint) (security-deposit uint))
  (begin
    (if (or (<= max-duration u0) (< max-duration min-duration) (<= capacity u0))
        ERR-INVALID-PARAMS
        (let ((sid (var-get next-space-id)))
          (map-set spaces {space-id: sid}
            {
              owner: tx-sender,
              name: name,
              capacity: capacity,
              price-per-block: price-per-block,
              min-duration: min-duration,
              max-duration: max-duration,
              security-deposit: security-deposit,
              active: true
            })
          (map-set space-next-available {space-id: sid} {next-available: stacks-block-height})
          (var-set next-space-id (+ sid u1))
          (ok sid)
        )
    )
  )
)

(define-public (set-space-active (space-id uint) (active bool))
  (begin
    (asserts! (is-ok (only-space-owner space-id)) ERR-NOT-OWNER)
    (let ((space (unwrap! (map-get? spaces {space-id: space-id}) ERR-SPACE-NOT-FOUND)))
      (map-set spaces {space-id: space-id}
        (merge space {active: active}))
      (ok active)
    )
  )
)

(define-public (approve-tenant (space-id uint) (tenant principal) (approved bool))
  (begin
    (asserts! (is-ok (only-space-owner space-id)) ERR-NOT-OWNER)
    (map-set approvals {space-id: space-id, tenant: tenant} {approved: approved})
    (ok approved)
  )
)

(define-public (create-lease (space-id uint) (start uint) (duration uint))
  (let (
        (space (unwrap! (map-get? spaces {space-id: space-id}) ERR-SPACE-NOT-FOUND))
        (next-avail (get-next-available space-id))
      )
    (begin
      (asserts! (get active space) ERR-SPACE-INACTIVE)
      (asserts! (is-eq (get-approval space-id tx-sender) true) ERR-TENANT-NOT-APPROVED)
      (asserts! (and (>= duration (get min-duration space)) (<= duration (get max-duration space))) ERR-DURATION-OUT-OF-BOUNDS)
      (asserts! (>= start next-avail) ERR-START-BEFORE-AVAILABLE)
      (let (
            (end (+ start duration))
            (rent (compute-rent (get price-per-block space) duration))
            (deposit (get security-deposit space))
            (owner (get owner space))
            (lid (var-get next-lease-id))
          )
        (begin
          ;; transfer rent + deposit to space owner (simple model)
          (asserts! (is-ok (transfer-or-fail (+ rent deposit) owner)) ERR-TRANSFER-FAILED)
          (map-set leases {lease-id: lid}
            { space-id: space-id, tenant: tx-sender, start: start, end: end, rent: rent, deposit: deposit })
          (map-set current-tenant {space-id: space-id} {tenant: tx-sender, start: start, end: end})
          (map-set space-next-available {space-id: space-id} {next-available: end})
          (var-set next-lease-id (+ lid u1))
          (ok lid)
        )
      )
    )
  )
)

(define-public (open-ticket (space-id uint) (description (string-utf8 200)))
  (let (
        (cur (map-get? current-tenant {space-id: space-id}))
      )
    (match cur
      ct
      (let ((tenant (get tenant ct)) (start (get start ct)) (end (get end ct)))
        (begin
          (asserts! (is-eq tenant tx-sender) ERR-NOT-CURRENT-TENANT)
          (asserts! (and (>= stacks-block-height start) (<= stacks-block-height end)) ERR-NOT-CURRENT-TENANT)
          (let ((tid (var-get next-ticket-id)))
            (map-set tickets {ticket-id: tid}
              {space-id: space-id, opened-by: tx-sender, description: description, status: u0})
            (var-set next-ticket-id (+ tid u1))
            (ok tid)
          )
        )
      )
      (begin ERR-NOT-CURRENT-TENANT)
    )
  )
)

(define-public (close-ticket (ticket-id uint))
  (let ((t (map-get? tickets {ticket-id: ticket-id})))
    (match t
      tkt
      (let ((sid (get space-id tkt))
            (space (unwrap! (map-get? spaces {space-id: (get space-id tkt)}) ERR-SPACE-NOT-FOUND)))
        (begin
          (asserts! (is-eq tx-sender (get owner space)) ERR-NOT-OWNER)
          (map-set tickets {ticket-id: ticket-id} (merge tkt {status: u1}))
          (ok u1)
        )
      )
      ERR-TICKET-NOT-FOUND
    )
  )
)

;; ----------------------------
;; read-only functions (exposed)
;; ----------------------------
(define-read-only (get-lease (lease-id uint))
  (map-get? leases {lease-id: lease-id})
)
