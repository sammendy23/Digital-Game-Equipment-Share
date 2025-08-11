;; contracts/equipment-share.clar
;; Digital Game Equipment Share - Complete System

(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_EQUIPMENT_NOT_FOUND (err u101))
(define-constant ERR_EQUIPMENT_UNAVAILABLE (err u102))
(define-constant ERR_ALREADY_CHECKED_OUT (err u103))
(define-constant ERR_NOT_CHECKED_OUT (err u104))
(define-constant ERR_MAX_LOANS_REACHED (err u105))
(define-constant ERR_USER_HAS_OVERDUE (err u106))

(define-data-var next-equipment-id uint u1)
(define-data-var next-loan-id uint u1)
(define-data-var max-loans-per-user uint u3)
(define-data-var default-loan-duration uint u144)

(define-map equipment-registry
  { equipment-id: uint }
  {
    name: (string-ascii 50),
    category: (string-ascii 20),
    condition: (string-ascii 10),
    is-available: bool,
    last-maintenance: uint,
    maintenance-interval: uint
  }
)

(define-map equipment-checkouts
  { equipment-id: uint }
  {
    borrower: principal,
    checkout-block: uint,
    due-block: uint
  }
)

(define-map user-profiles
  { user: principal }
  {
    active-loans: uint,
    total-loans: uint,
    overdue-count: uint,
    reputation-score: uint
  }
)

(define-read-only (get-equipment (equipment-id uint))
  (map-get? equipment-registry { equipment-id: equipment-id })
)

(define-read-only (get-checkout-info (equipment-id uint))
  (map-get? equipment-checkouts { equipment-id: equipment-id })
)

(define-read-only (get-user-profile (user principal))
  (default-to
    { active-loans: u0, total-loans: u0, overdue-count: u0, reputation-score: u100 }
    (map-get? user-profiles { user: user })
  )
)

(define-read-only (is-maintenance-due (equipment-id uint))
  (match (get-equipment equipment-id)
    equipment-data
    (let ((last-maintenance (get last-maintenance equipment-data))
          (maintenance-interval (get maintenance-interval equipment-data))
          (current-block stacks-block-height))
      (>= current-block (+ last-maintenance maintenance-interval)))
    false
  )
)

(define-read-only (can-borrow (user principal))
  (let ((profile (get-user-profile user)))
    (and
      (< (get active-loans profile) (var-get max-loans-per-user))
      (is-eq (get overdue-count profile) u0)
    )
  )
)

(define-public (add-equipment (name (string-ascii 50)) (category (string-ascii 20)) (condition (string-ascii 10)) (maintenance-interval uint))
  (let ((equipment-id (var-get next-equipment-id)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set equipment-registry
      { equipment-id: equipment-id }
      {
        name: name,
        category: category,
        condition: condition,
        is-available: true,
        last-maintenance: stacks-block-height,
        maintenance-interval: maintenance-interval
      }
    )
    (var-set next-equipment-id (+ equipment-id u1))
    (ok equipment-id)
  )
)

(define-public (borrow-equipment (equipment-id uint))
  (let ((equipment-data (unwrap! (get-equipment equipment-id) ERR_EQUIPMENT_NOT_FOUND))
        (user-profile (get-user-profile tx-sender))
        (loan-id (var-get next-loan-id)))
    (asserts! (get is-available equipment-data) ERR_EQUIPMENT_UNAVAILABLE)
    (asserts! (is-none (get-checkout-info equipment-id)) ERR_ALREADY_CHECKED_OUT)
    (asserts! (can-borrow tx-sender) ERR_MAX_LOANS_REACHED)

    (map-set equipment-checkouts
      { equipment-id: equipment-id }
      {
        borrower: tx-sender,
        checkout-block: stacks-block-height,
        due-block: (+ stacks-block-height (var-get default-loan-duration))
      }
    )
    (map-set equipment-registry
      { equipment-id: equipment-id }
      (merge equipment-data { is-available: false })
    )
    (map-set user-profiles
      { user: tx-sender }
      (merge user-profile {
        active-loans: (+ (get active-loans user-profile) u1),
        total-loans: (+ (get total-loans user-profile) u1)
      })
    )
    (var-set next-loan-id (+ loan-id u1))
    (ok loan-id)
  )
)

(define-public (return-equipment (equipment-id uint) (new-condition (string-ascii 10)))
  (let ((checkout-info (unwrap! (get-checkout-info equipment-id) ERR_NOT_CHECKED_OUT))
        (equipment-data (unwrap! (get-equipment equipment-id) ERR_EQUIPMENT_NOT_FOUND))
        (user-profile (get-user-profile tx-sender)))
    (asserts! (is-eq tx-sender (get borrower checkout-info)) ERR_UNAUTHORIZED)

    (map-delete equipment-checkouts { equipment-id: equipment-id })
    (map-set equipment-registry
      { equipment-id: equipment-id }
      (merge equipment-data {
        is-available: true,
        condition: new-condition
      })
    )
    (map-set user-profiles
      { user: tx-sender }
      (merge user-profile {
        active-loans: (- (get active-loans user-profile) u1)
      })
    )
    (ok true)
  )
)

(define-public (update-maintenance (equipment-id uint))
  (let ((equipment-data (unwrap! (get-equipment equipment-id) ERR_EQUIPMENT_NOT_FOUND)))
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (map-set equipment-registry
      { equipment-id: equipment-id }
      (merge equipment-data { last-maintenance: stacks-block-height })
    )
    (ok true)
  )
)
