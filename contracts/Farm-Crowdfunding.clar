(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-funding-closed (err u104))
(define-constant err-funding-active (err u105))
(define-constant err-insufficient-funds (err u106))
(define-constant err-min-investment (err u107))
(define-constant err-max-investment (err u108))
(define-constant err-target-not-reached (err u109))
(define-constant err-no-revenue (err u110))
(define-constant err-already-claimed (err u111))

(define-non-fungible-token farm-share uint)

(define-map projects
  { project-id: uint }
  {
    farmer: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    target-amount: uint,
    min-investment: uint,
    max-investment: uint,
    current-amount: uint,
    is-active: bool,
    is-funded: bool,
    total-shares: uint,
    revenue-generated: uint,
    revenue-distributed: uint
  }
)

(define-map investments
  { project-id: uint, investor: principal }
  {
    amount: uint,
    shares: uint
  }
)

(define-map revenue-claims
  { project-id: uint, investor: principal, distribution-id: uint }
  {
    amount: uint,
    claimed: bool
  }
)

(define-map project-distributions
  { project-id: uint }
  {
    distribution-count: uint
  }
)

(define-data-var next-project-id uint u1)
(define-data-var next-share-id uint u1)

(define-read-only (get-project (project-id uint))
  (map-get? projects { project-id: project-id })
)

(define-read-only (get-investment (project-id uint) (investor principal))
  (map-get? investments { project-id: project-id, investor: investor })
)

(define-read-only (get-distribution-count (project-id uint))
  (default-to { distribution-count: u0 }
    (map-get? project-distributions { project-id: project-id }))
)

(define-read-only (get-revenue-claim (project-id uint) (investor principal) (distribution-id uint))
  (map-get? revenue-claims { project-id: project-id, investor: investor, distribution-id: distribution-id })
)

(define-public (create-project (title (string-ascii 64)) (description (string-ascii 256)) (target-amount uint) (min-investment uint) (max-investment uint))
  (let ((project-id (var-get next-project-id)))
    (asserts! (> target-amount u0) (err u0))
    (asserts! (>= max-investment min-investment) (err u1))
    
    (map-set projects
      { project-id: project-id }
      {
        farmer: tx-sender,
        title: title,
        description: description,
        target-amount: target-amount,
        min-investment: min-investment,
        max-investment: max-investment,
        current-amount: u0,
        is-active: true,
        is-funded: false,
        total-shares: u0,
        revenue-generated: u0,
        revenue-distributed: u0
      }
    )
    
    (map-set project-distributions
      { project-id: project-id }
      { distribution-count: u0 }
    )
    
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (invest (project-id uint) (amount uint))
  (let (
    (project (unwrap! (get-project project-id) err-not-found))
    (investor tx-sender)
    (current-investment (default-to { amount: u0, shares: u0 } (get-investment project-id investor)))
    (new-amount (+ (get amount current-investment) amount))
    (share-id (var-get next-share-id))
  )
    (asserts! (get is-active project) err-funding-closed)
    (asserts! (>= amount (get min-investment project)) err-min-investment)
    (asserts! (<= new-amount (get max-investment project)) err-max-investment)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set projects
      { project-id: project-id }
      (merge project {
        current-amount: (+ (get current-amount project) amount),
        total-shares: (+ (get total-shares project) u1)
      })
    )
    
    (map-set investments
      { project-id: project-id, investor: investor }
      { amount: new-amount, shares: (+ (get shares current-investment) u1) }
    )
    
    (try! (nft-mint? farm-share share-id investor))
    
    (var-set next-share-id (+ share-id u1))
    
    (ok share-id)
  )
)

(define-public (close-funding (project-id uint))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (get is-active project) err-funding-closed)
    
    (map-set projects
      { project-id: project-id }
      (merge project {
        is-active: false,
        is-funded: (>= (get current-amount project) (get target-amount project))
      })
    )
    
    (ok true)
  )
)

(define-public (add-revenue (project-id uint) (amount uint))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (not (get is-active project)) err-funding-active)
    (asserts! (get is-funded project) err-target-not-reached)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set projects
      { project-id: project-id }
      (merge project {
        revenue-generated: (+ (get revenue-generated project) amount)
      })
    )
    
    (ok true)
  )
)

(define-public (distribute-revenue (project-id uint))
  (let (
    (project (unwrap! (get-project project-id) err-not-found))
    (distribution-data (get-distribution-count project-id))
    (distribution-id (get distribution-count distribution-data))
    (available-revenue (- (get revenue-generated project) (get revenue-distributed project)))
  )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (> available-revenue u0) err-no-revenue)
    
    (map-set project-distributions
      { project-id: project-id }
      { distribution-count: (+ distribution-id u1) }
    )
    
    (map-set projects
      { project-id: project-id }
      (merge project {
        revenue-distributed: (get revenue-generated project)
      })
    )
    
    (ok distribution-id)
  )
)

(define-public (claim-revenue (project-id uint) (distribution-id uint))
  (let (
    (project (unwrap! (get-project project-id) err-not-found))
    (investment (unwrap! (get-investment project-id tx-sender) err-not-found))
    (claim-data (default-to { amount: u0, claimed: false } 
                 (get-revenue-claim project-id tx-sender distribution-id)))
    (total-shares (get total-shares project))
    (investor-shares (get shares investment))
    (distribution-data (get-distribution-count project-id))
    (revenue-per-share (/ (get revenue-distributed project) total-shares))
    (investor-revenue (* revenue-per-share investor-shares))
  )
    (asserts! (< distribution-id (get distribution-count distribution-data)) err-not-found)
    (asserts! (not (get claimed claim-data)) err-already-claimed)
    
    (map-set revenue-claims
      { project-id: project-id, investor: tx-sender, distribution-id: distribution-id }
      { amount: investor-revenue, claimed: true }
    )
    
    (try! (as-contract (stx-transfer? investor-revenue tx-sender tx-sender)))
    
    (ok investor-revenue)
  )
)