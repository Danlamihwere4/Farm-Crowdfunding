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
(define-constant err-insurance-not-found (err u112))
(define-constant err-claim-expired (err u113))
(define-constant err-invalid-claim-type (err u114))
(define-constant err-insufficient-pool-funds (err u115))
(define-constant err-premium-too-low (err u116))


(define-map project-milestones
  { project-id: uint, milestone-id: uint }
  {
    title: (string-ascii 64),
    description: (string-ascii 256),
    funding-amount: uint,
    is-completed: bool,
    is-approved: bool,
    votes-for: uint,
    votes-against: uint,
    total-voters: uint
  }
)

(define-map milestone-counts
  { project-id: uint }
  { count: uint }
)

(define-map milestone-votes
  { project-id: uint, milestone-id: uint, voter: principal }
  { voted: bool, vote-type: bool }
)

(define-map milestone-funds
  { project-id: uint }
  { total-milestone-amount: uint, released-amount: uint }
)

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


(define-map project-updates
  { project-id: uint, update-id: uint }
  {
    title: (string-ascii 64),
    content: (string-ascii 512),
    timestamp: uint
  }
)

(define-map project-update-counts
  { project-id: uint }
  { count: uint }
)

(define-read-only (get-project-update (project-id uint) (update-id uint))
  (map-get? project-updates { project-id: project-id, update-id: update-id })
)

(define-public (post-project-update (project-id uint) (title (string-ascii 64)) (content (string-ascii 512)))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (update-count (default-to { count: u0 } (map-get? project-update-counts { project-id: project-id })))
      (new-update-id (get count update-count))
    )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    
    (map-set project-updates
      { project-id: project-id, update-id: new-update-id }
      {
        title: title,
        content: content,
        timestamp: stacks-block-height
      }
    )
    
    (map-set project-update-counts
      { project-id: project-id }
      { count: (+ new-update-id u1) }
    )
    
    (ok new-update-id)
  )
)


(define-public (get-project-updates (project-id uint))
  (let ((update-count (default-to { count: u0 } (map-get? project-update-counts { project-id: project-id }))))
    (ok update-count)
  )
)

(define-public (get-project-update-count (project-id uint))
  (let ((update-count (default-to { count: u0 } (map-get? project-update-counts { project-id: project-id }))))
    (ok update-count)
  )
)
(define-public (get-project-update-by-id (project-id uint) (update-id uint))
  (let ((update (unwrap! (get-project-update project-id update-id) err-not-found)))
    (ok update)
  )
)

(define-map project-ratings
  { project-id: uint, investor: principal }
  {
    rating: uint,
    comment: (string-ascii 256)
  }
)

(define-map project-rating-stats
  { project-id: uint }
  {
    total-ratings: uint,
    sum-ratings: uint
  }
)

(define-read-only (get-project-rating (project-id uint) (investor principal))
  (map-get? project-ratings { project-id: project-id, investor: investor })
)

(define-public (rate-project (project-id uint) (rating uint) (comment (string-ascii 256)))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (investment (unwrap! (get-investment project-id tx-sender) err-not-found))
      (stats (default-to { total-ratings: u0, sum-ratings: u0 } 
              (map-get? project-rating-stats { project-id: project-id })))
    )
    (asserts! (not (get is-active project)) err-funding-active)
    (asserts! (>= rating u1) (err u1))
    (asserts! (<= rating u5) (err u2))
    
    (map-set project-ratings
      { project-id: project-id, investor: tx-sender }
      { 
        rating: rating,
        comment: comment
      }
    )
    
    (map-set project-rating-stats
      { project-id: project-id }
      {
        total-ratings: (+ (get total-ratings stats) u1),
        sum-ratings: (+ (get sum-ratings stats) rating)
      }
    )
    
    (ok true)
  )
)


(define-read-only (get-milestone (project-id uint) (milestone-id uint))
  (map-get? project-milestones { project-id: project-id, milestone-id: milestone-id })
)

(define-read-only (get-milestone-count (project-id uint))
  (default-to { count: u0 } (map-get? milestone-counts { project-id: project-id }))
)

(define-read-only (get-milestone-funds (project-id uint))
  (default-to { total-milestone-amount: u0, released-amount: u0 } 
    (map-get? milestone-funds { project-id: project-id }))
)

(define-public (add-milestone (project-id uint) (title (string-ascii 64)) (description (string-ascii 256)) (funding-amount uint))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (milestone-count (get-milestone-count project-id))
      (new-milestone-id (get count milestone-count))
      (current-funds (get-milestone-funds project-id))
    )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (get is-active project) err-funding-closed)
    (asserts! (> funding-amount u0) (err u0))
    
    (map-set project-milestones
      { project-id: project-id, milestone-id: new-milestone-id }
      {
        title: title,
        description: description,
        funding-amount: funding-amount,
        is-completed: false,
        is-approved: false,
        votes-for: u0,
        votes-against: u0,
        total-voters: u0
      }
    )
    
    (map-set milestone-counts
      { project-id: project-id }
      { count: (+ new-milestone-id u1) }
    )
    
    (map-set milestone-funds
      { project-id: project-id }
      {
        total-milestone-amount: (+ (get total-milestone-amount current-funds) funding-amount),
        released-amount: (get released-amount current-funds)
      }
    )
    
    (ok new-milestone-id)
  )
)

(define-public (complete-milestone (project-id uint) (milestone-id uint))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
    )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (not (get is-completed milestone)) err-already-exists)
    
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { is-completed: true })
    )
    
    (ok true)
  )
)

(define-public (vote-milestone (project-id uint) (milestone-id uint) (approve bool))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
      (investment (unwrap! (get-investment project-id tx-sender) err-not-found))
      (existing-vote (default-to { voted: false, vote-type: false } 
                      (map-get? milestone-votes { project-id: project-id, milestone-id: milestone-id, voter: tx-sender })))
    )
    (asserts! (get is-completed milestone) (err u0))
    (asserts! (not (get is-approved milestone)) err-already-exists)
    (asserts! (not (get voted existing-vote)) err-already-exists)
    (asserts! (> (get amount investment) u0) err-unauthorized)
    
    (map-set milestone-votes
      { project-id: project-id, milestone-id: milestone-id, voter: tx-sender }
      { voted: true, vote-type: approve }
    )
    
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone {
        votes-for: (if approve (+ (get votes-for milestone) u1) (get votes-for milestone)),
        votes-against: (if approve (get votes-against milestone) (+ (get votes-against milestone) u1)),
        total-voters: (+ (get total-voters milestone) u1)
      })
    )
    
    (ok true)
  )
)

(define-public (release-milestone-funds (project-id uint) (milestone-id uint))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (milestone (unwrap! (get-milestone project-id milestone-id) err-not-found))
      (current-funds (get-milestone-funds project-id))
      (approval-threshold (/ (get total-shares project) u2))
    )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (get is-completed milestone) (err u0))
    (asserts! (not (get is-approved milestone)) err-already-exists)
    (asserts! (> (get votes-for milestone) approval-threshold) (err u0))
    
    (map-set project-milestones
      { project-id: project-id, milestone-id: milestone-id }
      (merge milestone { is-approved: true })
    )
    
    (map-set milestone-funds
      { project-id: project-id }
      {
        total-milestone-amount: (get total-milestone-amount current-funds),
        released-amount: (+ (get released-amount current-funds) (get funding-amount milestone))
      }
    )
    
    (try! (as-contract (stx-transfer? (get funding-amount milestone) tx-sender (get farmer project))))
    
    (ok (get funding-amount milestone))
  )
)

(define-read-only (get-milestone-vote (project-id uint) (milestone-id uint) (voter principal))
  (map-get? milestone-votes { project-id: project-id, milestone-id: milestone-id, voter: voter })
)


(define-map investor-preferences
  { investor: principal }
  {
    min-target-amount: uint,
    max-target-amount: uint,
    preferred-categories: (list 5 (string-ascii 32)),
    max-investment-per-project: uint,
    auto-invest-enabled: bool,
    auto-invest-percentage: uint
  }
)

(define-map project-categories
  { project-id: uint }
  {
    primary-category: (string-ascii 32),
    secondary-categories: (list 3 (string-ascii 32))
  }
)

(define-map investment-matches
  { match-id: uint }
  {
    investor: principal,
    project-id: uint,
    match-score: uint,
    created-at: uint,
    is-notified: bool,
    auto-invested: bool
  }
)

(define-map investor-match-counts
  { investor: principal }
  { count: uint }
)

(define-data-var next-match-id uint u1)

(define-read-only (get-investor-preferences (investor principal))
  (map-get? investor-preferences { investor: investor })
)

(define-read-only (get-project-category (project-id uint))
  (map-get? project-categories { project-id: project-id })
)

(define-read-only (get-investment-match (match-id uint))
  (map-get? investment-matches { match-id: match-id })
)

(define-read-only (get-investor-match-count (investor principal))
  (default-to { count: u0 } (map-get? investor-match-counts { investor: investor }))
)

(define-public (set-investor-preferences 
  (min-target uint) 
  (max-target uint) 
  (categories (list 5 (string-ascii 32))) 
  (max-investment uint) 
  (auto-invest bool) 
  (auto-percentage uint))
  (begin
    (asserts! (<= min-target max-target) (err u201))
    (asserts! (> max-investment u0) (err u202))
    (asserts! (<= auto-percentage u100) (err u203))
    
    (map-set investor-preferences
      { investor: tx-sender }
      {
        min-target-amount: min-target,
        max-target-amount: max-target,
        preferred-categories: categories,
        max-investment-per-project: max-investment,
        auto-invest-enabled: auto-invest,
        auto-invest-percentage: auto-percentage
      }
    )
    
    (ok true)
  )
)

(define-public (set-project-category 
  (project-id uint) 
  (primary-category (string-ascii 32)) 
  (secondary-categories (list 3 (string-ascii 32))))
  (let ((project (unwrap! (get-project project-id) err-not-found)))
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    
    (map-set project-categories
      { project-id: project-id }
      {
        primary-category: primary-category,
        secondary-categories: secondary-categories
      }
    )
    
    (ok true)
  )
)

(define-public (create-investment-match (investor principal) (project-id uint) (score uint))
  (let 
    (
      (match-id (var-get next-match-id))
      (investor-matches (get-investor-match-count investor))
    )
    (asserts! (is-some (get-project project-id)) err-not-found)
    (asserts! (<= score u100) (err u204))
    
    (map-set investment-matches
      { match-id: match-id }
      {
        investor: investor,
        project-id: project-id,
        match-score: score,
        created-at: stacks-block-height,
        is-notified: false,
        auto-invested: false
      }
    )
    
    (map-set investor-match-counts
      { investor: investor }
      { count: (+ (get count investor-matches) u1) }
    )
    
    (var-set next-match-id (+ match-id u1))
    
    (ok match-id)
  )
)

(define-public (calculate-match-score (investor principal) (project-id uint))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (preferences (unwrap! (get-investor-preferences investor) (err u205)))
      (categories (get-project-category project-id))
      (target-amount (get target-amount project))
      (base-score u0)
    )
    (asserts! (get is-active project) err-funding-closed)
    
    (let 
      (
        (target-match (if (and (>= target-amount (get min-target-amount preferences))
                              (<= target-amount (get max-target-amount preferences)))
                        u40 u0))
        (category-match (if (is-some categories) 
                          (if (is-eq (get primary-category (unwrap-panic categories)) 
                                    (unwrap-panic (element-at (get preferred-categories preferences) u0)))
                            u40 u20) u0))
        (timing-bonus (if (< (get current-amount project) (/ (get target-amount project) u4)) u20 u0))
        (total-score (+ base-score target-match category-match timing-bonus))
      )
      (if (> total-score u60)
        (begin
          (try! (create-investment-match investor project-id total-score))
          (ok total-score))
        (ok total-score))
    )
  )
)

(define-public (process-auto-investment (match-id uint))
  (let 
    (
      (match-data (unwrap! (get-investment-match match-id) err-not-found))
      (investor (get investor match-data))
      (project-id (get project-id match-data))
      (preferences (unwrap! (get-investor-preferences investor) (err u205)))
      (project (unwrap! (get-project project-id) err-not-found))
    )
    (asserts! (get auto-invest-enabled preferences) (err u206))
    (asserts! (not (get auto-invested match-data)) err-already-exists)
    (asserts! (get is-active project) err-funding-closed)
    
    (let 
      (
        (percentage-amount (/ (* (get max-investment-per-project preferences) 
                                (get auto-invest-percentage preferences)) u100))
        (investment-amount (if (<= percentage-amount (get max-investment-per-project preferences))
                              percentage-amount
                              (get max-investment-per-project preferences)))
      )
      (asserts! (>= investment-amount (get min-investment project)) err-min-investment)
      
      (map-set investment-matches
        { match-id: match-id }
        (merge match-data { auto-invested: true })
      )
      
      (try! (invest project-id investment-amount))
      
      (ok investment-amount)
    )
  )
)

(define-public (notify-match (match-id uint))
  (let ((match-data (unwrap! (get-investment-match match-id) err-not-found)))
    (asserts! (not (get is-notified match-data)) err-already-exists)
    
    (map-set investment-matches
      { match-id: match-id }
      (merge match-data { is-notified: true })
    )
    
    (ok true)
  )
)

(define-read-only (get-investor-active-matches (investor principal))
  (let ((match-count (get-investor-match-count investor)))
    (ok (get count match-count))
  )
)

;; Farm Insurance Pool System - Protects investments against agricultural risks
(define-map insurance-pools
  { pool-id: uint }
  {
    name: (string-ascii 64),
    total-pool-amount: uint,
    active-coverage: uint,
    premium-rate: uint, ;; basis points (e.g., 500 = 5%)
    max-claim-amount: uint,
    pool-creator: principal,
    is-active: bool,
    claim-count: uint
  }
)

(define-map pool-contributors
  { pool-id: uint, contributor: principal }
  {
    contributed-amount: uint,
    share-percentage: uint,
    rewards-earned: uint
  }
)

(define-map project-insurance
  { project-id: uint }
  {
    pool-id: uint,
    premium-paid: uint,
    coverage-amount: uint,
    coverage-start: uint,
    coverage-end: uint,
    is-covered: bool
  }
)

(define-map insurance-claims
  { claim-id: uint }
  {
    project-id: uint,
    pool-id: uint,
    claimant: principal,
    claim-type: uint, ;; 1=crop failure, 2=weather damage, 3=disease, 4=market crash
    claim-amount: uint,
    evidence-hash: (string-ascii 64),
    submitted-at: uint,
    status: uint, ;; 0=pending, 1=approved, 2=rejected, 3=paid
    votes-for: uint,
    votes-against: uint,
    total-voters: uint
  }
)

(define-map claim-votes
  { claim-id: uint, voter: principal }
  {
    voted: bool,
    vote-type: bool, ;; true=approve, false=reject
    voting-power: uint
  }
)

(define-data-var next-pool-id uint u1)
(define-data-var next-claim-id uint u1)

;; Read-only functions for insurance system
(define-read-only (get-insurance-pool (pool-id uint))
  (map-get? insurance-pools { pool-id: pool-id })
)

(define-read-only (get-pool-contribution (pool-id uint) (contributor principal))
  (map-get? pool-contributors { pool-id: pool-id, contributor: contributor })
)

(define-read-only (get-project-insurance-info (project-id uint))
  (map-get? project-insurance { project-id: project-id })
)

(define-read-only (get-insurance-claim (claim-id uint))
  (map-get? insurance-claims { claim-id: claim-id })
)

(define-read-only (get-claim-vote (claim-id uint) (voter principal))
  (map-get? claim-votes { claim-id: claim-id, voter: voter })
)

;; Create a new insurance pool for farm projects
(define-public (create-insurance-pool 
  (name (string-ascii 64)) 
  (premium-rate uint) 
  (max-claim-amount uint))
  (let ((pool-id (var-get next-pool-id)))
    (asserts! (> premium-rate u0) (err u300))
    (asserts! (< premium-rate u1000) (err u301)) ;; Max 10% premium rate
    (asserts! (> max-claim-amount u0) (err u302))
    
    (map-set insurance-pools
      { pool-id: pool-id }
      {
        name: name,
        total-pool-amount: u0,
        active-coverage: u0,
        premium-rate: premium-rate,
        max-claim-amount: max-claim-amount,
        pool-creator: tx-sender,
        is-active: true,
        claim-count: u0
      }
    )
    
    (var-set next-pool-id (+ pool-id u1))
    (ok pool-id)
  )
)

;; Contribute funds to an insurance pool to earn premium rewards
(define-public (contribute-to-pool (pool-id uint) (amount uint))
  (let 
    (
      (pool (unwrap! (get-insurance-pool pool-id) err-insurance-not-found))
      (existing-contribution (default-to { contributed-amount: u0, share-percentage: u0, rewards-earned: u0 }
                              (get-pool-contribution pool-id tx-sender)))
      (new-total-pool (+ (get total-pool-amount pool) amount))
      (new-contribution (+ (get contributed-amount existing-contribution) amount))
    )
    (asserts! (get is-active pool) err-funding-closed)
    (asserts! (> amount u0) err-insufficient-funds)
    
    ;; Transfer funds to contract
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    ;; Calculate new share percentage
    (let ((new-share-percentage (if (> new-total-pool u0)
                                   (/ (* new-contribution u10000) new-total-pool)
                                   u10000)))
      
      (map-set pool-contributors
        { pool-id: pool-id, contributor: tx-sender }
        {
          contributed-amount: new-contribution,
          share-percentage: new-share-percentage,
          rewards-earned: (get rewards-earned existing-contribution)
        }
      )
      
      (map-set insurance-pools
        { pool-id: pool-id }
        (merge pool { total-pool-amount: new-total-pool })
      )
      
      (ok new-contribution)
    )
  )
)

;; Purchase insurance coverage for a farm project
(define-public (purchase-project-insurance (project-id uint) (pool-id uint) (coverage-duration uint))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (pool (unwrap! (get-insurance-pool pool-id) err-insurance-not-found))
      (coverage-amount (get target-amount project))
      (premium-amount (/ (* coverage-amount (get premium-rate pool)) u10000))
      (coverage-start stacks-block-height)
      (coverage-end (+ coverage-start coverage-duration))
    )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (get is-active pool) err-funding-closed)
    (asserts! (>= premium-amount u1000) err-premium-too-low) ;; Minimum premium
    (asserts! (<= coverage-amount (get max-claim-amount pool)) (err u303))
    
    ;; Transfer premium to contract
    (try! (stx-transfer? premium-amount tx-sender (as-contract tx-sender)))
    
    (map-set project-insurance
      { project-id: project-id }
      {
        pool-id: pool-id,
        premium-paid: premium-amount,
        coverage-amount: coverage-amount,
        coverage-start: coverage-start,
        coverage-end: coverage-end,
        is-covered: true
      }
    )
    
    ;; Update pool's active coverage
    (map-set insurance-pools
      { pool-id: pool-id }
      (merge pool { active-coverage: (+ (get active-coverage pool) coverage-amount) })
    )
    
    (ok premium-amount)
  )
)

;; Submit an insurance claim for covered farm project
(define-public (submit-insurance-claim 
  (project-id uint) 
  (claim-type uint) 
  (claim-amount uint) 
  (evidence-hash (string-ascii 64)))
  (let 
    (
      (project (unwrap! (get-project project-id) err-not-found))
      (insurance-info (unwrap! (get-project-insurance-info project-id) err-insurance-not-found))
      (claim-id (var-get next-claim-id))
      (current-block stacks-block-height)
    )
    (asserts! (is-eq (get farmer project) tx-sender) err-unauthorized)
    (asserts! (get is-covered insurance-info) err-insurance-not-found)
    (asserts! (>= current-block (get coverage-start insurance-info)) (err u304))
    (asserts! (<= current-block (get coverage-end insurance-info)) err-claim-expired)
    (asserts! (and (>= claim-type u1) (<= claim-type u4)) err-invalid-claim-type)
    (asserts! (<= claim-amount (get coverage-amount insurance-info)) (err u305))
    
    (map-set insurance-claims
      { claim-id: claim-id }
      {
        project-id: project-id,
        pool-id: (get pool-id insurance-info),
        claimant: tx-sender,
        claim-type: claim-type,
        claim-amount: claim-amount,
        evidence-hash: evidence-hash,
        submitted-at: current-block,
        status: u0, ;; pending
        votes-for: u0,
        votes-against: u0,
        total-voters: u0
      }
    )
    
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Vote on insurance claim (pool contributors can vote)
(define-public (vote-on-claim (claim-id uint) (approve bool))
  (let 
    (
      (claim (unwrap! (get-insurance-claim claim-id) err-not-found))
      (pool-id (get pool-id claim))
      (contribution (unwrap! (get-pool-contribution pool-id tx-sender) err-unauthorized))
      (existing-vote (default-to { voted: false, vote-type: false, voting-power: u0 }
                      (get-claim-vote claim-id tx-sender)))
      (voting-power (get share-percentage contribution))
    )
    (asserts! (is-eq (get status claim) u0) err-already-exists) ;; Only pending claims
    (asserts! (not (get voted existing-vote)) err-already-exists)
    (asserts! (> voting-power u0) err-unauthorized)
    
    (map-set claim-votes
      { claim-id: claim-id, voter: tx-sender }
      {
        voted: true,
        vote-type: approve,
        voting-power: voting-power
      }
    )
    
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim {
        votes-for: (if approve (+ (get votes-for claim) voting-power) (get votes-for claim)),
        votes-against: (if approve (get votes-against claim) (+ (get votes-against claim) voting-power)),
        total-voters: (+ (get total-voters claim) u1)
      })
    )
    
    (ok true)
  )
)

;; Process approved insurance claim and distribute payout
(define-public (process-claim-payout (claim-id uint))
  (let 
    (
      (claim (unwrap! (get-insurance-claim claim-id) err-not-found))
      (pool-id (get pool-id claim))
      (pool (unwrap! (get-insurance-pool pool-id) err-insurance-not-found))
      (approval-threshold u5000) ;; 50% approval required
      (claim-amount (get claim-amount claim))
    )
    (asserts! (is-eq (get status claim) u0) err-already-exists) ;; Only pending claims
    (asserts! (> (get votes-for claim) approval-threshold) (err u306))
    (asserts! (>= (get total-pool-amount pool) claim-amount) err-insufficient-pool-funds)
    
    ;; Update claim status to approved and paid
    (map-set insurance-claims
      { claim-id: claim-id }
      (merge claim { status: u3 }) ;; paid
    )
    
    ;; Update pool amounts
    (map-set insurance-pools
      { pool-id: pool-id }
      (merge pool {
        total-pool-amount: (- (get total-pool-amount pool) claim-amount),
        active-coverage: (- (get active-coverage pool) claim-amount),
        claim-count: (+ (get claim-count pool) u1)
      })
    )
    
    ;; Transfer payout to claimant
    (try! (as-contract (stx-transfer? claim-amount tx-sender (get claimant claim))))
    
    (ok claim-amount)
  )
)

