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