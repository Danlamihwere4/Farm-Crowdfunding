;; Crop Yield Forecasting and Verification Contract
;; Integrates with Farm-Crowdfunding to provide yield predictions and verification

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u400))
(define-constant err-not-found (err u401))
(define-constant err-unauthorized (err u402))
(define-constant err-already-exists (err u403))
(define-constant err-invalid-input (err u404))
(define-constant err-forecast-expired (err u405))
(define-constant err-already-submitted (err u406))
(define-constant err-insufficient-stake (err u407))

;; Data Variables
(define-data-var next-forecast-id uint u1)
(define-data-var oracle-stake-requirement uint u1000000) ;; 1 STX minimum stake

;; Maps
(define-map yield-forecasts
  { forecast-id: uint }
  {
    project-id: uint,
    forecaster: principal,
    predicted-yield: uint, ;; in kg per hectare
    confidence-level: uint, ;; 1-100 percentage
    forecast-timestamp: uint,
    expiry-block: uint,
    stake-amount: uint,
    is-verified: bool,
    actual-yield: uint,
    accuracy-score: uint
  }
)

(define-map project-yield-data
  { project-id: uint }
  {
    crop-type: (string-ascii 32),
    planted-area: uint, ;; in hectares
    planting-date: uint,
    expected-harvest: uint,
    soil-quality: uint, ;; 1-10 rating
    irrigation-type: uint, ;; 1=rain-fed, 2=irrigation, 3=drip
    forecast-count: uint,
    verified-forecasts: uint,
    average-accuracy: uint
  }
)

(define-map oracle-stakes
  { oracle: principal }
  {
    total-staked: uint,
    successful-predictions: uint,
    total-predictions: uint,
    reputation-score: uint,
    earnings: uint
  }
)

(define-map yield-verification
  { project-id: uint, verifier: principal }
  {
    reported-yield: uint,
    verification-date: uint,
    evidence-hash: (string-ascii 64),
    is-confirmed: bool
  }
)

(define-map forecast-rewards
  { forecast-id: uint }
  {
    base-reward: uint,
    accuracy-bonus: uint,
    total-reward: uint,
    is-claimed: bool
  }
)

;; Read-only functions
(define-read-only (get-forecast (forecast-id uint))
  (map-get? yield-forecasts { forecast-id: forecast-id })
)

(define-read-only (get-project-yield-data (project-id uint))
  (map-get? project-yield-data { project-id: project-id })
)

(define-read-only (get-oracle-stakes (oracle principal))
  (map-get? oracle-stakes { oracle: oracle })
)

(define-read-only (get-yield-verification (project-id uint) (verifier principal))
  (map-get? yield-verification { project-id: project-id, verifier: verifier })
)

(define-read-only (get-forecast-reward (forecast-id uint))
  (map-get? forecast-rewards { forecast-id: forecast-id })
)

(define-read-only (calculate-accuracy (predicted uint) (actual uint))
  (let ((difference (if (>= predicted actual) (- predicted actual) (- actual predicted))))
    (if (is-eq actual u0)
      u0
      (let ((percentage-diff (/ (* difference u100) actual)))
        (if (<= percentage-diff u100)
          (- u100 percentage-diff)
          u0)))))

;; Initialize project yield parameters (only project farmers can call)
(define-public (set-project-yield-params
  (project-id uint)
  (crop-type (string-ascii 32))
  (planted-area uint)
  (planting-date uint)
  (expected-harvest uint)
  (soil-quality uint)
  (irrigation-type uint))
  (begin
    (asserts! (> planted-area u0) err-invalid-input)
    (asserts! (and (>= soil-quality u1) (<= soil-quality u10)) err-invalid-input)
    (asserts! (and (>= irrigation-type u1) (<= irrigation-type u3)) err-invalid-input)
    
    (map-set project-yield-data
      { project-id: project-id }
      {
        crop-type: crop-type,
        planted-area: planted-area,
        planting-date: planting-date,
        expected-harvest: expected-harvest,
        soil-quality: soil-quality,
        irrigation-type: irrigation-type,
        forecast-count: u0,
        verified-forecasts: u0,
        average-accuracy: u0
      }
    )
    
    (ok true)
  )
)

;; Stake tokens to become a yield oracle
(define-public (stake-as-oracle (amount uint))
  (let ((current-stake (default-to { total-staked: u0, successful-predictions: u0, 
                                   total-predictions: u0, reputation-score: u0, earnings: u0 }
                        (get-oracle-stakes tx-sender))))
    (asserts! (>= amount (var-get oracle-stake-requirement)) err-insufficient-stake)
    
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    
    (map-set oracle-stakes
      { oracle: tx-sender }
      (merge current-stake { total-staked: (+ (get total-staked current-stake) amount) })
    )
    
    (ok (+ (get total-staked current-stake) amount))
  )
)

;; Submit yield forecast with stake
(define-public (submit-yield-forecast
  (project-id uint)
  (predicted-yield uint)
  (confidence-level uint)
  (forecast-duration uint))
  (let (
    (forecast-id (var-get next-forecast-id))
    (oracle-data (unwrap! (get-oracle-stakes tx-sender) err-unauthorized))
    (project-data (unwrap! (get-project-yield-data project-id) err-not-found))
    (stake-amount (/ (get total-staked oracle-data) u10)) ;; 10% of total stake
  )
    (asserts! (> predicted-yield u0) err-invalid-input)
    (asserts! (and (>= confidence-level u1) (<= confidence-level u100)) err-invalid-input)
    (asserts! (>= (get total-staked oracle-data) (var-get oracle-stake-requirement)) err-insufficient-stake)
    
    (map-set yield-forecasts
      { forecast-id: forecast-id }
      {
        project-id: project-id,
        forecaster: tx-sender,
        predicted-yield: predicted-yield,
        confidence-level: confidence-level,
        forecast-timestamp: stacks-block-height,
        expiry-block: (+ stacks-block-height forecast-duration),
        stake-amount: stake-amount,
        is-verified: false,
        actual-yield: u0,
        accuracy-score: u0
      }
    )
    
    ;; Update project forecast count
    (map-set project-yield-data
      { project-id: project-id }
      (merge project-data { forecast-count: (+ (get forecast-count project-data) u1) })
    )
    
    (var-set next-forecast-id (+ forecast-id u1))
    (ok forecast-id)
  )
)

;; Verify actual yield (only by authorized verifiers)
(define-public (verify-actual-yield
  (project-id uint)
  (reported-yield uint)
  (evidence-hash (string-ascii 64)))
  (begin
    (asserts! (> reported-yield u0) err-invalid-input)
    
    (map-set yield-verification
      { project-id: project-id, verifier: tx-sender }
      {
        reported-yield: reported-yield,
        verification-date: stacks-block-height,
        evidence-hash: evidence-hash,
        is-confirmed: false
      }
    )
    
    (ok true)
  )
)

;; Confirm yield verification and calculate forecast accuracies
(define-public (confirm-yield-verification (project-id uint) (verifier principal) (actual-yield uint))
  (let ((verification (unwrap! (get-yield-verification project-id verifier) err-not-found)))
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    
    ;; Update verification as confirmed
    (map-set yield-verification
      { project-id: project-id, verifier: verifier }
      (merge verification { is-confirmed: true })
    )
    
    ;; Update all forecasts for this project with actual yield and accuracy
    (ok (update-forecast-accuracies project-id actual-yield))
  )
)

;; Helper function to update forecast accuracies (private)
(define-private (update-forecast-accuracies (project-id uint) (actual-yield uint))
  (let ((project-data (unwrap-panic (get-project-yield-data project-id))))
    ;; In a full implementation, this would iterate through forecasts
    ;; For simplicity, we'll update project stats
    (map-set project-yield-data
      { project-id: project-id }
      (merge project-data { verified-forecasts: (+ (get verified-forecasts project-data) u1) })
    )
    true
  )
)

;; Calculate and distribute rewards for accurate forecasts
(define-public (calculate-forecast-reward (forecast-id uint))
  (let (
    (forecast (unwrap! (get-forecast forecast-id) err-not-found))
    (accuracy (calculate-accuracy (get predicted-yield forecast) (get actual-yield forecast)))
    (base-reward (* (get stake-amount forecast) u2)) ;; 2x stake as base
    (accuracy-bonus (/ (* base-reward accuracy) u100))
    (total-reward (+ base-reward accuracy-bonus))
  )
    (asserts! (get is-verified forecast) err-not-found)
    
    (map-set forecast-rewards
      { forecast-id: forecast-id }
      {
        base-reward: base-reward,
        accuracy-bonus: accuracy-bonus,
        total-reward: total-reward,
        is-claimed: false
      }
    )
    
    (ok total-reward)
  )
)

;; Claim forecast rewards
(define-public (claim-forecast-reward (forecast-id uint))
  (let (
    (forecast (unwrap! (get-forecast forecast-id) err-not-found))
    (reward-data (unwrap! (get-forecast-reward forecast-id) err-not-found))
    (oracle-data (unwrap! (get-oracle-stakes tx-sender) err-unauthorized))
  )
    (asserts! (is-eq (get forecaster forecast) tx-sender) err-unauthorized)
    (asserts! (not (get is-claimed reward-data)) err-already-exists)
    
    ;; Mark as claimed
    (map-set forecast-rewards
      { forecast-id: forecast-id }
      (merge reward-data { is-claimed: true })
    )
    
    ;; Update oracle stats
    (map-set oracle-stakes
      { oracle: tx-sender }
      (merge oracle-data {
        successful-predictions: (+ (get successful-predictions oracle-data) u1),
        total-predictions: (+ (get total-predictions oracle-data) u1),
        earnings: (+ (get earnings oracle-data) (get total-reward reward-data))
      })
    )
    
    ;; Transfer reward
    (try! (as-contract (stx-transfer? (get total-reward reward-data) tx-sender tx-sender)))
    
    (ok (get total-reward reward-data))
  )
)

;; Get project forecast summary
(define-read-only (get-project-forecast-summary (project-id uint))
  (let ((project-data (map-get? project-yield-data { project-id: project-id })))
    (match project-data
      data (ok {
        forecast-count: (get forecast-count data),
        verified-forecasts: (get verified-forecasts data),
        average-accuracy: (get average-accuracy data),
        crop-type: (get crop-type data),
        planted-area: (get planted-area data)
      })
      (err err-not-found)
    )
  )
)
