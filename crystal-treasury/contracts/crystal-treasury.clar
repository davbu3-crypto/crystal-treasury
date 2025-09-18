;; Crystal Treasury DAO Governance Contract
;; A simplified implementation of multi-dimensional DAO governance

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-member (err u101))
(define-constant err-invalid-proposal (err u102))
(define-constant err-already-voted (err u103))
(define-constant err-proposal-ended (err u104))
(define-constant err-insufficient-stake (err u105))

;; Data Variables
(define-data-var proposal-counter uint u0)
(define-data-var treasury-balance uint u0)
(define-data-var min-stake-required uint u1000)

;; Data Maps
(define-map members principal {
    stake: uint,
    reputation: uint,
    join-height: uint,
    total-votes: uint,
    successful-votes: uint
})

(define-map proposals uint {
    proposer: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    amount: uint,
    recipient: (optional principal),
    start-height: uint,
    end-height: uint,
    yes-votes: uint,
    no-votes: uint,
    executed: bool,
    proposal-type: (string-ascii 20)
})

(define-map votes {proposal-id: uint, voter: principal} {
    vote: bool,
    weight: uint,
    block-height: uint
})

;; Treasury health scoring data
(define-map treasury-metrics uint {
    total-assets: uint,
    liquidity-ratio: uint,
    risk-score: uint,
    diversification-index: uint
})

;; Read-only functions

(define-read-only (get-member (member principal))
    (map-get? members member))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes {proposal-id: proposal-id, voter: voter}))

(define-read-only (get-treasury-balance)
    (var-get treasury-balance))

(define-read-only (calculate-voting-weight (member principal))
    (let ((member-data (unwrap! (map-get? members member) u0))
          (stake (get stake member-data))
          (reputation (get reputation member-data))
          (tenure (- block-height (get join-height member-data))))
        ;; Weight = stake * reputation factor * tenure bonus
        (+ stake 
           (* reputation u10)
           (/ tenure u100))))

(define-read-only (get-proposal-status (proposal-id uint))
    (match (map-get? proposals proposal-id)
        proposal-data 
        (if (> block-height (get end-height proposal-data))
            "ended"
            "active")
        "not-found"))

(define-read-only (calculate-quorum (proposal-id uint))
    (let ((proposal-data (unwrap! (map-get? proposals proposal-id) u0))
          (proposal-type (get proposal-type proposal-data)))
        ;; Dynamic quorum based on proposal type
        (if (is-eq proposal-type "treasury")
            u30  ;; 30% for treasury proposals
            u15))) ;; 15% for regular proposals

;; Private functions

(define-private (is-member (member principal))
    (is-some (map-get? members member)))

(define-private (update-reputation (member principal) (success bool))
    (match (map-get? members member)
        member-data
        (let ((current-rep (get reputation member-data))
              (total-votes (get total-votes member-data))
              (successful-votes (get successful-votes member-data)))
            (map-set members member
                (merge member-data {
                    total-votes: (+ total-votes u1),
                    successful-votes: (if success 
                                        (+ successful-votes u1) 
                                        successful-votes),
                    reputation: (if success 
                                  (+ current-rep u1)
                                  (if (> current-rep u0) (- current-rep u1) u0))
                })))
        false))

;; Public functions

(define-public (join-dao (initial-stake uint))
    (begin
        (asserts! (>= initial-stake (var-get min-stake-required)) err-insufficient-stake)
        (asserts! (not (is-member tx-sender)) (err u106))
        (map-set members tx-sender {
            stake: initial-stake,
            reputation: u10,
            join-height: block-height,
            total-votes: u0,
            successful-votes: u0
        })
        (var-set treasury-balance (+ (var-get treasury-balance) initial-stake))
        (ok true)))

(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (amount uint)
    (recipient (optional principal))
    (voting-period uint)
    (proposal-type (string-ascii 20)))
    (let ((proposal-id (+ (var-get proposal-counter) u1)))
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (<= voting-period u1000) err-invalid-proposal) ;; Max 1000 blocks
        (map-set proposals proposal-id {
            proposer: tx-sender,
            title: title,
            description: description,
            amount: amount,
            recipient: recipient,
            start-height: block-height,
            end-height: (+ block-height voting-period),
            yes-votes: u0,
            no-votes: u0,
            executed: false,
            proposal-type: proposal-type
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote-yes bool))
    (let ((proposal-data (unwrap! (map-get? proposals proposal-id) err-invalid-proposal))
          (voting-weight (calculate-voting-weight tx-sender)))
        (asserts! (is-member tx-sender) err-not-member)
        (asserts! (<= block-height (get end-height proposal-data)) err-proposal-ended)
        (asserts! (is-none (map-get? votes {proposal-id: proposal-id, voter: tx-sender})) err-already-voted)
        
        ;; Record the vote
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} {
            vote: vote-yes,
            weight: voting-weight,
            block-height: block-height
        })
        
        ;; Update proposal vote counts
        (map-set proposals proposal-id
            (merge proposal-data {
                yes-votes: (if vote-yes 
                             (+ (get yes-votes proposal-data) voting-weight)
                             (get yes-votes proposal-data)),
                no-votes: (if vote-yes
                            (get no-votes proposal-data)
                            (+ (get no-votes proposal-data) voting-weight))
            }))
        (ok true)))

(define-public (execute-proposal (proposal-id uint))
    (let ((proposal-data (unwrap! (map-get? proposals proposal-id) err-invalid-proposal))
          (total-votes (+ (get yes-votes proposal-data) (get no-votes proposal-data)))
          (yes-percentage (if (> total-votes u0) 
                            (/ (* (get yes-votes proposal-data) u100) total-votes) 
                            u0)))
        (asserts! (> block-height (get end-height proposal-data)) err-proposal-ended)
        (asserts! (not (get executed proposal-data)) (err u107))
        (asserts! (>= yes-percentage (calculate-quorum proposal-id)) (err u108))
        
        ;; Execute the proposal (simplified treasury transfer)
        (if (and (> (get amount proposal-data) u0) 
                 (is-some (get recipient proposal-data)))
            (begin
                (var-set treasury-balance 
                    (- (var-get treasury-balance) (get amount proposal-data)))
                ;; In a full implementation, this would transfer STX to recipient
                true)
            true)
        
        ;; Mark as executed
        (map-set proposals proposal-id
            (merge proposal-data {executed: true}))
        
        ;; Update reputation for successful voters
        ;; (Simplified - would iterate through all voters in full implementation)
        (ok true)))

(define-public (update-stake (additional-stake uint))
    (let ((member-data (unwrap! (map-get? members tx-sender) err-not-member)))
        (map-set members tx-sender
            (merge member-data {
                stake: (+ (get stake member-data) additional-stake)
            }))
        (var-set treasury-balance (+ (var-get treasury-balance) additional-stake))
        (ok true)))

(define-public (update-treasury-metrics 
    (total-assets uint)
    (liquidity-ratio uint)
    (risk-score uint)
    (diversification-index uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (map-set treasury-metrics block-height {
            total-assets: total-assets,
            liquidity-ratio: liquidity-ratio,
            risk-score: risk-score,
            diversification-index: diversification-index
        })
        (ok true)))

;; Emergency functions (owner only)

(define-public (emergency-pause)
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        ;; In full implementation, this would pause all operations
        (ok true)))

(define-public (set-min-stake (new-min-stake uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (var-set min-stake-required new-min-stake)
        (ok true)))

;; Initialize contract
(begin
    (var-set proposal-counter u0)
    (var-set treasury-balance u0)
)