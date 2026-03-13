;; StreamAnchor DAO Governance Platform
;; A liquid democracy system with reputation-based voting and expertise delegation

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-PROPOSAL-EXPIRED (err u103))
(define-constant ERR-INVALID-DELEGATION (err u104))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u105))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u106))

;; Data Variables
(define-data-var proposal-nonce uint u0)
(define-data-var min-reputation-to-propose uint u100)
(define-data-var voting-period uint u1440) ;; blocks (~10 days)

;; Data Maps

;; Member reputation scores by domain
(define-map member-reputation 
    { member: principal, domain: (string-ascii 50) }
    { 
        score: uint,
        participation-count: uint,
        last-active: uint
    }
)

;; Voting power delegations
(define-map delegations
    { delegator: principal, domain: (string-ascii 50) }
    { delegate: principal, weight: uint }
)

;; Proposals
(define-map proposals
    { proposal-id: uint }
    {
        proposer: principal,
        title: (string-ascii 100),
        domain: (string-ascii 50),
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        executed: bool,
        vote-threshold: uint
    }
)

;; Vote records
(define-map votes
    { proposal-id: uint, voter: principal }
    { 
        vote-power: uint,
        vote-choice: bool,
        voted-at: uint
    }
)

;; Member stakes for governance participation
(define-map member-stakes
    { member: principal }
    { 
        staked-amount: uint,
        stake-time: uint,
        total-votes-cast: uint
    }
)

;; Committee memberships
(define-map committee-members
    { domain: (string-ascii 50), member: principal }
    { 
        joined-at: uint,
        expertise-score: uint,
        active: bool
    }
)

;; Read-only functions

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals { proposal-id: proposal-id })
)

(define-read-only (get-member-reputation (member principal) (domain (string-ascii 50)))
    (default-to 
        { score: u0, participation-count: u0, last-active: u0 }
        (map-get? member-reputation { member: member, domain: domain })
    )
)

(define-read-only (get-delegation (delegator principal) (domain (string-ascii 50)))
    (map-get? delegations { delegator: delegator, domain: domain })
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-member-stake (member principal))
    (default-to
        { staked-amount: u0, stake-time: u0, total-votes-cast: u0 }
        (map-get? member-stakes { member: member })
    )
)

(define-read-only (get-voting-power (member principal) (domain (string-ascii 50)))
    (let
        (
            (stake-info (get-member-stake member))
            (rep-info (get-member-reputation member domain))
            (base-power (get staked-amount stake-info))
            (rep-multiplier (get score rep-info))
            (time-bonus (calculate-time-bonus (get stake-time stake-info)))
        )
        (+ base-power (* rep-multiplier time-bonus))
    )
)

(define-read-only (calculate-time-bonus (stake-time uint))
    (let
        ((blocks-staked (- block-height stake-time)))
        (if (> blocks-staked u10000)
            u3
            (if (> blocks-staked u5000)
                u2
                u1
            )
        )
    )
)

(define-read-only (is-proposal-active (proposal-id uint))
    (match (get-proposal proposal-id)
        proposal-data
            (and 
                (>= block-height (get start-block proposal-data))
                (<= block-height (get end-block proposal-data))
                (not (get executed proposal-data))
            )
        false
    )
)

;; Public functions

;; Stake tokens for governance participation
(define-public (stake-tokens (amount uint))
    (let
        (
            (current-stake (get-member-stake tx-sender))
            (new-amount (+ (get staked-amount current-stake) amount))
        )
        (map-set member-stakes
            { member: tx-sender }
            {
                staked-amount: new-amount,
                stake-time: (if (is-eq (get stake-time current-stake) u0) 
                    block-height 
                    (get stake-time current-stake)),
                total-votes-cast: (get total-votes-cast current-stake)
            }
        )
        (ok new-amount)
    )
)

;; Delegate voting power to an expert in a specific domain
(define-public (delegate-voting-power (delegate principal) (domain (string-ascii 50)) (weight uint))
    (begin
        (asserts! (not (is-eq tx-sender delegate)) ERR-INVALID-DELEGATION)
        (asserts! (<= weight u100) ERR-INVALID-DELEGATION)
        (ok (map-set delegations
            { delegator: tx-sender, domain: domain }
            { delegate: delegate, weight: weight }
        ))
    )
)

;; Create a new proposal
(define-public (create-proposal 
    (title (string-ascii 100)) 
    (domain (string-ascii 50))
    (vote-threshold uint))
    (let
        (
            (proposer-rep (get-member-reputation tx-sender domain))
            (proposal-id (+ (var-get proposal-nonce) u1))
        )
        (asserts! (>= (get score proposer-rep) (var-get min-reputation-to-propose)) 
            ERR-INSUFFICIENT-REPUTATION)
        
        (map-set proposals
            { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                title: title,
                domain: domain,
                start-block: block-height,
                end-block: (+ block-height (var-get voting-period)),
                yes-votes: u0,
                no-votes: u0,
                executed: false,
                vote-threshold: vote-threshold
            }
        )
        
        (var-set proposal-nonce proposal-id)
        (ok proposal-id)
    )
)

;; Cast a vote on a proposal
(define-public (cast-vote (proposal-id uint) (vote-choice bool))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
            (voter-power (get-voting-power tx-sender (get domain proposal)))
            (existing-vote (get-vote proposal-id tx-sender))
        )
        (asserts! (is-none existing-vote) ERR-ALREADY-VOTED)
        (asserts! (is-proposal-active proposal-id) ERR-PROPOSAL-NOT-ACTIVE)
        
        ;; Record the vote
        (map-set votes
            { proposal-id: proposal-id, voter: tx-sender }
            {
                vote-power: voter-power,
                vote-choice: vote-choice,
                voted-at: block-height
            }
        )
        
        ;; Update proposal vote counts
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal {
                yes-votes: (if vote-choice 
                    (+ (get yes-votes proposal) voter-power)
                    (get yes-votes proposal)),
                no-votes: (if (not vote-choice)
                    (+ (get no-votes proposal) voter-power)
                    (get no-votes proposal))
            })
        )
        
        ;; Update voter stats
        (update-member-participation tx-sender (get domain proposal))
        
        (ok true)
    )
)

;; Update member reputation and participation
(define-private (update-member-participation (member principal) (domain (string-ascii 50)))
    (let
        (
            (current-rep (get-member-reputation member domain))
            (new-count (+ (get participation-count current-rep) u1))
            (new-score (+ (get score current-rep) u10))
        )
        (map-set member-reputation
            { member: member, domain: domain }
            {
                score: new-score,
                participation-count: new-count,
                last-active: block-height
            }
        )
    )
)

;; Join a committee for a specific domain
(define-public (join-committee (domain (string-ascii 50)))
    (let
        (
            (member-rep (get-member-reputation tx-sender domain))
        )
        (asserts! (>= (get score member-rep) u50) ERR-INSUFFICIENT-REPUTATION)
        
        (ok (map-set committee-members
            { domain: domain, member: tx-sender }
            {
                joined-at: block-height,
                expertise-score: (get score member-rep),
                active: true
            }
        ))
    )
)

;; Award reputation to a member (can be called by contract owner or committee)
(define-public (award-reputation 
    (member principal) 
    (domain (string-ascii 50)) 
    (points uint))
    (let
        (
            (current-rep (get-member-reputation member domain))
        )
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        
        (ok (map-set member-reputation
            { member: member, domain: domain }
            {
                score: (+ (get score current-rep) points),
                participation-count: (get participation-count current-rep),
                last-active: block-height
            }
        ))
    )
)

;; Execute a passed proposal
(define-public (execute-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (get-proposal proposal-id) ERR-PROPOSAL-NOT-FOUND))
        )
        (asserts! (> block-height (get end-block proposal)) ERR-PROPOSAL-NOT-ACTIVE)
        (asserts! (not (get executed proposal)) ERR-PROPOSAL-NOT-ACTIVE)
        (asserts! (>= (get yes-votes proposal) (get vote-threshold proposal)) 
            ERR-NOT-AUTHORIZED)
        
        (map-set proposals
            { proposal-id: proposal-id }
            (merge proposal { executed: true })
        )
        
        (ok true)
    )
)

;; Administrative functions

(define-public (set-min-reputation (new-min uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (var-set min-reputation-to-propose new-min))
    )
)

(define-public (set-voting-period (new-period uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (var-set voting-period new-period))
    )
)