(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INVALID-PROPOSAL (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-VOTING-PERIOD-ENDED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-PROPOSAL-NOT-APPROVED (err u106))
(define-constant ERR-ALREADY-MEMBER (err u107))
(define-constant ERR-NOT-MEMBER (err u108))
(define-constant ERR-INVALID-AMOUNT (err u109))
(define-constant ERR-PROJECT-COMPLETED (err u110))
(define-constant ERR-INVALID-ENERGY-DATA (err u111))
(define-constant ERR-DATA-ALREADY-SUBMITTED (err u112))
(define-constant ERR-INSUFFICIENT-PERFORMANCE (err u113))

(define-constant VOTING-PERIOD u1440)
(define-constant MIN-CONTRIBUTION u1000000)
(define-constant APPROVAL-THRESHOLD u51)
(define-constant MIN-PERFORMANCE-THRESHOLD u80)
(define-constant PERFORMANCE-BONUS-RATE u20)

(define-data-var proposal-counter uint u0)
(define-data-var total-dao-funds uint u0)

(define-map dao-members principal { contribution: uint, joined-at: uint })
(define-map proposals uint {
    creator: principal,
    title: (string-ascii 64),
    description: (string-ascii 256),
    funding-goal: uint,
    installer: principal,
    created-at: uint,
    voting-ends-at: uint,
    yes-votes: uint,
    no-votes: uint,
    approved: bool,
    executed: bool,
    funds-released: uint
})
(define-map project-votes { proposal-id: uint, voter: principal } bool)
(define-map project-contributions { proposal-id: uint, contributor: principal } uint)
(define-map energy-rewards principal uint)
(define-map installer-certifications principal bool)
(define-map energy-production { proposal-id: uint, month: uint } { kwh-produced: uint, reported-by: principal, timestamp: uint })
(define-map project-performance uint { total-kwh: uint, months-active: uint, performance-score: uint })

(define-public (join-dao)
    (let ((caller tx-sender)
          (current-block burn-block-height))
        (asserts! (is-none (map-get? dao-members caller)) ERR-ALREADY-MEMBER)
        (map-set dao-members caller { contribution: u0, joined-at: current-block })
        (ok true)))

(define-public (certify-installer (installer principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (map-set installer-certifications installer true)
        (ok true)))

(define-public (contribute-to-dao (amount uint))
    (let ((caller tx-sender)
          (member-data (unwrap! (map-get? dao-members caller) ERR-NOT-MEMBER)))
        (asserts! (>= amount MIN-CONTRIBUTION) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount caller (as-contract tx-sender)))
        (map-set dao-members caller {
            contribution: (+ (get contribution member-data) amount),
            joined-at: (get joined-at member-data)
        })
        (var-set total-dao-funds (+ (var-get total-dao-funds) amount))
        (ok true)))

(define-public (create-proposal (title (string-ascii 64)) (description (string-ascii 256)) (funding-goal uint) (installer principal))
    (let ((caller tx-sender)
          (proposal-id (+ (var-get proposal-counter) u1))
          (current-block burn-block-height))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (> funding-goal u0) ERR-INVALID-PROPOSAL)
        (asserts! (default-to false (map-get? installer-certifications installer)) ERR-NOT-AUTHORIZED)
        (map-set proposals proposal-id {
            creator: caller,
            title: title,
            description: description,
            funding-goal: funding-goal,
            installer: installer,
            created-at: current-block,
            voting-ends-at: (+ current-block VOTING-PERIOD),
            yes-votes: u0,
            no-votes: u0,
            approved: false,
            executed: false,
            funds-released: u0
        })
        (var-set proposal-counter proposal-id)
        (ok proposal-id)))

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
    (let ((caller tx-sender)
          (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (current-block burn-block-height)
          (member-data (unwrap! (map-get? dao-members caller) ERR-NOT-MEMBER))
          (vote-key { proposal-id: proposal-id, voter: caller }))
        (asserts! (is-none (map-get? project-votes vote-key)) ERR-ALREADY-VOTED)
        (asserts! (<= current-block (get voting-ends-at proposal)) ERR-VOTING-PERIOD-ENDED)
        (map-set project-votes vote-key vote)
        (if vote
            (map-set proposals proposal-id (merge proposal { yes-votes: (+ (get yes-votes proposal) u1) }))
            (map-set proposals proposal-id (merge proposal { no-votes: (+ (get no-votes proposal) u1) })))
        (ok true)))

(define-public (finalize-proposal (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (current-block burn-block-height)
          (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
          (approval-percentage (if (> total-votes u0) (* (/ (get yes-votes proposal) total-votes) u100) u0)))
        (asserts! (> current-block (get voting-ends-at proposal)) ERR-VOTING-PERIOD-ENDED)
        (asserts! (not (get approved proposal)) ERR-PROPOSAL-NOT-APPROVED)
        (if (>= approval-percentage APPROVAL-THRESHOLD)
            (map-set proposals proposal-id (merge proposal { approved: true }))
            (map-set proposals proposal-id (merge proposal { approved: false })))
        (ok (>= approval-percentage APPROVAL-THRESHOLD))))

(define-public (contribute-to-project (proposal-id uint) (amount uint))
    (let ((caller tx-sender)
          (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (contribution-key { proposal-id: proposal-id, contributor: caller })
          (current-contribution (default-to u0 (map-get? project-contributions contribution-key))))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (get approved proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (not (get executed proposal)) ERR-PROJECT-COMPLETED)
        (asserts! (>= amount MIN-CONTRIBUTION) ERR-INVALID-AMOUNT)
        (try! (stx-transfer? amount caller (as-contract tx-sender)))
        (map-set project-contributions contribution-key (+ current-contribution amount))
        (ok true)))

(define-public (release-funds (proposal-id uint) (amount uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND)))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (get approved proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (<= (+ (get funds-released proposal) amount) (get funding-goal proposal)) ERR-INSUFFICIENT-FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender (get installer proposal))))
        (map-set proposals proposal-id (merge proposal { 
            funds-released: (+ (get funds-released proposal) amount),
            executed: (is-eq (+ (get funds-released proposal) amount) (get funding-goal proposal))
        }))
        (ok true)))

(define-public (distribute-energy-rewards (recipients (list 50 { recipient: principal, amount: uint })))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (ok (map distribute-reward recipients))))

(define-private (distribute-reward (reward-data { recipient: principal, amount: uint }))
    (let ((recipient (get recipient reward-data))
          (amount (get amount reward-data))
          (current-rewards (default-to u0 (map-get? energy-rewards recipient))))
        (map-set energy-rewards recipient (+ current-rewards amount))
        true))

(define-public (claim-energy-rewards)
    (let ((caller tx-sender)
          (rewards (default-to u0 (map-get? energy-rewards caller))))
        (asserts! (> rewards u0) ERR-INSUFFICIENT-FUNDS)
        (try! (as-contract (stx-transfer? rewards tx-sender caller)))
        (map-delete energy-rewards caller)
        (ok rewards)))

(define-public (submit-energy-data (proposal-id uint) (month uint) (kwh-produced uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (installer (get installer proposal))
          (current-block burn-block-height)
          (energy-key { proposal-id: proposal-id, month: month }))
        (asserts! (is-eq tx-sender installer) ERR-NOT-AUTHORIZED)
        (asserts! (get executed proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (> kwh-produced u0) ERR-INVALID-ENERGY-DATA)
        (asserts! (is-none (map-get? energy-production energy-key)) ERR-DATA-ALREADY-SUBMITTED)
        (map-set energy-production energy-key {
            kwh-produced: kwh-produced,
            reported-by: installer,
            timestamp: current-block
        })
        (update-project-performance proposal-id kwh-produced)
        (ok true)))

(define-public (claim-performance-rewards (proposal-id uint))
    (let ((caller tx-sender)
          (proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (performance-data (unwrap! (map-get? project-performance proposal-id) ERR-INVALID-PROPOSAL))
          (performance-score (get performance-score performance-data))
          (contribution (default-to u0 (map-get? project-contributions { proposal-id: proposal-id, contributor: caller })))
          (reward-amount (if (>= performance-score MIN-PERFORMANCE-THRESHOLD)
              (/ (* contribution PERFORMANCE-BONUS-RATE) u100)
              u0)))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (get executed proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (> contribution u0) ERR-INSUFFICIENT-FUNDS)
        (asserts! (>= performance-score MIN-PERFORMANCE-THRESHOLD) ERR-INSUFFICIENT-PERFORMANCE)
        (let ((current-rewards (default-to u0 (map-get? energy-rewards caller))))
            (map-set energy-rewards caller (+ current-rewards reward-amount)))
        (ok reward-amount)))

(define-private (update-project-performance (proposal-id uint) (kwh-this-month uint))
    (let ((current-performance (default-to { total-kwh: u0, months-active: u0, performance-score: u0 } 
                               (map-get? project-performance proposal-id)))
          (new-total-kwh (+ (get total-kwh current-performance) kwh-this-month))
          (new-months-active (+ (get months-active current-performance) u1))
          (average-monthly-kwh (/ new-total-kwh new-months-active))
          (calculated-score (/ (* average-monthly-kwh u100) u1200))
          (performance-score (if (> calculated-score u100) u100 calculated-score)))
        (map-set project-performance proposal-id {
            total-kwh: new-total-kwh,
            months-active: new-months-active,
            performance-score: performance-score
        })
        performance-score))

(define-public (emergency-withdraw (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT-OWNER)))
        (ok true)))

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id))

(define-read-only (get-member-info (member principal))
    (map-get? dao-members member))

(define-read-only (get-dao-stats)
    {
        total-members: (var-get proposal-counter),
        total-funds: (var-get total-dao-funds),
        total-proposals: (var-get proposal-counter)
    })

(define-read-only (get-project-contribution (proposal-id uint) (contributor principal))
    (map-get? project-contributions { proposal-id: proposal-id, contributor: contributor }))

(define-read-only (get-energy-rewards (member principal))
    (map-get? energy-rewards member))

(define-read-only (has-voted (proposal-id uint) (voter principal))
    (is-some (map-get? project-votes { proposal-id: proposal-id, voter: voter })))

(define-read-only (is-certified-installer (installer principal))
    (default-to false (map-get? installer-certifications installer)))

(define-read-only (get-contract-balance)
    (stx-get-balance (as-contract tx-sender)))

(define-read-only (calculate-voting-power (member principal))
    (let ((member-data (map-get? dao-members member)))
        (match member-data
            data (get contribution data)
            u0)))

(define-read-only (get-energy-production (proposal-id uint) (month uint))
    (map-get? energy-production { proposal-id: proposal-id, month: month }))

(define-read-only (get-project-performance (proposal-id uint))
    (map-get? project-performance proposal-id))

(define-read-only (calculate-performance-reward (proposal-id uint) (contributor principal))
    (let ((performance-data (map-get? project-performance proposal-id))
          (contribution (default-to u0 (map-get? project-contributions { proposal-id: proposal-id, contributor: contributor }))))
        (match performance-data
            data (if (>= (get performance-score data) MIN-PERFORMANCE-THRESHOLD)
                (/ (* contribution PERFORMANCE-BONUS-RATE) u100)
                u0)
            u0)))

(define-read-only (get-proposal-status (proposal-id uint))
    (let ((proposal (map-get? proposals proposal-id)))
        (match proposal
            data {
                status: (if (get executed data) "completed"
                          (if (get approved data) "approved"
                            (if (> burn-block-height (get voting-ends-at data)) "voting-ended" "voting-active"))),
                funding-progress: (/ (* (get funds-released data) u100) (get funding-goal data))
            }
            { status: "not-found", funding-progress: u0 })))
