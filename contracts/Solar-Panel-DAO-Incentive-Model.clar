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
(define-constant ERR-MILESTONE-NOT-FOUND (err u114))
(define-constant ERR-MILESTONE-ALREADY-COMPLETED (err u115))
(define-constant ERR-INVALID-MILESTONE (err u116))
(define-constant ERR-MILESTONES-NOT-SET (err u117))
(define-constant ERR-ALL-MILESTONES-COMPLETED (err u118))
(define-constant ERR-INVALID-CARBON-CREDITS (err u119))
(define-constant ERR-INSUFFICIENT-CARBON-CREDITS (err u120))
(define-constant ERR-CARBON-CREDIT-NOT-FOUND (err u121))
(define-constant ERR-INVALID-CARBON-PRICE (err u122))
(define-constant ERR-CARBON-CREDIT-ALREADY-SOLD (err u123))
(define-constant ERR-CANNOT-DELEGATE-TO-SELF (err u124))
(define-constant ERR-DELEGATION-LOOP (err u125))

(define-constant VOTING-PERIOD u1440)
(define-constant MIN-CONTRIBUTION u1000000)
(define-constant APPROVAL-THRESHOLD u51)
(define-constant MIN-PERFORMANCE-THRESHOLD u80)
(define-constant PERFORMANCE-BONUS-RATE u20)
(define-constant CARBON-OFFSET-RATE u50)
(define-constant MIN-CARBON-PRICE u100)

(define-data-var proposal-counter uint u0)
(define-data-var total-dao-funds uint u0)
(define-data-var carbon-credit-counter uint u0)

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
(define-map project-milestones { proposal-id: uint, milestone-id: uint } { description: (string-ascii 128), funding-percentage: uint, completed: bool, completed-at: uint })
(define-map project-milestone-count uint uint)
(define-map carbon-credits uint {
    project-id: uint,
    credits-amount: uint,
    price-per-credit: uint,
    created-by: principal,
    created-at: uint,
    sold: bool,
    buyer: (optional principal),
    sold-at: (optional uint)
})
(define-map project-carbon-balance uint uint)
(define-map member-carbon-holdings principal uint)
(define-map vote-delegation principal principal)
(define-map delegated-vote-count principal uint)

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
          (vote-key { proposal-id: proposal-id, voter: caller })
          (vote-weight (+ u1 (default-to u0 (map-get? delegated-vote-count caller)))))
        (asserts! (is-none (map-get? project-votes vote-key)) ERR-ALREADY-VOTED)
        (asserts! (<= current-block (get voting-ends-at proposal)) ERR-VOTING-PERIOD-ENDED)
        (map-set project-votes vote-key vote)
        (if vote
            (map-set proposals proposal-id (merge proposal { yes-votes: (+ (get yes-votes proposal) vote-weight) }))
            (map-set proposals proposal-id (merge proposal { no-votes: (+ (get no-votes proposal) vote-weight) })))
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

(define-public (create-project-milestones (proposal-id uint) (milestones (list 10 { description: (string-ascii 128), funding-percentage: uint })))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (caller tx-sender))
        (asserts! (is-eq caller (get creator proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (get approved proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (not (get executed proposal)) ERR-PROJECT-COMPLETED)
        (asserts! (is-none (map-get? project-milestone-count proposal-id)) ERR-MILESTONES-NOT-SET)
        (let ((total-percentage (fold + (map get-funding-percentage milestones) u0)))
            (asserts! (is-eq total-percentage u100) ERR-INVALID-MILESTONE)
            (map-set project-milestone-count proposal-id (len milestones))
            (ok (map create-milestone-entry (enumerate-milestones milestones proposal-id))))))

(define-private (get-funding-percentage (milestone { description: (string-ascii 128), funding-percentage: uint }))
    (get funding-percentage milestone))

(define-private (enumerate-milestones (milestones (list 10 { description: (string-ascii 128), funding-percentage: uint })) (proposal-id uint))
    (map create-milestone-with-id milestones (generate-milestone-ids (len milestones) proposal-id)))

(define-private (generate-milestone-ids (count uint) (proposal-id uint))
    (map + (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) (list proposal-id proposal-id proposal-id proposal-id proposal-id proposal-id proposal-id proposal-id proposal-id proposal-id)))

(define-private (create-milestone-with-id (milestone { description: (string-ascii 128), funding-percentage: uint }) (id-data uint))
    { milestone: milestone, milestone-id: (mod id-data u100), proposal-id: (/ id-data u100) })

(define-private (create-milestone-entry (milestone-data { milestone: { description: (string-ascii 128), funding-percentage: uint }, milestone-id: uint, proposal-id: uint }))
    (let ((milestone-key { proposal-id: (get proposal-id milestone-data), milestone-id: (get milestone-id milestone-data) })
          (milestone-info (get milestone milestone-data)))
        (map-set project-milestones milestone-key {
            description: (get description milestone-info),
            funding-percentage: (get funding-percentage milestone-info),
            completed: false,
            completed-at: u0
        })
        true))

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

(define-public (complete-milestone (proposal-id uint) (milestone-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (milestone-key { proposal-id: proposal-id, milestone-id: milestone-id })
          (milestone (unwrap! (map-get? project-milestones milestone-key) ERR-MILESTONE-NOT-FOUND))
          (current-block burn-block-height))
        (asserts! (is-eq tx-sender (get installer proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (get approved proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (not (get completed milestone)) ERR-MILESTONE-ALREADY-COMPLETED)
        (map-set project-milestones milestone-key (merge milestone {
            completed: true,
            completed-at: current-block
        }))
        (let ((release-amount (/ (* (get funding-goal proposal) (get funding-percentage milestone)) u100)))
            (try! (as-contract (stx-transfer? release-amount tx-sender (get installer proposal))))
            (map-set proposals proposal-id (merge proposal { 
                funds-released: (+ (get funds-released proposal) release-amount)
            }))
            (ok release-amount))))

(define-public (mark-project-completed (proposal-id uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (milestone-count (default-to u0 (map-get? project-milestone-count proposal-id))))
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (get approved proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (not (get executed proposal)) ERR-PROJECT-COMPLETED)
        (asserts! (> milestone-count u0) ERR-MILESTONES-NOT-SET)
        (map-set proposals proposal-id (merge proposal { executed: true }))
        (ok true)))

(define-read-only (count-completed-milestones (proposal-id uint) (milestone-count uint))
    (+ (if (and (> milestone-count u0) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u1 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u1) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u2 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u2) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u3 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u3) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u4 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u4) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u5 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u5) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u6 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u6) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u7 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u7) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u8 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u8) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u9 }) milestone (get completed milestone) false)) u1 u0)
       (if (and (> milestone-count u9) (match (map-get? project-milestones { proposal-id: proposal-id, milestone-id: u10 }) milestone (get completed milestone) false)) u1 u0)))

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

;; Carbon Credit Trading System Functions

(define-public (generate-carbon-credits (proposal-id uint) (energy-kwh uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (caller tx-sender)
          (carbon-credits-generated (/ (* energy-kwh CARBON-OFFSET-RATE) u100))
          (current-balance (default-to u0 (map-get? project-carbon-balance proposal-id))))
        (asserts! (is-eq caller (get installer proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (get executed proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (> energy-kwh u0) ERR-INVALID-CARBON-CREDITS)
        (map-set project-carbon-balance proposal-id (+ current-balance carbon-credits-generated))
        (let ((member-current (default-to u0 (map-get? member-carbon-holdings caller))))
            (map-set member-carbon-holdings caller (+ member-current carbon-credits-generated)))
        (ok carbon-credits-generated)))

(define-public (list-carbon-credits-for-sale (proposal-id uint) (credits-amount uint) (price-per-credit uint))
    (let ((proposal (unwrap! (map-get? proposals proposal-id) ERR-PROPOSAL-NOT-FOUND))
          (caller tx-sender)
          (current-balance (default-to u0 (map-get? project-carbon-balance proposal-id)))
          (credit-id (+ (var-get carbon-credit-counter) u1))
          (current-block burn-block-height))
        (asserts! (is-eq caller (get installer proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (get executed proposal) ERR-PROPOSAL-NOT-APPROVED)
        (asserts! (> credits-amount u0) ERR-INVALID-CARBON-CREDITS)
        (asserts! (>= price-per-credit MIN-CARBON-PRICE) ERR-INVALID-CARBON-PRICE)
        (asserts! (>= current-balance credits-amount) ERR-INSUFFICIENT-CARBON-CREDITS)
        (map-set project-carbon-balance proposal-id (- current-balance credits-amount))
        (map-set carbon-credits credit-id {
            project-id: proposal-id,
            credits-amount: credits-amount,
            price-per-credit: price-per-credit,
            created-by: caller,
            created-at: current-block,
            sold: false,
            buyer: none,
            sold-at: none
        })
        (var-set carbon-credit-counter credit-id)
        (ok credit-id)))

(define-public (buy-carbon-credits (credit-id uint))
    (let ((credit-listing (unwrap! (map-get? carbon-credits credit-id) ERR-CARBON-CREDIT-NOT-FOUND))
          (caller tx-sender)
          (total-cost (* (get credits-amount credit-listing) (get price-per-credit credit-listing)))
          (current-block burn-block-height)
          (member-current (default-to u0 (map-get? member-carbon-holdings caller))))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (not (get sold credit-listing)) ERR-CARBON-CREDIT-ALREADY-SOLD)
        (try! (stx-transfer? total-cost caller (get created-by credit-listing)))
        (map-set carbon-credits credit-id (merge credit-listing {
            sold: true,
            buyer: (some caller),
            sold-at: (some current-block)
        }))
        (map-set member-carbon-holdings caller (+ member-current (get credits-amount credit-listing)))
        (ok total-cost)))

(define-public (retire-carbon-credits (amount uint))
    (let ((caller tx-sender)
          (current-holdings (default-to u0 (map-get? member-carbon-holdings caller))))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (> amount u0) ERR-INVALID-CARBON-CREDITS)
        (asserts! (>= current-holdings amount) ERR-INSUFFICIENT-CARBON-CREDITS)
        (map-set member-carbon-holdings caller (- current-holdings amount))
        (ok amount)))

;; Carbon Credit Read-Only Functions

(define-read-only (get-carbon-credit-listing (credit-id uint))
    (map-get? carbon-credits credit-id))

(define-read-only (get-project-carbon-balance (proposal-id uint))
    (default-to u0 (map-get? project-carbon-balance proposal-id)))

(define-read-only (get-member-carbon-holdings (member principal))
    (default-to u0 (map-get? member-carbon-holdings member)))

(define-read-only (calculate-carbon-credits (energy-kwh uint))
    (/ (* energy-kwh CARBON-OFFSET-RATE) u100))

(define-read-only (get-carbon-credit-stats)
    {
        total-credits-listed: (var-get carbon-credit-counter),
        carbon-offset-rate: CARBON-OFFSET-RATE,
        min-carbon-price: MIN-CARBON-PRICE
    })

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

(define-read-only (get-milestone (proposal-id uint) (milestone-id uint))
    (map-get? project-milestones { proposal-id: proposal-id, milestone-id: milestone-id }))

(define-read-only (get-milestone-count (proposal-id uint))
    (default-to u0 (map-get? project-milestone-count proposal-id)))

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

(define-public (delegate-vote (delegate-to principal))
    (let ((caller tx-sender)
          (current-delegate (map-get? vote-delegation caller))
          (delegate-member-check (map-get? dao-members delegate-to)))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (is-some delegate-member-check) ERR-NOT-MEMBER)
        (asserts! (not (is-eq caller delegate-to)) ERR-CANNOT-DELEGATE-TO-SELF)
        (asserts! (is-none (map-get? vote-delegation delegate-to)) ERR-DELEGATION-LOOP)
        (match current-delegate
            old-delegate
                (let ((old-count (default-to u0 (map-get? delegated-vote-count old-delegate))))
                    (if (> old-count u0)
                        (map-set delegated-vote-count old-delegate (- old-count u1))
                        true))
            true)
        (map-set vote-delegation caller delegate-to)
        (let ((new-count (default-to u0 (map-get? delegated-vote-count delegate-to))))
            (map-set delegated-vote-count delegate-to (+ new-count u1)))
        (ok true)))

(define-public (revoke-delegation)
    (let ((caller tx-sender)
          (current-delegate (map-get? vote-delegation caller)))
        (asserts! (is-some (map-get? dao-members caller)) ERR-NOT-MEMBER)
        (asserts! (is-some current-delegate) ERR-NOT-AUTHORIZED)
        (match current-delegate
            delegate
                (let ((delegate-count (default-to u0 (map-get? delegated-vote-count delegate))))
                    (if (> delegate-count u0)
                        (map-set delegated-vote-count delegate (- delegate-count u1))
                        true)
                    (map-delete vote-delegation caller)
                    (ok true))
            ERR-NOT-AUTHORIZED)))

(define-read-only (get-vote-delegate (member principal))
    (map-get? vote-delegation member))

(define-read-only (get-delegated-vote-count (member principal))
    (default-to u0 (map-get? delegated-vote-count member)))

(define-read-only (get-total-voting-power (member principal))
    (+ u1 (get-delegated-vote-count member)))
