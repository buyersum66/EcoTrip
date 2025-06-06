(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PROJECT_NOT_FOUND (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_PROJECT_NOT_APPROVED (err u106))
(define-constant ERR_ALREADY_FUNDED (err u107))

(define-data-var next-project-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var min-proposal-deposit uint u1000000)
(define-data-var voting-period uint u1440)

(define-map projects
  uint
  {
    title: (string-ascii 100),
    description: (string-ascii 500),
    funding-goal: uint,
    proposer: principal,
    votes-for: uint,
    votes-against: uint,
    voting-ends: uint,
    status: (string-ascii 20),
    funded-amount: uint
  }
)

(define-map member-votes
  { project-id: uint, voter: principal }
  { vote: bool, amount: uint }
)

(define-map member-stakes
  principal
  uint
)

(define-map project-funders
  { project-id: uint, funder: principal }
  uint
)

(define-public (join-dao (stake-amount uint))
  (begin
    (asserts! (> stake-amount u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set member-stakes tx-sender 
      (+ (default-to u0 (map-get? member-stakes tx-sender)) stake-amount))
    (var-set treasury-balance (+ (var-get treasury-balance) stake-amount))
    (ok true)
  )
)

(define-public (propose-project (title (string-ascii 100)) (description (string-ascii 500)) (funding-goal uint))
  (let
    (
      (project-id (var-get next-project-id))
      (member-stake (default-to u0 (map-get? member-stakes tx-sender)))
    )
    (asserts! (>= member-stake (var-get min-proposal-deposit)) ERR_NOT_AUTHORIZED)
    (asserts! (> funding-goal u0) ERR_INVALID_AMOUNT)
    (map-set projects project-id
      {
        title: title,
        description: description,
        funding-goal: funding-goal,
        proposer: tx-sender,
        votes-for: u0,
        votes-against: u0,
        voting-ends: (+ stacks-block-height (var-get voting-period)),
        status: "voting",
        funded-amount: u0
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (vote-on-project (project-id uint) (vote-for bool))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (member-stake (default-to u0 (map-get? member-stakes tx-sender)))
      (existing-vote (map-get? member-votes { project-id: project-id, voter: tx-sender }))
    )
    (asserts! (> member-stake u0) ERR_NOT_AUTHORIZED)
    (asserts! (<= stacks-block-height (get voting-ends project)) ERR_VOTING_ENDED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (map-set member-votes 
      { project-id: project-id, voter: tx-sender }
      { vote: vote-for, amount: member-stake }
    )
    (if vote-for
      (map-set projects project-id
        (merge project { votes-for: (+ (get votes-for project) member-stake) }))
      (map-set projects project-id
        (merge project { votes-against: (+ (get votes-against project) member-stake) }))
    )
    (ok true)
  )
)

(define-public (finalize-voting (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (> stacks-block-height (get voting-ends project)) ERR_VOTING_ENDED)
    (if (> (get votes-for project) (get votes-against project))
      (map-set projects project-id (merge project { status: "approved" }))
      (map-set projects project-id (merge project { status: "rejected" }))
    )
    (ok true)
  )
)

(define-public (fund-project (project-id uint) (amount uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (current-funding (default-to u0 (map-get? project-funders { project-id: project-id, funder: tx-sender })))
    )
    (asserts! (is-eq (get status project) "approved") ERR_PROJECT_NOT_APPROVED)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (asserts! (< (get funded-amount project) (get funding-goal project)) ERR_ALREADY_FUNDED)
    (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
    (map-set project-funders 
      { project-id: project-id, funder: tx-sender }
      (+ current-funding amount)
    )
    (let
      (
        (new-funded-amount (+ (get funded-amount project) amount))
      )
      (map-set projects project-id
        (merge project { 
          funded-amount: new-funded-amount,
          status: (if (>= new-funded-amount (get funding-goal project)) "funded" "approved")
        })
      )
    )
    (ok true)
  )
)

(define-public (release-funds (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) "funded") ERR_PROJECT_NOT_APPROVED)
    (try! (as-contract (stx-transfer? (get funded-amount project) tx-sender (get proposer project))))
    (map-set projects project-id (merge project { status: "completed" }))
    (ok true)
  )
)

(define-public (withdraw-stake (amount uint))
  (let
    (
      (current-stake (default-to u0 (map-get? member-stakes tx-sender)))
    )
    (asserts! (>= current-stake amount) ERR_INSUFFICIENT_FUNDS)
    (asserts! (> amount u0) ERR_INVALID_AMOUNT)
    (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
    (map-set member-stakes tx-sender (- current-stake amount))
    (var-set treasury-balance (- (var-get treasury-balance) amount))
    (ok true)
  )
)

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-member-stake (member principal))
  (default-to u0 (map-get? member-stakes member))
)

(define-read-only (get-member-vote (project-id uint) (voter principal))
  (map-get? member-votes { project-id: project-id, voter: voter })
)

(define-read-only (get-project-funding (project-id uint) (funder principal))
  (default-to u0 (map-get? project-funders { project-id: project-id, funder: funder }))
)

(define-read-only (get-treasury-balance)
  (var-get treasury-balance)
)

(define-read-only (get-next-project-id)
  (var-get next-project-id)
)

(define-read-only (get-voting-period)
  (var-get voting-period)
)

(define-read-only (get-min-proposal-deposit)
  (var-get min-proposal-deposit)
)

(define-public (update-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set voting-period new-period)
    (ok true)
  )
)

(define-public (update-min-deposit (new-deposit uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set min-proposal-deposit new-deposit)
    (ok true)
  )
)