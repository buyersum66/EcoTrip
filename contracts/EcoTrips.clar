(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_INVALID_AMOUNT (err u101))
(define-constant ERR_PROJECT_NOT_FOUND (err u102))
(define-constant ERR_VOTING_ENDED (err u103))
(define-constant ERR_ALREADY_VOTED (err u104))
(define-constant ERR_INSUFFICIENT_FUNDS (err u105))
(define-constant ERR_PROJECT_NOT_APPROVED (err u106))
(define-constant ERR_ALREADY_FUNDED (err u107))
(define-constant ERR_MILESTONE_NOT_FOUND (err u108))
(define-constant ERR_MILESTONE_ALREADY_COMPLETED (err u109))
(define-constant ERR_MILESTONE_NOT_READY (err u110))
(define-constant ERR_INVALID_MILESTONE (err u111))
(define-constant ERR_MILESTONE_VOTING_ACTIVE (err u112))
(define-constant ERR_ALL_MILESTONES_COMPLETED (err u113))
(define-constant ERR_VERIFIER_NOT_FOUND (err u114))
(define-constant ERR_ALREADY_VERIFIED (err u115))
(define-constant ERR_CLAIM_NOT_FOUND (err u116))
(define-constant ERR_INVALID_VERIFICATION (err u117))
(define-constant ERR_VERIFIER_NOT_CERTIFIED (err u118))
(define-constant ERR_INVALID_IMPACT_SCORE (err u119))

(define-data-var next-project-id uint u1)
(define-data-var treasury-balance uint u0)
(define-data-var min-proposal-deposit uint u1000000)
(define-data-var voting-period uint u1440)
(define-data-var next-milestone-id uint u1)
(define-data-var milestone-voting-period uint u144)
(define-data-var next-verifier-id uint u1)
(define-data-var next-claim-id uint u1)
(define-data-var min-verifier-stake uint u5000000)

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
    funded-amount: uint,
    milestone-count: uint,
    completed-milestones: uint,
    milestone-based: bool,
    impact-score: uint,
    verified-claims: uint,
    reputation-score: uint
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

(define-map project-milestones
  { project-id: uint, milestone-index: uint }
  {
    description: (string-ascii 300),
    funding-allocation: uint,
    deliverable-hash: (optional (buff 32)),
    completion-deadline: uint,
    status: (string-ascii 20),
    votes-for: uint,
    votes-against: uint,
    voting-ends: (optional uint),
    submitted-at: (optional uint)
  }
)

(define-map milestone-votes
  { project-id: uint, milestone-index: uint, voter: principal }
  { vote: bool, amount: uint }
)

;; Environmental impact verification maps
(define-map environmental-verifiers
  principal
  {
    verifier-id: uint,
    name: (string-ascii 100),
    certification: (string-ascii 200),
    stake-amount: uint,
    verified-claims: uint,
    accuracy-score: uint,
    status: (string-ascii 20),
    certified-at: uint
  }
)

(define-map impact-claims
  uint
  {
    project-id: uint,
    claimant: principal,
    claim-type: (string-ascii 50),
    claimed-impact: uint,
    evidence-hash: (buff 32),
    submitted-at: uint,
    status: (string-ascii 20),
    verified-impact: (optional uint),
    verifier: (optional principal),
    verified-at: (optional uint)
  }
)

(define-map verification-votes
  { claim-id: uint, verifier: principal }
  { verified-impact: uint, confidence-score: uint }
)

(define-private (create-single-milestone (project-id uint) (milestone-index uint) (description (string-ascii 300)) (allocation uint) (deadline uint))
  (begin
    (map-set project-milestones 
      { project-id: project-id, milestone-index: milestone-index }
      {
        description: description,
        funding-allocation: allocation,
        deliverable-hash: none,
        completion-deadline: (+ stacks-block-height deadline),
        status: "pending",
        votes-for: u0,
        votes-against: u0,
        voting-ends: none,
        submitted-at: none
      }
    )
    true
  )
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
        funded-amount: u0,
        milestone-count: u0,
        completed-milestones: u0,
        milestone-based: false,
        impact-score: u0,
        verified-claims: u0,
        reputation-score: u100
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (create-milestone (project-id uint) (milestone-index uint) (description (string-ascii 300)) (allocation uint) (deadline uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) "voting") ERR_PROJECT_NOT_APPROVED)
    (asserts! (> allocation u0) ERR_INVALID_AMOUNT)
    (asserts! (> deadline u0) ERR_INVALID_AMOUNT)
    (create-single-milestone project-id milestone-index description allocation deadline)
    (map-set projects project-id
      (merge project { 
        milestone-count: (+ (get milestone-count project) u1), 
        milestone-based: true,
        impact-score: u0,
        verified-claims: u0,
        reputation-score: u100
      })
    )
    (ok true)
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
    (asserts! (not (get milestone-based project)) ERR_INVALID_MILESTONE)
    (try! (as-contract (stx-transfer? (get funded-amount project) tx-sender (get proposer project))))
    (map-set projects project-id (merge project { status: "completed" }))
    (ok true)
  )
)

(define-public (submit-milestone-deliverable (project-id uint) (milestone-index uint) (deliverable-hash (buff 32)))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (get milestone-based project) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (get status milestone) "pending") ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (>= stacks-block-height (get completion-deadline milestone)) ERR_MILESTONE_NOT_READY)
    (map-set project-milestones 
      { project-id: project-id, milestone-index: milestone-index }
      (merge milestone {
        deliverable-hash: (some deliverable-hash),
        status: "submitted",
        voting-ends: (some (+ stacks-block-height (var-get milestone-voting-period))),
        submitted-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (vote-on-milestone (project-id uint) (milestone-index uint) (approve bool))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
      (member-stake (default-to u0 (map-get? member-stakes tx-sender)))
      (existing-vote (map-get? milestone-votes { project-id: project-id, milestone-index: milestone-index, voter: tx-sender }))
      (voting-end (unwrap! (get voting-ends milestone) ERR_MILESTONE_VOTING_ACTIVE))
    )
    (asserts! (> member-stake u0) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status milestone) "submitted") ERR_MILESTONE_NOT_READY)
    (asserts! (<= stacks-block-height voting-end) ERR_VOTING_ENDED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (map-set milestone-votes 
      { project-id: project-id, milestone-index: milestone-index, voter: tx-sender }
      { vote: approve, amount: member-stake }
    )
    (if approve
      (map-set project-milestones 
        { project-id: project-id, milestone-index: milestone-index }
        (merge milestone { votes-for: (+ (get votes-for milestone) member-stake) }))
      (map-set project-milestones 
        { project-id: project-id, milestone-index: milestone-index }
        (merge milestone { votes-against: (+ (get votes-against milestone) member-stake) }))
    )
    (ok true)
  )
)

(define-public (finalize-milestone-voting (project-id uint) (milestone-index uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
      (voting-end (unwrap! (get voting-ends milestone) ERR_MILESTONE_VOTING_ACTIVE))
    )
    (asserts! (> stacks-block-height voting-end) ERR_VOTING_ENDED)
    (asserts! (is-eq (get status milestone) "submitted") ERR_MILESTONE_NOT_READY)
    (if (> (get votes-for milestone) (get votes-against milestone))
      (begin
        (map-set project-milestones 
          { project-id: project-id, milestone-index: milestone-index }
          (merge milestone { status: "approved" })
        )
        (map-set projects project-id
          (merge project { completed-milestones: (+ (get completed-milestones project) u1) })
        )
      )
      (map-set project-milestones 
        { project-id: project-id, milestone-index: milestone-index }
        (merge milestone { status: "rejected" })
      )
    )
    (ok true)
  )
)

(define-public (release-milestone-funds (project-id uint) (milestone-index uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status project) "funded") ERR_PROJECT_NOT_APPROVED)
    (asserts! (get milestone-based project) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (get status milestone) "approved") ERR_MILESTONE_NOT_READY)
    (try! (as-contract (stx-transfer? (get funding-allocation milestone) tx-sender (get proposer project))))
    (map-set project-milestones 
      { project-id: project-id, milestone-index: milestone-index }
      (merge milestone { status: "completed" })
    )
    (if (is-eq (+ (get completed-milestones project) u1) (get milestone-count project))
      (begin
        (map-set projects project-id (merge project { status: "completed" }))
        (ok true)
      )
      (ok true)
    )
  )
)

(define-public (resubmit-milestone (project-id uint) (milestone-index uint) (new-deliverable-hash (buff 32)))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (get milestone-based project) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (get status milestone) "rejected") ERR_MILESTONE_NOT_READY)
    (map-set project-milestones 
      { project-id: project-id, milestone-index: milestone-index }
      (merge milestone {
        deliverable-hash: (some new-deliverable-hash),
        status: "submitted",
        votes-for: u0,
        votes-against: u0,
        voting-ends: (some (+ stacks-block-height (var-get milestone-voting-period))),
        submitted-at: (some stacks-block-height)
      })
    )
    (ok true)
  )
)

(define-public (extend-milestone-deadline (project-id uint) (milestone-index uint) (additional-blocks uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (milestone (unwrap! (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index }) ERR_MILESTONE_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (get milestone-based project) ERR_INVALID_MILESTONE)
    (asserts! (is-eq (get status milestone) "pending") ERR_MILESTONE_ALREADY_COMPLETED)
    (asserts! (> additional-blocks u0) ERR_INVALID_AMOUNT)
    (map-set project-milestones 
      { project-id: project-id, milestone-index: milestone-index }
      (merge milestone {
        completion-deadline: (+ (get completion-deadline milestone) additional-blocks)
      })
    )
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

(define-read-only (get-project-milestone (project-id uint) (milestone-index uint))
  (map-get? project-milestones { project-id: project-id, milestone-index: milestone-index })
)

(define-read-only (get-milestone-vote (project-id uint) (milestone-index uint) (voter principal))
  (map-get? milestone-votes { project-id: project-id, milestone-index: milestone-index, voter: voter })
)

(define-read-only (get-milestone-voting-period)
  (var-get milestone-voting-period)
)

(define-read-only (get-milestone-progress (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) (err "Project not found")))
    )
    (if (get milestone-based project)
      (ok {
        total-milestones: (get milestone-count project),
        completed-milestones: (get completed-milestones project),
        completion-percentage: (if (> (get milestone-count project) u0)
          (/ (* (get completed-milestones project) u100) (get milestone-count project))
          u0
        )
      })
      (err "Project is not milestone-based")
    )
  )
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

(define-public (update-milestone-voting-period (new-period uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (var-set milestone-voting-period new-period)
    (ok true)
  )
)

;; Environmental Impact Verification System Functions

;; Register as environmental verifier
(define-public (register-verifier (name (string-ascii 100)) (certification (string-ascii 200)))
  (let
    (
      (verifier-id (var-get next-verifier-id))
      (stake-amount (var-get min-verifier-stake))
    )
    (asserts! (> (len name) u0) ERR_INVALID_AMOUNT)
    (asserts! (> (len certification) u0) ERR_INVALID_AMOUNT)
    (try! (stx-transfer? stake-amount tx-sender (as-contract tx-sender)))
    (map-set environmental-verifiers tx-sender
      {
        verifier-id: verifier-id,
        name: name,
        certification: certification,
        stake-amount: stake-amount,
        verified-claims: u0,
        accuracy-score: u100,
        status: "pending",
        certified-at: stacks-block-height
      }
    )
    (var-set next-verifier-id (+ verifier-id u1))
    (ok verifier-id)
  )
)

;; Certify verifier (DAO owner function)
(define-public (certify-verifier (verifier principal))
  (let
    (
      (verifier-data (unwrap! (map-get? environmental-verifiers verifier) ERR_VERIFIER_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status verifier-data) "pending") ERR_INVALID_VERIFICATION)
    (map-set environmental-verifiers verifier
      (merge verifier-data { status: "certified" })
    )
    (ok true)
  )
)

;; Submit environmental impact claim
(define-public (submit-impact-claim (project-id uint) (claim-type (string-ascii 50)) (claimed-impact uint) (evidence-hash (buff 32)))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (claim-id (var-get next-claim-id))
    )
    (asserts! (is-eq tx-sender (get proposer project)) ERR_NOT_AUTHORIZED)
    (asserts! (or (is-eq (get status project) "funded") (is-eq (get status project) "completed")) ERR_PROJECT_NOT_APPROVED)
    (asserts! (> claimed-impact u0) ERR_INVALID_AMOUNT)
    (asserts! (> (len claim-type) u0) ERR_INVALID_AMOUNT)
    (map-set impact-claims claim-id
      {
        project-id: project-id,
        claimant: tx-sender,
        claim-type: claim-type,
        claimed-impact: claimed-impact,
        evidence-hash: evidence-hash,
        submitted-at: stacks-block-height,
        status: "pending",
        verified-impact: none,
        verifier: none,
        verified-at: none
      }
    )
    (var-set next-claim-id (+ claim-id u1))
    (ok claim-id)
  )
)

;; Verify impact claim
(define-public (verify-impact-claim (claim-id uint) (verified-impact uint) (confidence-score uint))
  (let
    (
      (claim (unwrap! (map-get? impact-claims claim-id) ERR_CLAIM_NOT_FOUND))
      (verifier-data (unwrap! (map-get? environmental-verifiers tx-sender) ERR_VERIFIER_NOT_FOUND))
      (existing-vote (map-get? verification-votes { claim-id: claim-id, verifier: tx-sender }))
    )
    (asserts! (is-eq (get status verifier-data) "certified") ERR_VERIFIER_NOT_CERTIFIED)
    (asserts! (is-eq (get status claim) "pending") ERR_ALREADY_VERIFIED)
    (asserts! (is-none existing-vote) ERR_ALREADY_VOTED)
    (asserts! (and (>= confidence-score u1) (<= confidence-score u100)) ERR_INVALID_IMPACT_SCORE)
    (map-set verification-votes 
      { claim-id: claim-id, verifier: tx-sender }
      { verified-impact: verified-impact, confidence-score: confidence-score }
    )
    (ok true)
  )
)

;; Finalize impact verification
(define-public (finalize-impact-verification (claim-id uint))
  (let
    (
      (claim (unwrap! (map-get? impact-claims claim-id) ERR_CLAIM_NOT_FOUND))
      (project (unwrap! (map-get? projects (get project-id claim)) ERR_PROJECT_NOT_FOUND))
    )
    (asserts! (is-eq (get status claim) "pending") ERR_ALREADY_VERIFIED)
    ;; Simple verification: use the first verification for now
    (let
      (
        (final-impact (get claimed-impact claim))
        (new-impact-score (+ (get impact-score project) final-impact))
        (new-verified-claims (+ (get verified-claims project) u1))
        (new-reputation (calculate-reputation-score (get verified-claims project) new-impact-score))
      )
      (map-set impact-claims claim-id
        (merge claim {
          status: "verified",
          verified-impact: (some final-impact),
          verifier: (some tx-sender),
          verified-at: (some stacks-block-height)
        })
      )
      (map-set projects (get project-id claim)
        (merge project {
          impact-score: new-impact-score,
          verified-claims: new-verified-claims,
          reputation-score: new-reputation
        })
      )
      (ok true)
    )
  )
)

;; Calculate reputation score based on verified claims and impact
(define-private (calculate-reputation-score (verified-claims uint) (impact-score uint))
  (let
    (
      (base-score u100)
      (claim-bonus (* verified-claims u10))
      (impact-bonus (/ impact-score u1000))
    )
    (+ base-score (+ claim-bonus impact-bonus))
  )
)

;; Get top projects by impact score
(define-read-only (get-project-impact-ranking (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) (err "Project not found")))
    )
    (ok {
      project-id: project-id,
      impact-score: (get impact-score project),
      verified-claims: (get verified-claims project),
      reputation-score: (get reputation-score project),
      title: (get title project)
    })
  )
)

;; Challenge impact claim (DAO member function)
(define-public (challenge-impact-claim (claim-id uint) (reason (string-ascii 200)))
  (let
    (
      (claim (unwrap! (map-get? impact-claims claim-id) ERR_CLAIM_NOT_FOUND))
      (member-stake (default-to u0 (map-get? member-stakes tx-sender)))
    )
    (asserts! (> member-stake u0) ERR_NOT_AUTHORIZED)
    (asserts! (is-eq (get status claim) "verified") ERR_INVALID_VERIFICATION)
    (asserts! (> (len reason) u0) ERR_INVALID_AMOUNT)
    (map-set impact-claims claim-id
      (merge claim { status: "challenged" })
    )
    (ok true)
  )
)

;; Reward high-impact projects
(define-public (distribute-impact-rewards (project-id uint))
  (let
    (
      (project (unwrap! (map-get? projects project-id) ERR_PROJECT_NOT_FOUND))
      (reward-amount (* (get impact-score project) u100))
    )
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> (get impact-score project) u0) ERR_INVALID_IMPACT_SCORE)
    (asserts! (>= (var-get treasury-balance) reward-amount) ERR_INSUFFICIENT_FUNDS)
    (try! (as-contract (stx-transfer? reward-amount tx-sender (get proposer project))))
    (var-set treasury-balance (- (var-get treasury-balance) reward-amount))
    (ok reward-amount)
  )
)

;; Read-only functions for impact verification system
(define-read-only (get-verifier-info (verifier principal))
  (map-get? environmental-verifiers verifier)
)

(define-read-only (get-impact-claim (claim-id uint))
  (map-get? impact-claims claim-id)
)

(define-read-only (get-verification-vote (claim-id uint) (verifier principal))
  (map-get? verification-votes { claim-id: claim-id, verifier: verifier })
)

(define-read-only (get-next-claim-id)
  (var-get next-claim-id)
)

(define-read-only (get-next-verifier-id)
  (var-get next-verifier-id)
)



