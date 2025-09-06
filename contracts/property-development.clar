
;; title: property-development
;; version: 1.0.0
;; summary: Property Development Timeline Management Contract
;; description: A comprehensive contract for managing construction projects with permit tracking, 
;;              inspection coordination, contractor management, and milestone completion tracking

;; constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-status (err u103))
(define-constant err-already-exists (err u104))
(define-constant err-invalid-phase (err u105))

;; data vars
(define-data-var next-project-id uint u1)
(define-data-var next-permit-id uint u1)
(define-data-var next-inspection-id uint u1)
(define-data-var next-contractor-id uint u1)

;; data maps
(define-map projects
  uint
  {
    owner: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    location: (string-ascii 200),
    total-budget: uint,
    current-phase: (string-ascii 50),
    status: (string-ascii 20),
    created-at: uint,
    completion-percentage: uint
  }
)

(define-map permits
  uint
  {
    project-id: uint,
    permit-type: (string-ascii 100),
    status: (string-ascii 20),
    issued-date: (optional uint),
    expiry-date: (optional uint),
    issuing-authority: (string-ascii 100),
    requirements: (string-ascii 500)
  }
)

(define-map inspections
  uint
  {
    project-id: uint,
    inspection-type: (string-ascii 100),
    scheduled-date: uint,
    status: (string-ascii 20),
    inspector: (string-ascii 100),
    results: (optional (string-ascii 500)),
    passed: (optional bool)
  }
)

(define-map contractors
  uint
  {
    project-id: uint,
    name: (string-ascii 100),
    specialty: (string-ascii 100),
    contact: (string-ascii 200),
    status: (string-ascii 20),
    assigned-phase: (string-ascii 50),
    start-date: (optional uint),
    end-date: (optional uint)
  }
)

(define-map milestones
  { project-id: uint, phase: (string-ascii 50) }
  {
    description: (string-ascii 300),
    target-date: uint,
    completion-date: (optional uint),
    status: (string-ascii 20),
    completion-percentage: uint
  }
)

(define-map project-permissions
  { project-id: uint, user: principal }
  { can-view: bool, can-edit: bool }
)

;; public functions

(define-public (create-project (name (string-ascii 100)) 
                              (description (string-ascii 500))
                              (location (string-ascii 200))
                              (total-budget uint))
  (let ((project-id (var-get next-project-id)))
    (map-set projects project-id
      {
        owner: tx-sender,
        name: name,
        description: description,
        location: location,
        total-budget: total-budget,
        current-phase: "Planning",
        status: "Active",
        created-at: stacks-block-height,
        completion-percentage: u0
      }
    )
    (var-set next-project-id (+ project-id u1))
    (ok project-id)
  )
)

(define-public (add-permit (project-id uint)
                          (permit-type (string-ascii 100))
                          (issuing-authority (string-ascii 100))
                          (requirements (string-ascii 500)))
  (let ((permit-id (var-get next-permit-id)))
    (asserts! (is-project-owner-or-authorized project-id tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? projects project-id)) err-not-found)
    (map-set permits permit-id
      {
        project-id: project-id,
        permit-type: permit-type,
        status: "Pending",
        issued-date: none,
        expiry-date: none,
        issuing-authority: issuing-authority,
        requirements: requirements
      }
    )
    (var-set next-permit-id (+ permit-id u1))
    (ok permit-id)
  )
)

(define-public (update-permit-status (permit-id uint) 
                                    (new-status (string-ascii 20))
                                    (issued-date (optional uint))
                                    (expiry-date (optional uint)))
  (match (map-get? permits permit-id)
    permit-data
    (begin
      (asserts! (is-project-owner-or-authorized (get project-id permit-data) tx-sender) err-unauthorized)
      (map-set permits permit-id
        (merge permit-data {
          status: new-status,
          issued-date: issued-date,
          expiry-date: expiry-date
        })
      )
      (ok true)
    )
    err-not-found
  )
)

(define-public (schedule-inspection (project-id uint)
                                   (inspection-type (string-ascii 100))
                                   (scheduled-date uint)
                                   (inspector (string-ascii 100)))
  (let ((inspection-id (var-get next-inspection-id)))
    (asserts! (is-project-owner-or-authorized project-id tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? projects project-id)) err-not-found)
    (map-set inspections inspection-id
      {
        project-id: project-id,
        inspection-type: inspection-type,
        scheduled-date: scheduled-date,
        status: "Scheduled",
        inspector: inspector,
        results: none,
        passed: none
      }
    )
    (var-set next-inspection-id (+ inspection-id u1))
    (ok inspection-id)
  )
)

(define-public (complete-inspection (inspection-id uint)
                                   (results (string-ascii 500))
                                   (passed bool))
  (match (map-get? inspections inspection-id)
    inspection-data
    (begin
      (asserts! (is-project-owner-or-authorized (get project-id inspection-data) tx-sender) err-unauthorized)
      (map-set inspections inspection-id
        (merge inspection-data {
          status: "Completed",
          results: (some results),
          passed: (some passed)
        })
      )
      (ok true)
    )
    err-not-found
  )
)

(define-public (assign-contractor (project-id uint)
                                 (name (string-ascii 100))
                                 (specialty (string-ascii 100))
                                 (contact (string-ascii 200))
                                 (assigned-phase (string-ascii 50)))
  (let ((contractor-id (var-get next-contractor-id)))
    (asserts! (is-project-owner-or-authorized project-id tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? projects project-id)) err-not-found)
    (map-set contractors contractor-id
      {
        project-id: project-id,
        name: name,
        specialty: specialty,
        contact: contact,
        status: "Assigned",
        assigned-phase: assigned-phase,
        start-date: none,
        end-date: none
      }
    )
    (var-set next-contractor-id (+ contractor-id u1))
    (ok contractor-id)
  )
)

(define-public (update-contractor-status (contractor-id uint)
                                        (new-status (string-ascii 20))
                                        (start-date (optional uint))
                                        (end-date (optional uint)))
  (match (map-get? contractors contractor-id)
    contractor-data
    (begin
      (asserts! (is-project-owner-or-authorized (get project-id contractor-data) tx-sender) err-unauthorized)
      (map-set contractors contractor-id
        (merge contractor-data {
          status: new-status,
          start-date: start-date,
          end-date: end-date
        })
      )
      (ok true)
    )
    err-not-found
  )
)

(define-public (create-milestone (project-id uint)
                                (phase (string-ascii 50))
                                (description (string-ascii 300))
                                (target-date uint))
  (begin
    (asserts! (is-project-owner-or-authorized project-id tx-sender) err-unauthorized)
    (asserts! (is-some (map-get? projects project-id)) err-not-found)
    (asserts! (is-none (map-get? milestones { project-id: project-id, phase: phase })) err-already-exists)
    (map-set milestones
      { project-id: project-id, phase: phase }
      {
        description: description,
        target-date: target-date,
        completion-date: none,
        status: "Pending",
        completion-percentage: u0
      }
    )
    (ok true)
  )
)

(define-public (update-milestone (project-id uint)
                                (phase (string-ascii 50))
                                (completion-percentage uint)
                                (status (string-ascii 20)))
  (let ((milestone-key { project-id: project-id, phase: phase }))
    (match (map-get? milestones milestone-key)
      milestone-data
      (begin
        (asserts! (is-project-owner-or-authorized project-id tx-sender) err-unauthorized)
        (asserts! (<= completion-percentage u100) err-invalid-status)
        (map-set milestones milestone-key
          (merge milestone-data {
            completion-percentage: completion-percentage,
            status: status,
            completion-date: (if (is-eq completion-percentage u100)
                               (some stacks-block-height)
                               (get completion-date milestone-data))
          })
        )
        (try! (update-project-phase project-id phase))
        (ok true)
      )
      err-not-found
    )
  )
)

(define-public (update-project-phase (project-id uint) (new-phase (string-ascii 50)))
  (match (map-get? projects project-id)
    project-data
    (begin
      (asserts! (is-project-owner-or-authorized project-id tx-sender) err-unauthorized)
      (map-set projects project-id
        (merge project-data { current-phase: new-phase })
      )
      (ok true)
    )
    err-not-found
  )
)

(define-public (grant-project-access (project-id uint)
                                    (user principal)
                                    (can-view bool)
                                    (can-edit bool))
  (begin
    (asserts! (is-project-owner project-id tx-sender) err-owner-only)
    (map-set project-permissions
      { project-id: project-id, user: user }
      { can-view: can-view, can-edit: can-edit }
    )
    (ok true)
  )
)

;; read only functions

(define-read-only (get-project (project-id uint))
  (map-get? projects project-id)
)

(define-read-only (get-permit (permit-id uint))
  (map-get? permits permit-id)
)

(define-read-only (get-inspection (inspection-id uint))
  (map-get? inspections inspection-id)
)

(define-read-only (get-contractor (contractor-id uint))
  (map-get? contractors contractor-id)
)

(define-read-only (get-milestone (project-id uint) (phase (string-ascii 50)))
  (map-get? milestones { project-id: project-id, phase: phase })
)

(define-read-only (get-project-permissions (project-id uint) (user principal))
  (map-get? project-permissions { project-id: project-id, user: user })
)

(define-read-only (is-project-owner (project-id uint) (user principal))
  (match (map-get? projects project-id)
    project-data (is-eq (get owner project-data) user)
    false
  )
)

(define-read-only (can-edit-project (project-id uint) (user principal))
  (or
    (is-project-owner project-id user)
    (match (get-project-permissions project-id user)
      permissions (get can-edit permissions)
      false
    )
  )
)

(define-read-only (can-view-project (project-id uint) (user principal))
  (or
    (is-project-owner project-id user)
    (match (get-project-permissions project-id user)
      permissions (get can-view permissions)
      false
    )
  )
)

;; private functions

(define-private (is-project-owner-or-authorized (project-id uint) (user principal))
  (or
    (is-project-owner project-id user)
    (can-edit-project project-id user)
  )
)

