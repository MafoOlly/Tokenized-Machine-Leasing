
(define-non-fungible-token machine uint)

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-MACHINE-NOT-FOUND (err u101))
(define-constant ERR-ALREADY-LEASED (err u102))
(define-constant ERR-NOT-LEASED (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-INVALID-DURATION (err u105))
(define-constant ERR-LEASE-EXPIRED (err u106))
(define-constant ERR-LEASE-ACTIVE (err u107))
(define-constant ERR-NOT-OWNER (err u108))
(define-constant ERR-MACHINE-EXISTS (err u109))
(define-constant ERR-MAINTENANCE-SCHEDULED (err u110))
(define-constant ERR-MAINTENANCE-NOT-FOUND (err u111))
(define-constant ERR-INVALID-MAINTENANCE-PERIOD (err u112))
(define-constant ERR-USAGE-THRESHOLD-NOT-MET (err u113))
(define-constant ERR-INVALID-KPI (err u114))
(define-constant ERR-SERVICE-ALREADY-COMPLETED (err u115))
(define-constant ERR-INVALID-PRICING-PARAMS (err u116))
(define-constant ERR-PRICING-DISABLED (err u117))
(define-constant ERR-ALREADY-RATED (err u118))
(define-constant ERR-INVALID-RATING (err u119))
(define-constant ERR-NOT-LESSEE (err u120))
(define-constant ERR-LEASE-NOT-ENDED (err u121))

(define-data-var next-machine-id uint u1)
(define-data-var platform-fee-rate uint u250)
(define-data-var maintenance-threshold-hours uint u1000)
(define-data-var service-request-counter uint u0)

(define-map machines 
  uint 
  {
    factory: principal,
    name: (string-ascii 64),
    hourly-rate: uint,
    is-active: bool,
    created-at: uint
  }
)

(define-map machine-leases 
  uint 
  {
    lessee: principal,
    start-block: uint,
    end-block: uint,
    total-cost: uint,
    is-active: bool
  }
)

(define-map factory-earnings principal uint)
(define-map platform-earnings principal uint)

(define-map machine-maintenance
  uint
  {
    start-block: uint,
    end-block: uint,
    description: (string-ascii 128),
    is-active: bool
  }
)

(define-map machine-usage-stats
  uint
  {
    total-hours-used: uint,
    total-leases: uint,
    last-service-block: uint,
    hours-since-service: uint,
    efficiency-rating: uint,
    uptime-percentage: uint,
    maintenance-score: uint
  }
)

(define-map machine-kpis
  uint
  {
    avg-utilization: uint,
    peak-performance: uint,
    failure-count: uint,
    service-intervals: uint,
    predicted-next-service: uint,
    health-status: (string-ascii 20)
  }
)

(define-map service-requests
  uint
  {
    machine-id: uint,
    request-type: (string-ascii 50),
    priority: uint,
    requested-at: uint,
    completed-at: (optional uint),
    service-provider: (optional principal),
    estimated-duration: uint,
    cost-estimate: uint,
    is-automated: bool
  }
)

(define-map maintenance-history
  { machine-id: uint, service-block: uint }
  {
    service-type: (string-ascii 50),
    duration: uint,
    cost: uint,
    performance-impact: int,
    next-recommended: uint
  }
)

(define-map dynamic-pricing-config
  uint
  {
    base-rate: uint,
    demand-multiplier: uint,
    utilization-discount: uint,
    peak-hours-multiplier: uint,
    performance-bonus: uint,
    enabled: bool,
    min-rate: uint,
    max-rate: uint
  }
)

(define-map machine-demand-metrics
  uint
  {
    lease-requests-count: uint,
    recent-block-height: uint,
    demand-score: uint
  }
)

(define-map machine-reputation
  uint
  {
    total-ratings: uint,
    rating-sum: uint,
    avg-rating: uint,
    five-star-count: uint,
    one-star-count: uint
  }
)

(define-map lease-ratings
  { machine-id: uint, lessee: principal, lease-end-block: uint }
  {
    rating: uint,
    feedback-type: (string-ascii 20),
    rated-at: uint
  }
)

(define-public (tokenize-machine (name (string-ascii 64)) (hourly-rate uint))
  (let 
    (
      (machine-id (var-get next-machine-id))
    )
    (asserts! (> hourly-rate u0) ERR-INSUFFICIENT-PAYMENT)
    (asserts! (> (len name) u0) ERR-NOT-AUTHORIZED)
    
    (try! (nft-mint? machine machine-id tx-sender))
    
    (map-set machines machine-id {
      factory: tx-sender,
      name: name,
      hourly-rate: hourly-rate,
      is-active: true,
      created-at: stacks-block-height
    })
    
    (map-set machine-usage-stats machine-id {
      total-hours-used: u0,
      total-leases: u0,
      last-service-block: stacks-block-height,
      hours-since-service: u0,
      efficiency-rating: u100,
      uptime-percentage: u10000,
      maintenance-score: u100
    })
    
    (map-set machine-kpis machine-id {
      avg-utilization: u0,
      peak-performance: u0,
      failure-count: u0,
      service-intervals: u0,
      predicted-next-service: (+ stacks-block-height (var-get maintenance-threshold-hours)),
      health-status: "Healthy"
    })
    
    (map-set machine-reputation machine-id {
      total-ratings: u0,
      rating-sum: u0,
      avg-rating: u0,
      five-star-count: u0,
      one-star-count: u0
    })
    
    (var-set next-machine-id (+ machine-id u1))
    (ok machine-id)
  )
)

(define-public (lease-machine (machine-id uint) (duration-blocks uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (current-lease (map-get? machine-leases machine-id))
      (base-hourly-rate (get hourly-rate machine-data))
      (effective-rate (get-effective-pricing machine-id base-hourly-rate))
      (blocks-per-hour u144)
      (total-hours (/ duration-blocks blocks-per-hour))
      (total-cost (* total-hours effective-rate))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (factory-payment (- total-cost platform-fee))
      (factory (get factory machine-data))
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (get is-active machine-data) ERR-NOT-AUTHORIZED)
    (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
    (asserts! (>= duration-blocks u144) ERR-INVALID-DURATION)
    (asserts! (not (is-maintenance-scheduled machine-id)) ERR-MAINTENANCE-SCHEDULED)
    
    (match current-lease
      lease-info (asserts! (not (get is-active lease-info)) ERR-ALREADY-LEASED)
      true
    )
    
    (unwrap-panic (update-demand-metrics machine-id))
    
    (try! (stx-transfer? total-cost tx-sender (as-contract tx-sender)))
    
    (map-set machine-leases machine-id {
      lessee: tx-sender,
      start-block: stacks-block-height,
      end-block: end-block,
      total-cost: total-cost,
      is-active: true
    })
    
    (map-set factory-earnings factory 
      (+ (default-to u0 (map-get? factory-earnings factory)) factory-payment))
    
    (map-set platform-earnings CONTRACT-OWNER 
      (+ (default-to u0 (map-get? platform-earnings CONTRACT-OWNER)) platform-fee))
    
    (let ((usage-stats (unwrap! (map-get? machine-usage-stats machine-id) ERR-MACHINE-NOT-FOUND)))
      (map-set machine-usage-stats machine-id 
        (merge usage-stats {
          total-hours-used: (+ (get total-hours-used usage-stats) total-hours),
          total-leases: (+ (get total-leases usage-stats) u1),
          hours-since-service: (+ (get hours-since-service usage-stats) total-hours)
        })))
    
    (try! (check-for-maintenance-triggers machine-id))
    
    (ok {
      lease-start: stacks-block-height,
      lease-end: end-block,
      cost: total-cost
    })
  )
)

(define-public (end-lease (machine-id uint))
  (let 
    (
      (lease-data (unwrap! (map-get? machine-leases machine-id) ERR-NOT-LEASED))
      (lessee (get lessee lease-data))
    )
    (asserts! (or (is-eq tx-sender lessee) (is-eq tx-sender CONTRACT-OWNER)) ERR-NOT-AUTHORIZED)
    (asserts! (get is-active lease-data) ERR-NOT-LEASED)
    
    (map-set machine-leases machine-id 
      (merge lease-data { is-active: false }))
    
    (ok true)
  )
)

(define-public (deactivate-machine (machine-id uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (current-lease (map-get? machine-leases machine-id))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    
    (match current-lease
      lease-info (asserts! (not (get is-active lease-info)) ERR-LEASE-ACTIVE)
      true
    )
    
    (map-set machines machine-id 
      (merge machine-data { is-active: false }))
    
    (ok true)
  )
)

(define-public (reactivate-machine (machine-id uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    
    (map-set machines machine-id 
      (merge machine-data { is-active: true }))
    
    (ok true)
  )
)

(define-public (update-hourly-rate (machine-id uint) (new-rate uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (current-lease (map-get? machine-leases machine-id))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (> new-rate u0) ERR-INSUFFICIENT-PAYMENT)
    
    (match current-lease
      lease-info (asserts! (not (get is-active lease-info)) ERR-LEASE-ACTIVE)
      true
    )
    
    (map-set machines machine-id 
      (merge machine-data { hourly-rate: new-rate }))
    
    (ok true)
  )
)

(define-public (withdraw-earnings)
  (let 
    (
      (earnings (default-to u0 (map-get? factory-earnings tx-sender)))
    )
    (asserts! (> earnings u0) ERR-INSUFFICIENT-PAYMENT)
    
    (try! (as-contract (stx-transfer? earnings tx-sender tx-sender)))
    
    (map-delete factory-earnings tx-sender)
    
    (ok earnings)
  )
)

(define-public (withdraw-platform-fees)
  (let 
    (
      (earnings (default-to u0 (map-get? platform-earnings tx-sender)))
    )
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> earnings u0) ERR-INSUFFICIENT-PAYMENT)
    
    (try! (as-contract (stx-transfer? earnings tx-sender tx-sender)))
    
    (map-delete platform-earnings tx-sender)
    
    (ok earnings)
  )
)

(define-public (schedule-maintenance (machine-id uint) (start-block uint) (duration-blocks uint) (description (string-ascii 128)))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (current-lease (map-get? machine-leases machine-id))
      (end-block (+ start-block duration-blocks))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (> duration-blocks u0) ERR-INVALID-MAINTENANCE-PERIOD)
    (asserts! (> start-block stacks-block-height) ERR-INVALID-MAINTENANCE-PERIOD)
    (asserts! (> (len description) u0) ERR-INVALID-MAINTENANCE-PERIOD)
    
    (match current-lease
      lease-info (asserts! (or (not (get is-active lease-info)) (>= start-block (get end-block lease-info))) ERR-LEASE-ACTIVE)
      true
    )
    
    (map-set machine-maintenance machine-id {
      start-block: start-block,
      end-block: end-block,
      description: description,
      is-active: true
    })
    
    (ok {
      maintenance-start: start-block,
      maintenance-end: end-block,
      description: description
    })
  )
)

(define-public (cancel-maintenance (machine-id uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (maintenance-data (unwrap! (map-get? machine-maintenance machine-id) ERR-MAINTENANCE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (get is-active maintenance-data) ERR-MAINTENANCE-NOT-FOUND)
    (asserts! (> (get start-block maintenance-data) stacks-block-height) ERR-INVALID-MAINTENANCE-PERIOD)
    
    (map-set machine-maintenance machine-id 
      (merge maintenance-data { is-active: false }))
    
    (ok true)
  )
)

(define-public (complete-maintenance (machine-id uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (maintenance-data (unwrap! (map-get? machine-maintenance machine-id) ERR-MAINTENANCE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (get is-active maintenance-data) ERR-MAINTENANCE-NOT-FOUND)
    (asserts! (>= stacks-block-height (get start-block maintenance-data)) ERR-INVALID-MAINTENANCE-PERIOD)
    
    (map-set machine-maintenance machine-id 
      (merge maintenance-data { is-active: false }))
    
    (ok true)
  )
)

(define-public (update-platform-fee (new-rate uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (<= new-rate u1000) ERR-NOT-AUTHORIZED)
    
    (var-set platform-fee-rate new-rate)
    (ok true)
  )
)

(define-read-only (get-machine (machine-id uint))
  (map-get? machines machine-id)
)

(define-read-only (get-machine-lease (machine-id uint))
  (map-get? machine-leases machine-id)
)

(define-read-only (get-factory-earnings (factory principal))
  (default-to u0 (map-get? factory-earnings factory))
)

(define-read-only (get-platform-earnings)
  (default-to u0 (map-get? platform-earnings CONTRACT-OWNER))
)

(define-read-only (get-platform-fee-rate)
  (var-get platform-fee-rate)
)

(define-read-only (is-machine-available (machine-id uint))
  (match (map-get? machines machine-id)
    machine-data 
      (match (map-get? machine-leases machine-id)
        lease-data 
          (and 
            (get is-active machine-data)
            (not (get is-active lease-data))
            (not (is-maintenance-scheduled machine-id))
          )
        (and 
          (get is-active machine-data)
          (not (is-maintenance-scheduled machine-id))
        )
      )
    false
  )
)

(define-read-only (is-lease-expired (machine-id uint))
  (match (map-get? machine-leases machine-id)
    lease-data 
      (and 
        (get is-active lease-data)
        (>= stacks-block-height (get end-block lease-data))
      )
    false
  )
)

(define-read-only (get-lease-time-remaining (machine-id uint))
  (match (map-get? machine-leases machine-id)
    lease-data
      (if (and (get is-active lease-data) (< stacks-block-height (get end-block lease-data)))
        (some (- (get end-block lease-data) stacks-block-height))
        none
      )
    none
  )
)

(define-read-only (calculate-lease-cost (machine-id uint) (duration-blocks uint))
  (match (map-get? machines machine-id)
    machine-data
      (let 
        (
          (hourly-rate (get hourly-rate machine-data))
          (blocks-per-hour u144)
          (total-hours (/ duration-blocks blocks-per-hour))
          (total-cost (* total-hours hourly-rate))
          (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
        )
        (some {
          total-cost: total-cost,
          platform-fee: platform-fee,
          factory-payment: (- total-cost platform-fee)
        })
      )
    none
  )
)

(define-read-only (get-last-token-id) 
  (- (var-get next-machine-id) u1)
)

(define-read-only (get-token-uri (id uint)) 
  (ok none)
)

(define-read-only (get-owner (id uint)) 
  (ok (nft-get-owner? machine id))
)

(define-read-only (get-machine-maintenance (machine-id uint))
  (map-get? machine-maintenance machine-id)
)

(define-read-only (is-maintenance-scheduled (machine-id uint))
  (match (map-get? machine-maintenance machine-id)
    maintenance-data
      (and 
        (get is-active maintenance-data)
        (or 
          (and 
            (> (get start-block maintenance-data) stacks-block-height)
            (< stacks-block-height (get end-block maintenance-data))
          )
          (and 
            (<= (get start-block maintenance-data) stacks-block-height)
            (< stacks-block-height (get end-block maintenance-data))
          )
        )
      )
    false
  )
)

(define-read-only (is-maintenance-active (machine-id uint))
  (match (map-get? machine-maintenance machine-id)
    maintenance-data
      (and 
        (get is-active maintenance-data)
        (<= (get start-block maintenance-data) stacks-block-height)
        (< stacks-block-height (get end-block maintenance-data))
      )
    false
  )
)

(define-read-only (get-maintenance-time-remaining (machine-id uint))
  (match (map-get? machine-maintenance machine-id)
    maintenance-data
      (if (and (get is-active maintenance-data) (< stacks-block-height (get end-block maintenance-data)))
        (some (- (get end-block maintenance-data) stacks-block-height))
        none
      )
    none
  )
)

(define-private (check-for-maintenance-triggers (machine-id uint))
  (let 
    (
      (usage-stats (unwrap! (map-get? machine-usage-stats machine-id) ERR-MACHINE-NOT-FOUND))
      (kpis (unwrap! (map-get? machine-kpis machine-id) ERR-MACHINE-NOT-FOUND))
    )
    (if (>= (get hours-since-service usage-stats) (var-get maintenance-threshold-hours))
      (begin
        (try! (schedule-maintenance 
          machine-id 
          (get predicted-next-service kpis) 
          u144 
          "Automated usage-based maintenance"))
        (ok true))
      (ok false)
    )
  )
)

(define-public (update-machine-kpis (machine-id uint) (avg-utilization uint) (peak-performance uint) (failure-count uint) (health-status (string-ascii 20)))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (kpis (unwrap! (map-get? machine-kpis machine-id) ERR-MACHINE-NOT-FOUND))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (<= avg-utilization u100) ERR-INVALID-KPI)
    (asserts! (<= peak-performance u100) ERR-INVALID-KPI)
    (asserts! (> (len health-status) u0) ERR-INVALID-KPI)
    
    (map-set machine-kpis machine-id 
      (merge kpis {
        avg-utilization: avg-utilization,
        peak-performance: peak-performance,
        failure-count: failure-count,
        health-status: health-status
      }))
    
    (ok true)
  )
)

(define-public (request-manual-service (machine-id uint) (request-type (string-ascii 50)) (priority uint) (duration uint) (cost uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (request-id (+ (var-get service-request-counter) u1))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (> (len request-type) u0) ERR-INVALID-KPI)
    
    (map-set service-requests request-id {
      machine-id: machine-id,
      request-type: request-type,
      priority: priority,
      requested-at: stacks-block-height,
      completed-at: none,
      service-provider: none,
      estimated-duration: duration,
      cost-estimate: cost,
      is-automated: false
    })
    
    (var-set service-request-counter request-id)
    (ok request-id)
  )
)

(define-public (complete-service-request (request-id uint) (service-provider principal))
  (let 
    (
      (service-req (unwrap! (map-get? service-requests request-id) ERR-MAINTENANCE-NOT-FOUND))
      (machine-id (get machine-id service-req))
      (usage-stats (unwrap! (map-get? machine-usage-stats machine-id) ERR-MACHINE-NOT-FOUND))
    )
    (asserts! (is-none (get completed-at service-req)) ERR-SERVICE-ALREADY-COMPLETED)
    
    (map-set service-requests request-id 
      (merge service-req {
        completed-at: (some stacks-block-height),
        service-provider: (some service-provider)
      }))
    
    (map-set machine-usage-stats machine-id 
      (merge usage-stats {
        last-service-block: stacks-block-height,
        hours-since-service: u0
      }))
    
    (map-set maintenance-history { machine-id: machine-id, service-block: stacks-block-height } {
      service-type: (get request-type service-req),
      duration: (get estimated-duration service-req),
      cost: (get cost-estimate service-req),
      performance-impact: 10,
      next-recommended: (+ stacks-block-height (var-get maintenance-threshold-hours))
    })
    
    (ok true)
  )
)

(define-public (set-maintenance-threshold (new-threshold uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
    (asserts! (> new-threshold u0) ERR-INVALID-DURATION)
    
    (var-set maintenance-threshold-hours new-threshold)
    (ok true)
  )
)

(define-read-only (get-machine-usage-stats (machine-id uint))
  (map-get? machine-usage-stats machine-id)
)

(define-read-only (get-machine-kpis (machine-id uint))
  (map-get? machine-kpis machine-id)
)

(define-read-only (get-service-request (request-id uint))
  (map-get? service-requests request-id)
)

(define-read-only (get-maintenance-history (machine-id uint) (service-block uint))
  (map-get? maintenance-history { machine-id: machine-id, service-block: service-block })
)

(define-read-only (get-predicted-maintenance-status (machine-id uint))
  (let 
    (
      (usage-stats (map-get? machine-usage-stats machine-id))
      (kpis (map-get? machine-kpis machine-id))
    )
    (if (and (is-some usage-stats) (is-some kpis))
      (some {
        hours-since-service: (get hours-since-service (unwrap-panic usage-stats)),
        predicted-next-service: (get predicted-next-service (unwrap-panic kpis)),
        health-status: (get health-status (unwrap-panic kpis)),
        maintenance-score: (get maintenance-score (unwrap-panic usage-stats))
      })
      none
    )
  )
)

(define-read-only (get-maintenance-threshold)
  (var-get maintenance-threshold-hours)
)

(define-public (enable-dynamic-pricing 
  (machine-id uint) 
  (demand-multiplier uint) 
  (utilization-discount uint) 
  (peak-hours-multiplier uint)
  (performance-bonus uint)
  (min-rate uint)
  (max-rate uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (base-rate (get hourly-rate machine-data))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (<= demand-multiplier u300) ERR-INVALID-PRICING-PARAMS)
    (asserts! (<= utilization-discount u50) ERR-INVALID-PRICING-PARAMS)
    (asserts! (<= peak-hours-multiplier u200) ERR-INVALID-PRICING-PARAMS)
    (asserts! (<= performance-bonus u50) ERR-INVALID-PRICING-PARAMS)
    (asserts! (> min-rate u0) ERR-INVALID-PRICING-PARAMS)
    (asserts! (> max-rate min-rate) ERR-INVALID-PRICING-PARAMS)
    
    (map-set dynamic-pricing-config machine-id {
      base-rate: base-rate,
      demand-multiplier: demand-multiplier,
      utilization-discount: utilization-discount,
      peak-hours-multiplier: peak-hours-multiplier,
      performance-bonus: performance-bonus,
      enabled: true,
      min-rate: min-rate,
      max-rate: max-rate
    })
    
    (map-set machine-demand-metrics machine-id {
      lease-requests-count: u0,
      recent-block-height: stacks-block-height,
      demand-score: u100
    })
    
    (ok true)
  )
)

(define-public (disable-dynamic-pricing (machine-id uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (pricing-config (unwrap! (map-get? dynamic-pricing-config machine-id) ERR-PRICING-DISABLED))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    
    (map-set dynamic-pricing-config machine-id 
      (merge pricing-config { enabled: false }))
    
    (ok true)
  )
)

(define-public (update-pricing-parameters
  (machine-id uint)
  (demand-multiplier uint)
  (utilization-discount uint)
  (peak-hours-multiplier uint)
  (performance-bonus uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (pricing-config (unwrap! (map-get? dynamic-pricing-config machine-id) ERR-PRICING-DISABLED))
    )
    (asserts! (is-eq tx-sender (get factory machine-data)) ERR-NOT-OWNER)
    (asserts! (<= demand-multiplier u300) ERR-INVALID-PRICING-PARAMS)
    (asserts! (<= utilization-discount u50) ERR-INVALID-PRICING-PARAMS)
    (asserts! (<= peak-hours-multiplier u200) ERR-INVALID-PRICING-PARAMS)
    (asserts! (<= performance-bonus u50) ERR-INVALID-PRICING-PARAMS)
    
    (map-set dynamic-pricing-config machine-id 
      (merge pricing-config {
        demand-multiplier: demand-multiplier,
        utilization-discount: utilization-discount,
        peak-hours-multiplier: peak-hours-multiplier,
        performance-bonus: performance-bonus
      }))
    
    (ok true)
  )
)

(define-private (update-demand-metrics (machine-id uint))
  (let 
    (
      (current-metrics (default-to 
        { lease-requests-count: u0, recent-block-height: stacks-block-height, demand-score: u100 }
        (map-get? machine-demand-metrics machine-id)))
      (blocks-since-last (- stacks-block-height (get recent-block-height current-metrics)))
      (new-request-count (+ (get lease-requests-count current-metrics) u1))
      (demand-score (calculate-demand-score new-request-count blocks-since-last))
    )
    (map-set machine-demand-metrics machine-id {
      lease-requests-count: new-request-count,
      recent-block-height: stacks-block-height,
      demand-score: demand-score
    })
    
    (ok true)
  )
)

(define-private (calculate-demand-score (request-count uint) (blocks-elapsed uint))
  (let 
    (
      (blocks-per-day u144)
      (requests-per-day (if (> blocks-elapsed u0)
        (/ (* request-count blocks-per-day) blocks-elapsed)
        request-count))
    )
    (if (> requests-per-day u10)
      u200
      (if (> requests-per-day u5)
        u150
        (if (> requests-per-day u2)
          u120
          u100
        )
      )
    )
  )
)

(define-private (get-effective-pricing (machine-id uint) (base-rate uint))
  (match (map-get? dynamic-pricing-config machine-id)
    pricing-config
      (if (get enabled pricing-config)
        (let 
          (
            (usage-stats (unwrap-panic (map-get? machine-usage-stats machine-id)))
            (kpis (unwrap-panic (map-get? machine-kpis machine-id)))
            (demand-metrics (unwrap-panic (map-get? machine-demand-metrics machine-id)))
            (utilization (get avg-utilization kpis))
            (performance (get peak-performance kpis))
            (demand-score (get demand-score demand-metrics))
            (block-mod (mod stacks-block-height u144))
            (is-peak-hour (or (< block-mod u36) (> block-mod u108)))
            
            (demand-adjustment (/ (* base-rate (- demand-score u100) (get demand-multiplier pricing-config)) u10000))
            (utilization-adjustment (if (< utilization u30)
              (- u0 (/ (* base-rate (get utilization-discount pricing-config)) u100))
              u0))
            (peak-adjustment (if is-peak-hour
              (/ (* base-rate (- (get peak-hours-multiplier pricing-config) u100)) u100)
              u0))
            (performance-adjustment (if (>= performance u80)
              (/ (* base-rate (get performance-bonus pricing-config)) u100)
              u0))
            
            (adjusted-rate (+ base-rate (+ demand-adjustment (+ utilization-adjustment (+ peak-adjustment performance-adjustment)))))
            (min-rate (get min-rate pricing-config))
            (max-rate (get max-rate pricing-config))
          )
          (if (< adjusted-rate min-rate)
            min-rate
            (if (> adjusted-rate max-rate)
              max-rate
              adjusted-rate
            )
          )
        )
        base-rate
      )
    base-rate
  )
)

(define-read-only (get-dynamic-pricing-config (machine-id uint))
  (map-get? dynamic-pricing-config machine-id)
)

(define-read-only (get-demand-metrics (machine-id uint))
  (map-get? machine-demand-metrics machine-id)
)

(define-read-only (calculate-current-rate (machine-id uint))
  (match (map-get? machines machine-id)
    machine-data
      (some (get-effective-pricing machine-id (get hourly-rate machine-data)))
    none
  )
)

(define-read-only (get-pricing-breakdown (machine-id uint))
  (match (map-get? dynamic-pricing-config machine-id)
    pricing-config
      (if (get enabled pricing-config)
        (let 
          (
            (machine-data (unwrap-panic (map-get? machines machine-id)))
            (base-rate (get hourly-rate machine-data))
            (effective-rate (get-effective-pricing machine-id base-rate))
          )
          (some {
            base-rate: base-rate,
            effective-rate: effective-rate,
            dynamic-pricing-enabled: true,
            savings-or-premium: (if (> effective-rate base-rate)
              (- effective-rate base-rate)
              (- base-rate effective-rate)
            )
          })
        )
        none
      )
    none
  )
)

(define-public (rate-machine (machine-id uint) (rating uint) (feedback-type (string-ascii 20)))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (lease-data (unwrap! (map-get? machine-leases machine-id) ERR-NOT-LEASED))
      (reputation (unwrap! (map-get? machine-reputation machine-id) ERR-MACHINE-NOT-FOUND))
      (lease-end-block (get end-block lease-data))
      (rating-key { machine-id: machine-id, lessee: tx-sender, lease-end-block: lease-end-block })
    )
    (asserts! (is-eq tx-sender (get lessee lease-data)) ERR-NOT-LESSEE)
    (asserts! (not (get is-active lease-data)) ERR-LEASE-NOT-ENDED)
    (asserts! (and (>= rating u1) (<= rating u5)) ERR-INVALID-RATING)
    (asserts! (> (len feedback-type) u0) ERR-INVALID-RATING)
    (asserts! (is-none (map-get? lease-ratings rating-key)) ERR-ALREADY-RATED)
    
    (map-set lease-ratings rating-key {
      rating: rating,
      feedback-type: feedback-type,
      rated-at: stacks-block-height
    })
    
    (let 
      (
        (new-total-ratings (+ (get total-ratings reputation) u1))
        (new-rating-sum (+ (get rating-sum reputation) rating))
        (new-avg-rating (/ new-rating-sum new-total-ratings))
        (new-five-star (if (is-eq rating u5) (+ (get five-star-count reputation) u1) (get five-star-count reputation)))
        (new-one-star (if (is-eq rating u1) (+ (get one-star-count reputation) u1) (get one-star-count reputation)))
      )
      (map-set machine-reputation machine-id {
        total-ratings: new-total-ratings,
        rating-sum: new-rating-sum,
        avg-rating: new-avg-rating,
        five-star-count: new-five-star,
        one-star-count: new-one-star
      })
    )
    
    (ok true)
  )
)

(define-read-only (get-machine-reputation (machine-id uint))
  (map-get? machine-reputation machine-id)
)

(define-read-only (get-lease-rating (machine-id uint) (lessee principal) (lease-end-block uint))
  (map-get? lease-ratings { machine-id: machine-id, lessee: lessee, lease-end-block: lease-end-block })
)

(define-read-only (is-highly-rated (machine-id uint))
  (match (map-get? machine-reputation machine-id)
    rep
      (and (>= (get total-ratings rep) u5) (>= (get avg-rating rep) u4))
    false
  )
)
