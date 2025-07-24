
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

(define-data-var next-machine-id uint u1)
(define-data-var platform-fee-rate uint u250)

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
    
    (var-set next-machine-id (+ machine-id u1))
    (ok machine-id)
  )
)

(define-public (lease-machine (machine-id uint) (duration-blocks uint))
  (let 
    (
      (machine-data (unwrap! (map-get? machines machine-id) ERR-MACHINE-NOT-FOUND))
      (current-lease (map-get? machine-leases machine-id))
      (hourly-rate (get hourly-rate machine-data))
      (blocks-per-hour u144)
      (total-hours (/ duration-blocks blocks-per-hour))
      (total-cost (* total-hours hourly-rate))
      (platform-fee (/ (* total-cost (var-get platform-fee-rate)) u10000))
      (factory-payment (- total-cost platform-fee))
      (factory (get factory machine-data))
      (end-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (get is-active machine-data) ERR-NOT-AUTHORIZED)
    (asserts! (> duration-blocks u0) ERR-INVALID-DURATION)
    (asserts! (>= duration-blocks u144) ERR-INVALID-DURATION)
    
    (match current-lease
      lease-info (asserts! (not (get is-active lease-info)) ERR-ALREADY-LEASED)
      true
    )
    
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
          )
        (get is-active machine-data)
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
