(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-expired (err u105))
(define-constant err-not-expired (err u106))
(define-constant err-empty-bundle (err u107))
(define-constant err-invalid-price (err u108))
(define-data-var next-carbon-credit-id uint u1)
(define-data-var temp-buyer (optional principal) none)

(define-map carbon-credits
    uint
    {
        issuer: principal,
        amount: uint,
        price: uint,
        issued-date: uint,
        validity-period: uint,
        buyer: (optional principal),
        transferred: bool,
    }
)

(define-map credit-sellers
    principal
    {
        name: (string-ascii 64),
        verified: bool,
    }
)

(define-map buyer-credits
    principal
    (list 100 uint)
)

(define-read-only (get-carbon-credit (credit-id uint))
    (match (map-get? carbon-credits credit-id)
        credit (ok credit)
        err-not-found
    )
)

(define-public (register-seller (name (string-ascii 64)))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? credit-sellers tx-sender))
            err-already-exists
        )
        (ok (map-set credit-sellers tx-sender {
            name: name,
            verified: true,
        }))
    )
)

(define-public (issue-carbon-credit
        (amount uint)
        (price uint)
        (validity-period uint)
    )
    (let ((credit-id (var-get next-carbon-credit-id)))
        (asserts! (is-some (map-get? credit-sellers tx-sender)) err-unauthorized)
        (map-set carbon-credits credit-id {
            issuer: tx-sender,
            amount: amount,
            price: price,
            issued-date: stacks-block-height,
            validity-period: validity-period,
            buyer: none,
            transferred: false,
        })
        (var-set next-carbon-credit-id (+ credit-id u1))
        (ok credit-id)
    )
)
(define-public (buy-carbon-credit (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) err-not-found)))
        (asserts! (is-none (get buyer credit)) err-already-exists)
        (asserts! (>= (stx-get-balance tx-sender) (get price credit))
            err-insufficient-funds
        )
        (ok (begin
            (map-set carbon-credits credit-id
                (merge credit {
                    buyer: (some tx-sender),
                    transferred: true,
                })
            )
            credit-id
        ))
    )
)

(define-public (revoke-carbon-credit (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) err-not-found)))
        (asserts! (is-eq (get issuer credit) tx-sender) err-unauthorized)
        (ok (map-set carbon-credits credit-id
            (merge credit {
                transferred: false,
                buyer: none,
            })
        ))
    )
)

(define-read-only (get-buyer-credits (buyer principal))
    (ok (default-to (list) (map-get? buyer-credits buyer)))
)
(define-read-only (is-credit-expired (credit-id uint))
    (match (map-get? carbon-credits credit-id)
        credit (let ((expiry-block (+ (get issued-date credit) (get validity-period credit))))
            (>= stacks-block-height expiry-block)
        )
        true
    )
)

(define-read-only (get-active-credits-count)
    (let ((current-id (var-get next-carbon-credit-id)))
        (fold count-active-credits (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10) u0)
    )
)

(define-private (count-active-credits
        (credit-id uint)
        (count uint)
    )
    (if (and
            (is-some (map-get? carbon-credits credit-id))
            (not (is-credit-expired credit-id))
        )
        (+ count u1)
        count
    )
)

(define-public (cleanup-expired-credit (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) err-not-found)))
        (asserts! (is-credit-expired credit-id) (err u105))
        (asserts!
            (or
                (is-eq (get issuer credit) tx-sender)
                (is-eq tx-sender contract-owner)
            )
            err-unauthorized
        )
        (ok (map-delete carbon-credits credit-id))
    )
)

(define-public (buy-carbon-credit-with-expiry-check (credit-id uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) err-not-found)))
        (asserts! (not (is-credit-expired credit-id)) (err u106))
        (asserts! (is-none (get buyer credit)) err-already-exists)
        (asserts! (>= (stx-get-balance tx-sender) (get price credit))
            err-insufficient-funds
        )
        (ok (begin
            (map-set carbon-credits credit-id
                (merge credit {
                    buyer: (some tx-sender),
                    transferred: true,
                })
            )
            credit-id
        ))
    )
)
(define-data-var next-bundle-id uint u1)

(define-map credit-bundles
    uint
    {
        creator: principal,
        credit-ids: (list 20 uint),
        total-amount: uint,
        bundle-price: uint,
        created-date: uint,
        buyer: (optional principal),
        sold: bool,
    }
)

(define-map bundle-ownership
    principal
    (list 50 uint)
)

(define-private (calculate-bundle-amount
        (credit-id uint)
        (total uint)
    )
    (match (map-get? carbon-credits credit-id)
        credit (+ total (get amount credit))
        total
    )
)

(define-private (validate-credit-ownership
        (credit-id uint)
        (owner principal)
    )
    (match (map-get? carbon-credits credit-id)
        credit (and
            (is-eq (get issuer credit) owner)
            (is-none (get buyer credit))
            (not (get transferred credit))
        )
        false
    )
)

(define-private (check-all-credits-owned
        (credit-ids (list 20 uint))
        (owner principal)
    )
    (fold validate-ownership-fold credit-ids true)
)

(define-private (validate-ownership-fold
        (credit-id uint)
        (valid bool)
    )
    (and valid (validate-credit-ownership credit-id tx-sender))
)

(define-public (create-credit-bundle
        (credit-ids (list 20 uint))
        (bundle-price uint)
    )
    (let (
            (bundle-id (var-get next-bundle-id))
            (total-amount (fold calculate-bundle-amount credit-ids u0))
        )
        (asserts! (> (len credit-ids) u0) (err u107))
        (asserts! (check-all-credits-owned credit-ids tx-sender) err-unauthorized)
        (asserts! (> bundle-price u0) (err u108))
        (map-set credit-bundles bundle-id {
            creator: tx-sender,
            credit-ids: credit-ids,
            total-amount: total-amount,
            bundle-price: bundle-price,
            created-date: stacks-block-height,
            buyer: none,
            sold: false,
        })
        (var-set next-bundle-id (+ bundle-id u1))
        (ok bundle-id)
    )
)

(define-public (buy-credit-bundle (bundle-id uint))
    (let ((bundle (unwrap! (map-get? credit-bundles bundle-id) err-not-found)))
        (asserts! (not (get sold bundle)) err-already-exists)
        (asserts! (>= (stx-get-balance tx-sender) (get bundle-price bundle))
            err-insufficient-funds
        )
        (asserts! (not (is-eq (get creator bundle) tx-sender)) err-unauthorized)
        (ok (begin
            (map-set credit-bundles bundle-id
                (merge bundle {
                    buyer: (some tx-sender),
                    sold: true,
                })
            )
            (update-bundle-credits (get credit-ids bundle) tx-sender)
            bundle-id
        ))
    )
)

(define-private (update-bundle-credits
        (credit-ids (list 20 uint))
        (new-buyer principal)
    )
    (begin
        (var-set temp-buyer (some new-buyer))
        (map update-single-credit-buyer credit-ids)
        (var-set temp-buyer none)
        true
    )
)

(define-private (update-single-credit-buyer (credit-id uint))
    (match (map-get? carbon-credits credit-id)
        credit (map-set carbon-credits credit-id
            (merge credit {
                buyer: (var-get temp-buyer),
                transferred: true,
            })
        )
        false
    )
)

(define-private (update-credit-buyer
        (credit-id uint)
        (success bool)
        (new-buyer principal)
    )
    (match (map-get? carbon-credits credit-id)
        credit (begin
            (map-set carbon-credits credit-id
                (merge credit {
                    buyer: (some new-buyer),
                    transferred: true,
                })
            )
            success
        )
        success
    )
)

(define-read-only (get-credit-bundle (bundle-id uint))
    (match (map-get? credit-bundles bundle-id)
        bundle (ok bundle)
        err-not-found
    )
)

(define-read-only (get-user-bundles (user principal))
    (ok (default-to (list) (map-get? bundle-ownership user)))
)

(define-data-var next-auction-id uint u1)

(define-map credit-auctions
    uint
    {
        credit-id: uint,
        seller: principal,
        starting-price: uint,
        current-bid: uint,
        highest-bidder: (optional principal),
        start-block: uint,
        end-block: uint,
        active: bool,
        settled: bool,
    }
)

(define-map auction-bids
    {
        auction-id: uint,
        bidder: principal,
    }
    {
        amount: uint,
        block-height: uint,
    }
)

(define-map user-auction-history
    principal
    (list 50 uint)
)

(define-public (create-auction
        (credit-id uint)
        (starting-price uint)
        (duration uint)
    )
    (let (
            (auction-id (var-get next-auction-id))
            (credit (unwrap! (map-get? carbon-credits credit-id) err-not-found))
        )
        (asserts! (is-eq (get issuer credit) tx-sender) err-unauthorized)
        (asserts! (is-none (get buyer credit)) err-already-exists)
        (asserts! (not (get transferred credit)) err-already-exists)
        (asserts! (> starting-price u0) err-invalid-price)
        (asserts! (> duration u0) err-invalid-price)
        (map-set credit-auctions auction-id {
            credit-id: credit-id,
            seller: tx-sender,
            starting-price: starting-price,
            current-bid: starting-price,
            highest-bidder: none,
            start-block: stacks-block-height,
            end-block: (+ stacks-block-height duration),
            active: true,
            settled: false,
        })
        (var-set next-auction-id (+ auction-id u1))
        (ok auction-id)
    )
)

(define-public (place-bid
        (auction-id uint)
        (bid-amount uint)
    )
    (let ((auction (unwrap! (map-get? credit-auctions auction-id) err-not-found)))
        (asserts! (get active auction) err-expired)
        (asserts! (< stacks-block-height (get end-block auction)) err-expired)
        (asserts! (> bid-amount (get current-bid auction)) err-invalid-price)
        (asserts! (>= (stx-get-balance tx-sender) bid-amount)
            err-insufficient-funds
        )
        (asserts! (not (is-eq tx-sender (get seller auction))) err-unauthorized)
        (map-set auction-bids {
            auction-id: auction-id,
            bidder: tx-sender,
        } {
            amount: bid-amount,
            block-height: stacks-block-height,
        })
        (map-set credit-auctions auction-id
            (merge auction {
                current-bid: bid-amount,
                highest-bidder: (some tx-sender),
            })
        )
        (ok true)
    )
)

(define-public (settle-auction (auction-id uint))
    (let ((auction (unwrap! (map-get? credit-auctions auction-id) err-not-found)))
        (asserts! (get active auction) err-already-exists)
        (asserts! (>= stacks-block-height (get end-block auction))
            err-not-expired
        )
        (asserts! (not (get settled auction)) err-already-exists)
        (match (get highest-bidder auction)
            winner (begin
                (let ((credit (unwrap! (map-get? carbon-credits (get credit-id auction))
                        err-not-found
                    )))
                    (map-set carbon-credits (get credit-id auction)
                        (merge credit {
                            buyer: (some winner),
                            transferred: true,
                        })
                    )
                )
                (map-set credit-auctions auction-id
                    (merge auction {
                        active: false,
                        settled: true,
                    })
                )
                (ok winner)
            )
            (begin
                (map-set credit-auctions auction-id
                    (merge auction {
                        active: false,
                        settled: true,
                    })
                )
                (ok tx-sender)
            )
        )
    )
)

(define-read-only (get-auction (auction-id uint))
    (match (map-get? credit-auctions auction-id)
        auction (ok auction)
        err-not-found
    )
)

(define-read-only (get-user-bid
        (auction-id uint)
        (bidder principal)
    )
    (match (map-get? auction-bids {
        auction-id: auction-id,
        bidder: bidder,
    })
        bid (ok bid)
        err-not-found
    )
)

(define-read-only (is-auction-active (auction-id uint))
    (match (map-get? credit-auctions auction-id)
        auction (and
            (get active auction)
            (< stacks-block-height (get end-block auction))
        )
        false
    )
)

(define-data-var next-rating-id uint u1)

(define-map credit-ratings
    uint
    {
        credit-id: uint,
        assessor: principal,
        quality-score: uint,
        environmental-impact: uint,
        verification-level: uint,
        rating-date: uint,
        comments: (string-ascii 256),
    }
)

(define-map verified-assessors
    principal
    {
        name: (string-ascii 64),
        certification-level: uint,
        total-assessments: uint,
        verified-date: uint,
        active: bool,
    }
)

(define-map credit-rating-summary
    uint
    {
        total-ratings: uint,
        average-quality: uint,
        average-impact: uint,
        average-verification: uint,
        last-updated: uint,
    }
)

(define-map user-reputation
    principal
    {
        total-transactions: uint,
        successful-sales: uint,
        successful-purchases: uint,
        average-rating-given: uint,
        reputation-score: uint,
        last-updated: uint,
    }
)

(define-public (register-assessor
        (assessor principal)
        (name (string-ascii 64))
        (certification-level uint)
    )
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (is-none (map-get? verified-assessors assessor))
            err-already-exists
        )
        (asserts! (<= certification-level u5) err-invalid-price)
        (asserts! (> certification-level u0) err-invalid-price)
        (map-set verified-assessors assessor {
            name: name,
            certification-level: certification-level,
            total-assessments: u0,
            verified-date: stacks-block-height,
            active: true,
        })
        (ok true)
    )
)

(define-public (rate-carbon-credit
        (credit-id uint)
        (quality-score uint)
        (environmental-impact uint)
        (verification-level uint)
        (comments (string-ascii 256))
    )
    (let (
            (rating-id (var-get next-rating-id))
            (assessor-info (unwrap! (map-get? verified-assessors tx-sender) err-unauthorized))
            (credit (unwrap! (map-get? carbon-credits credit-id) err-not-found))
        )
        (asserts! (get active assessor-info) err-unauthorized)
        (asserts! (<= quality-score u100) err-invalid-price)
        (asserts! (<= environmental-impact u100) err-invalid-price)
        (asserts! (<= verification-level u5) err-invalid-price)
        (asserts! (> quality-score u0) err-invalid-price)
        (asserts! (> environmental-impact u0) err-invalid-price)
        (asserts! (> verification-level u0) err-invalid-price)
        (map-set credit-ratings rating-id {
            credit-id: credit-id,
            assessor: tx-sender,
            quality-score: quality-score,
            environmental-impact: environmental-impact,
            verification-level: verification-level,
            rating-date: stacks-block-height,
            comments: comments,
        })
        (map-set verified-assessors tx-sender
            (merge assessor-info { total-assessments: (+ (get total-assessments assessor-info) u1) })
        )
        (var-set next-rating-id (+ rating-id u1))
        (update-credit-rating-summary credit-id)
        (ok rating-id)
    )
)

(define-private (update-credit-rating-summary (credit-id uint))
    (let (
            (current-summary (default-to {
                total-ratings: u0,
                average-quality: u0,
                average-impact: u0,
                average-verification: u0,
                last-updated: u0,
            }
                (map-get? credit-rating-summary credit-id)
            ))
            (new-total (+ (get total-ratings current-summary) u1))
        )
        (map-set credit-rating-summary credit-id {
            total-ratings: new-total,
            average-quality: u0,
            average-impact: u0,
            average-verification: u0,
            last-updated: stacks-block-height,
        })
        true
    )
)

(define-public (update-user-reputation
        (user principal)
        (transaction-type uint)
    )
    (let ((current-rep (default-to {
            total-transactions: u0,
            successful-sales: u0,
            successful-purchases: u0,
            average-rating-given: u0,
            reputation-score: u0,
            last-updated: u0,
        }
            (map-get? user-reputation user)
        )))
        (map-set user-reputation user
            (merge current-rep {
                total-transactions: (+ (get total-transactions current-rep) u1),
                successful-sales: (if (is-eq transaction-type u1)
                    (+ (get successful-sales current-rep) u1)
                    (get successful-sales current-rep)
                ),
                successful-purchases: (if (is-eq transaction-type u2)
                    (+ (get successful-purchases current-rep) u1)
                    (get successful-purchases current-rep)
                ),
                reputation-score: (calculate-reputation-score
                    (+ (get total-transactions current-rep) u1)
                    (if (is-eq transaction-type u1)
                        (+ (get successful-sales current-rep) u1)
                        (get successful-sales current-rep)
                    )
                    (if (is-eq transaction-type u2)
                        (+ (get successful-purchases current-rep) u1)
                        (get successful-purchases current-rep)
                    )),
                last-updated: stacks-block-height,
            })
        )
        (ok true)
    )
)

(define-private (calculate-reputation-score
        (total uint)
        (sales uint)
        (purchases uint)
    )
    (if (> total u0)
        (/ (* (+ sales purchases) u100) total)
        u0
    )
)

(define-read-only (get-credit-rating (rating-id uint))
    (match (map-get? credit-ratings rating-id)
        rating (ok rating)
        err-not-found
    )
)

(define-read-only (get-credit-rating-summary (credit-id uint))
    (match (map-get? credit-rating-summary credit-id)
        summary (ok summary)
        err-not-found
    )
)

(define-read-only (get-assessor-info (assessor principal))
    (match (map-get? verified-assessors assessor)
        info (ok info)
        err-not-found
    )
)

(define-read-only (get-user-reputation (user principal))
    (match (map-get? user-reputation user)
        reputation (ok reputation)
        err-not-found
    )
)

(define-read-only (is-high-quality-credit (credit-id uint))
    (match (map-get? credit-rating-summary credit-id)
        summary (and
            (> (get total-ratings summary) u0)
            (>= (get average-quality summary) u75)
            (>= (get average-verification summary) u3)
        )
        false
    )
)
