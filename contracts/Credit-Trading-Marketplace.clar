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
(define-constant err-already-retired (err u109))
(define-constant err-not-owner (err u110))
(define-constant err-escrow-not-found (err u111))
(define-constant err-escrow-already-settled (err u112))
(define-constant err-escrow-not-expired (err u113))
(define-constant err-wrong-buyer (err u114))
(define-constant err-wrong-seller (err u115))
(define-data-var next-carbon-credit-id uint u1)
(define-data-var next-bundle-id uint u1)
(define-data-var next-auction-id uint u1)
(define-data-var next-rating-id uint u1)
(define-data-var next-retirement-id uint u1)
(define-data-var next-escrow-id uint u1)
(define-data-var temp-buyer (optional principal) none)
(define-data-var temp-purpose (string-ascii 128) "")
(define-data-var temp-certificate (optional (string-ascii 64)) none)

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
        retired: bool,
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

(define-map credit-retirements
    uint
    {
        credit-id: uint,
        retiree: principal,
        retirement-date: uint,
        purpose: (string-ascii 128),
        co2-amount: uint,
        certificate-hash: (optional (string-ascii 64)),
    }
)

(define-map user-retirement-summary
    principal
    {
        total-retired-credits: uint,
        total-co2-offset: uint,
        first-retirement: uint,
        last-retirement: uint,
    }
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
            retired: false,
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

(define-public (retire-carbon-credit
        (credit-id uint)
        (purpose (string-ascii 128))
        (certificate-hash (optional (string-ascii 64)))
    )
    (let (
            (credit (unwrap! (map-get? carbon-credits credit-id) err-not-found))
            (retirement-id (var-get next-retirement-id))
        )
        (asserts! (is-some (get buyer credit)) err-not-owner)
        (asserts! (is-eq (unwrap-panic (get buyer credit)) tx-sender)
            err-not-owner
        )
        (asserts! (not (get retired credit)) err-already-retired)
        (asserts! (not (is-credit-expired credit-id)) err-expired)
        (map-set carbon-credits credit-id (merge credit { retired: true }))
        (map-set credit-retirements retirement-id {
            credit-id: credit-id,
            retiree: tx-sender,
            retirement-date: stacks-block-height,
            purpose: purpose,
            co2-amount: (get amount credit),
            certificate-hash: certificate-hash,
        })
        (update-retirement-summary tx-sender (get amount credit))
        (var-set next-retirement-id (+ retirement-id u1))
        (ok retirement-id)
    )
)

(define-private (retire-single-credit (credit-id uint))
    (retire-carbon-credit credit-id (var-get temp-purpose)
        (var-get temp-certificate)
    )
)

(define-public (batch-retire-credits
        (credit-ids (list 10 uint))
        (purpose (string-ascii 128))
        (certificate-hash (optional (string-ascii 64)))
    )
    (begin
        (var-set temp-purpose purpose)
        (var-set temp-certificate certificate-hash)
        (map retire-single-credit credit-ids)
        (ok true)
    )
)

(define-private (update-retirement-summary
        (user principal)
        (co2-amount uint)
    )
    (let ((current-summary (default-to {
            total-retired-credits: u0,
            total-co2-offset: u0,
            first-retirement: stacks-block-height,
            last-retirement: stacks-block-height,
        }
            (map-get? user-retirement-summary user)
        )))
        (map-set user-retirement-summary user {
            total-retired-credits: (+ (get total-retired-credits current-summary) u1),
            total-co2-offset: (+ (get total-co2-offset current-summary) co2-amount),
            first-retirement: (if (> (get total-retired-credits current-summary) u0)
                (get first-retirement current-summary)
                stacks-block-height
            ),
            last-retirement: stacks-block-height,
        })
        true
    )
)

(define-read-only (get-retirement-record (retirement-id uint))
    (match (map-get? credit-retirements retirement-id)
        retirement (ok retirement)
        err-not-found
    )
)

(define-read-only (get-user-retirement-summary (user principal))
    (match (map-get? user-retirement-summary user)
        summary (ok summary)
        err-not-found
    )
)

(define-read-only (is-credit-retired (credit-id uint))
    (match (map-get? carbon-credits credit-id)
        credit (get retired credit)
        false
    )
)

(define-read-only (get-total-co2-offset (user principal))
    (match (map-get? user-retirement-summary user)
        summary (ok (get total-co2-offset summary))
        (ok u0)
    )
)

(define-read-only (can-retire-credit
        (credit-id uint)
        (user principal)
    )
    (match (map-get? carbon-credits credit-id)
        credit (and
            (is-some (get buyer credit))
            (is-eq (unwrap-panic (get buyer credit)) user)
            (not (get retired credit))
            (not (is-credit-expired credit-id))
        )
        false
    )
)

(define-map escrow-agreements
    uint
    {
        credit-id: uint,
        buyer: principal,
        seller: principal,
        amount: uint,
        created-block: uint,
        expiry-block: uint,
        status: uint,
        settled-block: (optional uint),
    }
)

(define-map escrow-balances
    uint
    uint
)

(define-map user-escrow-history
    principal
    (list 30 uint)
)

(define-public (create-escrow
        (credit-id uint)
        (seller principal)
        (duration uint)
    )
    (let (
            (credit (unwrap! (map-get? carbon-credits credit-id) err-not-found))
            (escrow-id (var-get next-escrow-id))
        )
        (asserts! (is-eq (get issuer credit) seller) err-unauthorized)
        (asserts! (is-none (get buyer credit)) err-already-exists)
        (asserts! (not (get transferred credit)) err-already-exists)
        (asserts! (>= (stx-get-balance tx-sender) (get price credit))
            err-insufficient-funds
        )
        (asserts! (> duration u0) err-invalid-price)
        (map-set escrow-agreements escrow-id {
            credit-id: credit-id,
            buyer: tx-sender,
            seller: seller,
            amount: (get price credit),
            created-block: stacks-block-height,
            expiry-block: (+ stacks-block-height duration),
            status: u1,
            settled-block: none,
        })
        (map-set escrow-balances escrow-id (get price credit))
        (var-set next-escrow-id (+ escrow-id u1))
        (ok escrow-id)
    )
)

(define-public (complete-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (is-eq (get seller escrow) tx-sender) err-wrong-seller)
        (asserts! (is-eq (get status escrow) u1) err-escrow-already-settled)
        (asserts! (< stacks-block-height (get expiry-block escrow)) err-expired)
        (let ((credit (unwrap! (map-get? carbon-credits (get credit-id escrow))
                err-not-found
            )))
            (map-set carbon-credits (get credit-id escrow)
                (merge credit {
                    buyer: (some (get buyer escrow)),
                    transferred: true,
                })
            )
            (map-set escrow-agreements escrow-id
                (merge escrow {
                    status: u2,
                    settled-block: (some stacks-block-height),
                })
            )
            (ok true)
        )
    )
)

(define-public (cancel-escrow (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts!
            (or
                (is-eq (get buyer escrow) tx-sender)
                (>= stacks-block-height (get expiry-block escrow))
            )
            err-unauthorized
        )
        (asserts! (is-eq (get status escrow) u1) err-escrow-already-settled)
        (map-set escrow-agreements escrow-id
            (merge escrow {
                status: u3,
                settled-block: (some stacks-block-height),
            })
        )
        (ok true)
    )
)

(define-public (claim-escrow-refund (escrow-id uint))
    (let ((escrow (unwrap! (map-get? escrow-agreements escrow-id) err-escrow-not-found)))
        (asserts! (is-eq (get buyer escrow) tx-sender) err-wrong-buyer)
        (asserts! (is-eq (get status escrow) u3) err-escrow-already-settled)
        (ok true)
    )
)

(define-read-only (get-escrow-agreement (escrow-id uint))
    (match (map-get? escrow-agreements escrow-id)
        escrow (ok escrow)
        err-escrow-not-found
    )
)

(define-read-only (get-escrow-balance (escrow-id uint))
    (match (map-get? escrow-balances escrow-id)
        balance (ok balance)
        (ok u0)
    )
)

(define-read-only (is-escrow-active (escrow-id uint))
    (match (map-get? escrow-agreements escrow-id)
        escrow (and
            (is-eq (get status escrow) u1)
            (< stacks-block-height (get expiry-block escrow))
        )
        false
    )
)

(define-read-only (can-complete-escrow
        (escrow-id uint)
        (user principal)
    )
    (match (map-get? escrow-agreements escrow-id)
        escrow (and
            (is-eq (get seller escrow) user)
            (is-eq (get status escrow) u1)
            (< stacks-block-height (get expiry-block escrow))
        )
        false
    )
)

;; ==============================================================================
;; CREDIT ANALYTICS DASHBOARD
;; ==============================================================================

;; Analytics data storage
(define-map marketplace-analytics
    uint  ;; period block height
    {
        total-credits-issued: uint,
        total-credits-sold: uint,
        total-volume: uint,
        average-price: uint,
        active-sellers: uint,
        active-buyers: uint,
        period-start: uint,
        period-end: uint,
    }
)

(define-map daily-statistics
    uint  ;; day (block height / 144)
    {
        credits-issued: uint,
        credits-traded: uint,
        volume-traded: uint,
        unique-traders: uint,
        avg-price: uint,
        highest-price: uint,
        lowest-price: uint,
    }
)

(define-map price-history
    uint  ;; sequential price point ID
    {
        credit-id: uint,
        price: uint,
        timestamp: uint,
        transaction-type: uint,  ;; 1=direct sale, 2=auction, 3=bundle
    }
)

(define-map seller-performance
    principal
    {
        total-credits-issued: uint,
        total-credits-sold: uint,
        total-revenue: uint,
        average-sale-price: uint,
        success-rate: uint,  ;; percentage
        last-activity: uint,
    }
)

(define-map buyer-analytics
    principal
    {
        total-purchases: uint,
        total-spent: uint,
        average-purchase-price: uint,
        credits-retired: uint,
        co2-offset: uint,
        last-purchase: uint,
    }
)

;; Analytics data variables
(define-data-var next-price-point-id uint u1)
(define-data-var total-marketplace-volume uint u0)
(define-data-var total-credits-ever-issued uint u0)
(define-data-var total-credits-ever-sold uint u0)

;; Update analytics when credits are issued
(define-public (update-issuance-analytics (credit-id uint) (amount uint) (price uint))
    (begin
        (asserts! (is-some (map-get? carbon-credits credit-id)) err-not-found)
        (record-price-point credit-id price u1)
        (update-seller-analytics tx-sender amount price false)
        (update-daily-stats amount u0 u0)
        (var-set total-credits-ever-issued (+ (var-get total-credits-ever-issued) u1))
        (ok true)
    )
)

;; Update analytics when credits are sold
(define-public (update-sale-analytics (credit-id uint) (buyer principal) (price uint))
    (let ((credit (unwrap! (map-get? carbon-credits credit-id) err-not-found)))
        (record-price-point credit-id price u1)
        (update-seller-analytics (get issuer credit) (get amount credit) price true)
        (update-buyer-analytics buyer price (get amount credit))
        (update-daily-stats u0 u1 price)
        (var-set total-credits-ever-sold (+ (var-get total-credits-ever-sold) u1))
        (var-set total-marketplace-volume (+ (var-get total-marketplace-volume) price))
        (ok true)
    )
)

;; Record price points for trend analysis
(define-private (record-price-point (credit-id uint) (price uint) (tx-type uint))
    (let ((price-id (var-get next-price-point-id)))
        (map-set price-history price-id {
            credit-id: credit-id,
            price: price,
            timestamp: stacks-block-height,
            transaction-type: tx-type,
        })
        (var-set next-price-point-id (+ price-id u1))
        true
    )
)

;; Update seller performance metrics
(define-private (update-seller-analytics (seller principal) (amount uint) (price uint) (sold bool))
    (let ((current (default-to {
            total-credits-issued: u0,
            total-credits-sold: u0,
            total-revenue: u0,
            average-sale-price: u0,
            success-rate: u0,
            last-activity: u0,
        } (map-get? seller-performance seller))))
        (map-set seller-performance seller {
            total-credits-issued: (+ (get total-credits-issued current) u1),
            total-credits-sold: (if sold 
                (+ (get total-credits-sold current) u1)
                (get total-credits-sold current)
            ),
            total-revenue: (if sold
                (+ (get total-revenue current) price)
                (get total-revenue current)
            ),
            average-sale-price: (if sold
                (calculate-average-price (get total-revenue current) price (get total-credits-sold current))
                (get average-sale-price current)
            ),
            success-rate: (calculate-success-rate 
                (+ (get total-credits-issued current) u1)
                (if sold (+ (get total-credits-sold current) u1) (get total-credits-sold current))
            ),
            last-activity: stacks-block-height,
        })
        true
    )
)

;; Update buyer analytics
(define-private (update-buyer-analytics (buyer principal) (price uint) (amount uint))
    (let ((current (default-to {
            total-purchases: u0,
            total-spent: u0,
            average-purchase-price: u0,
            credits-retired: u0,
            co2-offset: u0,
            last-purchase: u0,
        } (map-get? buyer-analytics buyer))))
        (map-set buyer-analytics buyer {
            total-purchases: (+ (get total-purchases current) u1),
            total-spent: (+ (get total-spent current) price),
            average-purchase-price: (calculate-average-price (get total-spent current) price (get total-purchases current)),
            credits-retired: (get credits-retired current),
            co2-offset: (get co2-offset current),
            last-purchase: stacks-block-height,
        })
        true
    )
)

;; Update daily statistics
(define-private (update-daily-stats (issued uint) (traded uint) (volume uint))
    (let ((day (/ stacks-block-height u144)))  ;; Approximate daily blocks
        (let ((current (default-to {
                credits-issued: u0,
                credits-traded: u0,
                volume-traded: u0,
                unique-traders: u0,
                avg-price: u0,
                highest-price: u0,
                lowest-price: u999999999,
            } (map-get? daily-statistics day))))
            (map-set daily-statistics day {
                credits-issued: (+ (get credits-issued current) issued),
                credits-traded: (+ (get credits-traded current) traded),
                volume-traded: (+ (get volume-traded current) volume),
                unique-traders: (get unique-traders current),
                avg-price: (if (> volume u0)
                    (calculate-daily-avg-price (get volume-traded current) volume (get credits-traded current) traded)
                    (get avg-price current)
                ),
                highest-price: (if (> volume (get highest-price current)) volume (get highest-price current)),
                lowest-price: (if (and (> volume u0) (< volume (get lowest-price current))) volume (get lowest-price current)),
            })
        )
        true
    )
)

;; Helper functions for calculations
(define-private (calculate-average-price (current-total uint) (new-amount uint) (count uint))
    (if (> count u0)
        (/ (+ current-total new-amount) count)
        u0
    )
)

(define-private (calculate-success-rate (total uint) (sold uint))
    (if (> total u0)
        (/ (* sold u100) total)
        u0
    )
)

(define-private (calculate-daily-avg-price (current-volume uint) (new-volume uint) (current-trades uint) (new-trades uint))
    (let ((total-volume (+ current-volume new-volume))
          (total-trades (+ current-trades new-trades)))
        (if (> total-trades u0)
            (/ total-volume total-trades)
            u0
        )
    )
)

;; Read-only functions for analytics dashboard
(define-read-only (get-marketplace-overview)
    (ok {
        total-credits-issued: (var-get total-credits-ever-issued),
        total-credits-sold: (var-get total-credits-ever-sold),
        total-volume: (var-get total-marketplace-volume),
        active-credits: (get-active-credits-count),
        current-block: stacks-block-height,
    })
)

(define-read-only (get-seller-performance (seller principal))
    (match (map-get? seller-performance seller)
        performance (ok performance)
        (ok {
            total-credits-issued: u0,
            total-credits-sold: u0,
            total-revenue: u0,
            average-sale-price: u0,
            success-rate: u0,
            last-activity: u0,
        })
    )
)

(define-read-only (get-buyer-analytics (buyer principal))
    (match (map-get? buyer-analytics buyer)
        analytics (ok analytics)
        (ok {
            total-purchases: u0,
            total-spent: u0,
            average-purchase-price: u0,
            credits-retired: u0,
            co2-offset: u0,
            last-purchase: u0,
        })
    )
)

(define-read-only (get-daily-statistics (day uint))
    (match (map-get? daily-statistics day)
        stats (ok stats)
        err-not-found
    )
)

(define-read-only (get-recent-price-points (limit uint))
    (let ((current-id (var-get next-price-point-id)))
        (if (> current-id limit)
            (ok (get-price-points-range (- current-id limit) current-id))
            (ok (get-price-points-range u1 current-id))
        )
    )
)

(define-private (get-price-points-range (start uint) (end uint))
    (let ((range-size (- end start)))
        (if (<= range-size u20)
            (fold collect-price-point (list u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 u17 u18 u19 u20) (list))
            (list)
        )
    )
)

(define-private (collect-price-point (index uint) (collected (list 20 {credit-id: uint, price: uint, timestamp: uint, transaction-type: uint})))
    (let ((current-id (var-get next-price-point-id))
          (point-id (- current-id index)))
        (if (> point-id u0)
            (match (map-get? price-history point-id)
                point (unwrap-panic (as-max-len? (append collected point) u20))
                collected
            )
            collected
        )
    )
)

(define-read-only (get-market-trends)
    (let ((current-day (/ stacks-block-height u144))
          (yesterday (- current-day u1)))
        (match (map-get? daily-statistics current-day)
            today-stats
                (match (map-get? daily-statistics yesterday)
                    yesterday-stats (ok {
                        today: today-stats,
                        yesterday: yesterday-stats,
                        volume-change: (if (> (get volume-traded yesterday-stats) u0)
                            (/ (* (- (get volume-traded today-stats) (get volume-traded yesterday-stats)) u100)
                               (get volume-traded yesterday-stats))
                            u0
                        ),
                        price-change: (if (> (get avg-price yesterday-stats) u0)
                            (/ (* (- (get avg-price today-stats) (get avg-price yesterday-stats)) u100)
                               (get avg-price yesterday-stats))
                            u0
                        ),
                    })
                    err-not-found
                )
            err-not-found
        )
    )
)
