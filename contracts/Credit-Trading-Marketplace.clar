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
    (fold update-credit-buyer credit-ids true)
)

(define-private (update-credit-buyer
        (credit-id uint)
        (success bool)
    )
    (match (map-get? carbon-credits credit-id)
        credit (begin
            (map-set carbon-credits credit-id
                (merge credit {
                    buyer: (some tx-sender),
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
