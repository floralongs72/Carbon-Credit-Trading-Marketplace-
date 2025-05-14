(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-insufficient-funds (err u104))

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
