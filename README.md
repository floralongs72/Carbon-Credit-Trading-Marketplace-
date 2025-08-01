# 🌳 Carbon Credit Trading Marketplace 🌍

A decentralized marketplace for trading carbon credits on the Stacks blockchain, providing transparent and verifiable carbon offsets for businesses and individuals.

## ✨ Features

- 🌱 Mint carbon credits with project details and vintage information
- ✅ Verification system for carbon credits by authorized verifiers
- 💹 List and trade carbon credits in a decentralized marketplace
- 🔄 Transfer carbon credits between users
- 📊 View active marketplace listings

## 📋 Contract Functions

### Carbon Credit Management

- `mint-carbon-credit`: Create a new carbon credit with project details
- `verify-carbon-credit`: Verify a carbon credit (verifier only)
- `transfer-carbon-credit`: Transfer a carbon credit to another user
- `set-verifier`: Set the authorized verifier (owner only)

### Marketplace Functions

- `list-carbon-credit`: List a carbon credit for sale
- `cancel-listing`: Cancel an active listing
- `buy-carbon-credit`: Purchase a carbon credit from the marketplace

### Read-Only Functions

- `get-token-metadata`: Get details about a specific carbon credit
- `get-listing`: Get details about a marketplace listing
- `get-active-listings`: Get all active marketplace listings

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- [Stacks wallet](https://hiro.so/wallet/install-web) for testing on testnet

### Installation

1. Clone this repository
2. Navigate to the project directory
3. Use Clarinet to deploy and test the contract:

```bash
clarinet console
```

### Example Usage

Mint a new carbon credit:
```
(contract-call? .credit-trading-marketplace mint-carbon-credit "Reforestation Project" "Amazon Rainforest" u100 u2023)
```

List a carbon credit for sale:
```
(contract-call? .credit-trading-marketplace list-carbon-credit u1 u1000000 u10000)
```

Buy a carbon credit:
```
(contract-call? .credit-trading-marketplace buy-carbon-credit u1)
```

## 🔒 Security Considerations

- All transfers require ownership verification
- Only verified carbon credits provide full transparency
- Marketplace listings have expiration dates

## 🤝 Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## 📜 License

This project is licensed under the MIT License.
```