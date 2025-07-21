# 🌞 Solar Panel DAO Incentive Model

> *Decentralized renewable energy funding through community-driven solar panel projects*

## 📋 Overview

The Solar Panel DAO Incentive Model enables neighborhoods to collaboratively fund solar panel installations through a decentralized autonomous organization (DAO). Community members can propose projects, vote on proposals, pool funds, and share the benefits of renewable energy installations.

## ✨ Key Features

- 🏘️ **Community Proposals**: Members can propose solar panel installation projects
- 🗳️ **Democratic Voting**: Community-driven approval process for project funding
- 💰 **Shared Funding**: Pool resources to overcome high upfront solar costs
- ⚡ **Energy Rewards**: Proportional distribution of energy savings and rewards
- 🛡️ **Certified Installers**: Only verified professionals can receive project funds
- 📊 **Transparent Tracking**: Real-time project status and funding progress

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX wallet for transactions

### Installation

1. Clone the repository
2. Run `clarinet check` to verify contract compilation
3. Deploy the contract to your preferred Stacks network

## 🔧 Usage

### Joining the DAO

```clarity
(contract-call? .solar-panel-dao join-dao)
```

### Contributing Funds

```clarity
(contract-call? .solar-panel-dao contribute-to-dao u1000000) ;; 1 STX minimum
```

### Creating a Proposal

```clarity
(contract-call? .solar-panel-dao create-proposal
  "Neighborhood Solar Array"
  "Install 20kW solar array for 15 households"
  u50000000 ;; 50 STX funding goal
  'SP1INSTALLER_ADDRESS)
```

### Voting on Proposals

```clarity
(contract-call? .solar-panel-dao vote-on-proposal u1 true) ;; Vote yes on proposal #1
```

### Contributing to Approved Projects

```clarity
(contract-call? .solar-panel-dao contribute-to-project u1 u5000000) ;; 5 STX
```

## 📊 Contract Functions

### Public Functions

| Function | Description |
|----------|-------------|
| `join-dao` | Join the DAO as a member |
| `contribute-to-dao` | Add funds to the general DAO treasury |
| `create-proposal` | Submit a new solar project proposal |
| `vote-on-proposal` | Vote on active proposals |
| `contribute-to-project` | Fund specific approved projects |
| `claim-energy-rewards` | Claim accumulated energy rewards |

### Read-Only Functions

| Function | Description |
|----------|-------------|
| `get-proposal` | Get details of a specific proposal |
| `get-member-info` | Get member contribution and join date |
| `get-dao-stats` | Get overall DAO statistics |
| `get-proposal-status` | Get current status and funding progress |

### Admin Functions

| Function | Description |
|----------|-------------|
| `certify-installer` | Verify installer credentials |
| `release-funds` | Release funds to certified installers |
| `distribute-energy-rewards` | Distribute energy savings rewards |

## 🏗️ Contract Architecture

### Data Structures

- **DAO Members**: Track member contributions and join dates
- **Proposals**: Store project details, voting data, and funding status
- **Votes**: Record individual member votes on proposals
- **Contributions**: Track member contributions to specific projects
- **Energy Rewards**: Manage reward distribution to members

### Key Constants

- `VOTING-PERIOD`: 1440 blocks (~7 days)
- `MIN-CONTRIBUTION`: 1,000,000 microSTX (1 STX)
- `APPROVAL-THRESHOLD`: 51% approval required

## 🔒 Security Features

- Owner-only installer certification
- Member-only proposal creation and voting
- Minimum contribution requirements
- Voting period enforcement
- Fund release controls

## 🌱 Impact

- **Democratizes** access to solar technology
- **Reduces** individual financial barriers
- **Promotes** sustainable community infrastructure
- **Enables** transparent renewable energy funding
- **Creates** shared economic benefits

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Add tests for new functionality
4. Ensure `clarinet check` passes
5. Submit a pull request

## 📄 License

This project is open source and available under the MIT License.

---

*Building a sustainable future, one solar panel at a time* ☀️🌍
