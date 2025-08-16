# 🌍 EcoTrips - Sustainable Tourism DAO

A decentralized autonomous organization (DAO) built on Stacks blockchain that enables communities to fund and vote on sustainable travel projects. EcoTrips empowers eco-conscious travelers and organizations to collectively support environmentally responsible tourism initiatives.

## ✨ Features

- 🗳️ **Democratic Voting**: Stake-weighted voting system for project proposals
- 💰 **Crowdfunding**: Community-driven funding for approved projects  
- 🌱 **Sustainable Focus**: Dedicated to eco-friendly travel initiatives
- 🏛️ **DAO Governance**: Decentralized decision-making process
- 🔒 **Secure Funding**: Smart contract-based fund management

## 🚀 Getting Started

### Prerequisites

- [Clarinet](https://github.com/hirosystems/clarinet) installed
- STX tokens for staking and funding

### Installation

1. Clone the repository
2. Navigate to the project directory
3. Deploy the contract using Clarinet

```bash
clarinet deploy
```

## 📖 Usage Guide

### 1. Join the DAO 🤝

Stake STX tokens to become a voting member:

```clarity
(contract-call? .EcoTrips join-dao u1000000)
```

### 2. Propose a Project 📝

Submit sustainable travel project proposals:

```clarity
(contract-call? .EcoTrips propose-project 
  "Coral Reef Restoration Tour" 
  "Educational diving tours that fund coral restoration efforts" 
  u5000000)
```

### 3. Vote on Projects 🗳️

Cast your vote on active proposals:

```clarity
(contract-call? .EcoTrips vote-on-project u1 true)
```

### 4. Fund Approved Projects 💸

Contribute to projects that passed voting:

```clarity
(contract-call? .EcoTrips fund-project u1 u1000000)
```

### 5. Finalize Voting ⏰

Close voting period and determine project status:

```clarity
(contract-call? .EcoTrips finalize-voting u1)
```

## 🔍 Read-Only Functions

- `get-project`: View project details
- `get-member-stake`: Check member's stake amount
- `get-treasury-balance`: View total DAO treasury
- `get-member-vote`: Check how a member voted
- `get-project-funding`: View funding contributions

## 🎯 Project Lifecycle

1. **Proposal** 📋 - Members with minimum stake can propose projects
2. **Voting** 🗳️ - Community votes during voting period
3. **Approved** ✅ - Projects with majority support move to funding
4. **Funded** 💰 - Fully funded projects ready for execution  
5. **Completed** 🎉 - Funds released to project proposer

## ⚙️ Configuration

- **Minimum Proposal Deposit**: 1 STX (adjustable by contract owner)
- **Voting Period**: 1440 blocks (~10 days, adjustable)
- **Voting Weight**: Based on member's staked amount

## 🛡️ Security Features

- Stake-based voting prevents spam proposals
- Time-locked voting periods ensure fair participation
- Secure fund management through smart contracts
- Member stake withdrawal protection

## 🌟 Example Projects

- 🐢 Sea turtle conservation tours
- 🌳 Reforestation travel experiences  
- 🏔️ Sustainable mountain trekking
- 🌊 Ocean cleanup expeditions
- 🦋 Wildlife sanctuary visits

## 📊 Error Codes

- `u100`: Not authorized
- `u101`: Invalid amount
- `u102`: Project not found
- `u103`: Voting period ended
- `u104`: Already voted
- `u105`: Insufficient funds
- `u106`: Project not approved
- `u107`: Already funded

## 🤝 Contributing

We welcome contributions to make EcoTrips even better! Feel free to submit issues, feature requests, or pull requests.

## 📄 License

This project is open source and available under the MIT License.


