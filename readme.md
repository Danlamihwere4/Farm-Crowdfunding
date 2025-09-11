# Farm-Crowdfunding & Revenue Sharing

A decentralized platform for farmers to raise funds for specific projects and share revenue with investors. This smart contract allows farmers to create funding campaigns for agricultural projects, while investors can purchase shares in these projects and receive proportional returns from the revenue generated.

## Features

- **Project Creation**: Farmers can create funding projects with specific targets
- **Investment Mechanism**: Investors can purchase shares in farm projects
- **NFT Shares**: Each investment is represented as an NFT share
- **Revenue Distribution**: Farmers can distribute revenue to investors
- **Revenue Claims**: Investors can claim their proportional revenue

## Contract Functions

### For Farmers

- `create-project`: Create a new funding project
- `close-funding`: Close the funding period for a project
- `add-revenue`: Add revenue generated from the project
- `distribute-revenue`: Distribute revenue to investors

### For Investors

- `invest`: Invest in a farming project
- `claim-revenue`: Claim your share of distributed revenue

## Usage Examples

### Creating a Project (Farmer)

```clarity
(contract-call? .farm-crowdfunding create-project "Organic Apple Orchard" "Expanding our organic apple orchard with 100 new trees" u10000000 u100000 u1000000)
```

### Investing in a Project (Investor)

```clarity
(contract-call? .farm-crowdfunding invest u1 u500000)
```

### Closing Funding (Farmer)

```clarity
(contract-call? .farm-crowdfunding close-funding u1)
```

### Adding Revenue (Farmer)

```clarity
(contract-call? .farm-crowdfunding add-revenue u1 u2000000)
```

### Distributing Revenue (Farmer)

```clarity
(contract-call? .farm-crowdfunding distribute-revenue u1)
```

### Claiming Revenue (Investor)

```clarity
(contract-call? .farm-crowdfunding claim-revenue u1 u0)
```

## Error Codes

- `u100`: Owner only operation
- `u101`: Resource not found
- `u102`: Unauthorized operation
- `u103`: Resource already exists
- `u104`: Funding period closed
- `u105`: Funding period still active
- `u106`: Insufficient funds
- `u107`: Below minimum investment
- `u108`: Above maximum investment
- `u109`: Funding target not reached
- `u110`: No revenue available
- `u111`: Revenue already claimed

## Getting Started

1. Clone this repository
2. Install Clarinet: https://github.com/hirosystems/clarinet
3. Run `clarinet console` to interact with the contract
```
