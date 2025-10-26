# 🏭 Tokenized Machine Leasing

A Clarity smart contract that enables factories to tokenize heavy machinery and allows small businesses to lease machine time through blockchain technology.

## 🚀 Features

- **🔧 Machine Tokenization**: Factories can mint NFTs representing physical machines
- **⏱️ Time-based Leasing**: Lease machines by blocks with hourly rate calculations  
- **💰 Automated Payments**: Smart contract handles payment distribution and platform fees
- **📊 Earnings Management**: Track and withdraw earnings for factories and platform
- **🔐 Access Control**: Secure ownership and lease management
- **⚡ Real-time Status**: Check machine availability and lease expiration

## 🏗️ Contract Architecture

The contract implements an NFT-based system where each machine is represented as a unique token. Factories own these tokens and can set hourly rates, while lessees pay to access machines for specific time periods.

## 📋 Core Functions

### Factory Operations

**🏭 Tokenize Machine**
```clarity
(tokenize-machine "CNC-Mill-X1" u1000)
```
Creates a new machine NFT with name and hourly rate (in microSTX).

**💸 Withdraw Earnings**
```clarity
(withdraw-earnings)
```
Withdraw accumulated earnings from machine leases.

**⚙️ Manage Machine**
```clarity
(deactivate-machine u1)
(reactivate-machine u1)
(update-hourly-rate u1 u1500)
```

### Lessee Operations

**📝 Lease Machine**
```clarity
(lease-machine u1 u1440)
```
Lease machine #1 for 1440 blocks (~10 hours).

**🔚 End Lease**
```clarity
(end-lease u1)
```

### Query Functions

**🔍 Check Availability**
```clarity
(is-machine-available u1)
(get-lease-time-remaining u1)
(calculate-lease-cost u1 u1440)
```

**📈 Get Information**
```clarity
(get-machine u1)
(get-machine-lease u1)
(get-factory-earnings 'ST1HTBVD3JG9C05J7HBJTHGR0GGW7KXW28M5JS8QE)
```

## ⚡ Quick Start

1. **Deploy Contract**
   ```bash
   clarinet deploy
   ```

2. **Tokenize Your First Machine**
   ```clarity
   (contract-call? .tokenized-machine-leasing tokenize-machine "Industrial-Lathe" u800)
   ```

3. **Lease a Machine**
   ```clarity
   (contract-call? .tokenized-machine-leasing lease-machine u1 u2880)
   ```

## 💡 Usage Examples

### Factory Workflow
```clarity
;; 1. Tokenize machine
(contract-call? .tokenized-machine-leasing tokenize-machine "3D-Printer-Pro" u500)

;; 2. Update rate later
(contract-call? .tokenized-machine-leasing update-hourly-rate u1 u600)

;; 3. Withdraw earnings
(contract-call? .tokenized-machine-leasing withdraw-earnings)
```

### Small Business Workflow  
```clarity
;; 1. Check availability and cost
(contract-call? .tokenized-machine-leasing is-machine-available u1)
(contract-call? .tokenized-machine-leasing calculate-lease-cost u1 u1440)

;; 2. Lease machine for 10 hours
(contract-call? .tokenized-machine-leasing lease-machine u1 u1440)

;; 3. Check remaining time
(contract-call? .tokenized-machine-leasing get-lease-time-remaining u1)
```

## 🔧 Configuration

- **Platform Fee**: Default 2.5% (250 basis points)
- **Minimum Lease**: 144 blocks (~1 hour)
- **Time Calculation**: 144 blocks = 1 hour

## 🛡️ Security Features

- ✅ Owner-only machine management
- ✅ Active lease protection
- ✅ Payment validation
- ✅ Access control enforcement
- ✅ Automatic fee distribution

## 📊 Economic Model

```
Lease Payment = Hours × Hourly Rate
Platform Fee = 2.5% of total payment
Factory Payment = 97.5% of total payment
```

## 🧪 Testing

```bash
clarinet test
```

## 🤝 Contributing

1. Fork the repository
2. Create your feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## 📄 License

This project is licensed under the MIT License.

---

Built with ❤️ for the decentralized manufacturing economy
