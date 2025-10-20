# Carbon Credit Trading System

## Overview
Introduces a comprehensive carbon credit trading system that allows solar panel projects to generate, list, trade, and retire carbon credits based on energy production. This feature adds environmental impact tracking and creates new revenue streams for renewable energy projects within the DAO ecosystem.

## Technical Implementation
**Key Functions Added:**
- `generate-carbon-credits`: Converts energy production (kWh) to carbon credits using configurable offset rates
- `list-carbon-credits-for-sale`: Creates marketplace listings for carbon credits with price discovery
- `buy-carbon-credits`: Enables DAO members to purchase carbon credits with STX payments
- `retire-carbon-credits`: Permanently removes credits from circulation for environmental claims

**Data Structures:**
- `carbon-credits` map: Tracks credit listings with project association, pricing, and transaction history
- `project-carbon-balance` map: Maintains carbon credit balances per solar project
- `member-carbon-holdings` map: Records individual member carbon credit portfolios

**Constants:**
- `CARBON-OFFSET-RATE`: 50 credits per 100 kWh (configurable carbon generation rate)
- `MIN-CARBON-PRICE`: 100 microSTX minimum pricing floor
- Enhanced error handling with 5 new carbon-specific error codes

## Testing & Validation
- ✅ Contract passes clarinet check
- ✅ All npm tests successful  
- ✅ CI/CD pipeline configured
- ✅ Clarity v3 compliant with proper error handling
