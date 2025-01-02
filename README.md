# Quantitative Strategies in R

This repository contains two projects that demonstrate the development, backtesting, and risk analysis of quantitative trading strategies using the `quantstrat` R package.

---

## 1. Advanced Momentum-Reversion Strategy

### Portfolio Backtesting and Risk Analysis with `quantstrat`

This implementation showcases a **systematic portfolio backtesting and risk analysis workflow** for an advanced momentum-reversion strategy. Below is a technical breakdown:

### Technical Implementation

1. **Portfolio Initialization**:
   - Six separate portfolios were initialized, each tied to specific symbols (e.g., AAPL, TSLA).
   - The account was funded with an initial equity of $100,000, evenly distributed across the portfolios.

2. **Market Data Acquisition**:
   - Historical price data for specified symbols (e.g., AAPL, TSLA, SPY) was fetched from Yahoo Finance for the period starting January 2020.

3. **Technical Indicators**:
   - The strategy utilized multiple indicators:
     - **RSI (Relative Strength Index)**: To identify overbought/oversold conditions.
     - **MACD (Moving Average Convergence Divergence)**: To detect momentum shifts.
     - **Bollinger Bands**: To identify price extremes and reversals.

4. **Entry and Exit Signals**:
   - **Entry Signals**: Triggered by:
     - RSI < 30 for long positions.
     - MACD bullish crossover.
     - Price crossing below the lower Bollinger Band.
   - **Exit Signals**: Triggered by:
     - RSI > 70 for closing long positions.
     - MACD bearish crossover.
     - Price crossing above the upper Bollinger Band.

5. **Dynamic Order Sizing**:
   - Order sizes were dynamically adjusted based on available funds and asset prices, ensuring adherence to portfolio constraints.

6. **Monte Carlo Simulation**:
   - Ran **1,000 portfolio simulations** over a 252-day horizon using daily portfolio return statistics (mean and standard deviation).
   - Key metrics included **Value at Risk (VaR)** and **Conditional VaR (CVaR)**.

7. **Performance Metrics**:
   - Calculated metrics included:
     - **Alpha**: Portfolio's excess return compared to the benchmark.
     - **Beta**: Sensitivity to benchmark movements.
     - **Sharpe Ratio** and **Sortino Ratio**.
     - **Maximum Drawdown** and **Annualized Volatility**.

8. **Visualization**:
   - Generated performance charts for:
     - Portfolio equity growth over time.
     - Individual position performance.
     - Monte Carlo simulation paths with average overlays.

---

## 2. Portfolio Creation & Risk Management

### Portfolio Backtesting and Monte Carlo Risk Analysis with `quantstrat`

This project implements a **buy-and-hold strategy** for two portfolios (`TechPortfolio` and `FinancePortfolio`) and benchmarks them against SPY. The analysis includes risk metrics, performance visualization, and Monte Carlo simulations.

### Overview

#### Setup
- **Symbols**:
  - **Tech Portfolio**: AAPL, MSFT, TSLA
  - **Finance Portfolio**: JPM, BAC, GS
  - Benchmarked against SPY.
- **Initial Equity**: $200,000 split evenly between the two portfolios.
- **Date Range**: January 1, 2024, to December 27, 2024.

#### Transaction Logic
For each symbol in the Tech and Finance portfolios:
- **Buy** on the second trading day.
- **Sell** on the last trading day.
- Quantities were determined dynamically based on available capital.

#### Performance Analysis
- Generated **equity curves** and **trade statistics** for each portfolio.
- Calculated key metrics, including:
  - **Alpha** and **Beta**.
  - **Sharpe Ratio** and **Sortino Ratio**.
  - **Maximum Drawdown**.

#### Monte Carlo Simulation
- Ran **1,000 portfolio simulations** over a 252-day horizon.
- Estimated risk metrics:
  - **Value at Risk (VaR)**: 95th percentile loss threshold.
  - **Conditional VaR (CVaR)**: Average loss below the VaR threshold.

---

### Key Features (for Both Strategies)

1. **Transaction Execution**:
   - Buy and sell logic with dynamic quantity allocation.

2. **Performance Metrics**:
   - **Total Return**, **Annualized Return**, **Alpha**, **Beta**, **Sharpe Ratio**, **Sortino Ratio**, and **Maximum Drawdown**.

3. **Monte Carlo Risk Analysis**:
   - Simulated portfolio performance paths with risk metrics:
     - **VaR (95%)**: Value at Risk.
     - **CVaR (95%)**: Conditional VaR.

4. **Visualization**:
   - Detailed charts for:
     - Portfolio equity over time.
     - Individual position performance.
     - Simulated portfolio paths.

---
