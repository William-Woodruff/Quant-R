# Load required libraries
library(quantstrat)
library(quantmod)
library(PerformanceAnalytics)

# Clear previous environments
if (exists(".blotter", envir = .GlobalEnv)) {
    rm(list = ls(envir = .blotter), envir = .blotter)
}
if (exists(".strategy", envir = .GlobalEnv)) {
    rm(list = ls(envir = .strategy), envir = .strategy)
}
if (exists(".instrument", envir = .GlobalEnv)) {
    rm(list = ls(envir = .instrument), envir = .instrument)
}

# Setup parameters
from <- "2024-01-01"
to <- "2024-12-27"
initDate <- "2024-01-01"
initEq <- 200000

# Symbols
# Benchmark symbol
benchmark <- "SPY"
Tech <- c("AAPL", "MSFT", "TSLA")
Finance <- c("JPM", "BAC", "GS")
symbols <- c(Tech, Finance)

# Create instruments and get symbol data
currency("USD")
stock(symbols, currency = "USD", multiplier = 1)
getSymbols(symbols, from = from, to = to, adjust = TRUE)

stock(benchmark, currency = "USD", multiplier = 1)
getSymbols(benchmark, from = from, to = to, adjust = TRUE)

# Initialize portfolios and account
initPortf("TechPortfolio", symbol = Tech, initDate = initDate)
initPortf("FinancePortfolio", symbol = Finance, initDate = initDate)
initAcct("BuyHoldAccount", portfolios = c("TechPortfolio", "FinancePortfolio"), initDate = initDate, initEq = initEq)

# Transaction logic for TechPortfolio
for (sym in Tech) {
    BuyDate <- time(get(sym))[2]
    BuyPrice <- as.numeric(Op(get(sym)[BuyDate, ]))
    SellDate <- last(time(get(sym)))
    SellPrice <- as.numeric(Cl(get(sym)[SellDate, ]))
    Qty <- trunc(initEq / (2 * length(Tech) * BuyPrice))

    # Add buy transaction
    addTxn(
        Portfolio = "TechPortfolio",
        Symbol = sym,
        TxnDate = BuyDate,
        TxnQty = Qty,
        TxnPrice = BuyPrice,
        TxnFees = 0
    )

    # Add sell transaction
    addTxn(
        Portfolio = "TechPortfolio",
        Symbol = sym,
        TxnDate = SellDate,
        TxnQty = -Qty,
        TxnPrice = SellPrice,
        TxnFees = 0
    )
}

# Transaction logic for FinancePortfolio
for (sym in Finance) {
    BuyDate <- time(get(sym))[2]
    BuyPrice <- as.numeric(Op(get(sym)[BuyDate, ]))
    SellDate <- last(time(get(sym)))
    SellPrice <- as.numeric(Cl(get(sym)[SellDate, ]))
    Qty <- trunc(initEq / (2 * length(Finance) * BuyPrice))

    # Add buy transaction
    addTxn(
        Portfolio = "FinancePortfolio",
        Symbol = sym,
        TxnDate = BuyDate,
        TxnQty = Qty,
        TxnPrice = BuyPrice,
        TxnFees = 0
    )

    # Add sell transaction
    addTxn(
        Portfolio = "FinancePortfolio",
        Symbol = sym,
        TxnDate = SellDate,
        TxnQty = -Qty,
        TxnPrice = SellPrice,
        TxnFees = 0
    )
}

# Update portfolios, account, and equity
updatePortf(Portfolio = "TechPortfolio")
updatePortf(Portfolio = "FinancePortfolio")
updateAcct(name = "BuyHoldAccount")
updateEndEq(Account = "BuyHoldAccount")

print("#------------------------------- Performance Stats -------------------------------#")

rets <- PortfReturns(Account = "BuyHoldAccount")

# Plot performance summary
quartz()
charts.PerformanceSummary(rets, main = "Buy-and-Hold Strategy Performance")

# Generate performance charts for each portfolio
for (sym in Tech) {
    quartz()
    chart.Posn("TechPortfolio", Symbol = sym, main = paste("Tech Performance of", sym))
}

for (sym in Finance) {
    quartz()
    chart.Posn("FinancePortfolio", Symbol = sym, main = paste("Finance Performance of", sym))
}

# Calculate and display per-trade statistics for each portfolio
tstats_tech <- tradeStats("TechPortfolio")
tstats_finance <- tradeStats("FinancePortfolio")

print("Tech Portfolio Trade Stats:")
print(t(tstats_tech))

print("Finance Portfolio Trade Stats:")
print(t(tstats_finance))

# Extract Ending Equity (End.Eq) for the account
end_eq <- getAccount(Account = "BuyHoldAccount")$summary$End.Eq

# Convert End.Eq to a time series
end_eq_ts <- xts(end_eq, order.by = index(end_eq))

# Plot Ending Equity
quartz()
plot(
    index(end_eq_ts),
    end_eq_ts,
    type = "l",
    col = "green",
    lwd = 2,
    xlab = "Date",
    ylab = "Ending Equity",
    main = "Ending Equity Over Time"
)
grid()


print("#------------------------------- Risk & Asset Stats -------------------------------#")
rets <- PortfReturns(Account = "BuyHoldAccount")

# Benchmark returns
benchmark_returns <- dailyReturn(Cl(SPY))

# Ensure returns are aligned
aligned_returns <- merge.xts(rets, benchmark_returns, all = FALSE)
portfolio_returns <- aligned_returns[, 1]
benchmark_returns <- aligned_returns[, 2]

# Calculate Alpha and Beta
alpha <- CAPM.alpha(portfolio_returns, benchmark_returns)
beta <- CAPM.beta(portfolio_returns, benchmark_returns)

# Print Alpha and Beta
cat("Portfolio Alpha: ", round(alpha, 4), "\n")
cat("Portfolio Beta: ", round(beta, 4), "\n")

# Metrics calculations
total_return <- Return.cumulative(portfolio_returns)
annualized_return <- Return.annualized(portfolio_returns)
sharpe_ratio <- SharpeRatio.annualized(portfolio_returns)
sortino_ratio <- SortinoRatio(portfolio_returns)
max_drawdown <- maxDrawdown(portfolio_returns)
volatility <- StdDev.annualized(portfolio_returns)
value_at_risk <- VaR(portfolio_returns, p = 0.95, method = "historical")
conditional_var <- ES(portfolio_returns, p = 0.95, method = "historical")

# Print metrics
metrics <- data.frame(
    Metric = c(
        "Total Return", "Annualized Return", "Sharpe Ratio", "Sortino Ratio",
        "Maximum Drawdown", "Volatility", "Value at Risk (VaR)", "Conditional VaR (CVaR)",
        "Alpha", "Beta"
    ),
    Value = c(
        total_return, annualized_return, sharpe_ratio, sortino_ratio,
        max_drawdown, volatility, value_at_risk, conditional_var, alpha, beta
    )
)
print(metrics)

print("#------------------------------- Account and Portfolio Returns -------------------------------#")

# Retrieve the initial and final account equity
a <- as.numeric(getAccount("BuyHoldAccount")$summary$End.Eq[1]) # Initial equity
b <- as.numeric(last(getAccount("BuyHoldAccount")$summary$End.Eq)) # Final equity


# Calculate and print the account return
account_return <- ((b - a) / a) * 100 # Correct return calculation
print(paste("Account Return:", round(account_return, 2), "%"))


# Retrieve portfolio P&L for TechPortfolio
tech_symbols <- names(getPortfolio("TechPortfolio")$symbols)
tech_pl <- sapply(tech_symbols, function(sym) {
    sum(getPortfolio("TechPortfolio")$symbols[[sym]]$txn$Net.Txn.Realized.PL)
})
tech_total_pl <- sum(tech_pl)
tech_initial_eq <- initEq / 2 # Assuming 50% of initial equity allocated to TechPortfolio
tech_return <- tech_total_pl / tech_initial_eq * 100

# Retrieve portfolio P&L for FinancePortfolio
finance_symbols <- names(getPortfolio("FinancePortfolio")$symbols)
finance_pl <- sapply(finance_symbols, function(sym) {
    sum(getPortfolio("FinancePortfolio")$symbols[[sym]]$txn$Net.Txn.Realized.PL)
})
finance_total_pl <- sum(finance_pl)
finance_initial_eq <- initEq / 2 # Assuming 50% of initial equity allocated to FinancePortfolio
finance_return <- finance_total_pl / finance_initial_eq * 100

# Print portfolio returns
print(paste("Tech Portfolio Return:", round(tech_return, 2), "%"))
print(paste("Finance Portfolio Return:", round(finance_return, 2), "%"))

print("#------------------------------- Portfolio Equity Curves -------------------------------#")

# Extract Net.Trading.PL for TechPortfolio and FinancePortfolio
net_trading_pl_tech <- getPortfolio("TechPortfolio")$summary$Net.Trading.PL
net_trading_pl_finance <- getPortfolio("FinancePortfolio")$summary$Net.Trading.PL

# Extract time index from End.Eq of BuyHoldAccount
time_index <- index(getAccount(Account = "BuyHoldAccount")$summary$End.Eq)

# Ensure both Net.Trading.PL align with the same index
net_trading_pl_tech_ts <- xts(net_trading_pl_tech, order.by = time_index)
net_trading_pl_finance_ts <- xts(net_trading_pl_finance, order.by = time_index)

# Start at 100,000 and accumulate Net.Trading.PL for both portfolios
starting_balance <- 100000
cumulative_balance_tech <- cumsum(net_trading_pl_tech_ts) + starting_balance
cumulative_balance_finance <- cumsum(net_trading_pl_finance_ts) + starting_balance

# Plot cumulative balances on the same graph
quartz()
plot(
    index(cumulative_balance_tech),
    cumulative_balance_tech,
    type = "l",
    col = "blue",
    lwd = 2,
    xlab = "Date",
    ylab = "Cumulative Balance",
    main = "Cumulative Balances of TechPortfolio and FinancePortfolio Over Time"
)
lines(
    index(cumulative_balance_finance),
    cumulative_balance_finance,
    col = "red",
    lwd = 2
)
legend(
    "topright",
    legend = c("TechPortfolio", "FinancePortfolio"),
    col = c("blue", "red"),
    lwd = 2
)
grid()

print("#------------------------------- Monte Carlos Simulation -------------------------------#")

# Monte Carlo Parameters
n_simulations <- 1000 # Number of simulations
n_days <- 252 # Simulation horizon (1 year of trading days)
starting_balance <- initEq # Initial equity (same as portfolio starting equity)

# Extract portfolio returns
portfolio_returns <- PortfReturns(Account = "BuyHoldAccount")
mean_return <- mean(rowSums(portfolio_returns, na.rm = TRUE)) # Mean daily return
sd_return <- sd(rowSums(portfolio_returns, na.rm = TRUE)) # Std deviation of daily return

# Monte Carlo Simulation
set.seed(42) # For reproducibility
mc_simulations <- matrix(NA, nrow = n_days, ncol = n_simulations)

for (i in 1:n_simulations) {
    daily_returns <- rnorm(n_days, mean = mean_return, sd = sd_return)
    mc_simulations[, i] <- cumprod(1 + daily_returns) * starting_balance
}

# Monte Carlo Visualization
quartz()
matplot(
    1:n_days,
    mc_simulations,
    type = "l",
    lty = 1,
    col = rgb(0, 0, 1, 0.1), # Semi-transparent blue for individual paths
    xlab = "Days",
    ylab = "Portfolio Value",
    main = "Monte Carlo Simulation of Portfolio Performance"
)
lines(1:n_days, apply(mc_simulations, 1, mean), col = "red", lwd = 2) # Average path
grid()

# Monte Carlo Risk Metrics
final_values <- mc_simulations[n_days, ]
VaR_95 <- quantile(final_values, probs = 0.05) # 5th percentile for Value at Risk
CVaR_95 <- mean(final_values[final_values <= VaR_95]) # Conditional VaR

# Print Results
cat("Monte Carlo Simulation Results:\n")
cat("Starting Balance:", starting_balance, "\n")
cat("Monte Carlo VaR (95%):", round(VaR_95, 2), "; Return", round((VaR_95 - initEq) / initEq, 2) * 100, "%", "\n")
cat("Monte Carlo CVaR (95%):", round(CVaR_95, 2), "; Return", round((CVaR_95 - initEq) / initEq, 2) * 100, "%", "\n")

# Correlation matrix for portfolio assets
cor_matrix <- cor(portfolio_returns, use = "pairwise.complete.obs")
View(cor_matrix)
