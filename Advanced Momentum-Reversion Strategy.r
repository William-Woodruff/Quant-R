# Load required libraries
library(quantstrat)
library(quantmod)
library(PerformanceAnalytics)

# Clear previous environments
if (exists(".blotter", envir = .GlobalEnv)) rm(list = ls(envir = .blotter), envir = .blotter)
if (exists(".strategy", envir = .GlobalEnv)) rm(list = ls(envir = .strategy), envir = .strategy)
if (exists(".instrument", envir = .GlobalEnv)) rm(list = ls(envir = .instrument), envir = .instrument)

# Initialize environment
currency("USD")
symbols <- c("AAPL", "TSLA", "V", "WMT", "BAC", "GS") # Add desired symbols
Sys.setenv(TZ = "UTC")

initDate <- "2020-01-01"
from <- "2020-01-02"
to <- Sys.Date()


for (symbol in c(symbols, "SPY")) {
    stock(symbol, currency = "USD", multiplier = 1)
    getSymbols(symbol, from = from, to = to, src = "yahoo")
}


# Set initial parameters
initEq <- 100000 # Initial equity
orderqty <- 100 # Order quantity per trade

accountName <- "MomentumReversionAccount"
p1 <- "portfolio1"
p2 <- "portfolio2"
p3 <- "portfolio3"
p4 <- "portfolio4"
p5 <- "portfolio5"
p6 <- "portfolio6"
strategyName <- "MomentumReversionStrategy"

# Initialize portfolios and account
p <- c(p1, p2, p3, p4, p5, p6)
initPortf(p1, symbols = symbols[1], initDate = initDate)
initPortf(p2, symbols = symbols[2], initDate = initDate)
initPortf(p3, symbols = symbols[3], initDate = initDate)
initPortf(p4, symbols = symbols[4], initDate = initDate)
initPortf(p5, symbols = symbols[5], initDate = initDate)
initPortf(p6, symbols = symbols[6], initDate = initDate)
initAcct(accountName, portfolios = p, initDate = initDate, initEq = initEq)
initOrders(portfolio = p1, initDate = initDate)
initOrders(portfolio = p2, initDate = initDate)
initOrders(portfolio = p3, initDate = initDate)
initOrders(portfolio = p4, initDate = initDate)
initOrders(portfolio = p5, initDate = initDate)
initOrders(portfolio = p6, initDate = initDate)

# Define strategy
strategy(strategyName, store = TRUE)

# Add Indicators
# RSI
add.indicator(strategyName,
    name = "RSI",
    arguments = list(price = quote(Cl(mktdata)), n = 14),
    label = "RSI"
)

# MACD
add.indicator(strategyName,
    name = "MACD",
    arguments = list(x = quote(Cl(mktdata))),
    label = "MACD"
)

# Bollinger Bands
add.indicator(strategyName,
    name = "BBands",
    arguments = list(HLC = quote(HLC(mktdata)), n = 20, maType = "SMA"),
    label = "Bollinger"
)

# Add Entry Signals
# Signal 1: RSI Oversold (Long) or Overbought (Short)
add.signal(strategyName,
    name = "sigThreshold",
    arguments = list(
        column = "rsi.RSI",
        threshold = 30,
        relationship = "lt", # RSI < 30 for long
        cross = TRUE
    ),
    label = "RSI_Long"
)

add.signal(strategyName,
    name = "sigThreshold",
    arguments = list(
        column = "rsi.RSI",
        threshold = 70,
        relationship = "gt", # RSI > 70 for short
        cross = TRUE
    ),
    label = "RSI_Short"
)

# Signal 2: MACD Crossover
add.signal(strategyName,
    name = "sigCrossover",
    arguments = list(
        columns = c("macd.MACD", "signal.MACD"),
        relationship = "gt" # MACD bullish crossover
    ),
    label = "MACD_Bullish"
)

add.signal(strategyName,
    name = "sigCrossover",
    arguments = list(
        columns = c("macd.MACD", "signal.MACD"),
        relationship = "lt" # MACD bearish crossover
    ),
    label = "MACD_Bearish"
)

# Signal 3: Bollinger Bands (Price Below Lower Band or Above Upper Band)
add.signal(strategyName,
    name = "sigThreshold",
    arguments = list(
        column = "dn.Bollinger", # Lower Bollinger Band
        threshold = quote(Cl(mktdata)), # Compare against close price
        relationship = "gt", # Close < Lower Band
        cross = TRUE
    ),
    label = "BBand_Long"
)

add.signal(strategyName,
    name = "sigThreshold",
    arguments = list(
        column = "up.Bollinger", # Upper Bollinger Band
        threshold = quote(Cl(mktdata)), # Compare against close price
        relationship = "lt", # Close > Upper Band
        cross = TRUE
    ),
    label = "BBand_Short"
)

# Combine Entry Signals
add.signal(strategyName,
    name = "sigFormula",
    arguments = list(
        formula = "RSI_Long", #  & MACD_Bullish & BBand_Long Logical AND of all signals
        cross = TRUE
    ),
    label = "Entry_Signal"
)

# Combine Exit Signals
add.signal(strategyName,
    name = "sigFormula",
    arguments = list(
        formula = "RSI_Short ", # & MACD_Bearish & BBand_Short Logical AND of all signals
        cross = TRUE
    ),
    label = "Exit_Signal"
)
buylongOrderSize <- function(timestamp, orderqty, portfolio, symbol, ...) {
    idx <- which(index(mktdata) == timestamp)
    current_price <- as.numeric(Op(mktdata[idx, ]))

    if (is.na(current_price) || current_price <= 0 || funds < current_price) {
        return(0) # No order if price is invalid or insufficient funds
    }
    cur_pos <- floor(funds / current_price)
    possize <<- possize + cur_pos
    funds <<- funds - current_price * cur_pos
    return(cur_pos)
}

closelongOrderSize <- function(timestamp, orderqty, portfolio, symbol, ...) {
    idx <- which(index(mktdata) == timestamp)
    if (idx < nrow(mktdata)) {
        next_timestamp <- index(mktdata[idx + 1])
    } else {
        next_timestamp <- NA
    }

    # Get the open price of the next timestamp
    if (!is.na(next_timestamp)) {
        current_price <- as.numeric(Op(mktdata[next_timestamp, ]))
    } else {
        current_price <- NA
    }

    # Check if price is valid
    if (is.na(current_price) || current_price <= 0) {
        possize <<- 0
        return(0) # No order if price is invalid
    }
    funds <<- funds + current_price * possize
    x <- possize
    possize <<- 0
    return(x * sign(orderqty))
}

# Add Entry Rules
add.rule(strategyName,
    name = "ruleSignal",
    arguments = list(
        sigcol = "Entry_Signal",
        sigval = TRUE,
        orderqty = orderqty,
        ordertype = "market",
        orderside = "long",
        osFUN = buylongOrderSize,
        replace = FALSE
    ),
    type = "enter",
    label = "EnterLong"
)

# Add Exit Rules
add.rule(strategyName,
    name = "ruleSignal",
    arguments = list(
        sigcol = "Exit_Signal",
        sigval = TRUE,
        orderqty = -orderqty, # Close all long positions
        ordertype = "market",
        orderside = "long",
        osFUN = closelongOrderSize,
        replace = TRUE
    ),
    type = "exit",
    label = "ExitLong"
)

# Apply Strategy
for (i in 1:length(symbols)) {
    funds <- initEq / length(symbols)
    possize <- 0
    print("DONE")
    applyStrategy(strategy = strategyName, portfolios = p[i])
    updatePortf(p[i])
}


# Update portfolios and account
updateAcct(accountName)
updateEndEq(accountName)

# Print Results
for (i in p) {
    print(t(tradeStats(i)))
}


rets <- PortfReturns(Account = accountName)
quartz()
charts.PerformanceSummary(rets)

# Generate performance charts for each symbol
for (i in p) {
    quartz()
    chart.Posn(i)
}
quartz()
chart.Posn(p[length(p)])



account_summary <- getAccount(accountName)
account_equity <- account_summary$summary$End.Eq
dates <- index(account_equity)

plot(
    dates,
    as.numeric(account_equity),
    type = "l",
    col = "blue",
    lwd = 2,
    xlab = "Date",
    ylab = "Account Equity",
    main = "Account Equity Over Time"
)
grid()

print("#------------------------------- Risk & Asset Stats -------------------------------#")
rets <- PortfReturns(Account = accountName)

# Benchmark returns
benchmark_returns <- dailyReturn(Cl(SPY))

# Ensure returns are aligned
aligned_returns <- merge.xts(rets, benchmark_returns, all = FALSE)
portfolio_returns <- aligned_returns[, 1]
benchmark_returns <- aligned_returns[, 2]

# Calculate Alpha and Beta
alpha <- CAPM.alpha(portfolio_returns, benchmark_returns)
beta <- CAPM.beta(portfolio_returns, benchmark_returns)

a <- as.numeric(getAccount(accountName)$summary$End.Eq[1]) # Initial equity
b <- as.numeric(last(getAccount(accountName)$summary$End.Eq)) # Final equity

# Inputs
portfolio_return <- ((b - a) / a) # 106.87% total return over 4 years
benchmark_return <- (as.numeric(last(Cl(SPY))) - as.numeric(first(Op(SPY)))) / as.numeric(first(Op(SPY))) # 80% total return over 4 years             # Portfolio beta
risk_free_rate <- 0.03 # Annual risk-free rate (3%)
years <- 4 # Investment period in years

# Calculate annualized returns (CAGR)
portfolio_annualized <- (1 + portfolio_return)^(1 / years) - 1
benchmark_annualized <- (1 + benchmark_return)^(1 / years) - 1

# Alpha calculation
excess_benchmark_return <- benchmark_annualized - risk_free_rate
expected_portfolio_return <- risk_free_rate + beta * excess_benchmark_return
alpha <- portfolio_annualized - expected_portfolio_return

# Output results
cat("Portfolio Annualized Return: ", round(portfolio_annualized * 100, 2), "%\n")
cat("Benchmark Annualized Return: ", round(benchmark_annualized * 100, 2), "%\n")
cat("Alpha: ", round(alpha * 100, 2), "%\n")
cat("Portfolio Beta: ", round(beta, 4), "\n")

# Metrics calculations
total_return <- Return.cumulative(portfolio_returns)
annualized_return <- Return.annualized(portfolio_returns)
sharpe_ratio <- SharpeRatio.annualized(portfolio_returns)
sortino_ratio <- SortinoRatio(portfolio_returns)
max_drawdown <- maxDrawdown(portfolio_returns)
volatility <- StdDev.annualized(portfolio_returns)

# Print metrics
metrics <- data.frame(
    Metric = c(
        "Total Return", "Annualized Return", "Sharpe Ratio", "Sortino Ratio",
        "Maximum Drawdown", "Volatility", "Alpha", "Beta"
    ),
    Value = c(
        total_return, annualized_return, sharpe_ratio, sortino_ratio,
        max_drawdown, volatility, alpha, beta
    )
)
print(metrics)

print("#------------------------------- Monte Carlos Simulation -------------------------------#")

# Monte Carlo Parameters
n_simulations <- 1000 # Number of simulations
n_days <- 252 # Simulation horizon (1 year of trading days)
starting_balance <- initEq # Initial equity (same as portfolio starting equity)

# Extract portfolio returns
portfolio_returns <- PortfReturns(Account = accountName)
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
cat("Monte Carlo Mean:", round(mean(final_values), 2), "; Return", round((mean(final_values) - initEq) / initEq, 2) * 100, "%", "\n")

# Correlation matrix for portfolio assets
cor_matrix <- cor(portfolio_returns, use = "pairwise.complete.obs")
View(cor_matrix)
