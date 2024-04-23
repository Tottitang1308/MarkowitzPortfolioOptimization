#1 Download the (adjusted) price data of five stocks: AAPL, MSFT, and NVDA, and SPY.

# Load required libraries
#install.packages("quantmod")
library(quantmod)

# Define the tickers of the stocks and ETF
tickers <- c("V", "MA", "AAPL", "MSFT", "NVDA", "SMCI", "PG", "JNJ", "BRK-B", "JPM", "SPY")
# Set the start and end dates
end_date <- as.Date("2024-04-06")  # Today's date
start_date <- end_date - 36*30  # 36 months ago (roughly)

# Fetch the data
getSymbols(tickers, from = start_date, to = end_date, src = "yahoo")

# Create a data frame to store adjusted closing prices
prices <- NULL

# Extract adjusted closing prices and create a data frame
for (ticker in tickers) {
  prices <- cbind(prices, Ad(get(ticker)))
}

prices = data.frame(prices)

# Rename columns with ticker symbols
colnames(prices) <- tickers

# View the first few rows of the data
head(prices)
tail(prices)

#2 Split the dataset into a training set and a test set

# Set the number of months for training and testing
train_months <- 24
test_months <- 12

# Extract the training and test data from the prices dataframe
train_data <- prices[1:(21*train_months), ]
test_data <- prices[(21*train_months+1): nrow(prices), ]

tail(train_data)
head(test_data)

# 3. Calculate the daily returns.

# Ensure train_data and test_data are numeric matrices
train_data <- as.matrix(train_data)
test_data <- as.matrix(test_data)

# Calculate daily returns for the training set
train_returns <- diff(log(train_data), lag = 1)

# Calculate daily returns for the test set
test_returns <- diff(log(test_data), lag = 1)

# Convert the returns matrices to data frames
train_returns <- as.data.frame(train_returns)
test_returns <- as.data.frame(test_returns)

head(train_returns)
head(test_returns)

# Remove NA values resulting from the differencing operation
train_returns <- train_returns[-1, ]
test_returns <- test_returns[-1, ]

# View the first few rows of the training and test returns
head(train_returns)
head(test_returns)

# 4. Estimate the expected return and the covariance matrix from the training set.

# Estimate the expected return from the training set
expected_return <- colMeans(train_returns)

# Estimate the covariance matrix from the training set
covariance_matrix <- cov(train_returns)

# Print the estimated expected return
print(expected_return)

# Print the estimated covariance matrix
print(covariance_matrix)

mu <- expected_return[1:10]
sigma <- covariance_matrix[1:10,1:10]

mu
sigma

# 5. Construction of the optimal portfolio

# Calculate the inverse of the covariance matrix
inv_sigma <- solve(sigma)

# Construct the minimum risk portfolio
w_m <- inv_sigma %*% rep(1,10) / as.numeric( ( t(rep(1,10)) %*% inv_sigma %*% rep(1,10) ) )

mu_m <- as.numeric( t(mu) %*% w_m )
sigma2_m <- as.numeric( t(w_m) %*%  sigma %*% w_m )
sigma_m <- sqrt(sigma2_m)

w_m
mu_m
sigma2_m

# Construct the zero-covariance portfolio
w_z <- inv_sigma %*% mu - as.numeric( ( t(rep(1,10)) %*% inv_sigma %*% mu ) ) / as.numeric( ( t(rep(1,10)) %*% inv_sigma %*% rep(1,10) ) ) * inv_sigma %*% rep(1,10)

mu_z <- as.numeric( t(mu) %*% w_z )
sigma2_z <- as.numeric( t(w_z) %*%  sigma %*% w_z )

w_z
mu_z
sigma2_z

# 6. In-sample efficient frontier

#Grid for 1/gamma
grid_1_gamma <- seq(0,1,0.01)

mu_p <- mu_m + mu_z * grid_1_gamma

sigma2_p <- sigma2_m + grid_1_gamma^2 * sigma2_z

sigma_p <- sqrt( sigma2_p )

plot(sigma_p, mu_p, type="l", col=1, lwd=2, xlab="standard deviation", ylab="expected return", main="Efficient frontier", xlim = c(0.005,0.04), ylim= c(-0.0005,0.0030))

# Highlight the minimum-risk portfolio point
points(sigma_m, mu_m, col = 2, pch = 19)
text(sigma_m, mu_m, labels = "Min-risk", pos = 3, col = 2)

# Highlight the 1/N portfolio
w_N = rep(1/10,10)
mu_N = as.numeric( t(mu) %*% w_N )
sigma2_N <- as.numeric( t(w_N) %*%  sigma %*% w_N )
sigma_N <- sqrt(sigma2_N)

points(sigma_N, mu_N, col = 3, pch = 19)
text(sigma_N, mu_N, labels = "1/N", pos = 3, col = 3)


# Highlight individual stocks and SPY
for (i in 1:11){
  points(sqrt(covariance_matrix[i,i]), expected_return[i], col = 3+i, pch = 19)
  text(sqrt(covariance_matrix[i,i]), expected_return[i], labels = names(expected_return)[i], pos = 3, col = 3+i)
}

###################

# 7. Performance evaluation on training data

# Define the risk-free rate ticker (e.g., Treasury bond yield)
risk_free_ticker <- "^IRX"  # 13-week Treasury bill rate

# Fetch the risk-free rate data
getSymbols(risk_free_ticker, from = start_date, to = end_date, src = "yahoo")

# Extract the risk-free rate data
rf<- na.omit(Ad(get('IRX')))/100/252

rf_train <- rf[rownames(train_returns),]

rf_test <- rf[rownames(test_returns),]

# calculate the daily returns for 1/N
return_N_train <- as.matrix(train_returns[,1:10]) %*% w_N

#risk aversion
gamma <- 2

# calculate the daily returns for optimal portfolio
w_p <- w_m + 1/gamma * w_z
print(w_p)

return_p_train <- as.matrix(train_returns[,1:10]) %*% w_p


# Calculate performance measures

performance_train <- data.frame(mean=c(0,0,0), sd=c(0,0,0), cum_ret=c(0,0,0), alpha=c(0,0,0), beta=c(0,0,0), Sharpe=c(0,0,0), Treynor=c(0,0,0) )
rownames(performance_train) <- c('SPY','Optimal','1/N')

# calculate mean return and standard deviation

#SPY
performance_train[1,1] <- expected_return[11]
performance_train[1,2] <- sqrt(covariance_matrix[11,11])

#optimal
performance_train[2,1] <- mean(return_p_train)
performance_train[2,2] <- sd(return_p_train)

#1/N
performance_train[3,1] <- mean(return_N_train)
performance_train[3,2] <- sd(return_N_train)

# calculate cumulative return

#SPY
performance_train[1,3] = prod(as.numeric(train_returns$SPY)+1)-1

#optimal
performance_train[2,3] <- prod(return_p_train+1)-1


#1/N
performance_train[3,3] <- prod(return_N_train+1)-1

# plot accumulated value
plot(index(return_N_train), cumprod(as.numeric(train_returns$SPY)+1), type="l", col=1, lwd=2, xlab="Time", ylab="Value", ylim=c(0,5), main="Performance Evaluation over Training Data")
lines(index(return_p_train), cumprod(return_p_train+1), type="l", col=2, lwd=2)
lines(index(return_N_train), cumprod(return_N_train+1), type="l", col=3, lwd=2)
legend(x = "bottomright", legend=c("SPY", "Optimal","1/N"), lty=1, col = c(1,2,3), lwd =2) 


# calculate alpha and beta

#SPY
performance_train[1,4] <- 0
performance_train[1,5] <- 1

#optimal
lm1 <- lm( I(return_p_train - rf_train) ~ I(train_returns[,11] - rf_train) )
  
performance_train[2,4] <- summary(lm1)$coefficients[1,1]
performance_train[2,5] <- summary(lm1)$coefficients[2,1]

#1/N
lm2 <- lm( I(return_N_train - rf_train) ~ I(train_returns[,11] - rf_train) )

performance_train[3,4] <- summary(lm2)$coefficients[1,1]
performance_train[3,5] <- summary(lm2)$coefficients[2,1]

# calculate Sharpe ratio and Treynor ratio

#SPY
performance_train[1,6] <- mean(as.numeric(train_returns$SPY) - rf_train)/performance_train[1,2]
performance_train[1,7] <- mean(as.numeric(train_returns$SPY) - rf_train)/performance_train[1,5]

#optimal
performance_train[2,6] <- mean(return_p_train - rf_train)/performance_train[2,2]
performance_train[2,7] <- mean(return_p_train - rf_train)/performance_train[2,5]


#1/N
performance_train[3,6] <- mean(return_N_train - rf_train)/performance_train[3,2]
performance_train[3,7] <- mean(return_N_train - rf_train)/performance_train[3,5]

performance_train

# 8. Performance evaluation on test data

# calculate the daily returns for 1/N
return_N_test <- as.matrix(test_returns[,1:10]) %*% w_N

# calculate the daily returns for optimal portfolio
return_p_test <- as.matrix(test_returns[,1:10]) %*% w_p


# Calculate performance measures

performance_test <- data.frame(mean=c(0,0,0), sd=c(0,0,0), cum_ret=c(0,0,0), alpha=c(0,0,0), beta=c(0,0,0), Sharpe=c(0,0,0), Treynor=c(0,0,0) )
rownames(performance_test) <- c('SPY','Optimal','1/N')

# calculate mean return and standard deviation

#SPY
performance_test[1,1] <- mean(test_returns[,11])
performance_test[1,2] <- sd(test_returns[,11])

#optimal
performance_test[2,1] <- mean(return_p_test)
performance_test[2,2] <- sd(return_p_test)

#1/N
performance_test[3,1] <- mean(return_N_test)
performance_test[3,2] <- sd(return_N_test)

# calculate cumulative return

#SPY
performance_test[1,3] = prod(as.numeric(test_returns$SPY)+1)-1

#optimal
performance_test[2,3] <- prod(return_p_test+1)-1


#1/N
performance_test[3,3] <- prod(return_N_test+1)-1

# plot accumulated value
plot(index(return_N_test), cumprod(as.numeric(test_returns$SPY)+1), type="l", col=1, lwd=2, xlab="Time", ylab="Value", xlim=c(0,250), ylim=c(0,6), main="Performance Evaluation over Test Data")
lines(index(return_p_test), cumprod(return_p_test+1), type="l", col=2, lwd=2)
lines(index(return_N_test), cumprod(return_N_test+1), type="l", col=3, lwd=2)
legend(x = "bottomright", legend=c("SPY", "Optimal","1/N"), lty=1, col = c(1,2,3), lwd =2) 

# calculate alpha and beta

#SPY
performance_test[1,4] <- 0
performance_test[1,5] <- 1

#optimal
lm3 <- lm( I(return_p_test - rf_test) ~ I(test_returns[,11] - rf_test) )

performance_test[2,4] <- summary(lm3)$coefficients[1,1]
performance_test[2,5] <- summary(lm3)$coefficients[2,1]

#1/N
lm4 <- lm( I(return_N_test - rf_test) ~ I(test_returns[,11] - rf_test) )

performance_test[3,4] <- summary(lm4)$coefficients[1,1]
performance_test[3,5] <- summary(lm4)$coefficients[2,1]

# calculate Sharpe ratio and Treynor ratio

#SPY
performance_test[1,6] <- mean(as.numeric(test_returns$SPY) - rf_test)/performance_test[1,2]
performance_test[1,7] <- mean(as.numeric(test_returns$SPY) - rf_test)/performance_test[1,5]

#optimal
performance_test[2,6] <- mean(return_p_test - rf_test)/performance_test[2,2]
performance_test[2,7] <- mean(return_p_test - rf_test)/performance_test[2,5]


#1/N
performance_test[3,6] <- mean(return_N_test - rf_test)/performance_test[3,2]
performance_test[3,7] <- mean(return_N_test - rf_test)/performance_test[3,5]

performance_test

# 9. Performance evaluation of the combined portfolio on test data

# Calculate the weights for the combined portfolio
w_combined <- (w_p + w_N) / 2
w_combined

# Calculate the daily returns for the combined portfolio
return_combined_test <- as.matrix(test_returns[,1:10]) %*% w_combined
cum_return_combined_test <- cumprod(return_combined_test + 1)

# Calculate performance measures for the combined portfolio
performance_combined_test <- data.frame(mean=0, sd=0, cum_ret=0, alpha=0, beta=0, Sharpe=0, Treynor=0)

# Calculate mean return and standard deviation for the combined portfolio
performance_combined_test$mean <- mean(return_combined_test)
performance_combined_test$sd <- sd(return_combined_test)

# Calculate cumulative return for the combined portfolio
performance_combined_test$cum_ret <- prod(return_combined_test + 1) - 1

# Calculate alpha and beta for the combined portfolio using the market (SPY) as a benchmark
lm_combined_test <- lm(I(return_combined_test - rf_test) ~ I(test_returns[,11] - rf_test))
performance_combined_test$alpha <- summary(lm_combined_test)$coefficients[1,1]
performance_combined_test$beta <- summary(lm_combined_test)$coefficients[2,1]

# Calculate Sharpe ratio and Treynor ratio for the combined portfolio
performance_combined_test$Sharpe <- mean(return_combined_test - rf_test) / performance_combined_test$sd
performance_combined_test$Treynor <- mean(return_combined_test - rf_test) / performance_combined_test$beta

# Print combined portfolio performance on the test set
print(performance_combined_test)

## Plot the performance of all portfolios
plot(index(return_N_test), cumprod(as.numeric(test_returns$SPY)+1), type="l", col=1, lwd=2, xlab="Time", ylab="Value", xlim=c(0,250), ylim=c(0,6), main="Portfolio Performance Comparison on Test Data")
lines(index(return_p_test), cumprod(return_p_test+1), type="l", col=2, lwd=2)
lines(index(return_N_test), cumprod(return_N_test+1), type="l", col=3, lwd=2)
lines(index(return_combined_test), cum_return_combined_test, type="l", col=4, lwd=2)

# Update the legend to include the combined portfolio
legend(x = "bottomright", legend=c("SPY", "Optimal", "1/N", "Combined"), lty=1, col=c(1, 2, 3, 4), lwd=2)


## 10. Forecasting
library(forecast)

set.seed(20000) # For reproducibility
forecast_days <- 90
portfolio_names <- c("SPY", "Optimal", "1/N", "Combined")

# Simulate future returns using the mean and standard deviation of the training data
simulated_returns <- lapply(list(SPY = return_p_train,
                                 Optimal = return_p_train,
                                 `1/N` = return_N_train,
                                 Combined = return_combined_test), 
                            function(portfolio_return) {
                              replicate(forecast_days, 
                                        mean(portfolio_return) + 
                                          rnorm(1, mean=0, sd=sd(portfolio_return)))
                            })

# Calculate the cumulative product of 1 plus simulated returns
simulated_cum_returns <- lapply(simulated_returns, function(x) cumprod(1 + x))

# Plot the forecasted performance
plot(1:forecast_days, simulated_cum_returns$SPY, type='l', col=1, ylim=c(0, 2.5), xlab="Days", ylab="Cumulative Returns", main="90-Day Forecasted Portfolio Performance (20000 Simulations)")
lines(1:forecast_days, simulated_cum_returns$Optimal, col=2)
lines(1:forecast_days, simulated_cum_returns$`1/N`, col=3)
lines(1:forecast_days, simulated_cum_returns$Combined, col=4)
legend("bottomleft", legend=portfolio_names, col=1:4, lty=1, cex=0.8)


#The backtesting shows that the optimal portfolio had higher historical returns but also came with higher volatility. 
#The Monte Carlo simulation for future performance suggests that the combined portfolio might achieve 
#a better balance between risk and return, indicating lower volatility while still capturing some of the higher returns 
#of the optimal portfolio.

#This outcome underscores the benefits of diversification - by spreading investment across a wider range of assets, 
#might reduce the risk of significant losses without sacrificing too much potential gain. 
#However, these simulations are hypothetical and based on past data; actual future performance could differ.