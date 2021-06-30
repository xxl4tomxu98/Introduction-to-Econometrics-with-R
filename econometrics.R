# atitsr.html chapter 16 VAR econometrics
# Tom Xu Guidehouse Washington, District of Columbia
# June 2021

library(AER)
library(readxl)
library(dynlm)
library(vars)
library(quantmod)
library(scales)
library(fGarch)

# load the U.S. macroeconomic data set
USMacroSWQ <- read_xlsx("us_macro_quarterly.xlsx",
                        sheet = 1,
                        col_types = c("text", rep("numeric", 9)))

# set the column names
colnames(USMacroSWQ) <- c("Date", "GDPC96", "JAPAN_IP", "PCECTPI", "GS10", 
                          "GS1", "TB3MS", "UNRATE", "EXUSUK", "CPIAUCSL")

# format the date column
USMacroSWQ$Date <- as.yearqtr(USMacroSWQ$Date, format = "%Y:0%q")

# define GDP as ts object
GDP <- ts(USMacroSWQ$GDPC96,
          start = c(1957, 1), 
          end = c(2013, 4), 
          frequency = 4)

# define GDP growth as a ts object
GDPGrowth <- ts(400*log(GDP[-1]/GDP[-length(GDP)]),
                start = c(1957, 2), 
                end = c(2013, 4), 
                frequency = 4)

# 3-months Treasury bill interest rate as a 'ts' object
TB3MS <- ts(USMacroSWQ$TB3MS,
            start = c(1957, 1), 
            end = c(2013, 4), 
            frequency = 4)

# 10-years Treasury bonds interest rate as a 'ts' object
TB10YS <- ts(USMacroSWQ$GS10, 
             start = c(1957, 1), 
             end = c(2013, 4), 
             frequency = 4)

# generate the term spread series
TSpread <- TB10YS - TB3MS

# Etimate both equations separately by OLS and use coeftest() to obtain robust standard errors
# Estimate both equations using 'dynlm()'
VAR_EQ1 <- dynlm(GDPGrowth ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2), 
                 start = c(1981, 1), 
                 end = c(2012, 4))

VAR_EQ2 <- dynlm(TSpread ~ L(GDPGrowth, 1:2) + L(TSpread, 1:2),
                 start = c(1981, 1),
                 end = c(2012, 4))

# rename regressors for better readability
names(VAR_EQ1$coefficients) <- c("Intercept","Growth_t-1", 
                                 "Growth_t-2", "TSpread_t-1", "TSpread_t-2")
names(VAR_EQ2$coefficients) <- names(VAR_EQ1$coefficients)

# robust coefficient summaries
coeftest(VAR_EQ1, vcov. = sandwich)
coeftest(VAR_EQ2, vcov. = sandwich)

# set up data for estimation using `VAR()`
VAR_data <- window(ts.union(GDPGrowth, TSpread), start = c(1980, 3), end = c(2012, 4))

# estimate model coefficients using `VAR()`
VAR_est <- VAR(y = VAR_data, p = 2)
VAR_est

#VAR() returns a list of lm objects which can be passed to the usual 
#functions, for example summary() and so it is straightforward to 
#obtain model statistics for the individual equations.
# obtain the adj. R^2 from the output of 'VAR()'
summary(VAR_est$varresult$GDPGrowth)$adj.r.squared
#> [1] 0.2887223
summary(VAR_est$varresult$TSpread)$adj.r.squared
#> [1] 0.8254311

# Granger causality tests:

# test if term spread has no power in explaining GDP growth
linearHypothesis(VAR_EQ1, 
                 hypothesis.matrix = c("TSpread_t-1", "TSpread_t-2"),
                 vcov. = sandwich)

# test if GDP growth has no power in explaining term spread
linearHypothesis(VAR_EQ2, 
                 hypothesis.matrix = c("Growth_t-1", "Growth_t-2"),
                 vcov. = sandwich)
#Both Granger causality tests reject at the level of 5%. This is evidence 
#in favor of the conjecture that the term spread has power in explaining 
#GDP growth and vice versa.

# compute iterated forecasts for GDP growth and term spread for the next 10 quarters
# that is Q1 of 2015 and h = 10 is used in predict()
forecasts <- predict(VAR_est)
forecasts

# visualize the iterated forecasts
par("mar")
par(mar=c(1,1,1,1))
plot(forecasts)

# estimate models for direct two-quarter-ahead forecasts
VAR_EQ1_direct <- dynlm(GDPGrowth ~ L(GDPGrowth, 2:3) + L(TSpread, 2:3), 
                        start = c(1981, 1), end = c(2012, 4))

VAR_EQ2_direct <- dynlm(TSpread ~ L(GDPGrowth, 2:3) + L(TSpread, 2:3), 
                        start = c(1981, 1), end = c(2012, 4))

# compute direct two-quarter-ahead forecasts
coef(VAR_EQ1_direct) %*% c(1, # intercept
                           window(GDPGrowth, start = c(2012, 3), end = c(2012, 4)), 
                           window(TSpread, start = c(2012, 3), end = c(2012, 4)))
#>          [,1]
#> [1,] 2.439497

coef(VAR_EQ2_direct) %*% c(1, # intercept
                           window(GDPGrowth, start = c(2012, 3), end = c(2012, 4)), 
                           window(TSpread, start = c(2012, 3), end = c(2012, 4)))
#>         [,1]
#> [1,] 1.66578

#Y(t)~I(0,1,2) if Y(t) itself, 1st diff, or 2nd diff is stationary
# define ts object of the U.S. PCE Price Index
PCECTPI <- ts(log(USMacroSWQ$PCECTPI), 
              start = c(1957, 1), 
              end = c(2012, 4), 
              freq = 4)

# plot logarithm of the PCE Price Index
plot(log(PCECTPI),
     main = "Log of United States PCE Price Index",
     ylab = "Logarithm",
     col = "steelblue", 
     lwd = 2)

#The logarithm of the price level has a smoothly varying trend. This is typical for an  
#I(2) series. If the price level is indeed I(2), the first differences of this 
#series should be I(1). Since we are considering the logarithm of the price level,
#we obtain growth rates by taking first differences.
# plot U.S. PCE price inflation
plot(400 * Delt(PCECTPI),
     main = "United States PCE Price Index",
     ylab = "Percent per annum",
     col = "steelblue", 
     lwd = 2)

# add a dashed line at y =  0 
abline(0, 0, lty = 2)

# DF-GLS test for unit root in GDP
summary(ur.ers(log(window(GDP, start = c(1962, 1), end = c(2012, 4))),
               model = "trend", 
               lag.max = 2))

# plot both interest series
plot(merge(as.zoo(TB3MS), as.zoo(TB10YS)), 
     plot.type = "single", 
     lty = c(2, 1),
     lwd = 2,
     xlab = "Date",
     ylab = "Percent per annum",
     ylim = c(-5, 17),
     main = "Interest Rates")

# add the term spread series
lines(as.zoo(TSpread), 
      col = "steelblue",
      lwd = 2,
      xlab = "Date",
      ylab = "Percent per annum",
      main = "Term Spread")

# shade the term spread
polygon(c(time(TB3MS), rev(time(TB3MS))), 
        c(TB10YS, rev(TB3MS)),
        col = alpha("steelblue", alpha = 0.3),
        border = NA)

# add horizontal line add 0
abline(0, 0)

# add a legend
legend("topright", 
       legend = c("TB3MS", "TB10YS", "Term Spread"),
       col = c("black", "black", "steelblue"),
       lwd = c(2, 2, 2),
       lty = c(2, 1, 1))

# test for nonstationarity of 3-month treasury bills using ADF test
ur.df(window(TB3MS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test for nonstationarity of 10-years treasury bonds using ADF test
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test for nonstationarity of 3-month treasury bills using DF-GLS test
ur.ers(window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)

# test for nonstationarity of 10-years treasury bonds using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)

# test if term spread is stationairy (cointegration of interest rates) using ADF
ur.df(window(TB10YS, c(1962, 1), c(2012, 4)) - window(TB3MS, c(1962, 1), c(2012 ,4)), 
      lags = 6, 
      selectlags = "AIC", 
      type = "drift")

# test if term spread is stationairy (cointegration of interest rates) using DF-GLS test
ur.ers(window(TB10YS, c(1962, 1), c(2012, 4)) - window(TB3MS, c(1962, 1), c(2012, 4)),
       model = "constant", 
       lag.max = 6)

# estimate first-stage regression of EG-ADF test
FS_EGADF <- dynlm(window(TB10YS, c(1962, 1), c(2012, 4)) ~ window(TB3MS, c(1962, 1), c(2012, 4)))
FS_EGADF

# compute the residuals
z_hat <- resid(FS_EGADF)

# compute the ADF test statistic
ur.df(z_hat, lags = 6, type = "none", selectlags = "AIC")

#Vector Error Correction Model(VECM)
TB10YS <- window(TB10YS, c(1962, 1), c(2012 ,4))
TB3MS <- window(TB3MS, c(1962, 1), c(2012, 4))

# set up error correction term
VECM_ECT <- TB10YS - TB3MS

# estimate both equations of the VECM using 'dynlm()'
VECM_EQ1 <- dynlm(d(TB10YS) ~ L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2) + L(VECM_ECT))
VECM_EQ2 <- dynlm(d(TB3MS) ~ L(d(TB3MS), 1:2) + L(d(TB10YS), 1:2) + L(VECM_ECT))

# rename regressors for better readability
names(VECM_EQ1$coefficients) <- c("Intercept", "D_TB3MS_l1", "D_TB3MS_l2",
                                  "D_TB10YS_l1", "D_TB10YS_l2", "ect_l1")
names(VECM_EQ2$coefficients) <- names(VECM_EQ1$coefficients)

# coefficient summaries using HAC standard errors
coeftest(VECM_EQ1, vcov. = NeweyWest(VECM_EQ1, prewhite = F, adjust = T))
coeftest(VECM_EQ2, vcov. = NeweyWest(VECM_EQ2, prewhite = F, adjust = T))

#autoregressive conditional heteroskedasticity (ARCH) Model
# import data on the Wilshire 5000 index
W5000 <- read.csv2("WILL5000INDFC.csv", 
                   stringsAsFactors = F, 
                   header = T, 
                   sep = ",", 
                   na.strings = ".")

# transform the columns
W5000$DATE <- as.Date(W5000$DATE, format = "%m/%d/%Y")
W5000$WILL5000INDFC <- as.numeric(W5000$WILL5000INDFC)

# remove NAs
W5000 <- na.omit(W5000)

# compute daily percentage changes
W5000_PC <- data.frame("Date" = W5000$DATE, 
                       "Value" = as.numeric(Delt(W5000$WILL5000INDFC) * 100))
W5000_PC <- na.omit(W5000_PC)

# plot percentage changes
plot(W5000_PC, 
     ylab = "Percent", 
     main = "Daily Percentage Changes",
     type="l", 
     col = "steelblue", 
     lwd = 0.5)

# add horizontal line at y = 0
abline(0, 0)

# plot sample autocorrelation of daily percentage changes
acf(W5000_PC$Value, main = "Wilshire 5000 Series")

# estimate GARCH(1,1) model of daily percentage changes
GARCH_Wilshire <- garchFit(data = W5000_PC$Value, trace = F)

# compute deviations of the percentage changes from their mean
dev_mean_W5000_PC <- W5000_PC$Value - GARCH_Wilshire@fit$coef[1]

# plot deviation of percentage changes from mean
plot(W5000_PC$Date, dev_mean_W5000_PC, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)

# add horizontal line at y = 0
abline(0, 0)

# add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] + GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

lines(W5000_PC$Date, 
      GARCH_Wilshire@fit$coef[1] - GARCH_Wilshire@sigma.t, 
      col = "darkred", 
      lwd = 0.5)
