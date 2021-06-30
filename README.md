### Introduction-to-Econometrics-with-R
VAR Tests by `Christoph Hanck, Martin Arnold, Alexander Gerber, and Martin Schmelzer`
This repo includes code the following advanced topics in time series regression and demonstrates how core techniques can be applied using R:

  - Vector autoregressions (VARs). Focus on using VARs for forecasting. Another branch of the literature is concerned with so-called Structural VARs which are, however, beyond the scope of this chapter.
  - Multiperiod forecasts. This includes a discussion of iterated and direct (multivariate) forecasts.
  - The DF-GLS test, a modification of the ADF test that has more power than the latter when the series has deterministic components and is close to being nonstationarity.
  - Cointegration analysis with an application to short- and long-term interest rates. We demonstrate how to estimate a vector error correction model.
  - Autoregressive conditional heteroskedasticity (ARCH) models. We show how a simple generalized ARCH (GARCH) model can be helpful in quantifying the risk associated with investing in the stock market in terms of estimation and forecasting of the volatility of asset returns.

### To reproduce the code examples, install the R packages listed below and make sure that the subsequent code chunk executes without any errors.

  - AER (Kleiber and Zeileis 2020)
  - dynlm (Zeileis 2019)
  - fGarch (Wuertz et al. 2020)
  - quantmod (Ryan and Ulrich 2020)
  - readxl (Wickham and Bryan 2019)
  - scales (Wickham and Seidel 2020)
  - vars (Pfaff 2018)
