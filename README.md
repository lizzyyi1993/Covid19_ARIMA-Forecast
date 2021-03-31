# Covid19_ARIMA-Forecast

## Objectives
The purpose of this project is to use ARIMa models to forecast COVID-19 cases and death for North Carolina, North Dakota, Nevada, New Hampshire, and New Jersey from 2/11/2021 to 3/12/2021

Steps:
1. Fit auto.arima (p,d,q) x (P,D,Q) model to the two time series on cases and deaths per state
2. Fit alternative ARIMA models that were deemed fit
3. Average the forecasts from the best model and the few good ones from the subset of alternative models

