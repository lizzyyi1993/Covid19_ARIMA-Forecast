library("forecast")
library("tseries") 		# reqired for adf.test of stationarity
library("dplyr")

data = read.csv("Latest_United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv", sep=",",dec=".",header=T) 	# daily data
names <- names(data)
data = cbind(data[,1:2], as.numeric(gsub(",", "", data$tot_cases)), as.numeric(gsub(",", "", data$tot_death)))
colnames(data) <- names

data_NC = data %>% filter(state == "NC")
data_ND = data %>% filter(state == "ND")
data_NE = data %>% filter(state == "NE")
data_NH = data %>% filter(state == "NH")
data_NJ = data %>% filter(state == "NJ")

##### Represeniting Data as Time Series Object #####

yy.cases_NC = ts(data_NC$tot_cases, frequency = 365, start = c(2020,1))		# coverts cases data as time series object with start date and frequency (daily here)
plot.ts(yy.cases_NC)												# ALWAYS plot time series to see patterns: trend, cycle, variance over time
yy.deaths_NC = ts(data_NC$tot_death, frequency = 365, start = c(2020,1))		# coverts sales data as time series object with start date and frequency (weekly here)
plot.ts(yy.deaths_NC)

yy.cases_ND = ts(data_ND$tot_cases, frequency = 365, start = c(2020,1))		# coverts cases data as time series object with start date and frequeNDy (daily here)
plot.ts(yy.cases_ND)												# ALWAYS plot time series to see patterns: trend, cycle, variaNDe over time
yy.deaths_ND = ts(data_ND$tot_death, frequency = 365, start = c(2020,1))		# coverts sales data as time series object with start date and frequeNDy (weekly here)
plot.ts(yy.deaths_ND)

yy.cases_NE = ts(data_NE$tot_cases, frequency = 365, start = c(2020,1))		# coverts cases data as time series object with start date and frequeNEy (daily here)
plot.ts(yy.cases_NE)												# ALWAYS plot time series to see patterns: trend, cycle, variaNEe over time
yy.deaths_NE = ts(data_NE$tot_death, frequency = 365, start = c(2020,1))		# coverts sales data as time series object with start date and frequeNEy (weekly here)
plot.ts(yy.deaths_NE)

yy.cases_NH = ts(data_NH$tot_cases, frequency = 365, start = c(2020,1))		# coverts cases data as time series object with start date and frequeNHy (daily here)
plot.ts(yy.cases_NH)												# ALWAYS plot time series to see patterns: trend, cycle, variaNHe over time
yy.deaths_NH = ts(data_NH$tot_death, frequency = 365, start = c(2020,1))		# coverts sales data as time series object with start date and frequeNHy (weekly here)
plot.ts(yy.deaths_NH)

yy.cases_NJ = ts(data_NJ$tot_cases, frequency = 365, start = c(2020,1))		# coverts cases data as time series object with start date and frequeNJy (daily here)
plot.ts(yy.cases_NJ)												# ALWAYS plot time series to see patterns: trend, cycle, variaNJe over time
yy.deaths_NJ = ts(data_NJ$tot_death, frequency = 365, start = c(2020,1))		# coverts sales data as time series object with start date and frequeNJy (weekly here)
plot.ts(yy.deaths_NJ)

##### General Process for Fitting ARIMA(p,d,q) x (P, D, Q) Models #####

## Step 1. Fit ARIMA automatically
m0_cases_NC = auto.arima(yy.cases_NC)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_cases_NC)						# finds p = 1, d = 2, q = 2, MAPE = 2.261587%
m0_deaths_NC = auto.arima(yy.deaths_NC)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_deaths_NC)						# finds p = 3, d = 2, q = 4, MAPE = 1.728189%

m0_cases_ND = auto.arima(yy.cases_ND)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_cases_ND)						# finds p = 1, d = 2, q = 4, MAPE = 1.762283%
m0_deaths_ND = auto.arima(yy.deaths_ND)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_deaths_ND)						# finds p = 5, d = 2, q = 4, MAPE = 2.724228%

m0_cases_NE = auto.arima(yy.cases_NE)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_cases_NE)						# finds p = 0, d = 2, q = 3, MAPE = 2.146583%
m0_deaths_NE = auto.arima(yy.deaths_NE)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_deaths_NE)						# finds p = 1, d = 2, q = 2, MAPE = 2.057239%

m0_cases_NH = auto.arima(yy.cases_NH)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_cases_NH)						# finds p = 0, d = 2, q = 1, MAPE = 1.908897%
m0_deaths_NH = auto.arima(yy.deaths_NH)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_deaths_NH)						# finds p = 2, d = 2, q = 2, MAPE = 1.912377%

m0_cases_NJ = auto.arima(yy.cases_NJ)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_cases_NJ)						# finds p = 1, d = 2, q = 2, MAPE = 2.539577%
m0_deaths_NJ = auto.arima(yy.deaths_NJ)		# fits ARIMA(p,d,q) x (P, D, Q) automatically
summary(m0_deaths_NJ)						# finds p = 0, d = 2, q = 1, MAPE = 2.18295%

# save scores from information criteria
aicc0_cases_NC <- m0_cases_NC$aicc
bic0_cases_NC <- m0_cases_NC$bic
aicc0_deaths_NC <- m0_deaths_NC$aicc
bic0_deaths_NC <- m0_deaths_NC$bic

aicc0_cases_ND <- m0_cases_ND$aicc
bic0_cases_ND <- m0_cases_ND$bic
aicc0_deaths_ND <- m0_deaths_ND$aicc
bic0_deaths_ND <- m0_deaths_ND$bic

aicc0_cases_NE <- m0_cases_NE$aicc
bic0_cases_NE <- m0_cases_NE$bic
aicc0_deaths_NE <- m0_deaths_NE$aicc
bic0_deaths_NE <- m0_deaths_NE$bic

aicc0_cases_NH <- m0_cases_NH$aicc
bic0_cases_NH <- m0_cases_NH$bic
aicc0_deaths_NH <- m0_deaths_NH$aicc
bic0_deaths_NH <- m0_deaths_NH$bic

aicc0_cases_NJ <- m0_cases_NJ$aicc
bic0_cases_NJ <- m0_cases_NJ$bic
aicc0_deaths_NJ <- m0_deaths_NJ$aicc
bic0_deaths_NJ <- m0_deaths_NJ$bic

## Step 2. Identify "good models"

## Fit ARIMA models +/- 1 in the neighborhood of p, q found from auto.arima
## Keep d fixed and change only if needed
####################### fit NC ###########################
m1_cases_NC = Arima(yy.cases_NC, order = c(0,2,2))			# d has to be increased to fix nonstationaity
aicc1_cases_NC <- m1_cases_NC$aicc
bic1_cases_NC <- m1_cases_NC$bic

m2_cases_NC = Arima(yy.cases_NC, order = c(0,2,1))			# d has to be increased to fix nonstationaity
aicc2_cases_NC <- m2_cases_NC$aicc
bic2_cases_NC <- m2_cases_NC$bic

m3_cases_NC = Arima(yy.cases_NC, order = c(1,2,3))			# d has to be increased to fix nonstationaity
aicc3_cases_NC <- m3_cases_NC$aicc
bic3_cases_NC <- m3_cases_NC$bic

m4_cases_NC = Arima(yy.cases_NC, order = c(2,2,3))			# d has to be increased to fix nonstationaity
aicc4_cases_NC <- m4_cases_NC$aicc
bic4_cases_NC <- m4_cases_NC$bic

## compare scores on informatin criteria to find competitive models
aicc.out_cases_NC = cbind(aicc0_cases_NC, aicc1_cases_NC, aicc2_cases_NC, aicc3_cases_NC, aicc4_cases_NC)
aicc.diff_cases_NC = aicc.out_cases_NC - min(aicc.out_cases_NC)			# m0 and m1 are equally competitive according to AIC_c
aicc.diff_cases_NC

bic.out_cases_NC = cbind(bic0_cases_NC, bic1_cases_NC, bic2_cases_NC, bic3_cases_NC, bic4_cases_NC)
bic.diff_cases_NC = bic.out_cases_NC - min(bic.out_cases_NC)				# m3 is competitive according to BIC
bic.diff_cases_NC
## Now check the plot and summary of m0

m0_cases_NC.predict = forecast:::forecast.Arima(m0_cases_NC, h = 30, level = c(95))
m2_cases_NC.predict = forecast:::forecast.Arima(m2_cases_NC, h = 30, level = c(95))
plot(m0_cases_NC.predict)										
summary(m0_cases_NC.predict)	
summary(m2_cases_NC.predict)	

######################################################
## Fit Death for NC ##################################
m1_deaths_NC = Arima(yy.deaths_NC, order = c(2,2,4))			# d has to be increased to fix nonstationaity
aicc1_deaths_NC <- m1_deaths_NC$aicc
bic1_deaths_NC <- m1_deaths_NC$bic

m2_deaths_NC = Arima(yy.deaths_NC, order = c(2,2,3))			# d has to be increased to fix nonstationaity
aicc2_deaths_NC <- m2_deaths_NC$aicc
bic2_deaths_NC <- m2_deaths_NC$bic

m3_deaths_NC = Arima(yy.deaths_NC, order = c(3,2,3))			# d has to be increased to fix nonstationaity
aicc3_deaths_NC <- m3_deaths_NC$aicc
bic3_deaths_NC <- m3_deaths_NC$bic
## and so on...

## compare scores on informatin criteria to find competitive models
aicc.out_deaths_NC = cbind(aicc0_deaths_NC, aicc1_deaths_NC, aicc2_deaths_NC, aicc3_deaths_NC)
aicc.diff_deaths_NC = aicc.out_deaths_NC - min(aicc.out_deaths_NC)			# m0 is competitive with m0 according to AIC_c
aicc.diff_deaths_NC

bic.out_deaths_NC = cbind(bic0_deaths_NC, bic1_deaths_NC, bic2_deaths_NC, bic3_deaths_NC)
bic.diff_deaths_NC = bic.out_deaths_NC - min(bic.out_deaths_NC)				# m0 is competitive with m0 according to BIC as well
bic.diff_deaths_NC

## Now check the plot and summary of m0
m0_deaths_NC.predict = forecast:::forecast.Arima(m0_deaths_NC, h = 30, level = c(95))
plot(m0_deaths_NC.predict)										
summary(m0_deaths_NC.predict)							

####################### fit ND ###########################
m1_cases_ND = Arima(yy.cases_ND, order = c(2,2,4))			# d has to be increased to fix nonstationaity
aicc1_cases_ND <- m1_cases_ND$aicc
bic1_cases_ND <- m1_cases_ND$bic

m2_cases_ND = Arima(yy.cases_ND, order = c(2,2,3))			# d has to be increased to fix nonstationaity
aicc2_cases_ND <- m2_cases_ND$aicc
bic2_cases_ND <- m2_cases_ND$bic

m3_cases_ND = Arima(yy.cases_ND, order = c(1,2,5))			# d has to be increased to fix nonstationaity
aicc3_cases_ND <- m3_cases_ND$aicc
bic3_cases_ND <- m3_cases_ND$bic

## compare scores on informatin criteria to find competitive models
aicc.out_cases_ND = cbind(aicc0_cases_ND, aicc1_cases_ND, aicc2_cases_ND, aicc3_cases_ND)
aicc.diff_cases_ND = aicc.out_cases_ND - min(aicc.out_cases_ND)			# m0 is competitive with m0??? according to AIC_c
aicc.diff_cases_ND

bic.out_cases_ND = cbind(bic0_cases_ND, bic1_cases_ND, bic2_cases_ND, bic3_cases_ND)
bic.diff_cases_ND = bic.out_cases_ND - min(bic.out_cases_ND)				# m0 is competitive with m0 according to BIC as well
bic.diff_cases_ND

## Now check the plot and summary of m0
m0_cases_ND.predict = forecast:::forecast.Arima(m0_cases_ND, h = 30, level = c(95))
plot(m0_cases_ND.predict)										
summary(m0_cases_ND.predict)	

######################################################
## Fit Death for ND ##################################
m1_deaths_ND = Arima(yy.deaths_ND, order = c(5,2,5))			# d has to be increased to fix nonstationaity
aicc1_deaths_ND <- m1_deaths_ND$aicc
bic1_deaths_ND <- m1_deaths_ND$bic

m2_deaths_ND = Arima(yy.deaths_ND, order = c(4,2,5))			# d has to be increased to fix nonstationaity
aicc2_deaths_ND <- m2_deaths_ND$aicc
bic2_deaths_ND <- m2_deaths_ND$bic

m3_deaths_ND = Arima(yy.deaths_ND, order = c(4,2,5))			# d has to be increased to fix nonstationaity
aicc3_deaths_ND <- m3_deaths_ND$aicc
bic3_deaths_ND <- m3_deaths_ND$bic

## compare scores on informatin criteria to find competitive models
aicc.out_deaths_ND = cbind(aicc0_deaths_ND, aicc1_deaths_ND, aicc2_deaths_ND, aicc3_deaths_ND)
aicc.diff_deaths_ND = aicc.out_deaths_ND - min(aicc.out_deaths_ND)			# m0 is competitive with m0 according to AIC_c
aicc.diff_deaths_ND 

bic.out_deaths_ND = cbind(bic0_deaths_ND, bic1_deaths_ND, bic2_deaths_ND, bic3_deaths_ND)
bic.diff_deaths_ND = bic.out_deaths_ND - min(bic.out_deaths_ND)				# m0 is competitive with m0 according to BIC as well
bic.diff_deaths_ND

## Now check the plot and summary of m0
m0_deaths_ND.predict = forecast:::forecast.Arima(m0_deaths_ND, h = 30, level = c(95))
plot(m0_deaths_ND.predict)										
summary(m0_deaths_ND.predict)	

####################### fit NE ###########################
m1_cases_NE = Arima(yy.cases_NE, order = c(1,2,3))			# d has to be increased to fix nonstationaity
aicc1_cases_NE <- m1_cases_NE$aicc
bic1_cases_NE <- m1_cases_NE$bic

m2_cases_NE = Arima(yy.cases_NE, order = c(1,2,4))			# d has to be increased to fix nonstationaity
aicc2_cases_NE <- m2_cases_NE$aicc
bic2_cases_NE <- m2_cases_NE$bic

m3_cases_NE = Arima(yy.cases_NE, order = c(0,2,4))			# d has to be increased to fix nonstationaity
aicc3_cases_NE <- m3_cases_NE$aicc
bic3_cases_NE <- m3_cases_NE$bic

## compare scores on informatin criteria to find competitive models
aicc.out_cases_NE = cbind(aicc0_cases_NE, aicc1_cases_NE, aicc2_cases_NE, aicc3_cases_NE)
aicc.diff_cases_NE = aicc.out_cases_NE - min(aicc.out_cases_NE)			# m2 is competitive with m0 according to AIC_c
aicc.diff_cases_NE

bic.out_cases_NE = cbind(bic0_cases_NE, bic1_cases_NE, bic2_cases_NE, bic3_cases_NE)
bic.diff_cases_NE = bic.out_cases_NE - min(bic.out_cases_NE)				# m2 is competitive with m0 according to BIC as well
aicc.diff_cases_NE

## m2 for AICs and m0 is competitive according to BIC so we combine these two together 
m2_cases_NE.predict = forecast:::forecast.Arima(m2_cases_NE, h = 30, level = c(95))
plot(m2_cases_NE.predict)										
summary(m2_cases_NE.predict)

######################################################
## Fit Death for NE ##################################
m1_deaths_NE = Arima(yy.deaths_NE, order = c(0,2,2))			# d has to be increased to fix nonstationaity
aicc1_deaths_NE <- m1_deaths_NE$aicc
bic1_deaths_NE <- m1_deaths_NE$bic

m2_deaths_NE = Arima(yy.deaths_NE, order = c(1,2,3))			# d has to be increased to fix nonstationaity
aicc2_deaths_NE <- m2_deaths_NE$aicc
bic2_deaths_NE <- m2_deaths_NE$bic

m3_deaths_NE = Arima(yy.deaths_NE, order = c(2,2,3))			# d has to be increased to fix nonstationaity
aicc3_deaths_NE <- m3_deaths_NE$aicc
bic3_deaths_NE <- m3_deaths_NE$bic

## compare scores on informatin criteria to find competitive models
aicc.out_deaths_NE = cbind(aicc0_deaths_NE, aicc1_deaths_NE, aicc2_deaths_NE, aicc3_deaths_NE)
aicc.diff_deaths_NE = aicc.out_deaths_NE - min(aicc.out_deaths_NE)			# m0 is competitive with m0 according to AIC_c
aicc.diff_deaths_NE

bic.out_deaths_NE = cbind(bic0_deaths_NE, bic1_deaths_NE, bic2_deaths_NE, bic3_deaths_NE)
bic.diff_deaths_NE = bic.out_deaths_NE - min(bic.out_deaths_NE)				# m0 is competitive with m0 according to BIC as well
bic.diff_deaths_NE

## Now check the plot and summary of m0
m0_deaths_NE.predict = forecast:::forecast.Arima(m0_deaths_NE, h = 30, level = c(95))
plot(m0_deaths_NE.predict)										
summary(m0_deaths_NE.predict)	

####################### fit NH ###########################
m1_cases_NH = Arima(yy.cases_NH, order = c(1,2,1))			# d has to be increased to fix nonstationaity
aicc1_cases_NH <- m1_cases_NH$aicc
bic1_cases_NH <- m1_cases_NH$bic

m2_cases_NH = Arima(yy.cases_NH, order = c(1,2,2))			# d has to be increased to fix nonstationaity
aicc2_cases_NH <- m2_cases_NH$aicc
bic2_cases_NH <- m2_cases_NH$bic

m3_cases_NH = Arima(yy.cases_NH, order = c(0,2,2))			# d has to be increased to fix nonstationaity
aicc3_cases_NH <- m3_cases_NH$aicc
bic3_cases_NH <- m3_cases_NH$bic

## compare scores on informatin criteria to find competitive models
aicc.out_cases_NH = cbind(aicc0_cases_NH, aicc1_cases_NH, aicc2_cases_NH, aicc3_cases_NH)
aicc.diff_cases_NH = aicc.out_cases_NH - min(aicc.out_cases_NH)			# m0 is competitive with m0 according to AIC_c
aicc.diff_cases_NH

bic.out_cases_NH = cbind(bic0_cases_NH, bic1_cases_NH, bic2_cases_NH, bic3_cases_NH)
bic.diff_cases_NH = bic.out_cases_NH - min(bic.out_cases_NH)				# m0 is competitive with m0 according to BIC as well
bic.diff_cases_NH

## Now check the plot and summary of m0
m0_cases_NH.predict = forecast:::forecast.Arima(m0_cases_NH, h = 30, level = c(95))
plot(m0_cases_NH.predict)										
summary(m0_cases_NH.predict)	

######################################################
## Fit Death for NH ##################################
m1_deaths_NH = Arima(yy.deaths_NH, order = c(1,2,2))			# d has to be increased to fix nonstationaity
aicc1_deaths_NH <- m1_deaths_NH$aicc
bic1_deaths_NH <- m1_deaths_NH$bic

m2_deaths_NH = Arima(yy.deaths_NH, order = c(1,2,3))			# d has to be increased to fix nonstationaity
aicc2_deaths_NH <- m2_deaths_NH$aicc
bic2_deaths_NH <- m2_deaths_NH$bic

m3_deaths_NH = Arima(yy.deaths_NH, order = c(2,2,3))			# d has to be increased to fix nonstationaity
aicc3_deaths_NH <- m3_deaths_NH$aicc
bic3_deaths_NH <- m3_deaths_NH$bic

## compare scores on informatin criteria to find competitive models
aicc.out_deaths_NH = cbind(aicc0_deaths_NH, aicc1_deaths_NH, aicc2_deaths_NH, aicc3_deaths_NH)
aicc.diff_deaths_NH = aicc.out_deaths_NH - min(aicc.out_deaths_NH)			# m3 is competitive with m3 according to AIC_c
aicc.diff_deaths_NH

bic.out_deaths_NH = cbind(bic0_deaths_NH, bic1_deaths_NH, bic2_deaths_NH, bic3_deaths_NH)
bic.diff_deaths_NH = bic.out_deaths_NH - min(bic.out_deaths_NH)				# m3 is competitive with m3 according to BIC as well
bic.diff_deaths_NH

## Now check the plot and summary of m3
m3_deaths_NH.predict = forecast:::forecast.Arima(m3_deaths_NH, h = 30, level = c(95))
plot(m3_deaths_NH.predict)										
summary(m3_deaths_NH.predict)	

####################### fit NJ ###########################
m1_cases_NJ = Arima(yy.cases_NJ, order = c(1,2,3))			# d has to be increased to fix nonstationaity
aicc1_cases_NJ <- m1_cases_NJ$aicc
bic1_cases_NJ <- m1_cases_NJ$bic

m2_cases_NJ = Arima(yy.cases_NJ, order = c(2,2,2))			# d has to be increased to fix nonstationaity
aicc2_cases_NJ <- m2_cases_NJ$aicc
bic2_cases_NJ <- m2_cases_NJ$bic

m3_cases_NJ = Arima(yy.cases_NJ, order = c(0,2,3))			# d has to be increased to fix nonstationaity
aicc3_cases_NJ <- m3_cases_NJ$aicc
bic3_cases_NJ <- m3_cases_NJ$bic

## compare scores on informatin criteria to find competitive models
aicc.out_cases_NJ = cbind(aicc0_cases_NJ, aicc1_cases_NJ, aicc2_cases_NJ, aicc3_cases_NJ)
aicc.diff_cases_NJ = aicc.out_cases_NJ - min(aicc.out_cases_NJ)			# m2 is competitive with m0 according to AIC_c
aicc.diff_cases_NJ

bic.out_cases_NJ = cbind(bic0_cases_NJ, bic1_cases_NJ, bic2_cases_NJ, bic3_cases_NJ)
bic.diff_cases_NJ = bic.out_cases_NJ - min(bic.out_cases_NJ)				# m0 is competitive with m0 according to BIC as well
bic.diff_cases_NJ

## Now check the plot and summary of m0
m0_cases_NJ.predict = forecast:::forecast.Arima(m0_cases_NJ, h = 30, level = c(95))

plot(m0_cases_NJ.predict)										
summary(m0_cases_NJ.predict)	

######################################################
## Fit Death for NJ ##################################
m1_deaths_NJ = Arima(yy.deaths_NJ, order = c(0,2,2))			# d has to be increased to fix nonstationaity
aicc1_deaths_NJ <- m1_deaths_NJ$aicc
bic1_deaths_NJ <- m1_deaths_NJ$bic

m2_deaths_NJ = Arima(yy.deaths_NJ, order = c(1,2,1))			# d has to be increased to fix nonstationaity
aicc2_deaths_NJ <- m2_deaths_NJ$aicc
bic2_deaths_NJ <- m2_deaths_NJ$bic

m3_deaths_NJ = Arima(yy.deaths_NJ, order = c(1,2,2))			# d has to be increased to fix nonstationaity
aicc3_deaths_NJ <- m3_deaths_NJ$aicc
bic3_deaths_NJ <- m3_deaths_NJ$bic

## compare scores on informatin criteria to find competitive models
aicc.out_deaths_NJ = cbind(aicc0_deaths_NJ, aicc1_deaths_NJ, aicc2_deaths_NJ, aicc3_deaths_NJ)
aicc.diff_deaths_NJ = aicc.out_deaths_NJ - min(aicc.out_deaths_NJ)			# m3 is competitive with m3 according to AIC_c
aicc.diff_deaths_NJ

bic.out_deaths_NJ = cbind(bic0_deaths_NJ, bic1_deaths_NJ, bic2_deaths_NJ, bic3_deaths_NJ)
bic.diff_deaths_NJ = bic.out_deaths_NJ - min(bic.out_deaths_NJ)				# m3 is competitive with m3 according to BIC as well
bic.diff_deaths_NJ

## Now check the plot and summary of m0
m0_deaths_NJ.predict = forecast:::forecast.Arima(m3_deaths_NJ, h = 30, level = c(95))
plot(m0_deaths_NJ.predict)										
summary(m0_deaths_NJ.predict)	

#################################################################################
# Step 3.  Consensus Forecast (aka forecasts combination).
# Because for some of the models, the best model according to AICc and BIC differ, we decide to combine the models and average the result.
# For those with consistent performance in both AICc and BIC, we will just use that for model building.

####################### predict NC ###########################
ybar0 <- m0_cases_NC.predict$mean						# auto.arima forecast
ybar1 <- m2_cases_NC.predict$mean						# m3 based forecat

ybar.avg = (ybar0 + ybar1)/2					# consensus forecast
NCC <- ybar.avg

NCD <- m0_deaths_NC.predict$mean

####################### predict ND ###########################
NDC <- m0_cases_ND.predict$mean
NDD <- m0_deaths_ND.predict$mean

####################### predict NE ###########################
NEC <- m2_cases_NE.predict$mean
NED <- m0_deaths_NE.predict$mean

####################### predict NH ###########################
NHC<-m0_cases_NH.predict$mean
NHD<-m3_deaths_NH.predict$mean

####################### predict NJ ###########################
NJC<-m0_cases_NJ.predict$mean
NJD<-m0_deaths_NJ.predict$mean

####################### Output################################
out <- cbind(NCC, NCD, NDC, NDD, NEC, NED, NHC, NHD, NJC, NJD)

write.csv(out, 'covid19_ARIMA.csv', row.names = FALSE)







