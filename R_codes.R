# Plot the age comparison bar chart.

data_1 = c(length(subset(UnempDur, age>=16 & age<=19)$age), length(subset(UnempDur, age>=20 & age<=24)$age), length(subset(UnempDur, age>=25 & age<=34)$age), length(subset(UnempDur, age>=35 & age<=44)$age), length(subset(UnempDur, age>=45 & age<=54)$age), length(subset(UnempDur, age>=55 & age<=64)$age), length(subset(UnempDur, age>=65)$age))
data_2 = c(4695, 12496, 33426, 31807, 30099, 25454, 9818)
data_1_len = sum(data_1)
data_2_len = sum(data_2)
data_1 = data_1 / data_1_len
data_2 = data_2 / data_2_len

is.na(data_1) <- data_1 == 0
is.na(data_2) <- data_2 == 0

d <- data.frame(column1=rep(c("16-19","20-24","25-34","35-44","45-54","55-64","65+"),each=2),column2=rep(c('UnempDur','Bureau of Labor and Statistics'),7),column3=c(data_1[1],data_2[1],data_1[2],data_2[2],data_1[3],data_2[3],data_1[4],data_2[4],data_1[5],data_2[5],data_1[6],data_2[6],data_1[7],data_2[7]))
require(lattice)
barchart(column3 ~ column1, groups=column2, d, auto.key = list(columns=2), xlab='Age (yr)', ylab='Frequency')




# Plot the income comparison bar chart.

probs = c(0.1,0.25,0.5,0.75,0.9)

data_1 = (exp(1)^quantile(UnempDur$logwage, probs = probs))*2.4749*52
data_2 = c(22810, 29020, 41950, 67410, 106050)

d <- data.frame(column1=rep(c("10%","25%","50%","75%","90%"),each=2),column2=rep(c('UnempDur','Bureau of Labor and Statistics (May 2020)'),5),column3=c(data_1[1],data_2[1],data_1[2],data_2[2],data_1[3],data_2[3],data_1[4],data_2[4],data_1[5],data_2[5]))
require(lattice)
barchart(column3 ~ column1, groups=column2, d, auto.key = list(columns=2), xlab='Percentile (%)', ylab='Annual wage (2020 USD)')




# Plot the scaled income comparison bar chart.

data_1 = as.vector((exp(1)^quantile(UnempDur$logwage, probs = probs))*2.4749*52)
data_2 = c(22810, 29020, 41950, 67410, 106050)
data_1 = data_1 * mean(data_2) / mean(data_1)

d <- data.frame(column1=rep(c("10%","25%","50%","75%","90%"),each=2),column2=rep(c('UnempDur (scaled up by ~1.25)','Bureau of Labor and Statistics (May 2020)'),5),column3=c(data_1[1],data_2[1],data_1[2],data_2[2],data_1[3],data_2[3],data_1[4],data_2[4],data_1[5],data_2[5]))
require(lattice)
barchart(column3 ~ column1, groups=column2, d, auto.key = list(columns=2), xlab='Percentile (%)', ylab='Annual wage (2020 USD)')

# And use the bootstrap-based Wald test on the resulting percentiles.

data_1 <- data_1/(1e4)
data_2 <- data_2/(1e4)

B = 10000
set.seed(1)

q = t(cbind(t(data_1), t(data_2)))
p = length(data_1)

q_samples <- lapply(1:B, function(i) c(quantile(sample(q[1:p], replace = T), probs = probs),quantile(sample(q[(p+1):(2*p)], replace = T),probs = probs))) 
q_samples <- do.call(rbind, q_samples)

data_1_samples <- q_samples[,1:p]
data_2_samples <- q_samples[,(p+1):(2*p)]

I = diag(B)
one = rep(1,B)
one_one_B = one%*%t(one)/B

V_1 <- t(data_1_samples)%*%(I - one_one_B)%*%data_1_samples
V_2 <- t(data_2_samples)%*%(I - one_one_B)%*%data_2_samples
V_1 <- V_1/(B-1)
V_2 <- V_2/(B-1)
library(magic)
V <- adiag(V_1,V_2)

I = diag(p)
A = cbind(I,-I)

library(matlib)
W = t(q)%*%t(A)%*%inv(A%*%(V)%*%t(A))%*%A%*%q




# Plot the spell vs tenure histogram.

library(ggplot2)
x = UnempDur$tenure
y = UnempDur$spell
df <- data.frame(x,y)
p <- ggplot(df, aes(x,y))
h1 <- p + stat_bin2d(bins=25) + scale_fill_gradientn(colours=r) + labs(title = "UnempDur dataset", x = "Tenure (yrs)", y = "Unemployment spell (2 wk)")
h1




# Plot the spell vs eligible replacement and disregard rates.

library(ggpubr)
library(ggplot2)

x = UnempDur$reprate*100
y = UnempDur$spell
df <- data.frame(x,y)
p <- ggplot(df, aes(x,y))
h1 <- p + stat_bin2d(bins=25) + scale_fill_gradientn(colours=r) + labs(title = "UnempDur dataset", x = "Eligible replacement rate (%)", y = "Unemployment spell (2 wk)")
h1

x = UnempDur$disrate*100
y = UnempDur$spell
df <- data.frame(x,y)
p <- ggplot(df, aes(x,y))
h2 <- p + stat_bin2d(bins=25) + scale_fill_gradientn(colours=r) + labs(title = "UnempDur dataset", x = "Eligible disregard rate (%)", y = "Unemployment spell (2 wk)")
h2

ggarrange(h1, h2)




# Plot the spell vs income.

library(ggplot2)
x = (exp(1)^(UnempDur$logwage))*2.4749*52/1000
y = UnempDur$spell
df <- data.frame(x,y)
p <- ggplot(df, aes(x,y))
h1 <- p + stat_bin2d(bins=25) + scale_fill_gradientn(colours=r) + labs(title = "UnempDur dataset", x = "Annual wage at the last job (1,000-s of 2020 USD)", y = "Unemployment spell (2 wk)")
h1



# Redefine the variables as explained in section 4.1 of the paper. C is the ``death'' indicator, meaning it is 1 when the person is currently either full-time or part-time unemployed.

n = length(UnempDur$spell)

probs_ranges = c(0.1,0.25,0.5,0.75,0.9)
logwage_quantiles = as.vector(quantile(UnempDur$logwage, probs = probs_ranges))
UnempDur$I1 = integer(n)
UnempDur$I2 = integer(n)
UnempDur$I3 = integer(n)
UnempDur$I4 = integer(n)
UnempDur$I5 = integer(n)
UnempDur$I6 = integer(n)
for (i in 1:n)
{
	if (UnempDur$logwage[i]<logwage_quantiles[1])
	{
		UnempDur$I1[i] = 1
	}
	else if (UnempDur$logwage[i]<logwage_quantiles[2])
	{
		UnempDur$I2[i] = 1
	}
	else if (UnempDur$logwage[i]<logwage_quantiles[3])
	{
		UnempDur$I3[i] = 1
	}
	else if (UnempDur$logwage[i]<logwage_quantiles[4])
	{
		UnempDur$I4[i] = 1
	}
	else if (UnempDur$logwage[i]<logwage_quantiles[5])
	{
		UnempDur$I5[i] = 1
	}
	else
	{
		UnempDur$I6[i] = 1
	}
}

tenure_ranges = c(5,10,15,20)
UnempDur$T1 = integer(n)
UnempDur$T2 = integer(n)
UnempDur$T3 = integer(n)
UnempDur$T4 = integer(n)
UnempDur$T5 = integer(n)
for (i in 1:n)
{
	if (UnempDur$tenure[i]<tenure_ranges[1])
	{
		UnempDur$T1[i] = 1
	}
	else if (UnempDur$tenure[i]<tenure_ranges[2])
	{
		UnempDur$T2[i] = 1
	}
	else if (UnempDur$tenure[i]<tenure_ranges[3])
	{
		UnempDur$T3[i] = 1
	}
	else if (UnempDur$tenure[i]<tenure_ranges[4])
	{
		UnempDur$T4[i] = 1
	}
	else
	{
		UnempDur$T5[i] = 1
	}
}

age_ranges = c(25,35,45,55)
UnempDur$Y1 = integer(n)
UnempDur$Y2 = integer(n)
UnempDur$Y3 = integer(n)
UnempDur$Y4 = integer(n)
UnempDur$Y5 = integer(n)
for (i in 1:n)
{
	if (UnempDur$age[i]<age_ranges[1])
	{
		UnempDur$Y1[i] = 1
	}
	else if (UnempDur$age[i]<age_ranges[2])
	{
		UnempDur$Y2[i] = 1
	}
	else if (UnempDur$age[i]<age_ranges[3])
	{
		UnempDur$Y3[i] = 1
	}
	else if (UnempDur$age[i]<age_ranges[4])
	{
		UnempDur$Y4[i] = 1
	}
	else
	{
		UnempDur$Y5[i] = 1
	}
}

UnempDur$J = integer(n)
for (i in 1:n)
{
	if (UnempDur$censor1[i] == 1)
	{
		UnempDur$J[i] = 1
	}
}

UnempDur$C = integer(n)
for (i in 1:n)
{
	if (UnempDur$censor1[i] == 1 | UnempDur$censor2[i] == 1)
	{
		UnempDur$C[i] = 1
	}
}

UnempDur$Z1 = UnempDur$reprate 
UnempDur$Z2 = UnempDur$disrate 




# Determine the cutoff points for eligible rates. Then spawn variables Z1 and Z2 respectively.

library(survminer)
surv_cutpoint(data=UnempDur, time='spell', event='C', variables=c('Z1','Z2'))# 0.304 and 0.163 
UnempDur$Z1 = integer(n)
for (i in 1:n)
{
	if (UnempDur$reprate[i]>=0.304)
	{
		UnempDur$Z1[i] = 1
	}
}
UnempDur$Z2 = integer(n)
for (i in 1:n)
{
	if (UnempDur$disrate[i]>=0.163)
	{
		UnempDur$Z2[i] = 1
	}
}




# Fit the M1 model without the last exponential term.

library(survival)
M1 <- coxph(Surv(spell, C) ~ I1 + I2 + I3 + I4 + I5 + I6 + T1 + T2 + T3 + T4 + T5 + Y1 + Y2 + Y3 + Y4 + Y5 + J, data=UnempDur) 

# Apply the AIC to simplify the model.

library(MASS)
M1 <- stepAIC(M1) # Only I3, T1, Y1, Y2, Y3 and J are left.

# Refit the full M1 model.
M1 <- coxph(Surv(spell, C) ~ I3 + T1 + Y1 + Y2 + Y3 + J + Z1 + Z2, data=UnempDur) #Z1 is statistically insignificant.
M1 <- coxph(Surv(spell, C) ~ I3 + T1 + Y1 + Y2 + Y3 + J + Z2, data=UnempDur)




# Now fit the M2 model without the last exponential term. M2f and M2p are the fits for the full-time and the part-time employed individuals.