# ------------------------------------- #
# QUESTION 1 - Run an OLS regression of per capita crisp consumption on category price levels, controlling for seasonality, holidays (X-mas), and area fixed effects? What do you conclude from this output? 
# ------------------------------------- #
df = read.csv('data potato crisp_GfK Germany.csv')

# convert area column into categorical 
df$area = as.factor(df$area)
df$VOLUME_population = df$VOLUME/df$population

# to test for direct effects we prefer to choose VOLUME over SALES, since volume can stay constant while the prices goes up which in turn leads to a higher sales
model1 = lm(data = df, VOLUME_population ~ CATPRICE + SEAS1 + SEAS2 + dxmas + area)
summary(model1)

# All variables are significant except for SEAS2

# ------------------------------------- #
# QUESTION 2 - Add per capital income to the equation. Does it change any of your eaerlier conclusions? Please describe/motivate.
# ------------------------------------- #
model2 = lm(data = df, VOLUME_population ~ CATPRICE + SEAS1 + SEAS2 + dxmas + area + INCOME)
summary(model2)

# Aforementioned coefficients still stay significant at a 5% level (and keep the same direction). The new variable per capita income is not significant however. 
# The coefficients size of the significant variables do change: 
# SEAS1 (-3.4e-03 vs -3.6e-03), dxmas (2.20e-02 vs 2.16e-02), area2 (-2.644e-02 vs -2.537e-02), area3 (-5.451e-02 vs -5.372e-02), area4(-1.586e-02 vs -1.535e-02), area5(-1.250e-02 vs -1.109e-02), area6(-1.952e-02 vs -1.847e-02)

# ------------------------------------- #
# QUESTION 3 - Test for endogeneity of price in this relationship using Hausman test
# ------------------------------------- #
# create instrumental variable for price by taking the average price in other areass in the same time period
for(row in 1:length(df$CATPRICE)){
  df[row, 'instrumental_variable'] = (mean(df[(df$area != df[row, 'area']) & df$month == df[row, 'month'], 'CATPRICE']))
}

# first stage regression
model3a = lm(data=df, CATPRICE ~ SEAS1 + SEAS2 + dxmas + area + INCOME + instrumental_variable)
df$residuals_model3 = residuals(model3a)

# second stage regression
model3b = lm(data=df, VOLUME_population ~ CATPRICE + SEAS1 + SEAS2 + dxmas + area + INCOME + residuals_model3)
summary(model3b)

# Since the p-value for the residuals is 0.43 we can conclude that price is not endogeneous

# ------------------------------------- #
# QUESTION 4 - Check for the strength of this instrument what is your conclusion? 
# ------------------------------------- #
# From model 3a we can derive that the instrumental variable has a p-value < .001. Hence, we can conlude the instrument is strong. 


# ------------------------------------- #
# QUESTION 5 - What other requirements needs to be satisfied before you can conclude that this is a good instrument? Please explain this requirement briefly and describe/propose how you could evaluate this in the current example. 
# ------------------------------------- #
# In order to be considered a good instrument, it must both be strong and valid (i.e. uncorrelated with the error term). In 4 we have already confirmed the strength. 
# The validity can be assessed if the number of instruments is greater than then number of endogenous variables. Here we could argue give the theoretical argument that the instrument is valid because the customers don't observe the price in other areas.
# Additionally, a Sargan/Hansen-J-Test can be performed (although there's some criticism about this test in the scientific community)

# eventueel nog een tweede instrument meenemen om te testen

# ------------------------------------- #
# QUESTION 6 - Correct for endogeneity using the IV approach. 
# ------------------------------------- #
# calculate correct standard error
df$instrumental_variable_predicted = predict(model3a)

model6 = lm(data=df, VOLUME_population ~ CATPRICE + SEAS1 + SEAS2 + dxmas + area + INCOME + instrumental_variable_predicted)
summary(model6)

# install.packages("AER")
# library(AER)
correct_standard_errors = ivreg(data=df, VOLUME_population ~ CATPRICE + SEAS1 + SEAS2 + dxmas + area + INCOME | CATPRICE + SEAS1 + SEAS2 + dxmas + area + INCOME + instrumental_variable_predicted)
summary(correct_standard_errors)


# ------------------------------------- #
# QUESTION 7 - Correct for endogeneity using the copulas approach.
# ------------------------------------- #

# inverse CDF normalized
# sort variable (low-high)
# calculate empirical distribution function

cum_emp_dis = ecdf(sort(df$CATPRICE))
df$CATPRICE_copulas = qnorm(cum_emp_dis(df$CATPRICE))

# replace Inf
df[df$CATPRICE_copulas == 'Inf', 'CATPRICE_copulas'] = 10

model7 = lm(data=df, VOLUME_population ~ CATPRICE_copulas + SEAS1 + SEAS2 + dxmas + area + INCOME)
summary(model7)

# since CATPRICE_copulas is insignificant (at a 5% significance level) in the model we can conclude CATPRICE is exogenous
