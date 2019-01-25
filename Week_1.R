# Advanced Research Methods 
# Class 1 

# ---------------------------- #
# ------ ASSIGNMENT 1 -------- #
# ---------------------------- #

# 1.1. load data
df = read.csv('data1.csv')

# 1.2 Create a new dependent variable that you label/name 'advtot' as the sum of consumer advertising (‘consadv’) and retailer advertising (‘retadv’).
df$advtot = df$consadv + df$retadv

# 1.3 Create a new dependent variable that you label/name 'lnsales' as the logarithm of the original sales variable ‘sales’
df$lnsales = log(df$sales)

# 1.4 Create a new variable 'lagadv' as the lagged value (or past period value) of the variable ‘advtot’.
# install.packages(DataCombine)
# library(DataCombine)
df$lagadv <- slide(df, Var = "advtot", slideBy = -1)

# 1.5 Create seasonal dummy variables for each of the four quartiles
for(counter in seq(1,12,3)){
  df[,paste0("q", ceiling(counter/3))] = ifelse(df$month %in% c(counter,counter+1,counter+2), 1, 0)
}

# 1.6 Add an observation number ‘obs’ to the dataset (i.e. obs={1,2,3,4,...,N}).
df$obs = 1:nrow(df)

# ---------------------------- #
# ------ ASSIGNMENT 2 -------- #
# ---------------------------- #

# 2.1 Start again from the permanent SAS dataset data1.sas7bdat created in the assignment 1 and create 3 sub-datasets for every year.
for (counter in 1:3){
  assign(paste0("df_y",counter), df[df$year == counter,])
}


# 2.2 Try to merge the datasets per year as follows:
df_merged = Reduce(function(x,y) merge(x,y,by='month'), list(df_y1, df_y2, df_y3))

# library(data.table)
new_columns = c("sales_y1", "consadv_y1", "retadv_y1", "sales_y2", "consadv_y2", "retadv_y2", "sales_y3", "consadv_y3", "retadv_y3")
setnames(df_merged, old=c("sales.x","consadv.x", "retadv.x", "sales.y","consadv.y", "retadv.y", "sales","consadv", "retadv"), new=new_columns)
df_merged_subset = df_merged[,c("month", new_columns)]


# ---------------------------- #
# ------ ASSIGNMENT 3 -------- #
# ---------------------------- #

# 3.1 Run a simple linear regression of sales on consumer and retailer advertising. Which type of advertising is most effective?
model = lm(sales ~ consadv + retadv, data = df)
summary(model) # since the coefficient for consadv is higher, this type of advertising is most effective.

# 3.2 Perform a hypothesis test (F-test) to see if consumer advertising and retailer advertising are jointly equal to 0.
# F-statistic 33.51, hence we can conclude consumer advertising and retailer advertising are jointly significant

# 3.3 Plot consumer advertising and sales as well as retailer advertising and sales in the same graph. Plot the residuals against the dependent variables from the regression.
plot(df$sales ~ df$retadv, col='red', main='Advertising vs Sales', pch=2, ylab = 'Sales', xlab='Advertising')
points(df$sales ~ df$consadv, col='blue')
legend(720000, 600000, legend=c("Retail", "Consumer"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


plot(residuals(model)~sales, data=df) # signs of heteroskedasticity

# 3.4 How much does the manufacturer spend on consumer and retailer advertising every month? Is this increasing over the years?
df$year = as.factor(df$year)
boxplot(consadv ~ year, data=df, main="Consumer spending by year", xlab='year', ylab='Consumer spending')
boxplot(retadv ~ year, data=df, main="Retailer spending by year", xlab='year', ylab='Retailer spending')

# 3.5 Run a non-linear power model (i.e. multiplicative or log-log model) of total advertising on sales. Plot the residuals against the dependent variables from the regression
non_linear_model = lm(lnsales ~ advtot, data = df)
summary(non_linear_model) 

plot(residuals(non_linear_model) ~ lnsales, data=df) # signs of heteroskedasticity

# ---------------------------- #
# ------ ASSIGNMENT 4 -------- #
# ---------------------------- #

# 4.1 Create a new dataset with total sales, total consumer advertising, total retailer advertising and total advertising for each month, aggregated over the past 3 years.
# library(dplyr)
grp = group_by(df, year)
df_summarised = summarise(grp, sum_sales=sum(sales), sum_consadv=sum(consadv), sum_retadv=sum(retadv), sum_advtot=sum(advtot))


# 4.2 Run again a simple linear regression of sales on consumer and retailer advertising. Do the same results still hold?
model2 = lm(sum_sales ~ sum_retadv, data=df_summarised)
summary(model2) # we don't reject the null hypothesis and thus it follows the results no longer hold.
