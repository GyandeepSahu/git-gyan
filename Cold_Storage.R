##Problem 1##
setwd("F:/GL/AssignmentProject1")
getwd()
Temp_data =read.csv("Cold_Storage_Temp_Data.csv", header = TRUE)
attach(Temp_data)

season <- Temp_data[,c("Season")]
mean(Temp_data$Season)
by(Temp_data, INDICES = Season, FUN = summary)
mean(Temp_data$Temperature)
sd(Temp_data$Temperature)

pnorm(2, mean = 2.96274, sd = 0.508589)

pnorm(4, mean = 2.96274, sd = 0.508589, lower.tail = FALSE)







##Problem 2##
setwd("F:/GL/AssignmentProject1")
getwd()
cold_storage = read.csv("Cold_Storage_Mar2018.csv", header = TRUE)
attach(cold_storage)
summary(cold_storage)


sd(cold_storage$Temperature)


Mean <- 3.974
mu <- 3.9
sd <- 0.159

Zval <- (Mean - mu)/sd

Zval
pnorm(-abs(Zval))


Pval <- pnorm(-abs(Zval))
Pval
PVal1<-(1-Pval)
PVal1






t.test(cold_storage, mu, alternative = "greater", conf.level = 0.9)



t <- (3.974 - 3.9)/0.1


xbar = 3.974
μ = 3.9
sd = 0.159
alpha = sqrt(35)
n =35
alpha

T = (xbar-μ) / (sd/alpha)
T
df = n-1
df
pvalue = pt(T,df)
pvalue
1-pvalue











