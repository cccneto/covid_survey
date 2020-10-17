
library(scales)
library(tidyverse)
library(magrittr)
library(utf8)
library(mice)
library(VIM)
library(lattice)
library(foreign)

library(haven)
covid_full <- read_dta("covid_full.dta")

library(dplyr)
id_muni <- read_dta("covid_graficos.dta")

#remove categorical variables
covid.miss <- subset(covid_full, select = -c(escolaridade, rendafam, raca, raca_1, result_teste_1))

# checando padr?o de missing values
md.pattern(covid.miss)

# plotando distribuicao dos missing values
mice_plot <- aggr(covid.miss, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(covid.miss), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))

# realizando imputacao dos dados
imputed_Data <- mice(covid.miss, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_Data)

# m  - Refers to 5 imputed data sets
# maxit - Refers to no. of iterations taken to impute missing values
# method - Refers to method used in imputation. we used predictive mean matching.

#check imputed values
imputed_Data$imp$morte_cov

# Since there are 5 imputed data sets, you can select any using complete() function.

# get complete data ( 2nd out of 5)
completeData <- complete(imputed_Data,2)
completeData <- complete(imputed_Data,5)


# Imputation Diagnostic Checks

# The first three observation were missing information for equip_defensiv.
imputed_Data$imp$contat_cov

# Combining imp_dataset with observed data set
imputed_Data2 <- complete(imputed_Data, "long", inc = TRUE)
# Apos imputacao o dataframe passa a ter 22 columas e 6 conjunto de dados definidos na coluna 1 .imp

# We can inspect the distributions of the original and imputed data using the stripplot function that
#   is part of the lattice package.

## labels observed data in blue and imputed data in red for y1
col <- rep(c("blue", "red")[1 + as.numeric(is.na(imputed_Data$data$contat_cov))], 6)

## plots data for y1 by imputation
stripplot(contat_cov ~ .imp, data = imputed_Data2, jit = TRUE, col = col, xlab = "imputation Number")

# Regression with imputed datasets

## linear regression for each imputed data set - 5 regression are run
fitm <- with(data = imputed_Data, lm(gasto_equip_defensivo ~ morte_cov + contat_cov))
summary(fitm)

# R will estimate our regression model separately for each imputed dataset, 1 though 5.
# We then need to summarize or pool those estimates to get one overall set of parameter estimates.

## pool coefficients and standard errors across all 5 regression models
pool(fitm)


## output parameter estimates
summary(pool(fitm))




