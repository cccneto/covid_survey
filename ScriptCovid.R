
#title: "THE COVID-19 PANDEMIC DISASTER: WHAT ARE THE DETERMINANTS OF SPENDING ON PROTECTION AGAINST SARS-COV-2?"
#author "Claudiano C. Cruz Neto; Roberta Teodoro Santos; Carlos Eduardo M. Silva"

df <- read.csv('https://github.com/cccneto/covid_survey/raw/master/data-raw/database.csv', header = TRUE)

database <- df 

as.factor(database$sexo)
as.factor(database$equip_defensiv)
as.factor(database$morte_cov)
as.factor(database$estcivil)
as.facotr(gender)


# changing variable names 

library(dplyr)

colnames(database)[which(names(database) == "equip_defensiv")] <- "pp.equip"
colnames(database)[which(names(database) == "lngastos")] <- "ln.expend"
colnames(database)[which(names(database) == "sexo")] <- "gender"
colnames(database)[which(names(database) == "d_raca")] <- "race"
colnames(database)[which(names(database) == "idade")] <- "age"
colnames(database)[which(names(database) == "lndeaths")] <- "ln.deaths"
colnames(database)[which(names(database) == "filho_dep")] <- "care"
colnames(database)[which(names(database) == "plano")] <- "h.insurance"
colnames(database)[which(names(database) == "morte_cov")] <- "d.contact"
colnames(database)[which(names(database) == "estcivil")] <- "married"
colnames(database)[which(names(database) == "lnrenda")] <- "ln.income"
colnames(database)[which(names(database) == "lnconfirmados")] <- "ln.cases"
colnames(database)[which(names(database) == "deaths")] <- "deaths"
colnames(database)[which(names(database) == "school")] <- "school"
colnames(database)[which(names(database) == "income")] <- "income"
colnames(database)[which(names(database) == "qtd_filho_dep")] <- "n.care"


library(dplyr)

database2 <- database %>% select(pp.equip, gender, d.contact, married, ln.expend, 
                     age, race, ln.deaths, care, ln.income, h.insurance)

library(psych)
describeBy(database2,
           database2$pp.equip) # grouping variable

library(pastecs)
attach(database2)
scores <- cbind(pp.equip, gender, d.contact, married, ln.expend, 
              age, race, ln.deaths, care, ln.income, h.insurance)

options(scipen=100)
options(digits=2)
stat.desc(scores, nbr.vall = FALSE)


# Descriptive Statistics
library(jmv)
descriptives(
  data = database2,
  vars = c("pp.equip", "gender", "d.contact", "married", "ln.expend", 
           "age", "race", "ln.deaths", "care", "ln.income", "h.insurance"),
  #splitBy = "gender",
  freq = FALSE, hist = FALSE,
  dens = FALSE, bar = FALSE, barCounts = FALSE, box = FALSE,
  violin = FALSE, dot = FALSE, dotType = "jitter",
  missing = F, mean = TRUE, median = F, mode = FALSE,
  sum = F, sd = T, variance = FALSE, range = FALSE,
  min = TRUE, max = TRUE, se = FALSE, skew = FALSE, kurt = FALSE,
  quart = FALSE, pcEqGr = FALSE, pcNEqGr = 4,  n = TRUE,
  )


# Heckit Procedure 
library(sampleSelection)
model_heck = heckit(
  selection = pp.equip ~ gender + d.contact + married,
  outcome = ln.expend ~ gender + age + race + ln.deaths + care + ln.income + h.insurance,
  data = database,
  method = "2step"
)

nrow(database)
summary(model_heck)
# checking condictions 
database %>%  filter(pp.equip == 1 & ln.expend == 0)
database %>%  filter(pp.equip == 0 & ln.expend > 0)
database %>%  filter(age < 16)

summary(model_heck)


