
library(haven)

covid_full <- read_dta("C:/Users/User/Documents/covid/dados_dta/covid_full.dta")

attach(covid_full)
library(visdat)

vis_dat(covid_full)
vis_miss(covid_full)

covid <- covid_full[, -c(3, 16, 21)]


##### lembrar de drop gastos > 5000

#omitindo NA
covid_semNA <- na.omit(covid)

#base
covid_semNA

#Verifi
library(metan)
find_outliers(base_cov, var = renda, plots = TRUE)
find_outliers(base_cov, var = gasto_equip_defensivo, plots = TRUE)

#Transformando variaveis "confirmed", "deaths" e "gasto_equip_defensivo" em log.


library(sampleSelection)

heck_cov1 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = gasto_equip_defensivo ~ sexo + morte_cov + contat_cov + filho_dep + idade + d_raca + renda + d_escolaridade,
  data = base_cov,
  method = "2step"
)
summary(heck_cov1)


