# chamando base sem imputacoes 
covid_full <- read_dta("C:/Users/User/Dropbox/Averting Expenditures Paper/rmd/dados_rob/covid_full1.dta")

# chamando base com imputacoes
covid_imp_rob <- read_dta("C:/Users/User/Dropbox/Averting Expenditures Paper/rmd/dados_rob/covid_basedados_imputado1.dta")

as.factor(covid_imp_rob$sexo)
as.factor(covid_imp_rob$equip_defensiv)
as.factor(covid_imp_rob$morte_cov)
as.factor(covid_imp_rob$estcivil)

library(sampleSelection)

# melhor modelo ate aqui
heck_cov1 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo  + idade + d_raca +lndeaths + 
    filho_dep + lnrenda,
  data = covid_imp_rob,
  method = "2step"
)

summary(heck_cov1)

heck_cov2 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + morte_cov + contat_cov + filho_dep + 
    idade + d_raca + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov2)

heck_cov3 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + morte_cov + contat_cov + filho_dep + 
    idade + d_raca + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov3)

heck_cov4 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo  + morte_cov + estcivil + idade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov4)

heck_cov5 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + morte_cov + estcivil + contat_cov
  + filho_dep + idade + lndeaths + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov5)

heck_cov6 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + morte_cov + contat_cov + filho_dep + 
    idade + lndeaths + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov6)

heck_cov7 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + idade + lndeaths + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov7)

heck_cov8 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + plano + idade + lndeaths + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov8)

heck_cov9 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + idade + lndeaths + lnrenda + d_escolaridade + confirmed_per_100k_inhabitants,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov9)


heck_cov10 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + idade + plano + lnrenda + d_escolaridade + confirmed_per_100k_inhabitants,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov10)

heck_cov11 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + plano + idade + lndeaths + lnrenda + d_escolaridade + confirmed_per_100k_inhabitants,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov11)

heck_cov12 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + d_raca + plano + idade + lndeaths + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov12)

heck_cov13 = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + filho_dep + d_raca + plano + idade + lndeaths + lnrenda + d_escolaridade,
  data = covid_imp_rob,
  method = "2step"
)
summary(heck_cov13)

library(stargazer)
stargazer(heck_cov1, heck_cov2, heck_cov3, heck_cov4, heck_cov5, type = 'text')
stargazer(heck_cov6, heck_cov7, heck_cov8, heck_cov9, type = 'text')
stargazer(heck_cov10, heck_cov11, heck_cov12, heck_cov13, type = 'text')

# Modelo escolhido


modelo_heck = heckit(
  selection = equip_defensiv ~ sexo + morte_cov + estcivil,
  outcome = lngastos ~ sexo + idade + d_raca + lndeaths + filho_dep + lnrenda 
  + plano,
  data = covid_imp_rob,
  method = "2step"
)