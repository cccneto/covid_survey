# Graficos

library(haven)
covid_graficos <- read_dta("covid_graficos.dta", encoding = 'utf-8')

library(dplyr)
covid_graficos %>%
  na.omit() %>%
  group_by(escolaridade) %>%
  arrange(desc(renda)) %>%
  head()

covid_graficos %>% 
  group_by(uf) %>% 
  summarise_all(funs(mean, max, sd), na.rm = TRUE)


library(ggplot2)
options(scipen=999)  # turn-off scientific notation like 1e+48
theme_set(theme_bw())  # pre-set the bw theme.

gg <- ggplot(covid_graficos, aes(x=confirmed, y=estimated_population_2019)) + 
  geom_point(aes(col=uf, size=confirmed_per_100k_inhabitants)) + 
  geom_smooth(method="loess", se=F) + 
  xlim(c(0, 0.1)) + 
  ylim(c(0, 500000)) + 
  labs(#subtitle="", 
       y="Population", 
       x="Confirmed Cases", 
       title="Confirmed cases Vs Population", 
       caption = "Source: research")

plot(gg)

# Cases confirmados vs Population
gd <- ggplot(covid_graficos, aes(x=confirmed, y=estimated_population_2019)) + 
  geom_point(aes(col=uf, size=confirmed_per_100k_inhabitants)) + 
  geom_smooth(method="loess", se=F) + 
  scale_x_log10() +
  scale_y_log10() +
  labs(#subtitle="", 
    y="Population", 
    x="Confirmed Cases", 
    title="Confirmed cases Vs Population", 
    caption = "Source: research")

plot(gd)


ggplot(covid_graficos, aes(x = confirmed, fill = raca)) +
  geom_density(alpha = .5) +xlab("Confirmed Cases")+
  ggtitle("Densidade - Confirmed cases")

# Plot Raça vs Quantidades de dependentes
g <- ggplot(covid_graficos, aes(raca, qtd_filho_dep))

gf <- g + geom_boxplot(varwidth=T, fill="plum") +
  scale_y_log10() +
  labs(title="Box plot", 
       subtitle="Quantidades de dependentes grouped by Race",
       caption="Source: research",
       x="Race",
       y="Number os dependents")

plot(gf)



# Renda vs Raca
h <- ggplot(covid_graficos, aes(raca, renda))

gh <- h + geom_boxplot(varwidth=T, fill="plum") +
  scale_y_log10() +
  labs(title="Box plot",
       subtitle="log Income by Race",
       caption="Source: research",
       x="Race",
       y="Income")

plot(gh)


####           VERIFICAR ESSA PARTE
library(dplyr)
library(pyramid)
library(forcats)
by_gender <- covid_graficos %>%
  group_by(sexo) %>% 
  mutate(genero = ifelse(sexo==1, "Male", "Female")) %>% 
  


#####


theme_set(theme_classic())

#preparando dados de freq
freqtable <- table(covid_graficos$uf)
dg <- as.data.frame.table(freqtable)

# Grafico Distribuição dos Respondentes por Estado
g <- ggplot(dg, aes(x = fct_rev(Var1), y = Freq))

plot()
gk <- g + geom_bar(stat="identity", width = 0.5, fill= "tomato2") + 
  labs(title="Distribution of responses by State", 
       caption="Source: Frequency of respondents from 'covid_graficos' dataset",
       x="States",
       y="Frequency") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
  
plot(gk)

# From on a categorical column variable

g <- ggplot(covid_graficos, aes(x = fct_infreq(escolaridade)))
g + geom_bar(aes(fill=raca), width = 0.6) + coord_flip() +
  theme(axis.text.x = element_text(angle= , vjust=0.7)) +
  scale_fill_brewer() +
  labs(title="Education level by race", 
       #subtitle= "", 
       caption="Source: covid_graficos dataset")

library(geobr)
library(ggplot2)
library(sf)
library(dplyr)
library(rio)

datasets <- list_geobr()

states <- read_state(code_state="all", year=2018)

# Remove plot axis
no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank())



# Plot all Brazilian states
ggplot() +
  geom_sf(data=states, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="States", size=8) +
  theme_minimal() +
  no_axis

# Download all municipalities
all_muni <- read_municipality( code_muni = "all", year= 2018)

all_muni_ba <- read_municipality(code_muni = "BA", year=2018)

# plot Bahia
ggplot() +
  geom_sf(data=all_muni_ba, fill="#2D3E50", color="#FEBF57", size=.15, show.legend = FALSE) +
  labs(subtitle="Municipalities of Bahia, 2018", size=8) +
  theme_minimal() +
  no_axis


# download Life Expectancy data
adh <- rio::import("http://atlasbrasil.org.br/2013/data/rawData/Indicadores%20Atlas%20-%20RADAR%20IDHM.xlsx", which = "Dados")

# keep only information for the year 2010 and the columns we want
adh <- subset(adh, ANO == 2014)

#  Download the sf of all Brazilian states
states <- read_state(year= 2014)

#preparando dados de freq, uf=Var1, qtd= Freq
freqtable <- table(covid_graficos$uf)
dg <- as.data.frame.table(freqtable)

# joind the databases
states1 <-left_join(states, adh, by = c("abbrev_state" = "NOME_AGREGA"))
states2 <- left_join(states, dg, by = c("abbrev_state" = "Var1"))
states3 <- left_join(all_muni_ba, covid_graficos, by = c("code_muni"))

#Grafico "Distribuição das Respostas por Estado"

# corrigir a enfase da cor = mais forte, mais respostas!!!!!!!!!!!!1
ggplot() +
  geom_sf(data=states2, aes(fill=Freq), color= "#ffffff", size=.15) +
  labs(subtitle="Distribuição das Respostas por Estado, 2020", size=8) +
  scale_fill_distiller(palette = "blue", name="Frequency of responses") +
  theme_light() +
  no_axis


# plot distribuição das respostas para Bahia

#preparando dados de freq, uf=Var1, qtd= Freq
freqtable1 <- table(covid_graficos$cidade)
dg <- as.data.frame.table(freqtable1)

# joind the databases
#states1 <-left_join(states, adh, by = c("abbrev_state" = "NOME_AGREGA"))
states2 <- left_join(all_muni_ba, dg, by = c("name_muni" = "Var1"))

#preparando dados de freq, uf=Var1, qtd= Freq
ggplot() +
  theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="Legend at Bottom") +
  geom_sf(data=states2, aes(fill=Freq), color = "#FFFFFF", size=.15) +
  labs(subtitle="Municipalities of Bahia, 2018", size=8) +
  #theme_minimal() +
  scale_fill_gradient2(midpoint=0, low="red", mid="white",
                       high="blue", space ="Lab") + 
  no_axis
