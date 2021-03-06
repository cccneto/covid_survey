library(tidyverse)
library(dplyr)

## updating the default plot dimensions to 12 x 6.
options(repr.plot.width = 12, repr.plot.height = 6)

# defining a custom theme here
## that mainly updates the backgrounds and legend position. We set this
## custom theme as the default, and also update the default for line size.

theme_custom <- function(base_size, ...){
  ggplot2::theme_gray(base_size = base_size, ...) +
    ggplot2::theme(
      plot.title = element_text(face = 'bold'),
      plot.subtitle = element_text(color = '#333333'),
      panel.background = element_rect(fill = "#EBF4F7"),
      strip.background = element_rect(fill = "#33AACC"),
      legend.position = "bottom"
    )
}
ggplot2::theme_set(theme_custom(base_size = 20))
ggplot2::update_geom_defaults("line", list(size = 1.5))

# Utility functions

## We will use a utility function to display the head of dataframes.
## Note that we need this hack mainly to add the class 'dataframe' to
## the tables that are printed. This should ideally be handled
## by the `repr` package, and I will be sending a PR.

display_df <- function(x){
  d <- as.character(
    knitr::kable(x, format = 'html', table.attr = "class='dataframe'")
  )
  IRdisplay::display_html(d)
}

# display_head <- function(x, n = 6){
#   display_df(head(x, n))
# }

# display_head <- function(x, n = 6){
#   display_df(dplyr::sample_n(x, n))
# }

# Number of new cases observed in a day
k = 0:69

# Arrival rate of new infections per day
lambda = c(10, 20, 30, 40)

poisson_densities = crossing(lambda = lambda, k = k) %>%
  mutate(p = dpois(k, lambda))

head(poisson_densities)

poisson_densities %>%
    # We convert lambda to a factor so that each line gets a discrete color
  mutate(lambda = as.factor(lambda)) %>%
  ggplot(aes(x = k, y = p, color = lambda)) +
  geom_line() +
  labs(
    title = expression(paste("Probability of k new cases P(k|", lambda, ")")),
    x = 'Number of new cases',
    y = NULL,
    color = expression(lambda)
  )

# Number of new cases observed in a day
k = 20

# Arrival rates of new infections per day
lambdas = seq(1, 45, length = 90)

# Compute likelihood and visualize them
tibble(lambda = lambdas, p = dpois(k, lambdas)) %>%
  ggplot(aes(x = lambda, y = p)) +
  geom_line(color = 'black') +
  labs(
    title = expression(paste("Poisson Likelihood L(", lambda, " | k"[t], ")")),
    x = expression(lambda),
    y = NULL
  )

# Before we move on, let us set up some core parameters

# r_t_range is a vector of possible values for R_t
R_T_MAX = 12
r_t_range = seq(0, R_T_MAX, length = R_T_MAX*100 + 1)

# Gamma is 1/serial interval
# https://wwwnc.cdc.gov/eid/article/26/6/20-0357_article
GAMMA = 1/4

# New cases by day
k =  c(20, 40, 55, 90)

likelihoods <- tibble(day = seq_along(k) - 1, k = k) %>%
  # Compute a vector of likelihoods
  mutate(
    r_t = list(r_t_range),
    lambda = map(dplyr::lag(k, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
    likelihood_r_t = map2(k, lambda, ~ dpois(.x, .y)/sum(dpois(.x, .y)))
  ) %>%
  # Ignore the 0th day
  filter(day > 0) %>%
  # Unnest the data to flatten it.
  select(-lambda) %>%
  unnest(c(r_t, likelihood_r_t))

head(likelihoods)



# We can now plot the likelihood conditional on the number of new cases observed.
likelihoods %>%  
  ggplot(aes(x = r_t, y = likelihood_r_t, color = as.factor(k))) +
  geom_line() +
  labs(
    title = expression(paste("Likelihood of R"[t], " given k")),
    subtitle = expression(paste("L(R"[t], "|k)")),
    x = expression("R"[t]),
    y = NULL, color = 'k'
  )

# we use the vectorized function cumprod to generate 
# posteriors for all days at once, and then normalize it.

posteriors <- likelihoods %>%
  group_by(r_t) %>%
  arrange(day) %>%
  mutate(posterior = cumprod(likelihood_r_t)) %>%
  group_by(k) %>%
  mutate(posterior = posterior / sum(posterior)) %>%
  ungroup()

head(posteriors$posterior)
unique(posteriors$posterior)

  # We can now visualize the posterior distribution.

posteriors %>%
  ggplot(aes(x = r_t, y = posterior, color = as.factor(day))) +
  geom_line() +
  labs(
    title = expression(paste("Posterior probability of R"[t], " given k")),
    subtitle = expression(paste("P(R"[t], "| k)")),
    x = expression("R"[t]), y = NULL, color = 'day'
  )

# We will now estimate the most likely value of Rt, and the highest density 
# interval surrounding it. Since the HDInterval::hdi function only works 
# with a vector of random values from a distribution, and we only 
# estimated the posterior probabilities, we will need to simulate 
# random values of Rt using the posterior 
# probabilities in order to compute the highest-density intervals.

# Install and load HDInterval package

library(HDInterval)

# Compute the most likely value of r_t and the highest-density interval
estimates <- posteriors %>%
  group_by(day) %>%
  summarize(
    r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
    r_t_most_likely = r_t_range[which.max(posterior)]
  ) %>%
  mutate(
    r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
    r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])
  ) %>%
  select(-r_t_simulated)

head(estimates)

# We can visualize these estimates and the uncertainty surrounding
# them using a line plot with ribbons.

estimates %>%
  ggplot(aes(x = day, y = r_t_most_likely)) +
  geom_point(color = "#ffc844", size = 5) +
  geom_line(color = 'black') +
  geom_ribbon(aes(ymin = r_t_lo, ymax = r_t_hi), fill = "#ffc844", alpha = 0.3) +
  labs(
    title = expression(paste('R'[t], ' by day')),
    subtitle = "The band represents the highest density interval",
    x = 'Day', y = NULL
  )

# Application to BRAZIL Data

## It is now time to take all this machinery we built and apply 
## it on COVID data from Brazil states to estimate the effective
## infection rate, Rt. Let us begin by fetching the data. We will 
## use the COVID dataset for the BRAZIL constructed by the Brasil.IO

# covid_cases <- readr::read_csv("C:/Users/User/Dropbox/Averting Expenditures Paper/rmd/Dadosbrutos/covid.rt.csv")



# retirando colunas que nao s�o de interesse
# covid_cases <- covid_cases[c("date", "state", "confirmed", "deaths", "city_ibge_code")]

library(dplyr)
library(tidyverse)

# renomeando as variaveis para ficar igual ao script
# covid_cases <- rename(covid_cases, cases = confirmed, flip = city_ibge_code)

url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
covid_cases_us <- readr::read_csv(url)

head(covid_cases_us)
# Install the smoother package

library(smoother)

#' Compute new cases and smooth them
smooth_new_cases <- function(cases){
  cases %>%
    arrange(date) %>%
    mutate(new_cases = c(cases[1], diff(cases))) %>%
    mutate(new_cases_smooth = round(
      smoother::smth(new_cases, window = 7, tails = TRUE)
    )) %>%
    select(state, date, new_cases, new_cases_smooth)
}

state_selected <- "New York"
covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  head()

head(state_selected)

plot_new_cases <- function(cases){
  cases %>%
    ggplot(aes(x = date, y = new_cases)) +
    geom_line(linetype = 'dotted', color = 'gray40') +
    geom_line(aes(y = new_cases_smooth), color = "#14243e") +
    labs(
      title = "New cases per day",
      subtitle = unique(cases$state),
      x = NULL, y = NULL
    )
}

covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  plot_new_cases()

# List columns are extremely handy when working with data. 
# The design pattern used here is very useful in many situations.

compute_likelihood <- function(cases){
  likelihood <- cases %>%
    filter(new_cases_smooth > 0) %>%
    mutate(
      r_t = list(r_t_range),
      lambda = map(lag(new_cases_smooth, 1), ~ .x * exp(GAMMA * (r_t_range - 1))),
      likelihood_r_t = map2(new_cases_smooth, lambda, dpois, log = TRUE)
    ) %>%
    slice(-1) %>%
    select(-lambda) %>%
    unnest(c(likelihood_r_t, r_t))
}

covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  head()

# We can use the rollapplyr function from the zoo package to compute 
# a rolling 7-day sum of the log likelihoods, and then exponentiate 
# it to compute the posterior. Finally, we normalize the posteriors 
# to 1.0.

compute_posterior <- function(likelihood){
  likelihood %>%
    arrange(date) %>%
    group_by(r_t) %>%
    mutate(posterior = exp(
      zoo::rollapplyr(likelihood_r_t, 7, sum, partial = TRUE)
    )) %>%
    group_by(date) %>%
    mutate(posterior = posterior / sum(posterior, na.rm = TRUE)) %>%
    # HACK: NaNs in the posterior create issues later on. So we remove them.
    mutate(posterior = ifelse(is.nan(posterior), 0, posterior)) %>%
    ungroup() %>%
    select(-likelihood_r_t)
}

covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  head()

# Let us visualize the posterior probabilities we computed. 
# Note how we set alpha = 0.2 to reduce the amount of overplotting. 
# This allows us to visualize the shifting posteriors.

plot_posteriors <- function(posteriors){
  posteriors %>%
    ggplot(aes(x = r_t, y = posterior, group = date)) +
    geom_line(alpha = 0.2) +
    labs(
      title = expression(paste("Daily Posterior of R"[t], " by day")),
      subtitle = unique(posteriors$state),
      x = '',
      y = ''
    ) +
    coord_cartesian(xlim = c(0.4, 4)) +
    theme(legend.position = 'none')
}

covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  plot_posteriors()

# Estimate Rt


# The final step is to estimate the values of Rt and the 
# highest density intervals surrounding them. Recall that we 
# need to simulate random values for rt using the posterior 
# probabilities in order to apply the HDIInterval::hdi function 
# to compute the highest density intervals.

# Estimate R_t and a 95% highest-density interval around it

estimate_rt <- function(posteriors){
  posteriors %>%
    group_by(state, date) %>%
    summarize(
      r_t_simulated = list(sample(r_t_range, 10000, replace = TRUE, prob = posterior)),
      r_t_most_likely = r_t_range[which.max(posterior)]
    ) %>%
    mutate(
      r_t_lo = map_dbl(r_t_simulated, ~ hdi(.x)[1]),
      r_t_hi = map_dbl(r_t_simulated, ~ hdi(.x)[2])
    ) %>%
    select(-r_t_simulated)
}

covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  estimate_rt() %>%
  head()

# Finally, we come to the moment of truth! Let us visualize 
# the estimated values of Rt

plot_estimates <- function(estimates){
  estimates %>%
    ggplot(aes(x = date, y = r_t_most_likely)) +
    geom_point(color = "darkorange", alpha = 0.8, size = 4) +
    geom_line(color = "#14243e") +
    geom_hline(yintercept = 1, linetype = 'dashed') +
    geom_ribbon(
      aes(ymin = r_t_lo, ymax = r_t_hi),
      fill = 'darkred',
      alpha = 0.2
    ) +
    labs(
      title = expression('Real time R'[t]), x = '', y = '',
      subtitle = unique(estimates$state)
    ) +
    coord_cartesian(ylim = c(0, 4))
}

covid_cases_us %>%
  filter(state == state_selected) %>%
  smooth_new_cases() %>%
  compute_likelihood() %>%
  compute_posterior() %>%
  estimate_rt() %>%
  plot_estimates()

# Loop across all states
# It is now time to loop across all states and compute 
# these estimates. We can do this easily by grouping by state, 
# splitting the data into one table per state, and 
# using purrr::map_df to estimate Rt for each state and 
# combine them back into a single table.

# This function can take a couple of minutes to run
#   as it loops across all states

estimates_all <- covid_cases_us %>%
  filter(date >= "2020-03-01") %>%
  group_by(state) %>%
  # Ignore states that have not reached 100 infections
  filter(max(cases) > 100 ) %>%
  group_split() %>%
  map_df(~ {
    .x %>%
      smooth_new_cases() %>%
      compute_likelihood() %>%
      compute_posterior() %>%
      estimate_rt()
  }) %>%
  ungroup()

estimates_all %>%
  head()

# We can now create a small multiples plot of the estimates 
# across all states. The use of ggplot2 makes this really easy and all 
# we had to add was an extra line of code!

# Increase plot height and width
options(repr.plot.height = 40, repr.plot.width = 20)
estimates_all %>%
  plot_estimates() +
  facet_wrap(~ state, ncol = 4) +
  labs(subtitle = "")

# Reset plot dimensions
options(repr.plot.height = 12, repr.plot.width = 8)

# Unfortunately, I was not able to install the geofacet package 
# on colab (I suspect it is because of system dependencies required
# by the sf package), and hence the code chunk below will
# fail to execute. I have included a saved version of the plot I 
# generated locally, so you can get a sense of how it looks.

# CAUTION: This code will error on colab
options(repr.plot.height = 40, repr.plot.width = 20)

estimates_all %>%
  mutate(state = state.abb[match(state, state.name)]) %>%
  plot_estimates() +
  geofacet::facet_geo(~ state, ncol = 4) +
  labs(subtitle = "") +
  theme(strip.text = element_text(hjust = 0))
 options(repr.plot.height = 12, repr.plot.width = 8)

# Finally, let us recreate the plot in Kevin's article that
# orders the states based on the most likely estimated value of Rt, 
# and colors them based on the state of lockdown.

options(repr.plot.width = 20, repr.plot.height = 8)
no_lockdown = c('North Dakota', 'South Dakota', 'Nebraska', 'Iowa', 'Arkansas')
partial_lockdown = c('Utah', 'Wyoming', 'Oklahoma')
estimates_all %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(state = forcats::fct_reorder(state, r_t_most_likely)) %>%
  mutate(lockdown = case_when(
    state %in% no_lockdown ~ 'None',
    state %in% partial_lockdown ~ 'Partial',
    TRUE ~ "Full"
  )) %>%
  ggplot(aes(x = state, y = r_t_most_likely)) +
  geom_col(aes(fill = lockdown)) +
  geom_hline(yintercept = 1, linetype = 'dotted') +
  geom_errorbar(aes(ymin = r_t_lo, ymax = r_t_hi), width = 0.2) +
  scale_fill_manual(values = c(None = 'darkred', Partial = 'gray50', Full = 'gray70')) +
  labs(
    title = expression(paste("Most Recent R"[t], " by state")),
    x = '', y = ''
  ) +
  theme(axis.text.x.bottom = element_text(angle = 90, hjust = 1, vjust = 0.5))
options(repr.plot.width = 12, repr.plot.height = 5)

