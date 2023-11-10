library(EpiSewer)
library(data.table)
library(cmdstanr)
library(here)
library(EpiNow2)
#remotes::install_github("epiforecasts/EpiNow2")

# Raw data ------------------------------------------

#remotes::install_github("epiforecasts/EpiNow2")
dat_raw <- fread("https://raw.githubusercontent.com/conedatascience/covid-data/master/data/timeseries/nc-cases-by-county.csv")

ww_range <- as.IDate(c("2021-07-07", "2022-02-02"))

dat_forsyth <- dat_raw[county=="Forsyth"][between(date, ww_range[1], ww_range[2])]

dat_forsyth

reported_cases <- dat_forsyth[,list(date = date, confirm = cases_daily)]

summary(reported_cases)

reporting_delay <- estimate_delay(
  rlnorm(1000, log(4), 1),
  max_value = 14, bootstraps = 1
)



estimates <- epinow(
  reported_cases = reported_cases,
  generation_time = generation_time_opts(example_generation_time),
  delays = delay_opts(example_incubation_period + reporting_delay),
  rt = rt_opts(prior = list(mean = 2, sd = 0.2)),
  stan = stan_opts(cores = 4, control = list(adapt_delta = 0.99)),
  verbose = interactive()
)


saveRDS(estimates, here::here("output", "epinow2_result.rds"))
