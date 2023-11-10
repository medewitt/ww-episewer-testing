library(EpiSewer)
library(data.table)
library(cmdstanr)
library(here)
library(EpiNow2)

# Raw data ------------------------------------------

# Scraped and consolidated North Carolina Wastewater Data

dat_raw <- data.table::fread("https://raw.githubusercontent.com/wf-id/ncdhhs-ww/main/output/ncww.csv")

# Add this for visualisation
dat_raw[,smooth_normalised := data.table::frollmean(sars_cov2_normalized, 3), by = "county_names"]

# Create the flow rates. Note everything in in liters (L)
dat_raw[,flow := sars_cov2_normalized*population_served/sars_cov2_raw_copiesL]

# Filter to a single county with 50 data points 

dat_one <- head(dat_raw[county_names=="Forsyth"][flow!=Inf], 50)

range(dat_one$date_new)

measurements <- dat_one[,list(date = date_new, concentration = sars_cov2_raw_copiesL)]

# Need to add in missing days
flows <- padr::pad(dat_one[,list(date = as.Date(date_new), flow)])

# Interpolate and impute the missing days
flows$flow <- as.numeric(forecast::na.interp(ts(flows$flow, frequency = 7)))


# Build the data ------------------------------------------
ww_data <- sewer_data(measurements = measurements, flows = flows)

cases <- dat_one[,list(date = date_new, cases = cases_new_cens_per10k * population_served/10000)]


# Use Adrian's defaults 
generation_dist <- get_discrete_gamma_shifted(gamma_mean = 3, gamma_sd = 2.4, maxX = 12)
incubation_dist <- get_discrete_gamma(gamma_shape = 8.5, gamma_scale = 0.4, maxX = 10)
shedding_dist <- get_discrete_gamma(gamma_shape = 0.929639, gamma_scale = 7.241397, maxX = 30)


suggest_load_per_case(
  measurements,
  cases,
  flows,
  ascertainment_prop = 1
)

load_per_case <- 1.5e+10

ww_assumptions <- sewer_assumptions(
  generation_dist = generation_dist,
  incubation_dist = incubation_dist,
  shedding_dist = shedding_dist,
  load_per_case = load_per_case
)

options(mc.cores = 4) # allow stan to use 4 cores, i.e. one for each chain
ww_result <- EpiSewer(
  data = ww_data,
  assumptions = ww_assumptions,
  infections = model_infections(R = R_estimate_rw()),
  fit_opts = set_fit_opts(sampler = sampler_stan_mcmc(iter_warmup = 1000, iter_sampling = 1000, chains = 4))
)

saveRDS(ww_result, here("output", "ww_result.rds"))


f_concentration <- plot_concentration(ww_result, measurements = measurements)

cowplot::ggsave2(plot = f_concentration, here::here("output", "concentration.pdf"), height = 8, width = 14)

f_re <- plot_R(ww_result)

cowplot::ggsave2(plot = f_re, here::here("output", "reproduction_number.pdf"), height = 8, width = 14)

