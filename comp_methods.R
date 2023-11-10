library(tidyverse)
library(data.table)

theme_set(theme_bw())

dat_ww_r <- readRDS(here::here("output", "ww_result.rds"))

dat_case_r <- readRDS(here::here("output", "epinow2_result.rds"))


dat_ww_scaled <- copy(dat_ww_r$summary$R_samples)

dat_ww_scaled <- dat_ww_scaled[,list(lower_90 = quantile(R, .05), 
                    upper_90 = quantile(R, .95)), by = "date"][
                        ,MethodDSC := "Wastewater"
                    ][,date := as.Date(date)]

d_r <- dat_case_r$estimates$summarised[variable == "R"][
    ,list(date, lower_90, upper_90)][
        ,MethodDSC := "Case Incidence (EpiNow2)"
    ][,date := as.Date(date)]

dat_graph <- rbindlist(list(d_r, dat_ww_scaled))

fig_comparison <- dat_graph |>
ggplot()+
geom_ribbon(aes(date, fill = MethodDSC, ymin = lower_90, ymax = upper_90), alpha = .4)+
geom_hline(yintercept = 1, lty = 2, lwd = 2)+
labs(
    y = "Effective Reproduction Number",
    x = NULL
)+
scale_fill_manual(name = "Rt approach",values = ggsci::pal_nejm()(2))+
theme(legend.position = "top")


cowplot::ggsave2(plot = fig_comparison, here::here("output", "reproduction_number_methods.pdf"), height = 8, width = 14)

