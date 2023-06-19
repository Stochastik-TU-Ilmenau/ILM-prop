suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
    library(reshape2)
    library(here)

    source(here("src/tools/data.R"))
    source(here("src/tools/nowcast.R"))
    source(here("src/tools/variables.R"))
})


tri_case <- read_interim("rt_7day_case_age.csv") %>%
    acast(county_date ~ delay ~ age_group, value.var = "increment", fill = 0)
rep_tri_hosp_age <- read_interim("rt_7day_hosp_age.csv") 
tri_hosp <- rep_tri_hosp_age %>%
    acast(case_date ~ delay ~ age_group, value.var = "increment", fill = 0, fun.aggregate = sum) 

wk_hosp_delay <- read_interim("prob_weekly_hosp_delay_age.csv") %>%
    group_by(age_group) %>%
    nest()

age_names <- sort(unique(rep_tri_hosp_age$age_group))

n_resids <- 8
retrospective_nowcasts <- expand_grid(
    # need 8 residuals for forecasting
    #data_date = today() - seq(n_resids * 7 + max(delays_to_nowcast), 0) - n_weeks * 7 + max(delays_to_nowcast),
    data_date = seq(ymd(rownames(tri_hosp)[1]), today(), by = "1 day"),
    delay = delays_to_nowcast,
    age_index = 1:6
    ) %>%
    mutate(date_to_nowcast = data_date - delay) %>%
    #filter(as.numeric(today() - date_to_nowcast) >= n_weeks * 7) %>%
    #filter(as.numeric(today() - date_to_nowcast) <= n_weeks * 7 + n_resids * 7) %>%
    select(-delay) %>%
    mutate(nowcast = pmap(list(data_date, date_to_nowcast, age_index), function(d,n,a) {
        point_nowcast(d,n,tri_case[,,a], tri_hosp[,,a], wk_hosp_delay$data[[a]])
    })) %>%
    unnest(c(nowcast)) %>%  
    mutate(age_group = age_names[age_index]) %>%
    select(-age_index, -date)


true_hospitalizations <- rep_tri_hosp_age  %>%
    filter(delay == n_weeks * 7)  %>%
    select(-delay, -increment, date_to_nowcast = case_date, age_group, actual = value, -hosp_date)  

retrospective_nowcasts <-  add_all_ages(retrospective_nowcasts, c(pred, known), data_date, date_to_nowcast)
#retrospective_nowcasts <-  add_all_ages(retrospective_nowcasts, pred, data_date, date_to_nowcast)
true_hospitalizations <-  add_all_ages(true_hospitalizations, actual, date_to_nowcast)

residuals <- true_hospitalizations %>%
    inner_join(retrospective_nowcasts) %>%
    mutate(log_resid = log(pred - known) - log(actual - known), resid = pred - actual) %>%
    #mutate(log_resid = log(pred) - log(actual), resid = pred - actual) %>%
    select(data_date, date_to_nowcast, age_group, everything())

new_daily_hospitalisations <- rep_tri_hosp_age %>%
    group_by(age_group, hosp_date) %>%
    summarise(new_hosp = sum(increment)) %>%
    add_all_ages(new_hosp, hosp_date) %>%
    arrange(hosp_date, age_group) %>%
    ungroup()

write_processed(retrospective_nowcasts, "retrospective_nowcasts.csv")
write_processed(residuals, "residuals.csv")
write_processed(new_daily_hospitalisations, "daily_hospitalisations.csv")
