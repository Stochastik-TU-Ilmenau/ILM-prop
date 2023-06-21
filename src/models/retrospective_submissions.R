suppressPackageStartupMessages({
    library(here)

    library(tidyverse)
    library(lubridate)
    library(reshape2)

    source(here("src/tools/data.R"))
    source(here("src/tools/nowcast.R"))
    source(here("src/tools/variables.R"))
})

root_dir <- here()

prediction_sd <- function(data_date, date_to_nowcast, prediction, known, age_group) {
    resid <- residuals %>%
        filter(as.numeric(data_date - date_to_nowcast) == as.numeric(!!data_date - !!date_to_nowcast)) %>%
        filter(date_to_nowcast <= !!data_date - 7 * n_weeks) %>%
        filter(age_group == !!age_group) %>%
        arrange(date_to_nowcast) %>%
        tail(28) %>%
        pull(!!(if (use_lognorm(data_date, date_to_nowcast, age_group)) "log_resid" else "resid"))

    resid <- resid[!is.nan(resid) & !is.infinite(resid) & !is.na(resid)]

    list(sd = sd(resid), n_sd = length(resid))
    # list(sd = diff(quantile(c(.5,.75), resid)) / diff(qnorm(c(.5, .75))), n_sd = length(resid))
}

prediction_interval <- function(data_date, date_to_nowcast, age_group, prediction, known, sd, ...) {
    quantiles <- c(.025, 0.1, 0.25, 0.5, 0.75, 0.9, 0.975)

    if (use_lognorm(data_date, date_to_nowcast, age_group)) {
        ret <- exp(qnorm(quantiles, mean = log(prediction - known), sd = sd)) + known
    } else {
        ret <- qnorm(quantiles, mean = prediction, sd = sd)
    }
    names(ret) <- quantiles

    ret
}


tri_case <- read_interim("rt_7day_case_age.csv") %>%
    acast(county_date ~ delay ~ age_group, value.var = "increment", fill = 0)
rep_tri_hosp_age <- read_interim("rt_7day_hosp_age.csv")
tri_hosp <- rep_tri_hosp_age %>%
    acast(case_date ~ delay ~ age_group, value.var = "increment", fill = 0, fun.aggregate = sum)

wk_hosp_delay <- read_interim("prob_weekly_hosp_delay_age.csv") %>%
    group_by(age_group) %>%
    nest()

age_names <- sort(dimnames(tri_hosp)[[3]])

retrospective_nowcasts <- expand_grid(
    data_date = seq(ymd(rownames(tri_hosp)[1]), today(), by = "1 day"),
    delay = delays_to_nowcast,
    age_index = 1:6
) %>%
    mutate(date_to_nowcast = data_date - delay) %>%
    select(-delay) %>%
    mutate(nowcast = pmap(list(data_date, date_to_nowcast, age_index), function(d, n, a) {
        point_nowcast(d, n, tri_case[, , a], tri_hosp[, , a], wk_hosp_delay$data[[a]])
    })) %>%
    unnest(c(nowcast)) %>%
    mutate(age_group = age_names[age_index]) %>%
    select(-age_index, -date)


retrospective_nowcasts <- add_all_ages(retrospective_nowcasts, c(pred, known), data_date, date_to_nowcast)
retrospective_predictions <- retrospective_nowcasts

true_hospitalizations <- rep_tri_hosp_age %>%
    filter(delay == n_weeks * 7) %>%
    select(-delay, -increment, date_to_nowcast = case_date, age_group, actual = value, -hosp_date)
true_hospitalizations <- add_all_ages(true_hospitalizations, actual, date_to_nowcast)
residuals <- true_hospitalizations %>%
    inner_join(retrospective_nowcasts) %>%
    mutate(log_resid = log(pred - known) - log(actual - known), resid = pred - actual) %>%
    select(data_date, date_to_nowcast, age_group, everything())

tri_hosp_full <- add_all_ages(select(rep_tri_hosp_age, -increment), value, hosp_date, case_date, delay) %>%
    rename(actual = value)

pred_with_sd <- retrospective_predictions %>%
    # filter(between(data_date, ymd("2021-10-01"), ymd("2021-10-05"))) %>%
    mutate(res = pmap(., prediction_sd)) %>%
    unnest_wider("res")

pred_with_pi <- pred_with_sd %>%
    select(-n_sd) %>%
    mutate(interval = pmap(., prediction_interval)) %>%
    mutate(interval = map(interval, enframe, name = "quantile")) %>%
    unnest(c(interval)) %>%
    mutate(delay = as.numeric(data_date - date_to_nowcast))

submission <- pred_with_pi %>%
    # retrospective nowcasts can have prediction intervals whose lower bounds go below
    # the actual reported hospitalizations (this is due to us assuming a normal / log-normal distribution for these)
    # thus we cap these intervals at the known values at the time
    inner_join(
        tri_hosp_full,
        by = c("data_date" = "hosp_date", "date_to_nowcast" = "case_date", "age_group", "delay")
    ) %>%
    mutate(value = pmax(value, actual)) %>%
    transmute(
        forecast_date = data_date,
        target = paste0(ifelse(delay > 0, "-", ""), delay, " day ahead inc hosp"),
        target_end_date = date_to_nowcast,
        location = "DE",
        age_group = age_group,
        type = "quantile",
        quantile = quantile,
        value = round(value),
        pathogen = "COVID-19"
    ) %>%
    distinct()

mean_submission <- pred_with_sd %>%
    mutate(delay = as.numeric(data_date - date_to_nowcast)) %>%
    # retrospective nowcasts can have prediction intervals whose lower bounds go below
    # the actual reported hospitalizations (this is due to us assuming a normal / log-normal distribution for these)
    # thus we cap these intervals at the known values at the time
    mutate(value = ifelse(use_lognorm(data_date, date_to_nowcast, age_group), known + (pred - known) * exp(sd^2 / 2), pred)) %>%
    mutate(value = pmax(value, known)) %>%
    transmute(
        forecast_date = data_date,
        target = paste0(ifelse(delay > 0, "-", ""), delay, " day ahead inc hosp"),
        target_end_date = date_to_nowcast,
        location = "DE",
        age_group = age_group,
        type = "mean",
        quantile = NA,
        value = round(value),
        pathogen = "COVID-19"
    ) %>%
    distinct()

submission %>%
    rbind(mean_submission) %>%
    write_processed(str_interp("submissions-ILM-prop.csv"))