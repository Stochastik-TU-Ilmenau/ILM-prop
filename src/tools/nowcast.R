suppressPackageStartupMessages({
    library(tidyverse)
    library(lubridate)
})
# assumes that the reporting triangels tri_case, tri hosp have rownames that correspond
# to dates
point_nowcast <- function(data_date, date_to_nowcast, tri_case, tri_hosp, wk_hosp_prob) {
    delay <- data_date - date_to_nowcast

    p_total <- wk_hosp_prob %>%
        dplyr::filter(t == data_date - delay - 7 * w, d == delay) %>%
        pull(p) %>%
        sum(na.rm = T)

    curr_I <- sum(tri_case[rownames(tri_case) == date_to_nowcast,  seq(delay + 1)])
    occured_hosp <- sum(tri_hosp[rownames(tri_hosp) == date_to_nowcast, seq(delay + 1)])

    tibble(
        date = date_to_nowcast,
        pred = curr_I * sum(p_total) + occured_hosp,
        known = occured_hosp
    )
}

add_all_states <- function(df, cols, ...) {
    cols <- enquo(cols)
    group_vars <- enquos(...)

    df %>%
        group_by(!!!group_vars) %>%
        summarize(across(!!cols, sum), .groups = "drop") %>%
        mutate(state = "DE") %>%
        rbind(df) %>%
        select(!!!group_vars, state, everything()) %>%
        arrange(!!!group_vars, state)
}

add_all_ages <- function(df, cols, ...) {

    cols <- enquo(cols)
    group_vars <- enquos(...)

    df %>%
        group_by(!!!group_vars) %>%
        summarize(across(!!cols, sum), .groups = "drop") %>%
        mutate(age_group = "00+") %>%
        rbind(df) %>%
        select(!!!group_vars, age_group, everything()) %>%
        arrange(!!!group_vars, age_group)
}

weekly_reporting_triangle <- function(tri) {
    stopifnot(`Weekly reporting triangle not yet implemented for arrays` = dim(dim(tri)) == 2)
    # fill up to obtain triangle with column length divisible by 7
    tri <- cbind(tri, matrix(0, nrow = nrow(tri), ncol = (-ncol(tri)) %% 7))

    # chunk columns (delays) in groups of 7, sum over them.
    # apply shuffles the axes, so we have to transpose afterwards
    t(apply(tri, 1, function(y) colSums(matrix(y, 7))))
}

weekly_delay_probabilities <- function(I,H) {
    cum_I <- t(apply(I, 1, cumsum))[,seq(7 * n_weeks + max(delays_to_nowcast)) ]
    cum_H <- t(apply(H, 1, cumsum))[,seq(7 * n_weeks + max(delays_to_nowcast))]

    p <- function(t,d,w) {
        if (d + 1 + 7*w > ncol(cum_H)) {
            NA
        }
        else {
            # have to account for array indexing starting at 1, but d at 0
            diff(cum_H[t, (d + 1) + 7 * c(w - 1, w)]) / cum_I[t,d + 1]
        }
    }

    expand_grid(t = seq(nrow(cum_H)), d = delays_to_nowcast, w = seq(n_weeks)) %>%
        mutate(p = pmap(., p)) %>%
        unnest(p) %>%
        mutate(t = ymd(rownames(cum_H)[t]))
}

reporting_triangle <- function(delay_df, report_date, past_date, add_rollsum, ...) {
    group_vars <- enquos(...)

    report_date <- enquo(report_date)
    past_date <- enquo(past_date)

    if (add_rollsum) {
        delay_df <- delay_df %>%
            arrange(!!report_date, !!past_date, !!!group_vars) %>%
            group_by(!!report_date, !!!group_vars) %>%
            arrange(!!past_date) %>%
            mutate(value = zoo::rollsum(value, k = 7, align = "right", na.pad = T)) %>%
            ungroup() 

    }

    delay_df %>%
        mutate(delay = as.numeric(!!report_date - !!past_date)) %>%
        group_by(!!past_date, !!!group_vars) %>%
        arrange(delay) %>%
        mutate(increment = diff(c(0, value))) %>%
        # remove negative values - these are probably reporting artifacts
        mutate(increment = pmax(increment, 0)) %>%
        select(!!past_date, delay, !!!group_vars, increment) %>%
        ungroup()
}

stratified_tri <- function(tri, ...) {
    grp_vars <- enquos(...)
    tri %>% 
        select(date, delay, increment, !!!grp_vars) %>%
        group_by(!!!grp_vars) %>%
        nest() %>%
        transmute(tri = map(data, acast, formula = date ~ delay, fun.aggregate = sum, value.var = "increment")) %>%
        ungroup()
}

normal_age_groups <- c("00-04", "05-14")
use_lognorm <- function(data_date, date_to_nowcast, age_group) {
    delay <- as.numeric(data_date - date_to_nowcast)

    !(age_group %in% normal_age_groups) 
}
