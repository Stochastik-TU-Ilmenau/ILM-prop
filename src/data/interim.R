suppressPackageStartupMessages({
    library(lubridate)
    library(tidyverse)
    library(reshape2)
    library(here)

    source(here("src/tools/data.R"))
    source(here("src/tools/variables.R"))
    source(here("src/tools/nowcast.R"))
})

max_weeks <- 3 * n_weeks

process_by_group <- function(hosp_df, cases_df, ...) {
    grp <- enquos(...)

    # fix +-1 data errors by substracting them from the
    # first reported value (within the grouping factor)
    # this is obviously not perfect, but most of the time
    # we are only dealing with < 10 corrections within each group
    positive_increments <- function(inc) {
        c(inc[1] + sum(inc[inc < 0]), pmax(inc[-1], 0))
    }

    tri_hosp <- hosp_df %>%
        mutate(delay = as.numeric(hosp_date - case_date)) %>%
        group_by(case_date, !!!grp) %>%
        arrange(delay) %>%
        mutate(increment = diff(c(0, value))) %>%
        mutate(increment = positive_increments(increment)) %>%
        filter(delay < max_weeks * 7, case_date >= min_date)

    tri_case <- cases_df %>%
        select(rki_date, county_date, !!!grp, value = cases) %>%
        reporting_triangle(rki_date, county_date, add_rollsum = T, !!!grp) %>%
        filter(delay < max_weeks * 7, county_date >= min_date) 

    nested_case <- tri_case %>%
        rename(date = county_date) %>%
        stratified_tri(!!!grp) %>%
        rename(tri_age = tri)


    nested_hosp <- tri_hosp %>%
        rename(date = case_date) %>%
        stratified_tri(!!!grp) %>%
        rename(tri_hosp = tri)

    hosp_delay_probs <- inner_join(nested_case, nested_hosp) %>%
        mutate(prob = map2(tri_age, tri_hosp, weekly_delay_probabilities)) %>%
        select(!!!grp, prob) %>%
        unnest(prob)

    list(
        hosp_delay_probs = hosp_delay_probs,
        tri_hosp = tri_hosp,
        tri_case = tri_case
    )
}

#### Age Groups
all_hosp_age <- read_external("all_hosp_age.csv.gz")  %>%
    select(-location)

delayed_cases_age <- read_external("delayed_cases_age.csv.gz") %>%
    mutate(age_group = str_replace_all(age_group, "A", "")) %>%
    filter(age_group != "unbekannt") %>%
    # cases have one day less than hospitalizations
    # this is because the hospitalizations associated with today
    # come from the PAST 7 days, i.e.
    # hospitalization incidence at T is the sum of days T-1, ..., T - 7
    # thus 7-day incidence of the day BEFORE T has to be chosen here
    # To deal with this, we shift the `county_date` variable by one day
    # before calculating the daily incidence
    mutate(county_date = county_date + 1)

age <- process_by_group(all_hosp_age, delayed_cases_age, age_group)

write_interim(age$hosp_delay_probs, "prob_weekly_hosp_delay_age.csv")
write_interim(age$tri_hosp, "rt_7day_hosp_age.csv")
write_interim(age$tri_case, "rt_7day_case_age.csv")