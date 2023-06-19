suppressPackageStartupMessages({
    library(tidyverse)
    library(here)

    source(here("src/tools/variables.R"))
    source(here("src/tools/nowcast.R"))
})

plt_nc <- function(sub, truth, covariate = c("age_group", "location"), location = c("mean", "median")) {

    covariate <- match.arg(covariate)
    location <- match.arg(covariate)

    adapted_truth <- if(covariate == "age_group") {
        filter(truth, age_group %in% unique(sub$age_group))
    } else {
        filter(truth, location %in% unique(sub$location))
    }

    location_df <- if (location == "mean") {
        sub %>%
            filter(type == "mean") %>%
            select(-type, -quantile) %>%
            rename(central_location = value)
    } else {
        sub %>%
            filter(type == "quantile", quantile == .5) %>%
            select(-type, -quantile) %>%
            rename(central_location = value)
    }

    prediction_df <- sub %>%
        filter(quantile %in% c(.025, .25, .5, .75, .975)) %>%
        pivot_wider(-c(value, quantile), names_from = "quantile", values_from = "value") %>%
        select(-type) %>%
        inner_join(location_df) %>%
        rename(date = target_end_date, prediction = `0.5`)


    prediction_df %>%
        ggplot(aes(x = date, y = prediction)) +
        geom_ribbon(aes(ymin = `0.25`, ymax = `0.75`), alpha = .2) +
        geom_ribbon(aes(ymin = `0.025`, ymax = `0.975`), alpha = .2) +
        geom_line(aes(y = central_location), linetype = 2) +
        geom_line(aes(date, value), data = adapted_truth) +
        ylim(0, NA) +
        ylab("7-day hospitalization incidence") +
        theme_minimal()
}

mark_mondays <- function() {
    geom_vline(xintercept = floor_date(today(), "week", week_start = 1) - seq(0, length(delays_to_nowcast), 7), alpha = .3, linetype = 2)
}

# function taken from https://stackoverflow.com/questions/54438495/shift-legend-into-empty-facets-of-a-faceted-plot-in-ggplot2
# allows to shift the legend of a plot to an empty facet
shift_legend <- function(p) {

	# check if p is a valid object
	if(!"gtable" %in% class(p)){
		if("ggplot" %in% class(p)){
			gp <- ggplotGrob(p) # convert to grob
		} else {
			message("This is neither a ggplot object nor a grob generated from ggplotGrob. Returning original plot.")
			return(p)
		}
	} else {
		gp <- p
	}

	# check for unfilled facet panels
	facet.panels <- grep("^panel", gp[["layout"]][["name"]])
	empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
	empty.facet.panels <- facet.panels[empty.facet.panels]
	if(length(empty.facet.panels) == 0){
		message("There are no unfilled facet panels to shift legend into. Returning original plot.")
		return(p)
	}

	# establish extent of unfilled facet panels (including any axis cells in between)
	empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
	empty.facet.panels <- list(min(empty.facet.panels[["t"]]), min(empty.facet.panels[["l"]]),
							   max(empty.facet.panels[["b"]]), max(empty.facet.panels[["r"]]))
	names(empty.facet.panels) <- c("t", "l", "b", "r")

	# extract legend & copy over to location of unfilled facet panels
	guide.grob <- which(gp[["layout"]][["name"]] == "guide-box")
	if(length(guide.grob) == 0){
		message("There is no legend present. Returning original plot.")
		return(p)
	}
	gp <- gtable_add_grob(x = gp,
						  grobs = gp[["grobs"]][[guide.grob]],
						  t = empty.facet.panels[["t"]],
						  l = empty.facet.panels[["l"]],
						  b = empty.facet.panels[["b"]],
						  r = empty.facet.panels[["r"]],
						  name = "new-guide-box")

	# squash the original guide box's row / column (whichever applicable)
	# & empty its cell
	guide.grob <- gp[["layout"]][guide.grob, ]
	if(guide.grob[["l"]] == guide.grob[["r"]]){
		gp <- gtable_squash_cols(gp, cols = guide.grob[["l"]])
	}
	if(guide.grob[["t"]] == guide.grob[["b"]]){
		gp <- gtable_squash_rows(gp, rows = guide.grob[["t"]])
	}
	gp <- gtable_remove_grobs(gp, "guide-box")

	return(gp)
}