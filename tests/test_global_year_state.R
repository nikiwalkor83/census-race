source("global_year_state.R")

fl_race <- readRDS(file.path("data", "fl_race_2000_2020.rds"))
fl_geo <- readRDS(file.path("data", "fl_geo.rds"))

expected_years <- as.integer(c(2000, 2010, 2020))
stopifnot(isTRUE(all.equal(sort(unique(fl_race$year)), expected_years)))

state <- new_year_state(unique(fl_race$year), selected_year = 2020)
stopifnot(state$selected_year == 2020)
stopifnot(isTRUE(all.equal(state$years, expected_years)))

state <- set_year(state, 2010)
stopifnot(state$selected_year == 2010)

state <- next_year(state, 1L)
stopifnot(state$selected_year == 2020)

state <- previous_year(state, 1L)
stopifnot(state$selected_year == 2010)

filtered <- filter_year_data(fl_race, state)
stopifnot(length(unique(filtered$year)) == 1L)
stopifnot(length(unique(filtered$GEOID)) == 67L)
stopifnot(length(unique(filtered$variable)) == 4L)

year_view <- prepare_year_view(fl_race, state, geo = fl_geo)
stopifnot(nrow(year_view) == 67 * 4)
stopifnot("GEOID" %in% names(year_view))
stopifnot("geometry" %in% names(year_view))
stopifnot(inherits(year_view, "sf"))

explore <- make_explore_florida_state(fl_race, fl_geo, unique(fl_race$year), selected_year = 2020)
stopifnot(explore$selected_year == 2020)
stopifnot(explore$ready_for_future_explore)
stopifnot(isTRUE(all.equal(explore$available_years, expected_years)))

print("global year state tests passed")
