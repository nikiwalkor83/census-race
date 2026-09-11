# Global year state for future Florida census visualizations.
# This module is intentionally kept separate from the visible Quarto page so the
# current website appearance and behavior remain unchanged until the UI is added.

suppressPackageStartupMessages({
  library(dplyr)
})

prepare_years <- function(years) {
  if (is.null(years)) {
    stop("A census year vector must be supplied.", call. = FALSE)
  }

  years <- sort(unique(as.integer(years)))

  if (length(years) == 0L) {
    stop("At least one census year is required.", call. = FALSE)
  }

  years
}

# Create a global year state object that can be shared by all future charts,
# maps, rankings, and summaries.
new_year_state <- function(years = NULL, selected_year = NULL, autoplay = FALSE, play_speed = 1) {
  years <- prepare_years(years)

  if (is.null(selected_year)) {
    selected_year <- max(years)
  }

  selected_year <- as.integer(selected_year)

  if (!selected_year %in% years) {
    stop(
      "selected_year must be one of the available years: ",
      paste(years, collapse = ", "),
      call. = FALSE
    )
  }

  state <- new.env(parent = emptyenv())
  state$years <- years
  state$selected_year <- selected_year
  state$year_index <- match(selected_year, years)
  state$autoplay <- autoplay
  state$play_speed <- as.numeric(play_speed)

  state
}

set_year <- function(state, year) {
  if (!inherits(state, "environment")) {
    stop("state must be an environment created by new_year_state().", call. = FALSE)
  }

  year <- as.integer(year)

  if (!year %in% state$years) {
    stop(
      "Year must be in the available years: ",
      paste(state$years, collapse = ", "),
      call. = FALSE
    )
  }

  state$selected_year <- year
  state$year_index <- match(year, state$years)

  state
}

next_year <- function(state, steps = 1L) {
  if (!inherits(state, "environment")) {
    stop("state must be an environment created by new_year_state().", call. = FALSE)
  }

  steps <- as.integer(steps)
  n_years <- length(state$years)
  target_index <- ((state$year_index + steps - 1L) %% n_years) + 1L
  set_year(state, state$years[target_index])
}

previous_year <- function(state, steps = 1L) {
  if (!inherits(state, "environment")) {
    stop("state must be an environment created by new_year_state().", call. = FALSE)
  }

  steps <- as.integer(steps)
  n_years <- length(state$years)
  target_index <- ((state$year_index - steps - 1L) %% n_years) + 1L
  set_year(state, state$years[target_index])
}

get_year_label <- function(state) {
  as.character(state$selected_year)
}

# Filter any statewide Florida dataframe to the currently selected year.
filter_year_data <- function(data, state, year_col = "year") {
  if (!year_col %in% names(data)) {
    stop(sprintf("Column '%s' was not found in the data.", year_col), call. = FALSE)
  }

  dplyr::filter(data, .data[[year_col]] == state$selected_year)
}

# Prepare the data for a future map or chart using the selected year while
# retaining the broader Florida statewide geography.
prepare_year_view <- function(data, state, geo = NULL, year_col = "year", geo_col = "GEOID") {
  year_data <- filter_year_data(data, state, year_col = year_col)

  if (!is.null(geo)) {
    if (!geo_col %in% names(geo)) {
      stop(sprintf("Column '%s' was not found in the geographic data.", geo_col), call. = FALSE)
    }

    if ("NAME" %in% names(year_data)) {
      year_data <- year_data |> dplyr::select(-dplyr::any_of("NAME"))
    }

    geo_df <- sf::st_drop_geometry(geo)
    filtered_geo <- geo_df |>
      dplyr::left_join(year_data, by = geo_col)

    geo_order <- match(filtered_geo[[geo_col]], geo[[geo_col]])
    filtered_geo$geometry <- sf::st_geometry(geo)[geo_order]
    year_data <- sf::st_as_sf(filtered_geo, sf_column_name = "geometry", crs = sf::st_crs(geo))
  }

  year_data
}

# Future-ready scaffolding for an interactive Explore Florida section.
make_explore_florida_state <- function(data, geo = NULL, years = NULL, selected_year = NULL) {
  if (is.null(years)) {
    years <- sort(unique(data$year))
  }

  state <- new_year_state(years = years, selected_year = selected_year)

  list(
    year_state = state,
    available_years = state$years,
    selected_year = state$selected_year,
    data = data,
    geo = geo,
    variables = if ("variable" %in% names(data)) unique(data$variable) else NULL,
    statewide_geography = geo,
    ready_for_future_explore = TRUE,
    story_prompt = "Future Explore Florida module: compare counties, patterns, profiles, and changes over time."
  )
}

# Convenience helper for the project's current statewide Florida data files.
load_florida_census_state <- function(data_path = "data/fl_race_2000_2020.rds",
                                     geo_path = "data/fl_geo.rds") {
  fl_race <- readRDS(data_path)
  fl_geo <- readRDS(geo_path)

  list(
    race = fl_race,
    geo = fl_geo,
    year_state = new_year_state(unique(fl_race$year), selected_year = max(unique(fl_race$year))),
    explore = make_explore_florida_state(fl_race, fl_geo, unique(fl_race$year), selected_year = max(unique(fl_race$year)))
  )
}
