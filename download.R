library(tidyverse)
library(tidycensus)
library(sf)

# 2020 Decennial variables (PL 94-171)
race_vars_2020 <- c(
  hispanic = "P2_002N",
  white    = "P2_005N",
  black    = "P2_006N",
  asian    = "P2_008N"
)

# 2010 Decennial variables (SF1)
race_vars_2010 <- c(
  hispanic = "P004003",
  white    = "P005003",
  black    = "P005004",
  asian    = "P005006"
)

# 2000 Decennial variables (SF1)
race_vars_2000 <- c(
  hispanic = "P004002",
  white    = "P004005",
  black    = "P004006",
  asian    = "P004008"
)

# Download 2020 county data with geometries
fl_2020 <- get_decennial(
  geography = "county",
  variables = race_vars_2020,
  summary_var = "P2_001N",
  state = "FL",
  year = 2020,
  geometry = TRUE
) |> 
  mutate(year = 2020)

# Download 2010 county data
fl_2010 <- get_decennial(
  geography = "county",
  variables = race_vars_2010,
  summary_var = "P005001",
  state = "FL",
  year = 2010
) |> 
  mutate(year = 2010)

# Download 2000 county data
fl_2000 <- get_decennial(
  geography = "county",
  variables = race_vars_2000,
  summary_var = "P004001",
  state = "FL",
  year = 2000
) |> 
  mutate(year = 2000)

# Save county geometries for mapping
fl_geo <- fl_2020 |> 
  filter(variable == "hispanic") |> 
  select(GEOID, NAME)

write_rds(fl_geo, "data/fl_geo.rds")

# Combine 2000, 2010, and 2020 census data
fl_race_all <- bind_rows(
  fl_2000,
  fl_2010,
  st_drop_geometry(fl_2020)
)

write_rds(fl_race_all, "data/fl_race_2000_2020.rds")

# -------------------------------------------------------------
# 2022 5-Year ACS Tract Data: Income, Education, and Race
# -------------------------------------------------------------
acs_vars <- c(
  med_income = "B19013_001",
  pop_25plus = "B15003_001",
  bachelors  = "B15003_022",
  masters    = "B15003_023",
  prof       = "B15003_024",
  doc        = "B15003_025",
  tot_pop    = "B03002_001",
  white      = "B03002_003",
  black      = "B03002_004",
  asian      = "B03002_006",
  hispanic   = "B03002_012"
)

fl_tracts_raw <- get_acs(
  geography = "tract",
  variables = acs_vars,
  state = "FL",
  year = 2022,
  output = "wide",
  geometry = TRUE
)

fl_tracts_acs <- fl_tracts_raw |>
  st_transform(4326) |>
  mutate(
    med_income   = med_incomeE,
    pct_bachelor = round(100 * (bachelorsE + mastersE + profE + docE) / pop_25plusE, 1),
    pct_white    = round(100 * whiteE / tot_popE, 1),
    pct_black    = round(100 * blackE / tot_popE, 1),
    pct_asian    = round(100 * asianE / tot_popE, 1),
    pct_hispanic = round(100 * hispanicE / tot_popE, 1)
  ) |>
  select(GEOID, NAME, med_income, pct_bachelor, pct_white, pct_black, pct_asian, pct_hispanic, geometry)

write_rds(fl_tracts_acs, "data/fl_tracts_acs.rds")

