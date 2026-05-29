# Copyright 2026 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

# Telemetry Data for Weekly Report -----------------------------------------------

# source("00_setup.R")
# source('functions.R')

# all_ts_meta <- read_csv('out/timeseries_info_w_approvals.csv')

station_locations <- readRDS('out/site_name_lookup.rds')

# Part 1: Extracting telemetry data for each site and parameter --------

# list of telemetry parameters of interest
parameters = c("TA", "TW", "BVolt", "Bvolt", "OC")

# pull the correct telemetry ids for each station from master list
telemetry_db = all_ts_meta |>
  filter(
    Label == "Telemetry" &
      Parameter %in% parameters |
      Label == "Working" & Parameter == "Discharge" |
      Label == "Working" & Parameter == "Stage" |
      Label == "Telemetry_m" |
      Label == "Working_Telemetry"
  ) #|>
#filter(!(Parameter == "Stage" & Label == "Working" & LocationIdentifier == "08NH0001"))

telemetry_list = telemetry_db |>
  select(LocationIdentifier, Parameter, Label, UniqueId) |>
  unique() |>
  left_join(station_locations, by = "LocationIdentifier")

telemetry_id = as.list(telemetry_db$UniqueId)

# define dates of interest for app

date_from = Sys.Date() - 30
date_to = Sys.Date()
mid_date = Sys.Date() - 7
cutoff_date = Sys.Date() - 2

dates = c(date_from, date_to, mid_date)


# Retrieve individual time-series data sets from specific date range
# map function over all telemetry ids of interest. ETA 2 mins

ts_telemetry = telemetry_id |>
  purrr::map(
    \(telemetry_id) {
      timeseries_req(telemetry_id, date_from, date_to, username, password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind()

# tidy combined telemetry data and calculate monthly means

ts_telemetry = ts_telemetry |>
  mutate(DateTime = ymd_hms(Timestamp)) |>
  group_by(LocationIdentifier, Parameter) |>
  arrange(DateTime) |>
  mutate(monthly_mean = mean(NumericValue1)) |>
  ungroup() |>
  left_join(station_locations, by = "LocationIdentifier") # add station names

# identify value from 7-days prior from current data (to provide a change metric)

week_prior = ts_telemetry |>
  filter(DateTime == mid_date) |>
  group_by(LocationName, LocationIdentifier, Parameter) |>
  slice_head() |>
  rename(WeekPrior = NumericValue1) |>
  select(LocationName, LocationIdentifier, Parameter, WeekPrior, monthly_mean)

# create a grid of all stations and all telemetry metrics to include in app - may change this

all_combinations <- expand_grid(
  LocationName = unique(ts_telemetry$LocationName),
  Parameter = unique(ts_telemetry$Parameter)
)

# get most recent reading at time of data run for current conditions

current_reading = ts_telemetry |>
  filter(DateTime == date_to) |>
  group_by(LocationName, LocationIdentifier, Parameter, .drop = FALSE) |>
  arrange(DateTime) |>
  slice_max(DateTime) |>
  select(c(LocationName, DateTime, NumericValue1, Parameter, Label, Unit)) |>
  unique() |>
  left_join(
    week_prior,
    by = c("LocationName", "LocationIdentifier", "Parameter")
  ) |>
  mutate(
    week_difference = NumericValue1 - WeekPrior,
    diff_from_monthly_ave = NumericValue1 - monthly_mean
  ) |>
  ungroup()

current_reading <- all_combinations |>
  left_join(current_reading, by = c('LocationName', 'Parameter'))

ts_telemetry_out <- ts_telemetry |>
  filter(Parameter == "Discharge" | Parameter == "Stage") |>
  select(c(
    'LocationName',
    'Timestamp',
    'DateTime',
    'Parameter',
    'NumericValue1',
    'monthly_mean'
  )) |>
  mutate(Hour = hour(DateTime), Date = date(DateTime)) |>
  group_by(LocationName, Parameter, Hour, Date, monthly_mean) |>
  summarise(NumericValue1 = mean(NumericValue1)) |>
  mutate(Timestamp = ymd_h(paste(Date, Hour)))

write_csv(ts_telemetry_out, 'app/data/ts_telemetry.csv')

#write_csv(ts_telemetry, 'out/ts_telemetry.csv')

write_csv(current_reading, 'app/data/current_reading.csv')

#write_csv(current_reading, 'out/current_reading.csv')

# Part 2: Capturing most recent discharge shift applied ---------------------------

## Extract shift IDs ---------------------------------------

shift_id_table = ts_sites |>
  purrr::map(
    \(ts_sites) shift_ids(ts_sites, username, password),
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  filter(InputParameter == "Stage") |>
  filter(Label != "GUIC") |>
  filter(Label != "CLP2.WLRS")


## Find most recent shift applied for telemetry dashboard --------

tele_shift_id = ts_telemetry |>
  filter(Parameter == "Stage") |>
  select(UniqueId, LocationIdentifier, Parameter) |>
  unique()

tele_id <- shift_id_table |>
  left_join(tele_shift_id, by = c('LocationIdentifier')) |>
  select(Identifier, UniqueId) |>
  mutate(
    Identifier = gsub(Identifier, pattern = " ", replacement = "%20"),
    Identifier = gsub(Identifier, pattern = "@", replacement = "%40")
  ) |>
  filter(UniqueId != "5ea7dc42f05e41f3b8b63069fc338efb") # older ts id for Five Mile


shift_id_list = unique(tele_id$Identifier)


# mapping shift function over all shift ids

shift_last_applied = shift_id_list |>
  purrr::map(
    \(shift_id_list) extract_shift(shift_id_list, username, password, tele_id),
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  left_join(tele_shift_id, by = "UniqueId") |>
  left_join(station_locations, by = "LocationIdentifier")

write_rds(shift_last_applied, 'app/data/shift_last_applied.rds')

# Part 3 Field Visit Information -------------------------------------------------
# I added this in for the weekly app - to be separate from the 01_field-visit.R

all_fv_info = sites |>
  purrr::map(
    \(sites) {
      extract_fv_info_mod(sites, username = username, password = password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind()

all_fv_info = all_fv_info |>
  left_join(station_locations, by = 'LocationIdentifier')

# Days since last visit

most_recent <- all_fv_info |>
  mutate(Date = as.Date(EndTime, '%d/%m/%Y')) |>
  group_by(LocationIdentifier) |>
  slice(which.max(Date)) |>
  mutate(Current_Date = Sys.Date()) |>
  mutate(Most_Recent = Current_Date - Date) |>
  select(-c(StartTime, EndTime)) |>
  ungroup() |>
  arrange(Most_Recent) |>
  mutate(
    color = case_when(
      Most_Recent > 30 ~ "#bf2c34",
      Most_Recent <= 30 & Most_Recent > 25 ~ "#f07857",
      Most_Recent <= 25 ~ "#4fb06d"
    ),
    legend = case_when(
      color == "#bf2c34" ~ "> 30",
      color == "#f07857" ~ "25-30",
      color == "#4fb06d" ~ "< 25"
    )
  ) |>
  left_join(
    all_location_meta |>
      select(LocationName, Latitude, Longitude, ElevationUnits, Elevation),
    by = c('LocationName')
  )

write_rds(most_recent, "app/data/most_recent_visit_weekly.rds")


# Data for map labels -----------------------------------------------------

map_data = current_reading |>
  mutate(
    perc_diff = 100 *
      (diff_from_monthly_ave / ((NumericValue1 + monthly_mean) / 2)),
    exceed_20 = case_when(
      abs(perc_diff) >= 20 ~ TRUE,
      abs(perc_diff) < 20 ~ FALSE
    )
  ) |>
  left_join(
    most_recent,
    by = c('LocationName', 'LocationIdentifier')
  )
# ) |>
# left_join(
#   fv_meta_data |>
#     select(
#       LocationName,
#       Latitude,
#       Longitude,
#       ElevationUnits,
#       Elevation,
#       color,
#       legend
#     ),
#   by = c('LocationName')
# )

write_csv(map_data, "app/data/map_data.csv")
