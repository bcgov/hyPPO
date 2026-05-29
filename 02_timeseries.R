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

#source('00_setup.R')
#source('functions.R')

site_name_lookup <- readRDS('out/site_name_lookup.rds')

# Retrieve all unique timeseries ids ---------------------------------------------

# find all unique timeseries ids
# Get information on what each timeseries id references

all_ts_ids = ts_sites |>
  purrr::map(
    \(ts_sites) {
      retrieve_ts_ids(
        ts_sites,
        username = username,
        password = password,
        output = "dataframe"
      )
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind()

ts_id = as.list(all_ts_ids$UniqueId)

# sample_ts <- all_ts_ids$UniqueId[[92]]
#
# ts_url = paste0("GetTimeSeriesCorrectedData?TimeSeriesUniqueId=", sample_ts)

## Approval Level ----------------------------------------------------------

all_approval_level = ts_id |>
  purrr::map(
    \(ts_id) {
      retrieve_approvals(ts_id, username = username, password = password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  left_join(all_ts_ids, by = "UniqueId")

#write_rds(all_approval_level, 'out/approval_levels.rds')

## Retrieve grade level and additional meta data-----------------------------------

grade_levels = ts_id |>
  purrr::map(
    \(ts_id) ts_meta_data(ts_id, username = username, password = password),
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  left_join(all_ts_ids, by = "UniqueId")

#write_csv(grade_levels, "out/grade_levels.csv")

all_ts_meta = grade_levels |>
  select(-c(GradeCode, StartTime, EndTime)) |>
  unique() |>
  left_join(all_approval_level, by = c("UniqueId", "site"))

#write_csv(all_ts_meta, 'out/timeseries_info_w_approvals.csv')

ts_id_info <- all_ts_ids |>
  rename(LocationIdentifier = site) |>
  left_join(all_ts_meta, by = c('UniqueId', 'LocationIdentifier')) |>
  select(c(
    UniqueId,
    LocationIdentifier,
    Parameter,
    Label,
    LevelDescription,
    NumPoints,
    DateAppliedUtc
  )) |>
  mutate(DateAppliedUtc = as_datetime(DateAppliedUtc)) |>
  distinct() |>
  arrange(UniqueId, LocationIdentifier, Parameter, DateAppliedUtc)

#write_csv(ts_id_info, 'timeseries_id_info.csv')

# Read in csv. if re-starting here -------------------------------

# all_ts_meta <- read_csv('timeseries_info_w_approvals.csv')
# ts_id_info <- read_csv('timeseries_id_info.csv')

# Timeseries Corrected Data -----------------------------------------------

ts_data_ids <- all_ts_meta |>
  filter(Parameter == "Stage" | Parameter == "Discharge") |>
  filter(
    Label == "Working" | Label == "Telemetry_m" | Label == "Working_Telemetry"
  ) |>
  distinct() |>
  filter(StartTime > "2000-01-01") |>
  mutate(
    LevelDescription = factor(
      LevelDescription,
      levels = c("Approved", "Reviewed", "In Review", "Working")
    ),
    StartTime = as_datetime(StartTime),
    EndTime = as_datetime(EndTime)
  ) |>
  left_join(site_name_lookup, by = "LocationIdentifier") |>
  mutate(
    EndTime = case_when(EndTime > Sys.Date() ~ StartTime, TRUE ~ EndTime)
  ) |>
  arrange(LocationIdentifier, Parameter, LevelDescription, DateAppliedUtc) |>
  ungroup()

latest_trans = ts_data_ids |>
  group_by(UniqueId, Parameter, Label, LocationIdentifier) |>
  arrange(desc(DateAppliedUtc)) |>
  slice(1) |>
  mutate(LatestTransaction = EndTime) |>
  select(UniqueId, Parameter, Label, LocationIdentifier, LatestTransaction)

# Set up daily grid to arrange timeseries timeline for figure

date_from = as.Date('2024-01-01')
date_to = as.Date(Sys.Date())
LocationIdentifier = unique(site_name_lookup$LocationIdentifier)
Parameter = c("Stage", "Discharge")

Date <- seq.Date(from = date_from, to = date_to, by = "day")

year_grid = crossing(Date, Parameter, LocationIdentifier) |>
  left_join(site_name_lookup, by = "LocationIdentifier")

ts_data_test <- year_grid |>
  left_join(
    ts_data_ids,
    by = c('Parameter', 'LocationIdentifier', 'LocationName'),
    relationship = "many-to-many"
  ) |>
  left_join(
    latest_trans,
    by = c('UniqueId', 'Parameter', 'Label', 'LocationIdentifier')
  ) |>
  mutate(
    Within_Series = case_when(
      Date <= EndTime & Date >= StartTime ~ TRUE,
      TRUE ~ FALSE
    )
  ) |>
  mutate(
    Working_Default = case_when(
      Date <= Sys.Date() & Date > LatestTransaction ~ TRUE,
      TRUE ~ FALSE
    ),
    ApprovalLevel = case_when(
      Working_Default == TRUE ~ 800,
      TRUE ~ as.numeric(ApprovalLevel)
    ),
    LevelDescription = case_when(
      Working_Default == TRUE ~ "Working",
      TRUE ~ as.factor(LevelDescription)
    )
  ) |>
  select(
    LocationName,
    LocationIdentifier,
    Parameter,
    Date,
    StartTime,
    EndTime,
    Within_Series,
    Working_Default,
    ApprovalLevel,
    LevelDescription,
    DateAppliedUtc
  ) |>
  filter(Within_Series == TRUE | Working_Default == TRUE) |>
  group_by(Date, LocationName, LocationIdentifier, Parameter) |>
  arrange(desc(ApprovalLevel)) |>
  slice(1) |>
  ungroup() |>
  arrange(LocationName, Parameter, Date, ApprovalLevel) |>
  select(
    LocationName,
    LocationIdentifier,
    Parameter,
    Date,
    ApprovalLevel,
    LevelDescription
  ) |>
  arrange(LocationName) |>
  mutate(LocationName = forcats::fct_inorder(LocationName)) |>
  mutate(
    LevelDescription = fct_relevel(
      LevelDescription,
      "Approved",
      "Reviewed",
      "In Review",
      "Working"
    )
  )


write_rds(ts_data_test, "app/data/timeseries_approval.rds")
