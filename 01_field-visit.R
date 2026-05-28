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

source('00_setup.R')
source('functions.R')

# Retrieving Locations ----------------------------------------------------

location_url <- "GetLocationData"

all_location_meta = sites |>
  map(
    \(sites) {
      location_data(
        location_url,
        sites,
        username = username,
        password = password
      )
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  mutate(LocationName = word(LocationName, 1, 2, sep = " "))


site_name_lookup <- all_location_meta |>
  select(LocationName, Identifier) |>
  rename(LocationIdentifier = Identifier)

write_rds(site_name_lookup, 'out/site_name_lookup.rds')

# Review Status by Station

all_fv_info = sites |>
  map(
    \(sites) {
      extract_fv_info_mod(sites, username = username, password = password)
    },
    .progress = TRUE
  ) |>
  purrr::list_rbind() |>
  left_join(site_name_lookup, by = 'LocationIdentifier')

write_rds(all_fv_info, "app/data/field_visits.rds")


# Extract field visit details for review and approval figures -------------

date_from = as.Date('2024-01-01')
date_to = as.Date(Sys.Date())

field_visit_year = all_fv_info |>
  filter(StartTime >= date_from) |>
  rename(FieldVisitDate = StartTime) |>
  select(
    Identifier,
    LocationIdentifier,
    LocationName,
    ApprovalLevel,
    LevelDescription,
    FieldVisitDate,
    UploadedByUser
  ) |>
  unique()

write_rds(field_visit_year, "app/data/field_visit_year.rds")
