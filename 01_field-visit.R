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
  purrr::map(
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

# Functions required ------------------------------------------------------

api_req <- function(url, username, password) {
  httr2::request(paste0(
    "https://bcmoe-prod.aquaticinformatics.net/AQUARIUS/Publish/v2/",
    url
  )) |>
    httr2::req_auth_basic(username = username, password = password)
}

# 01_field-visit functions ------------------------------------------------

location_data <- function(location_url, site, username, password) {
  site_specific_url <- paste0(location_url, "?LocationIdentifier=", site)

  req_location <- api_req(
    url = site_specific_url,
    username = username,
    password = password
  )

  resp_location <- req_location |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  location_meta = as_tibble(resp_location[1:12])
}


extract_fv_info_mod <- function(site, username, password) {
  fv_url <- "GetFieldVisitDataByLocation"

  site_specific_url <- paste0(fv_url, "?LocationIdentifier=", site)

  req_fv <- api_req(
    url = site_specific_url,
    username = username,
    password = password
  )

  resp_fv <- req_fv |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("FieldVisitData") |>
    tibble::as_tibble() |>
    # unnest(col = Attachments) |>
    tidyr::unnest(
      cols = c(
        Attachments,
        #ControlConditionActivity,
        Approval,
        InspectionActivity,
        LevelSurveyActivity,
        CompletedWork
      ),
      names_repair = "universal"
    )
  # pluck('Approval') |>
  # glimpse()

  fv_db <- resp_fv |>
    dplyr::select(
      Identifier,
      LocationIdentifier,
      ApprovalLevel,
      LevelDescription,
      StartTime,
      EndTime,
      UploadedByUser
    ) |>
    dplyr::mutate(
      StartTime = lubridate::as_date(StartTime),
      EndTime = lubridate::as_date(EndTime),
      #DateUploaded = lubridate::as_date(DateUploaded),
      Year = as.numeric(lubridate::year(StartTime))
    ) |>
    dplyr::arrange(LocationIdentifier, Identifier, StartTime) |>
    dplyr::group_by(
      Identifier,
      LocationIdentifier,
      ApprovalLevel,
      LevelDescription,
      StartTime,
      EndTime
    ) |>
    dplyr::slice_head() |>
    dplyr::ungroup() |>
    dplyr::group_by(Year) |>
    dplyr::mutate(n_per_year = n())
}


# Review Status by Station

all_fv_info = sites |>
  purrr::map(
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
