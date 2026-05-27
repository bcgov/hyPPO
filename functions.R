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

# 02_timeseries functions - Timeseries retrieval functions ------------------------------------------

#' Retrieve the approval status and the change in approvals over time by timeseries
#'
#' @param sample_ts The unique ID of a timeseries (several within each site)
#' @param username API username required
#' @param password API password required
#'
#' @returns A dataframe with approval levels and date of approval level change by unique timeseries id
#' @export
#'
#' @examples extract_appr <- retrieve_approvals(
#'   sample_ts = "8a0ed03b7fad4a40904122d3b6d0086f",
#'   "username", "password"
#' )
#'
retrieve_approvals <- function(sample_ts, username, password) {
  approval_url <- paste0(
    "GetApprovalsTransactionList?TimeSeriesUniqueId=",
    sample_ts
  )

  approval_data <- api_req(approval_url, username, password)

  appr_ts_data <- approval_data |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    purrr::pluck("ApprovalsTransactions") |>
    tibble::as_tibble() |>
    dplyr::mutate(UniqueId = sample_ts, .before = 1)

  appr_ts_data
}


#' Retrieve list of unique time-series ids
#'
#' @param site A character site number (format "11AA1111")
#' @param username API username required
#' @param password API password required
#' @param output Preferred format of the output ('list' or 'dataframe').
#' Dataframe provides site number with timeseries id, list does not.
#'
#' @returns A list or dataframe of timeseries ID by site number
#' @export
#'
#' @examples extract_list <- ts_unique_ids(site = "08MH0041", "username", "password", output = "list")
#'
retrieve_ts_ids <- function(
  site,
  username,
  password,
  output = c("list", "dataframe")
) {
  ts_url <- paste0("GetTimeSeriesUniqueIdList?LocationIdentifier=", site)

  req_ts <- api_req(ts_url, username, password)

  resp_ts <- req_ts |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE) |>
    tibble::as_tibble()

  ts_id <- resp_ts |>
    purrr::pluck("TimeSeriesUniqueIds") |>
    dplyr::mutate(site = site)

  if (output == "list") {
    ts_id <- as.list(ts_id$UniqueId)
    ts_id
  } else if (output == "dataframe") {
    ts_id
  }
}

ts_meta_data <- function(sample_ts, username, password) {
  ts_info_url = paste0(
    "GetTimeSeriesCorrectedData?TimeSeriesUniqueId=",
    sample_ts
  )

  ts_info_req <- api_req(ts_info_url, username, password)

  ts_info_data <- ts_info_req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  grades = ts_info_data |>
    pluck("Grades")

  ts_meta = as_tibble(ts_info_data[1:6]) |>
    cbind(grades)
}


# 03_telemetry functions --------------------------------------------------

# function for pulling individual time-series data sets from specific date range

timeseries_req <- function(sample_id, date_from, date_to, username, password) {
  ts_info_url = paste0(
    "GetTimeSeriesData?TimeSeriesUniqueIds=",
    sample_id,
    "&QueryFrom=",
    date_from,
    "&QueryTo=",
    date_to
  )

  ts_info_req <- api_req(ts_info_url, username, password)

  ts_info_data <- ts_info_req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  ts_base = ts_info_data |>
    pluck("TimeSeries") |>
    as_tibble()

  ts_points = ts_info_data |>
    pluck("Points") |>
    as_tibble() |>
    bind_cols(ts_base)
}

# Identify shift ids by site

shift_ids <- function(site_id, username, password) {
  sh_info_url = paste0(
    "GetRatingModelDescriptionList?LocationIdentifier=",
    site_id
  )

  sh_info_req <- api_req(sh_info_url, username, password)

  sh_info_data <- sh_info_req |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = TRUE)

  sh_base = sh_info_data |>
    pluck("RatingModelDescriptions") |>
    as_tibble()
}

# function to find most recent shift applied for individual ts

extract_shift <- function(shift_id, username, password, timeseries_id) {
  timeseries_id <- timeseries_id |>
    filter(Identifier == shift_id) |>
    pull(UniqueId)

  if (!is.na(timeseries_id)) {
    sh_applied_url <- paste0(
      "GetRatingModelEffectiveShifts?TimeSeriesUniqueId=",
      timeseries_id,
      "&RatingModelIdentifier=",
      shift_id
    )

    sh_applied_req <- api_req(sh_applied_url, username, password)

    sh_applied_data <- sh_applied_req |>
      httr2::req_perform() |>
      httr2::resp_body_json(simplifyVector = TRUE)

    sh_applied <- sh_applied_data |>
      pluck("EffectiveShifts") |>
      as_tibble() |>
      glimpse() |>
      mutate(Timestamp = lubridate::as_datetime(Timestamp))
    # group_by(Value) |>
    # mutate(
    #   first_occurence = first(Timestamp),
    #   last_occurence = last(Timestamp)
    # ) |>

    if ("Value" %in% colnames(sh_applied)) {
      tol <- 0 # e.g., tol <- 1e-6 for numeric noise

      df_runs <- sh_applied %>%
        arrange(Timestamp) %>%
        mutate(
          # Detect a change (TRUE) when current value differs from previous by more than tol
          changed = case_when(
            is.na(lag(Value)) ~ TRUE, # first row starts a run
            abs(Value - lag(Value)) > tol ~ TRUE,
            TRUE ~ FALSE
          ),
          run_id = cumsum(changed)
        ) %>%
        group_by(run_id) %>%
        summarise(
          Value = first(Value),
          start_ts = first(Timestamp),
          end_ts = last(Timestamp),
          .groups = "drop"
        )

      out <- df_runs |>
        arrange(run_id) |>
        slice_max(run_id) |>
        mutate(
          Identifier = shift_id,
          UniqueId = timeseries_id
        ) |>
        select(Identifier, UniqueId, Value, start_ts) |>
        rename(Timestamp = start_ts)

      out
    } else {
      out = sh_applied |>
        mutate(Value = NA, Identifier = shift_id, UniqueId = timeseries_id) |>
        slice_max(Timestamp)
    }
    return(out)
  }
}
