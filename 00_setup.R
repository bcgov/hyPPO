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

library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)
library(xml2)
library(purrr)
library(readr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(envreportutils)
library(stringr)
library(forcats)

if (!dir.exists('out')) {
  dir.create('out')
}

if (!dir.exists('app/data')) {
  dir.create('app/data')
}

username = Sys.getenv("APP_USERNAME")
password = Sys.getenv("APP_PASSWORD")

# safety check

# if (Sys.getenv("APP_USERNAME") == "" || Sys.getenv("APP_PASSWORD") == "") {
#   stop("Missing credentials: check GitHub secrets")
# }

prev_year <- 2024
current_year <- 2025

sites <- list(
  "08HA0045",
  "08HB0007",
  "08HB0021",
  "08LE0006",
  "08MH0041",
  "08NE0001",
  "08NH0001",
  "08LG0006",
  "08LG0015",
  "08HB0008",
  "08NJ0001",
  "08JC0008",
  "08JC0002", # added sites 2025-11-03
  "08MH0062", # added sites 2026-03-23
  "08MH0059",
  "08NA0002" # added 2026-04-27
)

ts_sites <- list(
  "08HA0045",
  "08HB0007",
  "08HB0021",
  "08LE0006",
  "08MH0041",
  "08NE0001",
  "08NH0001",
  "08LG0006",
  "08LG0015",
  "08HB0008",
  "08NJ0001",
  "08JC0008",
  "08MH0062",
  "08MH0059",
  "08NA0002" # added 2026-04-27
) # removed five-mile and corkscrew - no ts data yet

site <- "08MH0041"


# Setting color codes -----------------------------------------------------

# Timeseries approval status

review_levels <- c(
  "Working" = "#fe3200",
  "In Review" = "#ffcc02",
  "Reviewed" = "#b4fec0",
  "Approved" = "#31a926"
)

# Grade codes

grade_codes <- c(
  "0 - Undefined" = "#b4e2ff",
  "151 - Grade A" = "#117412",
  "141 - Grade B" = "#edff61",
  "131 - Grade C" = "#ec9e32",
  "121 - Grade E - Estimated" = "#ff0059",
  "100 - Grade U - Unknown" = "#b5b5b5"
)
