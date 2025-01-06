library(tidyverse)
library(googleCloudStorageR)
library(janitor)
# this script was drafted assuming that new redd data will follow data_input_instructions. There will be minimal data cleaning, and the focus will be to re-format to obtain clean data tables

# pull in data from google cloud ---------------------------------------------------

gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

gcs_get_object(object_name = "adult-holding-redd-and-carcass-surveys/battle-creek/data/battle_redd.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk = here::here("data-raw", "battle_daily_redd.csv"),
               overwrite = TRUE)

gcs_get_object(object_name = "standard-format-data/standard_adult_upstream_passage.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk = here::here("data-raw", "standard_adult_upstream_passage.csv"),
               overwrite = TRUE)

gcs_get_object(object_name = "standard-format-data/standard_adult_passage_estimate.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk = here::here("data-raw", "standard_adult_passage_estimate.csv"),
               overwrite = TRUE)

### redd raw ### ----
redd_raw_2001_2023 <- read.csv(here::here("data", "battle_redd.csv")) |> glimpse()

### upstream estimates raw / upstream_raw ### ----
upstream_estimates_raw <- read.csv(here::here("data-raw", "standard_adult_passage_estimate.csv")) |>
  filter(stream == "battle creek")

upstream_raw <- read.csv(here::here("data-raw", "standard_adult_upstream_passage.csv")) |>
  filter(stream == "battle creek")

### ---- CLEANING DATA ---- ###


### ---- SAVE CLEAN DATA --- ##
