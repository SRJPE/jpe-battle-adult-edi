library(tidyverse)
library(googleCloudStorageR)

# TODO personnel
# TODO title
# TODO keyword set
# TODO funding
# TODO project
# TODO coverage


# pull in data from google cloud ---------------------------------------------------

# notes for EDI package from adult data meeting
# prioritize redd and upstream passage data
# can add holding and carcass later
# restrict individual redd locations (simplify to reach level, no lat/longs)
# start with just adding spring run

# TODO should we use estimates or raw count data for upstream passage?

gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

gcs_get_object(object_name = "standard-format-data/standard_daily_redd.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk = here::here("data-raw", "standard_daily_redd.csv"),
               overwrite = TRUE)

gcs_get_object(object_name = "standard-format-data/standard_adult_upstream_passage.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk = here::here("data-raw", "standard_adult_upstream_passage.csv"),
               overwrite = TRUE)

gcs_get_object(object_name = "standard-format-data/standard_adult_passage_estimate.csv",
               bucket = gcs_get_global_bucket(),
               saveToDisk = here::here("data-raw", "standard_adult_passage_estimate.csv"),
               overwrite = TRUE)

redd_raw <- read.csv(here::here("data-raw", "standard_daily_redd.csv")) |>
  filter(stream == "battle creek")

escapement_estimates_raw <- read.csv(here::here("data-raw", "standard_adult_passage_estimate.csv")) |>
  filter(stream == "battle creek")

escapement_counts_raw <- read.csv(here::here("data-raw", "standard_adult_upstream_passage.csv")) |>
  filter(stream == "Battle Creek")


# clean data --------------------------------------------------------------

# TODO no run information
redd <- redd_raw |>
  mutate(date = as.Date(date),
         reach = ifelse(reach %in% c("1", "2", "3", "4", "5"), paste0("R", reach), reach)) |>
  select(-c(year, stream, method, depth_m, starting_elevation_ft,
            redd_id, num_of_fish_on_redd, latitude, longitude, species)) |> # empty columns and remove lat/longs
  glimpse()

# TODO keep comments?
escapement_raw <- escapement_counts_raw |>
  mutate(stream = tolower(stream),
         date = as.Date(date)) |>
  filter(run == "spring") |>
  select(-c(sex, viewing_condition, spawning_condition, jack_size,
            ladder, flow, temperature, hours, comments, stream)) |>
  glimpse()

# TODO this shows data from 1995 but they only began collecting in 1998?
escapement_estimates <- escapement_estimates_raw |> # all spring run
  select(-c(stream)) |>
  glimpse()


# write files -------------------------------------------------------------
write.csv(redd, here::here("data", "battle_redd.csv"), row.names = FALSE)
write.csv(escapement_raw, here::here("data", "battle_escapement_raw.csv"), row.names = FALSE)
write.csv(escapement_estimates, here::here("data", "battle_escapement_estimates.csv"), row.names = FALSE)


# review ------------------------------------------------------------------
read.csv(here::here("data", "battle_redd.csv")) |> glimpse()
read.csv(here::here("data", "battle_escapement_raw.csv")) |> glimpse()
read.csv(here::here("data", "battle_escapement_estimates.csv")) |> glimpse()
