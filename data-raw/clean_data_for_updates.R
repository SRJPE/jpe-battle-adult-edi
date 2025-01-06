library(tidyverse)
library(googleCloudStorageR)
library(janitor)
# this script was drafted assuming that new redd data will follow data_input_instructions. There will be minimal data cleaning, and the focus will be to re-format to obtain clean data tables
# note that data object name will need to be updated, this code won't work until new data is added, and names are updated.

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

# This is where the new data gets read in
# redd_raw_yyyy <- read_csv(here::here("data-raw", "name_of_redd_csv.csv"))

### upstream estimates raw / upstream_raw ### ----
upstream_estimates_raw <- read.csv(here::here("data-raw", "standard_adult_passage_estimate.csv")) |>
  filter(stream == "battle creek")

upstream_raw <- read.csv(here::here("data-raw", "standard_adult_upstream_passage.csv")) |>
  filter(stream == "battle creek")

### ---- CLEANING DATA ---- ###

redd_raw_yyyy_clean_1 <- redd_raw_yyyy |> #TODO update name with corresponding year
  janitor::clean_names() |>
  mutate(year = year(date),
         JPE_redd_id = paste0(as_date(date), "_", reach, "_", redd)) |>
  glimpse()

redd_raw_yyyy_clean_2 <- redd_raw_yyyy_clean_1|>
  mutate(date_1 = as.Date(date, format = "%m/%d/%Y"), # assign date to date_a (for first redd encounter)
         date_2 = as.Date(date_visit_2, format = "%m/%d/%Y"), # second redd encounter (if happens)
         date_3 = as.Date(date_visit_3, format = "%m/%d/%Y"), # etc.
         date_4 = as.Date(date_visit_4, format = "%m/%d/%Y"),
         date_5 = as.Date(date_visit_5, format = "%m/%d/%Y"),
         # age_1 = age, # is there a "age_1" value for age of first redd encounter age???
         age_2 = age_visit_2,
         age_3 = age_visit_3,
         age_4 = age_visit_4,
         age_5 = age_visit_5) |>
  pivot_longer(cols = c(age_2, age_3, age_4, age_5), # pivot all aging instances to age column
               values_to = "new_age",
               names_to = "age_index") |>
  # # for all aging instances, take the date where that aging occurred.
  # # check for what aging instance it was and pull that date (if present)
  mutate(new_date = case_when(age_index == "age_2" & !is.na(date_2) ~ date_2,
                              age_index == "age_3" & !is.na(date_3) ~ date_3,
                              age_index == "age_4" & !is.na(date_4) ~ date_4,
                              age_index == "age_5" & !is.na(date_5) ~ date_5,
                              TRUE ~ NA),
         age_index = case_when(age_index == "age_2" ~ 2,
                               age_index == "age_3" ~ 3,
                               age_index == "age_4" ~ 4,
                               age_index == "age_5" ~ 5),
         age_index = ifelse(is.na(new_age) & age_index == 1, 0, age_index)) |>
  filter(!is.na(new_date)) |>
  select(-c(date, date_1, date_2, date_3, date_4, date_5)) |>
  rename(age = new_age, date = new_date) |>
  mutate(run = ifelse(species == "Chinook", "spring", NA),
         species = ifelse(species == "O.mykiss", "O. mykiss", species)) |>
  rename(latitude = y_4, longitude = x_3,
         pre_redd_substrate_size = pre_redd_substrate_in,
         tail_substrate_size = tailspill_substrate_in,
         fish_guarding = fish_on_redd,
         pre_redd_depth = pre_redd_depth_in,
         redd_pit_depth = pre_redd_depth_in,
         tailspill = tailspill_depth_in,
         redd_length = length_in,
         redd_width = width_in,
         start_number_flow_meter_80 = flowmeter_80_percent_start,
         end_number_flow_meter_80 = flowmeter_80_percent_end,
         flow_meter_time_80 = flowmeter_80_percent_time,
         start_number_flow_meter = flowmeter_start,
         end_number_flow_meter = flowmeter_end,
         flow_meter_time = flowmeter_time_s,
         redd_substrate_size = side_substrate_in) |>
  select(c("JPE_redd_id", "longitude", "latitude", "river_mile", "date", "survey_method",
           "reach", "reach_sub_unit", "species", "age", "redd_loc", "pre_redd_substrate_size",
           "redd_substrate_size", "tail_substrate_size", "fish_guarding", "redd_measured", "why_not_measured",
           "date_measured", "pre_redd_depth", "redd_pit_depth", "redd_tail_depth", "tailspill", "redd_length",
           "redd_width", "flow_meter", "start_number_flow_meter", "end_number_flow_meter", "flow_meter_time", "start_number_flow_meter_80",
           "end_number_flow_meter_80", "flow_meter_time_80", "flow_fps", "survey", "run",
           "fork", "age_index")) |>
  glimpse()

# Binding all redd data ----
clean_2022_2023_data <- bind_rows(redd_raw_2001_2023, redd_raw_yyyy_clean_2) |> #TODO update name with corresponding year
  glimpse()

redd <- redd_2022_yyyy_data |> #TODO update name with corresponding year
  mutate(survey_method = str_to_lower(survey_method)) |>
  left_join(redd_substrate_size_lookup |>
              select(redd_substrate_size, redd_substrate_class),
            by = c("redd_substrate_size")) |>
  left_join(redd_substrate_size_lookup |>
              select(redd_substrate_size, tail_substrate_class = redd_substrate_class),
            by = c("tail_substrate_size" = "redd_substrate_size")) |>
  left_join(redd_substrate_size_lookup |>
              select(redd_substrate_size, pre_redd_substrate_class = redd_substrate_class),
            by = c("pre_redd_substrate_size" = "redd_substrate_size")) |>
  mutate(reach = case_when(reach == "4" ~ "R4",
                           reach == "1" ~ "R1",
                           reach == "2" ~ "R2",
                           reach == "5" ~ "R5",
                           reach == "3" ~ "R3",
                           reach == "R12" ~ "R1",
                           TRUE ~ reach),
         reach = ifelse(year(date) >= 2021 & reach == "R1", "R1B", reach)) |>
  select(date, redd_id = JPE_redd_id, reach, fish_on_redd = fish_guarding, age, run,
         redd_measured, redd_width, redd_length,
         pre_redd_depth, redd_pit_depth, redd_tail_depth,
         redd_substrate_class, tail_substrate_class, pre_redd_substrate_class,
         velocity = flow_fps)

# the goal of this summary is that we show data without changing to pivot longer
redd_summary <- redd |>
  mutate(year = year(date)) |>
  group_by(year) |>
  distinct(redd_id, .keep_all = T) |>
  mutate(redd_count = 1) |> # after selecting for distinct redd_id add redd_count
  summarize(total_annual_redd_count = sum(redd_count),
            number_reaches_surveyed = length(unique(reach)))

# upstream passage --------------------------------------------------------
upstream <- upstream_raw |>
  mutate(date = as.Date(date)) |>
  filter(run %in% c("spring", "not recorded", "unknown")) |>
  select(date, time, count, run, adipose_clipped, passage_direction, method) |>
  glimpse()

upstream_estimates <- upstream_estimates_raw |>  # all spring run
  select(-c(stream, lcl, ucl, confidence_interval, ladder)) |>
  glimpse()


# write files -------------------------------------------------------------
write_csv(redd, here::here("data", "battle_redd.csv"))
write_csv(redd_summary, here::here("data", "battle_redd_summary.csv"))
write_csv(upstream, here::here("data", "battle_upstream_passage_raw.csv"))
write_csv(upstream_estimates, here::here("data", "battle_upstream_passage_estimates.csv"))


# review ------------------------------------------------------------------
# read.csv(here::here("data", "battle_redd.csv")) |> glimpse()
# read.csv(here::here("data", "battle_upstream_passage_raw.csv")) |> glimpse()
# read.csv(here::here("data", "battle_upstream_passage_estimates.csv")) |> glimpse()

### ---- SAVE CLEAN DATA --- ##
