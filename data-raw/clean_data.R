library(tidyverse)
library(googleCloudStorageR)
library(janitor)

# pull in data from google cloud ---------------------------------------------------
# we are not reading carcass, environmental or adult data for now

gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

# Notes for context on work. Processing of new data has been done using JPE-datasets as guide:
# JPE-datasets to review processing of raw data. Link to repo: (https://github.com/SRJPE/JPE-datasets/blob/main/data-raw/qc-markdowns/adult-holding-redd-and-carcass-surveys/battle-creek/battle_creek_redd_qc.Rmd)
# the goal is to process new data the same way
# there is an overlap on 2022 data, old data covered some 2022, we are using the newest version of 2022 data (new data added)
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
redd_raw_2001_2022 <- read_csv(here::here("data-raw", "battle_daily_redd.csv")) |> # from last update
  glimpse()

# redd_raw_2022 <- readxl::read_excel("data-raw/2022_BC_flowwest data.xlsx", sheet = 3) |> # this data was added previously but we got an updated version
#   clean_names() |>
#   glimpse()

redd_raw_2022_new <- readxl::read_excel("data-raw/2022_BC_flowwest data_new.xlsx", sheet = 2) |> # this data was added for this update
  clean_names() |>
  glimpse()

redd_raw_2023 <- readxl::read_excel("data-raw/2023_BC_flowwest data.xlsx", sheet = 4) |> # this data was added for this update
  clean_names() |>
  glimpse()

redd_raw_2024 <- readxl::read_excel("data-raw/2024_BC_flowwest data.xlsx", sheet = 2) |> # this data was added for this update
  clean_names() |>
  glimpse()

### upstream estimates raw / upstream_raw ### ----
upstream_estimates_raw <- read.csv(here::here("data-raw", "standard_adult_passage_estimate.csv")) |>
  filter(stream == "battle creek")

upstream_raw <- read.csv(here::here("data-raw", "standard_adult_upstream_passage.csv")) |>
  filter(stream == "battle creek")

### ---- CLEANING DATA ---- ###

# redd --------------------------------------------------------------------

# 2022 data ----
# compared to previous 2022 data, this data is missing "date_mea", "Corr_Type" and "Horz_Prec"
redd_raw_2022_clean_1 <- redd_raw_2022_new |>
  janitor::clean_names() |>
  mutate(year = year(date),
         JPE_redd_id = paste0(as_date(date), "_", reach, "_", redd_id))

redd_raw_2022_clean_2 <- redd_raw_2022_clean_1 |>
  mutate(date_1 = as.Date(date, format = "%m/%d/%Y"),
         date_2 = as.Date(date_2, format = "%m/%d/%Y"),
         date_3 = as.Date(date_3, format = "%m/%d/%Y"),
         date_4 = as.Date(date_4, format = "%m/%d/%Y"),
         date_5 = as.Date(date_5, format = "%m/%d/%Y"),
         age_1 = age,
         age_2 = age_2,
         age_3 = age_3,
         age_4 = age_4,
         age_5 = age_5) |>
  select(-c(age)) |> # don't need anymore
  pivot_longer(cols = c(age_1, age_2, age_3, age_4, age_5), # pivot all aging instances to age column
               values_to = "new_age",
               names_to = "age_index") |>
  # for all aging instances, take the date where that aging occurred.
  # check for what aging instance it was and pull that date (if present)
  mutate(new_date = case_when(age_index == "age_2" & !is.na(date_2) ~ date_2,
                              age_index == "age_3" & !is.na(date_3) ~ date_3,
                              age_index == "age_4" & !is.na(date_4) ~ date_4,
                              age_index == "age_5" & !is.na(date_5) ~ date_5,
                              age_index == "age_1" ~ date_1,
                              TRUE ~ NA),
         age_index = case_when(age_index == "age_1" ~ 1,
                               age_index == "age_2" ~ 2,
                               age_index == "age_3" ~ 3,
                               age_index == "age_4" ~ 4,
                               age_index == "age_5" ~ 5),
         age_index = ifelse(is.na(new_age) & age_index == 1, 0, age_index)) |>
  filter(!is.na(new_date)) |>
  select(-c(date, date_1, date_2, date_3, date_4, date_5, redd_id)) |>
  rename(age = new_age, date = new_date) |>
  relocate(date, .before = point_x) |>
  relocate(JPE_redd_id, .before = date) |>
  mutate(run = ifelse(species == "Chinook", "spring", NA),
         species = ifelse(species == "O.mykiss", "O. mykiss", species)) |>
  glimpse()


redd_2022 <- redd_raw_2022_clean_2 |>
  select(-c(year, comments)) |> # use JPE_redd_id
  rename(latitude = point_y, longitude = point_x,
         pre_redd_substrate_size = pre_sub,
         tail_substrate_size = tail_sub, fish_guarding = fish_on_redd,
         redd_measured = measure,
         why_not_measured = why_not_measure,
         # date_measured = date, # there is not date_measured
         pre_redd_depth = pre_redd,
         redd_pit_depth = pit_in, redd_length = length_in,
         redd_width = width_in,
         start_number_flow_meter_80 = start_80,
         end_number_flow_meter_80 = end_80,
         flow_meter_time_80 = secs_80,
         flow_fps = water_velo,
         start_number_flow_meter = bomb_start,
         end_number_flow_meter = bomb_end,
         flow_meter_time = bomb_secon,
         redd_substrate_size = side_sub) |>
  mutate(redd_measured = ifelse(redd_measured == "Y", TRUE, redd_measured),
         date_measured = NA, #adding it to match previous data
         redd_loc = NA,
         survey = NA,
         reach_sub_unit = NA,
         redd_tail_depth = NA,
         flow_meter = NA,
         survey_method = NA,
         fish_guarding = case_when(fish_guarding == "N" ~ FALSE,
                                   fish_guarding == "Y" ~ TRUE)) |>
  select(c("JPE_redd_id", "date", "survey_method", "longitude", "latitude", "reach", "reach_sub_unit", "river_mile",
           "redd_loc","pre_redd_substrate_size","redd_substrate_size","tail_substrate_size", "fish_guarding","redd_measured",
           "why_not_measured","pre_redd_depth", "redd_pit_depth", "redd_tail_depth", "redd_length", "redd_width",
           "flow_meter","flow_fps","start_number_flow_meter", "end_number_flow_meter", "flow_meter_time", "start_number_flow_meter_80",
           "end_number_flow_meter_80","flow_meter_time_80","survey", "run","species", "age_index","age", "date_measured", "tailspill","fork")) |>
  glimpse()

#2023 data ----
redd_raw_2023_clean_1 <- redd_raw_2023 |>
  janitor::clean_names() |>
  mutate(year = year(date),
         JPE_redd_id = paste0(as_date(date), "_", reach, "_", redd)) |>
  glimpse()

redd_raw_2023_clean_2 <- redd_raw_2023_clean_1|>
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
  glimpse()

redd_2023 <- redd_raw_2023_clean_2|>
  rename(latitude = y_4, longitude = x_3,
         pre_redd_substrate_size = pre_redd_substrate_in,
         tail_substrate_size = tailspill_substrate_in, fish_guarding = fish_on_redd,
         # redd_measured = measure,
         # why_not_measured = why_not_me, # fileld not included, check if all NAs on other data, if so we can mutate
         pre_redd_depth = pre_redd_depth_in,
         redd_pit_depth = pre_redd_depth_in,
         tailspill = tailspill_depth_in,
         redd_length = length_in,
         redd_width = width_in,
         start_number_flow_meter_80 = flowmeter_80_percent_start,
         end_number_flow_meter_80 = flowmeter_80_percent_end,
         flow_meter_time_80 = flowmeter_80_percent_time,
         # flow_fps = water_velo,
         start_number_flow_meter = flowmeter_start,
         end_number_flow_meter = flowmeter_end,
         flow_meter_time = flowmeter_time_s,
         redd_substrate_size = side_substrate_in) |>
  mutate(redd_loc = NA,
         redd_measured = NA,
         why_not_measured = NA,
         date_measured = NA, #unsure?
         flow_fps = NA,
         survey = NA, #these are all NA in data anyway
         fork = NA,
         pre_redd_depth = NA,
         reach_sub_unit = NA,
         redd_tail_depth = NA,
         flow_meter = NA,
         survey_method = NA,
         fish_guarding = case_when(fish_guarding == "No" ~ FALSE)) |>
  select(-c(age_comments, x_35, y_36, gravel_type, pit_depth_in, comments, redd)) |> # these do't appear on previous data
  select(c("JPE_redd_id", "longitude", "latitude", "river_mile", "date", "survey_method",
    "reach", "reach_sub_unit", "species", "age", "redd_loc", "pre_redd_substrate_size",
    "redd_substrate_size", "tail_substrate_size", "fish_guarding", "redd_measured", "why_not_measured",
    "date_measured", "pre_redd_depth", "redd_pit_depth", "redd_tail_depth", "tailspill", "redd_length",
    "redd_width", "flow_meter", "start_number_flow_meter", "end_number_flow_meter", "flow_meter_time", "start_number_flow_meter_80",
    "end_number_flow_meter_80", "flow_meter_time_80", "flow_fps", "survey", "run",
    "fork", "age_index")) |>
  glimpse()

# removing 2022 data from redd_raw_2001_2022
redd_2001_2021 <- redd_raw_2001_2022 |>
  filter(year(date) != 2022) |>
  glimpse()

#2024 data ----
redd_raw_2024_clean_1 <- redd_raw_2024 |>
  janitor::clean_names() |>
  mutate(year = year(date),
         JPE_redd_id = paste0(as_date(date), "_", reach, "_", redd)) |>
  glimpse()

redd_raw_2024_clean_2 <- redd_raw_2024_clean_1|>
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
  glimpse()

redd_2024 <- redd_raw_2024_clean_2|>
  rename(latitude = y, longitude = x,
         pre_redd_substrate_size = pre_redd_substrate,
         tail_substrate_size = tailspill_substrate,
         fish_guarding = fish_on_redd,
         # redd_measured = measure,
         # why_not_measured = why_not_me, # fileld not included, check if all NAs on other data, if so we can mutate
         pre_redd_depth = pre_redd_depth_in,
         redd_pit_depth = pit_depth_in,
         tailspill = tailspill_depth_in,
         redd_length = length_in,
         redd_width = width_in,
         start_number_flow_meter_80 = flowmeter_80_percent_start,
         end_number_flow_meter_80 = flowmeter_80_percent_end,
         # flow_fps = water_velo,
         start_number_flow_meter = flowmeter_start,
         end_number_flow_meter = flowmeter_end,
         flow_meter_time = flowmeter_time_s,
         redd_substrate_size = side_substrate) |>
  mutate(river_mile = NA,
  redd_loc = NA,
         redd_measured = NA,
         why_not_measured = NA,
         date_measured = NA, #unsure?
         flow_fps = NA,
         survey = NA, #these are all NA in data anyway
         fork = NA,
         reach_sub_unit = NA,
         redd_tail_depth = NA,
         flow_meter = NA,
         survey_method = NA,
         fish_guarding = case_when(fish_guarding == "No" ~ FALSE),
  flow_meter_time_80 =  as.numeric(gsub(" seconds", "", flowmeter_80_percent_time))
  ) |>
  # select(-c(age_comments, x_35, y_36, gravel_type, pit_depth_in, comments, redd)) |> # these do't appear on previous data
  select(c("JPE_redd_id", "longitude", "latitude", "river_mile", "date", "survey_method",
           "reach", "reach_sub_unit", "species", "age", "redd_loc", "pre_redd_substrate_size",
           "redd_substrate_size", "tail_substrate_size", "fish_guarding", "redd_measured", "why_not_measured",
           "date_measured", "pre_redd_depth", "redd_pit_depth", "redd_tail_depth", "tailspill", "redd_length",
           "redd_width", "flow_meter", "start_number_flow_meter", "end_number_flow_meter", "flow_meter_time", "start_number_flow_meter_80",
           "end_number_flow_meter_80", "flow_meter_time_80", "flow_fps", "survey", "run",
           "fork", "age_index")) |>
  glimpse()


# Binding all redd data ----
clean_2022_2024_data <- bind_rows(redd_2022, redd_2023, redd_2001_2021, redd_2024) |>
  glimpse()

unique(clean_2022_2024_data$redd_substrate_size)
# Cleaning substrate size
clean_2022_2024_data$redd_substrate_size <- gsub("^'|\\s*'$", "", clean_2022_2024_data$redd_substrate_size)
clean_2022_2024_data$pre_redd_substrate_size <- gsub("^'|\\s*'$", "", clean_2022_2024_data$pre_redd_substrate_size)
clean_2022_2024_data$tail_substrate_size <- gsub("^'|\\s*'$", "", clean_2022_2024_data$tail_substrate_size)
redd_2022_2024_data <- clean_2022_2024_data |>
  mutate(redd_substrate_size = str_replace_all(redd_substrate_size, "\\s*-\\s*", "-"),
         redd_substrate_size = str_replace_all(redd_substrate_size, "^'|'$", ""),
         redd_substrate_size = case_when(
           redd_substrate_size %in% c("< 0.1", "<0.1") ~ "<0.25",
           redd_substrate_size %in% c("0.1 to 1", "0.1-1", "0.1 - 1") ~ "0.5-1",
           redd_substrate_size == "1" ~ "0.5-1",
           redd_substrate_size %in% c("1-2", "1 - 2", "'1-2", "'1 - 2") ~ "1-2",
           redd_substrate_size %in% c("1-3", "2-3", "2 - 3", "'2-3", "'2 - 3") ~ "2-4",
           redd_substrate_size %in% c("3-4", "'3-4") ~ "2-4",
           redd_substrate_size %in% c("3-5", "4-5", "4-6", "5-6", "6-7") ~ "4-8",
           redd_substrate_size == ">12" ~ "8-16",
           redd_substrate_size %in% c("8-16") ~ "8-16",
           redd_substrate_size == ">16" ~ ">16",
           redd_substrate_size %in% c("1-5") ~ "2-4",
           redd_substrate_size %in% c("2-4") ~ "2-4",
           TRUE ~ NA),
         pre_redd_substrate_size = str_replace_all(pre_redd_substrate_size, "\\s*-\\s*", "-"),
         pre_redd_substrate_size = str_replace_all(pre_redd_substrate_size, "^'|'$", ""),
         pre_redd_substrate_size = case_when(
           pre_redd_substrate_size %in% c("< 0.1", "<0.1") ~ "<0.25",
           pre_redd_substrate_size %in% c("0.1 to 1", "0.1-1", "0.1 - 1") ~ "0.5-1",
           pre_redd_substrate_size == "1" ~ "0.5-1",
           pre_redd_substrate_size %in% c("1-2", "1 - 2", "'1-2", "'1 - 2") ~ "1-2",
           pre_redd_substrate_size %in% c("1-3", "2-3", "2 - 3", "'2-3", "'2 - 3") ~ "2-4",
           pre_redd_substrate_size %in% c("3-4", "'3-4") ~ "2-4",
           pre_redd_substrate_size %in% c("3-5", "4-5", "4-6", "5-6", "6-7") ~ "4-8",
           pre_redd_substrate_size == ">12" ~ "8-16",
           pre_redd_substrate_size %in% c("8-16") ~ "8-16",
           pre_redd_substrate_size == ">16" ~ ">16",
           pre_redd_substrate_size %in% c("2-4") ~ "2-4",
           TRUE ~ NA),
         tail_substrate_size = str_replace_all(tail_substrate_size, "\\s*-\\s*", "-"),
         tail_substrate_size = str_replace_all(tail_substrate_size, "^'|'$", ""),
         tail_substrate_size = case_when(
           tail_substrate_size %in% c("< 0.1", "<0.1") ~ "<0.25",
           tail_substrate_size %in% c("0.1 to 1", "0.1-1", "0.1 - 1") ~ "0.5-1",
           tail_substrate_size == "1" ~ "0.5-1",
           tail_substrate_size %in% c("1-2", "1 - 2", "'1-2", "'1 - 2") ~ "1-2",
           tail_substrate_size %in% c("1-3", "2-3", "2 - 3", "'2-3", "'2 - 3") ~ "2-4",
           tail_substrate_size %in% c("3-4", "'3-4") ~ "2-4",
           tail_substrate_size %in% c("3-5", "4-5", "4-6", "5-6", "6-7") ~ "4-8",
           tail_substrate_size == ">12" ~ "8-16",
           tail_substrate_size %in% c("8-16") ~ "8-16",
           tail_substrate_size == ">16" ~ ">16",
           tail_substrate_size %in% c("1-5") ~ "2-4",
           tail_substrate_size %in% c("2-4") ~ "2-4",
           TRUE ~ NA))
unique(redd_2022_2024_data$redd_substrate_size)
# checked for NA's. None were introduced while binding
# standardize substrate sizes for redd using the Wentworth Scale, created by W.C Krumbein
# note: when the size range fell into two categories, they were rounded down

# ---- NOTE: ALL code below was already here from last update (keeping for reference, and as a goal to clean all data the same way)---

# standarized size ranges lookup
substrate_class = data.frame("standardized_size_range" = c("<0.25",
                                                           "0.25-0.5",
                                                           "0.5-1",
                                                           "1-2",
                                                           "2-4",
                                                           "4-8",
                                                           '8-16',
                                                           ">16"),
                             "redd_substrate_class" = c("fine sand",
                                                        "medium sand",
                                                        "coarse sand", "very coarse sand",
                                                        "very fine gravel", "fine gravel",
                                                        "medium gravel",
                                                        "coarse gravel to boulder"))
# fit current substrate size to categories above. As we do transformation do checks
#for NAs. Before joining, filter out 2022 from original data
unique(redd_2022_2024_data$redd_substrate_size)

redd_substrate_size_lookup <-
  data.frame("redd_substrate_size" = unique(redd_2022_2024_data$redd_substrate_size),
             "standardized_size_range" = c("1-2","2-4","0.5-1",NA,"<0.25","4-8","8-16")) |>
  left_join(substrate_class) # currently getting an error here

redd <- redd_2022_2024_data |>
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
         # Reach 1B began being surveyed in 2021 due to private land concerns
         reach = ifelse(year(date) >= 2021 & reach == "R1", "R1B", reach)) |>
  select(date, redd_id = JPE_redd_id, reach, fish_on_redd = fish_guarding, age, run,
         redd_measured, redd_width, redd_length,
         pre_redd_depth, redd_pit_depth, redd_tail_depth,
         redd_substrate_class, tail_substrate_class, pre_redd_substrate_class,
         velocity = flow_fps)

# the goal of this summary is that we show data without changing to pivot longer
redd_summary_earlier <- redd |>
  mutate(year = year(date)) |>
  group_by(year) |>
  distinct(redd_id, .keep_all = T) |>
  mutate(redd_count = 1) |> # after selecting for distinct redd_id add redd_count
  summarize(total_annual_redd_count = sum(redd_count),
            number_reaches_surveyed = length(unique(reach)))

# redd summary for 2022-2024 was done using the environmentals tab in the excel
redd_2022_environmentals <- readxl::read_excel("data-raw/2022_BC_flowwest data_new.xlsx", sheet = 1) |>
  clean_names() |>
  mutate(reach = case_when(
    reach == "R1B" ~ "R1",
    TRUE ~ reach),
    year = year(date)) |>
  group_by(year) |>
  summarize(number_reaches_surveyed = n_distinct(reach),
            reach_numbers = str_c(sort(unique(reach)), collapse = ", "))

redd_2023_environmentals <- readxl::read_excel("data-raw/2023_BC_flowwest data.xlsx", sheet = 1) |>
  clean_names() |>
  mutate(reach = case_when(
    reach == "R2a" ~ "R2",
    reach == "R2b" ~ "R2",
    TRUE ~ reach),
    year = year(date)) |>
  group_by(year) |>
  summarize(number_reaches_surveyed = n_distinct(reach),
            reach_numbers = str_c(sort(unique(reach)), collapse = ", "))

redd_2024_environmentals <- readxl::read_excel("data-raw/2024_BC_flowwest data.xlsx", sheet = 1) |>
  clean_names() |>
  mutate(year = year(date)) |>
  group_by(year) |>
  summarize(number_reaches_surveyed = n_distinct(reach),
            reach_numbers = str_c(sort(unique(reach)), collapse = ", "))

redd_summary <- bind_rows(redd_2022_environmentals, redd_2023_environmentals, redd_2024_environmentals, redd_summary_earlier)

#clean the repetitive rows
redd_summary <- redd_summary |>
  group_by(year) |>
  summarize(number_reaches_surveyed = max(number_reaches_surveyed, na.rm = TRUE),
            reach_numbers = str_c(sort(unique(na.omit(reach_numbers))), collapse = ", "),
            total_annual_redd_count = first(na.omit(total_annual_redd_count)),
            .groups = "drop") |>
  select(year, total_annual_redd_count, number_reaches_surveyed, reach_numbers) |>
  mutate(reach_numbers = gsub(",", "/", reach_numbers)) |>
  glimpse()

### NOTES
# redd_width ranged from 0 - 6.68 (2001-2021 data), newer data from 2022 and 2023 introduced much higher values (>100), range is now 0-206
# same with redd_length (0-351), pre_redd_depth (0-28), redd_pit_depth (0-39), we might just want to confirm that these values make sense
# metadata was updated with these values

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
redd <- read.csv(here::here("data", "battle_redd.csv")) |> glimpse()
redd_summary <- read.csv(here::here("data", "battle_redd_summary.csv")) |> glimpse()
# read.csv(here::here("data", "battle_upstream_passage_raw.csv")) |> glimpse()
# read.csv(here::here("data", "battle_upstream_passage_estimates.csv")) |> glimpse()
