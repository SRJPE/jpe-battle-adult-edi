library(tidyverse)
library(googleCloudStorageR)
library(janitor)

# pull in data from google cloud ---------------------------------------------------

# notes for EDI package from adult data meeting
# prioritize redd and upstream passage data
# we are not reading carcass, environmental or adult data for now
# restrict individual redd locations (simplify to reach level, no lat/longs)
# start with just adding spring run

gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

# read in battle creek redd data instead of standard redd data - this data was already manipulated

#TODOs
# look into JPE-datasets to review processing of raw data. Link to repo: (https://github.com/SRJPE/JPE-datasets/blob/main/data-raw/qc-markdowns/adult-holding-redd-and-carcass-surveys/battle-creek/battle_creek_redd_qc.Rmd)
# process new data the same way
# if there is an overlap on 2022 data, go with the newest version
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

# redd raw
redd_raw_2001_2022 <- read_csv(here::here("data-raw", "battle_daily_redd.csv")) |>
  glimpse()

redd_raw_2022 <- readxl::read_excel("data-raw/2022_BC_flowwest data.xlsx", sheet = 3) |>
  clean_names() |>
  glimpse()

redd_raw_2023 <- readxl::read_excel("data-raw/2023_BC_flowwest data.xlsx", sheet = 4) |>
  clean_names() |>
  glimpse()

# upstream estimates raw / upstream_raw
upstream_estimates_raw <- read.csv(here::here("data-raw", "standard_adult_passage_estimate.csv")) |>
  filter(stream == "battle creek")

upstream_raw <- read.csv(here::here("data-raw", "standard_adult_upstream_passage.csv")) |>
  filter(stream == "battle creek")

# CLEANING DATA
# redd --------------------------------------------------------------------

# 2022 data
# compared to previous 2022 data, this data is missing "date_mea", "Corr_Type" and "Horz_Prec"
clean_2022_data <- redd_raw_2022 |>
  janitor::clean_names() |>
  mutate(year = year(date),
         JPE_redd_id = paste0(year, "_", row_number())) |>
  glimpse()

# clean_2021_2022_data <- bind_rows(clean_2021_data, clean_2022_data) |>
redd_2022 <- clean_2022_data |>
  # clean up dates
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
  select(-c(date, date_1, date_2, date_3, date_4, date_5, qa_qc, qa_qc_date, redd_id)) |>
  rename(age = new_age, date = new_date) |>
  relocate(date, .before = point_x) |>
  relocate(JPE_redd_id, .before = date) |>
  mutate(run = ifelse(species == "Chinook", "spring", NA),
         species = ifelse(species == "O.mykiss", "O. mykiss", species)) |>
  glimpse()


#2023 data
# redd_2023_renamed <- redd_2023 |>
redd_2023 <- redd_raw_2023 |>
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
  # select(-c(age)) |> # don't need anymore
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
  # relocate(date, .before = point_x) |>
  # relocate(JPE_redd_id, .before = date) |>
  mutate(run = ifelse(species == "Chinook", "spring", NA),
         species = ifelse(species == "O.mykiss", "O. mykiss", species)) |>
  glimpse()

#TODO step 1 change cols of 2022 and 2023 to be consistent with previous data. 2 filter out 2022 data on original data
# 3 test/apply substrate class table below, 4 bind data
clean_redd_data <- bind_rows(redd_2022, redd_2023) |>
  # select(-c(year, corr_type, horz_prec, redd_call, redd_id, comments,
  #           survey_id, gravel, inj_site)) |> # use JPE_redd_id
  # rename(latitude = point_y, longitude = point_x,
  #        pre_redd_substrate_size = pre_sub,
  #        tail_substrate_size = tail_sub, fish_guarding = fish_on_re,
  #        redd_measured = measure,
  #        why_not_measured = why_not_me,
  #        date_measured = date_mea, pre_redd_depth = pre_redd,
  #        redd_pit_depth = pit_in, redd_length = length_in,
  #        redd_width = width_in,
  #        start_number_flow_meter_80 = start_80,
  #        end_number_flow_meter_80 = end_80,
  #        flow_meter_time_80 = secs_80,
  #        flow_fps = water_velo,
  #        start_number_flow_meter = bomb_start,
  #        end_number_flow_meter = bomb_end,
  #        flow_meter_time = bomb_secon,
  #        redd_substrate_size = side_sub) |>
  # mutate(redd_measured = ifelse(redd_measured == "y", TRUE, redd_measured))

# standardize substrate sizes for redd using the Wentworth Scale, created by W.C Krumbein
# when the size range fell into two categories, they were rounded down

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
unique(redd_raw$redd_substrate_size)

 # TODO check if ranges are the same
redd_substrate_size_lookup <-
  data.frame("redd_substrate_size" = unique(redd_raw$redd_substrate_size),
             "standardized_size_range" = c(NA, "1-2", "1-2", "2-4", "2-4",
                                           "0.5-1", "2-4","2-4", "8-16", "4-8",
                                           "1-2", "1-2", "4-8", "<0.25", "1-2",
                                           "2-4", "0.5-1", "1-2", "2-4", "2-4",
                                           "<0.25", "4-8", "4-8")) |>
  left_join(substrate_class)

redd <- redd_raw |>
  mutate(survey_method = str_to_lower(survey_method),
         # add redd count for all rows (each row is an observation)
         # to avoid double counting, you need to take the unique redd_id
         redd_count = 1) |>
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
  select(date, redd_id = JPE_redd_id, reach, fish_on_redd = fish_guarding, age, run, redd_count,
         redd_measured, redd_width, redd_length,
         pre_redd_depth, redd_pit_depth, redd_tail_depth,
         redd_substrate_class, tail_substrate_class, pre_redd_substrate_class,
         velocity = flow_fps)

# the goal of this summary is that we show data without changing to pivot longer
redd_summary <- redd |>
  mutate(year = year(date)) |>
  group_by(year) |>
  distinct(redd_id, .keep_all = T) |>
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
