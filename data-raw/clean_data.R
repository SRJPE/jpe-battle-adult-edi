library(tidyverse)
library(googleCloudStorageR)
library(janitor)

# pull in data from google cloud ---------------------------------------------------

# notes for EDI package from adult data meeting
# prioritize redd and upstream passage data
# can add holding and carcass later
# restrict individual redd locations (simplify to reach level, no lat/longs)
# start with just adding spring run

gcs_auth(json_file = Sys.getenv("GCS_AUTH_FILE"))
gcs_global_bucket(bucket = Sys.getenv("GCS_DEFAULT_BUCKET"))

# read in battle creek redd data instead of standard redd data - this data was already manipulated

#TODOs
# look into JPE-datasets to review processing of raw data. Link to repo: (https://github.com/SRJPE/JPE-datasets/blob/main/data-raw/qc-markdowns/adult-holding-redd-and-carcass-surveys/battle-creek/battle_creek_redd_qc.Rmd)
# process new data the same way
# if there is an overlap on 2022 data, go with the newest version
# add a note on not pulling carcass, adult, and environmental data
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
redd_raw <- read_csv(here::here("data-raw", "battle_daily_redd.csv")) |>
  glimpse()

redd_2022 <- readxl::read_excel("data-raw/2022_BC_flowwest data.xlsx", sheet = 3) |>
  clean_names() |>
  glimpse()

redd_2023 <- readxl::read_excel("data-raw/2023_BC_flowwest data.xlsx", sheet = 4) |>
  clean_names() |>
  glimpse()

# upstream estimates raw / upstream_raw
upstream_estimates_raw <- read.csv(here::here("data-raw", "standard_adult_passage_estimate.csv")) |>
  filter(stream == "battle creek")

upstream_raw <- read.csv(here::here("data-raw", "standard_adult_upstream_passage.csv")) |>
  filter(stream == "battle creek")

# carcass raw
carcass_raw <- readxl::read_excel("data-raw/2022_BC_flowwest data.xlsx", sheet = 4) |> #TODO do we know which fields we want to keep?
  clean_names() |>
  glimpse()

# environmental raw
environmental_raw_2022 <- readxl::read_excel("data-raw/2022_BC_flowwest data.xlsx", sheet = 1) |> #TODO do we know which fields we want to keep?
  clean_names() |>
  glimpse()

environmental_raw_2023 <- readxl::read_excel("data-raw/2023_BC_flowwest data.xlsx", sheet = 1) |> #TODO do we know which fields we want to keep?
  clean_names() |>
  glimpse()

environmental_raw <- bind_rows(environmental_raw_2022, environmental_raw_2023)

# Adult data
adult_raw_2022 <- readxl::read_excel("data-raw/2022_BC_flowwest data.xlsx", sheet = 2) |> #TODO do we know which fields we want to keep?
  clean_names() |>
  glimpse()
adult_raw_2023 <- readxl::read_excel("data-raw/2023_BC_flowwest data.xlsx", sheet = 2) |> #TODO do we know which fields we want to keep?
  clean_names() |>
  rename(total_fish = live_adult, #is this a fair assumption of what this field is? it is inconsistent across years
         number_of_jacks = subset_that_are_jacks,
         point_x = x,
         point_y = y) |>
  glimpse()

# CLEANING DATA
# redd --------------------------------------------------------------------

# 2022 data
redd_2022_renamed <- redd_2022 |>
  rename(JPE_redd_id = redd_id,
         longitude = point_x,
         latitude = point_y,
         reach_sub_unit = reach,
         pre_redd_substrate_size = pre_sub, # unsure match
         tail_substrate_size = tail_sub,
         redd_measured = measure,
         why_not_measured = why_not_measure,
         pre_redd_depth = pre_redd, #unsure
         redd_pit_depth = pit_in, #unsure match
         redd_length = length_in,
         redd_width = width_in,
         flow_fps = water_velo,
         start_number_flow_meter = bomb_start,
         end_number_flow_meter = bomb_end,
         flow_meter_time = bomb_secon,
         start_number_flow_meter_80 = start_80,
         end_number_flow_meter_80 = end_80,
         flow_meter_time_80 = secs_80) |>
  mutate(survey_method = NA,
         reach_sub_unit = NA,
         redd_loc = NA,
         redd_substrate_size = NA,
         fish_guarding = NA,
         redd_tail_depth = NA,
         flow_meter = NA,
         survey = NA,
         run = NA,
         age_index = NA,
         date_measured = NA) |>
  select(-c(qa_qc, qa_qc_date, side_sub, fish_on_redd, comments, age_2, date_2, age_3,
          date_3, age_4, date_4, age_5, date_5)) |>  #removing these fields for now - TODO ask meaning
  glimpse()



#2023 data
redd_2023_renamed <- redd_2023 |>
  rename(longitude = x_3,
         latitude = y_4,
         pre_redd_substrate_size = pre_redd_substrate_in,
         redd_substrate_size = side_substrate_in, # unsure match
         tail_substrate_size = tailspill_substrate_in,
         redd_measured = fish_on_redd,
         pre_redd_depth = pre_redd_depth_in,
         redd_pit_depth = pit_depth_in,
         redd_tail_depth = tailspill_depth_in,
         redd_length = length_in,
         redd_width = width_in,
         start_number_flow_meter = flowmeter_start,
         end_number_flow_meter = flowmeter_end,
         flow_meter_time = flowmeter_time_s,
         start_number_flow_meter_80 = flowmeter_80_percent_start,
         end_number_flow_meter_80 = flowmeter_80_percent_end,
         flow_meter_time_80 = flowmeter_80_percent_time,
         age = redd_age) |>
  mutate(JPE_redd_id = NA,
         survey_method = NA,
         reach_sub_unit = NA,
         redd_loc = NA,
         fish_guarding = NA,
         why_not_measured = NA,
         survey = NA,
         run = NA,
         age_index = NA,
         date_measured = NA,
         tailspill = NA,
         fork = NA) |>
  select(-c(redd, gravel_type, comments, date_visit_2, age_visit_2, date_visit_3, age_visit_3,
            date_visit_4, age_visit_4, date_visit_5, age_visit_5, age_comments, x_35, y_36)) |>  #removing these fields for now - TODO ask meaning
  glimpse()

redd_raw_cleaned <- redd_raw |>
  mutate(redd_measured = as.character(redd_measured))

redd_raw <- bind_rows(redd_raw_cleaned, redd_2022_renamed,redd_2023_renamed)
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

unique(redd_raw$redd_substrate_size)

 # TODO it looks like this table is not working, therefore code below is broken
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
