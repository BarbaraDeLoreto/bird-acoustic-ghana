######DATA WRANGGLING AND ANALYSIS FOR RICHNESS (FOREST BIRDS) - 20 MIN TRAD SURVEY VS BIRDNET
# BARBARA OLIVEIRA DE LORETO 30/04/2026 - WITH CLAUDE SUPPORT

# DEPENDENCIES
# This script requires birdnet_wrangled.csv in the Outputs folder
# Run the BirdNET batch analysis script first to generate this file
# Data files required in the Data folder or RDM - see README for details


################################################################################
######## Objectives##############################################################
#########1.Create one object/output out of all traditional surveys one M and 1 E per subplot,
############ fix dates, names and times.
#########2. a. create a dataset with BirdNet detections 20 min survey
########### b. create a BN dataset with 24h surveys
#########3. Create output for Richness analysis (richness per point based on sruvey method)
############include: 1. tree-shade cover at a plot level 2. filter only forest birds 3. for 3 confidence levels 0.6, 0.7 and 0.8
#############a. one output sampling like for like 
#############b. another like for 24H
######## 4. Use multipiple models in a sensitivity analysis - for richness
############ a. plot resuts

# Load packages


library(data.table)
library(lubridate)
library(stringr)
library(readxl)
library(dplyr)
library(openxlsx) #having an issue with readxl
library(ggplot2)
library(gt)
library(tidyr)
library(purrr)
library(lme4)
library(DHARMa)
library(glmmTMB)
library(viridis)

###############################################################################
#OBJECTIVE 1
###############################################################################

##### create a reference table with scientific names - including alternative naming used by Robert

# Load BirdLife Ghana species list 
birdlife <- fread("D:/QBIO7008/Bird_accoustic/Data/GHA-Species_BirdlifeInternational.csv")

# Quick check
names(birdlife)
nrow(birdlife)

# Robert's name correction table (raw, unchanged) 
robert_naming <- read_xlsx("D:/QBIO7008/Bird_accoustic/Data/Bird_Naming_Robert.xlsx") |>
  filter(!is.na(Name_Robert))

# Combined species lookup: Robert's corrections + BirdLife scientific names
species_lookup <- robert_naming |>
  left_join(birdlife[, c("CommonName", "ScientificName")],
            by = c("Name_Dropdown" = "CommonName")) |>
  select(Name_Robert, Name_Dropdown, scientific_name = ScientificName)

# Check
species_lookup
cat("With scientific name:", sum(!is.na(species_lookup$scientific_name)), "\n")
cat("Still missing:       ", sum(is.na(species_lookup$scientific_name)), "\n")

##### load survey data

# Load individual survey spreadsheets (Manually_check == "Y")

# Helper function to load and filter each file
# ── Load individual survey spreadsheets (Manually_checked == "Y") ─────────────
load_survey <- function(path) {
  df <- read.xlsx(path, sheet = "Survey Data")
  names(df) <- trimws(names(df))
  check_col <- grep("manual", names(df), ignore.case = TRUE, value = TRUE)[1]
  df <- df[!is.na(df[[check_col]]) & toupper(df[[check_col]]) == "Y", ]
  df
}

load_survey1 <- function(path) {
  df <- read.xlsx(path, sheet = "Sheet1")
  names(df) <- trimws(names(df))
  check_col <- grep("manual", names(df), ignore.case = TRUE, value = TRUE)[1]
  df <- df[!is.na(df[[check_col]]) & toupper(df[[check_col]]) == "Y", ]
  df
}

#load each survey

c01_checked  <- load_survey1("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C01.xlsx")
c05_checked  <- load_survey1("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C05.xlsx")
c08_checked  <- load_survey1("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C08.xlsx")
c09_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C09.xlsx")   
c11_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C11.xlsx")
c16_checked <- load_survey1("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C16.xlsx")
c19_checked <- load_survey1("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C19.xlsx")
c22_checked  <- load_survey1("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C22.xlsx")
c26_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C26.xlsx")
c27_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots//C27_all.xlsx")
c30_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C30.xlsx")
c39_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C39.xlsx")
c52_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C52.xlsx")
c58_checked  <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C58.xlsx")
c100_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C100.xlsx")
c102_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C102.xlsx")
c111_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C111.xlsx")
c116_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C116.xlsx") 
c117_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C117.xlsx") 
c131_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C131.xlsx")
c141_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C141.xlsx")
c1180_checked <- load_survey("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/02_Data_Sheets/02_Cocoa Plots/C1180.xlsx")

#### Merge all into one object 
all_surveys_checked <- bind_rows(
  lapply(list(
    c01_checked, c05_checked, c08_checked,
    c09_checked, c19_checked, c16_checked, c116_checked,  
    c11_checked, c22_checked, c26_checked, c27_checked, c30_checked,
    c39_checked, c52_checked, c58_checked, 
    c100_checked, c102_checked,
    c111_checked, c117_checked, c131_checked, c141_checked, c1180_checked
  ), function(df) mutate(df, across(everything(), as.character)))
)


# Check
nrow(all_surveys_checked)
unique(all_surveys_checked$plot_ID)
all_surveys_checked[is.na(all_surveys_checked$plot_ID), ]

# removing the weird line that has NAs
all_surveys_checked <- all_surveys_checked[!is.na(all_surveys_checked$plot_ID), ]

# Find which plot has non-NA values in X21
all_surveys_checked |>
  filter(!is.na(X21)) |>
  distinct(plot_ID, X21)
#no idea why and checked the sheet, so just deleting it
all_surveys_checked <- all_surveys_checked |>
  select(-X21)

# some Y in caps and others no
all_surveys_checked <- all_surveys_checked |>
  mutate(Agent_entered = toupper(Agent_entered))

all_surveys_checked <- all_surveys_checked |>
  mutate(Manually_checked = toupper(Manually_checked))


# Only select points 
all_surveys_checked <- all_surveys_checked |>
  filter (type == "point")

# Check
nrow(all_surveys_checked) #2501
unique(all_surveys_checked$ID) #53


##### adjust variable types, errors etc etc

#First species name

# Scientific names added using BirdLine spreadsheet 
all_surveys_checked <- all_surveys_checked |>
  left_join(birdlife[, c("CommonName", "ScientificName")],
            by = c("common_name" = "CommonName"))

# For unmatched, apply Robert's correction then match to BirdLife 
all_surveys_checked <- all_surveys_checked |>
  left_join(robert_naming[, c("Name_Robert", "Name_Dropdown")],
            by = c("common_name" = "Name_Robert")) |>
  left_join(birdlife[, c("CommonName", "ScientificName")],
            by = c("Name_Dropdown" = "CommonName"),
            suffix = c("", "_robert")) |>
  mutate(
    scientific_name = ifelse(!is.na(ScientificName), ScientificName, ScientificName_robert)
  ) |>
  select(-ScientificName, -ScientificName_robert, -Name_Dropdown)

# Check coverage 
cat("With scientific name:", sum(!is.na(all_surveys_checked$scientific_name)), "\n")
cat("Still missing:       ", sum(is.na(all_surveys_checked$scientific_name)), "\n")

# Which species are still missing?
all_surveys_checked |>
  filter(is.na(scientific_name)) |>
  distinct(common_name)

# #Manual corrections for the common names that were not matched to scientific names
# due to weird typos, introduction of spaces, hifens and reading issues

# Apply corrections and get scientific names inline 
all_surveys_checked <- all_surveys_checked |>
  mutate(corrected_name = case_when(
    common_name == "Grey-crowned Nigrita"             ~ "Grey-headed Nigrita",
    common_name == "Gray-headed Bristlebill"          ~ "Grey-headed Bristlebill",
    common_name == "black-throated coucal"            ~ "Black-throated Coucal",
    common_name == "West Africa Wattle-eye"           ~ "West African Wattle-eye",
    common_name == "African Green Pigeon"             ~"African Green-pigeon",
    common_name == "Little Grey Greenbul"             ~ "Little Grey Greenbul",
    common_name == "Africa Goshawk"                   ~ "Red-chested Goshawk",
    common_name == "African Goshawk"                  ~ "Red-chested Goshawk",
    common_name == "Senegal Bulbul"                   ~ "Common Bulbul",
    common_name == "Senegal coucal"                   ~ "Senegal Coucal",
    common_name == "West African Red Hornbill"        ~ "West African Pied Hornbill",
    common_name == "West African Pied Hornbill "       ~ "West African Pied Hornbill",
    common_name == "Black-and-white Casqued Hornbill" ~ "Black-and-white-casqued Hornbill",
    common_name == "African Harrier-Hawk"             ~ "African Harrier-hawk",
    common_name == "Finch’s Flycatcher"               ~ "Finsch's Flycatcher-thrush",
    common_name == "Black-headed Paradise Flycatcher" ~ "Red-bellied Paradise-flycatcher",
    common_name == "Black-and-white Shrike-flycatcher" ~ "Red-bellied Paradise-flycatcher",
    common_name == "Black-and-white Shrike-flycatcher " ~ "Red-bellied Paradise-flycatcher",
    common_name == "Kemp’s Longbill"                  ~ "Kemp's Longbill",
    common_name == "Blue-spotted Wood Dove"           ~ "Blue-spotted Wood-dove",
    common_name == "Blue-spotted Wood-Dove"           ~ "Blue-spotted Wood-dove",
    common_name == "West Africa Red Hornbill"         ~ "West African Pied Hornbill",
    common_name == "Africa pied hornbill"             ~ "West African Pied Hornbill",
    common_name == "West African Pied Hornbil"        ~ "West African Pied Hornbill",
    common_name == "West African Pied Hornbill"       ~ "West African Pied Hornbill",
    common_name == "West African Pied Hornbill"       ~ "West African Pied Hornbill",
    common_name == "Taborine Dove"                    ~ "Tambourine Dove",
    common_name == "Black-winged oriole"              ~ "Black-winged Oriole",
    common_name == "Simple leavelove"                 ~ "Simple Greenbul",
    common_name == "Swamp Palm Greenbul"              ~ "Swamp Palm Bulbul",
    common_name == "Black-necked Weaver"              ~ "Olive-naped Weaver",
    common_name == "Black-headed paradize flycatcher" ~ "Red-bellied Paradise-flycatcher",
    common_name == "Black-and-white Shrike-flycatcher" ~ "Red-bellied Paradise-flycatcher",
    common_name == "Tamborine Dove"                   ~ "Tambourine Dove",
    common_name == "Blued_throated Coucal"  ~ "Black-throated Coucal",
    common_name == "Chattering Yellowbill "           ~"Chattering Yellowbill",
    common_name == "Green Turaco "                    ~ "Green Turaco", 
    common_name == "West Africa Pied Hornbill"        ~ "West African Pied Hornbill",
    common_name == "Puvel’s Illadopsis"               ~"Puvel's Illadopsis",
    common_name == "Vieillot’s Barbet"                ~ "Vieillot's Barbet",
    common_name == "Cameroon Sombre Greenbull"        ~"Plain Greenbul", 
    TRUE ~ NA_character_
  )) |>
  mutate(
    scientific_name = ifelse(
      is.na(scientific_name) & !is.na(corrected_name),
      birdlife$ScientificName[match(corrected_name, birdlife$CommonName)],
      scientific_name
    )
  ) |>
  select(-corrected_name)

# Check coverage 
cat("With scientific name:", sum(!is.na(all_surveys_checked$scientific_name)), "\n")
cat("Still missing:       ", sum(is.na(all_surveys_checked$scientific_name)), "\n")

all_surveys_checked |>
  filter(is.na(scientific_name)) |>
  distinct(common_name)


# inspect the 9 in more details
all_surveys_checked |>
  filter(is.na(scientific_name)) |>
  select(plot_ID, ID, common_name, No..of.individual, comment) |>
  data.frame() |>
  print()

#NA and Zero are surveys with no Birds recorded
# Remove only the unresolvable species records - FOR NOW
remove_names <- c("Woodpecker", "Bush-shrike", "Vieillot's Black Weaver", 
                  "Olive Greenbul", "Grey-headed Nicator", "Cuckoo-shrike")

all_surveys_checked <- all_surveys_checked |>
  filter(!(common_name %in% remove_names & !is.na(common_name)))

# Confirm
cat("Remaining rows:           ", nrow(all_surveys_checked), "\n")
cat("Remaining missing sci name:", sum(is.na(all_surveys_checked$scientific_name)), "\n")

# Verify the zero-bird records are still there
all_surveys_checked |>
  filter(is.na(common_name) | common_name == "0") |>
  select(plot_ID, ID, common_name, No..of.individual, comment) |>
  data.frame()

# Homogenise zero-bird survey records: standardise species field to NA
all_surveys_checked <- all_surveys_checked |>
  mutate(
    common_name = ifelse(common_name == "0" | common_name == "None", NA_character_, common_name),
    scientific_name = ifelse(is.na(common_name), NA_character_, scientific_name),
    No..of.individual = ifelse(is.na(common_name), 0L, No..of.individual)
  )

# Verify zero-bird records look right
all_surveys_checked |>
  filter(is.na(common_name)) |>
  select(plot_ID, ID, common_name, scientific_name, No..of.individual, comment) |>
  data.frame()


# Check
nrow(all_surveys_checked) #2502
unique(all_surveys_checked$plot_ID) #22
unique(all_surveys_checked$ID) #53


# for dates

# Quick check dates 
all_surveys_checked |>
  select(plot_ID, date_DD_MM_YY) |>
  distinct() |>
  head(50)

# Fix Excel serial number dates 
all_surveys_checked <- all_surveys_checked |>
  mutate(date_DD_MM_YY = case_when(
    grepl("^\\d{5}$", date_DD_MM_YY) ~ format(
      as.Date(as.numeric(date_DD_MM_YY), origin = "1899-12-30"), 
      "%d/%m/%Y"),
    TRUE ~ date_DD_MM_YY
  ))

# Check
all_surveys_checked |>
  select(plot_ID, date_DD_MM_YY) |>
  distinct() |>
  arrange(plot_ID)


# Any remaining NAs?
all_surveys_checked |>
  filter(is.na(date_DD_MM_YY)) |>
  distinct(plot_ID, date_DD_MM_YY)

# For times

# Check what time formats are present 
all_surveys_checked |>
  select(plot_ID, start_time, end_time) |>
  distinct() |>
  arrange(plot_ID) |>
  head(30)

# Convert Excel decimal fractions to HH:MM for all time columns 
all_surveys_checked <- all_surveys_checked |>
  mutate(
    start_time = {
      total_minutes <- round(as.numeric(start_time) * 24 * 60)
      sprintf("%02d:%02d", total_minutes %/% 60, total_minutes %% 60)
    },
    end_time = {
      total_minutes <- round(as.numeric(end_time) * 24 * 60)
      sprintf("%02d:%02d", total_minutes %/% 60, total_minutes %% 60)
    }
  )

# Check
all_surveys_checked |>
  select(plot_ID, start_time, end_time) |>
  distinct() |>
  arrange(plot_ID) |>
  head(30)


# Check for remaining NAs 
all_surveys_checked |>
  filter(is.na(start_time) | is.na(end_time)) |>
  select(plot_ID, ID, survey_M_E, date_DD_MM_YY, start_time, end_time) |>
  distinct()

# Check all looks sensible 
all_surveys_checked |>
  select(plot_ID, start_time, end_time) |>
  distinct() |>
  arrange(plot_ID) |>
  head(30)

#check
unique(all_surveys_checked$plot_ID) #22
unique(all_surveys_checked$ID)#53

# Get unique survey windows from all_surveys_checked | create a daytime object because that is what is in Birdnet─
survey_windows <- all_surveys_checked |>
  distinct(ID, survey_M_E, date_DD_MM_YY, start_time, end_time) |>
  mutate(
    survey_start = dmy_hm(paste(date_DD_MM_YY, start_time), tz = "Africa/Accra"),
    survey_end   = dmy_hm(paste(date_DD_MM_YY, end_time),   tz = "Africa/Accra")
  )
#123 survey windows

#how many morning survey windows
survey_windows|> filter(survey_M_E == "M")|>nrow()
#63 morning surveys
survey_windows|> filter(survey_M_E == "E")|>nrow()
#60 evening surveys

# check if includes date and time
class(survey_windows$survey_start)
class(survey_windows$survey_end)

# Also check a few values
head(survey_windows[, c("survey_start", "survey_end")])

# Traditional survey duration per window
trad_duration <- survey_windows |>
  mutate(duration_minutes = as.numeric(difftime(survey_end, survey_start, units = "mins"))) |>
  select(ID, survey_M_E, survey_start, survey_end, duration_minutes) #C11_D evening is 20 min (raw data)

# Check survey coverage: which subplots have at least one M and one E
all_surveys_checked |>
  distinct(plot_ID, ID, survey_M_E) |>
  group_by(ID) |>
  summarise(
    has_M = any(survey_M_E == "M"),
    has_E = any(survey_M_E == "E"),
    surveys_present = paste(sort(unique(survey_M_E)), collapse = ", ")
  ) |>
  arrange(has_M & has_E) |>
  data.frame() #50 have a M and E (C27_B, C30_C, C52_O don't have two surveys) 
#i will reconsider this again after checking if these are present in BirdNET or not

# Check per plot: number of subplots and M/E coverage
all_surveys_checked |>
  distinct(plot_ID, ID, survey_M_E) |>
  group_by(plot_ID, ID) |>
  summarise(
    has_M = any(survey_M_E == "M"),
    has_E = any(survey_M_E == "E"),
    .groups = "drop"
  ) |>
  group_by(plot_ID) |>
  summarise(
    n_subplots      = n(),
    n_with_M        = sum(has_M),
    n_with_E        = sum(has_E),
    n_with_both_ME  = sum(has_M & has_E),
    subplots        = paste(sort(unique(ID)), collapse = ", "),
    .groups = "drop"
  ) |>
  data.frame() 

#create output file with all traditional surveys to be used
write.csv(all_surveys_checked, file = "D:/QBIO7008/Bird_accoustic/Outputs/all_traditional_surveys.csv")

###############################################################################
#OBJECTIVE 2
###############################################################################

##### create a BirdNET dataset like-for like

# Load wrangled BirdNET data
bn_w <- fread("D:/QBIO7008/Bird_accoustic/Outputs/birdnet_wrangled.csv")
names(bn_w)

# Check which subplot_IDs in all_surveys_checked are not in bn_w 
missing_in_bn <- all_surveys_checked |>
  distinct(ID) |>
  filter(!ID %in% bn_w$subplot_ID)

cat("Subplots in traditional survey but not in bn_w:\n")
missing_in_bn

# Filter BirdNET to all survey time windows 
bn_like_for_like <- survey_windows |>
  rowwise() |>
  reframe(
    cbind(
      survey_M_E = survey_M_E,
      trad_date  = date_DD_MM_YY,
      bn_w[subplot_ID == ID &
             detection_datetime >= survey_start &
             detection_datetime <= survey_end]
    )
  )

# Check
nrow(bn_like_for_like)#32352
head(bn_like_for_like)
str(bn_like_for_like)
unique(bn_like_for_like$plot_ID) #22
unique(bn_like_for_like$subplot_ID) #47 - already 6 subplots dropped

#fixing some of the columns
bn_like_for_like <- bn_like_for_like |>
  filter(!is.na(deployment)) |> #removed NAs created in previous filtering
  mutate(
    confidence  = as.numeric(confidence),
    start_time  = as.integer(start_time),
    end_time    = as.integer(end_time),
    recording_start    = as.POSIXct(recording_start, tz = "Africa/Accra"),
    detection_datetime = as.POSIXct(detection_datetime, tz = "Africa/Accra")
  )

# Check
str(bn_like_for_like)

#check
bn_like_for_like |>
  distinct(subplot_ID, survey_M_E, trad_date) |>
  nrow() #102 #21 surveys dropped

#Check
unique(bn_like_for_like$subplot_ID)
bn_like_for_like|> filter(survey_M_E == "M")|>
  distinct(subplot_ID, survey_M_E, trad_date) |>
  nrow()
#54 birdnet surveys 

unique(bn_like_for_like$subplot_ID)
bn_like_for_like|> filter(survey_M_E == "E")|>
  distinct(subplot_ID, survey_M_E, trad_date) |>
  nrow()
# 48 BirdNET surveys - 16 surveys don't have detections or were not recorded

# which subplots have at least one M and one E in bn_like_for_like
bn_like_for_like |>
  distinct(subplot_ID, survey_M_E) |>
  group_by(subplot_ID) |>
  summarise(
    has_M = any(survey_M_E == "M"),
    has_E = any(survey_M_E == "E"),
    surveys_present = paste(sort(unique(survey_M_E)), collapse = ", ")
  ) |>
  arrange(has_M & has_E) |>
  data.frame() #(C58_B, C27_B, C19_D, c102_O, C08_O, C08_A don't have two surveys) 


# per plot summary
bn_like_for_like |>
  distinct(subplot_ID, survey_M_E) |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+")) |>
  group_by(plot_ID, subplot_ID) |>
  summarise(
    has_M = any(survey_M_E == "M"),
    has_E = any(survey_M_E == "E"),
    .groups = "drop"
  ) |>
  group_by(plot_ID) |>
  summarise(
    n_subplots     = n(),
    n_with_M       = sum(has_M),
    n_with_E       = sum(has_E),
    n_with_both_ME = sum(has_M & has_E),
    subplots       = paste(sort(unique(subplot_ID)), collapse = ", "),
    .groups = "drop"
  ) |>
  data.frame() # 1 plot C102 has no subplots at all with 2 surveys (M and E)


#### checking the missalignment of BN and all_surveys
# what subplot/survey combinations exist in survey_windows (should exist)
expected <- survey_windows |>
  distinct(ID, survey_M_E) |>
  rename(subplot_ID = ID)

# what a has detections in bn_like_for_like
detected <- bn_like_for_like |>
  distinct(subplot_ID, survey_M_E)

# check if bn_w has ANY detections within each survey window
# (before confidence filtering - use bn_w directly)

recorded_in_window <- survey_windows |>
  rowwise() |>
  reframe(
    subplot_ID = ID,
    survey_M_E = survey_M_E,
    n_raw_detections = nrow(bn_w[bn_w$subplot_ID == ID &
                                   bn_w$detection_datetime >= survey_start &
                                   bn_w$detection_datetime <= survey_end, ])
  )

# Now check
expected |>
  left_join(detected |> mutate(has_detections = TRUE),
            by = c("subplot_ID", "survey_M_E")) |>
  left_join(recorded_in_window, by = c("subplot_ID", "survey_M_E")) |>
  mutate(
    has_detections   = replace_na(has_detections, FALSE),
    n_raw_detections = replace_na(n_raw_detections, 0),
    diagnosis = case_when(
      has_detections                        ~ "OK - detections present",
      n_raw_detections > 0 & !has_detections ~ "RECORDED - no detections passed filter",
      n_raw_detections == 0                  ~ "NOT RECORDED - no output in window"
    )
  ) |>
  filter(diagnosis != "OK - detections present") |>
  arrange(diagnosis, subplot_ID) |>
  data.frame()

# check survey window again
survey_windows |>
  filter(ID %in% c("C117_D", "C26_C")) |>
  select(ID, survey_M_E, survey_start, survey_end) |>
  data.frame()


# inspect the filteres object #duration will not be perfect as it is based on detection and not the survey window per se. So it might be slightly smaller
filter_check <- bn_like_for_like |>
  group_by(subplot_ID, trad_date, survey_M_E) |>
  summarise(
    first_detection = min(detection_datetime),
    last_detection  = max(detection_datetime),
    duration_min    = as.numeric(difftime(max(detection_datetime),
                                          min(detection_datetime),
                                          units = "mins")),
    n_detections    = n(),
    .groups = "drop"
  ) |>
  arrange(subplot_ID, trad_date, survey_M_E)


# number of survey windows vs filter_check rows 
cat("Survey windows:", nrow(survey_windows), "\n")
cat("Filter check rows:", nrow(filter_check), "\n") #21 surveys not included in BirdNET filtered

# Find which survey windows have no BirdNET detections 
#first create a key in each object
filter_check_ids <- filter_check |>
  mutate(key = paste(subplot_ID, trad_date, survey_M_E, sep = "_"))

survey_windows_ids <- survey_windows |>
  mutate(key = paste(ID, date_DD_MM_YY, survey_M_E, sep = "_"))

# Missing windows
missing_windows <- survey_windows_ids |>
  filter(!key %in% filter_check_ids$key) |>
  select(ID, survey_M_E, date_DD_MM_YY, survey_start, survey_end)

missing_windows #21 surveys

# Export missing windows for manual checking of raw audio data
missing_windows |>
  mutate(
    notes = NA_character_  # blank column for you to fill in during checking
  ) |>
  write.csv("D:/QBIO7008/Bird_accoustic/Outputs/missing_bn_windows_to_check.csv",
            row.names = FALSE)

# only ran for checks - run only if you have access to the file below
# # Load catalog of acoustic files #still some issues with this object
# catalog <- fread("D:/QBIO7008/Bird_accoustic/Outputs/recording_catalog_all_blocks.csv") |>
#   mutate(datetime = as.POSIXct(datetime, tz = "UTC"))
# 
# # For each missing window, check if any catalog file covers that time slot
# # A file "covers" a window if it starts <= survey_start and the next file 
# # starts > survey_start (i.e. the survey falls within the recording)
# 
# missing_windows_checked <- missing_windows |>
#   rowwise() |>
#   mutate(
#     # Find all files from that subplot on that date
#     candidate_files = list(
#       catalog |>
#         filter(
#           subplot == ID,
#           date    == as.Date(survey_start)
#         )
#     ),
#     # Check if any file starts at or before survey_start
#     # and within 1 hour (one recording chunk)
#     covering_file = {
#       cf <- candidate_files
#       if (nrow(cf) == 0) {
#         NA_character_
#       } else {
#         cf <- cf |>
#           filter(datetime <= survey_start,
#                  datetime >= survey_start - 3600)  # within 1hr before
#         if (nrow(cf) == 0) NA_character_ else cf$filename[which.max(cf$datetime)]
#       }
#     },
#     recording_exists = !is.na(covering_file)
#   ) |>
#   ungroup() |>
#   select(ID, survey_M_E, date_DD_MM_YY, survey_start, survey_end,
#          recording_exists, covering_file)
# 
# # Summary
# cat("Missing windows WITH a covering recording:", sum(missing_windows_checked$recording_exists), "\n")
# cat("Missing windows with NO recording at all: ", sum(!missing_windows_checked$recording_exists), "\n")
# 
# # Full table
# missing_windows_checked |> data.frame() |> print()
# 
# # Save
# fwrite(missing_windows_checked,
#        "D:/QBIO7008/Bird_accoustic/Outputs/missing_windows_checked.csv")
# 

# write the dataset Bn_like-for-like
write.csv(bn_like_for_like,
          "D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like.csv",
          row.names = FALSE)

# plot level coverage summary
bn_like_for_like |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+")) |>
  distinct(plot_ID, subplot_ID, survey_M_E) |>
  group_by(plot_ID, subplot_ID) |>
  summarise(has_M = any(survey_M_E == "M"),
            has_E = any(survey_M_E == "E"),
            .groups = "drop") |>
  group_by(plot_ID) |>
  summarise(
    n_subplots_with_BN    = n(),
    n_subplots_with_both  = sum(has_M & has_E),
    subplots_ok           = paste(subplot_ID[has_M & has_E], collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(plot_ID) |>
  write.csv("D:/QBIO7008/Bird_accoustic/Outputs/BN_like_for_like_coverage.csv",
            row.names = FALSE)

#C102 does not have any subplots with morning AND eving recordings. it is either one of the other.
#moving forward with 21 plots


#######################################################################
######## filter forest birds###########################################
#######################################################################

# load the forest birds data
forest_birds <- fread("D:/QBIO7008/Bird_accoustic/Data/forest_bird_species_list.csv")

#check
head(forest_birds)

# create forest_species flag in all_surveys_checked 
all_surveys_checked <- all_surveys_checked |>
  mutate(forest_species = if_else(
    scientific_name %in% forest_birds$scientific_name_birdlife,
    "Y", "N"
  ))

# Check
table(all_surveys_checked$forest_species, useNA = "always")

# Which species are flagged as forest species?
all_surveys_checked |>
  filter(forest_species == "Y") |>
  distinct(common_name, scientific_name) |>
  arrange(scientific_name)

# now have a filtered set
all_surveys_forest_birds <- all_surveys_checked |> filter (forest_species == "Y")
nrow (all_surveys_forest_birds)

#Check all looks sensible 
all_surveys_forest_birds |>
  select(plot_ID, start_time, end_time) |>
  distinct() |>
  arrange(plot_ID) |>
  head(30)

#check
unique(all_surveys_forest_birds$plot_ID) #22 - no deletions with filtering
unique(all_surveys_forest_birds$ID) #53 - no deletions with filtering

# Save traditional surveys filtered for forest 
write.csv(all_surveys_forest_birds,
          "D:/QBIO7008/Bird_accoustic/Outputs/all_surveys_forest_birds.csv",
          row.names = FALSE)

# Check if survey widows decreased because of the filtering
all_surveys_forest_birds |>
  distinct(ID, survey_M_E, date_DD_MM_YY, start_time, end_time) |>
  mutate(
    survey_start = dmy_hm(paste(date_DD_MM_YY, start_time), tz = "Africa/Accra"),
    survey_end   = dmy_hm(paste(date_DD_MM_YY, end_time),   tz = "Africa/Accra")
  )
#121 seems like 2 have dropped

# check if includes date and time
class(survey_windows$survey_start)
class(survey_windows$survey_end)

# no need to re-filter BN data - only for the forest species
bn_like_for_like_forest<- bn_like_for_like |>
  mutate(forest_species = if_else(
    scientific_name %in% forest_birds$scientific_name_birdlife,
    "Y", "N"
  ))

# Check
table(bn_like_for_like_forest$forest_species, useNA = "always")

# Which species are flagged as forest species? # just for curiosity
bn_like_for_like_forest |>
  filter(forest_species == "Y") |>
  distinct(common_name, scientific_name) |>
  arrange(scientific_name)

# now have a filtered set
bn_like_for_like_forest <- bn_like_for_like_forest |> filter (forest_species == "Y")
nrow (bn_like_for_like_forest) #22838

# Save bn like for like filtered for forest 
write.csv(bn_like_for_like_forest,
          "D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like_forest.csv",
          row.names = FALSE)

#checks
# Traditional surveys - forest birds only
all_surveys_forest_birds |>
  distinct(plot_ID, ID, survey_M_E) |>
  group_by(plot_ID, ID) |>
  summarise(has_M = any(survey_M_E == "M"),
            has_E = any(survey_M_E == "E"),
            .groups = "drop") |>
  group_by(plot_ID) |>
  summarise(
    n_subplots      = n(),
    n_with_both_ME  = sum(has_M & has_E),
    subplots_ok     = paste(ID[has_M & has_E], collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(plot_ID) |>
  data.frame() #still 49 subplots of and #22 plots

# BirdNET like-for-like - forest birds only
bn_like_for_like_forest |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+")) |>
  distinct(plot_ID, subplot_ID, survey_M_E) |>
  group_by(plot_ID, subplot_ID) |>
  summarise(has_M = any(survey_M_E == "M"),
            has_E = any(survey_M_E == "E"),
            .groups = "drop") |>
  group_by(plot_ID) |>
  summarise(
    n_subplots_with_BN    = n(),
    n_subplots_with_both  = sum(has_M & has_E),
    subplots_ok           = paste(subplot_ID[has_M & has_E], collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(plot_ID) |>
  data.frame() #two plots are not ok 102 and 141 (only have a morning or evening survey)
# 40 subplots and 20 plots moving forward

# Before forest filter
bn_like_for_like |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+")) |>
  distinct(plot_ID, subplot_ID, survey_M_E) |>
  group_by(plot_ID, subplot_ID) |>
  summarise(has_M = any(survey_M_E == "M"),
            has_E = any(survey_M_E == "E"),
            .groups = "drop") |>
  group_by(plot_ID) |>
  summarise(
    n_subplots_with_both  = sum(has_M & has_E),
    subplots_ok           = paste(subplot_ID[has_M & has_E], collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(plot_ID) -> coverage_before

# After forest filter
bn_like_for_like_forest |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+")) |>
  distinct(plot_ID, subplot_ID, survey_M_E) |>
  group_by(plot_ID, subplot_ID) |>
  summarise(has_M = any(survey_M_E == "M"),
            has_E = any(survey_M_E == "E"),
            .groups = "drop") |>
  group_by(plot_ID) |>
  summarise(
    n_subplots_with_both  = sum(has_M & has_E),
    subplots_ok           = paste(subplot_ID[has_M & has_E], collapse = ", "),
    .groups = "drop"
  ) |>
  arrange(plot_ID) -> coverage_after

# Compare
left_join(coverage_before, coverage_after, 
          by = "plot_ID", 
          suffix = c("_before", "_after")) |>
  data.frame()

#conclusion - moving forward with 20 subplots - 102 was already missing overlaping Birnet data, 141 is missing forest birds data so should be zero

##### create a BirdNET dataset # 18h standardised window: 14:00 day 1 to 08:00 day 2
# Anchor: 14:00 on the date of first detection per subplot from bn_w
# C102 excluded - insufficient recording duration

subplots_in_analysis <- unique(all_surveys_forest_birds$ID) # already missing 102

all_anchors <- bn_w[subplot_ID %in% subplots_in_analysis,
                    .(first_detection = min(recording_start)),
                    by = subplot_ID] |>
  mutate(
    window_start = as.POSIXct(
      paste(as.Date(first_detection), "14:00:00"),
      tz = "Africa/Accra"),
    window_end = window_start + hours(18)
  ) |>
  select(subplot_ID, window_start, window_end)

bn_18h <- bn_w[subplot_ID %in% subplots_in_analysis] |>
  filter(!startsWith(subplot_ID, "C102")) |>
  left_join(all_anchors, by = "subplot_ID") |>
  filter(!is.na(window_start),
         detection_datetime >= window_start,
         detection_datetime <  window_end)

# Check
cat("Detections in 18h window:", nrow(bn_18h), "\n")
cat("Unique subplots:", uniqueN(bn_18h$subplot_ID), "\n") #50
cat("Unique plots:", uniqueN(bn_18h$plot_ID), "\n")



# Apply forest bird filter
bn_18h_forest <- bn_18h |>
  mutate(forest_species = if_else(
    scientific_name %in% forest_birds$scientific_name_birdlife,
    "Y", "N"
  )) |>
  filter(forest_species == "Y")

# Check
cat("Detections after forest filter:", nrow(bn_18h_forest), "\n")
cat("Unique subplots:", uniqueN(bn_18h_forest$subplot_ID), "\n") #46
cat("Unique plots:", uniqueN(bn_18h_forest$plot_ID), "\n")

bn_18h |>
  group_by(subplot_ID) |>
  summarise(
    hours_first_to_last = as.numeric(difftime(max(detection_datetime),
                                              min(detection_datetime),
                                              units = "hours")),
    .groups = "drop"
  ) |>
  arrange(hours_first_to_last) |>
  print(n = 50)

#drop the ones that don't have the recording window
# Check which subplots will be dropped
bn_18h |>
  group_by(subplot_ID) |>
  summarise(
    hours_first_to_last = as.numeric(difftime(max(detection_datetime),
                                              min(detection_datetime),
                                              units = "hours")),
    .groups = "drop"
  ) |>
  filter(hours_first_to_last < 17) |>
  arrange(hours_first_to_last)

subplots_18h_ok <- bn_18h |>
  group_by(subplot_ID) |>
  summarise(
    hours_first_to_last = as.numeric(difftime(max(detection_datetime),
                                              min(detection_datetime),
                                              units = "hours")),
    .groups = "drop"
  ) |>
  filter(hours_first_to_last >= 17) |>
  pull(subplot_ID)

cat("Subplots retained:", length(subplots_18h_ok), "\n")

# Apply subplot filter to both objects
bn_18h <- bn_18h |> filter(subplot_ID %in% subplots_18h_ok)
bn_18h_forest <- bn_18h_forest |> filter(subplot_ID %in% subplots_18h_ok)

# Check
cat("Unique subplots after drop:", uniqueN(bn_18h$subplot_ID), "\n") #43
cat("Unique plots after drop:", uniqueN(bn_18h$plot_ID), "\n") #21
cat("Detections after drop:", nrow(bn_18h_forest), "\n")

# Save
fwrite(bn_18h_forest, "D:/QBIO7008/Bird_accoustic/Outputs/bn_cocoa_18h_forest.csv")

# conclusion BN18h data does not have all plots and subplots because they dont have 18h

###############################################################################
# OBJECTIVE 3 - RICHNESS DATASETS (ALL VALIDATED SUBPLOTS)
###############################################################################

# Define confidence thresholds for sensitivity analysis
thresholds <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# Define validated subplot list - only subplots with both M and E in bn_like_for_like
validated_subplots <- bn_like_for_like |>
  distinct(subplot_ID, survey_M_E) |>
  group_by(subplot_ID) |>
  summarise(has_M = any(survey_M_E == "M"),
            has_E = any(survey_M_E == "E"),
            .groups = "drop") |>
  filter(has_M & has_E) |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+")) |>
  select(subplot_ID, plot_ID)

# Check
cat("Total validated subplots:", nrow(validated_subplots), "\n")
cat("Total plots:             ", n_distinct(validated_subplots$plot_ID), "\n")
validated_subplots |>
  group_by(plot_ID) |>
  summarise(n = n(), subplots = paste(sort(subplot_ID), collapse = ", ")) |>
  data.frame()

####BN like-for-like richness (pooled M+E, all validated subplots)

bn_lfl_richness <- map_dfr(thresholds, function(thr) {
  bn_like_for_like_forest |>
    filter(confidence >= thr,
           subplot_ID %in% validated_subplots$subplot_ID) |>
    group_by(subplot_ID) |>
    summarise(richness = n_distinct(scientific_name), .groups = "drop") |>
    mutate(threshold = thr) |>
    right_join(validated_subplots |> select(subplot_ID), by = "subplot_ID") |>
    mutate(richness  = replace_na(richness, 0),
           threshold = replace_na(threshold, thr))
}) |>
  pivot_wider(names_from = threshold, values_from = richness,
              names_prefix = "richness_bn_lfl_") |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+"))

# Check
nrow(bn_lfl_richness)
map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_lfl_", thr)
  bn_lfl_richness |>
    summarise(n_subplots  = n(),
              n_with_data = sum(.data[[col]] > 0),
              n_zero      = sum(.data[[col]] == 0)) |>
    mutate(threshold = thr)
}) |> data.frame()

write.csv(bn_lfl_richness,
          "D:/QBIO7008/Bird_accoustic/Outputs/bn_lfl_richness_all_subplots.csv",
          row.names = FALSE)

####BN 18h richness (all validated subplots in 18h window) ----
# 7 subplots dropped due to insufficient recording coverage (< 17h of detections)
# C102 excluded - recordings end at 23:00 on day 1
# Window: 14:00 day 1 to 08:00 day 2

bn_18h_richness <- map_dfr(thresholds, function(thr) {
  bn_18h_forest |>
    filter(confidence >= thr,
           subplot_ID %in% validated_subplots$subplot_ID) |>
    group_by(subplot_ID) |>
    summarise(richness = n_distinct(scientific_name), .groups = "drop") |>
    mutate(threshold = thr) |>
    # right_join only to subplots actually in bn_18h - not all validated subplots
    right_join(
      validated_subplots |>
        filter(subplot_ID %in% unique(bn_18h$subplot_ID)) |>
        select(subplot_ID),
      by = "subplot_ID") |>
    mutate(richness  = replace_na(richness, 0),
           threshold = replace_na(threshold, thr))
}) |>
  pivot_wider(names_from = threshold, values_from = richness,
              names_prefix = "richness_bn_18h_") |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+"))

# Check
nrow(bn_18h_richness)
map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_18h_", thr)
  bn_18h_richness |>
    summarise(n_subplots  = n(),
              n_with_data = sum(.data[[col]] > 0),
              n_zero      = sum(.data[[col]] == 0)) |>
    mutate(threshold = thr)
}) |> data.frame()

write.csv(bn_18h_richness,
          "D:/QBIO7008/Bird_accoustic/Outputs/bn_18h_richness_all_subplots.csv",
          row.names = FALSE)

####Traditional survey richness (pooled M+E, all validated subplots)

trad_richness <- all_surveys_forest_birds |>
  filter(ID %in% validated_subplots$subplot_ID) |>
  group_by(ID) |>
  summarise(richness_trad = n_distinct(scientific_name, na.rm = TRUE),
            .groups = "drop") |>
  rename(subplot_ID = ID) |>
  right_join(validated_subplots |> select(subplot_ID), by = "subplot_ID") |>
  mutate(richness_trad = replace_na(richness_trad, 0),
         plot_ID = str_extract(subplot_ID, "^C\\d+"))

# Check
nrow(trad_richness)
cat("Subplots with data:", sum(trad_richness$richness_trad > 0), "\n")
cat("Subplots with zero:", sum(trad_richness$richness_trad == 0), "\n")

write.csv(trad_richness,
          "D:/QBIO7008/Bird_accoustic/Outputs/trad_richness_all_subplots.csv",
          row.names = FALSE)

####Analytical datasets 

# Like-for-like (BN 20min vs Trad 20min)
richness_lfl <- trad_richness |>
  left_join(bn_lfl_richness, by = c("subplot_ID", "plot_ID"))

nrow(richness_lfl)
write.csv(richness_lfl,
          "D:/QBIO7008/Bird_accoustic/Outputs/richness_lfl_dataset_all_subplots.csv",
          row.names = FALSE)

# 18h (BN 18h vs Trad 20min)
richness_18h <- trad_richness |>
  left_join(bn_18h_richness, by = c("subplot_ID", "plot_ID"))

nrow(richness_18h)
write.csv(richness_18h,
          "D:/QBIO7008/Bird_accoustic/Outputs/richness_18h_dataset_all_subplots.csv",
          row.names = FALSE)

###############################################################################
# OBJECTIVE 4 - STATISTICAL ANALYSIS
###############################################################################

# Load plot level shade cover data
plot_shade <- read.csv("D:/QBIO7008/Bird_accoustic/Data/plot_shade_cover.csv")

# Join shade cover
richness_lfl <- richness_lfl |>
  left_join(plot_shade, by = c("plot_ID" = "plot_id"))

richness_18h <- richness_18h |>
  left_join(plot_shade, by = c("plot_ID" = "plot_id"))

# Check for NAs
cat("NAs in lfl shade:", sum(is.na(richness_lfl$plot_shade)), "\n")
cat("NAs in 18h shade:", sum(is.na(richness_18h$plot_shade)), "\n")

#Distribution check

hist(richness_lfl$plot_shade,
     main = "Shade cover distribution", xlab = "Plot shade cover")


### SENSITIVITY ANALYSIS - slope concordance with traditional survey

#Reference model: traditional survey only 

model_trad_lfl <- glmmTMB(richness_trad ~ plot_shade + (1|plot_ID),
                      data   = richness_lfl,
                      family = poisson) #using plots as random effect - grouping - subplot not nested in it so to not introduce convergence issues

summary(model_trad_lfl)
deviance(model_trad_lfl) / df.residual(model_trad_lfl)

#check fit
sim_res_lfl <- simulateResiduals(model_trad_lfl)
plot(sim_res_lfl)



# slope and SE - glmmTMB extraction
trad_slope_lfl    <- unname(fixef(model_trad_lfl)$cond["plot_shade"])
trad_slope_lfl_se <- unname(sqrt(diag(vcov(model_trad_lfl)$cond))["plot_shade"])
trad_p_lfl        <- unname(summary(model_trad_lfl)$coefficients$cond["plot_shade", "Pr(>|z|)"])

cat("Traditional survey shade slope:", round(trad_slope_lfl, 4), "\n")
cat("Traditional survey slope SE:   ", round(trad_slope_lfl_se, 4), "\n")
cat("Traditional survey p-value:    ", round(trad_p_lfl, 3), "\n")

#LFL sensitivity: fit one model per threshold

lfl_sensitivity_models <- map(thresholds, function(thr) {
  col <- paste0("richness_bn_lfl_", thr)
  glmmTMB(richness_bn ~ plot_shade + (1|plot_ID) ,
      data   = richness_lfl |> rename(richness_bn = all_of(col)),
      family = poisson)
})
names(lfl_sensitivity_models) <- paste0("thr_", thresholds)

# DHARMa diagnostics

walk2(lfl_sensitivity_models, thresholds, function(m, thr) {
  sim <- simulateResiduals(m)
  plot(sim, main = paste("LFL BirdNET - threshold", thr))
}) #0.3 till 0.9 is ok

# Extract slopes and compare to traditional survey

lfl_sensitivity <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_lfl_", thr)
  m   <- lfl_sensitivity_models[[paste0("thr_", thr)]]
  
  disp        <- deviance(m) / df.residual(m)
  bn_slope    <- unname(fixef(m)$cond["plot_shade"])
  bn_slope_se <- unname(sqrt(diag(vcov(m)$cond))["plot_shade"])
  bn_p        <- unname(summary(m)$coefficients$cond["plot_shade", "Pr(>|z|)"])
  slope_diff  <- bn_slope - trad_slope_lfl
  pooled_se   <- sqrt(bn_slope_se^2 + trad_slope_lfl_se^2)
  z_stat      <- slope_diff / pooled_se
  p_diff      <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
  
  data.frame(
    threshold         = thr,
    n_with_detections = sum(richness_lfl[[col]] > 0),
    dispersion        = round(disp, 2),
    trad_slope        = round(trad_slope_lfl, 4),
    trad_p_value      = round(trad_p_lfl, 3),
    bn_slope          = round(bn_slope, 4),
    bn_p_value        = round(bn_p, 3),
    slope_diff        = round(slope_diff, 4),
    pooled_se         = round(pooled_se, 4),
    p_diff            = round(p_diff, 3)
  )
})

cat("=== LFL SENSITIVITY ANALYSIS ===\n")
print(lfl_sensitivity)

write.csv(lfl_sensitivity,
          "D:/QBIO7008/Bird_accoustic/Outputs/sensitivity_lfl.csv",
          row.names = FALSE)

# 18h reference model

model_trad_18h <- glmmTMB(richness_trad ~ plot_shade + (1|plot_ID),
                          data   = richness_18h |>
                            filter(!is.na(richness_bn_18h_0.5)),
                          family = poisson)

summary(model_trad_18h)
deviance(model_trad_18h) / df.residual(model_trad_18h)

#check
sim_res_trad_18h <- simulateResiduals(model_trad_18h)
plot(sim_res_trad_18h) # minor issue but not so relevant to fit

#slope and SE
trad_slope_18h    <- unname(fixef(model_trad_18h)$cond["plot_shade"])
trad_slope_18h_se <- unname(sqrt(diag(vcov(model_trad_18h)$cond))["plot_shade"])
trad_p_18h        <- unname(summary(model_trad_18h)$coefficients$cond["plot_shade", "Pr(>|z|)"])

cat("Traditional survey shade slope (18h):", round(trad_slope_18h, 4), "\n")
cat("Traditional survey slope SE (18h):   ", round(trad_slope_18h_se, 4), "\n")
cat("Traditional survey p-value (18h):    ", round(trad_p_18h, 3), "\n")

#18h sensitivity: fit one model per threshold

h18_sensitivity_models <- map(thresholds, function(thr) {
  col <- paste0("richness_bn_18h_", thr)
  df  <- richness_18h |>
    filter(!is.na(.data[[col]])) |>
    rename(richness_bn = all_of(col))
  glmmTMB(richness_bn ~ plot_shade + (1|plot_ID),
          data   = df,
          family = poisson)
})
names(h18_sensitivity_models) <- paste0("thr_", thresholds)

#DHARMa diagnostics 

walk2(h18_sensitivity_models, thresholds, function(m, thr) {
  sim <- simulateResiduals(m)
  plot(sim, main = paste("18h BirdNET - threshold", thr))
})

# Extract slopes and compare to traditional survey

h18_sensitivity <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_18h_", thr)
  m   <- h18_sensitivity_models[[paste0("thr_", thr)]]
  
  disp        <- deviance(m) / df.residual(m)
  bn_slope    <- unname(fixef(m)$cond["plot_shade"])
  bn_slope_se <- unname(sqrt(diag(vcov(m)$cond))["plot_shade"])
  bn_p        <- unname(summary(m)$coefficients$cond["plot_shade", "Pr(>|z|)"])
  slope_diff  <- bn_slope - trad_slope_18h
  pooled_se   <- sqrt(bn_slope_se^2 + trad_slope_18h_se^2)
  z_stat      <- slope_diff / pooled_se
  p_diff      <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
  
  data.frame(
    threshold         = thr,
    n_with_detections = sum(!is.na(richness_18h[[col]]) & richness_18h[[col]] > 0),
    dispersion        = round(disp, 2),
    trad_slope        = round(trad_slope_18h, 4),
    trad_p_value      = round(trad_p_18h, 3),
    bn_slope          = round(bn_slope, 4),
    bn_p_value        = round(bn_p, 3),
    slope_diff        = round(slope_diff, 4),
    pooled_se         = round(pooled_se, 4),
    p_diff            = round(p_diff, 3)
  )
})

cat("=== 18H SENSITIVITY ANALYSIS ===\n")
print(h18_sensitivity)

write.csv(h18_sensitivity,
          "D:/QBIO7008/Bird_accoustic/Outputs/sensitivity_18h.csv",
          row.names = FALSE)

#### building a table for the report

# LFL sensitivity table 

gt_sensitivity_lfl <- lfl_sensitivity |>
  gt() |>
  tab_header(
    title = "Appendix 1a. Sensitivity analysis — Like-for-like (20-minute window)"
  ) |>
  cols_label(
    threshold         = "Confidence threshold",
    n_with_detections = "Subplots with detections",
    dispersion        = "Dispersion",
    trad_slope        = "Traditional slope",
    trad_p_value      = "Traditional p",
    bn_slope          = "BirdNET slope",
    bn_p_value        = "BirdNET p",
    slope_diff        = "Slope difference",
    pooled_se         = "Pooled SE",
    p_diff            = "p (difference)"
  ) |>
  tab_footnote(
    footnote = "Slopes are on the log scale from Poisson GLMM. Traditional survey slope is the reference model fitted to traditional survey richness only. BirdNET slope is fitted separately per threshold. p (difference) is the two-sided z-test p-value for the difference between BirdNET and traditional survey slopes."
  ) |>
  tab_style(
    style = cell_fill(color = "grey90"),
    locations = cells_body(rows = threshold == 0.5)
  ) |>
  tab_options(
    table.font.size = 11,
    heading.align   = "left"
  )

gt_sensitivity_lfl

# 18h sensitivity table 

gt_sensitivity_18h <- h18_sensitivity |>
  gt() |>
  tab_header(
    title = "Appendix 1b. Sensitivity analysis — Extended window (18-hour recording)"
  ) |>
  cols_label(
    threshold         = "Confidence threshold",
    n_with_detections = "Subplots with detections",
    dispersion        = "Dispersion",
    trad_slope        = "Traditional slope",
    trad_p_value      = "Traditional p",
    bn_slope          = "BirdNET slope",
    bn_p_value        = "BirdNET p",
    slope_diff        = "Slope difference",
    pooled_se         = "Pooled SE",
    p_diff            = "p (difference)"
  ) |>
  tab_footnote(
    footnote = "Slopes are on the log scale from Poisson GLMM. Traditional survey slope is the reference model fitted to traditional survey richness only. BirdNET slope is fitted separately per threshold. p (difference) is the two-sided z-test p-value for the difference between BirdNET and traditional survey slopes."
  ) |>
  tab_style(
    style = cell_fill(color = "grey90"),
    locations = cells_body(rows = threshold == 0.5)
  ) |>
  tab_options(
    table.font.size = 11,
    heading.align   = "left"
  ) 

gt_sensitivity_18h

# Save both to Word 

gt_sensitivity_lfl |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/appendix1a_sensitivity_lfl.docx")

gt_sensitivity_18h |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/appendix1b_sensitivity_18h.docx")

###############################################################################
# MAIN MODELS
# Threshold selected after reviewing sensitivity analysis results above
# Update 0.5 below to whichever threshold minimises slope_diff
###############################################################################

#  Like-for-like main model 
# Threshold selected based on sensitivity analysis
# Plain Poisson GLMM 
# and no overdispersion found in reference model and sensitivity analysis

model_lfl <- glmmTMB(
  richness ~ plot_shade * method + (1|plot_ID),
  data = richness_lfl |>
    select(subplot_ID, plot_ID, plot_shade,
           richness_trad,
           richness_bn = richness_bn_lfl_0.5) |>
    pivot_longer(cols      = c(richness_trad, richness_bn),
                 names_to  = "method",
                 values_to = "richness") |>
    mutate(method = factor(method,
                           levels = c("richness_trad", "richness_bn"),
                           labels = c("Traditional", "BirdNET"))),
  family = poisson)

summary(model_lfl)
deviance(model_lfl) / df.residual(model_lfl)

sim_res_lfl <- simulateResiduals(model_lfl)
plot(sim_res_lfl)

#18h main model


model_18h <- glmmTMB(
  richness ~ plot_shade * method + (1|plot_ID),
  data = richness_18h |>
    filter(!is.na(richness_bn_18h_0.5)) |>
    select(subplot_ID, plot_ID, plot_shade,
           richness_trad,
           richness_bn = richness_bn_18h_0.5) |>
    pivot_longer(cols      = c(richness_trad, richness_bn),
                 names_to  = "method",
                 values_to = "richness") |>
    mutate(method = factor(method,
                           levels = c("richness_trad", "richness_bn"),
                           labels = c("Traditional", "BirdNET"))),
  family = poisson)

summary(model_18h)
deviance(model_18h) / df.residual(model_18h)

sim_res_18h <- simulateResiduals(model_18h)
plot(sim_res_18h)


################################################################################
#### OBJECTIVE 5 - FIGURES FOR RESULTS SECTION##################################
################################################################################


# shade range for prediction
shade_seq <- seq(min(richness_lfl$plot_shade, na.rm = TRUE),
                 max(richness_lfl$plot_shade, na.rm = TRUE),
                 length.out = 100)

####Like-for-like model 

#raw data 

lfl_raw <- richness_lfl |>
  select(subplot_ID, plot_ID, plot_shade, richness_trad,
         richness_bn = richness_bn_lfl_0.5) |>
  pivot_longer(cols = c(richness_trad, richness_bn),
               names_to = "method", values_to = "richness") |>
  mutate(method = factor(method,
                         levels = c("richness_trad", "richness_bn"),
                         labels = c("Traditional survey", "BirdNET (0.5 confidence level)")))

# Like-for-like: predicted values with 95% CI
lfl_pred <- expand.grid(
  plot_shade = shade_seq,
  method     = factor(c("Traditional survey", "BirdNET (0.5 confidence level)"))
) |>
  mutate(method_internal = ifelse(method == "Traditional survey", "Traditional", "BirdNET"))

# Predictions on the link (log) scale with SE
# re.form = NA gives population-level predictions excluding random effects
lfl_fit <- predict(model_lfl,
                   newdata = data.frame(
                     plot_shade = lfl_pred$plot_shade,
                     method     = factor(lfl_pred$method_internal,
                                         levels = c("Traditional", "BirdNET"))
                   ),
                   type    = "link",
                   se.fit  = TRUE,
                   re.form = NA)

# Back-transform to response scale (species counts) using exp()
# CI computed on log scale first, then back-transformed to keep intervals
# asymmetric and bounded above zero - this is correct for Poisson GLM
lfl_pred <- lfl_pred |>
  mutate(
    fit    = exp(lfl_fit$fit),                            # predicted richness
    ci_low = exp(lfl_fit$fit - 1.96 * lfl_fit$se.fit),   # lower 95% CI
    ci_up  = exp(lfl_fit$fit + 1.96 * lfl_fit$se.fit)    # upper 95% CI
  )

# plot
# All values plotted on response scale (species richness counts)
# Raw data points are original observed counts - no transformation applied

fig_lfl <- ggplot() +
  geom_ribbon(data = lfl_pred,
              aes(x = plot_shade, ymin = ci_low, ymax = ci_up, group = method),
              fill = "grey80", alpha = 0.5, show.legend = FALSE) +
  geom_point(data = lfl_raw,
             aes(x = plot_shade, y = richness, colour = method),
             alpha = 0.5, size = 2) +
  geom_line(data = lfl_pred,
            aes(x = plot_shade, y = fit, colour = method),
            linewidth = 1) +
  scale_colour_brewer(palette = "Dark2") +
  guides(colour = guide_legend(title = "Method")) +
  labs(
    x      = "Plot shade cover",
    y      = "Forest bird species richness",
    title  = "Like-for-like sampling (20-minute window)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

fig_lfl

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_richness_lfl.png",
       fig_lfl, width = 6, height = 5, dpi = 300)

####18h: raw data

h18_raw <- richness_18h |>
  filter(!is.na(richness_bn_18h_0.5)) |>
  select(subplot_ID, plot_ID, plot_shade, richness_trad,
         richness_bn = richness_bn_18h_0.5) |>
  pivot_longer(cols = c(richness_trad, richness_bn),
               names_to = "method", values_to = "richness") |>
  mutate(method = factor(method,
                         levels = c("richness_trad", "richness_bn"),
                         labels = c("Traditional survey", "BirdNET (0.5 confidence level)")))

#predicted values with 95% CI 

shade_seq_18h <- seq(min(richness_18h$plot_shade, na.rm = TRUE),
                     max(richness_18h$plot_shade, na.rm = TRUE),
                     length.out = 100)

h18_pred <- expand.grid(
  plot_shade = shade_seq_18h,
  method     = factor(c("Traditional survey", "BirdNET (0.5 confidence level)"))
) |>
  mutate(method_internal = ifelse(method == "Traditional survey", "Traditional", "BirdNET"))

# Predictions on the link (log) scale with SE
# re.form = NA gives population-level predictions excluding random effects
h18_fit <- predict(model_18h,
                   newdata = data.frame(
                     plot_shade = h18_pred$plot_shade,
                     method     = factor(h18_pred$method_internal,
                                         levels = c("Traditional", "BirdNET"))
                   ),
                   type    = "link",
                   se.fit  = TRUE,
                   re.form = NA)

# Back-transform to response scale (species counts) using exp()
# CI computed on log scale first, then back-transformed to keep intervals
# asymmetric and bounded above zero - this is correct for Poisson GLMM
h18_pred <- h18_pred |>
  mutate(
    fit    = exp(h18_fit$fit),                            # predicted richness
    ci_low = exp(h18_fit$fit - 1.96 * h18_fit$se.fit),   # lower 95% CI
    ci_up  = exp(h18_fit$fit + 1.96 * h18_fit$se.fit)    # upper 95% CI
  )


#plot


fig_18h <- ggplot() +
  geom_ribbon(data = h18_pred,
              aes(x = plot_shade, ymin = ci_low, ymax = ci_up, group = method),
              fill = "grey80", alpha = 0.5, show.legend = FALSE) +
  geom_point(data = h18_raw,
             aes(x = plot_shade, y = richness, colour = method),
             alpha = 0.5, size = 2) +
  geom_line(data = h18_pred,
            aes(x = plot_shade, y = fit, colour = method),
            linewidth = 1) +
  scale_colour_brewer(palette = "Dark2") +
  guides(colour = guide_legend(title = "Method")) +
  labs(
    x      = "Plot shade cover",
    y      = "Forest bird species richness",
    title  = "Extended window (18-hour recording)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

fig_18h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_richness_18h.png",
       fig_18h, width = 6, height = 5, dpi = 300)



###############################################################################
#### OBJECTIVE 6 - TABLE OF COEFICIENTS MAIN MODELS - FOR REPORT###############
###############################################################################


# Extract coefficients from both models 

# LFL model (glmmTMB)
lfl_coefs <- summary(model_lfl)$coefficients$cond |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  rename(estimate_lfl = Estimate,
         se_lfl       = `Std. Error`,
         z_lfl        = `z value`,
         p_lfl        = `Pr(>|z|)`)

# 18h model (glmmTMB)
h18_coefs <- summary(model_18h)$coefficients$cond |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  rename(estimate_18h = Estimate,
         se_18h       = `Std. Error`,
         z_18h        = `z value`,
         p_18h        = `Pr(>|z|)`)


# Join and relabel
model_table <- lfl_coefs |>
  left_join(h18_coefs, by = "term") |>
  mutate(term = recode(term,
                       "(Intercept)"              = "Intercept (Traditional survey, shade = 0)",
                       "plot_shade"               = "Shade tree cover (plot level)",
                       "methodBirdNET"            = "Difference in intercept (BirdNET vs Traditional survey)",
                       "plot_shade:methodBirdNET" = "Difference in shade slope (BirdNET vs Traditional survey)"
  )) |>
  mutate(across(c(estimate_lfl, se_lfl, z_lfl,
                  estimate_18h, se_18h, z_18h), ~ round(.x, 3)),
         p_lfl = round(p_lfl, 4),
         p_18h = round(p_18h, 4))


# Build gt table
gt_table <- model_table |>
  gt() |>
  tab_header(
    title = "Table X. Poisson GLMM results for forest bird species richness"
  ) |>
  tab_spanner(
    label   = "Like-for-like (20-minute window)",
    columns = c(estimate_lfl, se_lfl, z_lfl, p_lfl)
  ) |>
  tab_spanner(
    label   = "Extended window (18-hour recording)",
    columns = c(estimate_18h, se_18h, z_18h, p_18h)
  ) |>
  cols_label(
    term         = "Term",
    estimate_lfl = "Estimate",
    se_lfl       = "SE",
    z_lfl        = "z",
    p_lfl        = "p",
    estimate_18h = "Estimate",
    se_18h       = "SE",
    z_18h        = "z",
    p_18h        = "p"
  ) |>
  tab_footnote(
    footnote = "Reference level is traditional point count survey. BirdNET detections filtered at 0.5 confidence threshold. Estimates are on the log scale."
  ) |>
  tab_options(
    table.font.size  = 12,
    heading.align    = "left"
  )

gt_table

# Export to Word
gt_table |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/richness_model_table.docx")

###############################################################################
#### OBJECTIVE 7 - KEY DATA FOR THE REPORT ####################################
###############################################################################

# number of plots included like for like
unique(richness_lfl$plot_ID) #21

# number of subplots included with M and E surveys pooled 
unique(richness_lfl$subplot_ID) #41

# min max and average richness trad surveys
max(richness_lfl$richness_trad)
min(richness_lfl$richness_trad)
mean(richness_lfl$richness_trad)
sd(richness_lfl$richness_trad)

# Traditional survey detections (filtered to subplots in richness_lfl)
all_surveys_forest_birds |>
  filter(ID %in% richness_lfl$subplot_ID) |>
  nrow()

# LFL BirdNET detections at 0.5 (filtered to subplots in richness_lfl)
bn_like_for_like_forest |>
  filter(subplot_ID %in% richness_lfl$subplot_ID,
         confidence >= 0.5) |>
  nrow()

# min max and average richness BirdNET
max(richness_lfl$richness_bn_lfl_0.5)
min(richness_lfl$richness_bn_lfl_0.5)
mean(richness_lfl$richness_bn_lfl_0.5)
sd(richness_lfl$richness_bn_lfl_0.5)

#shade range and mean
max(richness_lfl$plot_shade)
min(richness_lfl$plot_shade)
mean(richness_lfl$plot_shade)
sd(richness_lfl$plot_shade)

# number of plots included like for like
unique(richness_18h$plot_ID) #21

# number of subplots included with M and E surveys pooled 
unique(richness_18h$subplot_ID) #41


# 18h BirdNET detections at 0.5 (filtered to subplots in richness_18h with data)
bn_18h_forest |>
  filter(subplot_ID %in% richness_18h$subplot_ID,
         confidence >= 0.5) |>
  nrow()

#18h BirdNET range and mean - this dataset includes 30_O that is NA for the 18H
max(richness_18h$richness_bn_18h_0.5, na.rm = TRUE)
min(richness_18h$richness_bn_18h_0.5, na.rm = TRUE)
mean(richness_18h$richness_bn_18h_0.5, na.rm = TRUE)
sd(richness_18h$richness_bn_18h_0.5, na.rm = TRUE)

#shade range and mean
max(richness_18h$plot_shade)
min(richness_18h$plot_shade)
mean(richness_18h$plot_shade)
sd(richness_18h$plot_shade)

# min max and average richness trad surveys
max(richness_18h$richness_trad)
min(richness_18h$richness_trad)
mean(richness_18h$richness_trad)
sd(richness_18h$richness_trad)

# Extract coefficients from LFL model (glmmTMB)
lfl_coefs <- summary(model_lfl)$coefficients$cond

# Log scale values
cat("=== LOG SCALE ===\n")
print(round(lfl_coefs, 4))

# Back-transformed (exponentiated)
cat("\n=== BACK-TRANSFORMED (exp) ===\n")
cat("Intercept (baseline richness trad, shade=0):", round(exp(lfl_coefs["(Intercept)", "Estimate"]), 2), "\n")
cat("Shade effect (trad survey, rate ratio):", round(exp(lfl_coefs["plot_shade", "Estimate"]), 2), "\n")
cat("BirdNET intercept difference (rate ratio):", round(exp(lfl_coefs["methodBirdNET", "Estimate"]), 2), "\n")
cat("BirdNET slope difference (rate ratio):", round(exp(lfl_coefs["plot_shade:methodBirdNET", "Estimate"]), 2), "\n")

# BirdNET actual slope (trad slope + interaction)
bn_slope_lfl <- lfl_coefs["plot_shade", "Estimate"] + lfl_coefs["plot_shade:methodBirdNET", "Estimate"]
cat("\nBirdNET actual slope (log scale):", round(bn_slope_lfl, 4), "\n")
cat("BirdNET actual slope (back-transformed):", round(exp(bn_slope_lfl), 2), "\n")

# Shade effect across observed range - pulled dynamically from model
shade_slope_lfl <- lfl_coefs["plot_shade", "Estimate"]
shade_range     <- diff(range(richness_lfl$plot_shade, na.rm = TRUE))

cat("Per 10 percentage points:", round(exp(shade_slope_lfl * 0.1), 2), "\n")
cat("Across full range:", round(exp(shade_slope_lfl * shade_range), 2), "\n")
range(richness_lfl$plot_shade, na.rm = TRUE)

#### other plots I want ti include in the supplementary info
#Confidence levels histogram
# Like-for-like 

fig_hist_lfl <- bn_like_for_like_forest |>
  filter(subplot_ID %in% richness_lfl$subplot_ID) |>
  ggplot(aes(x = confidence)) +
  geom_histogram(binwidth = 0.05, fill = "grey60", colour = "white") +
  labs(
    x     = "BirdNET confidence level",
    y     = "Number of detections",
    title = "Like-for-like window (20-minute | Forest species)"
  ) +
  theme_classic()

fig_hist_lfl

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_hist_lfl.png",
       fig_hist_lfl, width = 6, height = 4, dpi = 300)

# 18h 

fig_hist_18h <- bn_18h_forest |>
  filter(subplot_ID %in% richness_18h$subplot_ID) |>
  ggplot(aes(x = confidence)) +
  geom_histogram(binwidth = 0.05, fill = "grey60", colour = "white") +
  labs(
    x     = "BirdNET confidence level",
    y     = "Number of detections",
    title = "Extended window (18-hour recording | Forest species)"
  ) +
  theme_classic()

fig_hist_18h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_hist_18h.png",
       fig_hist_18h, width = 6, height = 4, dpi = 300)

#### raw data panels
# Build long format with all thresholds plus traditional survey
lfl_scatter <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_lfl_", thr)
  richness_lfl |>
    select(subplot_ID, plot_ID, plot_shade, richness = all_of(col)) |>
    mutate(panel = paste0("BirdNET ", thr))
}) |>
  bind_rows(
    richness_lfl |>
      select(subplot_ID, plot_ID, plot_shade, richness = richness_trad) |>
      mutate(panel = "Traditional survey")
  ) |>
  mutate(panel = factor(panel,
                        levels = c("Traditional survey",
                                   paste0("BirdNET ", thresholds))))

# Fixed axis limits
lfl_scatter_max <- max(lfl_scatter$richness, na.rm = TRUE)

# Colour palette - 10 panels (1 traditional + 9 thresholds)
panel_colours <- setNames(
  c("darkorange", viridis::viridis(9)),
  c("Traditional survey", paste0("BirdNET ", thresholds))
)

fig_scatter_lfl <- ggplot(lfl_scatter,
                          aes(x = plot_shade, y = richness, colour = panel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~ panel, ncol = 5) +
  scale_colour_manual(values = panel_colours) +
  scale_y_continuous(limits = c(0, lfl_scatter_max)) +
  labs(
    x     = "Plot shade cover",
    y     = "Forest bird species richness",
    title = "Like-for-like window (20-minute | Forest species)"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 9),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95"),
    legend.position  = "none"
  )

fig_scatter_lfl

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_scatter_lfl.png",
       fig_scatter_lfl, width = 12, height = 6, dpi = 300)

# Scatterplot richness vs shade — 18h 

h18_scatter <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_18h_", thr)
  richness_18h |>
    filter(!is.na(.data[[col]])) |>
    select(subplot_ID, plot_ID, plot_shade, richness = all_of(col)) |>
    mutate(panel = paste0("BirdNET ", thr))
}) |>
  bind_rows(
    richness_18h |>
      filter(!is.na(richness_bn_18h_0.5)) |>
      select(subplot_ID, plot_ID, plot_shade, richness = richness_trad) |>
      mutate(panel = "Traditional survey")
  ) |>
  mutate(panel = factor(panel,
                        levels = c("Traditional survey",
                                   paste0("BirdNET ", thresholds))))

# Fixed axis limits
h18_scatter_max <- max(h18_scatter$richness, na.rm = TRUE)

# Colour palette - 10 panels (1 traditional + 9 thresholds)
panel_colours <- setNames(
  c("darkorange", viridis::viridis(9)),
  c("Traditional survey", paste0("BirdNET ", thresholds))
)

fig_scatter_18h <- ggplot(h18_scatter,
                          aes(x = plot_shade, y = richness, colour = panel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~ panel, ncol = 5) +
  scale_colour_manual(values = panel_colours) +
  scale_y_continuous(limits = c(0, h18_scatter_max)) +
  labs(
    x     = "Plot shade cover",
    y     = "Forest bird species richness",
    title = "Extended window (18-hour recording | Forest species)"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 9),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95"),
    legend.position  = "none"
  )

fig_scatter_18h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_scatter_18h.png",
       fig_scatter_18h, width = 12, height = 6, dpi = 300)


#### plot showing the recording hours

# Recording span Gantt chart 

# Build recording span from bn_w filtered to validated subplots (LFL analysis)
recording_span <- bn_w[subplot_ID %in% validated_subplots$subplot_ID] |>
  group_by(subplot_ID) |>
  summarise(
    first_detection = min(detection_datetime),
    last_detection  = max(detection_datetime),
    .groups = "drop"
  ) |>
  mutate(
    included   = ifelse(subplot_ID %in% subplots_18h_ok, "Included", "Excluded"),
    subplot_ID = factor(subplot_ID, levels = sort(unique(subplot_ID))),
    # Extract time of day in decimal hours
    first_hour = hour(first_detection) + minute(first_detection) / 60,
    last_hour  = hour(last_detection)  + minute(last_detection)  / 60,
    # Add 24 only if recording crosses into a different date
    crosses_midnight = as.Date(last_detection) > as.Date(first_detection),
    last_hour  = ifelse(crosses_midnight, last_hour + 24, last_hour)
  )

# Split bars at midnight for subplots crossing midnight

recording_span_split <- recording_span |>
  mutate(crosses_midnight = last_hour > 24) |>
  rowwise() |>
  reframe(
    subplot_ID = subplot_ID,
    included   = included,
    first_hour = if (crosses_midnight) c(first_hour, 24) else first_hour,
    last_hour  = if (crosses_midnight) c(24, last_hour)  else last_hour
  )

# Y axis breaks and labels 

hour_breaks <- seq(0, 42, by = 2)
hour_labels <- c(
  "00:00", "02:00", "04:00", "06:00", "08:00", "10:00",
  "12:00", "14:00", "16:00", "18:00", "20:00", "22:00",
  "00:00", "02:00", "04:00", "06:00", "08:00", "10:00",
  "12:00", "14:00", "16:00", "18:00"
)

# Plot 

fig_duration <- ggplot(recording_span_split,
                       aes(x = subplot_ID, fill = included)) +
  geom_rect(aes(xmin = as.numeric(subplot_ID) - 0.4,
                xmax = as.numeric(subplot_ID) + 0.4,
                ymin = first_hour,
                ymax = last_hour)) +
  # Reference lines for 18h window: 13:00 and 07:00 next day (= 31)
  geom_hline(yintercept = 13, colour = "red",   linetype = "dashed", linewidth = 0.7) +
  geom_hline(yintercept = 31, colour = "red",   linetype = "dashed", linewidth = 0.7) +
  # Midnight reference line
  geom_hline(yintercept = 24, colour = "black", linetype = "dotted", linewidth = 0.5) +
  scale_fill_manual(values = c("Included" = "#1B9E77",
                               "Excluded" = "grey70")) +
  scale_y_reverse(breaks = hour_breaks,
                  labels = hour_labels,
                  limits = c(42, 0)) +
  annotate("text", x = 0.5, y = 13, label = "13:00",
           hjust = 0, vjust = -0.5, size = 3, colour = "red") +
  annotate("text", x = 0.5, y = 31, label = "07:00 (+1)",
           hjust = 0, vjust = -0.5, size = 3, colour = "red") +
  annotate("text", x = 0.5, y = 24, label = "Midnight",
           hjust = 0, vjust = -0.5, size = 3, colour = "black") +
  labs(
    x     = "Subplot",
    y     = "Time of day",
    fill  = NULL,
    title = "Recording span per subplot"
  ) +
  theme_classic() +
  theme(
    axis.text.x        = element_text(angle = 90, hjust = 1,
                                      vjust = 0.5, size = 7),
    axis.text.y        = element_text(size = 8),
    legend.position    = "bottom",
    panel.grid.major.y = element_line(colour = "grey90")
  )

fig_duration

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_recording_duration.png",
       fig_duration, width = 14, height = 6, dpi = 300)


