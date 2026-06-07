######DATA EXPLORING - BIRDNET OUTPUTS
# BARBARA OLIVEIRA DE LORETO 04/04/2026 - WITH CLAUSE SUPPORT

#############################################################
######## Objective###########################################
##########1. create a dataset with BirdNet detections associated
########## with a time stamp and a plot/subplot for block 1, 2 and 3 analysis##




#############################################################
###### Look into BirdNET output#############################

# ── 0. Install/load packages ──────────────────────────────────────────────────
library(data.table)
library(lubridate)
library(stringr)
library(knitr)
library(kableExtra)
library(gt)
library(tidyr)
library(dplyr)

# Load the files 
# fread because fies are too large
bn_block_1 <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/birdnet_ALL_results_merged_block_1.csv")
bn_block_3 <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/birdnet_ALL_results_merged_block_3.csv")
bn <- rbind(bn_block_1, bn_block_3)



# check
dim(bn)          # how many rows and columns?
names(bn)        # column names
str(bn)          # data types of each column
head(bn, 10)     # first 10 rows

# check
summary(bn)

# Check confidence score distribution 
# BirdNET outputs a confidence value —  understand its range
range(bn$confidence, na.rm = TRUE)
hist(bn$confidence, breaks = 50, main = "BirdNET confidence scores", xlab = "Confidence")

png("D:/QBIO7008/Bird_accoustic/Plots/birdnet_confidence_histogram.png", 
    width = 800, height = 600, res = 120)

hist(bn$confidence, breaks = seq(0.05, 1.0, by = 0.05),
     main = "BirdNET confidence scores", xlab = "Confidence",
     xlim = c(0, 1), xaxt = "n")
axis(1, at = seq(0, 1, by = 0.1))

dev.off()
###############################################################################
###### Link date, time variables###########################

# Parse detection datetime directly from BirdNET filename (recorders were set up for Ghana time but there are tro weird files)
bn[, recording_start := ymd_hms(
  str_extract(filename, "\\d{8}T\\d{6}"),
  tz = "Africa/Accra"
)]

bn[, detection_datetime := recording_start + seconds(start_time)]

# Check it looks right
bn[, .(filename, start_time, recording_start, detection_datetime)] |> head(5)

###########################################################################
###### removing entried with suspocious timezones (+1000) time stamp overlaps with +0000 files in the same plots
# Remove detections from +1000 timezone files 
bn <- bn[!grepl("\\+1000", filename)]

# Confirm they are gone
bn[, .N]
bn |>
  mutate(tz_offset = str_extract(filename, "[+-]\\d{4}")) |>
  distinct(tz_offset)

# fix some naming issues
# fix deployment names 
bn[, deployment := fcase(
  # C26 lowercase
  deployment == "C26\\c26_A",             "C26\\C26_A",
  deployment == "C26\\c26_B",             "C26\\C26_B",
  deployment == "C26\\c26_C",             "C26\\C26_C",
  deployment == "C26\\c26_D",             "C26\\C26_D",
  deployment == "C26\\c26_O",             "C26\\C26_O",
  # C26 and C30 extra plots
  deployment == "C26\\C26 EXTRA PLOT",    "C26\\C26_extra",
  deployment == "C30\\Extral Plot",       "C30\\C30_extra",
  # C30 PLOT naming
  deployment == "C30\\PLOT A",            "C30\\C30_A",
  deployment == "C30\\PLOT B",            "C30\\C30_B",
  deployment == "C30\\PLOT C",            "C30\\C30_C",
  deployment == "C30\\PLOT D",            "C30\\C30_D",
  deployment == "C30\\PLOT O",            "C30\\C30_O",
  # C52 space
  deployment == "C52\\C 52_A",            "C52\\C52_A",
  deployment == "C52\\C 52_B",            "C52\\C52_B",
  deployment == "C52\\C 52_C",            "C52\\C52_C",
  deployment == "C52\\C 52_O",            "C52\\C52_O",
  # F03 typo
  deployment == "F03\\FO3_C",             "F03\\F03_C",
  deployment == "F03\\FO3_D",             "F03\\F03_D",
  deployment == "F03\\FO3_O",             "F03\\F03_O",
  # C114 → C116
  deployment == "C114\\C114_A",           "C116\\C116_A",
  deployment == "C114\\C114_B",           "C116\\C116_B",
  deployment == "C114\\C114_C",           "C116\\C116_C",
  deployment == "C114\\C114_D",           "C116\\C116_D",
  deployment == "C114\\C114_O",           "C116\\C116_O",
  # C118 Point naming
  deployment == "C118\\Point A",          "C118\\C118_A",
  deployment == "C118\\Point B",          "C118\\C118_B",
  deployment == "C118\\Point C",          "C118\\C118_C",
  deployment == "C118\\Point D",          "C118\\C118_D",
  deployment == "C118\\Point O",          "C118\\C118_O",
  # C131 space
  deployment == "C131\\C131 A",           "C131\\C131_A",
  deployment == "C131\\C131 B",           "C131\\C131_B",
  deployment == "C131\\C131 C",           "C131\\C131_C",
  deployment == "C131\\C131 D",           "C131\\C131_D",
  deployment == "C131\\C131 O",           "C131\\C131_O",
  # C39 extra subfolder info
  deployment == "C39\\C39_B\\SD A20",     "C39\\C39_B",
  deployment == "C39\\C39_O\\SD A12",     "C39\\C39_O",
  rep(TRUE, nrow(bn)),                     deployment
)]

# check
bn[, unique(deployment)] |> sort()

#issues with C19
bn[deployment == "C19", .(deployment, source_folder, filename)] |> head(20)
# Fix C19 deployment using source_folder 
bn[deployment == "C19", deployment := fcase(
  str_starts(source_folder, "C19_A"), "C19\\C19_A",
  str_starts(source_folder, "C19_B"), "C19\\C19_B",
  str_starts(source_folder, "C19_C"), "C19\\C19_C",
  str_starts(source_folder, "C19_D"), "C19\\C19_D",
  str_starts(source_folder, "C19_O"), "C19\\C19_O",
  rep(TRUE, .N),                      "C19\\C19_root"
)]

# Check
bn[startsWith(deployment, "C19"), unique(deployment)]

#separate deployment colum into plot and subplot
bn <- bn |>
  mutate(
    subplot_ID = sub(".*\\\\", "", deployment),
    plot_ID    = sub("\\\\.*", "", deployment)
  )

#check
unique(bn$plot_ID)
unique(bn$subplot_ID)

##############################################################################
###### create a deployment summary to better understand the data#############

# Summary table: each deployment with its recording dates and period
deployment_summary <- bn[, .(
  first_recording  = min(recording_start),
  last_recording   = max(recording_start),
  recording_spam  = as.numeric(difftime(max(recording_start), min(recording_start), units = "hours")),
  n_files          = uniqueN(filename)
), by = deployment][order(deployment)]

deployment_summary #has recording spam but not recording hours (actual exact sum of all the hours recorded in multiple files for any subplot)

# create a recording duration variable
# First calculate max end_time per file
file_durations <- bn[, .(file_duration_s = max(end_time)), by = .(deployment, filename)]

# Then sum per deployment
duration_per_deployment <- file_durations[, .(recording_duration_hours = sum(file_duration_s) / 3600), 
                                          by = deployment]

# Join to deployment summary
deployment_summary <- deployment_summary[duration_per_deployment, on = "deployment"]

# Add a column indicating if deployment starts with C
deployment_summary[, cocoa_plot := startsWith(deployment, "C")]

# Count deployments starting with C
cat("cocoa_plot:", sum(deployment_summary$cocoa_plot), "\n")

fwrite(deployment_summary, "D:/QBIO7008/Bird_accoustic/Outputs/Acoustic_deployment_summary.csv")

#create a pdf file

deployment_summary |>
  gt() |>
  tab_header(title = "BirdNET deployment summary") |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/deployment_summary.pdf")

#######################################################
# fix the taxonomic missmatch identified for some species # discussion with experts plus crosswalk analysis

# Check by scientific name
check_sci <- c("Aerospiza tachiro", "Chloropicus pyrrhogaster", 
               "Platysteira blissetti", "Phyllastrephus scandens")

check_sci %in% bn$scientific_name

# Check by common name
check_com <- c("African Goshawk", "Fire-bellied Woodpecker", 
               "Red-cheeked Wattle-eye", "Leaf-love")

check_com %in% bn$common_name

# only Platysteira blissetti (Red-cheeked Wattle-eye) will need changes
bn <- bn %>%
  mutate(scientific_name = case_when(
    scientific_name == "Platysteira blissetti" ~ "Dyaphorophyia blissetti",
    TRUE ~ scientific_name
  ))

#check
# Should return FALSE - old name gone
"Platysteira blissetti" %in% bn$scientific_name

# Should return TRUE - new name present

"Dyaphorophyia blissetti" %in% bn$scientific_name

bn <- bn %>%
  mutate(scientific_name = case_when(
    scientific_name == "Psittacula krameri"    ~ "Alexandrinus krameri",
    TRUE ~ scientific_name
  ))

#check
# Check if Psittacula krameri is detected in bn 
"Psittacula krameri" %in% bn$scientific_name

############################################################################
###### create an output file###############################################

# Save the wrangled BirdNET data to your Outputs folder
fwrite(bn, "D:/QBIO7008/Bird_accoustic/Outputs/birdnet_wrangled.csv")

