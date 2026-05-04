######Composition of species
# BARBARA OLIVEIRA DE LORETO 23/04/2026 - with Claude support

################################################################################
######## Objective#############################################################
########1. explore data in terms of species composition
###########a. most frequent species across methods
###########b. species in one that don't appear in others
###########c. most common species at highest shade sites 
###########  
#######  
################################################################################

#load packages
library(readxl)
library(dplyr)
library(openxlsx) #having an issue with readxl
library(ggplot2)
library(tidyr)
library(gt)

# load data
# species in traditional survey
all_surveys_checked <- fread("D:/QBIO7008/Bird_accoustic/Outputs/all_traditional_surveys.xlsx")
str(all_surveys_checked)

#load all BN data - filteres by the survey windows - all birds not just forest

bn_like_for_like <- fread("D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like.csv")
str(bn_like_for_like)

# load shade tree cover dataset
shade <- read.csv("D:/QBIO7008/Bird_accoustic/Data/subplot_shade_cover.csv", sep = ";")
str(shade)

# ── Load BirdLife Ghana species list ──────────────────────────────────────────
birdlife <- fread("D:/QBIO7008/Bird_accoustic/Data/GHA-Species_BirdlifeInternational.csv")
str(birdlife)

# BirdNET frequency - arranged by bn_detections
bn_species_freq <- bn_like_for_like |>
  group_by(scientific_name) |>
  summarise(bn_detections = n_distinct(paste(subplot_ID, trad_date, survey_M_E)), .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName), 
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(bn_detections))

# Traditional survey frequency - arranged by trad_detections
trad_species_freq <- all_surveys_checked |>
  group_by(scientific_name) |>
  summarise(trad_detections = n_distinct(paste(ID, date_DD_MM_YY, survey_M_E)), .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName), 
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(trad_detections))

# Combined - arranged by bn_detections
species_freq_combined <- full_join(trad_species_freq, bn_species_freq, 
                                   by = c("scientific_name", "common_name_birdlife")) |>
  select(common_name_birdlife, scientific_name, trad_detections, bn_detections) |>
  arrange(desc(bn_detections))

#visually for top 15

# Top 15 species by BirdNET detections
top15 <- species_freq_combined |>
  slice(1:15) |>
  mutate(common_name_birdlife = ifelse(is.na(common_name_birdlife), scientific_name, common_name_birdlife),
         trad_detections = ifelse(is.na(trad_detections), 0, trad_detections),
         bn_detections = ifelse(is.na(bn_detections), 0, bn_detections))

# --- DIVERGING BAR CHART ---
diverging_plot <- top15 |>
  mutate(common_name_birdlife = reorder(common_name_birdlife, bn_detections),
         trad_detections = -trad_detections) |>  # flip traditional to left
  pivot_longer(cols = c(trad_detections, bn_detections),
               names_to = "method", values_to = "detections") |>
  mutate(method = ifelse(method == "trad_detections", "Traditional survey", "BirdNET")) |>
  ggplot(aes(x = detections, y = common_name_birdlife, fill = method)) +
  geom_col() +
  scale_x_continuous(labels = abs) +  # show positive numbers on both sides
  scale_fill_manual(values = c("Traditional survey" = "steelblue", "BirdNET" = "darkorange")) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.5) +
  labs(title = "Top 15 BirdNET species by detection frequency",
       x = "Number of survey occasions detected",
       y = NULL,
       fill = "Method") +
  theme_bw()


diverging_plot

# now for top 15 traditional surveys

top15 <- species_freq_combined |>
  arrange(desc(trad_detections)) |>
  slice(1:15) |>
  mutate(common_name_birdlife = ifelse(is.na(common_name_birdlife), scientific_name, common_name_birdlife),
         trad_detections = ifelse(is.na(trad_detections), 0, trad_detections),
         bn_detections = ifelse(is.na(bn_detections), 0, bn_detections))

# --- DIVERGING BAR CHART ---
diverging_plot2 <- top15 |>
  mutate(common_name_birdlife = reorder(common_name_birdlife, bn_detections),
         trad_detections = -trad_detections) |>  # flip traditional to left
  pivot_longer(cols = c(trad_detections, bn_detections),
               names_to = "method", values_to = "detections") |>
  mutate(method = ifelse(method == "trad_detections", "Traditional survey", "BirdNET")) |>
  ggplot(aes(x = detections, y = common_name_birdlife, fill = method)) +
  geom_col() +
  scale_x_continuous(labels = abs) +  # show positive numbers on both sides
  scale_fill_manual(values = c("Traditional survey" = "steelblue", "BirdNET" = "darkorange")) +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.5) +
  labs(title = "Top 15 Traditional Survey species by detection frequency",
       x = "Number of survey occasions detected",
       y = NULL,
       fill = "Method") +
  theme_bw()


diverging_plot2

# save the data with combined frequencies
write.csv(species_freq_combined,
          "D:/QBIO7008/Bird_accoustic/Outputs/forest_species_freq_combined.csv",
          row.names = FALSE)

################################################################################
# Birdnet only species

bn_only_species <- bn_species_freq |>
  filter(!scientific_name %in% all_surveys_checked$scientific_name) |>
  arrange(desc(bn_detections))

head(bn_only_species, 20)

#create table
bn_only_species |>
  select(common_name_birdlife, scientific_name, bn_detections) |>
  gt() |>
  cols_label(
    common_name_birdlife = "Common name",
    scientific_name = "Scientific name",
    bn_detections = "BirdNET detections"
  ) |>
  tab_header(
    title = "Species detected by BirdNET not recorded in traditional surveys"
  ) |>
  cols_align(align = "left", columns = everything()) |>
  opt_stylize(style = 1) |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/bn_only_species.html")


################################# compare using 24h birdnet data
# load 24h data for birdnet
bn_24h <- fread("D:/QBIO7008/Bird_accoustic/Outputs/bn_cocoa_24h.csv")
str(bn_24h)

bn_24h_only_species <- bn_24h |>
  group_by(scientific_name) |>
  summarise(bn_detections = n_distinct(subplot_ID), .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  filter(!scientific_name %in% all_surveys_checked$scientific_name) |>
  arrange(desc(bn_detections))

head(bn_24h_only_species, 20)

#visual
bn_24h_only_species |>
  select(common_name_birdlife, scientific_name, bn_detections) |>
  mutate(common_name_birdlife = ifelse(is.na(common_name_birdlife), scientific_name, common_name_birdlife)) |>
  gt() |>
  cols_label(
    common_name_birdlife = "Common name",
    scientific_name = "Scientific name",
    bn_detections = "Subplots detected"
  ) |>
  tab_header(
    title = "Species detected by BirdNET (24h) not recorded in traditional surveys"
  ) |>
  cols_align(align = "left", columns = everything()) |>
  opt_stylize(style = 1) |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/bn_24h_only_species.html")


