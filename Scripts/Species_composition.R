######Composition of species
# BARBARA OLIVEIRA DE LORETO 23/04/2026 - with Claude support

################################################################################
######## Objective#############################################################
########1. explore data in terms of species composition
###########a. most frequent species across methods
###########b. species in one that don't appear in others
###########c. most common species at highest shade sites 
########2. Prepare data PRESENCE/ABSENCE
########3. Calculate Jaccard dissimilarity matrix
########4. Run PERMANOVA
########5. Check dispersion
########6. Visualise with NMDS 
################################################################################

#load packages
library(readxl)
library(dplyr)
library(openxlsx) #having an issue with readxl
library(ggplot2)
library(tidyr)
library(gt)
library(stringr)
library(vegan)
library(scatterplot3d)


###############################################################################
#OBJECTIVE 1 - Data Exploration
###############################################################################

library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

####load data

all_surveys_forest_birds <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/all_surveys_forest_birds.csv",
                                     stringsAsFactors = FALSE)

bn_like_for_like_forest <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like_forest.csv",
                                    stringsAsFactors = FALSE)

bn_20h_forest <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/bn_cocoa_20h_forest.csv",
                          stringsAsFactors = FALSE)

birdlife <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara//GHA-Species_BirdlifeInternational.csv")

shade <- read.csv("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/subplot_shade_cover.csv")

plot_shade <- read.csv("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/plot_shade_cover.csv")
head(plot_shade)

#filter bn data to 0.5 confidence threshold informed by Richness

bn_lfl_thresh  <- bn_like_for_like_forest |> filter(confidence >= 0.5)
bn_20h_thresh  <- bn_20h_forest           |> filter(confidence >= 0.5)

#### make sure data only includes subplots that have M and E surveys
#### make sure subplots that dropped with the forest filter are include with zero

# load data before forest filter
bn_like_for_like <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like.csv",
                             stringsAsFactors = FALSE)
all_surveys_checked <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/all_traditional_surveys.csv",
                                stringsAsFactors = FALSE)
subplots_long_ok <- read.csv(  "D:/QBIO7008/Bird_accoustic/Outputs/subplots_long_ok.csv") 

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



#### checks
# How many validated subplots?
nrow(validated_subplots) #41

# How many plots?
n_distinct(validated_subplots$plot_ID) #21

# How many 20h subplots?
length(subplots_long_ok$subplot_ID) #37

# Is C141_O in validated_subplots?
"C141_O" %in% validated_subplots$subplot_ID # yes, but in the BN data it has no forest detections in the evening

# Which validated subplots are NOT in subplots_long_ok?
validated_subplots$subplot_ID[!validated_subplots$subplot_ID %in% subplots_long_ok$subplot_ID]

#conclusion, for comparison, we will go with the #41 validated for the like for like analysis and 37 for 20h


####species detection frequency

# total number of validated subplots per dataset
n_subplots_trad <- n_distinct(all_surveys_forest_birds$ID[
  all_surveys_forest_birds$ID %in% validated_subplots$subplot_ID])

n_subplots_lfl <- nrow(validated_subplots)

n_subplots_20h <- length(subplots_long_ok$subplot_ID)

# point count survey - proportion of subplots
trad_species_freq <- all_surveys_forest_birds |>
  filter(ID %in% validated_subplots$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(trad_prop = n_distinct(ID) / n_subplots_trad,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(trad_prop))

write.csv(trad_species_freq,
          "D:/QBIO7008/Bird_accoustic/Outputs/composition_freq_trad_survey.csv",
          row.names = FALSE)

# LFL BirdNET - proportion of subplots
bn_lfl_species_freq <- bn_lfl_thresh |>
  filter(subplot_ID %in% validated_subplots$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(bn_prop = n_distinct(subplot_ID) / n_subplots_lfl,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(bn_prop))

write.csv(bn_lfl_species_freq,
          "D:/QBIO7008/Bird_accoustic/Outputs/composition_freq_birdnet_lfl.csv",
          row.names = FALSE)

# 20h BirdNET - proportion of subplots
bn_20h_species_freq <- bn_20h_thresh |>
  filter(subplot_ID %in% subplots_long_ok$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(bn_prop = n_distinct(subplot_ID) / n_subplots_20h,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(bn_prop))

write.csv(bn_20h_species_freq,
          "D:/QBIO7008/Bird_accoustic/Outputs/composition_freq_birdnet_20h.csv",
          row.names = FALSE)


# Combined frequency tables
species_freq_lfl <- full_join(trad_species_freq, bn_lfl_species_freq,
                              by = c("scientific_name", "common_name_birdlife")) |>
  select(common_name_birdlife, scientific_name, trad_prop, bn_prop) |>
  arrange(desc(bn_prop))

species_freq_20h <- full_join(trad_species_freq, bn_20h_species_freq,
                              by = c("scientific_name", "common_name_birdlife")) |>
  select(common_name_birdlife, scientific_name, trad_prop, bn_prop) |>
  arrange(desc(bn_prop))


#### developing diverging bar chart

#functions for chart creation
make_diverging_chart <- function(freq_table, chart_title) {
  top15 <- freq_table |>
    slice(1:15) |>
    mutate(
      common_name_birdlife = ifelse(is.na(common_name_birdlife), scientific_name, common_name_birdlife),
      trad_prop            = ifelse(is.na(trad_prop), 0, trad_prop),
      bn_prop              = ifelse(is.na(bn_prop),   0, bn_prop)
    )
  
  top15 |>
    mutate(
      common_name_birdlife = reorder(common_name_birdlife, bn_prop),
      trad_prop            = -trad_prop
    ) |>
    pivot_longer(cols = c(trad_prop, bn_prop),
                 names_to = "method", values_to = "proportion") |>
    mutate(method = ifelse(method == "trad_prop", "point count survey", "BirdNET")) |>
    ggplot(aes(x = proportion, y = common_name_birdlife, fill = method)) +
    geom_col() +
    scale_x_continuous(labels = abs,
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, by = 0.25)) +
    scale_fill_manual(values = c("point count survey" = "steelblue",
                                 "BirdNET"            = "darkorange")) +
    geom_vline(xintercept = 0, colour = "black", linewidth = 0.5) +
    labs(title = chart_title,
         x     = "Proportion of subplots detected",
         y     = NULL,
         fill  = "Method") +
    theme_bw()
}

#charts

chart_lfl <- make_diverging_chart(
  species_freq_lfl,
  "Top 15 species by BirdNET detections — like-for-like window"
)

chart_20h <- make_diverging_chart(
  species_freq_20h,
  "Top 15 species by BirdNET detections — 20h window"
)

chart_lfl
chart_20h

###############################################################################
#OBJECTIVE 2 - prepare presence absence data
###############################################################################

#### make sure data only includes subplots that have M and E surveys
#### make sure subplots that dropped with the forest filter are include with zero

#### checks
# How many validated subplots?
nrow(validated_subplots) #41

# How many plots?
n_distinct(validated_subplots$plot_ID) #21

# How many 20h subplots?
length(subplots_long_ok$subplot_ID) #37


#### create the matrix

#trad_survey

comm_trad <- all_surveys_forest_birds |>
  filter(ID %in% validated_subplots$subplot_ID) |>
  distinct(plot_ID, ID, scientific_name) |>          # collapse to presence only
  mutate(present = 1L) |>                   # add a value column
  pivot_wider(
    id_cols     = c(ID, plot_ID),            # one row per subplot
    names_from  = scientific_name,           # one column per species
    values_from = present,                   # fill with 1
    values_fill = 0L                         # absent species get 0
  ) |>
  rename(subplot_ID = ID)

dim(comm_trad)
comm_trad <- comm_trad |>
  select(plot_ID, subplot_ID, everything())

#bn like for like filter
#check for the 0.5 CI (for BirdNET) filter which subplots remain
# Which validated subplots have zero detections in LFL at 0.5 threshold?
validated_subplots$subplot_ID[!validated_subplots$subplot_ID %in% 
                                unique(bn_lfl_thresh$subplot_ID)] #C08_B and C27_O

#check NAs
bn_lfl_thresh |>
  filter(subplot_ID %in% validated_subplots$subplot_ID) |>
  filter(is.na(scientific_name)) |>
  distinct(subplot_ID, common_name, scientific_name)


comm_bn_lfl <- bn_lfl_thresh |>
  filter(subplot_ID %in% validated_subplots$subplot_ID) |>
  distinct(plot_ID, subplot_ID, scientific_name) |> # collapse to presence only
  mutate(present = 1L) |>                   # add a value column
  pivot_wider(
    id_cols     = c(plot_ID,subplot_ID),    # one row per subplot
    names_from  = scientific_name,           # one column per species
    values_from = present,                   # fill with 1
    values_fill = 0L                         # absent species get 0
  ) 

#add back plots that have zero after the forest bird filter and after the 0.5 filter
# create a dataframe of zero rows for missing subplots
zero_rows <- tibble(
  subplot_ID = c("C08_B", "C27_O"),
  plot_ID    = c(  "C08",   "C27")
)

# add missing species columns filled with 0
zero_rows <- zero_rows |>
  bind_cols(
    matrix(0L, 
           nrow = 2, 
           ncol = ncol(comm_bn_lfl) - 2,  # minus subplot_ID and plot_ID
           dimnames = list(NULL, 
                           names(comm_bn_lfl)[!names(comm_bn_lfl) %in% 
                                                c("subplot_ID", "plot_ID")])) |>
      as.data.frame()
  )

# bind to main matrix
comm_bn_lfl <- bind_rows(comm_bn_lfl, zero_rows)


dim(comm_bn_lfl)

# for the 20h bn dataset
#check first which ones would have dropped and are zeros
subplots_long_ok[!subplots_long_ok$subplot_ID %in% unique(bn_20h_thresh$subplot_ID)] #0

#check NAs
bn_20h_thresh |>
  filter(subplot_ID %in% subplots_long_ok$subplot_ID) |>
  filter(is.na(scientific_name)) |>
  distinct(subplot_ID, common_name, scientific_name)

# build matrix
comm_bn_20h <- bn_20h_thresh |>
  filter(subplot_ID %in% subplots_long_ok$subplot_ID) |>
  distinct(plot_ID, subplot_ID, scientific_name) |> # collapse to presence only
  mutate(present = 1L) |>                   # add a value column
  pivot_wider(
    id_cols     = c(plot_ID,subplot_ID),    # one row per subplot
    names_from  = scientific_name,           # one column per species
    values_from = present,                   # fill with 1
    values_fill = 0L                         # absent species get 0
  ) 

dim(comm_bn_20h)


#checks 
names(comm_trad)[1:5]
names(comm_bn_lfl)[1:5]
names(comm_bn_20h)[1:5]

###############################################################################
#OBJECTIVE 3 - jaccard distance matrix
###############################################################################

#### stacking matrices for the analysis
#first bn_lfk and trad_surveys

comm_lfl_combined <- bind_rows(
  comm_trad    |> mutate(method = "point count"),
  comm_bn_lfl  |> mutate(method = "BirdNET")
) |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0L)))

# build metadata
meta_lfl <- comm_lfl_combined |>
  select(subplot_ID, plot_ID, method) |>
  left_join(plot_shade |> select(plot_id, plot_shade),
            by = c("plot_ID" = "plot_id"))

# extract species matrix - for vegdist
spp_lfl <- comm_lfl_combined |>
  select(-subplot_ID, -plot_ID, -method)

# calculate distance matrix
dist_lfl <- vegdist(spp_lfl, method = "jaccard", binary = TRUE)
### conclusion, jaccard cannot deal with the zero input surveys. so they will need to be removed 


#check
dim(spp_lfl)       # should be 41 x n species
dim(meta_lfl)      # should be 41 x 4
sum(is.na(meta_lfl$plot_shade))  # should be 0
empty_rows <- rowSums(spp_lfl) == 0
meta_lfl[empty_rows, ]
### conclusion #C08_B and C27_O removed

# remove empty row subplots from both methods
comm_lfl_combined <- comm_lfl_combined |>
  filter(!subplot_ID %in% c("C08_B", "C27_O"))

# re-extract species matrix and metadata
spp_lfl <- comm_lfl_combined |>
  select(-subplot_ID, -plot_ID, -method)

meta_lfl <- comm_lfl_combined |>
  select(subplot_ID, plot_ID, method) |>
  left_join(plot_shade |> select(plot_id, plot_shade),
            by = c("plot_ID" = "plot_id"))

# recalculate distance matrix
dist_lfl <- vegdist(spp_lfl, method = "jaccard", binary = TRUE)

# check
dim(spp_lfl)   # should be 78 rows (39 subplots x 2 methods)
dim(meta_lfl)  # should be 78 x 4

#second bn_20h and trad_surveys
#make sure to not accidentaly include additional surveys in the trad data

comm_20h_combined <- bind_rows(
  comm_trad |>
    filter(subplot_ID %in% subplots_long_ok$subplot_ID) |>
    mutate(method = "point count"),
  comm_bn_20h |>
    mutate(method = "BirdNET")
) |>
  mutate(across(where(is.numeric), ~ replace_na(.x, 0L)))

# Check
comm_20h_combined |> filter(method == "point count") |> nrow()
comm_20h_combined |> filter(method == "BirdNET") |> nrow()

#check subplots
# which subplot is not in subplots_long_ok?
comm_20h_combined |>
  filter(method == "BirdNET") |>
  filter(!subplot_ID %in% subplots_long_ok$subplot_ID) |>
  distinct(subplot_ID)


# build metadata
meta_20h <- comm_20h_combined |>
  select(subplot_ID, plot_ID, method) |>
  left_join(plot_shade |> select(plot_id, plot_shade),
            by = c("plot_ID" = "plot_id"))

# extract species matrix - for vegdist
spp_20h <- comm_20h_combined |>
  select(-subplot_ID, -plot_ID, -method)

# recalculate distance matrix
dist_20h <- vegdist(spp_20h, method = "jaccard", binary = TRUE)

# check
dim(spp_20h)  #74 113
dim(meta_20h) #80 x 4
sum(is.na(meta_20h$plot_shade))  

###############################################################################
#OBJECTIVE 4 - run PERMANOVA
###############################################################################

#by margin was attempted but it is returning only the interaction term
perm_lfl <- adonis2(dist_lfl ~ method * plot_shade,
            data         = meta_lfl,
            permutations = 999,
            strata       = meta_lfl$plot_ID,
            by           = "margin")

print(perm_lfl)

#by term - most important being the interaction term for my question and since that is lat, it should be ok
perm_lfl_terms <- adonis2(dist_lfl ~ method * plot_shade,
                          data         = meta_lfl,
                          permutations = 999,
                          strata       = meta_lfl$plot_ID,
                          by           = "terms")

print(perm_lfl_terms)

# now for the 20h dataset

perm_20h <- adonis2(dist_20h ~ method * plot_shade,
                    data         = meta_20h,
                    permutations = 999,
                    strata       = meta_20h$plot_ID,
                    by           = "terms")

print(perm_20h)

###############################################################################
#OBJECTIVE 5 - check dispersion
###############################################################################

# LFL
bd_lfl <- betadisper(dist_lfl, meta_lfl$method)
set.seed(123)
perm_bd_lfl <- permutest(bd_lfl, permutations = 999) #significant result may mean dispersion difference and not centroid difference

#checking which method is more dispersed
bd_lfl$group.distances

# dispersion birdnet vs point count(0.62 vs 0.47, p = 0.001)

#now for 20h survey period
bd_20h <- betadisper(dist_20h, meta_20h$method)
set.seed(123)
perm_bd_20h <-permutest(bd_20h, permutations = 999)
bd_20h$group.distances

# dispersion birdnet vs point count (0.44 vs 0.46, p = 0.056)
###############################################################################
#OBJECTIVE 6 - visualise with NMDS
###############################################################################

#for symetrical sampling first
# nmds_lfl <- metaMDS(dist_lfl, k = 2, trymax = 100)
# nmds_lfl$stress
# # stress a bit over 0.2 and dificulty converging (warning) 

nmds_lfl_k2 <- metaMDS(dist_lfl, k = 2, trymax = 100)
nmds_lfl_k2$stress

# #attempt different k and trymax
# nmds_lfl <- metaMDS(dist_lfl, k = 3, trymax = 200)
# nmds_lfl$stress #better overall - lowers strees and found a solution

#now for 20h
nmds_20h_k2 <- metaMDS(dist_20h, k = 2, trymax = 100)
nmds_20h_k2$stress

#### extracting data for plotting

# LFL scores
scores_lfl <- as.data.frame(scores(nmds_lfl_k2, display = "sites")) |>
  bind_cols(meta_lfl)

# 20h scores
scores_20h <- as.data.frame(scores(nmds_20h_k2, display = "sites")) |>
  bind_cols(meta_20h)

####LFL

# stress label
stress_label_lfl <- data.frame(
  label = paste("Stress =", round(nmds_lfl_k2$stress, 3))
)

fig_nmds_lfl <- ggplot(scores_lfl,
                       aes(x = NMDS1, y = NMDS2,
                           shape = method)) +
  stat_ellipse(aes(group = method),
               colour = "grey40",
               linetype = "dashed",
               level = 0.95,
               show.legend = FALSE) +
  geom_point(aes(colour = method),
             size = 3, alpha = 0.8) +
  geom_text(data = stress_label_lfl,
            aes(x = Inf, y = -Inf, label = label),
            hjust = 1.1, vjust = -0.5,
            size = 3.5, inherit.aes = FALSE) +
  labs(x = "NMDS1",
       y = "NMDS2",
       title = "Community composition — Like-for-like (20-minute window)") +
  theme_classic() +
  theme(legend.position = "bottom")

fig_nmds_lfl

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_nmds_lfl.png",
       fig_nmds_lfl, width = 14, height = 6, dpi = 300)

####20h

# stress label
stress_label_20h <- data.frame(
  label = paste("Stress =", round(nmds_20h_k2$stress, 3))
)

fig_nmds_20h <- ggplot(scores_20h,
                       aes(x = NMDS1, y = NMDS2,
                           shape = method)) +
  stat_ellipse(aes(group = method),
               colour = "grey40",
               linetype = "dashed",
               level = 0.95,
               show.legend = FALSE) +
  geom_point(aes(colour = method),
             size = 3, alpha = 0.8) +
  geom_text(data = stress_label_20h,
            aes(x = Inf, y = -Inf, label = label),
            hjust = 1.1, vjust = -0.5,
            size = 3.5, inherit.aes = FALSE) +
  labs(x = "NMDS1",
       y = "NMDS2",
       title = "Community composition — Extended window (20-hour recording)") +
  theme_classic() +
  theme(legend.position = "bottom")

fig_nmds_20h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_nmds_20h.png",
       fig_nmds_20h, width = 14, height = 6, dpi = 300)

##### tables and figures for the report

#permanova tables
perm_lfl_terms
perm_20h

# helper function to convert adonis2 output to a clean dataframe
permanova_to_df <- function(perm_obj) {
  as.data.frame(perm_obj) |>
    tibble::rownames_to_column("Term") |>
    mutate(Term = recode(Term,
                         "method"            = "Method",
                         "plot_shade"        = "Shade cover",
                         "method:plot_shade" = "Method × Shade cover",
                         "Residual"          = "Residual",
                         "Total"             = "Total")) |>
    mutate(across(c(R2, F), ~ round(.x, 3)),
           SumOfSqs = round(SumOfSqs, 3),
           `Pr(>F)` = ifelse(is.na(`Pr(>F)`), NA,
                             ifelse(`Pr(>F)` < 0.001, "<0.001",
                                    as.character(round(`Pr(>F)`, 3)))))
}

# build tables
df_lfl <- permanova_to_df(perm_lfl_terms)
df_20h <- permanova_to_df(perm_20h)

# LFL PERMANOVA table
gt_perm_lfl <- df_lfl |>
  gt() |>
  tab_header(
    title = "PERMANOVA results — Like-for-like (20-minute window)"
  ) |>
  cols_label(
    Term     = "Term",
    Df       = "df",
    SumOfSqs = "Sum of squares",
    R2       = "R²",
    F        = "F",
    `Pr(>F)` = "p"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = `Pr(>F)` %in% c("0.001", "<0.001")
    )
  ) |>
  tab_footnote(
    footnote = "Permutations restricted within plot identity (strata = plot_ID). Terms tested sequentially. Bold values indicate p ≤ 0.001."
  ) |>
  tab_options(table.font.size = 11, heading.align = "left")

gt_perm_lfl |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/permanova_table_lfl.docx")


# 20h PERMANOVA table
gt_perm_20h <- df_20h |>
  gt() |>
  tab_header(
    title = "PERMANOVA results — Extended window (20-hour recording)"
  ) |>
  cols_label(
    Term     = "Term",
    Df       = "df",
    SumOfSqs = "Sum of squares",
    R2       = "R²",
    F        = "F",
    `Pr(>F)` = "p"
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = `Pr(>F)` %in% c("0.001", "<0.001")
    )
  ) |>
  tab_footnote(
    footnote = "Permutations restricted within plot identity (strata = plot_ID). Terms tested sequentially. Bold values indicate p ≤ 0.001."
  ) |>
  tab_options(table.font.size = 11, heading.align = "left")

gt_perm_20h |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/permanova_table_20h.docx")

### for dispersion

# build dispersion summary dataframe
disp_df <- data.frame(
  Analysis         = c("Like-for-like (20-minute window)",
                       "Extended window (20-hour recording)"),
  BirdNET_distance = c(bd_lfl$group.distances["BirdNET"],
                       bd_20h$group.distances["BirdNET"]),
  Trad_distance    = c(bd_lfl$group.distances["point count"],
                       bd_20h$group.distances["point count"]),
  F_stat           = c(perm_bd_lfl$tab["Groups", "F"],
                       perm_bd_20h$tab["Groups", "F"]),
  p_value          = c(round(perm_bd_lfl$tab["Groups", "Pr(>F)"], 3),
                       round(perm_bd_20h$tab["Groups", "Pr(>F)"], 3))
)

gt_dispersion <- disp_df |>
  gt() |>
  tab_header(
    title = "Multivariate dispersion results (betadisper)"
  ) |>
  cols_label(
    Analysis         = "Analysis",
    BirdNET_distance = "BirdNET",
    Trad_distance    = "point count",
    F_stat           = "F",
    p_value          = "p"
  ) |>
  tab_spanner(
    label   = "Mean distance to centroid",
    columns = c(BirdNET_distance, Trad_distance)
  ) |>
  tab_footnote(
    footnote = "F and p values from permutation tests (999 permutations, seed = 123)."
  ) |>
  tab_options(table.font.size = 11, heading.align = "left")

gt_dispersion|>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/betadisper_tabble.docx")



#check
bd_lfl$group.distances
bd_20h$group.distances
set.seed(123)
permutest(bd_lfl, permutations = 999)$tab
set.seed(123)
permutest(bd_20h, permutations = 999)$tab



###############################################################################
#### OBJECTIVE 7 - KEY DATA FOR THE REPORT ####################################
###############################################################################

#number of subplots included in the analysis lkl
dim(meta_lfl)      # should be 78 x 4 #78/2 as for each subpoint two sets of data
unique(meta_lfl$subplot_ID)

#number of subplots included in the analysis 20H
dim(meta_20h)

# total number of species detected
#by method
all_surveys_forest_birds |>
  filter(ID %in% meta_lfl$subplot_ID) |>
 distinct(scientific_name) #84

bn_lfl_thresh |>
  filter(subplot_ID %in%meta_lfl$subplot_ID) |>
  distinct(scientific_name) #38

bn_20h_thresh |>
  filter(subplot_ID %in% meta_20h$subplot_ID) |>
  distinct(scientific_name) #78

  
# number of subplots lfl analysis
unique(meta_lfl$subplot_ID) #39

unique(meta_20h$subplot_ID)#37

#species in  point count
species_trad <- all_surveys_forest_birds |>
  filter(ID %in% meta_lfl$subplot_ID) |>
  distinct(scientific_name)|>
  mutate(method= "point count")

#species in BN lfl
species_bn_lfl <- bn_lfl_thresh |>
  filter(subplot_ID %in% meta_lfl$subplot_ID) |>
  distinct(scientific_name)|>
  mutate(method = "BN_lfl")

#species in BN 20h
species_bn_20h <- bn_20h_thresh |>
  filter(subplot_ID %in% meta_20h$subplot_ID) |>
  distinct(scientific_name)|>
  mutate(method= "BN_20h")

#all species classified by method
all_species <- full_join(species_trad,species_bn_lfl)|>
  full_join(species_bn_20h)

# Species unique to point count (not detected by either BirdNET method)
bn_species <- all_species |>
  filter(method != "point count") |>
  pull(scientific_name) |>
  unique()

all_species |>
  filter(method == "point count") |>
  filter(!scientific_name %in% bn_species) |>
  distinct(scientific_name) |>
  arrange(scientific_name)

# do the same using the frequency dataset - first LFL
#trad survey adjusted to the subplots in the analysis
trad_species_freq_adjusted <- all_surveys_forest_birds |>
  filter(ID %in% meta_lfl$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(trad_prop = n_distinct(ID) / n_subplots_trad,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(trad_prop))

write.csv(trad_species_freq_adjusted,
          "D:/QBIO7008/Bird_accoustic/Outputs/composition_freq_trad_survey_adjusted.csv",
          row.names = FALSE)

#most frequent species point-count pool only
trad_species_freq_adjusted|> filter(!scientific_name %in% species_bn_lfl$scientific_name)|>
  distinct()

#Number of species unique to BirdNET LKL

bn_lfl_species_freq_adjusted <- bn_lfl_thresh |>
  filter(subplot_ID %in% meta_lfl$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(bn_prop = n_distinct(subplot_ID) / n_subplots_lfl,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(bn_prop))

write.csv(bn_lfl_species_freq_adjusted,
          "D:/QBIO7008/Bird_accoustic/Outputs/composition_freq_birdnet_lfl_adjusted.csv",
          row.names = FALSE)



#species only found with BN in the 20min sampling
bn_lfl_species_freq_adjusted |> filter(!scientific_name %in% species_trad$scientific_name)|>
  distinct()


#proportion of point count also detected by BirdNET lkl
trad_species_freq_adjusted|> filter(scientific_name %in% species_bn_lfl$scientific_name)|>
  distinct() #35

# species in LFL analysis that only occur in one subplot
# For traditional survey
all_surveys_forest_birds |>
  filter(ID %in% meta_lfl$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(n_subplots = n_distinct(ID), .groups = "drop") |>
  filter(n_subplots == 1) |>
  arrange(scientific_name)|>
  print(n=40)

# For BirdNET LFL
bn_lfl_thresh |>
  filter(subplot_ID %in% meta_lfl$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(n_subplots = n_distinct(subplot_ID), .groups = "drop") |>
  filter(n_subplots == 1) |>
  arrange(scientific_name)

# do the same using the frequency dataset - 20h
#trad survey adjusted to the subplots in the analysis
trad_species_freq_adjusted <- all_surveys_forest_birds |>
  filter(ID %in% meta_20h$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(trad_prop = n_distinct(ID) / n_subplots_trad,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(trad_prop))

trad_species_freq_adjusted|> filter(!scientific_name %in% species_bn_20h$scientific_name)|>
  distinct() #34

#Number of species unique to BirdNET 20h

bn_20h_species_freq_adjusted <- bn_20h_thresh |>
  filter(subplot_ID %in% meta_20h$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(bn_prop = n_distinct(subplot_ID) / n_subplots_lfl,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(bn_prop))

write.csv(bn_20h_species_freq_adjusted,
          "D:/QBIO7008/Bird_accoustic/Outputs/composition_freq_birdnet_20h_adjusted.csv",
          row.names = FALSE)

bn_20h_species_freq_adjusted |> filter(!scientific_name %in% species_trad$scientific_name)|>
  distinct()



#proportion of point count also detected by BirdNET 20h
trad_species_freq_adjusted|> filter(scientific_name %in% species_bn_20h$scientific_name)|>
  distinct() #48
