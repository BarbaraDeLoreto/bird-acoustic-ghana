#Investigating the issues with missing species
# run after Bird

library(readxl)
library(openxlsx)
library(dplyr)
library(data.table)
library(stringr)
library(tidyr)

# Load data 
ebird_species <- read_excel("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/eBird-Clements_v2025-integrated-checklist-October-2025.xlsx") %>%
  filter(category == "species")
birdlife <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/GHA-Species_BirdlifeInternational.csv")


birdnet_list_used <- read_excel("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/birdnet_species_list_used.xlsx")

birdnet_labels_full <- readLines("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara//BirdNET_GLOBAL_6K_V2.4_Labels.txt")
birdnet_full_sci <- sub("_.*", "", birdnet_labels_full)

bn_w <- fread("D:/QBIO7008/Bird_accoustic/Outputs/birdnet_wrangled.csv")


all_surveys_forest_birds <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/all_surveys_forest_birds.csv",
                                     stringsAsFactors = FALSE)

all_surveys_checked <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/all_traditional_surveys.csv",
                                                                stringsAsFactors = FALSE)

validated_subplots <-   read.csv("D:/QBIO7008/Bird_accoustic/Outputs/validated_subplots.csv")

# Get missing species 
trad_species <- unique(all_surveys_checked$scientific_name)
bn_species <- unique(bn_w$scientific_name)
missing_from_bn <- trad_species[!trad_species %in% bn_species]

# Get most frequent common name per species
common_name_lookup <- all_surveys_checked %>%
  filter(!is.na(scientific_name), !is.na(common_name)) %>%
  group_by(scientific_name) %>%
  count(common_name) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(scientific_name, common_name) %>%
  ungroup()

#  Build summary table 
missing_df <- data.frame(scientific_name = sort(missing_from_bn)) %>%
  left_join(common_name_lookup, by = "scientific_name") %>%
  mutate(
    in_ebird_clements   = scientific_name %in% ebird_species$`scientific name`,
    in_birdnet_list_used = scientific_name %in% birdnet_list_used$scientific_name,
    in_birdnet_full_model = scientific_name %in% birdnet_full_sci
  )

print(missing_df)
nrow(missing_df)

# Save 
write.xlsx(missing_df,
           "D:/QBIO7008/Bird_accoustic/Results/trad_survey_species_missing_from_bn.xlsx",
           rowNames = FALSE)


# Get missing forest species 
trad_forest_species <- all_surveys_forest_birds |> 
  filter(ID %in% validated_subplots$subplot_ID) |>
  distinct(scientific_name)
trad_forest_species <- unique(trad_forest_species$scientific_name)

missing_from_bn_forest <- trad_forest_species[!trad_forest_species %in% bn_species]

missing_from_bn_labels <- trad_forest_species[!trad_forest_species %in% birdnet_full_sci]

missing_from_bn_forest%in% missing_from_bn_labels

# Get most frequent common name per forest species 
common_name_lookup_forest <- all_surveys_forest_birds %>%
  filter(!is.na(scientific_name), !is.na(common_name)) %>%
  group_by(scientific_name) %>%
  count(common_name) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  select(scientific_name, common_name) %>%
  ungroup()

# Build forest summary table 
missing_df_forest <- data.frame(scientific_name = sort(missing_from_bn_forest)) %>%
  left_join(common_name_lookup_forest, by = "scientific_name") %>%
  mutate(
    in_ebird_clements     = scientific_name %in% ebird_species$`scientific name`,
    in_birdnet_list_used  = scientific_name %in% birdnet_list_used$scientific_name,
    in_birdnet_full_model = scientific_name %in% birdnet_full_sci
  )

print(missing_df_forest)
nrow(missing_df_forest)

# Save forest table
write.xlsx(missing_df_forest,
           "D:/QBIO7008/Bird_accoustic/Results/trad_survey_forest_species_missing_from_bn.xlsx",
           rowNames = FALSE)

#list of species in the point count forest data that is missing from birdnet labesl
missing_from_bn_labels <- trad_forest_species[!trad_forest_species %in% birdnet_full_sci]
write.csv(missing_from_bn_labels,
           "D:/QBIO7008/Bird_accoustic/Results/trad_survey_forest_species_missing_from_bn_labels.csv")

################################################################################
# investigating the forest birds filter
################################################################################

# load the forest birds data
forest_birds <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/forest_bird_species_list.csv")

names(forest_birds)
nrow(forest_birds)

length(birdnet_full_sci)
head(birdnet_full_sci, 5)


# Which forest_birds names are absent from BirdNET's full label set? ----
# These are species BirdNET could never output under the BirdLife name
missing_from_birdnet_labels <- forest_birds %>%
  filter(!scientific_name_birdlife %in% birdnet_full_sci) %>%
  select(scientific_name_birdlife, common_name_birdlife)

nrow(missing_from_birdnet_labels)
print(missing_from_birdnet_labels)

# For those missing, check if they exist in eBird/Clements
# If in eBird, BirdNET may know them under a different (Clements) name
missing_from_birdnet_labels <- missing_from_birdnet_labels %>%
  mutate(
    in_ebird_by_sciname = scientific_name_birdlife %in% ebird_species$`scientific name`,
    in_ebird_by_comname = common_name_birdlife %in% ebird_species$`English name`
  )

# Summary of categories
missing_from_birdnet_labels %>%
  count(in_ebird_by_sciname, in_ebird_by_comname)

write.xlsx(
  missing_from_birdnet_labels |>
    select(scientific_name_birdlife, common_name_birdlife, 
           in_ebird_by_sciname, in_ebird_by_comname),
  "D:/QBIO7008/Bird_accoustic/Results/forest_birds_missing_from_birdnet_134.xlsx",
  rowNames = FALSE
)

# claude used to come up with an analysis of the reason for the 134 species being absent. They were put into categories
# Cat A & B — verify proposed BirdNET equivalents exist in label set
cat_ab_equivalents <- c(
  # Cat A
  "Ketupa poensis",
  "Ketupa shelleyi",
  "Ketupa leucosticta",
  "Tachyspiza erythropus",
  "Aerospiza toussenelii",
  "Accipiter toussenelii",
  "Turdoides atripennis",
  "Platysteira concreta",
  "Neocossyphus finschi",
  # Cat B
  "Lophoceros fasciatus",
  "Trachyphonus purpuratus",
  "Cercococcyx mechowi",
  "Buccanodon duchaillui",
  "Guttera pucherani"
)

data.frame(
  name = cat_ab_equivalents,
  in_birdnet = cat_ab_equivalents %in% birdnet_full_sci
)

# Cat E — check if BirdLife scientific name is already in BirdNET
cat_e_names <- c(
  "Canirallus oculeus", "Otus icterorhynchus", "Scotopelia ussheri",
  "Aviceda cuculoides", "Circaetus cinerascens", "Horizocerus hartlaubi",
  "Rhinopomastus castaneiceps", "Ispidina lecontei", "Ispidina picta",
  "Corythornis leucogaster", "Lobotos lobatus", "Elminia nigromitrata",
  "Anthoscopus flavifrons", "Bathmocercus cerviniventris",
  "Psalidoprocne nitens", "Psalidoprocne obscura", "Eurillas gracilis",
  "Campethera maculosa", "Geokichla princei", "Tychaedon leucosticta",
  "Muscicapa epulata", "Bradornis comitatus", "Fraseria cinerascens",
  "Malimbus cassini", "Mandingoa nitidula"
)

data.frame(
  name = cat_e_names,
  in_birdnet = cat_e_names %in% birdnet_full_sci
)


# Check if Lophoceros semifasciatus and Trachylaemus goffinii are in BirdNET
# (crosswalk says concepts_match so Clements recognises them — they should have labels)
c("Lophoceros semifasciatus", "Trachylaemus goffinii",
  "Bubo poensis", "Bubo shelleyi", "Bubo leucostictus",
  "Accipiter erythropus") %in% birdnet_full_sci

# Search birdnet_labels_full for our key species by partial scientific name
# Labels format is "Common Name_Scientific Name"

search_birdnet <- function(sci_names) {
  sapply(sci_names, function(sp) {
    hits <- birdnet_labels_full[str_detect(birdnet_labels_full, fixed(sp))]
    if (length(hits) == 0) NA else paste(hits, collapse = " | ")
  })
}

search_birdnet(c(
  "Lophoceros semifasciatus",
  "Lophoceros fasciatus",
  "Trachylaemus goffinii",
  "Trachyphonus purpuratus",
  "Bubo poensis",
  "Bubo shelleyi", 
  "Bubo leucostictus",
  "Ketupa poensis",
  "Ketupa shelleyi",
  "Ketupa leucosticta",
  "Accipiter erythropus",
  "Tachyspiza erythropus",
  "Accipiter toussenelii",
  "Aerospiza tachiro"
))



clements_rename_names <- c(
  "Ketupa poensis",
  "Ketupa leucosticta", 
  "Ketupa shelleyi",
  "Chloropicus pyrrhogaster",
  "Psittacula krameri",
  "Platysteira hormophora",
  "Platysteira tonsa",
  "Platysteira blissetti",
  "Telophorus multicolor",
  "Phyllastrephus scandens",
  "Bradornis ussheri",
  "Cercotrichas leucosticta"
)

clements_parent_names <- c(
  "Cercococcyx mechowi",
  "Trachyphonus purpuratus",
  "Buccanodon duchaillui",
  "Platysteira concreta",
  "Phyllastrephus albigularis",
  "Turdoides atripennis",
  "Anthreptes rectirostris"
)

data.frame(
  name = c(clements_rename_names, clements_parent_names),
  type = c(rep("rename", length(clements_rename_names)), 
           rep("parent", length(clements_parent_names))),
  in_birdnet = c(clements_rename_names, clements_parent_names) %in% birdnet_full_sci
)

# For the FALSE renames, check if BirdNET uses the OLD BirdLife name instead
old_birdlife_names <- c(
  "Bubo poensis",           # rename to Ketupa poensis failed
  "Bubo leucostictus",      # rename to Ketupa leucosticta failed
  "Bubo shelleyi",          # rename to Ketupa shelleyi failed
  "Dendropicos pyrrhogaster", # rename to Chloropicus pyrrhogaster failed
  "Dyaphorophyia hormophora", # rename to Platysteira hormophora failed
  "Dyaphorophyia tonsa",      # rename to Platysteira tonsa failed
  "Chlorophoneus multicolor", # rename to Telophorus multicolor failed
  "Pyrrhurus scandens",       # rename to Phyllastrephus scandens failed
  "Artomyias ussheri",        # rename to Bradornis ussheri failed
  "Tychaedon leucosticta"     # rename to Cercotrichas leucosticta failed
)

data.frame(
  birdlife_name = old_birdlife_names,
  in_birdnet = old_birdlife_names %in% birdnet_full_sci
)



# Step 1: get all BirdNET scientific names
birdnet_sci <- data.frame(scientific_name = birdnet_full_sci)

# Step 2: load the crosswalk
crosswalk <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/OTT_crosswalk_2023.csv")

# Step 3: join BirdNET species to crosswalk on Clements scientific name
birdnet_mapped <- birdnet_sci |>
  left_join(
    crosswalk |> select(SCI_NAME, PRIMARY_COM_NAME, Birdlife_name, Birdlife_match_type),
    by = c("scientific_name" = "SCI_NAME")
  )

# Step 4: for each BirdNET species, check if its BirdLife name
# (or any of the semicolon-separated BirdLife names) is in forest_birds
# Split the Birdlife_name column on semicolons first
library(tidyr)

birdnet_expanded <- birdnet_mapped |>
  mutate(Birdlife_name = str_split(Birdlife_name, ";")) |>
  unnest(Birdlife_name) |>
  mutate(Birdlife_name = str_trim(Birdlife_name))

# Step 5: flag which BirdNET species map to a forest_birds species
# but are NOT themselves in forest_birds
candidates <- birdnet_expanded |>
  filter(
    Birdlife_name %in% forest_birds$scientific_name_birdlife,  # maps to a forest bird
    !scientific_name %in% forest_birds$scientific_name_birdlife  # but not already in filter
  ) |>
  select(scientific_name, PRIMARY_COM_NAME, Birdlife_name, Birdlife_match_type)

print(candidates)
nrow(candidates)

###############################################################################
########## Frequency of the missing species in the point count data############
################################################################################
# total number of validated subplots per dataset
n_subplots_trad <- n_distinct(all_surveys_forest_birds$ID[
  all_surveys_forest_birds$ID %in% validated_subplots$subplot_ID])


#missing species frequency
# point count survey - proportion of subplots
missing_species_freq <- all_surveys_forest_birds |> 
  filter(ID %in% validated_subplots$subplot_ID) |>
  group_by(scientific_name) |>
  summarise(trad_prop = n_distinct(ID) / n_subplots_trad,
            .groups = "drop") |>
  left_join(birdlife |> select(ScientificName, CommonName),
            by = c("scientific_name" = "ScientificName")) |>
  rename(common_name_birdlife = CommonName) |>
  arrange(desc(trad_prop))|>
  filter(scientific_name %in% missing_from_bn_labels)

write.csv(missing_species_freq,
          "D:/QBIO7008/Bird_accoustic/Outputs/missing_species_freq.csv",
          row.names = FALSE)
