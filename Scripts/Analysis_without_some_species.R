###############################################################################
############# TEST REMOVING FROM ANALISYS SPECIES NOT PRESENT IN BIRDNET
#Barbara Oliveira De Loreto 20/05 with Claude's help in syntax of some fuctions, solving bugs

#####after Investigating issues script and idea was sugested to exclude the species
# not present in BirdNET and run the analysis again. This scrip attempts that



###############################################################################
########## FIRST FOR RICHNESS###################################################
##############################################################################


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
library(vegan)

###############################################################################
#OBJECTIVE 1 - load all datasets
###############################################################################

#load the all surveys dataset
all_surveys_forest_birds <- fread("D:/QBIO7008/Bird_accoustic/Outputs/all_surveys_forest_birds.csv")

#load the list fo species not included in BirdNET - forest
missing_df_forest <- read_excel("D:/QBIO7008/Bird_accoustic/Results/trad_survey_forest_species_missing_from_bn.xlsx")
missing_from_bn_labels <- read.csv("D:/QBIO7008/Bird_accoustic/Results/trad_survey_forest_species_missing_from_bn_labels.csv")|>
  pull(x)

#load the birdnet data already filtered for forest birds
bn_like_for_like_forest <- fread ("D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like_forest.csv")
bn_like_for_like <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like.csv")

# load the 20h file
bn_20h_forest <- fread ("D:/QBIO7008/Bird_accoustic/Outputs/bn_cocoa_20h_forest.csv")

## now filter out of the trad data the missing in birdnet ones
all_surveys_forest_birds_bn_only <- all_surveys_forest_birds %>%
  filter(!scientific_name %in% missing_from_bn_labels)

# Verify
cat("Before:", n_distinct(all_surveys_forest_birds$scientific_name), "species\n")
cat("After:", n_distinct(all_surveys_forest_birds_bn_only$scientific_name), "species\n")


# conclusion 29 species removed

all_surveys_forest_birds <- all_surveys_forest_birds_bn_only

all_surveys_forest_birds_bn_only|> distinct(scientific_name)|> nrow ()

# list fo validated subplots for the 20H analysis
subplots_long_ok <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/subplots_long_ok.csv") |>
  pull(subplot_ID)

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

n_subplots_trad <- nrow(validated_subplots)
n_subplots_lfl  <- length(unique(
  bn_like_for_like_forest$subplot_ID[
    bn_like_for_like_forest$subplot_ID %in% validated_subplots$subplot_ID
  ]
))





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
          "D:/QBIO7008/Bird_accoustic/Outputs/bn_lfl_richness_all_subplots_alt.csv",
          row.names = FALSE)

####BN 20h richness (all validated subplots in 20h window) ----


bn_20h_richness <- map_dfr(thresholds, function(thr) {
  bn_20h_forest |>
    filter(confidence >= thr,
           subplot_ID %in% subplots_long_ok) |>
    group_by(subplot_ID) |>
    summarise(richness = n_distinct(scientific_name), .groups = "drop") |>
    mutate(threshold = thr) |>
    right_join(
      validated_subplots |>
        filter(subplot_ID %in% subplots_long_ok) |>
        select(subplot_ID),
      by = "subplot_ID") |>
    mutate(richness  = replace_na(richness, 0),
           threshold = replace_na(threshold, thr))
}) |>
  pivot_wider(names_from = threshold, values_from = richness,
              names_prefix = "richness_bn_20h_") |>
  mutate(plot_ID = str_extract(subplot_ID, "^C\\d+"))

# Check
nrow(bn_20h_richness)
unique(bn_20h_richness$subplot_ID)

# Check
nrow(bn_20h_richness)

#checking at each confidence level
map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_20h_", thr)
  bn_20h_richness |>
    summarise(n_subplots  = n(),
              n_with_data = sum(.data[[col]] > 0),
              n_zero      = sum(.data[[col]] == 0)) |>
    mutate(threshold = thr)
}) |> data.frame()

write.csv(bn_20h_richness,
          "D:/QBIO7008/Bird_accoustic/Outputs/bn_20h_richness_all_subplots_alt.csv",
          row.names = FALSE)

####point count survey richness (pooled M+E, all validated subplots)

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
          "D:/QBIO7008/Bird_accoustic/Outputs/trad_richness_all_subplots_alt.csv",
          row.names = FALSE)

####Analytical datasets 

# Like-for-like (BN 20min vs Trad 20min)
richness_lfl <- trad_richness |>
  left_join(bn_lfl_richness, by = c("subplot_ID", "plot_ID"))

nrow(richness_lfl)
write.csv(richness_lfl,
          "D:/QBIO7008/Bird_accoustic/Outputs/richness_lfl_dataset_all_subplots_alt.csv",
          row.names = FALSE)

# 20h (BN 20h vs Trad 20min)
richness_20h <- trad_richness |>
  filter(subplot_ID %in% subplots_long_ok) |>
  left_join(bn_20h_richness, by = c("subplot_ID", "plot_ID"))

# Check
nrow(richness_20h)
unique(richness_20h$subplot_ID)

nrow(richness_20h)
write.csv(richness_20h,
          "D:/QBIO7008/Bird_accoustic/Outputs/richness_20h_dataset_all_subplots_alt.csv",
          row.names = FALSE)

###############################################################################
# OBJECTIVE 4 - STATISTICAL ANALYSIS
###############################################################################

# Load plot level shade cover data
plot_shade <- read.csv("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/plot_shade_cover.csv")

# Join shade cover
richness_lfl <- richness_lfl |>
  left_join(plot_shade, by = c("plot_ID" = "plot_id"))

richness_20h <- richness_20h |>
  left_join(plot_shade, by = c("plot_ID" = "plot_id"))

# Check for NAs
cat("NAs in lfl shade:", sum(is.na(richness_lfl$plot_shade)), "\n")
cat("NAs in 20h shade:", sum(is.na(richness_20h$plot_shade)), "\n")

#Distribution check

hist(richness_lfl$plot_shade,
     main = "Shade cover distribution", xlab = "Plot shade cover")


### SENSITIVITY ANALYSIS - slope concordance with point count survey

#Reference model: point count survey only 

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

cat("point count survey shade slope:", round(trad_slope_lfl, 4), "\n")
cat("point count survey slope SE:   ", round(trad_slope_lfl_se, 4), "\n")
cat("point count survey p-value:    ", round(trad_p_lfl, 3), "\n")

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

# Extract slopes and compare to point count survey

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
          "D:/QBIO7008/Bird_accoustic/Outputs/sensitivity_lfl_alt.csv",
          row.names = FALSE)

# 20h reference model

model_trad_20h <- glmmTMB(richness_trad ~ plot_shade + (1|plot_ID),
                          data   = richness_20h |>
                            filter(!is.na(richness_bn_20h_0.5)),
                          family = poisson)

summary(model_trad_20h)
deviance(model_trad_20h) / df.residual(model_trad_20h)

#check
sim_res_trad_20h <- simulateResiduals(model_trad_20h)
plot(sim_res_trad_20h) # minor issue but not so relevant to fit

#slope and SE
trad_slope_20h    <- unname(fixef(model_trad_20h)$cond["plot_shade"])
trad_slope_20h_se <- unname(sqrt(diag(vcov(model_trad_20h)$cond))["plot_shade"])
trad_p_20h        <- unname(summary(model_trad_20h)$coefficients$cond["plot_shade", "Pr(>|z|)"])

cat("point count survey shade slope (20h):", round(trad_slope_20h, 4), "\n")
cat("point count survey slope SE (20h):   ", round(trad_slope_20h_se, 4), "\n")
cat("point count survey p-value (20h):    ", round(trad_p_20h, 3), "\n")

#20h sensitivity: fit one model per threshold

h20_sensitivity_models <- map(thresholds, function(thr) {
  col <- paste0("richness_bn_20h_", thr)
  df  <- richness_20h |>
    filter(!is.na(.data[[col]])) |>
    rename(richness_bn = all_of(col))
  glmmTMB(richness_bn ~ plot_shade + (1|plot_ID),
          data   = df,
          family = poisson)
})
names(h20_sensitivity_models) <- paste0("thr_", thresholds)

#DHARMa diagnostics 

walk2(h20_sensitivity_models, thresholds, function(m, thr) {
  sim <- simulateResiduals(m)
  plot(sim, main = paste("20h BirdNET - threshold", thr))
})

# Extract slopes and compare to point count survey

h20_sensitivity <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_20h_", thr)
  m   <- h20_sensitivity_models[[paste0("thr_", thr)]]
  
  disp        <- deviance(m) / df.residual(m)
  bn_slope    <- unname(fixef(m)$cond["plot_shade"])
  bn_slope_se <- unname(sqrt(diag(vcov(m)$cond))["plot_shade"])
  bn_p        <- unname(summary(m)$coefficients$cond["plot_shade", "Pr(>|z|)"])
  slope_diff  <- bn_slope - trad_slope_20h
  pooled_se   <- sqrt(bn_slope_se^2 + trad_slope_20h_se^2)
  z_stat      <- slope_diff / pooled_se
  p_diff      <- 2 * pnorm(abs(z_stat), lower.tail = FALSE)
  
  data.frame(
    threshold         = thr,
    n_with_detections = sum(!is.na(richness_20h[[col]]) & richness_20h[[col]] > 0),
    dispersion        = round(disp, 2),
    trad_slope        = round(trad_slope_20h, 4),
    trad_p_value      = round(trad_p_20h, 3),
    bn_slope          = round(bn_slope, 4),
    bn_p_value        = round(bn_p, 3),
    slope_diff        = round(slope_diff, 4),
    pooled_se         = round(pooled_se, 4),
    p_diff            = round(p_diff, 3)
  )
})

cat("=== 20H SENSITIVITY ANALYSIS ===\n")
print(h20_sensitivity)

write.csv(h20_sensitivity,
          "D:/QBIO7008/Bird_accoustic/Outputs/sensitivity_20h_alt.csv",
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
    trad_slope        = "point count slope",
    trad_p_value      = "point count p",
    bn_slope          = "BirdNET slope",
    bn_p_value        = "BirdNET p",
    slope_diff        = "Slope difference",
    pooled_se         = "Pooled SE",
    p_diff            = "p (difference)"
  ) |>
  tab_footnote(
    footnote = "Slopes are on the log scale from Poisson GLMM. point count survey slope is the reference model fitted to point count survey richness only. BirdNET slope is fitted separately per threshold. p (difference) is the two-sided z-test p-value for the difference between BirdNET and point count survey slopes."
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

# 20h sensitivity table 

gt_sensitivity_20h <- h20_sensitivity |>
  gt() |>
  tab_header(
    title = "Appendix 1b. Sensitivity analysis — Extended window (20-hour recording)"
  ) |>
  cols_label(
    threshold         = "Confidence threshold",
    n_with_detections = "Subplots with detections",
    dispersion        = "Dispersion",
    trad_slope        = "point count slope",
    trad_p_value      = "point count p",
    bn_slope          = "BirdNET slope",
    bn_p_value        = "BirdNET p",
    slope_diff        = "Slope difference",
    pooled_se         = "Pooled SE",
    p_diff            = "p (difference)"
  ) |>
  tab_footnote(
    footnote = "Slopes are on the log scale from Poisson GLMM. point count survey slope is the reference model fitted to point count survey richness only. BirdNET slope is fitted separately per threshold. p (difference) is the two-sided z-test p-value for the difference between BirdNET and point count survey slopes."
  ) |>
  tab_style(
    style = cell_fill(color = "grey90"),
    locations = cells_body(rows = threshold == 0.5)
  ) |>
  tab_options(
    table.font.size = 11,
    heading.align   = "left"
  ) 

gt_sensitivity_20h

# Save both to Word 

gt_sensitivity_lfl |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/appendix1a_sensitivity_lfl_alt.docx")

gt_sensitivity_20h |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/appendix1b_sensitivity_20h_alt.docx")

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
                           labels = c("point count", "BirdNET"))),
  family = poisson)

summary(model_lfl)
deviance(model_lfl) / df.residual(model_lfl)

sim_res_lfl <- simulateResiduals(model_lfl)
plot(sim_res_lfl)

#20h main model


model_20h <- glmmTMB(
  richness ~ plot_shade * method + (1|plot_ID),
  data = richness_20h |>
    filter(!is.na(richness_bn_20h_0.5)) |>
    select(subplot_ID, plot_ID, plot_shade,
           richness_trad,
           richness_bn = richness_bn_20h_0.5) |>
    pivot_longer(cols      = c(richness_trad, richness_bn),
                 names_to  = "method",
                 values_to = "richness") |>
    mutate(method = factor(method,
                           levels = c("richness_trad", "richness_bn"),
                           labels = c("point count", "BirdNET"))),
  family = poisson)

summary(model_20h)
deviance(model_20h) / df.residual(model_20h)

sim_res_20h <- simulateResiduals(model_20h)
plot(sim_res_20h)


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
                         labels = c("point count survey", "BirdNET (0.5 confidence level)")))

# Like-for-like: predicted values with 95% CI
lfl_pred <- expand.grid(
  plot_shade = shade_seq,
  method     = factor(c("point count survey", "BirdNET (0.5 confidence level)"))
) |>
  mutate(method_internal = ifelse(method == "point count survey", "point count", "BirdNET"))

# Predictions on the link (log) scale with SE
# re.form = NA gives population-level predictions excluding random effects
lfl_fit <- predict(model_lfl,
                   newdata = data.frame(
                     plot_shade = lfl_pred$plot_shade,
                     method     = factor(lfl_pred$method_internal,
                                         levels = c("point count", "BirdNET"))
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

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_richness_lfl_alt.png",
       fig_lfl, width = 6, height = 5, dpi = 300)

####20h: raw data

h20_raw <- richness_20h |>
  filter(!is.na(richness_bn_20h_0.5)) |>
  select(subplot_ID, plot_ID, plot_shade, richness_trad,
         richness_bn = richness_bn_20h_0.5) |>
  pivot_longer(cols = c(richness_trad, richness_bn),
               names_to = "method", values_to = "richness") |>
  mutate(method = factor(method,
                         levels = c("richness_trad", "richness_bn"),
                         labels = c("point count survey", "BirdNET (0.5 confidence level)")))

#predicted values with 95% CI 

shade_seq_20h <- seq(min(richness_20h$plot_shade, na.rm = TRUE),
                     max(richness_20h$plot_shade, na.rm = TRUE),
                     length.out = 100)

h20_pred <- expand.grid(
  plot_shade = shade_seq_20h,
  method     = factor(c("point count survey", "BirdNET (0.5 confidence level)"))
) |>
  mutate(method_internal = ifelse(method == "point count survey", "point count", "BirdNET"))

# Predictions on the link (log) scale with SE
# re.form = NA gives population-level predictions excluding random effects
h20_fit <- predict(model_20h,
                   newdata = data.frame(
                     plot_shade = h20_pred$plot_shade,
                     method     = factor(h20_pred$method_internal,
                                         levels = c("point count", "BirdNET"))
                   ),
                   type    = "link",
                   se.fit  = TRUE,
                   re.form = NA)

# Back-transform to response scale (species counts) using exp()
# CI computed on log scale first, then back-transformed to keep intervals
# asymmetric and bounded above zero - this is correct for Poisson GLMM
h20_pred <- h20_pred |>
  mutate(
    fit    = exp(h20_fit$fit),                            # predicted richness
    ci_low = exp(h20_fit$fit - 1.96 * h20_fit$se.fit),   # lower 95% CI
    ci_up  = exp(h20_fit$fit + 1.96 * h20_fit$se.fit)    # upper 95% CI
  )


#plot


fig_20h <- ggplot() +
  geom_ribbon(data = h20_pred,
              aes(x = plot_shade, ymin = ci_low, ymax = ci_up, group = method),
              fill = "grey80", alpha = 0.5, show.legend = FALSE) +
  geom_point(data = h20_raw,
             aes(x = plot_shade, y = richness, colour = method),
             alpha = 0.5, size = 2) +
  geom_line(data = h20_pred,
            aes(x = plot_shade, y = fit, colour = method),
            linewidth = 1) +
  scale_colour_brewer(palette = "Dark2") +
  guides(colour = guide_legend(title = "Method")) +
  labs(
    x      = "Plot shade cover",
    y      = "Forest bird species richness",
    title  = "Extended window (20-hour recording)"
  ) +
  theme_classic() +
  theme(legend.position = "bottom")

fig_20h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_richness_20h_alt.png",
       fig_20h, width = 6, height = 5, dpi = 300)



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

# 20h model (glmmTMB)
h20_coefs <- summary(model_20h)$coefficients$cond |>
  as.data.frame() |>
  tibble::rownames_to_column("term") |>
  rename(estimate_20h = Estimate,
         se_20h       = `Std. Error`,
         z_20h        = `z value`,
         p_20h        = `Pr(>|z|)`)


# Join and relabel
model_table <- lfl_coefs |>
  left_join(h20_coefs, by = "term") |>
  mutate(term = recode(term,
                       "(Intercept)"              = "Intercept (point count survey, shade = 0)",
                       "plot_shade"               = "Shade tree cover (plot level)",
                       "methodBirdNET"            = "Difference in intercept (BirdNET vs point count survey)",
                       "plot_shade:methodBirdNET" = "Difference in shade slope (BirdNET vs point count survey)"
  )) |>
  mutate(across(c(estimate_lfl, se_lfl, z_lfl,
                  estimate_20h, se_20h, z_20h), ~ round(.x, 3)),
         p_lfl = round(p_lfl, 4),
         p_20h = round(p_20h, 4))


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
    label   = "Extended window (20-hour recording)",
    columns = c(estimate_20h, se_20h, z_20h, p_20h)
  ) |>
  cols_label(
    term         = "Term",
    estimate_lfl = "Estimate",
    se_lfl       = "SE",
    z_lfl        = "z",
    p_lfl        = "p",
    estimate_20h = "Estimate",
    se_20h       = "SE",
    z_20h        = "z",
    p_20h        = "p"
  ) |>
  tab_footnote(
    footnote = "Reference level is point count point count survey. BirdNET detections filtered at 0.5 confidence threshold. Estimates are on the log scale."
  ) |>
  tab_options(
    table.font.size  = 12,
    heading.align    = "left"
  )

gt_table

# Export to Word
gt_table |>
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/richness_model_table_alt.docx")

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

# point count survey detections (filtered to subplots in richness_lfl)
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
unique(richness_20h$plot_ID) #21

# number of subplots included with M and E surveys pooled 
unique(richness_20h$subplot_ID) #37


# 20h BirdNET detections at 0.5 (filtered to subplots in richness_20h with data)
bn_20h_forest |>
  filter(subplot_ID %in% richness_20h$subplot_ID,
         confidence >= 0.5) |>
  nrow()

# point count survey detections (filtered to subplots in richness_lfl)
all_surveys_forest_birds |>
  filter(ID %in% richness_20h$subplot_ID) |>
  nrow() #1275

#20h BirdNET range and mean - this dataset includes 30_O that is NA for the 20H
max(richness_20h$richness_bn_20h_0.5, na.rm = TRUE)
min(richness_20h$richness_bn_20h_0.5, na.rm = TRUE)
mean(richness_20h$richness_bn_20h_0.5, na.rm = TRUE)
sd(richness_20h$richness_bn_20h_0.5, na.rm = TRUE)

#shade range and mean
max(richness_20h$plot_shade)
min(richness_20h$plot_shade)
mean(richness_20h$plot_shade)
sd(richness_20h$plot_shade)

# min max and average richness trad surveys
max(richness_20h$richness_trad)
min(richness_20h$richness_trad)
mean(richness_20h$richness_trad)
sd(richness_20h$richness_trad)

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

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_hist_lfl_alt.png",
       fig_hist_lfl, width = 6, height = 4, dpi = 300)

# 20h 

fig_hist_20h <- bn_20h_forest |>
  filter(subplot_ID %in% richness_20h$subplot_ID) |>
  ggplot(aes(x = confidence)) +
  geom_histogram(binwidth = 0.05, fill = "grey60", colour = "white") +
  labs(
    x     = "BirdNET confidence level",
    y     = "Number of detections",
    title = "Extended window (20-hour recording | Forest species)"
  ) +
  theme_classic()

fig_hist_20h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_hist_20h_alt.png",
       fig_hist_20h, width = 6, height = 4, dpi = 300)

#### raw data panels
# Build long format with all thresholds plus point count survey
lfl_scatter <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_lfl_", thr)
  richness_lfl |>
    select(subplot_ID, plot_ID, plot_shade, richness = all_of(col)) |>
    mutate(panel = paste0("BirdNET ", thr))
}) |>
  bind_rows(
    richness_lfl |>
      select(subplot_ID, plot_ID, plot_shade, richness = richness_trad) |>
      mutate(panel = "point count survey")
  ) |>
  mutate(panel = factor(panel,
                        levels = c("point count survey",
                                   paste0("BirdNET ", thresholds))))

# Fixed axis limits
lfl_scatter_max <- max(lfl_scatter$richness, na.rm = TRUE)

# Colour palette - 10 panels (1 point count + 9 thresholds)
panel_colours <- setNames(
  c("darkorange", viridis::viridis(9)),
  c("point count survey", paste0("BirdNET ", thresholds))
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

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_scatter_lfl_alt.png",
       fig_scatter_lfl, width = 12, height = 6, dpi = 300)

# Scatterplot richness vs shade — 20h 

h20_scatter <- map_dfr(thresholds, function(thr) {
  col <- paste0("richness_bn_20h_", thr)
  richness_20h |>
    filter(!is.na(.data[[col]])) |>
    select(subplot_ID, plot_ID, plot_shade, richness = all_of(col)) |>
    mutate(panel = paste0("BirdNET ", thr))
}) |>
  bind_rows(
    richness_20h |>
      filter(!is.na(richness_bn_20h_0.5)) |>
      select(subplot_ID, plot_ID, plot_shade, richness = richness_trad) |>
      mutate(panel = "point count survey")
  ) |>
  mutate(panel = factor(panel,
                        levels = c("point count survey",
                                   paste0("BirdNET ", thresholds))))

# Fixed axis limits
h20_scatter_max <- max(h20_scatter$richness, na.rm = TRUE)

# Colour palette - 10 panels (1 point count + 9 thresholds)
panel_colours <- setNames(
  c("darkorange", viridis::viridis(9)),
  c("point count survey", paste0("BirdNET ", thresholds))
)

fig_scatter_20h <- ggplot(h20_scatter,
                          aes(x = plot_shade, y = richness, colour = panel)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~ panel, ncol = 5) +
  scale_colour_manual(values = panel_colours) +
  scale_y_continuous(limits = c(0, h20_scatter_max)) +
  labs(
    x     = "Plot shade cover",
    y     = "Forest bird species richness",
    title = "Extended window (20-hour recording | Forest species)"
  ) +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text       = element_text(size = 9),
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey95"),
    legend.position  = "none"
  )

fig_scatter_20h

ggsave("D:/QBIO7008/Bird_accoustic/Plots/appendix_scatter_20h_alt.png",
       fig_scatter_20h, width = 12, height = 6, dpi = 300)


###############################################################################
####### NOW FOR COMPOSITION###################################################
##############################################################################

####load data


birdlife <- fread("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/GHA-Species_BirdlifeInternational.csv")

shade <- read.csv("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/subplot_shade_cover.csv")

plot_shade <- read.csv("C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/Data_R_Barbara/plot_shade_cover.csv")
head(plot_shade)

#filter bn data to 0.5 confidence threshold informed by Richness

bn_lfl_thresh  <- bn_like_for_like_forest |> filter(confidence >= 0.5)
bn_20h_thresh  <- bn_20h_forest           |> filter(confidence >= 0.5)

#### make sure data only includes subplots that have M and E surveys
#### make sure subplots that dropped with the forest filter are include with zero

# load data before forest filter
# bn_like_for_like <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/bn_like_for_like.csv",
#                              stringsAsFactors = FALSE)
# all_surveys_checked <- read.csv("D:/QBIO7008/Bird_accoustic/Outputs/all_traditional_surveys.csv",
#                                 stringsAsFactors = FALSE)
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
dim(spp_lfl)       # should be 78 x n species
dim(meta_lfl)      # should be 78 x 4
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
dim(spp_20h)  #74 85
dim(meta_20h) #84 x 4
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
perm_bd_lfl

#checking which method is more dispersed
bd_lfl$group.distances

# dispersion birdnet vs point count(0.62 vs 0.47, p = 0.001)

#now for 20h survey period
bd_20h <- betadisper(dist_20h, meta_20h$method)
set.seed(123)
perm_bd_20h <- permutest(bd_20h, permutations = 999)
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

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_nmds_lfl_alt.png",
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

ggsave("D:/QBIO7008/Bird_accoustic/Plots/fig_nmds_20h_alt.png",
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
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/permanova_table_lfl_alt.docx")


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
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/permanova_table_20h_alt.docx")

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
  gtsave("D:/QBIO7008/Bird_accoustic/Outputs/betadisper_tabble_alt.docx")



#check
bd_lfl$group.distances
bd_20h$group.distances
set.seed(123)
permutest(bd_lfl, permutations = 999)$tab
set.seed(123)
permutest(bd_20h, permutations = 999)$tab

#checks
#number of subplots included in the analysis lkl
dim(meta_lfl)      # should be 78 x 4 #78/2 as for each subpoint two sets of data
unique(meta_lfl$subplot_ID)


#number of subplots included in the analysis 20H
dim(meta_20h)
