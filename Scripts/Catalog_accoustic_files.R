###### Catalog of audio files - writtennby claude and checked by Barbara 30/04

library(stringr)
library(dplyr)
library(data.table)
library(tidyr)

# ── Root directories ───────────────────────────────────────────────────────────
roots <- list(
  Block_1        = "C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/03_Acoustic_Recordings/01_Short_Term/Block_1",
  Block_2_silent = "C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/03_Acoustic_Recordings/Block_2_silent",
  Block_3        = "C:/Users/badel/Nextcloud/Bird_Biodiv_Agrof-Q8597/03_Acoustic_Recordings/01_Short_Term/Block_3"
)

output_csv <- "D:/QBIO7008/Bird_accoustic/Outputs/recording_catalog_all_blocks.csv"

# ── Catalog function ───────────────────────────────────────────────────────────
catalog_block <- function(root_dir, block_name) {
  
  cat("Scanning", block_name, "...\n")
  
  wav_files <- list.files(root_dir, pattern = "\\.wav$",
                          recursive = TRUE, full.names = TRUE,
                          ignore.case = TRUE)
  
  cat("  Found", length(wav_files), "WAV files\n")
  if (length(wav_files) == 0) return(NULL)
  
  data.frame(filepath = wav_files, stringsAsFactors = FALSE) |>
    mutate(
      block           = block_name,
      rel_path        = str_remove(filepath, fixed(paste0(root_dir, "/"))),
      parts           = str_split(rel_path, "/"),
      plot_raw        = sapply(parts, `[`, 1),
      subplot         = sapply(parts, `[`, 2),
      schedule_folder = sapply(parts, `[`, 3),
      filename        = basename(filepath),
      
      # Normalise plot name: remove spaces, uppercase  (e.g. "C 58" → "C58", "c 117" → "C117")
      plot            = str_to_upper(str_remove_all(plot_raw, " ")),
      
      # Parse datetime from filename
      dt_str          = str_extract(filename, "\\d{8}T\\d{6}"),
      datetime        = as.POSIXct(dt_str, format = "%Y%m%dT%H%M%S", tz = "UTC"),
      date            = as.Date(datetime),
      start_time      = format(datetime, "%H:%M:%S"),
      size_mb         = round(file.size(filepath) / 1024^2, 1)
    ) |>
    filter(!is.na(datetime)) |>
    select(block, plot, subplot, schedule_folder, filename,
           date, start_time, datetime, size_mb, filepath)
}

# ── Run across all blocks ──────────────────────────────────────────────────────
catalog <- bind_rows(
  mapply(catalog_block, roots, names(roots), SIMPLIFY = FALSE)
)

# ── Summary ────────────────────────────────────────────────────────────────────
cat("\n── Catalog summary ───────────────────────────────────\n")
catalog |>
  group_by(block) |>
  summarise(
    n_files    = n(),
    n_plots    = n_distinct(plot),
    n_subplots = n_distinct(subplot),
    date_range = paste(min(date), "to", max(date)),
    .groups = "drop"
  ) |>
  data.frame() |>
  print()

# ── Check for any PLOT F folders (forest plots) ────────────────────────────────
cat("\nPLOT F entries found:\n")
catalog |> filter(str_detect(plot, "^PLOTF|^PLOT")) |>
  distinct(plot, subplot) |> data.frame() |> print()

# ── Save ───────────────────────────────────────────────────────────────────────
fwrite(catalog, output_csv)
cat("\nSaved to:", output_csv, "\n")



# create a summary of actual recordings

# Find all log files across all blocks
log_files <- list.files(
  c("F:/Acoustic_Monitoring_Block_01/Short_Term_Block_01",
    "F:/Acoustic_Monitoring_Block_01/Short_Term_Block_02_silent",
    "H:/Acoustic Monitoring/Short Term"),
  pattern = "logfile.*\\.txt$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat("Log files found:", length(log_files), "\n")

# Show a sample of paths to confirm structure
head(log_files, 10)




# ── Step 1: deduplicate catalog first ─────────────────────────────────────────
# Some files appear in multiple subplot folders (same filename, different subplot)
# These are genuinely different files - keep all but flag the join key as subplot+filename

catalog_clean <- catalog |>
  filter(str_detect(filename, "\\.wav$")) |>
  # Normalise subplot names to uppercase, remove spaces
  mutate(subplot = str_to_upper(str_remove_all(subplot, " ")))

cat("Catalog rows:", nrow(catalog_clean), "\n")
cat("Unique subplot+filename combinations:", 
    n_distinct(paste(catalog_clean$subplot, catalog_clean$filename)), "\n")

# ── Step 2: calculate duration from gap to next file within each subplot ───────
catalog_with_duration <- catalog_clean |>
  arrange(subplot, datetime) |>
  group_by(subplot) |>
  mutate(
    next_start   = lead(datetime),
    # Duration = gap to next file (exact for all files except last)
    duration_min = as.numeric(difftime(next_start, datetime, units = "mins")),
    is_last_file = is.na(next_start)
  ) |>
  ungroup()

# Check what gap sizes we have - should be 60 or 360 min for complete files
cat("\nCommon gap sizes (minutes):\n")
catalog_with_duration |>
  filter(!is_last_file) |>
  mutate(gap_rounded = round(duration_min)) |>
  count(gap_rounded) |>
  arrange(desc(n)) |>
  head(10) |>
  data.frame() |>
  print()

# ── Step 3: extract stop times from logs for last files only ───────────────────
extract_last_stop <- function(log_path) {
  
  lines <- readLines(log_path, warn = FALSE)
  
  path_parts <- str_split(log_path, "/")[[1]]
  subplot    <- str_to_upper(str_remove_all(path_parts[length(path_parts) - 1], " "))
  
  # Find all stop lines
  stop_idx <- which(str_detect(lines, "Recording stopped\\."))
  if (length(stop_idx) == 0) return(NULL)
  
  # We only need the LAST stop in the log = end of final recording
  last_stop_line <- lines[max(stop_idx)]
  
  stop_dt <- as.POSIXct(
    str_extract(last_stop_line, "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"),
    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"
  )
  
  data.frame(
    subplot = subplot,
    last_stop_dt = stop_dt
  )
}

cat("Extracting final stop times from", length(log_files), "logs...\n")

last_stops <- bind_rows(
  lapply(log_files, function(f) {
    tryCatch(extract_last_stop(f), error = function(e) NULL)
  })
) |>
  # If multiple logs per subplot, keep the latest stop
  group_by(subplot) |>
  summarise(last_stop_dt = max(last_stop_dt), .groups = "drop")

cat("Subplots with last stop time:", nrow(last_stops), "\n")

# ── Step 4: fill in duration for last files using log stop time ────────────────
catalog_with_duration <- catalog_with_duration |>
  left_join(last_stops, by = "subplot") |>
  mutate(
    duration_min = case_when(
      !is_last_file              ~ duration_min,           # gap-based, already exact
      !is.na(last_stop_dt)       ~ as.numeric(difftime(last_stop_dt, datetime, units = "mins")),
      TRUE                       ~ NA_real_                # no log found
    )
  ) |>
  select(-next_start, -last_stop_dt)

# ── Step 5: build deployment summary ──────────────────────────────────────────
deployment_summary <- catalog_with_duration |>
  group_by(block, plot, subplot) |>
  summarise(
    first_recording = min(datetime),
    last_recording  = max(datetime) + minutes(round(last(duration_min[!is.na(duration_min)]))),
    deployment_days = as.integer(as.Date(max(datetime)) - as.Date(min(datetime))) + 1,
    n_recordings    = n(),
    total_hours     = round(sum(duration_min, na.rm = TRUE) / 60, 2),
    n_missing_dur   = sum(is.na(duration_min)),
    .groups = "drop"
  ) |>
  arrange(block, plot, subplot)

# ── Step 6: check and save ─────────────────────────────────────────────────────
cat("\nDeployment summary rows:", nrow(deployment_summary), "\n")
cat("Subplots with missing durations:", sum(deployment_summary$n_missing_dur > 0), "\n")

# Spot check the previously problematic subplots
deployment_summary |>
  filter(subplot %in% c("C117_B", "C58__C", "C101_O", "C58_C", "C101_O")) |>
  data.frame() |>
  print()

# Step 1: Remove Block_2_silent (duplicate data, all zeros)
deployment_summary <- deployment_summary |>
  filter(block != "Block_2_silent")

# Step 2: Remove junk subplots
deployment_summary <- deployment_summary |>
  filter(!subplot %in% c("C26EXTRAPLOT", "EXTRALPLOT",
                         "20250913_SCHEDULE", "20250914_SCHEDULE"))

# Step 3: Apply name corrections
deployment_summary <- deployment_summary |>
  mutate(subplot = case_when(
    # C131 spaces
    subplot == "C131A" ~ "C131_A",
    subplot == "C131B" ~ "C131_B",
    subplot == "C131C" ~ "C131_C",
    subplot == "C131D" ~ "C131_D",
    subplot == "C131O" ~ "C131_O",
    # C118 Point labels
    subplot == "POINTA" ~ "C118_A",
    subplot == "POINTB" ~ "C118_B",
    subplot == "POINTC" ~ "C118_C",
    subplot == "POINTD" ~ "C118_D",
    subplot == "POINTO" ~ "C118_O",
    # C30 PLOT labels
    subplot == "PLOTA"  ~ "C30_A",
    subplot == "PLOTB"  ~ "C30_B",
    subplot == "PLOTC"  ~ "C30_C",
    subplot == "PLOTD"  ~ "C30_D",
    subplot == "PLOTO"  ~ "C30_O",
    # C16 dash
    subplot == "C16_-C" ~ "C16_C",
    # C10 zero vs O
    subplot == "C10_0"  ~ "C10_O",
    # C58 double underscore
    subplot == "C58__C" ~ "C58_C",
    TRUE ~ subplot
  ))

# Check final state
cat("Rows remaining:", nrow(deployment_summary), "\n")
cat("Remaining name issues:\n")
deployment_summary |>
  filter(str_detect(subplot, " |[a-z]|-C$|__")) |>
  select(block, plot, subplot) |>
  data.frame() |>
  print()

deployment_summary <- deployment_summary |>
  mutate(subplot = case_when(
    subplot == "FO7__B" ~ "FO7_B",
    subplot == "F08_D"  ~ "FO8_D",
    TRUE ~ subplot
  ))

# Flag remaining issues for your records
deployment_summary |>
  filter(
    total_hours == 0 |
      n_missing_dur > 0 |
      (block == "Block_3" & plot == "C19" & subplot == "C19_D")
  ) |>
  select(block, plot, subplot, first_recording, deployment_days, 
         n_recordings, total_hours, n_missing_dur) |>
  data.frame() |>
  print()

# Save final version
fwrite(deployment_summary,
       "D:/QBIO7008/Bird_accoustic/Outputs/deployment_summary_exact.csv")
cat("Saved.\n")

