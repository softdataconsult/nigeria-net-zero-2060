# =============================================================
# Nigeria Electricity Access by State — RQ2 Covariate Prep
# Unlike flaring/VIIRS, there's no clean API or satellite product for
# this: NBS and NERC publish state-level figures in periodic reports
# (PDF/Excel), so this script assumes you've extracted the numbers into
# a simple CSV yourself, and focuses on getting that CSV analysis-ready
# and merged with your flaring panel.
# =============================================================

# ---- 0. Packages ----
pkgs <- c("readr", "dplyr", "tidyr", "stringr")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# =============================================================
# STEP 1 — Where to get the source numbers (manual, one-time per year)
# =============================================================
#
# Option A (recommended first stop): Nigeria Multiple Indicator Cluster
# Survey (MICS) and Nigeria Demographic and Health Survey (NDHS) both
# report % of households with electricity access BY STATE, and are
# designed for exactly this kind of sub-national comparison. Available at:
#   https://www.dhsprogram.com/data/dataset_admin/index.cfm (search "Nigeria")
# These are multi-year (NDHS: 2013, 2018, 2023-24; MICS: ~2016-17, 2021)
# rather than annual, so you'll have fewer time points than your flaring
# panel - a real limitation to note, but usable for a pooled/cross-
# sectional check or an interpolated series.
#
# Option B: NBS General Household Survey (GHS) panel - annual-ish,
# state-representative, includes electricity access questions.
#   https://nigerianstat.gov.ng -> Microdata -> General Household Survey
#
# Option C: NERC/Discos annual reports - these report by DISCO franchise
# area, not by state cleanly (Disco areas cross state boundaries), so
# expect to build a Disco-to-state crosswalk if you use this source.
#   https://nerc.gov.ng -> Industry Statistics
#
# Whichever you use, build a CSV with this exact shape and save it as
# data/raw/electricity_access_by_state.csv:
#
#   state,year,elec_access_pct
#   Delta,2013,54.2
#   Delta,2018,61.0
#   ...

# =============================================================
# STEP 2 — Load, clean, and validate your manually compiled CSV
# =============================================================

elec_path <- "data/raw/electricity_access_by_state.csv"

if (!file.exists(elec_path)) {
  stop("Expected file not found: ", elec_path, "\n",
       "Create it first with columns: state, year, elec_access_pct ",
       "(see STEP 1 above for sources).")
}

elec_raw <- read_csv(elec_path, show_col_types = FALSE)

# ---- Auto-detect LGA-level source data and aggregate to state level ----
# Your actual source (an NDHS-derived extract) comes as one row per LGA
# (lga_name, state_name, electricity as a 0-1 proportion), not pre-
# aggregated to state. Detect that shape and aggregate here, rather than
# requiring a manual aggregation step each time.
if (all(c("lga_name", "state_name", "electricity") %in% names(elec_raw))) {
  message("Detected LGA-level source data (", nrow(elec_raw), " LGAs) - ",
          "aggregating to state-level mean.")
  elec_raw <- elec_raw %>%
    group_by(state = state_name) %>%
    summarise(elec_access_pct = round(mean(electricity, na.rm = TRUE) * 100, 1),
              n_lgas = n(), .groups = "drop")
}

# ---- Year handling ----
# Some sources (e.g. this LGA-level extract) won't include a year column
# at all, since it's a single cross-sectional estimate. If it's missing,
# set it once here rather than requiring every source file to carry a
# redundant constant column.
SURVEY_YEAR <- 2018   # <-- confirmed: NDHS 2018 round

if (!"year" %in% names(elec_raw)) {
  message("No 'year' column found in the source file - adding a constant ",
          "year = ", SURVEY_YEAR, " for all rows (single cross-sectional survey).")
  elec_raw <- elec_raw %>% mutate(year = SURVEY_YEAR)
}

required_cols <- c("state", "year", "elec_access_pct")
missing_cols <- setdiff(required_cols, names(elec_raw))
if (length(missing_cols) > 0) {
  stop("Missing required column(s): ", paste(missing_cols, collapse = ", "),
       ". Expected columns are 'state' and 'elec_access_pct' at minimum ",
       "('year' is added automatically if absent - see SURVEY_YEAR above).")
}

# ---- Clean state names to match the flaring panel exactly ----
# This is the most common failure point when merging hand-compiled data:
# "Akwa-Ibom" vs "Akwa Ibom", trailing spaces, inconsistent capitalization.
elec_clean <- elec_raw %>%
  mutate(
    state = str_trim(state),
    state = str_replace_all(state, "-", " "),
    state = str_to_title(state),
    # Common source-specific fixes - extend this list as you find more
    state = case_when(
      state == "Fct"              ~ "FCT",
      state == "Federal Capital Territory" ~ "FCT",
      state == "Nassarawa"        ~ "Nasarawa",
      TRUE                        ~ state
    )
  )

# ---- Validate against the known 37-unit state list ----
official_states <- c("Abia","Adamawa","Akwa Ibom","Anambra","Bauchi","Bayelsa",
  "Benue","Borno","Cross River","Delta","Ebonyi","Edo","Ekiti","Enugu","FCT",
  "Gombe","Imo","Jigawa","Kaduna","Kano","Katsina","Kebbi","Kogi","Kwara",
  "Lagos","Nasarawa","Niger","Ogun","Ondo","Osun","Oyo","Plateau","Rivers",
  "Sokoto","Taraba","Yobe","Zamfara")

unmatched <- setdiff(unique(elec_clean$state), official_states)
if (length(unmatched) > 0) {
  warning("These state names in your electricity file don't match the ",
          "standard 37-state list - fix before merging: ",
          paste(unmatched, collapse = ", "))
}

missing_states <- setdiff(official_states, unique(elec_clean$state))
if (length(missing_states) > 0) {
  message("Note: no electricity data found for: ",
          paste(missing_states, collapse = ", "),
          " - these will be NA, not zero, in the merged panel (NA is ",
          "correct here since 'no data' and 'zero access' are not the same).")
}

write_csv(elec_clean, "data/processed/electricity_access_by_state_clean.csv")

# =============================================================
# STEP 3 — Merge onto the flaring panel for the spatial model (RQ2)
# =============================================================
# Electricity survey years rarely line up with your 2012-2024 annual
# flaring panel. Two honest options, not a silent workaround:
#
#   (a) Nearest-year match: for each flaring-panel year, use the closest
#       available survey year's electricity figure. Transparent but
#       assumes access didn't change much in between.
#   (b) Cross-sectional design: instead of a full panel model, average
#       flaring over the years nearest to each survey wave and run a
#       separate cross-sectional spatial model per wave (e.g., one model
#       anchored on 2018 flaring vs 2018 NDHS access, another on 2023).
#
# Option (a) implemented below; switch to (b) if you'd rather not
# interpolate.

flare_avg <- read_csv("data/processed/flare_onshore_state_panel.csv",
                       show_col_types = FALSE) %>%
  group_by(state) %>%
  summarise(
    flare_vol_mcm_avg  = mean(flare_vol_mcm),
    n_onshore_sites_avg = mean(n_onshore_sites),
    .groups = "drop"
  )

# Nearest-year electricity figure per state, using the survey year closest
# to the midpoint of your flaring panel as the default anchor - adjust
# `anchor_year` if you'd rather anchor on a specific survey wave.
anchor_year <- 2018

elec_nearest <- elec_clean %>%
  mutate(dist = abs(year - anchor_year)) %>%
  group_by(state) %>%
  slice_min(dist, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(state, elec_access_pct, elec_survey_year = year)

merged_rq2 <- tibble(state = official_states) %>%
  left_join(flare_avg, by = "state") %>%
  left_join(elec_nearest, by = "state") %>%
  mutate(
    flare_vol_mcm_avg    = replace_na(flare_vol_mcm_avg, 0),
    n_onshore_sites_avg  = replace_na(n_onshore_sites_avg, 0)
  )

write_csv(merged_rq2, "data/processed/rq2_flaring_electricity_merged.csv")
message("Saved: data/processed/rq2_flaring_electricity_merged.csv (",
        nrow(merged_rq2), " rows)")

n_na_elec <- sum(is.na(merged_rq2$elec_access_pct))
if (n_na_elec > 0) {
  message(n_na_elec, " state(s) have no electricity-access figure at all - ",
          "these will need to be dropped from the RQ2 model or the source ",
          "data extended before running lagsarlm/errorsarlm with this covariate.")
}

print(merged_rq2)

# =============================================================
# NEXT STEP
# =============================================================
# Re-run 04_spatial_model.R's RQ2 section using both covariates:
#   flare_vol_mcm_avg ~ elec_access_pct + n_onshore_sites_avg
# using `merged_rq2` in place of `panel_avg` (drop any NA rows first, or
# the lm/spatial functions will error).
