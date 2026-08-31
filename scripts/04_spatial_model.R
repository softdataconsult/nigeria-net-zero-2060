# =============================================================
# Nigeria Onshore Flaring — Spatial Autocorrelation & Spatial Models
# RQ1: Is there significant spatial autocorrelation in emissions-proxy
#      intensity across Nigerian states?
# RQ2: Which variables predict deviation from ETP benchmarks? (lag/error
#      models set up here; add covariates once electricity/NTL data are ready)
#
# Input:  flare_onshore_state_panel.csv (from 02_geocode_flares.R)
# Output: moran_results_by_year.csv, spatial_model_summary.txt
# =============================================================

# ---- 0. Packages ----
pkgs <- c("sf", "geodata", "spdep", "spatialreg", "dplyr", "readr", "tidyr")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load state boundaries (full 36 states + FCT) ----
nga_states <- gadm(country = "NGA", level = 1, path = "data/boundaries")
nga_states_sf <- st_as_sf(nga_states) %>%
  select(state = NAME_1) %>%
  arrange(state)

message("Total states/FCT in boundary file: ", nrow(nga_states_sf))

# ---- 2. Load the onshore flaring panel and build a FULL balanced panel ----
# Moran's I requires every unit in the adjacency structure to have a value -
# states with zero flaring are still valid, informative neighbors (their
# zero pulls down a neighbor's spatial lag), so they must be included, not
# dropped as in the flaring-only spatial panel from script 02.
flare_onshore <- read_csv("data/processed/flare_onshore_state_panel.csv",
                           show_col_types = FALSE)

years <- sort(unique(flare_onshore$Year))

full_grid <- expand_grid(state = nga_states_sf$state, Year = years)

panel_full <- full_grid %>%
  left_join(flare_onshore %>% select(state, Year, flare_vol_mcm),
            by = c("state", "Year")) %>%
  mutate(flare_vol_mcm = replace_na(flare_vol_mcm, 0))

message("Full balanced panel: ", nrow(panel_full), " rows (",
        length(unique(panel_full$state)), " states x ", length(years), " years)")

# Sanity check: state names must match exactly between your flaring data and
# the GADM boundary file, or the join above silently produces extra rows /
# NAs. Verify no state name mismatches:
mismatch <- setdiff(unique(flare_onshore$state), nga_states_sf$state)
if (length(mismatch) > 0) {
  warning("These flaring-data state names did not match the boundary file: ",
          paste(mismatch, collapse = ", "),
          " - check spelling (e.g. 'Akwa Ibom' vs 'AkwaIbom') before proceeding.")
}

# ---- 3. Build spatial weights (queen contiguity) ----
nb <- poly2nb(nga_states_sf, queen = TRUE)
# Check for islands (states with no neighbors under queen contiguity) -
# these must be handled (e.g. assigned a nearest-neighbor link) before
# Moran's I / spatial models will run, since a zero-neighbor unit breaks
# the row-standardized weights matrix.
n_no_neighbors <- sum(card(nb) == 0)
if (n_no_neighbors > 0) {
  warning(n_no_neighbors, " state(s) have zero queen-contiguity neighbors ",
          "(likely offshore-bordering or island-like polygons in the GADM ",
          "file). Consider knn2nb(knearneigh(coords, k=1)) as a fallback ",
          "for those units before finalizing.")
}

lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# ---- 4. Global Moran's I, year by year ----
moran_results <- lapply(years, function(yr) {
  vec <- panel_full %>% filter(Year == yr) %>% arrange(match(state, nga_states_sf$state)) %>% pull(flare_vol_mcm)
  mt <- tryCatch(
    moran.test(vec, lw, zero.policy = TRUE),
    error = function(e) NULL
  )
  if (is.null(mt)) return(tibble(Year = yr, moran_I = NA, p_value = NA))
  tibble(Year = yr, moran_I = unname(mt$estimate["Moran I statistic"]),
         p_value = mt$p.value)
}) %>% bind_rows()

print(moran_results)
write_csv(moran_results, "data/processed/moran_results_by_year.csv")
message("Saved: data/processed/moran_results_by_year.csv")

# Interpretation guide (for your results section):
# - Moran's I > 0 and significant (p < 0.05): flaring intensity clusters
#   spatially - high-flaring states tend to neighbor other high-flaring
#   states (expected, given Niger Delta geology).
# - Moran's I near 0 / non-significant: no spatial dependence detectable
#   that year - plausible in early or late years if flaring became more
#   dispersed or concentrated in isolated fields.

# ---- 5. Cross-sectional spatial models on the 2012-2024 AVERAGE ----
# A single, well-powered cross-section (mean flaring intensity per state
# across the full period) is the standard starting point before attempting
# a full spatial panel model (which needs the splm package and a longer,
# stationary panel to be reliable with only 13 time points).
panel_avg <- panel_full %>%
  group_by(state) %>%
  summarise(flare_vol_mcm_avg = mean(flare_vol_mcm), .groups = "drop") %>%
  arrange(match(state, nga_states_sf$state))

stopifnot(identical(panel_avg$state, nga_states_sf$state))  # order must match lw exactly

# Global Moran's I on the average
moran_avg <- moran.test(panel_avg$flare_vol_mcm_avg, lw, zero.policy = TRUE)
print(moran_avg)

# ---- 6. LM diagnostic tests: choose spatial lag vs spatial error model ----
# Standard practice (Anselin's decision rule): fit an OLS baseline first,
# then use Lagrange Multiplier tests to decide whether a spatial lag or
# spatial error specification (or neither) fits best.
# NOTE: once your electricity-access and nighttime-lights data are ready,
# add them as covariates on the right-hand side here (RQ2) - this
# skeleton currently regresses flaring on an intercept only, which is
# sufficient for the Moran's I / clustering question (RQ1) but not yet
# for the predictor question (RQ2).
ols_baseline <- lm(flare_vol_mcm_avg ~ 1, data = panel_avg)

lm_tests <- lm.LMtests(ols_baseline, lw, test = "all", zero.policy = TRUE)
print(lm_tests)

# ---- 7. Fit spatial lag and spatial error models ----
lag_model <- lagsarlm(flare_vol_mcm_avg ~ 1, data = panel_avg, listw = lw,
                       zero.policy = TRUE)
err_model <- errorsarlm(flare_vol_mcm_avg ~ 1, data = panel_avg, listw = lw,
                         zero.policy = TRUE, Durbin = FALSE)

summary(lag_model)
summary(err_model)

# ---- 8. Save a plain-text summary of everything for your records ----
sink("data/processed/spatial_model_summary.txt")
cat("=== Moran's I by year ===\n"); print(moran_results)
cat("\n=== Moran's I on 2012-2024 average ===\n"); print(moran_avg)
cat("\n=== LM diagnostic tests (lag vs error) ===\n"); print(lm_tests)
cat("\n=== Spatial lag model ===\n"); print(summary(lag_model))
cat("\n=== Spatial error model ===\n"); print(summary(err_model))
sink()
message("Saved: data/processed/spatial_model_summary.txt")

# =============================================================
# RQ2 MODEL — flaring intensity predicted by electricity access
# Input: rq2_flaring_electricity_merged.csv (from 05_electricity_access.R)
# =============================================================
rq2_data <- read_csv("data/processed/rq2_flaring_electricity_merged.csv",
                      show_col_types = FALSE)
rq2_data <- rq2_data[match(nga_states_sf$state, rq2_data$state), ]
stopifnot(identical(rq2_data$state, nga_states_sf$state))  # order must match lw

ols_rq2 <- lm(flare_vol_mcm_avg ~ elec_access_pct + n_onshore_sites_avg, data = rq2_data)
print(summary(ols_rq2))

lm_tests_rq2 <- lm.LMtests(ols_rq2, lw, test = "all", zero.policy = TRUE)
print(lm_tests_rq2)

lag_model_rq2 <- lagsarlm(flare_vol_mcm_avg ~ elec_access_pct + n_onshore_sites_avg,
                           data = rq2_data, listw = lw, zero.policy = TRUE)
err_model_rq2 <- errorsarlm(flare_vol_mcm_avg ~ elec_access_pct + n_onshore_sites_avg,
                             data = rq2_data, listw = lw, zero.policy = TRUE, Durbin = FALSE)

summary(lag_model_rq2)
summary(err_model_rq2)

# Append RQ2 results to the same summary file
sink("data/processed/spatial_model_summary.txt", append = TRUE)
cat("\n\n=== RQ2: flaring ~ electricity access ===\n")
cat("\n--- OLS baseline ---\n"); print(summary(ols_rq2))
cat("\n--- LM diagnostic tests ---\n"); print(lm_tests_rq2)
cat("\n--- Spatial lag model ---\n"); print(summary(lag_model_rq2))
cat("\n--- Spatial error model ---\n"); print(summary(err_model_rq2))
sink()
message("Appended RQ2 results to data/processed/spatial_model_summary.txt")

# =============================================================
# RQ2 covariates: electricity access (household welfare proxy) and
# onshore flare-site density (oil/gas infrastructure proxy). Nighttime-
# lights was considered as a third covariate but dropped - see Section
# 3.4 of the proposal for the rationale (data-access constraints, and
# partial redundancy with the flaring variable itself, since flare sites
# are visible in VIIRS imagery).
# =============================================================
