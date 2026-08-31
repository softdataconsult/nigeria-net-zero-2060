# =============================================================
# Nigeria Nighttime Lights (VIIRS) — State-Level Extraction
# Output: ntl_by_state_year.csv (panel: state x year x mean radiance)
#
# Primary method: NASA Black Marble annual composites (VNP46A4) via the
# `blackmarbler` package's bm_extract() — pulls the raster AND aggregates
# to your state polygons in one step. Requires a free NASA Earthdata
# bearer token.
#
# Fallback method (Section B): manual EOG VIIRS DNB annual composite
# download + terra::extract, if Black Marble access isn't available.
# =============================================================

# ---- 0. Packages ----
pkgs <- c("blackmarbler", "sf", "dplyr", "readr", "geodata")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# =============================================================
# SECTION A — NASA Black Marble via blackmarbler (recommended)
# =============================================================
#
# 1. Get a free NASA Earthdata account: https://urs.earthdata.nasa.gov/
# 2. Generate a bearer token: profile page -> "Generate Token"
# 3. Store it as an environment variable (don't hardcode it in a script
#    you might share) - add this line to ~/.Renviron, then restart R:
#      NASA_EARTHDATA_TOKEN=your-token-here

bearer <- Sys.getenv("NASA_EARTHDATA_TOKEN")
if (bearer == "") {
  stop("Set NASA_EARTHDATA_TOKEN in your environment (.Renviron) before running. ",
       "Get a token at https://urs.earthdata.nasa.gov/ -> Generate Token")
}

# Reuse the same state boundaries from the flare geocoding step
nga_states <- gadm(country = "NGA", level = 1, path = "data/boundaries")
nga_states_sf <- st_as_sf(nga_states)

# Annual composites (VNP46A4), matching the flare data's 2012-2024 window.
# bm_extract accepts a vector of years/dates directly and returns one row
# per polygon per date - no manual year-by-year loop needed.
years <- 2012:2024

ntl_raw <- bm_extract(
  roi_sf           = nga_states_sf,
  product_id       = "VNP46A4",
  date             = years,
  bearer           = bearer,
  aggregation_fun  = c("mean"),   # zonal mean radiance per state per year
  quality_flag_rm  = c(1, 2)      # drop poor-quality / gap-filled pixels (keep flag 0 only)
)

# ntl_raw has one row per state x year. Inspect column names before
# renaming - they vary slightly by package version (e.g. 'ntl_mean' vs
# a generic aggregation column name).
print(names(ntl_raw))
print(head(ntl_raw))

ntl_by_state_year <- ntl_raw %>%
  rename(state = NAME_1) %>%   # adjust if gadm's name column differs
  select(state, date, everything()) %>%
  arrange(state, date)

write_csv(ntl_by_state_year, "data/processed/ntl_by_state_year.csv")
message("Saved: data/processed/ntl_by_state_year.csv (", nrow(ntl_by_state_year), " rows)")

# Quick sanity check: radiance should be visibly higher in Lagos and the
# Niger Delta oil/gas states than in low-density northern states. Check
# `names(ntl_by_state_year)` above for the exact mean-value column name
# and adjust below if it isn't 'ntl_mean'.
# ntl_by_state_year %>% filter(date == max(years)) %>% arrange(desc(ntl_mean)) %>% print(n = 20)

# =============================================================
# SECTION B — Fallback: manual EOG VIIRS DNB download + terra::extract
# Use this ONLY if Black Marble / Earthdata access isn't available.
# =============================================================
#
# 1. Register (free) and download annual VNL composites from:
#    https://eogdata.mines.edu/products/vnl/  (choose "annual", latest
#    version, cloud-free/median-masked composite, .tif per year)
# 2. Save each year's .tif to data/raw/ntl/, named e.g. VNL_2012.tif ... VNL_2024.tif
# 3. Uncomment and run:
#
# library(terra)
# nga_states_vect <- vect(nga_states_sf)
# ntl_manual_list <- list()
# for (yr in 2012:2024) {
#   f <- sprintf("data/raw/ntl/VNL_%d.tif", yr)
#   if (!file.exists(f)) { message("Missing: ", f); next }
#   r <- rast(f)
#   vals <- terra::extract(r, nga_states_vect, fun = mean, na.rm = TRUE, ID = FALSE)
#   ntl_manual_list[[as.character(yr)]] <- tibble(
#     state = nga_states_sf$NAME_1, Year = yr, ntl_mean = vals[[1]]
#   )
# }
# ntl_by_state_year_manual <- bind_rows(ntl_manual_list) %>% arrange(state, Year)
# write_csv(ntl_by_state_year_manual, "data/processed/ntl_by_state_year_manual.csv")

# =============================================================
# NOTE on interpreting nighttime lights for this study
# =============================================================
# VIIRS radiance picks up ALL light sources - cities, roads, gas flares
# themselves, and industrial activity - not just general economic
# activity. Since flaring sites also emit visible light, there is a
# mechanical overlap between your flaring proxy and the nighttime-lights
# proxy in the Niger Delta states. Flag this explicitly in your
# methodology/limitations section: the two proxies are not fully
# independent. A robustness check - e.g., re-running the composite index
# with flare-affected pixels masked out, or testing the flaring and NTL
# variables separately before combining - is advisable before treating
# them as independent components of a composite index.
