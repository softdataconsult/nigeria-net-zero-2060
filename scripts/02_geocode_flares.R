# =============================================================
# Nigeria Flare Data — Geocode Site-Level Flares to States
# Input:  2012-2024-Flare-Volume-Estimates-by-individual-Flare-Location.xlsx
# Output: flare_by_state_year.csv (panel: state x year x flaring volume)
# =============================================================

# ---- 0. Packages ----
pkgs <- c("readxl", "dplyr", "sf", "geodata", "readr", "stringr", "tidyr")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("data/boundaries", recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load the site-level flare data ----
flare_raw <- read_excel(
  "2012-2024-Flare-Volume-Estimates-by-individual-Flare-Location.xlsx"
)

nga_flares <- flare_raw %>%
  filter(Country == "Nigeria") %>%
  rename(
    lat        = Latitude,
    lon        = Longitude,
    field_type = `Field  Type`,
    field_name = `Field Name`,
    operator   = `Field  Operator`,
    location   = Location,          # ONSHORE / OFFSHORE
    flare_lvl  = `Flare Level`,
    vol_mcm    = `Flaring Vol (million m3)`
  ) %>%
  mutate(row_id = row_number())

message("Nigeria site-year records: ", nrow(nga_flares))
stopifnot(nrow(nga_flares) > 0)

# ---- 2. Get Nigeria state boundaries (ADM1) ----
# geodata::gadm() downloads GADM boundaries directly (not GitHub/LFS-based,
# so this works from a normal R session with internet access).
nga_states <- gadm(country = "NGA", level = 1, path = "data/boundaries")
nga_states_sf <- st_as_sf(nga_states)

message("States loaded: ", nrow(nga_states_sf))
print(sort(nga_states_sf$NAME_1))

# ---- 3. Convert flare sites to spatial points and join to states ----
# Onshore and offshore sites are handled separately: offshore sites fall
# outside every state polygon by definition, so a direct join leaves them
# unmatched — that's expected, not an error (see step 4).

flare_sf <- st_as_sf(nga_flares, coords = c("lon", "lat"), crs = 4326, remove = FALSE)

flare_joined <- st_join(flare_sf, nga_states_sf["NAME_1"], join = st_within)

# ---- 4. Handle offshore / unmatched sites ----
# Sites with no matching state polygon (mostly OFFSHORE) get an explicit tag
# rather than being silently dropped, so totals still reconcile to the
# national series.
flare_joined <- flare_joined %>%
  mutate(
    state = case_when(
      !is.na(NAME_1)              ~ NAME_1,
      location == "OFFSHORE"      ~ "Offshore (unassigned)",
      TRUE                        ~ "Unmatched (check coordinates)"
    )
  )

unmatched_n <- sum(flare_joined$state == "Unmatched (check coordinates)")
if (unmatched_n > 0) {
  message("WARNING: ", unmatched_n, " onshore-flagged sites did not match any state polygon.",
          " Inspect these rows before finalizing (likely near-coastline coordinate imprecision).")
}

# ---- 5. Build the full state x year panel (all categories, for auditing) ----
flare_by_state_year_all <- flare_joined %>%
  st_drop_geometry() %>%
  group_by(state, Year) %>%
  summarise(
    n_sites          = n(),
    flare_vol_mcm    = sum(vol_mcm, na.rm = TRUE),
    n_offshore_sites = sum(location == "OFFSHORE"),
    n_onshore_sites  = sum(location == "ONSHORE"),
    .groups = "drop"
  ) %>%
  arrange(state, Year)

write_csv(flare_by_state_year_all, "data/processed/flare_by_state_year_ALL_categories.csv")
message("Saved (audit copy, includes offshore/unmatched rows): ",
        "data/processed/flare_by_state_year_ALL_categories.csv (",
        nrow(flare_by_state_year_all), " rows)")

# The handful of near-coastline sites that failed to match any state polygon
# are folded into "Offshore (unassigned)" rather than kept as a separate
# category, since they are negligible in volume (see reconciliation below)
# and are almost certainly coastal/offshore in reality.

# =============================================================
# PANEL 1 — Onshore-only state panel (for the spatial model, RQ1/RQ2)
# Excludes offshore and unmatched sites entirely. Every row here is a
# real, defensible state-level observation: no allocation assumption.
# =============================================================
flare_onshore_state_panel <- flare_by_state_year_all %>%
  filter(!state %in% c("Offshore (unassigned)", "Unmatched (check coordinates)")) %>%
  arrange(state, Year)

write_csv(flare_onshore_state_panel, "data/processed/flare_onshore_state_panel.csv")
message("Saved: data/processed/flare_onshore_state_panel.csv (",
        nrow(flare_onshore_state_panel), " rows) — use this for Moran's I / spatial lag models")

# Balanced panel check: spatial econometrics generally expects every state
# to appear in every year (even as zero), not just states with recorded
# flaring. Fill the implicit zeros for the onshore-producing states so the
# panel is rectangular.
all_years  <- sort(unique(flare_by_state_year_all$Year))
all_states <- sort(unique(flare_onshore_state_panel$state))
balanced_grid <- expand_grid(state = all_states, Year = all_years)

flare_onshore_balanced <- balanced_grid %>%
  left_join(flare_onshore_state_panel, by = c("state", "Year")) %>%
  mutate(across(c(n_sites, flare_vol_mcm, n_offshore_sites, n_onshore_sites),
                ~ replace_na(.x, 0)))

write_csv(flare_onshore_balanced, "data/processed/flare_onshore_state_panel_balanced.csv")
message("Saved: data/processed/flare_onshore_state_panel_balanced.csv (",
        nrow(flare_onshore_balanced), " rows, rectangular state x year grid)")

# =============================================================
# PANEL 2 — National series, onshore + offshore combined (for ARDL, RQ3)
# This is the 100%-of-volume series: matches the national bcm totals.
# =============================================================
flare_national_panel <- flare_by_state_year_all %>%
  group_by(Year) %>%
  summarise(
    flare_vol_mcm_total    = sum(flare_vol_mcm, na.rm = TRUE),
    flare_vol_mcm_onshore  = sum(flare_vol_mcm[!state %in% c("Offshore (unassigned)", "Unmatched (check coordinates)")], na.rm = TRUE),
    flare_vol_mcm_offshore = sum(flare_vol_mcm[state == "Offshore (unassigned)"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(offshore_share_pct = round(100 * flare_vol_mcm_offshore / flare_vol_mcm_total, 1)) %>%
  arrange(Year)

write_csv(flare_national_panel, "data/processed/flare_national_panel.csv")
message("Saved: data/processed/flare_national_panel.csv (",
        nrow(flare_national_panel), " rows) — use this for the ARDL model")

# ---- 6. Reconciliation check against the national bcm series ----
# flare_vol_mcm_total above must match the national 'Flare volume' sheet
# from the second workbook (converted from bcm to million m3: bcm * 1000).
print(flare_national_panel)
message("\nCompare 'flare_vol_mcm_total' above to the national bcm series x 1000. ",
        "They should match exactly since this is the same underlying dataset, ",
        "just aggregated differently.")

# ---- 7. Quick summary: top onshore flaring states ----
top_states <- flare_onshore_state_panel %>%
  group_by(state) %>%
  summarise(total_2012_2024 = sum(flare_vol_mcm), .groups = "drop") %>%
  arrange(desc(total_2012_2024))

print(top_states)
write_csv(top_states, "data/processed/top_flaring_states_2012_2024.csv")
