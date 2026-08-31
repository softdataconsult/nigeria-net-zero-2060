# =============================================================
# DHS Cluster -> State Electricity Access Aggregation
# Input: your DHS extract, now including BOTH hv001 (cluster) and
#        hv024 (state/region) - re-extract from the same source file
#        used for your MPI project if you only have hv001 currently.
# Output: electricity_access_by_state.csv, ready for 05_electricity_access.R
# =============================================================

library(dplyr)
library(readr)
library(haven)   # only needed if reading the raw .dta/.sav directly

# =============================================================
# OPTION A — If you still have the original DHS .dta/.sav file
# =============================================================
# Re-extract directly, keeping hv001, hv024, and the electricity variable
# (hv206 in standard DHS coding: 1 = has electricity, 0 = does not - a
# 0/1 flag per household, which is what your cluster-level 0-1 proportions
# were almost certainly averaged FROM).
#
# dhs_raw <- read_dta("path/to/your/NGHR7X.dta")  # or read_sav() for .sav
# hv024_labels <- as_factor(dhs_raw$hv024)          # decode numeric codes to state names
#
# cluster_state_lookup <- dhs_raw %>%
#   distinct(hv001, hv024) %>%
#   mutate(state = as.character(as_factor(hv024)))

# =============================================================
# OPTION B — If you only have your existing hv001 + electricity_access
# CSV (as pasted) and a SEPARATE hv001-to-hv024 lookup you export fresh
# =============================================================

# Your existing cluster-level file (hv001, electricity_access proportion)
cluster_data <- read_csv("data/raw/dhs_cluster_electricity.csv",
                          show_col_types = FALSE)  # your pasted hv001/electricity_access data

# A fresh, minimal export from the same DHS file: just hv001 and hv024
cluster_state_lookup <- read_csv("data/raw/dhs_cluster_state_lookup.csv",
                                  show_col_types = FALSE)  # hv001, hv024 (or state name directly)

# ---- Merge cluster data with state lookup ----
merged <- cluster_data %>%
  left_join(cluster_state_lookup, by = "hv001")

n_unmatched <- sum(is.na(merged$hv024) | is.na(merged$state))
if (n_unmatched > 0) {
  warning(n_unmatched, " clusters in your electricity data have no matching ",
          "state in the lookup - check hv001 values line up exactly between ",
          "the two files (same DHS survey round).")
}

# ---- If hv024 is numeric, decode to state names ----
# DHS numeric codes for hv024 vary by survey round - check your DHS
# recode manual (or the value labels in the original .dta/.sav) for the
# exact code-to-state mapping. Example structure if you need to build it
# by hand from the codebook:
#
# hv024_codes <- c(
#   "1" = "Sokoto", "2" = "Zamfara", "3" = "Katsina",  # etc - fill in
#   # from your specific NDHS round's codebook
# )
# merged <- merged %>% mutate(state = recode(as.character(hv024), !!!hv024_codes))

# ---- Aggregate cluster-level access to state-level mean ----
# Note: a simple mean across clusters is NOT population-weighted. DHS
# clusters don't represent equal population sizes, so for a publication-
# grade figure you should ideally weight by hv005 (household sample
# weight) if available. Simple mean shown here as a starting point -
# flag this as a limitation if you don't weight.
state_access <- merged %>%
  group_by(state) %>%
  summarise(
    elec_access_pct = round(mean(electricity_access, na.rm = TRUE) * 100, 1),
    n_clusters       = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(elec_access_pct))

print(state_access)

# ---- Save in the exact format 05_electricity_access.R expects ----
state_access_final <- state_access %>%
  mutate(year = 2018) %>%   # CHANGE to your actual NDHS survey year
  select(state, year, elec_access_pct)

write_csv(state_access_final, "data/raw/electricity_access_by_state.csv")
message("Saved: data/raw/electricity_access_by_state.csv (",
        nrow(state_access_final), " states) - ready for 05_electricity_access.R")
