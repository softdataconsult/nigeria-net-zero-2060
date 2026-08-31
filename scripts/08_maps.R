# =============================================================
# Nigeria Onshore Flaring — Maps for the Paper
# Figure 1: Choropleth of average onshore flaring intensity by state
# Figure 2: LISA cluster map (local Moran's I) - which states drive the
#           spatial clustering found in RQ1
#
# Inputs:  flare_onshore_state_panel.csv (from 02_geocode_flares.R)
# Outputs: figure1_flaring_choropleth.png, figure2_lisa_cluster_map.png
# =============================================================

# ---- 0. Packages ----
pkgs <- c("sf", "geodata", "spdep", "dplyr", "readr", "ggplot2", "viridis")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("figures", recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load boundaries and flaring data ----
nga_states <- gadm(country = "NGA", level = 1, path = "data/boundaries")
nga_states_sf <- st_as_sf(nga_states) %>%
  select(state = NAME_1) %>%
  arrange(state)

flare_onshore <- read_csv("data/processed/flare_onshore_state_panel.csv",
                           show_col_types = FALSE)

flare_avg <- flare_onshore %>%
  group_by(state) %>%
  summarise(flare_vol_mcm_avg = mean(flare_vol_mcm), .groups = "drop")

# Full balanced panel (all 37 states, zero-filled) - same approach as
# 04_spatial_model.R, needed for both maps since Moran's I / LISA require
# the complete adjacency structure, not just the flaring states.
map_data <- nga_states_sf %>%
  left_join(flare_avg, by = "state") %>%
  mutate(flare_vol_mcm_avg = tidyr::replace_na(flare_vol_mcm_avg, 0))

# State-name mismatch check (same caution as 04_spatial_model.R) - if any
# flaring-data state name doesn't match the boundary file exactly, it
# silently drops out of the map with a wrong (zero) value instead of
# erroring, so check explicitly.
mismatch <- setdiff(unique(flare_avg$state), nga_states_sf$state)
if (length(mismatch) > 0) {
  warning("These flaring-data state names did not match the boundary ",
          "file and will show as zero on the map: ", paste(mismatch, collapse=", "))
}

# =============================================================
# FIGURE 1 — Choropleth of average onshore flaring intensity
# =============================================================
# sqrt transform on the fill scale, not the data itself, so the legend
# still shows real million-m3 values - flaring is extremely right-skewed
# (Delta ~1548 vs most states at 0), and a linear scale would make
# everything except Delta/Rivers look identical.
fig1 <- ggplot(map_data) +
  geom_sf(aes(fill = flare_vol_mcm_avg), color = "white", linewidth = 0.15) +
  scale_fill_viridis(
    option = "inferno",
    name = "Avg. flaring\n(million m3,\n2012-2024)",
    trans = "sqrt",
    labels = scales::comma
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Average Onshore Gas Flaring Intensity by State",
    subtitle = "Nigeria, 2012-2024 (offshore flaring excluded - see Methods 3.2)",
    caption = "Source: GGFR satellite-derived flare site data, geocoded to state boundaries."
  )

ggsave("figures/figure1_flaring_choropleth.png", fig1,
       width = 8, height = 8, dpi = 300, bg = "white")
message("Saved: figures/figure1_flaring_choropleth.png")

# =============================================================
# FIGURE 2 — LISA cluster map (local Moran's I)
# =============================================================
# This is the real local-indicator computation (not placeholder
# categories) - classifies each state into High-High, Low-Low, High-Low,
# Low-High, or Not Significant based on its own value and its
# neighbors' values, using the same queen-contiguity weights as
# 04_spatial_model.R for consistency between the paper's statistical
# results and its figures.
nb <- poly2nb(nga_states_sf, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Reorder map_data to match the boundary file's row order exactly -
# local Moran's I is order-sensitive (positional, not name-matched).
map_data <- map_data[match(nga_states_sf$state, map_data$state), ]
stopifnot(identical(map_data$state, nga_states_sf$state))

local_moran <- localmoran(map_data$flare_vol_mcm_avg, lw, zero.policy = TRUE)

map_data <- map_data %>%
  mutate(
    local_I  = local_moran[, "Ii"],
    local_p  = local_moran[, "Pr(z != E(Ii))"],
    z_value  = scale(flare_vol_mcm_avg)[, 1],
    z_lag    = scale(lag.listw(lw, flare_vol_mcm_avg, zero.policy = TRUE))[, 1],
    cluster  = case_when(
      local_p >= 0.05           ~ "Not Significant",
      z_value >= 0 & z_lag >= 0 ~ "High-High",
      z_value < 0  & z_lag < 0  ~ "Low-Low",
      z_value >= 0 & z_lag < 0  ~ "High-Low",
      z_value < 0  & z_lag >= 0 ~ "Low-High"
    ),
    cluster = factor(cluster, levels = c("High-High", "Low-Low", "High-Low",
                                          "Low-High", "Not Significant"))
  )

message("LISA cluster counts:")
print(table(map_data$cluster))

fig2 <- ggplot(map_data) +
  geom_sf(aes(fill = cluster), color = "white", linewidth = 0.15) +
  scale_fill_manual(
    values = c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6",
               "High-Low" = "#fdae61", "Low-High" = "#abd9e9",
               "Not Significant" = "grey85"),
    name = "LISA cluster",
    drop = FALSE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  ) +
  labs(
    title = "Local Indicators of Spatial Association (LISA)",
    subtitle = "Onshore flaring intensity, 2012-2024 average",
    caption = "High-High: high-flaring states surrounded by high-flaring neighbors (the core cluster)."
  )

ggsave("figures/figure2_lisa_cluster_map.png", fig2,
       width = 8, height = 8, dpi = 300, bg = "white")
message("Saved: figures/figure2_lisa_cluster_map.png")

# =============================================================
# Save the LISA data itself (not just the map) - useful for a results
# table alongside the figure, and for reporting exact p-values per state.
# =============================================================
lisa_table <- map_data %>%
  st_drop_geometry() %>%
  select(state, flare_vol_mcm_avg, local_I, local_p, cluster) %>%
  arrange(desc(flare_vol_mcm_avg))

write_csv(lisa_table, "data/processed/lisa_results_by_state.csv")
message("Saved: data/processed/lisa_results_by_state.csv")
print(lisa_table)
