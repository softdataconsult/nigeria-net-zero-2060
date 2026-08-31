# =============================================================
# Nigeria Net-Zero 2060 — Data Acquisition Script
# Pulls national-level series programmatically; documents manual
# steps for datasets without a stable API (NOSDRA, VIIRS, NBS).
# =============================================================

# ---- 0. Packages ----
pkgs <- c("WDI", "readr", "dplyr", "httr", "jsonlite", "countrycode")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

# =============================================================
# 1. WORLD BANK — CO2 emissions, GDP, energy use, electricity access
#    (National level, Nigeria + South Africa + India for comparison)
# =============================================================

countries <- c("NGA", "ZAF", "IND")

wdi_indicators <- c(
  co2_total      = "EN.GHG.CO2.MT.CE.AR5",   # CO2 emissions (Mt CO2e) - AR5 series
  co2_pc         = "EN.ATM.CO2E.PC",          # CO2 emissions per capita (may be deprecated - check)
  gdp            = "NY.GDP.MKTP.KD",          # GDP (constant 2015 US$)
  gdp_pc         = "NY.GDP.PCAP.KD",          # GDP per capita
  energy_use_pc  = "EG.USE.PCAP.KG.OE",       # Energy use per capita
  elec_access    = "EG.ELC.ACCS.ZS",          # Access to electricity (% pop)
  elec_fossil    = "EG.ELC.FOSL.ZS",          # Electricity from fossil fuels (%)
  renew_pct      = "EG.FEC.RNEW.ZS"           # Renewable energy consumption (%)
)

wdi_data <- WDI(
  country   = countries,
  indicator = wdi_indicators,
  start     = 2000,
  end       = 2025,
  extra     = TRUE
)

write_csv(wdi_data, "data/raw/wdi_national_panel.csv")
message("Saved: data/raw/wdi_national_panel.csv (", nrow(wdi_data), " rows)")

# NOTE: World Bank indicator codes for CO2 change periodically (they moved
# from EN.ATM.CO2E.KT to Climate Watch/EDGAR-sourced codes). If a pull
# returns all-NA for co2_total, check the current code at:
# https://databank.worldbank.org/source/world-development-indicators

# =============================================================
# 2. OUR WORLD IN DATA — clean CO2 & GHG time series (backup/primary)
#    Direct CSV, no API key needed
# =============================================================

owid_url <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
owid_all <- read_csv(owid_url, show_col_types = FALSE)

owid_subset <- owid_all %>%
  filter(iso_code %in% countries) %>%
  select(country, iso_code, year, co2, co2_per_capita, gdp, population,
         coal_co2, oil_co2, gas_co2, flaring_co2, cumulative_co2,
         energy_per_capita, share_global_co2)

write_csv(owid_subset, "data/raw/owid_co2_nga_zaf_ind.csv")
message("Saved: data/raw/owid_co2_nga_zaf_ind.csv (", nrow(owid_subset), " rows)")
# owid-co2-data includes a 'flaring_co2' column directly - useful cross-check
# against your NOSDRA-derived flaring series.

# =============================================================
# 3. WORLD BANK GGFR — Global Gas Flaring Database (national annual)
#    No clean API; download the published Excel/CSV manually from:
#    https://datacatalog.worldbank.org/search/dataset/0037743
#    Then load it here:
# =============================================================

# ggfr <- read_csv("data/raw/ggfr_flaring_manual_download.csv")
# (uncomment once you've placed the manually downloaded file)

# =============================================================
# 4. NOSDRA GAS FLARE TRACKER — site-level, geocoded (Nigeria only)
#    Portal: https://nosdra.gasflaretracker.ng
#    This is a Leaflet/JS dashboard, not a documented public API.
#    Two practical options:
#
#    (a) Manual export: the portal usually allows a data/table export
#        per site or per year from its interface - download and save as
#        data/raw/nosdra_flare_sites.csv, then load:
#
#        flares <- read_csv("data/raw/nosdra_flare_sites.csv")
#
#    (b) If no export option is available, contact NOSDRA directly
#        (data@nosdra.gov.ng or via the portal's contact form) citing
#        academic/research use - satellite-derived flare data is
#        often shared on request even when not bulk-downloadable.
#
#    Once you have site-level lat/lon + flare volume, geocode to state
#    with a Nigeria state-boundary shapefile (e.g., from GADM):
# =============================================================

# library(sf)
# nga_states <- st_read("data/raw/gadm41_NGA_1.shp")  # download from gadm.org
# flare_sf   <- st_as_sf(flares, coords = c("lon", "lat"), crs = 4326)
# flare_state <- st_join(flare_sf, nga_states["NAME_1"])
# flare_by_state <- flare_state %>%
#   st_drop_geometry() %>%
#   group_by(NAME_1, year) %>%
#   summarise(flare_volume = sum(volume, na.rm = TRUE), .groups = "drop")
# write_csv(flare_by_state, "data/processed/flare_by_state.csv")

# =============================================================
# 5. VIIRS NIGHTTIME LIGHTS — state-level radiance
#    Two routes:
#
#    (a) Google Earth Engine via rgee (recommended - avoids downloading
#        huge global rasters):
#
#        install.packages("rgee"); library(rgee); ee_Initialize()
#        viirs <- ee$ImageCollection("NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG")
#        # reduce mean radiance by state polygon per year, export as CSV
#
#    (b) Direct raster download from the Earth Observation Group:
#        https://eogdata.mines.edu/products/vnl/
#        (annual composites - free, requires a login for bulk download)
#        Then process locally:
#
#        library(terra)
#        ntl <- rast("data/raw/VNL_v2_npp_2024_global_vcmslcfg_c202502.tif")
#        nga_states <- vect("data/raw/gadm41_NGA_1.shp")
#        ntl_by_state <- extract(ntl, nga_states, fun = mean, na.rm = TRUE)
#        ntl_by_state$state <- nga_states$NAME_1
#        write_csv(ntl_by_state, "data/processed/ntl_by_state.csv")
# =============================================================

# =============================================================
# 6. NBS / NERC — electricity access & consumption by state
#    No stable API. Manual steps:
#      - NBS: https://nigerianstat.gov.ng -> search "electricity" or
#        "energy statistics" -> download the relevant report (usually
#        PDF/Excel) -> extract the state-level table into CSV.
#      - NERC: https://nerc.gov.ng -> Disco quarterly/annual reports.
#    Once extracted:
#
#    elec <- read_csv("data/raw/nbs_electricity_access_by_state.csv")
# =============================================================

# =============================================================
# 7. ETP / LT-LEDS sectoral targets — reference values, not a dataset
#    Manually transcribe key benchmark figures (by sector: power, oil &
#    gas, transport, cooking, industry) into a small lookup table you
#    control, e.g.:
# =============================================================

etp_targets <- tribble(
  ~sector,     ~baseline_year, ~target_year, ~target_metric,          ~source,
  "power",      2020,           2060,        "renewables_share_pct",  "Nigeria ETP 2022/2024",
  "oil_gas",    2020,           2030,        "zero_routine_flaring",  "GGFR / Climate Change Act",
  "transport",  2020,           2060,        "ev_share_pct",          "Nigeria ETP 2022/2024",
  "cooking",    2020,           2030,        "clean_cooking_access",  "Nigeria ETP 2022/2024",
  "industry",   2020,           2060,        "emissions_intensity",   "Nigeria ETP 2022/2024"
)
write_csv(etp_targets, "data/processed/etp_sectoral_targets_template.csv")
message("Template saved - fill in actual target values from the ETP/LT-LEDS PDFs")

message("\nDone. National-level data (WDI, OWID) is pulled automatically.
Flaring (NOSDRA/GGFR), nighttime lights (VIIRS), and electricity access
(NBS/NERC) require the manual/semi-manual steps documented above, since
none currently expose a stable public API.")
