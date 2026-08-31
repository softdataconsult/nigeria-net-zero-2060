# Pathways to Net-Zero by 2060: A Spatial-Econometric Assessment of Nigeria's Energy Transition Plan Implementation

Research repository for a spatial-econometric assessment of Nigeria's net-zero-by-2060 pathway,
combining sub-national gas flaring analysis with a national ARDL emissions-growth-energy model.

**Author:** Isaac O. Ajao, Department of Statistics, Federal Polytechnic Ado-Ekiti, Nigeria

## Research Questions

- **RQ1:** Is there significant spatial autocorrelation in onshore flaring intensity across Nigerian states?
- **RQ2:** Do electricity access and onshore flare-site density predict state-level flaring intensity, and do they explain the RQ1 clustering?
- **RQ3:** What is the long-run and short-run relationship between CO2 emissions, GDP, and energy use nationally, and is there evidence of a shift associated with the 2021 Climate Change Act?

## Repository Structure

```
scripts/      R scripts, run in numeric order (01-08)
data/raw/     Source data (flaring, electricity, GDP, OWID CO2/energy)
data/processed/  Cleaned/merged panels and model-ready datasets
figures/      Final maps (choropleth, LISA cluster map)
manuscript/   Research proposal and full manuscript draft (.docx)
results/      Plain-text model output summaries (ARDL, spatial models)
```

## Pipeline (run scripts/ in order)

| Script | Purpose |
|---|---|
| `01_get_data.R` | Pulls national CO2/GDP/energy data (World Bank, OWID) |
| `02_geocode_flares.R` | Geocodes site-level GGFR flare data to Nigerian states; splits onshore/offshore |
| `04_spatial_model.R` | Moran's I (global), spatial lag/error models (RQ1, RQ2) |
| `05_electricity_access.R` | Aggregates LGA-level NDHS electricity access to state level; merges with flaring |
| `06_dhs_cluster_to_state.R` | Helper: DHS cluster-to-state crosswalk (if starting from raw DHS microdata) |
| `07_ardl_model.R` | National ARDL bounds-testing model (RQ3) |
| `08_maps.R` | Produces the two final figures (choropleth, LISA cluster map) |

`03_get_nighttime_lights_DEPRECATED.R` — VIIRS/Black Marble nighttime-lights extraction, **not used in the
final analysis**. Dropped due to data-access friction (HDF5/GDAL driver issues on Windows) and because
flare sites are themselves a light source, creating mechanical correlation with the flaring outcome. Kept
for reference only.

## Key Findings

- **RQ1:** Significant positive spatial clustering of onshore flaring every year, 2012-2024 (Moran's I = 0.37-0.59, p < 0.001).
- **RQ2:** Onshore flare-site density is a strong, significant predictor of flaring intensity (p < 0.001); electricity access is not. Site density explains most of the RQ1 spatial clustering.
- **RQ3:** UECM error-correction term is negative and significant (-0.699, p = 0.003), evidencing a long-run CO2-GDP-energy relationship. The formal Pesaran et al. (2001) bounds F-test could not be computed due to a documented small-sample degrees-of-freedom limitation.

See `manuscript/Nigeria_NetZero_Manuscript.docx` for the full write-up.

## Data Sources

- Gas flaring: World Bank Global Gas Flaring Reduction (GGFR) partnership, satellite-derived site-level estimates (2012-2024)
- Electricity access: NDHS-derived LGA-level extract (2018), aggregated to state level
- CO2/GDP/energy: Our World in Data / Global Carbon Project; GDP series extended separately (1986-2024)
- State boundaries: GADM (via the R `geodata` package)

## Requirements

R packages: `sf`, `geodata`, `spdep`, `spatialreg`, `ARDL`, `dynlm`, `tseries`, `urca`, `lmtest`,
`sandwich`, `strucchange`, `dplyr`, `tidyr`, `readr`, `ggplot2`, `viridis`.

## Status

Analysis and manuscript draft complete. Not yet submitted. A comparative benchmarking extension
(South Africa 2050, India 2070 net-zero pathways) was scoped but not completed in this iteration.
