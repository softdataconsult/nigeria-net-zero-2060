# =============================================================
# Nigeria National-Level ARDL Bounds-Testing Model
# RQ3: What is the estimated gap between pledged and current-trajectory
#      emissions at the 2030 interim milestone and the 2060 target?
#
# Inputs:  nga_owid_national.csv (from 01_get_data.R / OWID CO2 data)
#          flare_national_panel.csv (from 02_geocode_flares.R)
# Output:  ardl_model_summary.txt
# =============================================================

# ---- 0. Packages ----
pkgs <- c("readr", "dplyr", "tseries", "urca", "dynlm", "lmtest",
          "sandwich", "strucchange", "ARDL")
new  <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]
if (length(new)) install.packages(new, repos = "https://cloud.r-project.org")
lapply(pkgs, library, character.only = TRUE)

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

# =============================================================
# STEP 1 — Build the national annual panel
# =============================================================
# NOTE: OWID's own GDP column stops at 2022 (lags CO2/energy data by 1-2
# years in that source), which would leave only 2 post-2021 observations
# for the policy_index below - too thin to estimate a policy effect
# credibly. Use a separately-sourced GDP series (e.g. World Bank WDI,
# current US$, NY.GDP.MKTP.CD) with fuller 1986-2024+ coverage instead.
# Expected columns: year, gdp_new.
#
# Reads from data/raw/owid_co2_nga_zaf_ind.csv, produced by 01_get_data.R
# (contains Nigeria, South Africa, and India together - filtered to
# Nigeria here).
owid_all <- read_csv("data/raw/owid_co2_nga_zaf_ind.csv", show_col_types = FALSE)
owid <- owid_all %>%
  filter(iso_code == "NGA") %>%
  select(year, co2, co2_per_capita, gdp, population, coal_co2, oil_co2,
         gas_co2, flaring_co2, energy_per_capita) %>%
  filter(year >= 1995)

gdp_ext <- read_csv("data/raw/gdp_extended.csv", show_col_types = FALSE)  # year, gdp_new
flare_nat <- read_csv("data/processed/flare_national_panel.csv", show_col_types = FALSE) %>%
  rename(year = Year)

national <- owid %>%
  select(-gdp) %>%
  inner_join(gdp_ext, by = "year") %>%
  rename(gdp = gdp_new) %>%
  left_join(flare_nat, by = "year") %>%
  filter(!is.na(co2), !is.na(gdp)) %>%
  arrange(year)

# ---- Policy index (RQ3 regressor) ----
# A simple, transparent step-dummy capturing the post-Climate-Change-Act
# policy regime, rather than an elaborate compliance index that would
# need far more milestone-level documentation to defend. State this
# explicitly as a simplifying assumption in your methods section - a
# more granular index (e.g. weighted by which of the Act's specific
# deadlines were met each year) is a natural extension, not a
# requirement for this first model.
#
# IMPORTANT: with data through 2024, this gives ~4 post-2021 observations
# (2021-2024) - still thin for precisely estimating a policy effect, but
# a meaningful improvement over the 2 observations available before the
# GDP series was extended. Report the resulting standard error honestly;
# a wide confidence interval on this coefficient is an accurate reflection
# of the data available, not a modeling failure.
national <- national %>%
  mutate(policy_index = if_else(year >= 2021, 1, 0))

write_csv(national, "data/processed/national_ardl_panel.csv")
message("National ARDL panel: ", nrow(national), " years (",
        min(national$year), "-", max(national$year), ")")
print(national)

# =============================================================
# STEP 2 — Unit root tests (rule out I(2) before trusting ARDL bounds test)
# =============================================================
# ARDL bounds testing tolerates a MIX of I(0)/I(1) regressors, but is
# invalid if any variable is I(2). Test levels and first differences for
# each candidate variable with both ADF (null: unit root) and KPSS
# (null: stationarity) - using both, rather than just one, because they
# have opposite nulls and disagreement between them is itself informative
# in short samples like this one.

run_unit_root <- function(x, name) {
  x <- na.omit(x)
  cat("\n===", name, "===\n")
  adf_lvl  <- tryCatch(adf.test(x), error = function(e) NULL)
  kpss_lvl <- tryCatch(kpss.test(x, null = "Level"), error = function(e) NULL)
  if (!is.null(adf_lvl))  cat("ADF (level):  stat=", round(adf_lvl$statistic, 3),
                               " p=", round(adf_lvl$p.value, 3), "\n")
  if (!is.null(kpss_lvl)) cat("KPSS (level): stat=", round(kpss_lvl$statistic, 3),
                               " p=", round(kpss_lvl$p.value, 3), "\n")

  dx <- diff(x)
  adf_diff  <- tryCatch(adf.test(dx), error = function(e) NULL)
  kpss_diff <- tryCatch(kpss.test(dx, null = "Level"), error = function(e) NULL)
  if (!is.null(adf_diff))  cat("ADF (1st diff):  stat=", round(adf_diff$statistic, 3),
                                " p=", round(adf_diff$p.value, 3), "\n")
  if (!is.null(kpss_diff)) cat("KPSS (1st diff): stat=", round(kpss_diff$statistic, 3),
                                " p=", round(kpss_diff$p.value, 3), "\n")

  # Flag likely I(2): if 1st-differenced series STILL shows a unit root
  # (ADF fails to reject AND KPSS rejects stationarity), the variable may
  # be I(2), which would invalidate the ARDL bounds test for this variable.
  if (!is.null(adf_diff) && !is.null(kpss_diff)) {
    if (adf_diff$p.value > 0.10 && kpss_diff$p.value < 0.05) {
      warning(name, " may be I(2) - 1st difference still non-stationary. ",
              "ARDL bounds testing is NOT valid if this holds up under ",
              "further scrutiny (e.g. a longer sample or alternative test).")
    }
  }
}

candidate_vars <- c("co2", "gdp", "energy_per_capita", "gas_co2", "flare_vol_mcm_total")
for (v in candidate_vars) {
  if (v %in% names(national)) run_unit_root(national[[v]], v)
}

# =============================================================
# STEP 3 — Automatic ARDL order selection
# =============================================================
# Model: CO2 emissions as a function of GDP, energy use per capita, and
# the policy index. Adjust the RHS variables here once you've reviewed
# the unit root results above and finalized your variable set (e.g. you
# may prefer gas_co2 or flare_vol_mcm_total in place of / alongside
# energy_per_capita, depending on which RQ3 framing you want to lead with).

national_df <- as.data.frame(national)  # ARDL package expects a plain data.frame

# ---- Collinearity check before fitting ----
# With a short annual sample (~30 obs) and a step dummy that only changes
# value once (2021), including several lags of everything risks
# collinearity between the dummy and its own lags, or between lagged
# regressors more generally. Check pairwise correlations among candidate
# lagged terms before fitting - a correlation near +-1 here is a strong
# early warning sign for the "singular matrix" error that a bounds test
# or covariance calculation would otherwise throw much less
# informatively.
policy_lag1 <- dplyr::lag(national_df$policy_index)
corr_check <- cor(national_df$policy_index, policy_lag1, use = "complete.obs")
message("Correlation between policy_index and its 1st lag: ", round(corr_check, 3))
if (abs(corr_check) > 0.7) {
  message("This is high enough to risk a singular covariance matrix if ",
          "policy_index is given any lags. Kept at 0 lags below for ",
          "exactly this reason.")
}

# max_order: with ~25-30 annual observations, keep max lag order modest
# (2, not 3) for the continuous regressors - a longer lag structure eats
# degrees of freedom fast in a short annual series.
#
# IMPORTANT: policy_index is a step dummy that should NOT be lagged (a
# lag of it is highly collinear with its own level - see the correlation
# check above). The ARDL package has a dedicated syntax for exactly this:
# variables placed AFTER a "|" in the formula are treated as fixed
# regressors included only at their contemporaneous value, and are
# excluded entirely from the order search - so max_order's length only
# needs to cover the variables BEFORE the "|" (co2, gdp,
# energy_per_capita = 3 variables), not policy_index. This replaces an
# earlier, incorrect attempt to force this via max_order/fixed_order,
# which have a fragile length-matching contract not worth relying on
# when the formula syntax does the same thing directly.
auto_sel <- auto_ardl(
  co2 ~ gdp + energy_per_capita | policy_index,
  data = national_df,
  max_order = c(2, 2, 2)   # co2 (p), gdp (q1), energy_per_capita (q2)
)

message("Best ARDL order selected (by AIC): ", paste(auto_sel$best_order, collapse = ", "))
best_model <- auto_sel$best_model
print(summary(best_model))

# =============================================================
# STEP 4 — Bounds test for cointegration
# =============================================================
# case = 3: unrestricted intercept, no trend - the standard choice for
# macro/energy series without a strong theoretical reason to include a
# deterministic trend term. Revisit if a visual/theoretical case for a
# trend emerges once you inspect the series.
bounds_result <- tryCatch(
  bounds_f_test(best_model, case = 3),
  error = function(e) {
    message("Bounds test failed: ", conditionMessage(e))
    message("If this is a 'singular matrix' error, the most likely cause ",
            "is still collinearity from policy_index or its lags given the ",
            "thin (4-year) post-2021 sample. Trying a fallback model without ",
            "policy_index below to isolate this.")
    NULL
  }
)
if (!is.null(bounds_result)) print(bounds_result)

# ---- Fallback: bounds test WITHOUT policy_index ----
# The Pesaran et al. (2001) bounds-testing framework is built to test
# cointegration among the dependent variable and "long-run forcing" I(1)
# regressors (here: gdp, energy_per_capita). A fixed, unlagged step dummy
# like policy_index sits outside that framework's original design, and
# combined with only ~20 residual degrees of freedom, can be exactly
# what tips the test's internal covariance matrix into singularity - even
# though the ARDL model ITSELF (Step 3) estimates fine with policy_index
# included, since dynlm's OLS fit is far less sensitive to this than the
# bounds test's specific Wald-type computation.
#
# If bounds_result above is NULL, run the cointegration test on the
# "clean" long-run-forcing variables only, and treat the policy question
# as a separate exercise (see the NEXT STEP note at the end of this
# script) rather than forcing it into the same joint test.
if (is.null(bounds_result)) {
  message("\nRunning fallback: bounds test WITHOUT policy_index, to isolate ",
          "whether it was the cause.")
  auto_sel_noPolicy <- auto_ardl(
    co2 ~ gdp + energy_per_capita,
    data = national_df,
    max_order = c(2, 2, 2)
  )
  best_model_noPolicy <- auto_sel_noPolicy$best_model
  message("Fallback ARDL order (co2, gdp, energy_per_capita): ",
          paste(auto_sel_noPolicy$best_order, collapse = ", "))

  bounds_result_noPolicy <- tryCatch(
    bounds_f_test(best_model_noPolicy, case = 3),
    error = function(e) {
      message("Fallback bounds test ALSO failed: ", conditionMessage(e),
              " - the issue is likely the short sample itself (only ~28-30 ",
              "annual observations), not specifically policy_index. Consider ",
              "reducing max_order further (e.g. c(1,1,1)) before concluding ",
              "bounds testing isn't feasible with this data.")
      NULL
    }
  )
  if (!is.null(bounds_result_noPolicy)) {
    print(bounds_result_noPolicy)
    message("\nIf this succeeded: policy_index was confirmed as the cause. ",
            "Report cointegration using this policy_index-free model for ",
            "RQ3's core long-run relationship, and address the policy shift ",
            "separately - e.g. a Chow test / mean-shift check on this ",
            "model's residuals split at 2021, rather than a joint bounds test.")
  }
}

message("\nInterpretation: if the F-statistic exceeds the UPPER bound at ",
        "your chosen significance level, cointegration (a long-run ",
        "relationship) is supported. Below the LOWER bound: no ",
        "cointegration. Between the two: inconclusive - a common outcome ",
        "in short annual samples, and worth reporting honestly as such ",
        "rather than picking whichever side is more convenient.")

# =============================================================
# STEP 5 — Long-run and short-run (UECM) estimates
# =============================================================
uecm_model <- tryCatch(
  uecm(best_model),
  error = function(e) {
    message("UECM estimation failed: ", conditionMessage(e))
    message("Same likely cause as above (collinearity from a thin post-",
            "policy sample) - simplify the formula (drop policy_index or ",
            "reduce max_order further) and re-run.")
    NULL
  }
)
if (!is.null(uecm_model)) print(summary(uecm_model))

# Long-run multipliers (the "eventual" effect of a 1-unit change in each
# regressor on CO2 emissions, once the system returns to equilibrium)
lr_multipliers <- tryCatch(
  multipliers(best_model),
  error = function(e) {
    message("Long-run multipliers failed: ", conditionMessage(e), " - see above.")
    NULL
  }
)
if (!is.null(lr_multipliers)) print(lr_multipliers)

# =============================================================
# STEP 6 — Diagnostic tests
# =============================================================
# Standard battery for ARDL/UECM validity: serial correlation,
# heteroskedasticity, normality, and parameter stability. A model that
# fails these needs re-specification (different lag order, different
# regressors, or a structural break term) before its coefficients can be
# trusted for RQ3's gap estimate.

# =============================================================
# STEP 6 — Diagnostic tests
# =============================================================
# Standard battery for ARDL/UECM validity: serial correlation,
# heteroskedasticity, normality, and parameter stability. A model that
# fails these needs re-specification (different lag order, different
# regressors, or a structural break term) before its coefficients can be
# trusted for RQ3's gap estimate.
#
# These only run if Step 5's UECM estimation succeeded - if it failed
# (see the message above), fix the model specification first and re-run
# from Step 3 rather than expecting diagnostics on a model that doesn't exist.

if (!is.null(uecm_model)) {

  cat("\n=== Breusch-Godfrey (serial correlation) ===\n")
  print(bgtest(uecm_model, order = 2))

  cat("\n=== Breusch-Pagan (heteroskedasticity) ===\n")
  print(bptest(uecm_model))

  cat("\n=== Jarque-Bera (normality of residuals) ===\n")
  print(jarque.bera.test(residuals(uecm_model)))

  cat("\n=== CUSUM stability test ===\n")
  # strucchange needs a plain lm-compatible object - ARDL package provides
  # to_lm() for exactly this conversion. NOTE: to_lm() returns a standard
  # "lm" object, which has NO $formula field (that returned NULL here,
  # which is what caused efp() to fail with "no terms component nor
  # attribute" downstream) - use the formula() generic instead, which
  # correctly extracts it from the model's $call.
  uecm_lm <- to_lm(uecm_model)
  cusum_test <- efp(formula(uecm_lm), data = uecm_lm$model, type = "OLS-CUSUM")
  print(sctest(cusum_test))
  message("A significant CUSUM test (p < 0.05) suggests parameter ",
          "instability - plausible here given the 2021-2022 policy shift, ",
          "and worth flagging/investigating rather than treated as a pure ",
          "model failure.")

  # =============================================================
  # STEP 7 — Save everything for your records
  # =============================================================
  sink("data/processed/ardl_model_summary.txt")
  cat("=== Best ARDL model ===\n"); print(summary(best_model))
  cat("\n=== Bounds test for cointegration ===\n"); print(bounds_result)
  cat("\n=== UECM (long-run + short-run) ===\n"); print(summary(uecm_model))
  cat("\n=== Long-run multipliers ===\n"); print(lr_multipliers)
  cat("\n=== Diagnostics ===\n")
  cat("\n--- Breusch-Godfrey ---\n"); print(bgtest(uecm_model, order = 2))
  cat("\n--- Breusch-Pagan ---\n"); print(bptest(uecm_model))
  cat("\n--- Jarque-Bera ---\n"); print(jarque.bera.test(residuals(uecm_model)))
  cat("\n--- CUSUM stability ---\n"); print(sctest(cusum_test))
  sink()
  message("Saved: data/processed/ardl_model_summary.txt")

} else {
  message("Skipping Steps 6-7 (diagnostics, summary file) since the UECM ",
          "model in Step 5 did not estimate successfully. Resolve that ",
          "first - see the message printed above Step 5.")
}

# =============================================================
# NEXT STEP (RQ3): use the long-run multiplier on `policy_index` and the
# model's fitted trajectory to project CO2 emissions forward to 2030 and
# 2060 under current trends, then compare against the ETP's stated
# sectoral targets (transcribed in data/processed/etp_sectoral_targets_template.csv
# from 01_get_data.R) to quantify the pledged-vs-trajectory gap.
# =============================================================
