# -----------------------------------------------------------------------------
# 
# Function for getting results:
#      Summary hazard ratios
#      KM style cumulative incidence curves over time
#      Restricted mean survival time 
#
# In the function, must define: 
#      clone_data: fully stacked clone data, created in make_clone_dataset function
#      mab_data: subset of non-zero mab weights, created in make_clone_dataset function
#      control_data: subset of non-zero control weights, created in make_clone_dataset function
#      pref_data: subset of non-zero preF weights, created in make_clone_dataset function
#      msm_formula: MSM formula for hazard ratio
#      km_formula: Formula for KM style cumulative incidence curves
#      n_weeks: number of weeks to follow-up for
#      n_wks_rmst: number of weeks to follow-up for restricted mean survival time
#
#
# -----------------------------------------------------------------------------

here::i_am("code/4_results_function.R")

get_results <- function(clone_data, mab_data, mab_data_ideal, control_data, pref_data, msm_formula, km_formula, n_weeks = 6*4.3, n_wks_rmst = 6*4.3) {
  
  # ----- Fit Models -----
  msm_fit <- glm(msm_formula, weights = wt, data = clone_data, family = binomial())
  
  km_control    <- glm(km_formula, weights = wt_cntrl,     data = control_data)
  km_mab        <- glm(km_formula, weights = wt_mab,       data = mab_data)
  km_mab_ideal  <- glm(km_formula, weights = wt_mab_ideal, data = mab_data_ideal)
  km_pref       <- glm(km_formula, weights = wt_pref,      data = pref_data)
  
  # ----- Extract MSM coefficients into a named vector -----
  msm_coefs <- setNames(coef(msm_fit), paste0("msm_coef_", names(coef(msm_fit))))
  
  # ----- Predictions -----
  pred_df <- data.frame(week = 0:n_weeks)
  
  haz_control   <- predict(km_control,   newdata = pred_df, type = "response")
  haz_mab       <- predict(km_mab,       newdata = pred_df, type = "response")
  haz_mab_ideal <- predict(km_mab_ideal, newdata = pred_df, type = "response")
  haz_pref      <- predict(km_pref,      newdata = pred_df, type = "response")
  
  cum_inc_control   <- 1 - cumprod(1 - haz_control)
  cum_inc_mab       <- 1 - cumprod(1 - haz_mab)
  cum_inc_mab_ideal <- 1 - cumprod(1 - haz_mab_ideal)
  cum_inc_pref      <- 1 - cumprod(1 - haz_pref)
  
  # S_control   <- c(1, cumprod(1 - haz_control))
  # S_mab       <- c(1, cumprod(1 - haz_mab))
  # S_mab_ideal <- c(1, cumprod(1 - haz_mab_ideal))
  # S_pref      <- c(1, cumprod(1 - haz_pref))
  # 
  # rmst_control   <- sum(S_control[1:n_wks_rmst])
  # rmst_mab       <- sum(S_mab[1:n_wks_rmst])
  # rmst_mab_ideal <- sum(S_mab_ideal[1:n_wks_rmst])
  # rmst_pref      <- sum(S_pref[1:n_wks_rmst])
  
  # ----- Wide Results Dataframe -----
  result <- data.frame(
    # rmst_control             = rmst_control,
    # rmst_mab                 = rmst_mab,
    # rmst_mab_ideal           = rmst_mab_ideal,
    # rmst_pref                = rmst_pref,
    # rmst_mab_vs_control      = rmst_mab - rmst_control,
    # rmst_mab_idea_vs_control = rmst_mab_ideal - rmst_control,
    # rmst_pref_vs_control     = rmst_pref - rmst_control,
    # rmst_mab_vs_pref         = rmst_mab - rmst_pref,
    # rmst_mab_ideal_vs_mab    = rmst_mab_ideal - rmst_mab,
    
    setNames(as.list(cum_inc_control),   paste0("cum_inc_control_wk",   0:n_weeks)),
    setNames(as.list(cum_inc_mab),       paste0("cum_inc_mab_wk",       0:n_weeks)),
    setNames(as.list(cum_inc_mab_ideal), paste0("cum_inc_mab_ideal_wk", 0:n_weeks)),
    setNames(as.list(cum_inc_pref),      paste0("cum_inc_pref_wk",      0:n_weeks)),
    
    as.list(msm_coefs),
    
    msm_mab_vs_pref       = unname(msm_coefs["msm_coef_factor(clone)mab"]       - msm_coefs["msm_coef_factor(clone)pref"]),
    msm_mab_ideal_vs_pref = unname(msm_coefs["msm_coef_factor(clone)mab_ideal"] - msm_coefs["msm_coef_factor(clone)pref"]),
    msm_mab_vs_mab_ideal  = unname(msm_coefs["msm_coef_factor(clone)mab"]       - msm_coefs["msm_coef_factor(clone)mab_ideal"])
  )  
  
  return(result)
}  


# results for medically attended rsv
results <- get_results(
  clone_data = clone_data$clone_data_stacked, 
  mab_data = clone_data$mab_data, 
  mab_data_ideal = clone_data$mab_data_ideal,
  control_data = clone_data$control_data, 
  pref_data = clone_data$pref_data, 
  msm_formula = rsv_ma_tv ~ factor(week) + factor(clone),
  km_formula = rsv_ma_tv ~ factor(week),
  n_weeks = 6*4.3,
  n_wks_rmst = 6*4.3
)
saveRDS(results, "output/rsv_ma/rsv_ma_results_7jul2026.rds")


# rsv hosp
results <- get_results(
  clone_data = clone_data$clone_data_stacked, 
  mab_data = clone_data$mab_data, 
  mab_data_ideal = clone_data$mab_data_ideal,
  control_data = clone_data$control_data, 
  pref_data = clone_data$pref_data, 
  msm_formula = rsv_hosp_tv ~ factor(week) + factor(clone),
  km_formula = rsv_hosp_tv ~ factor(week),
  n_weeks = 6*4.3,
  n_wks_rmst = 6*4.3
)

saveRDS(results, "output/rsv_hosp/rsv_hosp_results_8jul2026.rds")

