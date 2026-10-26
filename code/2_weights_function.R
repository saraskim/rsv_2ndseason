# -----------------------------------------------------------------------------
# 
# Function for making propensity weights for:
#     Long-acting monoclonal antibodies
#     RSVpreF during pregnancy
#     Control
#
# Uses dataset already in long, weekly format with time varying variables for:
#     Outcome
#     Each intervention
#     Loss to follow up
# 
# In the function, must define: 
# data: long dataset, with one row per week
# grace_weeks: time where not receiving intervention yet is consistent with intervention protocol
# mab_denom_formula: denominator formula for learning about mab recpients 
# pref_denom_formula: denominator formula for learning about RSVpreF recipients
# ltfu_denom_formula: denominator formula for learning about loss to follow up,
# mab_num_formula: numerator forumula for counterfactual scenario where infants receive mabs within the grace period
# (don't need numerator models for RSVpreF and loss to follow up)
# 
# -----------------------------------------------------------------------------

here::i_am("code/2_weights_function.R")

# ----- function
make_weights <- function(
    data,
    mab_week_col,
    mab_tv_col,
    grace_weeks = 6*4.3,
    mab_denom_formula,
    pref_denom_formula,
    ltfu_denom_formula,
    mab_num_formula
) {
  
  # Resolve the mab_week column once at the top
  data <- data %>% mutate(mab_week = .data[[mab_week_col]],
                          mab_tv = .data[[mab_tv_col]])
  
  # ----- Denominator Models -----
  
  # Mab propensity model
  #pref_binary_proper == 0 means moms who did not get pref at all or got pref <14 days before delivery
  mab_propensity_model_data <- data %>%
    filter(pref_binary_proper == 0, week <= mab_week)
  
  mab_propensity_model <- glm(
    mab_denom_formula,
    family = binomial(),
    data = mab_propensity_model_data
  )
  
  data$mab_propensity_prediction <- predict(mab_propensity_model, newdata = data, type = "response")
  rm(mab_propensity_model)
  gc()
  
  # Pref propensity model
  pref_propensity_model_data <- data %>% filter(week == 0)
  
  pref_propensity_model <- glm(
    pref_denom_formula,
    family = binomial(),
    data = pref_propensity_model_data
  )
  
  data$pref_propensity_prediction <- predict(pref_propensity_model, newdata = data, type = "response")
  rm(pref_propensity_model)
  gc()
  
  # LTFU (right censoring) propensity model
  ltfu_propensity_model_data <- data %>% filter(week <= ltfu_week)
  
  ltfu_propensity_model <- glm(
    ltfu_denom_formula,
    family = binomial(),
    data = ltfu_propensity_model_data
  )
  
  data$ltfu_propensity_prediction <- predict(ltfu_propensity_model, newdata = data, type = "response")
  rm(ltfu_propensity_model)
  gc()
  
  # ----- Numerator Model -----
  # mabs okay for moms who had pref <14 days or no pref
  # ie who got mabs according to policy - get rid of people who got both greater than 14 days prior to delivery
  mab_num_model <- glm(
    mab_num_formula, family = binomial(),
    data = data %>% filter(!(pref_binary_proper == 1 & mab_binary == 1), mab_week <= grace_weeks)
  )
  raw_num_preds <- predict(mab_num_model, newdata = data, type = "response")
  rm(mab_num_model); gc()
  
  data$mab_num_propensity_prediction <- ifelse(data$week == grace_weeks, 1, raw_num_preds)
  rm(raw_num_preds); gc()
  
  # ----- Make probabilities -----
  
  data <- data %>%
    mutate(
      
      # denominators
      mab_propensity = case_when(
        (week < mab_week)  & (mab_week <= grace_weeks) ~ 1 - mab_propensity_prediction,
        (week == mab_week) & (mab_week <= grace_weeks) ~ mab_propensity_prediction,
        (week > mab_week)  & (mab_week <= grace_weeks) ~ 1,
        (week <= grace_weeks) & (mab_week > grace_weeks) ~ 1 - mab_propensity_prediction,
        (week > grace_weeks)  & (mab_week > grace_weeks) ~ 99999
      ),
      
      mab_propensity_ideal = ifelse(
        week == 0, mab_propensity_prediction, 1
      ),
      
      # numerators
      pref_num = case_when(
        pref_tv == 0               ~ 0,
        pref_tv == 1 & mab_tv == 0 ~ 1,
        pref_tv == 1 & mab_tv == 1 ~ 0
      ),
      
      control_num = case_when(
        pref_tv == 1               ~ 0,
        pref_tv == 0 & mab_tv == 0 ~ 1,
        pref_tv == 0 & mab_tv == 1 ~ 0
      ),
      
      mab_num_propensity = case_when(
        (week < mab_week)   & (mab_week <= grace_weeks) ~ 1 - mab_num_propensity_prediction,
        (week == mab_week)  & (mab_week <= grace_weeks) ~ mab_num_propensity_prediction,
        (week > mab_week)   & (mab_week <= grace_weeks) ~ 1,
        (week < grace_weeks)  & (mab_week > grace_weeks) ~ 1 - mab_num_propensity_prediction,
        (week >= grace_weeks) & (mab_week > grace_weeks) ~ 0
      ),
      
      mab_num_ideal = ifelse(
        mab_week == 0, 1, 0
      )
    )
  
  # ----- Weights -----
  
  data <- data %>%
    group_by(NEWBORN_ID) %>%
    arrange(week) %>%
    mutate(
      # mab
      wt_mab = ((1 - pref_binary) * cumprod(mab_num_propensity)) /
        ((1 - pref_propensity_prediction) * cumprod(mab_propensity * (1 - ltfu_propensity_prediction))),
      # mab within first week of life (week = 0)
      wt_mab_ideal = (1 - pref_binary) * mab_num_ideal /
        ((1 - pref_propensity_prediction) * cumprod(mab_propensity_ideal * (1 - ltfu_propensity_prediction))),
      
      # wt_pref logic to includes those who got pref >= 14 days prior to delivery versus those who got pref <14 before delivery
      wt_pref = case_when(
        #>= 14 days before delivery
        pref_binary_proper == 1 ~ 
          pref_num /
          (pref_propensity_prediction * cumprod((1 - mab_propensity_prediction) * (1 - ltfu_propensity_prediction))),
        #<14 days before delivery
        pref_binary == 1 & pref_lt14 == 1 ~ 
          cumprod(mab_num_propensity) / ((pref_propensity_prediction) * cumprod(mab_propensity * (1 - ltfu_propensity_prediction))),
        #did not get pref
        pref_binary == 0 ~ 0
      ),
      #control
      wt_cntrl = control_num /
        ((1 - pref_propensity_prediction) * cumprod((1 - mab_propensity_prediction) * (1 - ltfu_propensity_prediction)))
    ) %>%
    ungroup()
  
  return(data)
}



# marketscan
data_weekly <- readRDS("data/weekly/marketscan_data_weekly_7jul2026.rds")

data_weekly_weights <- make_weights(
  data = data_weekly,
  mab_week_col = "mab_week_ari_ma_fixed",
  mab_tv_col = "mab_ari_ma_tv",
  grace_weeks = 6*4.3,
  mab_denom_formula = mab_ari_ma_tv ~ MAT_AGE + mat_cardio_derived + mat_gd_derived +
    mat_obesity_derived + mat_liver_derived + mat_hema_derived + mat_immunocomp_derived + 
    abnorm_fetal_pres_derived + placental_abruption_derived + obstetric_hemorrhage_derived +
    hemolysis_derived + umbilical_cord_combined + fetal_growth_combined +
    circulatory_malformations_derived + pulmonary_abnormalities_derived + 
    preterm_combined + prenatal_counts +
    splines::ns(week)*month_rel_season + season + REGION +
    pref_lt14 + preftobirth,
  
  pref_denom_formula = pref_tv ~ MAT_AGE + mat_cardio_derived + mat_gd_derived +
    mat_obesity_derived + mat_liver_derived + mat_hema_derived + mat_immunocomp_derived + 
    prenatal_counts + splines::ns(epi_week_tv) + month_rel_season + season + REGION,
  
  ltfu_denom_formula = ltfu_tv ~ MAT_AGE + mat_cardio_derived + mat_gd_derived +
    mat_obesity_derived + mat_liver_derived + mat_hema_derived + mat_immunocomp_derived + 
    abnorm_fetal_pres_derived + placental_abruption_derived + obstetric_hemorrhage_derived +
    hemolysis_derived + umbilical_cord_combined + fetal_growth_combined +
    circulatory_malformations_derived + pulmonary_abnormalities_derived + REGION +
    preterm_combined + prenatal_counts +
    splines::ns(week) + splines::ns(epi_week_tv),
  
  mab_num_formula = mab_ari_ma_tv ~ MAT_AGE + mat_cardio_derived + mat_gd_derived +
    mat_obesity_derived + mat_liver_derived + mat_hema_derived + mat_immunocomp_derived + 
    abnorm_fetal_pres_derived + placental_abruption_derived + obstetric_hemorrhage_derived +
    hemolysis_derived + umbilical_cord_combined + fetal_growth_combined +
    circulatory_malformations_derived + pulmonary_abnormalities_derived + 
    preterm_combined + prenatal_counts +
    splines::ns(week)*month_rel_season + season + REGION +
    pref_lt14 + preftobirth
)

