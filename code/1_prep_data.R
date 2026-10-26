# ------------------------------------------------------------------------------
#
# Format dataset: light cleaning, transpose to be long, weekly
#
# Apr 20, 2026
# 
# ------------------------------------------------------------------------------

# -- load libs
pacman::p_load("dplyr","tidyr", "lubridate", "zoo", "ggplot2", "purrr", "gtsummary")

# -- establish path
here::i_am("code/1_prep_data.R")

# -- load data
raw_data <- readRDS("data/raw/marketscan_clean_6jul2026.rds")


 # -- light cleaning 
#          exclude those who did not meet general eligibility criteria
#          select only relevant variables

data2 <- raw_data %>% # 378,768
  filter(as.Date(NEWBORN_DOB_ESTIMATE) > as.Date("2023-09-30")) %>% #261,602
  filter(bornrsvseason == 1) %>% # 148051
  filter(mom_paired == 1) %>% # 124970
  filter(eligibility == 1) %>% # 120669
  filter(pal_binary != 1) %>% #120587

  mutate(mab_date = case_when(!is.na(NMAB_RECEIPT_DATE) ~ NMAB_RECEIPT_DATE,
                              is.na(NMAB_RECEIPT_DATE) ~ NA),
         birth_date = as.Date(NEWBORN_DOB_ESTIMATE),
         mab_date = as.Date(mab_date),
         overall_enr_end = as.Date(overall_enr_end),
         
         first_rsv_hosp_date = as.Date(first_rsv_hosp_date),
         first_rsv_ma_date = as.Date(first_rsv_ma_date),
         first_ari_hosp_date = as.Date(first_ari_hosp_date),
         first_ari_ma_date = as.Date(first_ari_ma_date),
         first_bronc_hosp_date = as.Date(first_bronc_hosp_date),
         first_bronc_ma_date = as.Date(first_bronc_ma_date),
         
         last_study_date = as.Date(birth_date + 182),
         
         date_end_rsv_hosp = pmin(last_study_date, first_rsv_hosp_date, overall_enr_end, na.rm = TRUE),
         date_end_rsv_hosp = as.Date(date_end_rsv_hosp),
         date_end_rsv_ma = pmin(last_study_date, first_rsv_ma_date, overall_enr_end, na.rm = TRUE),
         date_end_rsv_ma = as.Date(date_end_rsv_ma),
         
         date_end_bronc_hosp = pmin(last_study_date, first_bronc_hosp_date, overall_enr_end, na.rm = TRUE),
         date_end_bronc_hosp = as.Date(date_end_bronc_hosp),
         date_end_bronc_ma = pmin(last_study_date, first_bronc_ma_date, overall_enr_end, na.rm = TRUE),
         date_end_bronc_ma = as.Date(date_end_bronc_ma),
         
         date_end_ari_hosp = pmin(last_study_date, first_ari_hosp_date, overall_enr_end, na.rm = TRUE),
         date_end_ari_hosp = as.Date(date_end_ari_hosp),
         date_end_ari_ma = pmin(last_study_date, first_ari_ma_date, overall_enr_end, na.rm = TRUE),
         date_end_ari_ma = as.Date(date_end_ari_ma),
         
         rsv_hosp_within_fu = case_when(first_rsv_hosp_date < last_study_date ~ 1, 
                                   first_rsv_hosp_date == last_study_date ~ 1, 
                                   is.na(first_rsv_hosp_date) ~ 0,
                                   first_rsv_hosp_date > last_study_date ~ 0),
         
         rsv_ma_within_fu = case_when(first_rsv_ma_date < last_study_date ~ 1, 
                                   first_rsv_ma_date == last_study_date ~ 1, 
                                   is.na(first_rsv_ma_date) ~ 0,
                                   first_rsv_ma_date > last_study_date ~ 0),
         
         bronc_hosp_within_fu = case_when(first_bronc_hosp_date < last_study_date ~ 1, 
                                        first_bronc_hosp_date == last_study_date ~ 1, 
                                        is.na(first_bronc_hosp_date) ~ 0,
                                        first_bronc_hosp_date > last_study_date ~ 0),
         
         bronc_ma_within_fu = case_when(first_bronc_ma_date < last_study_date ~ 1, 
                                      first_bronc_ma_date == last_study_date ~ 1, 
                                      is.na(first_bronc_ma_date) ~ 0,
                                      first_bronc_ma_date > last_study_date ~ 0),
         
         ari_hosp_within_fu = case_when(first_ari_hosp_date < last_study_date ~ 1, 
                                        first_ari_hosp_date == last_study_date ~ 1, 
                                        is.na(first_ari_hosp_date) ~ 0,
                                        first_ari_hosp_date > last_study_date ~ 0),
         
         ari_ma_within_fu = case_when(first_ari_ma_date < last_study_date ~ 1, 
                                      first_ari_ma_date == last_study_date ~ 1, 
                                      is.na(first_ari_ma_date) ~ 0,
                                      first_ari_ma_date > last_study_date ~ 0),

         
         month_rel_season = (month(birth_date) - 10) %% 12 + 1,
         
         pref_lt14 = ifelse(pref_binary == 1 & pref_binary_proper == 0, 1, 0),
         preftobirth = ifelse(pref_binary == 1, birth_date - ABRYSVO_RECEIPT_DATE, 0),
         
         pref_month = month(ABRYSVO_RECEIPT_DATE),
         
         rsv_ma_with_test = ifelse(rsv_ma == 1 & test_flag == 1,1,0),
         rsv_hosp_with_test = ifelse(rsv_hosp == 1 & test_flag == 1,1,0),
         first_rsv_ma_test_date = if_else(rsv_ma_with_test == 1, as.Date(first_rsv_ma_date), NA),
         first_rsv_hosp_test_date = if_else(rsv_hosp_with_test == 1, as.Date(first_rsv_hosp_date), NA),
         date_end_rsv_hosp_test = pmin(last_study_date, first_rsv_hosp_test_date, overall_enr_end, na.rm = TRUE),
         date_end_rsv_hosp_test = as.Date(date_end_rsv_hosp_test),
         date_end_rsv_ma_test = pmin(last_study_date, first_rsv_ma_test_date, overall_enr_end, na.rm = TRUE),
         date_end_rsv_ma_test = as.Date(date_end_rsv_ma_test),
         rsv_hosp_test_within_fu = case_when(first_rsv_hosp_test_date < last_study_date ~ 1, 
                                        first_rsv_hosp_test_date == last_study_date ~ 1, 
                                        is.na(first_rsv_hosp_test_date) ~ 0,
                                        first_rsv_hosp_test_date > last_study_date ~ 0),
         
         rsv_ma_test_within_fu = case_when(first_rsv_ma_test_date < last_study_date ~ 1, 
                                      first_rsv_ma_test_date == last_study_date ~ 1, 
                                      is.na(first_rsv_ma_test_date) ~ 0,
                                      first_rsv_ma_date > last_study_date ~ 0)
         ) %>%
  
  filter(date_end_rsv_ma >= NEWBORN_DOB_ESTIMATE) %>% #120586
  
  
  select(c(NEWBORN_ID, birth_date, birth_month, month_rel_season, season, bornrsvseason, # infant id, infant birth date
           REGION,
           mab_binary, mab_date, timetomab, # mab 0/1, mab receipt date, days from birth to mab
           pref_binary, pref_binary_proper, pref_lt14, preftobirth, ABRYSVO_RECEIPT_DATE, pref_month,# mat vax >= 14 days prior to delivery 0/1
           rsv_hosp, first_rsv_hosp_date, rsv_hosp_within_fu,
           rsv_ma, first_rsv_ma_date, rsv_ma_within_fu, # first rsv encounter 0/1; date of first rsv ma
           any_hosp, first_ari_hosp_date, ari_hosp_within_fu,
           any_ma, first_ari_ma_date, ari_ma_within_fu,
           bronc_hosp, first_bronc_hosp_date, bronc_hosp_within_fu,
           bronc_ma, first_bronc_ma_date, bronc_ma_within_fu,
           overall_enr_end, last_study_date, # kaiser enrollment end date; study end date
           date_end_rsv_hosp, fu_time_rsv_hosp,
           date_end_rsv_ma, fu_time_rsv_ma, # date_end_rsv_ma: first of rsv date, kaiser enrollment end date, or study end; fu: days from birth to date_end 
           date_end_ari_hosp, fu_time_ari_hosp,
           date_end_ari_ma, fu_time_ari_ma,
           date_end_bronc_hosp, fu_time_bronc_hosp,
           date_end_bronc_ma, fu_time_bronc_ma,
           circulatory_malformations_derived, pulmonary_abnormalities_derived, cystic_fibrosis_derived,
           neuromuscular_derived, asphyxia_derived, preterm_combined, prenatal_counts,# covariates
           MAT_AGE, mat_cardio_derived, mat_gd_derived, mat_obesity_derived, mat_liver_derived, mat_hema_derived,
           mat_immunocomp_derived, placental_abruption_derived, abnorm_fetal_pres_derived, obstetric_hemorrhage_derived,
           hemolysis_derived, umbilical_cord_combined, fetal_growth_combined,
           any_newborn_complication_derived, any_mat_comorbid_derived, any_mat_obstetric_derived, test_flag,
           rsv_ma_with_test, rsv_hosp_with_test, first_rsv_ma_test_date, first_rsv_hosp_test_date, 
           date_end_rsv_hosp_test,date_end_rsv_ma_test,rsv_hosp_test_within_fu,rsv_ma_test_within_fu))

# -- covert to long 
data_weekly <- data2 %>%
  rowwise() %>%
  mutate(
    # generate weekly sequence from birth to stop_date
    week_date = list(seq(birth_date, date_end_rsv_ma, by = "week")),
  ) %>%
  unnest(week_date) %>%
  ungroup() %>%
  
  # create week number for reference
  mutate(
    week = as.integer((week_date - birth_date) / 7),
    any_mab_week_frac =  ifelse(!is.na(mab_date), (mab_date - birth_date) / 7, 99999),
    any_mab_week =  ifelse(!is.na(mab_date), as.integer((mab_date - birth_date) / 7), 99999),
    pref_week = ifelse(pref_binary == 1, 0, 99999),
    
    rsv_ma_week_frac = ifelse(!is.na(first_rsv_ma_date), (first_rsv_ma_date - birth_date) / 7, 99999),
    rsv_ma_week      = ifelse(!is.na(first_rsv_ma_date), as.integer((first_rsv_ma_date - birth_date) / 7), 99999),
    rsv_hosp_week_frac = ifelse(!is.na(first_rsv_hosp_date), (first_rsv_hosp_date - birth_date) / 7, 99999),
    rsv_hosp_week      = ifelse(!is.na(first_rsv_hosp_date), as.integer((first_rsv_hosp_date - birth_date) / 7), 99999),
    
    ari_ma_week_frac = ifelse(!is.na(first_ari_ma_date), (first_ari_ma_date - birth_date) / 7, 99999),
    ari_ma_week      = ifelse(!is.na(first_ari_ma_date), as.integer((first_ari_ma_date - birth_date) / 7), 99999),
    ari_hosp_week_frac = ifelse(!is.na(first_ari_hosp_date), (first_ari_hosp_date - birth_date) / 7, 99999),
    ari_hosp_week      = ifelse(!is.na(first_ari_hosp_date), as.integer((first_ari_hosp_date - birth_date) / 7), 99999),
    
    bronc_ma_week_frac = ifelse(!is.na(first_bronc_ma_date), (first_bronc_ma_date - birth_date) / 7, 99999),
    bronc_ma_week      = ifelse(!is.na(first_bronc_ma_date), as.integer((first_bronc_ma_date - birth_date) / 7), 99999),
    bronc_hosp_week_frac = ifelse(!is.na(first_bronc_hosp_date), (first_bronc_hosp_date - birth_date) / 7, 99999),
    bronc_hosp_week      = ifelse(!is.na(first_bronc_hosp_date), as.integer((first_bronc_hosp_date - birth_date) / 7), 99999),
    
    rsv_ma_test_week_frac = ifelse(!is.na(first_rsv_ma_test_date), (first_rsv_ma_test_date - birth_date) / 7, 99999),
    rsv_ma_test_week      = ifelse(!is.na(first_rsv_ma_test_date), as.integer((first_rsv_ma_test_date - birth_date) / 7), 99999),
    rsv_hosp_test_week_frac = ifelse(!is.na(first_rsv_hosp_test_date), (first_rsv_hosp_test_date - birth_date) / 7, 99999),
    rsv_hosp_test_week      = ifelse(!is.na(first_rsv_hosp_test_date), as.integer((first_rsv_hosp_test_date - birth_date) / 7), 99999),
    
    ltfu_week         = ifelse(overall_enr_end < last_study_date, as.integer((overall_enr_end - birth_date) / 7), 99999),
    
    mab_week_rsv_hosp = ifelse(any_mab_week_frac < rsv_hosp_week_frac, any_mab_week, 99999),
    mab_week_rsv_hosp_fixed = case_when(mab_week_rsv_hosp >= 0 ~ mab_week_rsv_hosp,
                                        mab_week_rsv_hosp == -1 ~ 0, # estimated birth dates lend to some mab receipt as negative weeks. 
                                        mab_week_rsv_hosp == -2 ~ 0),
    mab_week_rsv_ma = ifelse(any_mab_week_frac < rsv_ma_week_frac, any_mab_week, 99999),
    mab_week_rsv_ma_fixed = case_when(mab_week_rsv_ma >= 0 ~ mab_week_rsv_ma,
                                        mab_week_rsv_ma == -1 ~ 0,
                                        mab_week_rsv_ma == -2 ~ 0),
    
    mab_week_ari_hosp = ifelse(any_mab_week_frac < ari_hosp_week_frac, any_mab_week, 99999),
    mab_week_ari_hosp_fixed = case_when(mab_week_ari_hosp >= 0 ~ mab_week_ari_hosp,
                                        mab_week_ari_hosp == -1 ~ 0,
                                        mab_week_ari_hosp == -2 ~ 0),
    mab_week_ari_ma = ifelse(any_mab_week_frac < ari_ma_week_frac, any_mab_week, 99999),
    mab_week_ari_ma_fixed = case_when(mab_week_ari_ma >= 0 ~ mab_week_ari_ma,
                                        mab_week_ari_ma == -1 ~ 0,
                                        mab_week_ari_ma == -2 ~ 0),
    
    mab_week_bronc_hosp = ifelse(any_mab_week_frac < bronc_hosp_week_frac, any_mab_week, 99999),
    mab_week_bronc_hosp_fixed = case_when(mab_week_bronc_hosp >= 0 ~ mab_week_bronc_hosp,
                                        mab_week_bronc_hosp == -1 ~ 0,
                                        mab_week_bronc_hosp == -2 ~ 0),
    mab_week_bronc_ma = ifelse(any_mab_week_frac < bronc_ma_week_frac, any_mab_week, 99999),
    mab_week_bronc_ma_fixed = case_when(mab_week_bronc_ma >= 0 ~ mab_week_bronc_ma,
                                          mab_week_bronc_ma == -1 ~ 0,
                                          mab_week_bronc_ma == -2 ~ 0),
    
    mab_week_rsv_hosp_test = ifelse(any_mab_week_frac < rsv_hosp_test_week_frac, any_mab_week, 99999),
    mab_week_rsv_hosp_test_fixed = case_when(mab_week_rsv_hosp_test >= 0 ~ mab_week_rsv_hosp_test,
                                        mab_week_rsv_hosp_test == -1 ~ 0,
                                        mab_week_rsv_hosp_test == -2 ~ 0),
    mab_week_rsv_ma_test = ifelse(any_mab_week_frac < rsv_ma_test_week_frac, any_mab_week, 99999),
    mab_week_rsv_ma_test_fixed = case_when(mab_week_rsv_ma_test >= 0 ~ mab_week_rsv_ma_test,
                                      mab_week_rsv_ma_test == -1 ~ 0,
                                      mab_week_rsv_ma_test == -2 ~ 0)
    
  ) %>%
  
  # variable_tv: 1 starting week of intervention/outcome and onward
  mutate(
    month_tv = month(week_date),
    mab_rsv_ma_tv = ifelse(mab_week_rsv_ma < 99999 & week >= mab_week_rsv_ma, 1, 0),
    mab_rsv_hosp_tv = ifelse(mab_week_rsv_hosp < 99999 & week >= mab_week_rsv_hosp, 1, 0),
    mab_ari_ma_tv = ifelse(mab_week_ari_ma < 99999 & week >= mab_week_ari_ma, 1, 0),
    mab_ari_hosp_tv = ifelse(mab_week_ari_hosp < 99999 & week >= mab_week_ari_hosp, 1, 0),
    mab_bronc_ma_tv = ifelse(mab_week_bronc_ma < 99999 & week >= mab_week_bronc_ma, 1, 0),
    mab_bronc_hosp_tv = ifelse(mab_week_bronc_hosp < 99999 & week >= mab_week_bronc_hosp, 1, 0),
    mab_rsv_ma_test_tv = ifelse(mab_week_rsv_ma_test < 99999 & week >= mab_week_rsv_ma_test, 1, 0),
    mab_rsv_hosp_test_tv = ifelse(mab_week_rsv_hosp_test < 99999 & week >= mab_week_rsv_hosp_test, 1, 0),
    pref_tv = ifelse(week >= pref_week, 1, 0),
    rsv_ma_tv = ifelse(!is.na(first_rsv_ma_date) & week >= rsv_ma_week, 1, 0),
    rsv_hosp_tv = ifelse(!is.na(first_rsv_hosp_date) & week >= rsv_hosp_week, 1, 0),
    ari_ma_tv = ifelse(!is.na(first_ari_ma_date) & week >= ari_ma_week, 1, 0),
    ari_hosp_tv = ifelse(!is.na(first_ari_hosp_date) & week >= ari_hosp_week, 1, 0),
    bronc_ma_tv = ifelse(!is.na(first_bronc_ma_date) & week >= bronc_ma_week, 1, 0),
    bronc_hosp_tv = ifelse(!is.na(first_bronc_hosp_date) & week >= bronc_hosp_week, 1, 0),
    rsv_ma_test_tv = ifelse(!is.na(first_rsv_ma_test_date) & week >= rsv_ma_test_week, 1, 0),
    rsv_hosp_test_tv = ifelse(!is.na(first_rsv_hosp_test_date) & week >= rsv_hosp_test_week, 1, 0),
    ltfu_tv = ifelse(!is.na(overall_enr_end) & overall_enr_end != last_study_date & 
        week >= ltfu_week, 1, 0),
    epi_week_tv = epiweek(week_date),
  ) %>%
  arrange(NEWBORN_ID)

# CHECK that no pref babies got the outcome on week 0. if they did, we want to make sure they are included 
# as of 7/7, no babies got outcomes that early. 
test <- data_weekly %>%
  filter(pref_binary_proper == 1 & ltfu_week == 0)

# how many got both interventions?
test <- data2 %>%
  filter(pref_binary == 1 & mab_binary == 1) %>%
  select(c(NEWBORN_ID, pref_binary, pref_binary_proper, pref_lt14, mab_binary))
table(test$pref_binary_proper, test$pref_lt14, useNA = "always")

# -- save
saveRDS(data_weekly, "data/weekly/marketscan_data_weekly_9jul2026.rds")




