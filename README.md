Code flow for the RSV monoclonal antibody versus maternal vaccination "clone censor weight" analysis. 
Purpose: modify code to work for longer follow-up into a second season with repeated outcomes 

1_prep_data: takes data from one row per infant to one row per follow-up week per infant. Creates time-varying indicators for outcomes and interventions. 
2_weights_function: reads in weekly data created in 1_prep_data; specifies propensity models; adds weights for interventions 
3_clone_function: reads in weekly data with weights; takes non-zero weighted individuals into various subsets
4_results_function: reads in clone datasets; calculates summary effectiveness from MSM model and cumulative incidence of outcome per week 
