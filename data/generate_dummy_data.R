


rm(list = ls())

library(tidyverse)

set.seed(123)  # Setting seed for reproducibility

arms <- c( "ARM A - SXMB-101 300mg", "ARM B - SXMB-101 600mg", "ARM C - SXMB-101 900mg", "ARM D - SXMB-101 Placebo")


arms %>% length()

# Generate subject IDs
# n_subjects <- 80
# subject_ids <- sprintf("SXMB-101-%03d", 1:n_subjects)
# assigned_arms <- sample(rep(arms, each = 20))  # Randomly shuffle 20 subjects per arm

subjects_per_arm <- 3
n_subjects <- subjects_per_arm*length(arms)
subject_ids <- sprintf("SXMB-101-%03d", 1:n_subjects)
assigned_arms <- sample(rep(arms, each = subjects_per_arm))  # Randomly shuffle 20 subjects per arm

df_1 <- tibble(
  USUBJID = subject_ids,
  ARM = assigned_arms,
  TRTSTDTM = sample(seq(ymd_hms('2021-01-01 11:00:00'), ymd_hms('2022-12-31 11:00:00'), by = "day"), size = n_subjects),
  C_0 = rnorm(length(USUBJID), mean = 50, sd = 10)
)

visits <- tibble(
  ADY = 1,
  ATPTN = seq(0,8, by = 2),
  AVISITN = 1
) %>% rbind( tibble(ADY = c( c(2:7), seq(, 56, by = 7)), ATPTN = NA, AVISITN = NA)) %>% 
  mutate(
    AVISIT = paste0("DAY ", ADY),
    
    
    AVISITN = accumulate(AVISITN, ~if_else(is.na(.y), max(.x) + 1, .y)),
    
    ATPT = if_else(!is.na(ATPTN), paste0(ATPTN, " h"), "")
  ) %>% 
  select(AVISIT, AVISITN, ATPT, ATPTN, ADY) 

df_2 <- cross_join(df_1, visits) %>% 
  mutate(
    ADTM = TRTSTDTM + ddays(ADY) + if_else(!is.na(ATPTN), dhours(ATPTN), dhours(0)),
    ADIFF = (ADTM - TRTSTDTM)/ddays(1),
    
    noise = rnorm(n(), mean = 0, sd = 4),
    
    AVAL = case_when(
      str_detect(ARM, "Placebo") ~ 0,
      .default  = C_0*exp(-ADIFF/(10)) + noise
    ),
    
    # AVAL = AVAL + if_else(rnorm(length(AVAL), mean = 0, sd = 2),
    
    x_ord = AVISITN + if_else(!is.na(ATPTN), ATPTN/10, 0),
    x_label = paste0(AVISIT, if_else(!is.na(ATPTN), paste0(": ", ATPT), ""))
    
  ) %>% 
  select(-noise) 



df <- df_2 %>% 
  # filter(USUBJID %in% c("SXMB-101-001", "SXMB-101-002")) %>% 
  select(everything())


# haven::write_xpt(df, "data/adpc_small.xpt")

# 
# ggplot(df, aes(x = factor(x_ord), y = AVAL, group = USUBJID, color = USUBJID)) +
#   geom_line() +           # Create lines for each subject
#   geom_point() +          # Mark each data point with a point
#   labs(x = "Ordinal Variable", y = "Concentration (AVAL)", title = "Concentration Over Time by Subject") +
#   theme_minimal() +
#   theme(legend.position = "none")  # Remove the legend if there are many subjects


# ls()[ls()!= "df"]

rm(list = ls()[ls()!= "df"])

