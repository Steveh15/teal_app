breaks = unique(df$x_ord)
labels = unique(df$x_label) 


breaks

labels


tibble(
  x = breaks,
  lab = labels
)






df %>% count(USUBJID, ARM) %>% count(ARM)
arm_choices <- unique(df$ARM) %>% sort()

subject_counts <- sapply(arm_choices, function(arm) {
  length(unique(df$USUBJID[df$ARM == arm]))  # Count unique subjects for each arm
})


subject_counts <- sapply(arm_choices, function(arm) {
  sum(df$ARM == arm)  # Count the number of subjects for each arm
})


df %>% count(USUBJID) %>% count(USUBJID)

visits <- tibble(
  ADY = 1,
  ATPTN = seq(0,8, by = 2),
  AVISITN = 1
) %>% rbind( tibble(ADY = c( c(2:7), seq(14, 56, by = 7)), ATPTN = NA, AVISITN = NA)) %>% 
  mutate(
    AVISIT = paste0("DAY ", ADY),
    
    
    AVISITN = accumulate(AVISITN, ~if_else(is.na(.y), max(.x) + 1, .y)),
    
    ATPT = if_else(!is.na(ATPTN), paste0(ATPTN, " h"), "")
  ) %>% 
  select(AVISIT, AVISITN, ATPT, ATPTN, ADY) 



tibble(ADY = c( c(2:7), seq(7, 56, by = 7)), ATPTN = NA, AVISITN = NA)
