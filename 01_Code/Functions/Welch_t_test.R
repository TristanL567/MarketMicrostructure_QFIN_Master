Welch_t_test <- function(data, term_name) {
  subset_data <- data %>% filter(term_math == term_name)
  
  n_fomc <- sum(subset_data$Event_Type == "FOMC")
  n_control <- sum(subset_data$Event_Type == "Control")
  
  if(n_fomc < 2 | n_control < 2) {
    return(tibble(term = term_name, result = "Not enough data"))
  }
  
  test_result <- t.test(estimate ~ Event_Type, data = subset_data, var.equal = FALSE)
  
  tidy(test_result) %>%
    mutate(term = term_name) %>%
    select(term, estimate, statistic, p.value, conf.low, conf.high)
}