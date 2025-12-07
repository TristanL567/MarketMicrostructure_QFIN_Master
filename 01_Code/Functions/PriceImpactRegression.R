PriceImpactRegression <- function(data,
                                  NeweyWest = TRUE,
                                  lag_NW = 6,
                                  ...){
  
  #==========================#
  # 1. Estimate AR(1) for Order Flow (Phi)
  #==========================#
  
  ar1_model <- lm(q_t ~ lag_q_t, data = data) 
  phi_hat   <- coef(ar1_model)["lag_q_t"]
  
  #==========================#
  # 2. Main Price Impact Regression
  #==========================#
  price_impact_reg <- lm(
    delta_p ~ q_t + lag_q_t + delta_d_t, 
    data = data
  )
  
  #==========================#
  # 3. Determine Covariance Matrix & F-Stats based on Method
  #==========================#
  if(NeweyWest){
    # NW Covariance Matrix
    my_vcov <- NeweyWest(price_impact_reg, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
    
    # Robust F-Statistic
    f_test <- waldtest(price_impact_reg, vcov = my_vcov, test = "F")
    f_stat_val <- f_test$F[2]
    f_p_val    <- f_test$`Pr(>F)`[2]
    
  } else {
    # Standard OLS Covariance Matrix
    my_vcov <- vcov(price_impact_reg)
    
    # Standard F-Statistic (extracted from summary)
    f_sum <- summary(price_impact_reg)$fstatistic
    f_stat_val <- f_sum["value"]
    f_p_val    <- pf(f_sum["value"], f_sum["numdf"], f_sum["dendf"], lower.tail = FALSE)
  }
  
  #==========================#
  # 4. Extract Coefficients (Standard or Robust SEs)
  #==========================#
  # coeftest handles both standard (if vcov provided) and robust cases
  coefs_obj <- coeftest(price_impact_reg, vcov. = my_vcov)
  tidy_results <- broom::tidy(coefs_obj)
  
  #==========================#
  # 5. Structural Parameters (Lambda & Beta) with Delta Method
  #==========================#
  # We use the same 'my_vcov' (either NW or OLS) for consistency
  lambda_dm <- deltaMethod(price_impact_reg, 
                           g = paste0("-lag_q_t / ", phi_hat), 
                           vcov. = my_vcov)
  
  beta_dm   <- deltaMethod(price_impact_reg, 
                           g = paste0("q_t + (lag_q_t / ", phi_hat, ")"), 
                           vcov. = my_vcov)
  
  structural_results <- data.frame(
    term = c("Lambda", "Beta"),
    estimate = c(lambda_dm$Estimate[1], beta_dm$Estimate[1]),
    std.error = c(lambda_dm$SE[1], beta_dm$SE[1]),
    statistic = c(lambda_dm$Estimate[1]/lambda_dm$SE[1], beta_dm$Estimate[1]/beta_dm$SE[1])
  ) %>%
    mutate(p.value = 2 * (1 - pnorm(abs(statistic))))
  
  #==========================#
  # 6. Combine & Format Output
  #==========================#
  all_terms <- bind_rows(tidy_results, structural_results)
  
  # Prepare Model-Level Stats
  model_stats <- broom::glance(price_impact_reg) %>%
    dplyr::mutate(
      statistic = as.numeric(f_stat_val), # Use our calculated F-stat (Robust or OLS)
      p.value   = as.numeric(f_p_val)     # Use our calculated p-val
    ) %>%
    dplyr::rename(
      f.statistic = statistic, 
      f.p.value = p.value
    )
  
  # Create Final Dataframe
  final_df <- tidyr::crossing(all_terms, model_stats) %>%
    data.frame()
  
  # Custom Ordering
  custom_order <- c("(Intercept)", "delta_d_t", "lag_q_t", "q_t", "Beta", "Lambda")
  
  final_df <- final_df %>%
    mutate(term = factor(term, levels = custom_order)) %>%
    arrange(term) %>%
    mutate(term = as.character(term))
  
  return(final_df)
 
#==== END ===================================================================#
  
  
}