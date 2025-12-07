PriceImpact_Short <- function(df, lag_nw = 6) {
  
  ar1_model <- tryCatch(
    lm(q_t ~ lag_q_t, data = df),
    error = function(e) NULL
  )
  if(is.null(ar1_model)) return(NULL)
  
  phi_hat <- coef(ar1_model)["lag_q_t"]
  
  main_mod <- tryCatch(
    lm(delta_p ~ q_t + lag_q_t + delta_d_t, data = df),
    error = function(e) NULL
  )
  if(is.null(main_mod)) return(NULL)
  
  nw_vcov <- tryCatch(
    NeweyWest(main_mod, lag = lag_nw, prewhite = FALSE, adjust = TRUE),
    error = function(e) NULL
  )
  if(is.null(nw_vcov)) return(NULL)
  
  # Delta Method for Lambda and Beta
  lambda_dm <- tryCatch(
    deltaMethod(main_mod, g = paste0("-lag_q_t / ", phi_hat), vcov. = nw_vcov),
    error = function(e) NULL
  )
  
  beta_dm <- tryCatch(
    deltaMethod(main_mod, g = paste0("q_t + (lag_q_t / ", phi_hat, ")"), vcov. = nw_vcov),
    error = function(e) NULL
  )
  
  if(is.null(lambda_dm) || is.null(beta_dm)) return(NULL)
  
  tibble(
    term = c("Lambda", "Beta"),
    estimate = c(lambda_dm$Estimate, beta_dm$Estimate),
    std.error = c(lambda_dm$SE, beta_dm$SE),
    statistic = c(lambda_dm$Estimate/lambda_dm$SE, beta_dm$Estimate/beta_dm$SE)
  ) %>%
    mutate(p.value = 2 * (1 - pnorm(abs(statistic))))
}