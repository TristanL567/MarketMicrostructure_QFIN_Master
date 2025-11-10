#==============================================================================#
#==== 00 - Description ========================================================#
#==============================================================================#

#==============================================================================#
#==== 1 - Working Directory & Libraries =======================================#
#==============================================================================#

silent=F
.libPaths()

# Path <- "C:/Users/TristanLeiter/Documents/Privat/Market_Microstructure/04_Presentation/MarketMicrostructure_QFIN_Master"
Path <- file.path(here::here(""))

#==== 1A - Libraries ==========================================================#

## Needs to enable checking for install & if not then autoinstall.

packages <- c("dplyr", "tidyr", "lubridate",
              "ggplot2","patchwork",
              "purrr", "broom",
              "usethis",
              "PINstimation",
              "sandwich",    ## For Neway-West adjusted SE.
              "lmtest",
              "here"
              )

for(i in 1:length(packages)){
  package_name <- packages[i]
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, character.only = TRUE)
    cat(paste("Package '", package_name, "' was not installed. It has now been installed and loaded.\n", sep = ""))
  } else {
    cat(paste("Package '", package_name, "' is already installed and has been loaded.\n", sep = ""))
  }
  library(package_name, character.only = TRUE)
}

#==== 1B - Functions ==========================================================#

sourceFunctions <- function (functionDirectory)  {
  functionFiles <- list.files(path = functionDirectory, pattern = "*.R", 
                              full.names = T)
  ssource <- function(path) {
    try(source(path, echo = F, verbose = F, print.eval = T, 
               local = F))
  }
  sapply(functionFiles, ssource)
}


#==== 1C - Parameters =========================================================#

## Directories.
Data_Directory <- file.path(Path, "02_Data")
Charts_Directory <- file.path(Path, "03_Charts")
Functions_Directory <- file.path(Path, "01_Code/Functions")

## Load all code files in the functions directory.
sourceFunctions(Functions_Directory)

## Input Data Files.
Data_FOMC_Directory <- file.path(Data_Directory, "FOMC")
Data_Controls_Directory <- file.path(Data_Directory, "Controls")

# Output Charts Files.
Charts_FOMC_Directory <- file.path(Charts_Directory, "FOMC")
Charts_Controls_Directory <- file.path(Charts_Directory, "Controls")

## Get all files in the directories.
FOMC_files <- list.files(path = Data_FOMC_Directory, 
                         pattern = "\\.csv$", 
                         full.names = TRUE)

Controls_files <- list.files(path = Data_Controls_Directory, 
                             pattern = "\\.csv$", 
                             full.names = TRUE)

## Used dates.
FOMC_Dates <- c("2025-01-29", "2025-03-19", "2025-05-07",
                "2025-06-18", "2025-07-30", "2025-09-29",
                "2025-10-29")

Controls_Dates <- c("2025-02-04", "2025-03-27", "2025-05-20",
                    "2025-06-23", "2025-08-04", "2025-10-03",
                    "2025-11-05")

## Newey-West.

lag_NW <- 6

## Output.
Kyle_Regression_Output <- list()
Kyle_Regression_Output_All <- list()
Lambda_results_Output <- list()

Kyle_Regression_Output_Controls_All <- list()
Lambda_results_Controls_Output <- list()

## Plotting.
blue <- "#004890"
grey <- "#708090"
orange <- "#F37021"
red <- "#B22222"

height <- 3750
width <- 1833

#==== 1D - git ================================================================#

usethis::use_git_ignore(c(
  "FOMC_20251029_DataSummary.xlsx",
  "Quote_20251029.csv",
  "Quote_Minutes_20251029.csv",
  "Trade_20251029.csv",
  "Trade_Minutes_20251029.csv"
))

#==============================================================================#
#==== 02 - FOMC ===============================================================#
#==============================================================================#

for(file in 1:length(FOMC_Dates)){
  
tryCatch({

#==== 02a - Read-in Data ======================================================#

Date_used <- FOMC_Dates[file]
Date_used <- gsub("-", "", Date_used)

matching_files <- grep(pattern = Date_used, 
                       x = FOMC_files, 
                       value = TRUE)
## Get the right paths.
Trades_path <- grep(pattern = "Trades", 
                    x = matching_files, 
                    value = TRUE)
Quotes_path <- grep(pattern = "Quotes", 
                    x = matching_files, 
                    value = TRUE)

## Load the data.
Trade <- read.csv(Trades_path)
Quote <- read.csv(Quotes_path)

#==== 02b - Data Manipulation =================================================#

Trade_clean <- Trade %>%
  mutate(
    datetime = as.POSIXct(paste(DATE, TIME_M), format = "%Y-%m-%d %H:%M:%S"),
    is_trade = TRUE
  ) %>%
  select(datetime, PRICE, SIZE, is_trade)

Quote_clean <- Quote %>%
  mutate(
    datetime = as.POSIXct(paste(DATE, TIME_M), format = "%Y-%m-%d %H:%M:%S"),
    is_trade = FALSE
  ) %>%
  filter(BID > 0, ASK > 0, BIDSIZ > 0, ASKSIZ > 0) %>%
  select(datetime, BID, ASK, is_trade)

## Now merge both with tidyr.
Data <- bind_rows(Trade_clean, Quote_clean) %>%
  arrange(datetime) %>%
  fill(BID, ASK, .direction = "down") %>%
  filter(is_trade == TRUE) %>%
  select(-is_trade)
Data <- Data %>%
  na.omit()

## Apply the Lee-ready algorithm to get the trade direction.
Data_adj <- Lee_Ready_Algo(Data)

## Prepare for the regressions.
regression_data <- Data_adj %>%
  mutate(q_t = d_t * SIZE) %>%
  mutate(delta_p = MIDQUOTE - lag(MIDQUOTE)) %>%
  mutate(delta_d_t = d_t - lag(d_t))

regression_data <- regression_data %>%
  filter(d_t != 0)
regression_data <- regression_data %>%
  drop_na(delta_p, delta_d_t)

#==== 02c - Kyle-Regression for the whole time period =========================#
# The formula is:
# delta_p ~ d_t + q_t + delta_d_t
#
# Where:
# - 'delta_p' is the change in the midquote (your dependent variable)
# - 'd_t' estimates λ₀ (fixed impact of direction)
# - 'q_t' estimates λ₁ (Kyle's Lambda, the impact of signed size)
# - 'delta_d_t' estimates γ (the transient, non-info cost component)

## From 13:00:00 to 16:00:00

kyle_model_whole_period <- lm(
  delta_p ~ d_t + q_t + delta_d_t, 
  data = regression_data
)

nw_vcov <- NeweyWest(kyle_model_whole_period, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
coefs <- coeftest(kyle_model_whole_period, vcov. = nw_vcov)
tidy_results <- broom::tidy(coefs)

alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
model_stats <- broom::glance(kyle_model_whole_period) %>%
  dplyr::rename(
    f.statistic = statistic, 
    f.p.value = p.value
  )

combined_summary <- tidyr::crossing(tidy_results, model_stats)
combined_summary <- data.frame(combined_summary)
####

Kyle_Regression_Output[[1]] <- kyle_model_whole_period
Kyle_Regression_Output[[2]] <- combined_summary

#==== 02d - Kyle-Regression for the subperiods ================================#
## Period 1: 13:00:00 to 14:25:00

regression_data_filtered <- regression_data %>%
  filter(format(datetime, "%H:%M:%S") < "14:25:00")

kyle_model_period_1 <- lm(
  delta_p ~ d_t + q_t + delta_d_t, 
  data = regression_data_filtered
)

nw_vcov <- NeweyWest(kyle_model_period_1, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
coefs <- coeftest(kyle_model_period_1, vcov. = nw_vcov)
tidy_results <- broom::tidy(coefs)

alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
model_stats <- broom::glance(kyle_model_period_1) %>%
  dplyr::rename(
    f.statistic = statistic, 
    f.p.value = p.value
  )

combined_summary <- tidyr::crossing(tidy_results, model_stats)
combined_summary <- data.frame(combined_summary)

Kyle_Regression_Output[[3]] <- kyle_model_period_1
Kyle_Regression_Output[[4]] <- combined_summary

## Period 2: 14:25:00 to 16:00:00

regression_data_filtered <- regression_data %>%
  filter(format(datetime, "%H:%M:%S") >= "14:25:00")

kyle_model_period_2 <- lm(
  delta_p ~ d_t + q_t + delta_d_t, 
  data = regression_data_filtered
)

nw_vcov <- NeweyWest(kyle_model_period_2, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
coefs <- coeftest(kyle_model_period_2, vcov. = nw_vcov)
tidy_results <- broom::tidy(coefs)

alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
model_stats <- broom::glance(kyle_model_period_2) %>%
  dplyr::rename(
    f.statistic = statistic, 
    f.p.value = p.value
  )

combined_summary <- tidyr::crossing(tidy_results, model_stats)
combined_summary <- data.frame(combined_summary)

Kyle_Regression_Output[[5]] <- kyle_model_period_2
Kyle_Regression_Output[[6]] <- combined_summary

#==== 02e - Kyle-Regression for the constrained (short) subperiods ============#
## Run the rolling regression for the short subperiods.

tryCatch({
  
  lambda_over_time_nw <- regression_data %>%
    mutate(window_start = floor_date(datetime, "2 minutes")) %>%
    group_by(window_start) %>%
    nest() %>%
        mutate(model = map(data, ~ lm(delta_p ~ d_t + q_t + delta_d_t, data = .x))) %>%
        mutate(nw_tidied = map(model, ~ {
            nw_vcov <- sandwich::NeweyWest(.x, 
                                     lag = lag_NW, 
                                     prewhite = FALSE, 
                                     adjust = TRUE)
            nw_coefs <- lmtest::coeftest(.x, vcov. = nw_vcov)
            broom::tidy(nw_coefs)
    }))
  
  lambda_results_nw <- lambda_over_time_nw %>%
    unnest(nw_tidied) %>% 
    filter(term == "q_t") %>% 
    select(window_start, estimate, p.value) %>%
    ungroup()

}, silent = TRUE)

## Plot the price impact.
avg_price <- mean(regression_data$PRICE)
shares_in_10k_trade <- 10000 / avg_price
lambda_results <- lambda_results_nw %>%
  mutate(significant = ifelse(p.value < 0.05, "Yes", "No"))

Plot <- ggplot(lambda_results, aes(x = window_start, y = estimate * shares_in_10k_trade)) +
  geom_line() +
  geom_point(aes(color = significant)) +
  scale_color_manual(
    values = c("Yes" = "red", "No" = "grey"),
    name = "Significant (p < 0.05)" # Added a clearer legend title
  ) +
  labs(
    title = "Price Impact of a $10,000 Trade",
    y = "Price Impact (in Dollars)",
    x = "Time"
  ) +
  theme_minimal() +  # Using your requested theme
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) # Using your requested axis text rotation
  )

Path <- paste(Charts_FOMC_Directory, "/01_Price_Impact_",Date_used, ".png", sep = "")
ggsave(
  filename = Path,
  plot = Plot,
  width = height,
  height = width,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

#==== 02f - Output & Returning ================================================#

names(Kyle_Regression_Output) <- c("Full period", "Full Period (NW)",
                                   "Subperiod 1", "Subperiod 1 (NW)",
                                   "Subperiod 2", "Subperiod 2 (NW)")
Kyle_Regression_Output_All[[file]] <- Kyle_Regression_Output

Lambda_results_Output[[file]] <- lambda_results

## End of the TryCatch Statement.
    }, silent = TRUE)
## End of the LOOP.
}

##

names(Kyle_Regression_Output_All) <- FOMC_Dates
names(Lambda_results_Output) <- FOMC_Dates

#==============================================================================#
#==== 03 - Controls ===========================================================#
#==============================================================================#

for(file in 1:length(Controls_Dates)){
  
  tryCatch({
    
#==== 03a - Read-in Data ======================================================#
    
Date_used <- Controls_Dates[file]
Date_used <- gsub("-", "", Date_used)
    
matching_files <- grep(pattern = Date_used, 
                           x = Controls_files, 
                           value = TRUE)
## Get the right paths.
    Trades_path <- grep(pattern = "Trades", 
                        x = matching_files, 
                        value = TRUE)
    Quotes_path <- grep(pattern = "Quotes", 
                        x = matching_files, 
                        value = TRUE)
    
    ## Load the data.
    Trade <- read.csv(Trades_path)
    Quote <- read.csv(Quotes_path)
    
#==== 03b - Data Manipulation =================================================#
    
    Trade_clean <- Trade %>%
      mutate(
        datetime = as.POSIXct(paste(DATE, TIME_M), format = "%Y-%m-%d %H:%M:%S"),
        is_trade = TRUE
      ) %>%
      select(datetime, PRICE, SIZE, is_trade)
    
    Quote_clean <- Quote %>%
      mutate(
        datetime = as.POSIXct(paste(DATE, TIME_M), format = "%Y-%m-%d %H:%M:%S"),
        is_trade = FALSE
      ) %>%
      filter(BID > 0, ASK > 0, BIDSIZ > 0, ASKSIZ > 0) %>%
      select(datetime, BID, ASK, is_trade)
    
    ## Now merge both with tidyr.
    Data <- bind_rows(Trade_clean, Quote_clean) %>%
      arrange(datetime) %>%
      fill(BID, ASK, .direction = "down") %>%
      filter(is_trade == TRUE) %>%
      select(-is_trade)
    Data <- Data %>%
      na.omit()
    
    ## Apply the Lee-ready algorithm to get the trade direction.
    Data_adj <- Lee_Ready_Algo(Data)
    
    ## Prepare for the regressions.
    regression_data <- Data_adj %>%
      mutate(q_t = d_t * SIZE) %>%
      mutate(delta_p = MIDQUOTE - lag(MIDQUOTE)) %>%
      mutate(delta_d_t = d_t - lag(d_t))
    
    regression_data <- regression_data %>%
      filter(d_t != 0)
    regression_data <- regression_data %>%
      drop_na(delta_p, delta_d_t)
    
#==== 03c - Kyle-Regression for the whole time period =========================#
    # The formula is:
    # delta_p ~ d_t + q_t + delta_d_t
    #
    # Where:
    # - 'delta_p' is the change in the midquote (your dependent variable)
    # - 'd_t' estimates λ₀ (fixed impact of direction)
    # - 'q_t' estimates λ₁ (Kyle's Lambda, the impact of signed size)
    # - 'delta_d_t' estimates γ (the transient, non-info cost component)
    
    ## From 13:00:00 to 16:00:00
    
kyle_model_whole_period <- lm(
      delta_p ~ d_t + q_t + delta_d_t, 
      data = regression_data
)
    
nw_vcov <- NeweyWest(kyle_model_whole_period, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
coefs <- coeftest(kyle_model_whole_period, vcov. = nw_vcov)
tidy_results <- broom::tidy(coefs)

alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
model_stats <- broom::glance(kyle_model_whole_period) %>%
  dplyr::rename(
    f.statistic = statistic, 
    f.p.value = p.value
  )

combined_summary <- tidyr::crossing(tidy_results, model_stats)
combined_summary <- data.frame(combined_summary)
####

Kyle_Regression_Output[[1]] <- kyle_model_whole_period
Kyle_Regression_Output[[2]] <- combined_summary
    
#==== 03d - Kyle-Regression for the subperiods ================================#
## Period 1: 13:00:00 to 14:25:00
    
regression_data_filtered <- regression_data %>%
      filter(format(datetime, "%H:%M:%S") < "14:25:00")
    
kyle_model_period_1 <- lm(
      delta_p ~ d_t + q_t + delta_d_t, 
      data = regression_data_filtered
)
    
    nw_vcov <- NeweyWest(kyle_model_period_1, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
    coefs <- coeftest(kyle_model_period_1, vcov. = nw_vcov)
    tidy_results <- broom::tidy(coefs)
    
    alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
    t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
    model_stats <- broom::glance(kyle_model_period_1) %>%
      dplyr::rename(
        f.statistic = statistic, 
        f.p.value = p.value
      )
    
    combined_summary <- tidyr::crossing(tidy_results, model_stats)
    combined_summary <- data.frame(combined_summary)
    
    Kyle_Regression_Output[[3]] <- kyle_model_period_1
    Kyle_Regression_Output[[4]] <- combined_summary
    
## Period 2: 14:25:00 to 16:00:00
    
    regression_data_filtered <- regression_data %>%
      filter(format(datetime, "%H:%M:%S") >= "14:25:00")
    
    kyle_model_period_2 <- lm(
      delta_p ~ d_t + q_t + delta_d_t, 
      data = regression_data_filtered
    )
    
    nw_vcov <- NeweyWest(kyle_model_period_2, lag = lag_NW, prewhite = FALSE, adjust = TRUE)
    coefs <- coeftest(kyle_model_period_2, vcov. = nw_vcov)
    tidy_results <- broom::tidy(coefs)
    
    alpha <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(estimate)
    t_stat <- tidy_results %>% dplyr::filter(term == "(Intercept)") %>% pull(statistic)
    model_stats <- broom::glance(kyle_model_period_2) %>%
      dplyr::rename(
        f.statistic = statistic, 
        f.p.value = p.value
      )
    
    combined_summary <- tidyr::crossing(tidy_results, model_stats)
    combined_summary <- data.frame(combined_summary)
    
    Kyle_Regression_Output[[5]] <- kyle_model_period_2
    Kyle_Regression_Output[[6]] <- combined_summary
    
#==== 03e - Kyle-Regression for the constrained (short) subperiods ============#
## Run the rolling regression for the short subperiods.
    
tryCatch({
      
      lambda_over_time_nw <- regression_data %>%
        mutate(window_start = floor_date(datetime, "2 minutes")) %>%
        group_by(window_start) %>%
        nest() %>%
        mutate(model = map(data, ~ lm(delta_p ~ d_t + q_t + delta_d_t, data = .x))) %>%
        mutate(nw_tidied = map(model, ~ {
          nw_vcov <- sandwich::NeweyWest(.x, 
                                         lag = lag_NW, 
                                         prewhite = FALSE, 
                                         adjust = TRUE)
          nw_coefs <- lmtest::coeftest(.x, vcov. = nw_vcov)
          broom::tidy(nw_coefs)
        }))
      
      lambda_results_nw <- lambda_over_time_nw %>%
        unnest(nw_tidied) %>% 
        filter(term == "q_t") %>% 
        select(window_start, estimate, p.value) %>%
        ungroup()
      
    }, silent = TRUE)
    
## Plot the price impact.
    avg_price <- mean(regression_data$PRICE)
    shares_in_10k_trade <- 10000 / avg_price
    lambda_results <- lambda_results_nw %>%
      mutate(significant = ifelse(p.value < 0.05, "Yes", "No"))
    
    Plot <- ggplot(lambda_results, aes(x = window_start, y = estimate * shares_in_10k_trade)) +
      geom_line() +
      geom_point(aes(color = significant)) +
      scale_color_manual(
        values = c("Yes" = "red", "No" = "grey"),
        name = "Significant (p < 0.05)" # Added a clearer legend title
      ) +
      labs(
        title = "Price Impact of a $10,000 Trade",
        y = "Price Impact (in Dollars)",
        x = "Time"
      ) +
      theme_minimal() +  # Using your requested theme
      theme(
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) # Using your requested axis text rotation
      )
    
    Path <- paste(Charts_Controls_Directory, "/01_Price_Impact_",Date_used, ".png", sep = "")
    ggsave(
      filename = Path,
      plot = Plot,
      width = height,
      height = width,
      units = "px",
      dpi = 300,
      limitsize = FALSE
    )
    
#==== 03f - Output & Returning ================================================#
    
names(Kyle_Regression_Output) <- c("Full period", "Full Period (NW)",
                                       "Subperiod 1", "Subperiod 1 (NW)",
                                       "Subperiod 2", "Subperiod 2 (NW)")
Kyle_Regression_Output_Controls_All[[file]] <- Kyle_Regression_Output
    
Lambda_results_Controls_Output[[file]] <- lambda_results
    
## End of the TryCatch Statement.
  }, silent = TRUE)
## End of the LOOP.
}

##

names(Kyle_Regression_Output_Controls_All) <- Controls_Dates
names(Lambda_results_Controls_Output) <- Controls_Dates

## Save all the output to an .RDA file.

## ""

#==============================================================================#
#==== 010 - Appendix (ALL CODE BELOW IS CURRENTLY NOT ACTIVE) =================#
#==============================================================================#




#==== 02f - Charts & Visualisation ============================================#
## Price jump by midquote (currently not used).

tryCatch({
  
  # plot_data_jump <- regression_data %>%
  #     filter(format(datetime, "%H:%M:%S") >= "14:29:59" &
  #              format(datetime, "%H:%M:%S") < "14:30:10")
  # 
  # plot_data_jump <- regression_data %>%
  #   filter(format(datetime, "%H:%M:%S") >= "13:59:55" &
  #            format(datetime, "%H:%M:%S") < "14:00:10")
  # 
  # ggplot(plot_data_jump, aes(x = datetime, y = MIDQUOTE)) +
  #   geom_line(color = "blue", size = 1) +
  #     geom_vline(xintercept = as.POSIXct("2025-10-29 14:00:00.000"),
  #              linetype = "dashed",
  #              color = "red",
  #              size = 1) +
  # 
  #   ggtitle("Price Discovery: Instant Midquote Jump at 14:00:00") +
  #   xlab("Time (with Milliseconds)") +
  #   ylab("SPY Midquote Price ($)") +
  # 
  #   # Format the x-axis to show milliseconds
  #   scale_x_datetime(date_labels = "%H:%M:%OS3") +
  #   theme_minimal()
  
  
}, silent = TRUE)

## Bid-Ask spread (currently not used)

tryCatch({
  
  # plot_data_jump <- regression_data %>%
  #     filter(format(datetime, "%H:%M:%S") >= "14:29:59" &
  #              format(datetime, "%H:%M:%S") < "14:30:10")
  # 
  # plot_data_freeze <- plot_data_jump %>%
  #   pivot_longer(
  #     cols = c("BID", "ASK"),
  #     names_to = "Quote_Type",
  #     values_to = "Quote_Price"
  #   )
  # 
  # Plot <- ggplot(plot_data_freeze, aes(x = datetime)) +
  #   geom_line(aes(y = Quote_Price, color = Quote_Type), size = 1.2) +
  #   geom_point(aes(y = PRICE, shape = "Trade Price"), color = "black", size = 2.5) +
  #   geom_vline(xintercept = as.POSIXct("2025-10-29 14:00:00.000"), 
  #              linetype = "dashed", 
  #              color = "red", 
  #              size = 1) +
  #   ggtitle("Market 'Freeze': Bid-Ask Spread Explodes at 14:00") +
  #   xlab("Time (with Milliseconds)") +
  #   ylab("Price ($)") +
  #   scale_x_datetime(date_labels = "%H:%M:%OS3") +
  #   scale_color_manual(name = "Quotes",
  #                      values = c("BID" = "green", "ASK" = "orange")) +
  #   scale_shape_manual(name = "", 
  #                      values = c("Trade Price" = 16)) + # 16 is a solid circle
  #   theme_minimal() +
  #   theme(legend.position = "bottom")
  
}, silent = TRUE)

#==============================================================================#
#==== 03 - PIN | VPIN measure (for the FOMC data) =============================#
#==============================================================================#

#==== 03a - VPIN measure implementation =======================================#
## Using PINstimation.
tryCatch({
#   
# hft_data <- data.frame(
#   timestamp = Trade_clean$datetime,
#   price = Trade_clean$PRICE,
#   volume = Trade_clean$SIZE
# )
# 
# # We'll start with an initial "burn-in" period to get the first calculation.
# # Let's use the first 5 minutes of data as our starting point.
# initial_burn_in_time <- hft_data$timestamp[1] + (30 * 60) # 5 minutes * 60 seconds
# initial_rows <- which(hft_data$timestamp >= initial_burn_in_time)[1]
# 
# # If the dataset is shorter than 5 minutes, we'll adjust.
# if (is.na(initial_rows)) {
#   initial_rows <- nrow(hft_data)
# }
# 
# chunk_size <- 5000 # Process 5000 new trades per update
# 
# vpin_updates_minutes <- list()
# 
# for (i in seq(from = initial_rows, to = nrow(hft_data), by = chunk_size)) {
#     current_data <- hft_data[1:i, ]
#   vpin_results <- vpin(current_data, verbose = FALSE)
#     if (nrow(vpin_results@bucketdata) > 0) {
#     
#     latest_vpin <- tail(vpin_results@bucketdata$vpin, 1)
#     latest_timestamp <- tail(vpin_results@bucketdata$endtime, 1)
#     
#     cat(sprintf("Time: %s  |  Trades Processed: %d  |  Latest VPIN: %.4f\n",
#                 format(latest_timestamp, "%H:%M:%S"),
#                 i,
#                 latest_vpin))
#     
#     update_key <- format(latest_timestamp, "%Y-%m-%d %H:%M:%S")
#     vpin_updates_minutes[[update_key]] <- latest_vpin
#   }
# }
# 
# cat("--------------------------------------\n")
# cat("...Simulation finished.\n")
# 
# 
# if (length(vpin_updates_minutes) > 0) {
#   vpin_minutes <- data.frame(
#     timestamp = as.POSIXct(names(vpin_updates_minutes)),
#     vpin = unlist(vpin_updates_minutes)
#   )
#   
#   plot(vpin_minutes$timestamp, vpin_minutes$vpin, type = 'l', col = "blue",
#        xlab = "Time", ylab = "VPIN Estimate",
#        main = "Simulated Continuous VPIN Updates for Trade_Minutes_clean")
#   grid()
# } else {
#   cat("Not enough data to generate VPIN updates. Try with a larger dataset or smaller vpin() parameters.\n")
# }
# 
}, silent = TRUE)

#==== 06b - Visualisation =====================================================#

tryCatch({
  
# if (!is.null(vpin_minutes) && !is.null(vpin_speech)) {
#   
#   cat("\nGenerating comparative plot...\n")
#   
#   # Plot 1: VPIN for the first dataset
#   p1 <- ggplot(vpin_minutes, aes(x = timestamp, y = vpin)) +
#     geom_line(color = "dodgerblue", size = 1) +
#     labs(
#       title = "VPIN Evolution (13:55 - 14:25)",
#       subtitle = "Standard trading period",
#       x = "Time",
#       y = "VPIN Estimate"
#     ) +
#     theme_minimal() +
#     theme(plot.title = element_text(face = "bold"))
#   
#   # Plot 2: VPIN for the speech dataset
#   p2 <- ggplot(vpin_speech, aes(x = timestamp, y = vpin)) +
#     geom_line(color = "firebrick", size = 1) +
#     labs(
#       title = "VPIN Evolution (14:25 - 14:55)",
#       subtitle = "Trading period during speech",
#       x = "Time",
#       y = "VPIN Estimate"
#     ) +
#     theme_minimal() +
#     theme(plot.title = element_text(face = "bold"))
#   
#   # Combine the two plots vertically using patchwork
#   combined_plot <- p1 | p2
#   
#   # Display the combined plot
#   print(combined_plot)
#   
#   Path <- file.path(Charts_Directory, "03_VPN_Combined_Plot.png")
#   ggsave(
#     filename = Path,
#     plot = combined_plot,
#     width = 3750,
#     height = 1833,
#     units = "px",
#     dpi = 300,
#     limitsize = FALSE
#   )
#   
# } else {
#   cat("\nCould not generate the comparative plot because one or both simulations failed to produce data.\n")
# }

}, silent = TRUE)

#==============================================================================#
#==============================================================================#
#==============================================================================#