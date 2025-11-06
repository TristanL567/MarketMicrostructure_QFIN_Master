#==============================================================================#
#==== 00 - Description ========================================================#
#==============================================================================#

#==============================================================================#
#==== 1 - Working Directory & Libraries =======================================#
#==============================================================================#

silent=F
.libPaths()

Path <- "C:/Users/TristanLeiter/Documents/Privat/Market_Microstructure/04_Presentation/MarketMicrostructure_QFIN_Master"

#==== 1A - Libraries ==========================================================#

## Needs to enable checking for install & if not then autoinstall.

packages <- c("dplyr", "tidyr", "lubridate",
              "ggplot2","patchwork",
              "purrr", "broom",
              "usethis",
              "PINstimation")

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



#==== 1C - Parameters =========================================================#

## Directories.
Data_Directory <- file.path(Path, "02_Data")
Charts_Directory <- file.path(Path, "03_Charts")

## Input Data Files.
Trade_Data_Directory <- file.path(Data_Directory, "Trade_20251029.csv")
Trade_Minutes_Data_Directory <- file.path(Data_Directory, "Trade_Minutes_20251029.csv")

Quote_Data_Directory <- file.path(Data_Directory, "Quote_20251029.csv")
Quote_Minutes_Data_Directory <- file.path(Data_Directory, "Quote_Minutes_20251029.csv")

#==== 1D - git ================================================================#

usethis::use_git_ignore(c(
  "FOMC_20251029_DataSummary.xlsx",
  "Quote_20251029.csv",
  "Quote_Minutes_20251029.csv",
  "Trade_20251029.csv",
  "Trade_Minutes_20251029.csv"
))

#==============================================================================#
#==== 02 - Data ===============================================================#
#==============================================================================#

#==== 02a - Read-in Data ======================================================#

Trade <- read.csv(Trade_Data_Directory)
Quote <- read.csv(Quote_Data_Directory)

Trade_Minutes <- read.csv(Trade_Minutes_Data_Directory)
Quote_Minutes <- read.csv(Quote_Minutes_Data_Directory)

#==== 02b - Data Manipulation =================================================#

tryCatch({
  
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

## Same for the Minutes.
Trade_Minutes_clean <- Trade_Minutes %>%
  mutate(
    datetime = as.POSIXct(paste(DATE, TIME_M), format = "%Y-%m-%d %H:%M:%S"),
    is_trade = TRUE
  ) %>%
  select(datetime, PRICE, SIZE, is_trade)

Quote_Minutes_clean <- Quote_Minutes %>%
  mutate(
    datetime = as.POSIXct(paste(DATE, TIME_M), format = "%Y-%m-%d %H:%M:%S"),
    is_trade = FALSE
  ) %>%
  filter(BID > 0, ASK > 0, BIDSIZ > 0, ASKSIZ > 0) %>%
  select(datetime, BID, ASK, is_trade)

## Now merge both with tidyr.
merged_data <- bind_rows(Trade_clean, Quote_clean) %>%
  arrange(datetime) %>%
    fill(BID, ASK, .direction = "down") %>%
    filter(is_trade == TRUE) %>%
    select(-is_trade)
merged_data <- merged_data %>%
  na.omit()

## Same for minutes.
merged_data_minutes <- bind_rows(Trade_Minutes_clean, Quote_Minutes_clean) %>%
  arrange(datetime) %>%
  fill(BID, ASK, .direction = "down") %>%
  filter(is_trade == TRUE) %>%
  select(-is_trade)
merged_data_minutes <- merged_data_minutes %>%
  na.omit()

}, silent = TRUE)

#==== 02c - Lee-Ready Algorithm - estimate the trade direction ================#

tryCatch({
  
merged_data <- merged_data %>%
  arrange(datetime) %>%
    mutate(
    MIDQUOTE = (BID + ASK) / 2,
    prev_PRICE = lag(PRICE) # Get previous trade price
  ) %>%
    mutate(
    d_t = case_when(
      PRICE > MIDQUOTE ~ 1,   # 1. Quote Rule: Price > Midquote -> Buy
      PRICE < MIDQUOTE ~ -1,  # 2. Quote Rule: Price < Midquote -> Sell
      PRICE > prev_PRICE ~ 1,   # 3. Tick Test: Price > Prev Price -> Buy
      PRICE < prev_PRICE ~ -1,  # 4. Tick Test: Price < Prev Price -> Sell
      TRUE ~ 0                  # Unclassifiable (e.g., zero-tick)
    )
  )

## Same for the minutes.
merged_data_minutes <- merged_data_minutes %>%
  arrange(datetime) %>%
  mutate(
    MIDQUOTE = (BID + ASK) / 2,
    prev_PRICE = lag(PRICE) # Get previous trade price
  ) %>%
  mutate(
    d_t = case_when(
      PRICE > MIDQUOTE ~ 1,   # 1. Quote Rule: Price > Midquote -> Buy
      PRICE < MIDQUOTE ~ -1,  # 2. Quote Rule: Price < Midquote -> Sell
      PRICE > prev_PRICE ~ 1,   # 3. Tick Test: Price > Prev Price -> Buy
      PRICE < prev_PRICE ~ -1,  # 4. Tick Test: Price < Prev Price -> Sell
      TRUE ~ 0                  # Unclassifiable (e.g., zero-tick)
    )
  )

}, silent = TRUE)

#==============================================================================#
#==== 03 - Analysis ===========================================================#
#==============================================================================#
## In this part we prepare the data and run the regression utilizing the Kyle model.

#==== 03a - Regression Preparation ============================================#

tryCatch({
  
regression_data <- merged_data %>%
  mutate(q_t = d_t * SIZE) %>%
  mutate(delta_p = MIDQUOTE - lag(MIDQUOTE)) %>%
    mutate(delta_d_t = d_t - lag(d_t))

regression_data <- regression_data %>%
  filter(d_t != 0)
regression_data <- regression_data %>%
  drop_na(delta_p, delta_d_t)
print(head(regression_data))

## Same for minutes.
regression_data_minutes <- merged_data_minutes %>%
  mutate(q_t = d_t * SIZE) %>%
  mutate(delta_p = MIDQUOTE - lag(MIDQUOTE)) %>%
  mutate(delta_d_t = d_t - lag(d_t))

regression_data_minutes <- regression_data_minutes %>%
  filter(d_t != 0)
regression_data_minutes <- regression_data_minutes %>%
  drop_na(delta_p, delta_d_t)
print(head(regression_data_minutes))

}, silent = TRUE)

#==== 03b - Run the regression analysis =======================================#
# The formula is:
# delta_p ~ d_t + q_t + delta_d_t
#
# Where:
# - 'delta_p' is the change in the midquote (your dependent variable)
# - 'd_t' estimates λ₀ (fixed impact of direction)
# - 'q_t' estimates λ₁ (Kyle's Lambda, the impact of signed size)
# - 'delta_d_t' estimates γ (the transient, non-info cost component)

price_impact_model <- lm(
  delta_p ~ d_t + q_t + delta_d_t, 
  data = regression_data
)

## Same for minutes.
price_impact_model_minutes <- lm(
  delta_p ~ d_t + q_t + delta_d_t, 
  data = regression_data_minutes
)

## Output.
print(summary(price_impact_model))

## The signed size of the trade, q_t, had no measureable impact in this period.
## Strong transient effect (gamma). Negative coefficient implies a price reversal.
## -> If a buy follows a sell, the price tends to dip. 

## Output for Minutes.
print(summary(price_impact_model_minutes))

#==============================================================================#
#==== 04 - Visualisation ======================================================#
#==============================================================================#

#==== 04a - Plot: Price Jump (by Midquote) ====================================#

tryCatch({
  
# start_zoom <- as.POSIXct("2025-10-29 13:59:59.000")
start_zoom <- as.POSIXct("2025-10-29 14:29:59.000")

# end_zoom   <- as.POSIXct("2025-10-29 14:00:10.000")
end_zoom   <- as.POSIXct("2025-10-29 14:30:10.000")

# plot_data_jump <- regression_data_minutes %>%
#   filter(datetime >= start_zoom & datetime <= end_zoom)

plot_data_jump <- regression_data %>%
  filter(datetime >= start_zoom & datetime <= end_zoom)

ggplot(plot_data_jump, aes(x = datetime, y = MIDQUOTE)) +
  geom_line(color = "blue", size = 1) +
    geom_vline(xintercept = as.POSIXct("2025-10-29 14:00:00.000"), 
             linetype = "dashed", 
             color = "red", 
             size = 1) +
  
  ggtitle("Price Discovery: Instant Midquote Jump at 14:00:00") +
  xlab("Time (with Milliseconds)") +
  ylab("SPY Midquote Price ($)") +
  
  # Format the x-axis to show milliseconds
  scale_x_datetime(date_labels = "%H:%M:%OS3") + 
  theme_minimal()

## We see an instant jump in the mid-price.

}, silent = TRUE)

#==== 04b - Plot: Trades follow the discovered Price ==========================#

tryCatch({
  
ggplot(plot_data_jump, aes(x = datetime)) +
  geom_line(aes(y = MIDQUOTE, color = "Midquote"), size = 1.2) +
    geom_point(aes(y = PRICE, color = "Trade Price"), size = 2) +
    geom_vline(xintercept = as.POSIXct("2025-10-29 14:00:00.000"), 
             linetype = "dashed", 
             color = "red", 
             size = 1) +
  ggtitle("Trades Follow the Price (14:00 Event)") +
  xlab("Time (with Milliseconds)") +
  ylab("Price ($)") +
    scale_x_datetime(date_labels = "%H:%M:%OS3") +
    scale_color_manual(name = "Price Type",
                     values = c("Midquote" = "blue", "Trade Price" = "black")) +
  theme_minimal() +
  theme(legend.position = "bottom")

}, silent = TRUE)

#==== 04c - Plot: Bid-Ask =====================================================#

tryCatch({
  
plot_data_freeze <- regression_data %>%
  filter(datetime >= start_zoom & datetime <= end_zoom) %>%
    pivot_longer(
    cols = c("BID", "ASK"),
    names_to = "Quote_Type",
    values_to = "Quote_Price"
  )

ggplot(plot_data_freeze, aes(x = datetime)) +
    geom_line(aes(y = Quote_Price, color = Quote_Type), size = 1.2) +
  geom_point(aes(y = PRICE, shape = "Trade Price"), color = "black", size = 2.5) +
    geom_vline(xintercept = as.POSIXct("2025-10-29 14:00:00.000"), 
             linetype = "dashed", 
             color = "red", 
             size = 1) +
  ggtitle("Market 'Freeze': Bid-Ask Spread Explodes at 14:00") +
  xlab("Time (with Milliseconds)") +
  ylab("Price ($)") +
    scale_x_datetime(date_labels = "%H:%M:%OS3") +
    scale_color_manual(name = "Quotes",
                     values = c("BID" = "green", "ASK" = "orange")) +
    scale_shape_manual(name = "", 
                     values = c("Trade Price" = 16)) + # 16 is a solid circle
  theme_minimal() +
  theme(legend.position = "bottom")

}, silent = TRUE)

#==============================================================================#
#==== 05 - Extended Regression ================================================#
#==============================================================================#

#==== 05a - Extended Regression ===============================================#

tryCatch({
  
press_conf_start <- as.POSIXct("2025-10-29 13:55:00")
press_conf_end   <- as.POSIXct("2025-10-29 14:25:00") # 30 min window

lambda_over_time <- regression_data_minutes %>%
  filter(datetime >= press_conf_start & datetime < press_conf_end) %>%
    mutate(window_start = floor_date(datetime, "1 minutes")) %>%
    group_by(window_start) %>%
    nest() %>%
    mutate(model = map(data, ~ lm(delta_p ~ d_t + q_t + delta_d_t, data = .x))) %>%
    mutate(tidied = map(model, tidy))
lambda_results <- lambda_over_time %>%
  unnest(tidied) %>%                 
  filter(term == "q_t") %>%         
  select(window_start, estimate, p.value) 

print(lambda_results)

}, silent = TRUE)

#==============================================================================#
#==== 06 - PIN | VPIN measure. ================================================#
#==============================================================================#

#==== 06a - VPIN measure implementation =======================================#
## Using PINstimation.

## For the minutes dataset (14:25 to 14:55).
tryCatch({
  
hft_data <- data.frame(
  timestamp = Trade_Minutes_clean$datetime,
  price = Trade_Minutes_clean$PRICE,
  volume = Trade_Minutes_clean$SIZE
)

# We'll start with an initial "burn-in" period to get the first calculation.
# Let's use the first 5 minutes of data as our starting point.
initial_burn_in_time <- hft_data$timestamp[1] + (5 * 60) # 5 minutes * 60 seconds
initial_rows <- which(hft_data$timestamp >= initial_burn_in_time)[1]

# If the dataset is shorter than 5 minutes, we'll adjust.
if (is.na(initial_rows)) {
  initial_rows <- nrow(hft_data)
}

chunk_size <- 1000 # Process 1000 new trades per update

vpin_updates_minutes <- list()

for (i in seq(from = initial_rows, to = nrow(hft_data), by = chunk_size)) {
    current_data <- hft_data[1:i, ]
  vpin_results <- vpin(current_data, verbose = FALSE)
    if (nrow(vpin_results@bucketdata) > 0) {
    
    latest_vpin <- tail(vpin_results@bucketdata$vpin, 1)
    latest_timestamp <- tail(vpin_results@bucketdata$endtime, 1)
    
    cat(sprintf("Time: %s  |  Trades Processed: %d  |  Latest VPIN: %.4f\n",
                format(latest_timestamp, "%H:%M:%S"),
                i,
                latest_vpin))
    
    update_key <- format(latest_timestamp, "%Y-%m-%d %H:%M:%S")
    vpin_updates_minutes[[update_key]] <- latest_vpin
  }
}

cat("--------------------------------------\n")
cat("...Simulation finished.\n")


if (length(vpin_updates_minutes) > 0) {
  vpin_minutes <- data.frame(
    timestamp = as.POSIXct(names(vpin_updates_minutes)),
    vpin = unlist(vpin_updates_minutes)
  )
  
  plot(vpin_minutes$timestamp, vpin_minutes$vpin, type = 'l', col = "blue",
       xlab = "Time", ylab = "VPIN Estimate",
       main = "Simulated Continuous VPIN Updates for Trade_Minutes_clean")
  grid()
} else {
  cat("Not enough data to generate VPIN updates. Try with a larger dataset or smaller vpin() parameters.\n")
}

}, silent = TRUE)

## For the speech dataset (14:25 to 14:55).

tryCatch({
  
hft_data <- data.frame(
  timestamp = Trade_clean$datetime,
  price = Trade_clean$PRICE,
  volume = Trade_clean$SIZE
)

# We'll start with an initial "burn-in" period to get the first calculation.
# Let's use the first 5 minutes of data as our starting point.
initial_burn_in_time <- hft_data$timestamp[1] + (5 * 60) # 5 minutes * 60 seconds
initial_rows <- which(hft_data$timestamp >= initial_burn_in_time)[1]

# If the dataset is shorter than 5 minutes, we'll adjust.
if (is.na(initial_rows)) {
  initial_rows <- nrow(hft_data)
}

chunk_size <- 1000 # Process 1000 new trades per update

vpin_updates_speech <- list()

for (i in seq(from = initial_rows, to = nrow(hft_data), by = chunk_size)) {
  current_data <- hft_data[1:i, ]
  vpin_results <- vpin(current_data, verbose = FALSE)
  if (nrow(vpin_results@bucketdata) > 0) {
    
    latest_vpin <- tail(vpin_results@bucketdata$vpin, 1)
    latest_timestamp <- tail(vpin_results@bucketdata$endtime, 1)
    
    cat(sprintf("Time: %s  |  Trades Processed: %d  |  Latest VPIN: %.4f\n",
                format(latest_timestamp, "%H:%M:%S"),
                i,
                latest_vpin))
    
    update_key <- format(latest_timestamp, "%Y-%m-%d %H:%M:%S")
    vpin_updates_speech[[update_key]] <- latest_vpin
  }
}

cat("--------------------------------------\n")
cat("...Simulation finished.\n")


if (length(vpin_updates_speech) > 0) {
  vpin_speech <- data.frame(
    timestamp = as.POSIXct(names(vpin_updates_speech)),
    vpin = unlist(vpin_updates_speech)
  )
  
  plot(vpin_speech$timestamp, vpin_speech$vpin, type = 'l', col = "blue",
       xlab = "Time", ylab = "VPIN Estimate",
       main = "Simulated Continuous VPIN Updates for Trade_Minutes_clean")
  grid()
} else {
  cat("Not enough data to generate VPIN updates. Try with a larger dataset or smaller vpin() parameters.\n")
}

}, silent = TRUE)


#==== 06b - Visualisation =====================================================#

if (!is.null(vpin_minutes) && !is.null(vpin_speech)) {
  
  cat("\nGenerating comparative plot...\n")
  
  # Plot 1: VPIN for the first dataset
  p1 <- ggplot(vpin_minutes, aes(x = timestamp, y = vpin)) +
    geom_line(color = "dodgerblue", size = 1) +
    labs(
      title = "VPIN Evolution (13:55 - 14:25)",
      subtitle = "Standard trading period",
      x = "Time",
      y = "VPIN Estimate"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  # Plot 2: VPIN for the speech dataset
  p2 <- ggplot(vpin_speech, aes(x = timestamp, y = vpin)) +
    geom_line(color = "firebrick", size = 1) +
    labs(
      title = "VPIN Evolution (14:25 - 14:55)",
      subtitle = "Trading period during speech",
      x = "Time",
      y = "VPIN Estimate"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  # Combine the two plots vertically using patchwork
  combined_plot <- p1 | p2
  
  # Display the combined plot
  print(combined_plot)
  
  Path <- file.path(Charts_Directory, "03_VPN_Combined_Plot.png")
  ggsave(
    filename = Path,
    plot = combined_plot,
    width = 3750,
    height = 1833,
    units = "px",
    dpi = 300,
    limitsize = FALSE
  )
  
} else {
  cat("\nCould not generate the comparative plot because one or both simulations failed to produce data.\n")
}

#==============================================================================#
#==============================================================================#
#==============================================================================#