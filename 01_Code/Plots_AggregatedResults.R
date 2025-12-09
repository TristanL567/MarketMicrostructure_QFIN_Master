#==============================================================================#
#==== 00 - Description ========================================================#
#==============================================================================#

## Code contains the Analysis for the aggregated results.
## Additionally, general charts will be generated in this file.

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
              "ggplot2","patchwork", ## For plotting and creating multi-plots.
              "here",
              "purrr", "ggrepel",
              "hms",
              "stargazer",
              "sandwich", "lmtest", "ggrepel"
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

## Plotting.
create_period_plot <- function(period_data, dummy_scale_data, plot_title) {
  
  ggplot(period_data, aes(x = estimate, y = Date)) +
    geom_blank(data = dummy_scale_data, aes(x = estimate, y = NULL)) +
    geom_point(aes(color = Event_Type), size = 2) +
    geom_errorbarh(aes(
      xmin = estimate - 1.96 * std.error,
      xmax = estimate + 1.96 * std.error,
      color = Event_Type
    ), height = 0) +
    
    # --- 3. Plot formatting ---
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    facet_grid(
      Event_Type ~ term, 
      scales = "free_x"  # This works *because* geom_blank set the range
    ) + 
    scale_color_manual(values = c("FOMC" = "red", "Control" = "grey")) +
    labs(
      title = plot_title,
      subtitle = "Lines are 95% C.I. using Newey-West SE",
      x = "Coefficient Estimate",
      y = "Date"
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 8),
      strip.text.y = element_text(angle = 0) # Nicer y-axis facet labels
    )
}


#==== 1C - Parameters =========================================================#

## Directories.
Data_Directory <- file.path(Path, "02_Data")
Charts_Directory <- file.path(Path, "03_Charts")
Functions_Directory <- file.path(Path, "01_Code/Functions")

## Load all code files in the functions directory.
sourceFunctions(Functions_Directory)

## Input Data Files.
Data_Input_Directory <- file.path(Data_Directory, "Data_All.RData")

# Output Charts Files.
Charts_Aggregate_Directory <- file.path(Charts_Directory, "Aggregate")

## Used dates.
FOMC_Dates <- c("2025-01-29", "2025-03-19", "2025-05-07",
                "2025-06-18", "2025-07-30", "2025-09-29",
                "2025-10-29")

Controls_Dates <- c("2025-02-04", "2025-03-27", "2025-05-20",
                    "2025-06-23", "2025-08-04", "2025-10-03",
                    "2025-11-05")

## Plotting.
blue <- "#004890"
grey <- "#708090"
orange <- "#F37021"
red <- "#B22222"

height <- 1833
width <- 3750

#==== 1D - git ================================================================#

# usethis::use_git_ignore(c())

#==============================================================================#
#==== 02 - Data ===============================================================#
#==============================================================================#

#==== 02a - Load the pre-processed dataset ====================================#

load(Data_Input_Directory)

#==== 02b - Overall results (Full Period) =====================================#

tryCatch({
  
  fomc_results <- map_dfr(
    Price_Impact_Regressions_All, 
    ~ .x[["Full Period (NW)"]], 
    .id = "Date"
  ) %>% 
    mutate(Event_Type = "FOMC")
  
  control_results <- map_dfr(
    Price_Impact_Regressions_Controls_All,
    ~ .x[["Full Period (NW)"]], 
    .id = "Date"
  ) %>% 
    mutate(Event_Type = "Control") # Add an identifier
  
  all_results <- bind_rows(fomc_results, control_results)
  all_results <- all_results %>%
    mutate(Date = as.Date(Date))
  all_results_full <- all_results
  
  # paired_data <- lambda_results %>%
  #   arrange(Event_Type, Date) %>% 
  #   group_by(Event_Type) %>%
  #   mutate(pair_id = row_number()) %>%
  #   ungroup() %>%
  #   select(pair_id, Event_Type, Date, estimate) %>%
  #   pivot_wider(
  #     names_from = Event_Type,
  #     values_from = c(estimate, Date)
  #   )
  
## =========================== ##
## Visualisation
## =========================== ##
  
  all_outliers <- c("2025-09-29", "2025-10-29")
  crash_date   <- "2025-10-29"
  
  clean_data <- all_results_full %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Event_Type = factor(Event_Type, levels = c("Control", "FOMC")),
      is_significant = if_else(p.value < 0.05, "Significant", "Insignificant"),
      term_math = case_when(
        term == "Lambda"    ~ "lambda",
        term == "Beta"      ~ "beta",
        term == "delta_d_t" ~ "gamma",
        term == "q_t"       ~ "lambda + beta", # Assuming this is what you meant by "lambda+gamma"
        term == "lag_q_t"   ~ "-lambda * phi",
        TRUE                ~ term
      ),
      label_text = case_when(
        term_math %in% c("gamma", "lambda + beta") & as.character(Date) == crash_date ~ as.character(Date),
                !term_math %in% c("gamma", "lambda + beta") & as.character(Date) %in% all_outliers ~ as.character(Date),
        TRUE ~ NA_character_
      )
    )
  
  plot <- ggplot(clean_data, aes(x = Event_Type, y = estimate, fill = Event_Type)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
        geom_jitter(aes(color = is_significant), width = 0.2, size = 1.5, alpha = 0.6) + 
        geom_text_repel(
      aes(label = label_text),
      size = 3,
      box.padding = 0.5,
      max.overlaps = Inf,
      min.segment.length = 0,
      color = "black"
    ) +
    
    facet_wrap(~ term_math, scales = "free", ncol = 3, labeller = label_parsed) +
    
    scale_fill_manual(values = c("Control" = "grey", "FOMC" = "red")) +
    scale_color_manual(values = c("Significant" = "orange", "Insignificant" = "black")) +
    
    labs(
      title = "",
      subtitle = "",
      y = "Estimate",
      x = ""
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold")
    )
  
  Path <- file.path(Charts_Aggregate_Directory, "10a_CoefficientEstimates_Fullperiod_Boxplot.png")
  ggsave(
    filename = Path,
    plot = plot,
    width = height,
    height = width,
    units = "px",
    dpi = 300,
    limitsize = FALSE
  )
  
  
## =========================== ##
## Visualisation Boxplot.
## =========================== ##
  
#   Plot <- ggplot(all_results %>% filter(term != "(Intercept)"), 
#                  aes(x = estimate, y = Date)) +
#     geom_point(aes(color = Event_Type), size = 2) +
#     geom_errorbarh(aes(
#       xmin = estimate - 1.96 * std.error,
#       xmax = estimate + 1.96 * std.error,
#       color = Event_Type
#     ), height = 0) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#     facet_grid(
#       Event_Type ~ term, 
#       scales = "free_x"
#     ) + 
#     scale_color_manual(values = c("FOMC" = "red", "Control" = "grey")) +
#     labs(
#       title = "",
#       subtitle = "Lines are 95% C.I. using Newey-West SE",
#       x = "Coefficient Estimate",
#       y = "Date"
#     ) +
#     theme_bw() + # `theme_bw` often works better for facets
#     theme(legend.position = "none",
#           axis.text.y = element_text(size = 8)) # Smaller text for dates
# 
# Path <- file.path(Charts_Aggregate_Directory, "1a_CoefficientEstimates_Fullperiod.png")
# ggsave(
#   filename = Path,
#   plot = Plot,
#   width = height,
#   height = width,
#   units = "px",
#   dpi = 300,
#   limitsize = FALSE
# )

}, silent = TRUE)

#==== 02c - Overall results (Subperiod 1) =====================================#

tryCatch({
  
fomc_results <- map_dfr(
  Price_Impact_Regressions_All, 
  ~ .x[["Subperiod 1 (NW)"]], 
  .id = "Date"
) %>% 
  mutate(Event_Type = "FOMC")

control_results <- map_dfr(
  Price_Impact_Regressions_Controls_All,
  ~ .x[["Subperiod 1 (NW)"]], 
  .id = "Date"
) %>% 
  mutate(Event_Type = "Control") # Add an identifier

all_results <- bind_rows(fomc_results, control_results)
all_results <- all_results %>%
  mutate(Date = as.Date(Date))
all_results_sub1 <- all_results

# paired_data <- lambda_results %>%
#   arrange(Event_Type, Date) %>% 
#   group_by(Event_Type) %>%
#   mutate(pair_id = row_number()) %>%
#   ungroup() %>%
#   select(pair_id, Event_Type, Date, estimate) %>%
#   pivot_wider(
#     names_from = Event_Type,
#     values_from = c(estimate, Date)
#   )

## =========================== ##
## Visualisation
## =========================== ##

all_outliers    <- c("2025-09-29", "2025-10-29") # FOMC outliers
crash_date      <- "2025-10-29"                  # FOMC crash day
control_outlier <- "2025-02-04"                  # The specific Control outlier

clean_data <- all_results_sub1 %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Event_Type = factor(Event_Type, levels = c("Control", "FOMC")),
    
    # 1. Significance Flag
    is_significant = if_else(p.value < 0.05, "Significant", "Insignificant"),
    
    # 2. Math Terms
    term_math = case_when(
      term == "Lambda"    ~ "lambda",
      term == "Beta"      ~ "beta",
      term == "delta_d_t" ~ "gamma",
      term == "q_t"       ~ "lambda + beta", 
      term == "lag_q_t"   ~ "-lambda * phi",
      TRUE                ~ term
    ),
    
    # 3. Refined Label Logic
    label_text = case_when(
      # Case A: Gamma (Only the crash date)
      term_math == "gamma" & as.character(Date) == crash_date ~ as.character(Date),
      
      # Case B: Lambda + Beta (Crash date AND the specific Control outlier)
      term_math == "lambda + beta" & as.character(Date) %in% c(crash_date, control_outlier) ~ as.character(Date),
      
      # Case C: Lambda and Lag Q (Both FOMC outliers AND the specific Control outlier)
      term_math %in% c("lambda", "-lambda * phi") & as.character(Date) %in% c(all_outliers, control_outlier) ~ as.character(Date),
      
      # Case D: Everything else (i.e., Beta) - Mark both FOMC outliers
      as.character(Date) %in% all_outliers ~ as.character(Date),
      
      # Default: No label
      TRUE ~ NA_character_
    )
  )

plot <- ggplot(clean_data, aes(x = Event_Type, y = estimate, fill = Event_Type)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    geom_jitter(aes(color = is_significant), width = 0.2, size = 1.5, alpha = 0.6) + 
    geom_text_repel(
    aes(label = label_text),
    size = 3,
    box.padding = 0.5,
    max.overlaps = Inf,
    min.segment.length = 0,
    color = "black"
  ) +
  
  facet_wrap(~ term_math, scales = "free", ncol = 3, labeller = label_parsed) +
  scale_fill_manual(values = c("Control" = "grey", "FOMC" = "red")) +
  scale_color_manual(values = c("Significant" = "orange", "Insignificant" = "black")) +
  
  labs(
    title = "",
    subtitle = "",
    y = "Estimate",
    x = ""
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.text = element_text(size = 12, face = "bold")
  )

Path <- file.path(Charts_Aggregate_Directory, "10b_CoefficientEstimates_Period1_Boxplot.png")
ggsave(
  filename = Path,
  plot = plot,
  width = height,
  height = width,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

## =========================== ##
## Visualisation
## =========================== ##

# Plot <- ggplot(all_results %>% filter(term != "(Intercept)"), 
#        aes(x = estimate, y = Date)) +
#   geom_point(aes(color = Event_Type), size = 2) +
#   geom_errorbarh(aes(
#     xmin = estimate - 1.96 * std.error,
#     xmax = estimate + 1.96 * std.error,
#     color = Event_Type
#   ), height = 0) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#   facet_grid(
#     Event_Type ~ term, 
#     scales = "free_x"
#   ) + 
#   scale_color_manual(values = c("FOMC" = "red", "Control" = "grey")) +
#   labs(
#     title = "",
#     subtitle = "Lines are 95% C.I. using Newey-West SE",
#     x = "Coefficient Estimate",
#     y = "Date"
#   ) +
#   theme_bw() + # `theme_bw` often works better for facets
#   theme(legend.position = "none",
#         axis.text.y = element_text(size = 8)) # Smaller text for dates
# 
# Path <- file.path(Charts_Aggregate_Directory, "1b_CoefficientEstimates_SUbperiod1.png")
# ggsave(
#   filename = Path,
#   plot = Plot,
#   width = height,
#   height = width,
#   units = "px",
#   dpi = 300,
#   limitsize = FALSE
# )

}, silent = TRUE)

#==== 02d - Overall results (Subperiod 2) =====================================#

tryCatch({
  
  fomc_results <- map_dfr(
    Price_Impact_Regressions_All, 
    ~ .x[["Subperiod 2 (NW)"]], 
    .id = "Date"
  ) %>% 
    mutate(Event_Type = "FOMC")
  
  control_results <- map_dfr(
    Price_Impact_Regressions_Controls_All,
    ~ .x[["Subperiod 2 (NW)"]], 
    .id = "Date"
  ) %>% 
    mutate(Event_Type = "Control") # Add an identifier
  
  all_results <- bind_rows(fomc_results, control_results)
  all_results <- all_results %>%
    mutate(Date = as.Date(Date))
  all_results_sub2 <- all_results
  
  # paired_data <- lambda_results %>%
  #   arrange(Event_Type, Date) %>% 
  #   group_by(Event_Type) %>%
  #   mutate(pair_id = row_number()) %>%
  #   ungroup() %>%
  #   select(pair_id, Event_Type, Date, estimate) %>%
  #   pivot_wider(
  #     names_from = Event_Type,
  #     values_from = c(estimate, Date)
  #   )
  # 
  
  ## =========================== ##
  ## Visualisation
  ## =========================== ##
  
  all_outliers    <- c("2025-09-29", "2025-10-29") 
  crash_date      <- "2025-10-29"
  control_outlier <- ""
  
  clean_data <- all_results_sub2 %>%
    filter(term != "(Intercept)") %>%
    mutate(
      Event_Type = factor(Event_Type, levels = c("Control", "FOMC")),
      is_significant = if_else(p.value < 0.05, "Significant", "Insignificant"),
      term_math = case_when(
        term == "Lambda"    ~ "lambda",
        term == "Beta"      ~ "beta",
        term == "delta_d_t" ~ "gamma",
        term == "q_t"       ~ "lambda + beta", 
        term == "lag_q_t"   ~ "-lambda * phi",
        TRUE                ~ term
      ),
      label_text = case_when(
        term_math == "gamma" & as.character(Date) == crash_date ~ as.character(Date),
        term_math == "lambda + beta" & as.character(Date) %in% c(crash_date, control_outlier) ~ as.character(Date),
          term_math %in% c("lambda", "-lambda * phi") & as.character(Date) %in% c(all_outliers, control_outlier) ~ as.character(Date),
          as.character(Date) %in% all_outliers & term_math == "beta" ~ as.character(Date),
        
        TRUE ~ NA_character_
      )
    )
  
  plot <- ggplot(clean_data, aes(x = Event_Type, y = estimate, fill = Event_Type)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA) +
    
    geom_jitter(aes(color = is_significant), width = 0.2, size = 1.5, alpha = 0.6) + 
    
    geom_text_repel(
      aes(label = label_text),
      size = 3,
      box.padding = 0.5,
      max.overlaps = Inf,
      min.segment.length = 0,
      color = "black"
    ) +
    
    facet_wrap(~ term_math, scales = "free", ncol = 3, labeller = label_parsed) +
    scale_fill_manual(values = c("Control" = "grey", "FOMC" = "red")) +
    scale_color_manual(values = c("Significant" = "orange", "Insignificant" = "black")) +
    labs(
      title = "",
      subtitle = "",
      y = "Estimate",
      x = ""
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 12, face = "bold")
    )

  Path <- file.path(Charts_Aggregate_Directory, "10c_CoefficientEstimates_Period2_Boxplot.png")
  ggsave(
    filename = Path,
    plot = plot,
    width = height,
    height = width,
    units = "px",
    dpi = 300,
    limitsize = FALSE
  )
  
  ## =========================== ##
  ## Visualisation
  ## =========================== ##
  
# Plot <- ggplot(all_results %>% filter(term != "(Intercept)"), 
#          aes(x = estimate, y = Date)) +
#     geom_point(aes(color = Event_Type), size = 2) +
#     geom_errorbarh(aes(
#       xmin = estimate - 1.96 * std.error,
#       xmax = estimate + 1.96 * std.error,
#       color = Event_Type
#     ), height = 0) +
#     geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
#     facet_grid(
#       Event_Type ~ term, 
#       scales = "free_x"
#     ) + 
#     scale_color_manual(values = c("FOMC" = "red", "Control" = "grey")) +
#     labs(
#       title = "",
#       subtitle = "Lines are 95% C.I. using Newey-West SE",
#       x = "Coefficient Estimate",
#       y = "Date"
#     ) +
#     theme_bw() + # `theme_bw` often works better for facets
#     theme(legend.position = "none",
#           axis.text.y = element_text(size = 8)) # Smaller text for dates
#   
# Path <- file.path(Charts_Aggregate_Directory, "1c_CoefficientEstimates_SUbperiod2.png")
# ggsave(
#   filename = Path,
#   plot = Plot,
#   width = height,
#   height = width,
#   units = "px",
#   dpi = 300,
#   limitsize = FALSE
# )

}, silent = TRUE)

#==== 02e - Overall results (All) =============================================#

tryCatch({
all_results_full <- all_results_full %>% 
  mutate(Period = "Full Period (13:00-16:00)")

all_results_sub1 <- all_results_sub1 %>% 
  mutate(Period = "Subperiod 1") 

all_results_sub2 <- all_results_sub2 %>% 
  mutate(Period = "Subperiod 2") 


all_results_combined <- bind_rows(
  all_results_full, 
  all_results_sub1, 
  all_results_sub2
)

all_results_combined <- all_results_combined %>%
  mutate(Period = factor(Period, levels = c(
    "Full Period (13:00-16:00)", 
    "Subperiod 1", 
    "Subperiod 2"
  )))

new_term_names <- c(
  "d_t" = "λ_0",
  "q_t" = "λ_1",
  "delta_d_t" = "γ"
)

all_results_renamed <- all_results_combined %>%
  filter(term != "(Intercept)") %>%
  mutate(term = recode(term, !!!new_term_names))

scale_limits <- all_results_renamed %>%
  mutate(
    xmin = estimate - 1.96 * std.error,
    xmax = estimate + 1.96 * std.error
  ) %>%
  group_by(term) %>%
  summarise(
    global_min = min(xmin, na.rm = TRUE),
    global_max = max(xmax, na.rm = TRUE)
  ) %>%
  ungroup()

scale_limits$global_min[3] <- -0.000000000000001
scale_limits$global_max[3] <- 0.0000000000000001

dummy_data <- bind_rows(
  scale_limits %>% select(term, estimate = global_min),
  scale_limits %>% select(term, estimate = global_max)
)

plot_full <- create_period_plot(
  period_data = all_results_renamed %>% 
    filter(Period == "Full Period (13:00-16:00)"),
  dummy_scale_data = dummy_data,
  plot_title = "Coefficient Estimates: 13:00-16:00"
)

Path <- file.path(Charts_Aggregate_Directory, "1a_CoefficientEstimates_Fullperiod.png")
ggsave(
  filename = Path,
  plot = plot_full,
  width = height,
  height = width,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

# --- Plot 2: Subperiod 1 ---
plot_sub1 <- create_period_plot(
  period_data = all_results_renamed %>% 
    filter(Period == "Subperiod 1"),
  dummy_scale_data = dummy_data,
  plot_title = "Coefficient Estimates: 13:00-14:25"
)

Path <- file.path(Charts_Aggregate_Directory, "1b_CoefficientEstimates_Subperiod1.png")
ggsave(
  filename = Path,
  plot = plot_sub1,
  width = height,
  height = width,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

# --- Plot 3: Subperiod 2 ---
plot_sub2 <- create_period_plot(
  period_data = all_results_renamed %>% 
    filter(Period == "Subperiod 2"),
  dummy_scale_data = dummy_data,
  plot_title = "Coefficient Estimates: 14:25-16:00"
)

Path <- file.path(Charts_Aggregate_Directory, "1c_CoefficientEstimates_Subperiod2.png")
ggsave(
  filename = Path,
  plot = plot_sub2,
  width = height,
  height = width,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

}, silent = TRUE)

#==============================================================================#
#==== 03 - Analysis ===========================================================#
#==============================================================================#

#==== 03a - Aggregate the effect (FOMC) =======================================#

## =========================== ##
## FOMC
## =========================== ##
## Coefficients.
Dates <- do.call(cbind, lapply(Lambda_results_Output, function(x) x[, 1]))[,1]
Dates <- as.POSIXct(Dates, format = "%Y-%m-%d %H:%M:%S")

Coefficient_Estimate <- do.call(cbind, lapply(Lambda_results_Output, function(x) x[, 2]))
colnames(Coefficient_Estimate) <- FOMC_Dates

## Process the estimates.
Coefficient_Estimate_abs <- abs(Coefficient_Estimate)
Coefficient_Estimate_abs_aggregate <- rowMeans(Coefficient_Estimate_abs) ## Absolute value.
Coefficient_Estimate_aggregate <- rowMeans(Coefficient_Estimate) ## Simple Average.
# Coefficient_Estimate_aggregate <- vapply(Coefficient_Estimate, 1) ## Median.


## P-values.
pval_Estimate <- do.call(cbind, lapply(Lambda_results_Output, function(x) x[, 3]))
colnames(pval_Estimate) <- FOMC_Dates
pval_Estimate_aggregate <- rowMeans(pval_Estimate)

## Combine the data.
data_combined <- cbind(Dates, 
                       Coefficient_Estimate_aggregate,
                       pval_Estimate_aggregate)
colnames(data_combined) <- c("window_start", "estimate", "p.value")
data_combined <- data.frame(data_combined)

## Absolute value.
data_combined_abs <- cbind(Dates, 
                           Coefficient_Estimate_abs_aggregate,
                           pval_Estimate_aggregate)
colnames(data_combined_abs) <- c("window_start", "estimate", "p.value")
data_combined_abs <- data.frame(data_combined_abs)

## =========================== ##
## Visualisation
## =========================== ##

## Simple Average.
tryCatch({
  
  lambda_results <- data_combined %>%
    mutate(significant = ifelse(p.value < 0.05, "Yes", "No"))
  
  Plot <- ggplot(lambda_results, aes(x = window_start, y = estimate * shares_in_10k_trade)) +
    geom_line() +
    geom_point(aes(color = significant)) +
    scale_color_manual(
      values = c("Yes" = "red", "No" = "grey"),
      name = "Significant (p < 0.05)" # Added a clearer legend title
    ) +
    labs(
      title = "",
      y = "Coefficient size (Absolute Value)",
      x = "Time"
    ) +
    theme_minimal() +  # Using your requested theme
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) # Using your requested axis text rotation
    )
  
  Path <- file.path(Charts_Aggregate_Directory, "FOMC_Aggregate_Coefficient_Size.png")
  ggsave(
    filename = Path,
    plot = Plot,
    width = height,
    height = width,
    units = "px",
    dpi = 300,
    limitsize = FALSE
  )
  
}, silent = TRUE)

## Absolute Value.

tryCatch({
  
lambda_results <- data_combined_abs %>%
  mutate(significant = ifelse(p.value < 0.05, "Yes", "No"))

Plot <- ggplot(lambda_results, aes(x = window_start, y = estimate * shares_in_10k_trade)) +
  geom_line() +
  geom_point(aes(color = significant)) +
  scale_x_datetime(labels = scales::date_format("%H:%M:%S")) +
  scale_color_manual(
    values = c("Yes" = "red", "No" = "grey"),
    name = "Significant (p < 0.05)" # Added a clearer legend title
  ) +
  labs(
    title = "",
    y = "Coefficient size (Absolute Value)",
    x = "Time"
  ) +
  theme_minimal() +  # Using your requested theme
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1) # Using your requested axis text rotation
  )

Path <- file.path(Charts_Aggregate_Directory, "FOMC_Aggregate_Coefficient_Size_abs.png")
ggsave(
  filename = Path,
  plot = Plot,
  width = height,
  height = width,
  units = "px",
  dpi = 300,
  limitsize = FALSE
)

}, silent = TRUE)

#==== 03a - Aggregate the effect (Controls) ===================================#

## =========================== ##
## Controls
## =========================== ##
## Coefficients.
Dates <- do.call(cbind, lapply(Lambda_results_Output, function(x) x[, 1]))[,1]
Dates <- as.POSIXct(Dates, format = "%Y-%m-%d %H:%M:%S")

Coefficient_Estimate <- do.call(cbind, lapply(Lambda_results_Output, function(x) x[, 2]))
colnames(Coefficient_Estimate) <- FOMC_Dates

## Process the estimates.
Coefficient_Estimate_abs <- abs(Coefficient_Estimate)
Coefficient_Estimate_abs_aggregate <- rowMeans(Coefficient_Estimate_abs) ## Absolute value.
Coefficient_Estimate_aggregate <- rowMeans(Coefficient_Estimate) ## Simple Average.

#==============================================================================#
#==== 04 - Visualisation ======================================================#
#==============================================================================#

date_variable <- 2
output_stargazer <- list()

#==== 04a - Regression Output (with stargazer) ================================#

tryCatch({
  
  date_selected <- FOMC_Dates[date_variable]
reg_model <- Kyle_Regression_Output_All[[date_selected]]$`Full period`
Output_plot <- Kyle_Regression_Output_All[[date_selected]]$`Full Period (NW)`
model_coef_names <- names(coef(reg_model))

match_index <- match(model_coef_names, Output_plot$term)

nw_se <- Output_plot$std.error[match_index]
nw_t  <- Output_plot$statistic[match_index]
nw_p  <- Output_plot$p.value[match_index]
reg_labels <- model_coef_names[model_coef_names != "(Intercept)"]

output_stargazer[[1]] <- list(reg_model, nw_t, nw_p)

Path <- file.path(Charts_Aggregate_Directory, "2a_Regression_Summary_FOMC_20250319.html")

  stargazer(
      reg_model,
      type = "html",
      out = Path,
      se = list(nw_t),  # <-- CHANGED: Pass t-stats to the 'se' argument
      t  = list(nw_t),
      p  = list(nw_p),
      title = "",
      dep.var.labels = "Price Change",
      # covariate.labels = reg_labels,
      notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
      header = FALSE,
      align = TRUE,
      digits = 8,
      omit.stat = c("f", "adj.rsq", "ll"),
      notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
    )
  
}, silent = TRUE)

## Control.

tryCatch({
  
  date_selected <- Controls_Dates[date_variable]
  reg_model <- Kyle_Regression_Output_Controls_All[[date_selected]]$`Full period`
  Output_plot <- Kyle_Regression_Output_Controls_All[[date_selected]]$`Full Period (NW)`
  model_coef_names <- names(coef(reg_model))
  
  match_index <- match(model_coef_names, Output_plot$term)
  
  nw_se <- Output_plot$std.error[match_index]
  nw_t  <- Output_plot$statistic[match_index]
  nw_p  <- Output_plot$p.value[match_index]
  reg_labels <- model_coef_names[model_coef_names != "(Intercept)"]
  output_stargazer[[2]] <- list(reg_model, nw_t, nw_p)
  
  Path <- file.path(Charts_Aggregate_Directory, "2b_Regression_Summary_Control_20250327.html")
  
  stargazer(
    reg_model,
    type = "html",
    out = Path,
    se = list(nw_t),
    t  = list(nw_t),
    p  = list(nw_p),
    title = "",
    dep.var.labels = "Price Change",
    # covariate.labels = reg_labels,
    header = FALSE,
    notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
    align = TRUE,
    digits = 8,
    omit.stat = c("f", "adj.rsq", "ll"),
    notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
  )
  
}, silent = TRUE)


#==== 04b - Regression Output (Subperiod 1) ===================================#

tryCatch({
  
  date_selected <- FOMC_Dates[date_variable]
  reg_model <- Kyle_Regression_Output_All[[date_selected]]$`Subperiod 1`
  Output_plot <- Kyle_Regression_Output_All[[date_selected]]$`Subperiod 1 (NW)`
  model_coef_names <- names(coef(reg_model))
  
  match_index <- match(model_coef_names, Output_plot$term)
  
  nw_se <- Output_plot$std.error[match_index]
  nw_t  <- Output_plot$statistic[match_index]
  nw_p  <- Output_plot$p.value[match_index]
  reg_labels <- model_coef_names[model_coef_names != "(Intercept)"]
  output_stargazer[[3]] <- list(reg_model, nw_t, nw_p)
  
  Path <- file.path(Charts_Aggregate_Directory, "3a_Regression_Subperiod1_Summary_FOMC_20250319.html")
  
  stargazer(
    reg_model,
    type = "html",
    out = Path,
    se = list(nw_t),  # <-- CHANGED: Pass t-stats to the 'se' argument
    t  = list(nw_t),
    p  = list(nw_p),
    title = "",
    dep.var.labels = "Price Change",
    # covariate.labels = reg_labels,
    notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
    header = FALSE,
    align = TRUE,
    digits = 8,
    omit.stat = c("f", "adj.rsq", "ll"),
    notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
  )
  
}, silent = TRUE)

## Control.

tryCatch({
  
  date_selected <- Controls_Dates[date_variable]
  reg_model <- Kyle_Regression_Output_Controls_All[[date_selected]]$`Subperiod 1`
  Output_plot <- Kyle_Regression_Output_Controls_All[[date_selected]]$`Subperiod 1 (NW)`
  model_coef_names <- names(coef(reg_model))
  
  match_index <- match(model_coef_names, Output_plot$term)
  
  nw_se <- Output_plot$std.error[match_index]
  nw_t  <- Output_plot$statistic[match_index]
  nw_p  <- Output_plot$p.value[match_index]
  reg_labels <- model_coef_names[model_coef_names != "(Intercept)"]
  output_stargazer[[4]] <- list(reg_model, nw_t, nw_p)
  
  Path <- file.path(Charts_Aggregate_Directory, "3b_Regression_Subperiod1_Summary_Control_20250327.html")
  
  stargazer(
    reg_model,
    type = "html",
    out = Path,
    se = list(nw_t),
    t  = list(nw_t),
    p  = list(nw_p),
    title = "",
    dep.var.labels = "Price Change",
    # covariate.labels = reg_labels,
    header = FALSE,
    notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
    align = TRUE,
    digits = 8,
    omit.stat = c("f", "adj.rsq", "ll"),
    notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
  )
  
}, silent = TRUE)

#==== 04c - Regression Output (Subperiod 2) ===================================#


tryCatch({
  
  date_selected <- FOMC_Dates[date_variable]
  reg_model <- Kyle_Regression_Output_All[[date_selected]]$`Subperiod `
  Output_plot <- Kyle_Regression_Output_All[[date_selected]]$`Subperiod 2 (NW)`
  model_coef_names <- names(coef(reg_model))
  
  match_index <- match(model_coef_names, Output_plot$term)
  
  nw_se <- Output_plot$std.error[match_index]
  nw_t  <- Output_plot$statistic[match_index]
  nw_p  <- Output_plot$p.value[match_index]
  reg_labels <- model_coef_names[model_coef_names != "(Intercept)"]
  output_stargazer[[5]] <- list(reg_model, nw_t, nw_p)
  
  Path <- file.path(Charts_Aggregate_Directory, "4a_Regression_Subperiod2_Summary_FOMC_20250319.html")
  
  stargazer(
    reg_model,
    type = "html",
    out = Path,
    se = list(nw_t),  # <-- CHANGED: Pass t-stats to the 'se' argument
    t  = list(nw_t),
    p  = list(nw_p),
    title = "",
    dep.var.labels = "Price Change",
    # covariate.labels = reg_labels,
    notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
    header = FALSE,
    align = TRUE,
    digits = 8,
    omit.stat = c("f", "adj.rsq", "ll"),
    notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
  )
  
}, silent = TRUE)

## Control.

tryCatch({
  
  date_selected <- Controls_Dates[date_variable]
  reg_model <- Kyle_Regression_Output_Controls_All[[date_selected]]$`Subperiod 2`
  Output_plot <- Kyle_Regression_Output_Controls_All[[date_selected]]$`Subperiod 2 (NW)`
  model_coef_names <- names(coef(reg_model))
  
  match_index <- match(model_coef_names, Output_plot$term)
  
  nw_se <- Output_plot$std.error[match_index]
  nw_t  <- Output_plot$statistic[match_index]
  nw_p  <- Output_plot$p.value[match_index]
  reg_labels <- model_coef_names[model_coef_names != "(Intercept)"]
  output_stargazer[[6]] <- list(reg_model, nw_t, nw_p)
  
  Path <- file.path(Charts_Aggregate_Directory, "4b_Regression_Subperiod2_Summary_Control_20250327.html")
  
  stargazer(
    reg_model,
    type = "html",
    out = Path,
    se = list(nw_t),
    t  = list(nw_t),
    p  = list(nw_p),
    title = "",
    dep.var.labels = "Price Change",
    # covariate.labels = reg_labels,
    header = FALSE,
    notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
    align = TRUE,
    digits = 8,
    omit.stat = c("f", "adj.rsq", "ll"),
    notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
  )
  
}, silent = TRUE)

#==== 04d - Regression Output (Combined) ======================================#

Models_list <- lapply(output_stargazer, function(x) x[[1]])
T_stat_list <- lapply(output_stargazer, function(x) x[[2]])
p_val_list <- lapply(output_stargazer, function(x) x[[3]])

Path <- file.path(Charts_Aggregate_Directory, "5a_Regression_Summary_20250327.html")

stargazer(
  Models_list,
  type = "html",
  out = Path,
  se = list(T_stat_list[[1]], T_stat_list[[2]],
            T_stat_list[[3]], T_stat_list[[4]],
            T_stat_list[[5]], T_stat_list[[6]]),
  t  = list(T_stat_list[[1]], T_stat_list[[2]],
            T_stat_list[[3]], T_stat_list[[4]],
            T_stat_list[[5]], T_stat_list[[6]]),
  p  = list(p_val_list[[1]], p_val_list[[1]],
            p_val_list[[3]], p_val_list[[4]],
            p_val_list[[5]], p_val_list[[6]]),
  title = "",
  dep.var.labels = "Price Change",
  column.labels = c("FOMC", 
                    "Control",
                    "FOMC (P.1)",
                    "Control (P.1)",
                    "FOMC (P.2)",
                    "Control (P.2)"),
  # covariate.labels = reg_labels,
  header = FALSE,
  notes.append = FALSE, # <-- ADDED: Prevents default "Standard errors..." note
  align = TRUE,
  digits = 8,
  omit.stat = c("f", "adj.rsq", "ll"),
  notes = paste0("Standard errors are Newey-West robust (lag=", lag_NW, ").")
)

#==== 04e - Bid-ask spread & reaction of the MM ===============================#


#==============================================================================#
#==============================================================================#
#==============================================================================#