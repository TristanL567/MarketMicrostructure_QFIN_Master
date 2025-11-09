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

Path <- "C:/Users/TristanLeiter/Documents/Privat/Market_Microstructure/04_Presentation/MarketMicrostructure_QFIN_Master"

#==== 1A - Libraries ==========================================================#

## Needs to enable checking for install & if not then autoinstall.

packages <- c("dplyr", "tidyr", "lubridate",
              "ggplot2","patchwork")

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




#==============================================================================#
#==============================================================================#
#==============================================================================#