# Utilities for coupled ecohydrofire model analysis
# Includes files/directories and functions


# ---------------------------------------------------------------------
# Libraries
library(RHESSysIOinR)
library(ggplot2)
library(tidyr)
library(dplyr)
library(lubridate)
library(sensitivity)
library(zoo)
library(beepr)
library(readr)


# ---------------------------------------------------------------------
# Files and Directories

RHESSYS_OUT_DIR_1.1_P301_RUN1 <- "ws_p301/out/1.1_p301_simulation/run1"
RHESSYS_ALLSIM_DIR_1.1_P301_RUN1 <- file.path(RHESSYS_OUT_DIR_1.1_P301_RUN1, "allsim")
RHESSYS_PAR_FILE_1.1_P301_RUN1 <- file.path(RHESSYS_OUT_DIR_1.1_P301_RUN1, "p301_simulation_parameter_sets.csv")


# ---------------------------------------------------------------------
# Functions





