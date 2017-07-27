# Watershed Simulation for P301
# 
# This script is used to simulate vegetation at patch level, beginning with no
# vegetation. A copy of the landscape (worldfile) is exported at various time 
# intervals for patch level fire analysis.

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# Model inputs

# Processing options
parameter_method <- "all_combinations"


# RHESSys Inputs
input_rhessys <- list()
input_rhessys$rhessys_version <- "bin/rhessys5.20.1_fire_p301"
input_rhessys$tec_file <- "ws_p301/tecfiles/p301_fire.tec"
input_rhessys$world_file <- "ws_p301/worldfiles/p301_30m_2canopy_fire.world"
input_rhessys$world_hdr_prefix <- "p301_30m_fire"
input_rhessys$flow_file <- "ws_p301/flowtables/p301_30m.flow"
input_rhessys$start_date <- "1941 10 1 1"
input_rhessys$end_date <- "2021 10 1 1"
input_rhessys$output_folder <- "ws_p301/out/1.1_p301_simulation"
input_rhessys$output_filename <- "p301_simulation7"
input_rhessys$command_options <- c("-b -g -c 1 189 8081 8081 -p 1 189 8081 8081 -firespread 30")


# HDR (header) file
input_hdr_list <- list()
input_hdr_list$basin_def <- c("ws_p301/defs/basin_p301.def")
input_hdr_list$hillslope_def <- c("ws_p301/defs/hill_p301.def")
input_hdr_list$zone_def <- c("ws_p301/defs/zone_p301.def")
input_hdr_list$soil_def <- c("ws_p301/defs/patch_p301.def")
input_hdr_list$landuse_def <- c("ws_p301/defs/lu_p301.def")
input_hdr_list$fire_def <- c("ws_p301/defs/fire_p301.def")
input_hdr_list$stratum_def <- c("ws_p301/defs/veg_p301_conifer.def", "ws_p301/defs/veg_p301_shrub.def")
input_hdr_list$base_stations <- c("ws_p301/clim/Grove_lowprov_clim_1942_2453.base")


# Define path to a pre-selected df containing parameter sets
input_preexisting_table <- NULL


# Def file parameter changes
# List of lists containing def_file, parameter and parameters values
#input_def_list <- NULL
input_def_list <- list(
  # Lower canopy parameters
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(0.4)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(0.1)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(1.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(0.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(0.2)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(0.9)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(0.02)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_exp", c(0.57)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(4.0)),
  # Lower canopy fire parameters
  list(input_hdr_list$stratum_def[2], "overstory_height_thresh", c(7)),
  list(input_hdr_list$stratum_def[2], "understory_height_thresh", c(4)),
  list(input_hdr_list$stratum_def[2], "pspread_loss_rel", c(1)),
  list(input_hdr_list$stratum_def[2], "vapor_loss_rel", c(0.01)),
  list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k1", c(-10)),
  list(input_hdr_list$stratum_def[2], "biomass_loss_rel_k2", c(1)),
  # -----
  # Upper canopy parameters
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_exp", c(0.57)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(11.39)),
  # Upper canopy fire parameters
  list(input_hdr_list$stratum_def[1], "overstory_height_thresh", c(7)),
  list(input_hdr_list$stratum_def[1], "understory_height_thresh", c(4)),
  list(input_hdr_list$stratum_def[1], "pspread_loss_rel", c(1)),
  list(input_hdr_list$stratum_def[1], "vapor_loss_rel", c(0.01)),
  list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k1", c(-10)),
  list(input_hdr_list$stratum_def[1], "biomass_loss_rel_k2", c(1))
  # -----
  # Fire spread parameters
  
)


# Make a list of dated sequence data.frames (year, month, day, hour, value)
input_dated_seq_file <- NULL
input_dated_seq_list <- NULL

# Standard sub-surface parameters
# input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(1.792761),
  k = c(1.566492),
  m_v = c(1.792761),
  k_v = c(1.566492),
  pa = c(7.896941),
  po = c(1.179359),
  gw1 = c(0.1668035),
  gw2 = c(0.178753)
)


# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)


# List of lists containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
output_variables <- NULL
# output_variables <- list()
# output_variables[[1]] <- list("lai", "awks/output_var_bd_lai.awk","patch_sim_basin.daily")
# output_variables[[2]] <- list("leafc", "awks/output_var_cdg_leafc.awk","patch_sim_grow_stratum.daily")
# output_variables[[3]] <- list("stemc", "awks/output_var_cdg_stemc.awk","patch_sim_grow_stratum.daily")
# output_variables[[4]] <- list("live_stemc", "awks/output_var_cdg_live_stemc.awk","patch_sim_grow_stratum.daily")
# output_variables[[5]] <- list("dead_stemc", "awks/output_var_cdg_dead_stemc.awk","patch_sim_grow_stratum.daily")
# output_variables[[6]] <- list("rootc", "awks/output_var_cdg_rootc.awk","patch_sim_grow_stratum.daily")
# 
# output_variables[[7]] <- list("litrc", "awks/output_var_bd_litrc.awk","patch_sim_basin.daily")
# output_variables[[8]] <- list("cwdc", "awks/output_var_cdg_cwdc.awk","patch_sim_grow_stratum.daily")
# output_variables[[9]] <- list("soil1c", "awks/output_var_pdg_soil1c.awk","patch_sim_grow_patch.daily")
# 
# output_variables[[10]] <- list("height", "awks/output_var_cd_height.awk","patch_sim_stratum.daily")

# ---------------------------------------------------------------------

system.time(
run_rhessys(parameter_method = parameter_method,
            input_rhessys = input_rhessys,
            input_hdr_list = input_hdr_list,
            input_preexisting_table = input_preexisting_table,
            input_def_list = input_def_list,
            input_standard_par_list = input_standard_par_list,
            input_dated_seq_file = input_dated_seq_file,
            input_tec_data = input_tec_data,
            output_variables = output_variables)
)

beep(1)

