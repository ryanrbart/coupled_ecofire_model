# Watershed Simulation for P301
# 
#

source("R/0.1_utilities.R")

# ---------------------------------------------------------------------
# Model inputs    

# Processing options
parameter_method <- "all_combinations"

  
# RHESSys Inputs
input_rhessys <- list()
#input_rhessys$rhessys_version <- "bin/rhessys5.20.1_fire_p301"
input_rhessys$rhessys_version <- "bin/rhessys6.0_salsa_fire"
input_rhessys$tec_file <- "ws_p301/tecfiles/p301_fire.tec"
input_rhessys$world_file <- "ws_p301/worldfiles/p301_30m_2canopy_fire.world"
input_rhessys$world_hdr_prefix <- "p301_30m_fire"
input_rhessys$flow_file <- "ws_p301/flowtables/p301_30m.flow"
input_rhessys$start_date <- "1941 10 1 1"
input_rhessys$end_date <- "1943 10 1 1"
input_rhessys$output_folder <- "ws_p301/out/1.1_p301_simulation"
input_rhessys$output_filename <- "p301_simulation"
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
  # Patch parameters
  list(input_hdr_list$soil_def[1], "soil_depth", c(4.468668012)),
  list(input_hdr_list$soil_def[1], "overstory_height_thresh", c(7)),
  list(input_hdr_list$soil_def[1], "understory_height_thresh", c(4)),
  
  # Upper canopy parameters
  list(input_hdr_list$stratum_def[1], "epc.alloc_frootc_leafc", c(1.4)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_crootc_stemc", c(0.328926288)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_stemc_leafc", c(0.4)),
  list(input_hdr_list$stratum_def[1], "epc.alloc_livewoodc_woodc", c(0.628006947)),
  list(input_hdr_list$stratum_def[1], "epc.leaf_turnover", c(0.251341906)),
  list(input_hdr_list$stratum_def[1], "epc.livewood_turnover", c(0.476537964)),
  list(input_hdr_list$stratum_def[1], "epc.branch_turnover", c(0.002788359)),
  list(input_hdr_list$stratum_def[1], "epc.height_to_stem_coef", c(10.77209291)),
  # Upper canopy fire parameters
  list(input_hdr_list$stratum_def[1], "understory_mort", c(1)),
  list(input_hdr_list$stratum_def[1], "consumption", c(1)),
  list(input_hdr_list$stratum_def[1], "overstory_mort_k1", c(-10)),
  list(input_hdr_list$stratum_def[1], "overstory_mort_k2", c(1)),
  
  # Lower canopy parameters
  list(input_hdr_list$stratum_def[2], "epc.alloc_frootc_leafc", c(1.4)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_crootc_stemc", c(0.404030788)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_stemc_leafc", c(0.2)),
  list(input_hdr_list$stratum_def[2], "epc.alloc_livewoodc_woodc", c(0.824053249)),
  list(input_hdr_list$stratum_def[2], "epc.leaf_turnover", c(0.407269482)),
  list(input_hdr_list$stratum_def[2], "epc.livewood_turnover", c(0.090120688)),
  list(input_hdr_list$stratum_def[2], "epc.branch_turnover", c(0.019216124)),
  list(input_hdr_list$stratum_def[2], "epc.height_to_stem_coef", c(3.161467927)),
  # Lower canopy fire parameters 
  list(input_hdr_list$stratum_def[2], "understory_mort", c(1)),
  list(input_hdr_list$stratum_def[2], "consumption", c(1)),
  list(input_hdr_list$stratum_def[2], "overstory_mort_k1", c(-10)),
  list(input_hdr_list$stratum_def[2], "overstory_mort_k2", c(1)),
  
  # -----
  # Fire spread parameters
  list(input_hdr_list$fire_def[1], "mean_ign", c(1)),
  list(input_hdr_list$fire_def[1], "ran_seed", c(0)),
  list(input_hdr_list$fire_def[1], "spread_calc_type", c(9))
)

# Standard sub-surface parameters
# input_standard_par_list <- NULL
input_standard_par_list <- list(
  m = c(2),
  k = c(2),
  m_v = c(2),
  k_v = c(2),
  pa = c(1.152206001),
  po = c(0.766755034),
  gw1 = c(0.24280128),
  gw2 = c(0.2)
)


# Make climate base station file
#input_clim_base_list <- NULL
input_clim_base_list <- list(
  list(core = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       annual = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       monthly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       daily = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE),
       hourly = data.frame(c1=character(),c2=character(),stringsAsFactors=FALSE)
  )
)
input_clim_base_list[[1]][[1]][1,] <- data.frame(c1=101, c2="base_station_id",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][2,] <- data.frame(c1=100.0, c2="x_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][3,] <- data.frame(c1=100.0, c2="y_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][4,] <- data.frame(c1=1748, c2="z_coordinate",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][5,] <- data.frame(c1=3.5, c2="effective_lai",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[1]][6,] <- data.frame(c1=2, c2="screen_height",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[2]][1,] <- data.frame(c1="annual", c2="annual_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[2]][2,] <- data.frame(c1=0, c2="number_non_critical_annual_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[3]][1,] <- data.frame(c1="monthly", c2="monthly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[3]][2,] <- data.frame(c1=0, c2="number_non_critical_monthly_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[4]][1,] <- data.frame(c1="ws_p301/clim/Grove_lowprov_clim_1942_2453", c2="daily_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[4]][2,] <- data.frame(c1=0, c2="number_non_critical_daily_sequences",stringsAsFactors=FALSE)

input_clim_base_list[[1]][[5]][1,] <- data.frame(c1="hourly", c2="hourly_climate_prefix",stringsAsFactors=FALSE)
input_clim_base_list[[1]][[5]][2,] <- data.frame(c1=0, c2="number_non_critical_hourly_sequences",stringsAsFactors=FALSE)


# Make a list of dated sequence data.frames (file name, year, month, day, hour, value)
 input_dated_seq_list <- NULL
# input_dated_seq_list = list()
# input_dated_seq_list[[1]] <- data.frame(name="lowProv",type="pspread",year=1941,month=10,day=7,hour=1,value=sobol_model$X$pspread[aa],stringsAsFactors=FALSE)


# Make tec-file
#input_tec_data <- NULL
input_tec_data <- data.frame(year=integer(),month=integer(),day=integer(),hour=integer(),name=character(),stringsAsFactors=FALSE)
input_tec_data[1,] <- data.frame(1941, 10, 1, 1, "print_daily_on", stringsAsFactors=FALSE)
input_tec_data[2,] <- data.frame(1941, 10, 1, 2, "print_daily_growth_on", stringsAsFactors=FALSE)
input_tec_data[3,] <- data.frame(2041, 9, 30, 1, "output_current_state", stringsAsFactors=FALSE)


# Data frame containing variable of interest, location/name of awk file (relative to output
# file location), and the location/name of rhessys output file with variable of interest.
output_variables <- NULL
# output_variables <- data.frame(variable=character(),awk_path=character(),out_file=character(),stringsAsFactors=FALSE)
# output_variables[1,] <- data.frame("lai", "awks/output_var_bd_lai.awk","p301_simulation_basin.daily",stringsAsFactors=FALSE)
# output_variables[2,] <- data.frame("leafc", "awks/output_var_cdg_leafc.awk","p301_simulation_grow_stratum.daily",stringsAsFactors=FALSE)
# output_variables[3,] <- data.frame("stemc", "awks/output_var_cdg_stemc.awk","p301_simulation_grow_stratum.daily",stringsAsFactors=FALSE)
# output_variables[4,] <- data.frame("rootc", "awks/output_var_cdg_rootc.awk","p301_simulation_grow_stratum.daily",stringsAsFactors=FALSE)
# 
# output_variables[5,] <- data.frame("litrc", "awks/output_var_bd_litrc.awk","p301_simulation_basin.daily",stringsAsFactors=FALSE)
# output_variables[6,] <- data.frame("cwdc", "awks/output_var_cdg_cwdc.awk","p301_simulation_grow_stratum.daily",stringsAsFactors=FALSE)
# output_variables[7,] <- data.frame("soil1c", "awks/output_var_pdg_soil1c.awk","p301_simulation_grow_patch.daily",stringsAsFactors=FALSE)
# 
# output_variables[8,] <- data.frame("height", "awks/output_var_cd_height.awk","p301_simulation_stratum.daily",stringsAsFactors=FALSE)
# 
# output_variables[9,] <- data.frame("understory_leafc", "awks/output_var_bdg_understory_leafc.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[10,] <- data.frame("understory_stemc", "awks/output_var_bdg_understory_stemc.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[11,] <- data.frame("understory_biomassc", "awks/output_var_bdg_understory_biomassc.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[12,] <- data.frame("understory_height", "awks/output_var_bdg_understory_height.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[13,] <- data.frame("overstory_leafc", "awks/output_var_bdg_overstory_leafc.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[14,] <- data.frame("overstory_stemc", "awks/output_var_bdg_overstory_stemc.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[15,] <- data.frame("overstory_biomassc", "awks/output_var_bdg_overstory_biomassc.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)
# output_variables[16,] <- data.frame("overstory_height", "awks/output_var_bdg_overstory_height.awk","p301_simulation_grow_basin.daily",stringsAsFactors=FALSE)


# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------

system.time(
  
  run_rhessys(parameter_method = parameter_method,
              output_method="awk",
              input_rhessys = input_rhessys,
              input_hdr_list = input_hdr_list,
              input_preexisting_table = input_preexisting_table,
              input_def_list = input_def_list,
              input_standard_par_list = input_standard_par_list,
              input_clim_base_list = input_clim_base_list,
              input_dated_seq_list = input_dated_seq_list,
              input_tec_data = input_tec_data,
              output_variables = output_variables,
              output_initiation = 1)
)

beep(1)



