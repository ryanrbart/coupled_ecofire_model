# Code for testing the coupled model in p301
#
# 

source("R/0.1_utilities.R")

#theme_set(theme_bw(base_size = 11))
#theme_set(theme_bw(base_size = 16))


# ---------------------------------------------------------------------
# P301 coupled model data processing

run_num <- seq(14,15)

for (hh in seq_along(run_num)){
  print(paste("Importing number",run_num[hh]))
  results_tmp <- readin_rhessys_output(paste("ws_p301/out/1.1_p301_simulation/p301_simulation",run_num[hh],sep=""), b=1, p=1, c=1, g=1)
  assign(paste("bd",run_num[hh],sep=""), results_tmp$bd)
  assign(paste("bdg",run_num[hh],sep=""), results_tmp$bdg)
  assign(paste("pd",run_num[hh],sep=""), results_tmp$pd)
  assign(paste("pdg",run_num[hh],sep=""), results_tmp$pdg)
  assign(paste("cd",run_num[hh],sep=""), separate_canopy_output(results_tmp$cd, 2))
  assign(paste("cdg",run_num[hh],sep=""), separate_canopy_output(results_tmp$cdg, 2))
  assign(paste("bd",run_num[hh],".wy",sep=""), results_tmp$bd.wy)
  assign(paste("bdg",run_num[hh],".wy",sep=""), results_tmp$bdg.wy)
  assign(paste("pd",run_num[hh],".wy",sep=""), results_tmp$pd.wy)
  assign(paste("pdg",run_num[hh],".wy",sep=""), results_tmp$pdg.wy)
  assign(paste("cd",run_num[hh],".wy",sep=""), {
    separate_canopy_output(results_tmp$cd, 2) %>%
      dplyr::group_by(wy, canopy_layer) %>% 
      summarise_all(mean)})
  assign(paste("cdg",run_num[hh],".wy",sep=""), {
    separate_canopy_output(results_tmp$cdg, 2) %>%
      dplyr::group_by(wy, canopy_layer) %>% 
      summarise_all(mean)})

  assign(paste("fire_size",run_num[hh],sep=""), {
    tmp <- read.table(paste("FireSizes",run_num[hh],".txt",sep=""), header = FALSE)
    names(tmp) <- c("p_burned", "year", "month", "wind_dir", "wind_speed", "n_ignitions")
    tmp
    })
}
beep(1)

# ---------------------------------------------------------------------
# Watershed-level analysis

run_num <- seq(1,15)

shed_output <- data.frame(years = double(),
                          total_fires = double(),
                          ignitions_per_year = double(),
                          fires_per_year = double(),
                          median_patches_burned = double(),
                          mean_patches_burned = double(),
                          fire_return_int = double(),
                          stringsAsFactors=FALSE)

for (ii in seq_along(run_num)){
  shed_vect = vector()
  
  fire_size <- get(paste("fire_size",run_num[ii],sep=""))
  
  # Inputs
  patches <- 926   # p301 = 926 patches
  n_months <- length(fire_size$p_burned)
  n_years <- n_months/12
  # --------------------------
  
  # Fire return interval (fri)
  fri <- sum(fire_size$p_burned)/(n_years*patches)
  fri <- (n_years*patches)/sum(fire_size$p_burned)

  # -----
  # Fire frequency
  ignitions_per_year <- sum(fire_size$n_ignitions)/n_years
  fire_events <- dplyr::filter(fire_size, p_burned > 0)
  fires_per_year <- nrow(fire_events)/n_years
  
  shed_vect <- c(years=n_years, 
                 total_fires=length(fire_events$p_burned),
                 ignitions_per_year=ignitions_per_year,
                 fires_per_year=round(fires_per_year,2),
                 median_patches_burned=median(fire_events$p_burned),
                 mean_patches_burned=round(mean(fire_events$p_burned),2),
                 fire_return_int=fri)
  shed_output <- bind_rows(shed_output, shed_vect)
}
print(shed_output)

# -----
# Watershed-level plots

fire_events <- dplyr::filter(fire_size1, p_burned > 0)

# Histogram of fire sizes
ggplot(fire_events, aes(p_burned)) +
  geom_histogram(binwidth=25)

# Seasonality of fires
ggplot(fire_events, aes(month)) +
  geom_histogram(binwidth=.5)


# ---------------------------------------------------------------------
# Patch-level analysis

# Plot height
ggplot(cd12.wy) +
  geom_line(aes(x=date, y=height, color=as.factor(canopy_layer))) +
  ylim(0,35)

# Plot plantc (plantc includes roots. Need to exclude.)
ggplot(cdg8) +
  geom_line(aes(x=date, y=plantc, color=as.factor(canopy_layer))) +
  ylim(0,11000)
  
# Plot leafc
ggplot(cdg5) +
  geom_line(aes(x=date, y=leafc, color=as.factor(canopy_layer))) +
  ylim(0,3000)

# Plot live_stemc
ggplot(cdg) +
  geom_line(aes(x=date, y=live_stemc, color=as.factor(canopy_layer)))

# Plot dead_stemc
ggplot(cdg) +
  geom_line(aes(x=date, y=dead_stemc, color=as.factor(canopy_layer))) +
  ylim(0,4000)

# Plot live_crootc
ggplot(cdg) +
  geom_line(aes(x=date, y=dead_crootc, color=as.factor(canopy_layer)))


# Plot litrc
ggplot(pdg) +
  geom_line(aes(x=date, y=litr4c))

ggplot(bdg11.wy) +
  geom_line(aes(x=date, y=litrc)) +
  ylim(0,0.9)

ggplot(results$bdg.wyd) +
  geom_line(aes(x=wyd, y=litrc))

# ---------------------------------------------------------------------
# Old


plot(results$bd$lai)
plot(results$bd$height)

plot(results$cd$height)

plot(results$pdg$litr1c)
plot(results$pdg$litr2c)
plot(results$pdg$litr3c)
plot(results$pdg$litr4c)

plot(results$cdg$cwdc)







