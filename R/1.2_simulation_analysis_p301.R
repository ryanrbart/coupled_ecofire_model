# Code for testing the coupled model in p301
#
# 

source("R/0.1_utilities.R")

#theme_set(theme_bw(base_size = 11))
#theme_set(theme_bw(base_size = 16))


# ---------------------------------------------------------------------
# P301 coupled model data processing

results <- readin_rhessys_output("ws_p301/out/1.1_p301_simulation/p301_simulation6", b=1, p=1, c=1, g=1)
bd <- results$bd 
bdg <- results$bdg
pd <- results$pd
pdg <- results$pdg
cd <- separate_canopy_output(results$cd, 2)
cdg <- separate_canopy_output(results$cdg, 2)

fire_sizes <- read.table("FireSizes6.txt", header = FALSE)
names(fire_sizes) <- c("p_burned", "year", "month", "wind1", "wind2", "ignitions")
#head(fire_sizes)
beep(1)

# ---------------------------------------------------------------------
# Watershed-level analysis

# Inputs
patches <- 926   # p301 = 926 patches
n_months <- length(fire_sizes$p_burned)
n_years <- n_months/12
print(paste("Years:", round(n_years,2)))

# --------------------------

# Fire return interval (fri)
fri <- sum(fire_sizes$p_burned)/(n_years*patches)
fri <- (n_years*patches)/sum(fire_sizes$p_burned)
print(paste("Fire Return Interval (years):", round(fri,2)))

# -----
# Fire frequency
fire_events <- dplyr::filter(fire_sizes, p_burned > 0)
print(paste("Total fire events:", length(fire_events$p_burned)))
fires_per_year <- nrow(fire_events)/n_years
print(paste("Fires per year:", round(fires_per_year,2)))

# -----
# Fire Size
fire_events <- dplyr::filter(fire_sizes, p_burned > 0)
print(paste("Mean patches burned:", round(mean(fire_events$p_burned),2)))
print(paste("Median patches burned:", median(fire_events$p_burned)))

# Histogram of fire sizes
ggplot(fire_events, aes(p_burned)) +
  geom_histogram(binwidth=25)
#plot(fire_sizes$p_burned, log="y")

# -----
# Seasonality of fires
ggplot(fire_events, aes(month)) +
  geom_histogram(binwidth=.5)



# ---------------------------------------------------------------------
# Patch-level analysis

# Plot height
ggplot(cd) +
  geom_line(aes(x=date, y=height, color=as.factor(canopy_layer))) +
  ylim(0,35)

# Plot plantc (plantc includes roots. Need to exclude.)
ggplot(cdg) +
  geom_line(aes(x=date, y=plantc, color=as.factor(canopy_layer))) +
  geom_hline(aes(yintercept = 700)) +
  geom_hline(aes(yintercept = 500))
  
# Plot leafc
ggplot(cdg) +
  geom_line(aes(x=date, y=leafc, color=as.factor(canopy_layer)))

# Plot live_stemc
ggplot(cdg) +
  geom_line(aes(x=date, y=live_stemc, color=as.factor(canopy_layer)))

# Plot dead_stemc
ggplot(cdg) +
  geom_line(aes(x=date, y=dead_stemc, color=as.factor(canopy_layer)))

# Plot live_crootc
ggplot(cdg) +
  geom_line(aes(x=date, y=dead_crootc, color=as.factor(canopy_layer)))


# Plot litrc
ggplot(pdg) +
  geom_line(aes(x=date, y=litr4c))

ggplot(bdg) +
  geom_line(aes(x=date, y=litrc)) +
  geom_hline(aes(yintercept = 0.7))

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







