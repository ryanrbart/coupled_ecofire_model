# Code for testing the coupled model in p301
#
# 

source("R/0.1_utilities.R")

#theme_set(theme_bw(base_size = 11))
theme_set(theme_bw(base_size = 24))


# ---------------------------------------------------------------------
# P301 coupled model data processing

# Code for reading in broken output
# bd <- read_table2(paste("ws_p301/out/1.1_p301_simulation/run5/p301_simulation_basin.daily", sep=""))
# bdg <- read_table2(paste("ws_p301/out/1.1_p301_simulation/run5/p301_simulation_grow_basin.daily", sep=""))
# cd <- read_table2(paste("ws_p301/out/1.1_p301_simulation/run7/p301_simulation_stratum.daily", sep=""))
# cdg <- read_table2(paste("ws_p301/out/1.1_p301_simulation/run7/p301_simulation_grow_stratum.daily", sep=""))
# plot(cd$height)
git

run_num <- c(8)

# Import rhessys output
for (hh in seq_along(run_num)){
  print(paste("Importing number",run_num[hh]))
  results_tmp <- readin_rhessys_output(paste("ws_p301/out/1.1_p301_simulation/run",run_num[hh],"/p301_simulation", sep=""), b=1, p=1, c=1, g=1)
  assign(paste("bd",run_num[hh],sep=""), head(results_tmp$bd,-1))
  assign(paste("bdg",run_num[hh],sep=""), head(results_tmp$bdg,-1))
  assign(paste("pd",run_num[hh],sep=""), head(results_tmp$pd,-1))
  assign(paste("pdg",run_num[hh],sep=""), head(results_tmp$pdg,-1))
  assign(paste("cd",run_num[hh],sep=""), separate_canopy_output(head(results_tmp$cd,-2), 2))
  assign(paste("cdg",run_num[hh],sep=""), separate_canopy_output(head(results_tmp$cdg,-2), 2))
  assign(paste("bd",run_num[hh],".wy",sep=""), head(results_tmp$bd.wy,-1))
  assign(paste("bdg",run_num[hh],".wy",sep=""), head(results_tmp$bdg.wy,-1))
  assign(paste("pd",run_num[hh],".wy",sep=""), head(results_tmp$pd.wy,-1))
  assign(paste("pdg",run_num[hh],".wy",sep=""), head(results_tmp$pdg.wy,-1))
  assign(paste("cd",run_num[hh],".wy",sep=""), {
    separate_canopy_output(head(results_tmp$cd,-2), 2) %>%
      dplyr::group_by(wy, canopy_layer) %>%
      summarise_all(mean)})
  assign(paste("cdg",run_num[hh],".wy",sep=""), {
    separate_canopy_output(head(results_tmp$cdg,-2), 2) %>%
      dplyr::group_by(wy, canopy_layer) %>%
      summarise_all(mean)})
}

run_num <- seq(1,9)
# Import fire sizes 
for (hh in seq_along(run_num)){
  assign(paste("fire_size",run_num[hh],sep=""), {
    tmp <- read.table(paste("FireSizes",run_num[hh],".txt",sep=""), header = FALSE)
    names(tmp) <- c("p_burned", "year", "month", "wind_dir", "wind_speed", "n_ignitions")
    #tmp <- mutate(tmp, Date = zoo::as.yearmon(paste(year,"-", month, sep="")))
    tmp <- mutate(tmp, Date = ymd(paste(year,"-", month, "-", rep(1,nrow(tmp)), sep="")))
    tmp <- mutate(tmp, percent_burn = pmin(p_burned/926*100,100))
    tmp
    })
}
beep(1)

# ---------------------------------------------------------------------
# Watershed-level analysis

#run_num <- seq(1,9)
run_num <- 8

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

# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# ---------------------------------------------------------------------
# Watershed-level plots

# Timeseries
x <- ggplot() +
  geom_line(data=fire_size8, aes(x=Date, y=percent_burn), col="chocolate4") +
  labs(title = "", x = "Year", y="Percent Watershed Burnt") +
  ggtitle("Wildfire Timeseries")+
  theme(legend.position="none")
print(x)
ggsave("sierra_watershed_timeseries_fire_size.eps", plot = x, path = "outputs/")


# ---
# Remove non-fire events
fire_events <- dplyr::filter(fire_size8, p_burned > 0)

# Histogram of percent burned
x <- ggplot(fire_events, aes(percent_burn)) +
  geom_histogram(binwidth=2, fill="chocolate", col="chocolate4") +
  labs(title = "Size of Wildfires", x = "Percent Watershed Burnt", y="Number of Fires")
print(x)
ggsave("sierra_watershed_hist_percent_burn.eps", plot = x, path = "outputs/")

x_breaks <- seq(1,12)
x_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Seasonality of fires
x <- ggplot(fire_events, aes(month)) +
  geom_histogram(binwidth=.5, fill="chocolate", col="chocolate4") +
  scale_x_continuous(name="Month", breaks=x_breaks, labels=x_labels) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Seasonality of Wildfires", y="Number of Fire Starts") +
  expand_limits(x=c(1,12), y=c(0,100))
print(x)
ggsave("sierra_watershed_seasonality_fire_sizes.eps", plot = x, path = "outputs/")



# ---------------------------------------------------------------------
# Patch-level analysis

# Plot height
x <- ggplot(cd8) +
  geom_line(aes(x=date, y=height, color=as.factor(canopy_layer)), size=1.4) +
  ylim(0,35) +
  labs(title = "Vegetation Timeseries", x="Year", y="Vegetation Height (m)") +
  scale_color_brewer(palette  = "Dark2", 
                     name ="Canopy",
                     breaks = c(1,2),
                     labels=c("Upper", "Lower"))
 # theme(legend.position="none")
print(x)
ggsave("sierra_patch_timeseries_height.eps", plot = x, path = "outputs/")

# Plot litrc from patch

pdg8_dummy <- cbind(pdg8, dummy = rep(1,nrow(pdg8)))

x <- ggplot(pdg8_dummy) +
  geom_line(aes(x=date, y=(litr1c+litr2c+litr3c+litr4c), color = as.factor(dummy))) +
  labs(title = "Litter Timeseries", x="Year", y="Litter (kg/m2)") +
  # theme(
  #   legend.text = element_text(color="white"),
  #   legend.title = element_text(color="white"),
  #   legend.key = element_rect(fill="white")) +
  scale_color_brewer(palette = "Dark2") +
                     # name = "Canopy",
                     # guide = guide_legend(override.aes = list(color="white")))
  theme(legend.position="none")
print(x)
ggsave("sierra_patch_timeseries_litter.eps", plot = x, path = "outputs/")





# -------------------

# Plot plantc (plantc includes roots. Need to exclude.)
ggplot(cdg8) +
  geom_line(aes(x=date, y=plantc, color=as.factor(canopy_layer))) +
  #ylim(0,300) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")
  #geom_vline(aes(xintercept=2275))

# Plot leafc
ggplot(cdg8) +
  geom_line(aes(x=date, y=leafc, color=as.factor(canopy_layer))) +
  ylim(0,3000) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

# Plot live_stemc
ggplot(cdg8.wy) +
  geom_line(aes(x=date, y=live_stemc, color=as.factor(canopy_layer))) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

# Plot dead_stemc
ggplot(cdg8.wy) +
  geom_line(aes(x=date, y=dead_stemc, color=as.factor(canopy_layer))) +
  #ylim(0,4000) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

# Plot live_crootc
ggplot(cdg8.wy) +
  geom_line(aes(x=date, y=dead_crootc, color=as.factor(canopy_layer))) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")


# Plot litr1c
ggplot(pdg8) +
  geom_line(aes(x=date, y=litr1c)) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

# Plot litr2c
ggplot(pdg8) +
  geom_line(aes(x=date, y=litr2c)) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

# Plot litr3c
ggplot(pdg8) +
  geom_line(aes(x=date, y=litr3c)) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

# Plot litr4c
ggplot(pdg8) +
  geom_line(aes(x=date, y=litr4c)) +
  #xlim(ymd("2231-1-1"),ymd("2288-1-1")) +
  theme(legend.position="none")

# Plot litrc from patch
ggplot(pdg8) +
  geom_line(aes(x=date, y=(litr1c+litr2c+litr3c+litr4c))) +
  #xlim(ymd("2251-1-1"),ymd("2288-1-1")) +
  theme(legend.position="none")

max(pdg8$litr1c+pdg8$litr2c+pdg8$litr3c+pdg8$litr4c)

ggplot(bdg8) +
  geom_line(aes(x=date, y=litrc)) +
  #ylim(0,0.9) +
  #xlim(ymd("2271-1-1"),ymd("2278-1-1")) +
  theme(legend.position="none")

ggplot(bdg8.wy) +
  geom_line(aes(x=wy, y=litrc)) +
  theme(legend.position="none")

# ----------------







# ---------------------------------------------------------------------


# SSCZO plots

fire_size_czo <- bind_rows(Baseline = fire_size5, Warming = fire_size8, .id=c("Temperature"))
fire_size_czo <- mutate(fire_size_czo, Date = zoo::as.yearmon(paste(year,"-", month, sep="")))
fire_size_czo <- mutate(fire_size_czo, percent_burn = p_burned/926*100)

x <- ggplot(fire_size_czo) +
  geom_line(aes(x=Date, y=percent_burn)) +
  labs(title = "Wildfire Time-series", y = "Percent Watershed Burned") +
  #ylim(0,100) +
  facet_grid(Temperature~.)
plot(x)
ggsave("time-series.pdf",plot = x, path = "outputs/ssczo_figures")







