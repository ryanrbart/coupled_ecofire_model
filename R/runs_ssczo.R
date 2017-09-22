# Runs for ssczo meeting

runs <- data.frame(par1="biomass_loss_rel_k2", value1 = 1, par2 = "-", value2 = NA, seed=106, comment = "-", stringsAsFactors = FALSE)
runs[2,] <- list(par1="biomass_loss_rel_k2", value1 = 0.5, par2 = "-", value2 = NA, seed=106, comment = "-")
runs[3,] <- list(par1="biomass_loss_rel_k2", value1 = 0.75, par2 = "-", value2 = NA, seed=106, comment = "-")
runs[4,] <- list(par1="biomass_loss_rel_k2", value1 = 0.9, par2 = "-", value2 = NA, seed=106, comment = "-")
runs[5,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "-", value2 = NA, seed=107, comment = "-")
runs[6,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "vapor_loss_rel", value2 = 100, seed=107, comment = "Lots of litter")
runs[7,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "vapor_loss_rel", value2 = 0.01, seed=107, comment = "Little litter")
runs[8,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "-", value2 = NA, seed=107, comment = "3-degree warming")
runs[9,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "-", value2 = NA, seed=108, comment = "3-degree warming")
runs[10,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "mean_ign", value2 = 0.5, seed=110, comment = "3-degree warming")
runs[11,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "mean_ign", value2 = 2.0, seed=110, comment = "3-degree warming")
runs[12,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "-", value2 = NA, seed=110, comment = "3-degree warming")
runs[13,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "-", value2 = NA, seed=110, comment = "-")
runs[14,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "mean_ign", value2 = 0.5, seed=110, comment = "-")
runs[15,] <- list(par1="biomass_loss_rel_k2", value1 = 1, par2 = "mean_ign", value2 = 2.0, seed=110, comment = "-")


print(runs)



