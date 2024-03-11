###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/coverage.R
#
# PURPOSE:  Print observed coverage levels.
#
# INPUT:    ROOT/store_data/finaldata
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference. 
# 
###############################################################################################################################
###############################################################################################################################

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)

cfg_coms = as.data.frame(aggregate(x ~ clusterCFG, data = finaldata, FUN = mean))
lec_coms = as.data.frame(aggregate(x ~ clusterLEC, data = finaldata, FUN = mean))

hist(cfg_coms$x, breaks = 30, xlim = c(0,1))
hist(lec_coms$x, breaks = 30, xlim = c(0,1))

