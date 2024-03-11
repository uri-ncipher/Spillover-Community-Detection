###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/ms_propscores.R
#
# PURPOSE:  Fit propensity score models under CFG and LEC communities. 
#
# INPUT:    ROOT/store_data/finaldata
#
# OUTPUT:   ROOT/store_data/final_formula_cfg 
#           ROOT/store_data/imp_cfg
#           ROOT/store_data/final_formula_lec
#           ROOT/store_data/imp_lec
#
# NOTES:    Libraries to load: inferference
#           Community 213 has only 1 member and therefore not subject to dissemination, and therefore is removed from LEC models.# 
###############################################################################################################################
###############################################################################################################################

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)

library(inferference)

set.seed(seednum)

# Remove variables not included in the propensity score model based on model selection
varsp = vars[! vars %in% c(
  # < x1, x2, ...>
)]

# CFG
nvarsp = length(varsp)
mat = as.data.frame(t(combn(varsp, nvarsp)))
mat[,ncol(mat)+1] = c("yb")
mat[,ncol(mat)+1] = c("(1|clusterCFG)|clusterCFG")
mat= as.matrix(mat)
final_formula_cfg = reformulate(mat[1,], response = "y_imputed_cfg|x")

imp_cfg = interference(formula = final_formula_cfg,
                       allocations = c(0.20, 0.40, 0.60),
                       data = finaldata,
                       options = list(maxiter=100000),
                       randomization = 1, 
                       method = 'simple')

# LEC

# Data processing to remove row where 1 person is the only member of their community.
finaldata_l = finaldata
finaldata_l$clusterLEC[which(finaldata_l$clusterLEC == "213")]=NA
finaldata_l$clusterLEC2 = as.numeric(as.character(finaldata_l$clusterLEC))
finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)] = finaldata_l$clusterLEC2[finaldata_l$clusterLEC2>213 & !is.na(finaldata_l$clusterLEC2)]-1
finaldata_l$clusterLEC = NA
finaldata_l$clusterLEC = as.factor(finaldata_l$clusterLEC2)
finaldata_l = finaldata_l[which(!is.na(finaldata_l$clusterLEC)),]

mat = as.data.frame(t(combn(varsp, nvarsp)))
mat[,ncol(mat)+1] = c("yb")
mat[,ncol(mat)+1] = c("(1|clusterLEC)|clusterLEC")
mat= as.matrix(mat)
final_formula_lec = reformulate(mat[1,], response = "y_imputed_lec|x")

imp_cfg = interference(formula = final_formula_cfg,
                       allocations = c(0.20, 0.40, 0.60),
                       data = finaldata,
                       options = list(maxiter=100000),
                       randomization = 1, 
                       method = 'simple')

# Save models and formulas

save(final_formula_cfg, file = paste(ROOT, "/store_data/final_formula_cfg", sep = ""))
save(imp_cfg, file = paste(ROOT, "/store_data/imp_cfg", sep = ""))

save(final_formula_lec, file = paste(ROOT, "/store_data/final_formula_lec", sep = ""))
save(imp_lec, file = paste(ROOT, "/store_data/imp_lec", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars", "varsp")))

