###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/model_tables.R
#
# PURPOSE:  Output model results to table for LaTeX. 
#
# INPUT:    ROOT/store_data/imp_cfg, nul_cfg, alt_cfg, imp_lec, nul_lec, alt_lec.
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: inferference, stargazer. 
#           The function 'gaze2mods' uses the 'stargazer' package to create one table of results for 2 models 
#           placing results from both models side by side. Executing the function produces text to be copied
#           and pasted into LaTeX.
# 
###############################################################################################################################
###############################################################################################################################

library(inferference)
library(stargazer)

gaze2mods = function(mod1, mod2){
  
  alpha1 = cbind(format(round(c(cbind(
    direct_effect(mod1)[1,1], direct_effect(mod1)[2,1], direct_effect(mod1)[3,1]),
    indirect_effect(mod1, .60, .20)[,1],
    indirect_effect(mod1, .4, .20)[,1],
    indirect_effect(mod1, .60, .4)[,1],
    total_effect(mod1, .60, .20)[,1],
    total_effect(mod1, .4, .20)[,1],
    total_effect(mod1, .60, .4)[,1],
    overall_effect(mod1, .60, .20)[,1],
    overall_effect(mod1, .4, .20)[,1],
    overall_effect(mod1, .60, .4)[,1]), 2), nsmall=2))
  
  alpha2 = cbind(format(round(c(cbind(
    direct_effect(mod1)[1,3], direct_effect(mod1)[2,3], direct_effect(mod1)[3,3]),
    indirect_effect(mod1, .60, .20)[,3],
    indirect_effect(mod1, .4, .20)[,3],
    indirect_effect(mod1, .60, .4)[,3],
    total_effect(mod1, .60, .20)[,3],
    total_effect(mod1, .4, .20)[,3],
    total_effect(mod1, .60, .4)[,3],
    overall_effect(mod1, .60, .20)[,3],
    overall_effect(mod1, .4, .20)[,3],
    overall_effect(mod1, .60, .4)[,3]), 2), nsmall=2))
  
  Estimates1 = cbind(format(round(c(cbind(
    -direct_effect(mod1)[1,5], -direct_effect(mod1)[2,5], -direct_effect(mod1)[3,5]),
    indirect_effect(mod1, .60, .20)[,5],
    indirect_effect(mod1, .4, .20)[,5],
    indirect_effect(mod1, .60, .4)[,5],
    -total_effect(mod1, .20, .60)[,5],
    -total_effect(mod1, .20, .4)[,5],
    -total_effect(mod1, .4, .60)[,5],
    overall_effect(mod1, .60, .20)[,5],
    overall_effect(mod1, .4, .20)[,5],
    overall_effect(mod1, .60, .4)[,5]), 2), nsmall=2))
  
  Lbound01 = cbind(format(round(c(cbind(
    -direct_effect(mod1)[1,8], -direct_effect(mod1)[2,8], -direct_effect(mod1)[3,8]),
    indirect_effect(mod1, .60, .20)[,7],
    indirect_effect(mod1, .4, .20)[,7],
    indirect_effect(mod1, .60, .4)[,7],
    -total_effect(mod1, .20, .60)[,8],
    -total_effect(mod1, .20, .4)[,8],
    -total_effect(mod1, .4, .60)[,8],
    overall_effect(mod1, .60, .20)[,7],
    overall_effect(mod1, .4, .20)[,7],
    overall_effect(mod1, .60, .4)[,7]), 2), nsmall=2))
  
  Lbound1 = cbind(paste(rep("(", 12), Lbound01, sep = ""))
  
  Ubound01 = cbind(format(round(c(cbind(
    -direct_effect(mod1)[1,7], -direct_effect(mod1)[2,7], -direct_effect(mod1)[3,7]),
    indirect_effect(mod1, .60, .20)[,8],
    indirect_effect(mod1, .4, .20)[,8],
    indirect_effect(mod1, .60, .4)[,8],
    -total_effect(mod1, .20, .60)[,7],
    -total_effect(mod1, .20, .4)[,7],
    -total_effect(mod1, .4, .60)[,7],
    overall_effect(mod1, .60, .20)[,8],
    overall_effect(mod1, .4, .20)[,8],
    overall_effect(mod1, .60, .4)[,8]), 2), nsmall=2))
  
  Ubound1 = cbind(paste(Ubound01, rep(")",12)))
  
  Estimates2 = cbind(format(round(c(cbind(
    -direct_effect(mod2)[1,5], -direct_effect(mod2)[2,5], -direct_effect(mod2)[3,5]),
    indirect_effect(mod2, .60, .20)[,5],
    indirect_effect(mod2, .4, .20)[,5],
    indirect_effect(mod2, .60, .4)[,5],
    -total_effect(mod2, .20, .60)[,5],
    -total_effect(mod2, .20, .4)[,5],
    -total_effect(mod2, .4, .60)[,5],
    overall_effect(mod2, .60, .20)[,5],
    overall_effect(mod2, .4, .20)[,5],
    overall_effect(mod2, .60, .4)[,5]), 2), nsmall=2))
  
  Lbound02 = cbind(format(round(c(cbind(
    -direct_effect(mod2)[1,8], -direct_effect(mod2)[2,8], -direct_effect(mod2)[3,8]),
    indirect_effect(mod2, .60, .20)[,7],
    indirect_effect(mod2, .4, .20)[,7],
    indirect_effect(mod2, .60, .4)[,7],
    -total_effect(mod2, .20, .60)[,8],
    -total_effect(mod2, .20, .4)[,8],
    -total_effect(mod2, .4, .60)[,8],
    overall_effect(mod2, .60, .20)[,7],
    overall_effect(mod2, .4, .20)[,7],
    overall_effect(mod2, .60, .4)[,7]), 2), nsmall=2))
  
  Lbound2 = cbind(paste(rep("(", 12), Lbound02, sep = ""))
  
  Ubound02 = cbind(format(round(c(cbind(
    -direct_effect(mod2)[1,7], -direct_effect(mod2)[2,7], -direct_effect(mod2)[3,7]),
    indirect_effect(mod2, .60, .20)[,8],
    indirect_effect(mod2, .4, .20)[,8],
    indirect_effect(mod2, .60, .4)[,8],
    -total_effect(mod2, .20, .60)[,7],
    -total_effect(mod2, .20, .4)[,7],
    -total_effect(mod2, .4, .60)[,7],
    overall_effect(mod2, .60, .20)[,8],
    overall_effect(mod2, .4, .20)[,8],
    overall_effect(mod2, .60, .4)[,8]), 2), nsmall=2))
  
  Ubound2 = cbind(paste(Ubound02, rep(")",12)))
  
  bounds1 = gsub(" )",")", paste(Lbound1,rep(", ",12), Ubound1, sep = ""))
  bounds2 = gsub(" )",")", paste(Lbound2,rep(", ",12), Ubound2, sep = ""))
  
  rnames1 = cbind(c("Direct", "Direct", "Direct", 
                    "Spillover", "Spillover", "Spillover", 
                    "Total", "Total", "Total", 
                    "Overall", "Overall", "Overall"))
  rnames = paste(rnames1, rep(" (",12), alpha1, rep(", ",12), alpha2, rep(")",12), sep = "")
  cnames = c("Effect", "RD", "95% CI", "RD", "95% CI")
  
  output = matrix(cbind(rnames, Estimates1, bounds1, Estimates2, bounds2), nrow = 12, ncol = 5, byrow = F)
  colnames(output) = cnames
  stargazer(output, digits = 2, colnames = T, rownames = F)
  
}

load(file = paste(ROOT, "/store_data/nul_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/nul_lec", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/alt_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/alt_lec", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_cfg", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/imp_lec", sep = ""), envir = .GlobalEnv)

gaze2mods(imp_cfg, imp_lec)

gaze2mods(alt_cfg, alt_lec)

gaze2mods(nul_cfg, nul_lec)

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))
