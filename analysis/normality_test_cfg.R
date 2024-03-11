###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/normality_test_cfg.R
#
# PURPOSE:  Test for a normal distribution of the random effects of CFG communities
#
# INPUT:    ROOT/store_data/finaldata
#           ROOT/store_data/final_formula_cfg
#
# OUTPUT:   None
#
# NOTES:    Libraries to load: lme4, survival
# 
###############################################################################################################################
###############################################################################################################################

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/final_formula_cfg", sep = ""), envir = .GlobalEnv)

library(lme4)
library(survival)

set.seed(seednum)


# Remove the 'y|imputed_cfg' 
glmr_form_cfg = gsub('y_imputed_cfg \\| x', 'x', final_formula_cfg)
# Remove the '1 | clusterCFG' 
glmr_form_cfg = gsub('\\(1 \\| clusterCFG\\) \\| clusterCFG', '\\(1 \\| clusterCFG\\)', glmr_form_cfg)
glmr_form_cfg = paste(glmr_form_cfg[[2]], glmr_form_cfg[[1]], glmr_form_cfg[[3]])

### Fit the GLMM model

glmm.cfg <- glmer(
  formula = glmr_form_cfg,
  family = "binomial",
  data = finaldata
)

# Swap '(1|clusterCFG)' for 'strata(clusterCFG)' in the formula for the clogit model.
strat_form_cfg = gsub('\\(1 \\| clusterCFG\\)', 'strata(clusterCFG)', glmr_form_cfg)
strat_form_cfg = formula(paste(strat_form_cfg, collapse = "+"))

### Fit the conditional Logit model

clogit.cfg <- clogit(
  formula = strat_form_cfg,
  data = finaldata,
  method = "exact"
)


### Create a function

calc_TC_statistic = function(cl, gl, degrees_of_freedom){ idx=c(2:length(fixef(gl)))

if ("glmerMod" %in% class(gl)){
  message('glmer')
  
  foo_delta = cl$coefficients - lme4::getME(gl, "fixef")[idx]
  
  glmm_vcov = vcov(gl)[idx, idx]
}

if ("HLfit" %in% class(gl)){
  message('spaMM')
  
  foo_delta = cl$coefficients - gl$fixef[idx]
  
  glmm_vcov = vcov(gl)[idx, idx]
}

foo_sigma_M = cl$var - glmm_vcov

TC_stat = as.numeric(t(foo_delta) %*% solve(foo_sigma_M) %*% foo_delta)
p_value = pchisq(TC_stat, df = degrees_of_freedom, lower.tail = F)

output = list(
  results = data.frame(D = TC_stat, p=p_value, stringsAsFactors = FALSE)
  , mats = list(delta = foo_delta, sigma = foo_sigma_M)
)
output
}

calculated_tc_statistic = calc_TC_statistic(
  cl=clogit.cfg
  , gl = glmm.cfg
  , degrees_of_freedom = length(fixef(glmm.cfg))
)

normality_pval_cfg = calculated_tc_statistic$results

normality_pval_cfg

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))


