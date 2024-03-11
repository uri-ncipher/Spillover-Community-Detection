###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/sensitivity/make_data/sense_md_finaldata.R
#
# PURPOSE:  Create final dataset where missing values for the outcome are imputed based on predictive model. 
#
# INPUT:    ROOT/store_data/interim3
#           ROOT/sensitivity/store_data/impute_y_formula_cfg
#
# OUTPUT:   ROOT/sensitivity/store_data/sense_finaldata
#
# NOTES:    none
#
###############################################################################################################################
###############################################################################################################################

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/interim3", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/impute_y_formula_cfg", sep = ""), envir = .GlobalEnv)

sense_finaldata = interim3
data_ycom = sense_finaldata[!is.na(sense_finaldata$y),]

set.seed(seednum)

#######################################################
#  IMPUTE MISSING Y BASED ON PREDICTIVE GLM MODELS  #
#######################################################

# Remove the random effect for the community from the formula
impute_y_formula = gsub('\\+ \\(1 \\| clusterCFG)', '', impute_y_formula_cfg)
impute_y_formula = paste(impute_y_formula[[2]], impute_y_formula[[1]], impute_y_formula[[3]])

# Use the same predictors as for the outcome imputation in the main analysis,
# but with no random effect for the communities.

compmod = glm(impute_y_formula
              , family = binomial(link="logit"), data=data_ycom)
y_prob = predict(compmod, type="response", newdata=sense_finaldata)
y_gen = rbinom(nrow(sense_finaldata), 1, y_prob)
sense_finaldata$y_imputed = ifelse(is.na(sense_finaldata$y), y_gen, sense_finaldata$y)


save(sense_finaldata, file = paste(ROOT, "/sensitivity/store_data/sense_finaldata", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))
