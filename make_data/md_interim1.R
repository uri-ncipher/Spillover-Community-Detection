###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/make_data/md_interim1.R
#
# PURPOSE:  Create first intermediate dataset with all variables needed for analysis, model selection, and derivations.
#
# INPUT:    DATA_SOURCE/TRIP_nodespublic_randomID_2022-11-14.csv
#
# OUTPUT:   ROOT/store_data/interim1
#
# NOTES:    None
#
###############################################################################################################################
###############################################################################################################################

#############################
######### LOAD DATA #########
#############################

in.nodes = read.csv(paste(DATA_SOURCE, '/TRIP_nodespublic_randomID_2022-11-14.csv', sep = ""))

set.seed(seednum)

#####################################################
###################### Outcome ######################
#####################################################

# BASELINE

in.nodes$yb = NA
in.nodes$yb[in.nodes$share.x==0 | in.nodes$share.x==96]=0
in.nodes$yb[in.nodes$share.x==1]=1
table(in.nodes$yb, useNA = "ifany")

# FOLLOW-UP

in.nodes$y = NA
in.nodes$y[in.nodes$share.y==0 | in.nodes$share.y==96]=0
in.nodes$y[in.nodes$share.y==1]=1
table(in.nodes$y, useNA = "ifany")

#####################################################
###################### Exposure###################### 
#####################################################

in.nodes$x = NA
in.nodes$x[in.nodes$opioidmeds.x==0 | in.nodes$opioidmeds.x==96]=0
in.nodes$x[in.nodes$opioidmeds.x==1]=1
table(in.nodes$x, useNA = "ifany")

#####################################################
##################### Covariates ####################
#####################################################

################################
# Socio/Demographic Info

# Age
in.nodes$age = in.nodes$age.x

# Age (z-score)
in.nodes$agez = (in.nodes$age.x - mean(in.nodes$age.x)) / sd(in.nodes$age.x)

# Sex 
in.nodes$sex=in.nodes$gender.x

# Housing
in.nodes$homeless=NA
# 1. in own house or apartment
# 2. in family for friends house and do not pay rent
# 3. in family for friends house and do pay rent
# 4. in rented house
# 5. multiple of above
# 6. homeless
# 7. other
in.nodes$homeless[in.nodes$b07.x==97]=NA # other/missing
in.nodes$homeless[in.nodes$b07.x<=5 | in.nodes$b07.x==7]=0 # not homeless
in.nodes$homeless[in.nodes$b07.x==6]=1 # homeless

# Highest Level Education
in.nodes$edu=NA
# 1.	Less than primary school 
# 2.	Primary school (or similar) 
# 3.	High school (first 3 years) 
# 4.	High school (last 3 years)  
# 5.	Institutes of Vocational Training (IEK), private universities/colleges
# 6.	Public Technological Educational Institutes or Universities 
# 7.	Postgraduate studies/PhD 
in.nodes$edu[in.nodes$b03_1.x==1 | in.nodes$b03_1.x==2]=0 # primary school or less
in.nodes$edu[in.nodes$b03_1.x==3 | in.nodes$b03_1.x==4]=1 # high school
in.nodes$edu[in.nodes$b03_1.x==5 | in.nodes$b03_1.x==6 | in.nodes$b03_1.x==7]=2 # at lease some college
in.nodes$edu = as.factor(in.nodes$edu)

# Employment Status
in.nodes$employ=NA
# 1.	Employed full-time
# 2.	Employed part-time
# 3.	Run my own business
# 4.	Have occasional earnings
# 5.	Homemaker (somebody who manages his or her own home as a primary job
# 6.	Full-time student
# 7.	Retired
# 8.	Not employed and looking for work
# 9.	Unable to work for health reasons
# 10. Other (b04a)
in.nodes$employ[in.nodes$b04.x==8] = 1 # unemployed and looking for work
in.nodes$employ[in.nodes$b04.x==9] = 2 # unable to work for health reasons
in.nodes$employ[in.nodes$b04.x==1 | in.nodes$b04.x==2 | in.nodes$b04.x==3 |
                  in.nodes$b04.x==4 | in.nodes$b04.x==5 | in.nodes$b04.x==6 |
                  in.nodes$b04.x==7 | in.nodes$b04.x==10] = 0 # otherwise

# Marital Status
in.nodes$married = NA
# 1.	Married and living with a husband or wife
# 2.	Married, but not living together with either a husband or a wife, and living with another sexual partner
# 3.	Married but do not live either with my husband or wife nor with another sexual partner
# 4.	Officially not married, but living with sexual partner
# 5.	Officially not married and not living with sexual partner
in.nodes$married[in.nodes$b06.x==97]=NA # not asked
in.nodes$married[in.nodes$b06.x==1 | in.nodes$b06.x==2 | in.nodes$b06.x==3]=0 # married
in.nodes$married[in.nodes$b06.x==4 | in.nodes$b06.x==5]=1 # not married

# Nationality
in.nodes$nat = NA
# 1.	Greek
# 2.	Afghan
# 3.	African (specify)
# 4.	Albanian
# 5.	Arab (specify)
# 6.	Bulgarian
# 7.	Georgian
# 8.	Iranian
# 9.	Kurd
# 10.	Pakistani
# 11.	Romanian
# 12.	Russian
# 13.	Ukrainian
# 14.	Other European (specify)
# 15.	Other  (B05_1a) specify
in.nodes$nat[in.nodes$b05_1.x>=2]=0
in.nodes$nat[in.nodes$b05_1.x==1]=1

################################
# Substance use / health-related

# Injected any drugs in Last 6 Months
in.nodes$inject=in.nodes$c18.x

# Cocaine use in Last 6 Months
in.nodes$cocaine = NA
in.nodes$cocaine[in.nodes$c16j.x<=4]=0 # less than once per day
in.nodes$cocaine[in.nodes$c16j.x>=5]=1 # once per day or more

# Heroin Use in Last 6 Months
in.nodes$heroin = NA
in.nodes$heroin[in.nodes$c16n.x==0]=0 # never
in.nodes$heroin[in.nodes$c16n.x>0]=1 # a few times or more

# Crack use in Last 6 Months
in.nodes$crack = NA
in.nodes$crack[in.nodes$c16k.x==0]=0 # never
in.nodes$crack[in.nodes$c16k.x>0]=1 # a few times or more

# Methamphetamine Use in Last 6 Months
in.nodes$meth = NA
in.nodes$meth[in.nodes$c16h.x==0]=0 # never
in.nodes$meth[in.nodes$c16h.x>0]=1 # a few times or more

# Alcohol Use in Last 6 Months
in.nodes$alcohol = NA
in.nodes$alcohol[in.nodes$c16a.x<=4]=0 # less than once per day
in.nodes$alcohol[in.nodes$c16a.x>4]=1 # once per day or more

# Able to Get Medical Care When Needed
in.nodes$care = NA
# 0. strongly Disagree
# 1. somewhat Disagree
# 2. neither agree nor disagree
# 3. somewhat agree
# 4. strongly agree
in.nodes$care[in.nodes$g01.x==97 | in.nodes$g01.x==99]=NA
in.nodes$care[in.nodes$g01.x==0 | in.nodes$g01.x==1]=1 # Unable to Get Medical Care
in.nodes$care[in.nodes$g01.x==2 | in.nodes$g01.x==3 | in.nodes$g01.x==4]=0 # Otherwise

################################
# HIV-related

# Participant Status
in.nodes$type=NA
in.nodes$type[in.nodes$participant.x=="LONG  TERM CONTROL"]="LTC"
in.nodes$type[in.nodes$participant.x=="NEGATIVE CONTROL"]="NC"
in.nodes$type[in.nodes$participant.x=="SEED"]="S"
in.nodes$type[in.nodes$participant.x=="NETWORK MEMBER  OF LONG TERM CONTROL"]="MLTC"
in.nodes$type[in.nodes$participant.x=="NETWORK MEMBER OF SEED"]="MS"

# HIV Status
in.nodes$hiv=NA
in.nodes$hiv[in.nodes$posneg.x=="NEG"]=0
in.nodes$hiv[in.nodes$posneg.x=="POS"]=1


#####################################################
##################### Formatting ####################
#####################################################

# Formatting
in.nodes$id         = as.factor(in.nodes$EGO.ID)
in.nodes$edu        = as.factor(in.nodes$edu)
in.nodes$employ     = as.factor(in.nodes$employ)
in.nodes$type       = as.factor(in.nodes$type)

# For ID 3993, select the record where baseline and follow-up are closer being 6 months apart.

in.nodes$remove_rec = NA
in.nodes$remove_rec[in.nodes$EGO.ID==3993 & in.nodes$a10.x=="2014-01-28" & in.nodes$a10.y=="2015-01-05"]="Y"
in.nodes[in.nodes$id==3993, c("id", "a10.x", "a10.y", "remove_rec")]

interim1 = in.nodes[is.na(in.nodes$remove_rec),]

# Dataset with key variables only

interim1 = subset(interim1, select = c("id",
                                          "yb", 
                                          "y", 
                                          "x",
                                          "age",
                                          "agez",
                                          "sex",
                                          "homeless",
                                          "edu",
                                          "employ",
                                          "married",
                                          "nat",
                                          "cocaine",
                                          "heroin", 
                                          "crack",
                                          "meth",
                                          "alcohol",
                                          "care",
                                          "hiv",
                                          "type",
                                          "inject"))

#####################################################
################### SAVE OUTPUTS ####################
#####################################################

save(interim1, file = paste(ROOT, "/store_data/interim1", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "DATA_SOURCE", "seednum", "vars")))

   