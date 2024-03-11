###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/voi_CFG_at_10_20.R
#
# PURPOSE:  Determine what VOI is when 10% and 20% of nodes are randomly selected,
#           and randomly reassigned to different communities after having been determined by CFG. 
#
# INPUT:    ROOT/store_data/net_gc
#           ROOT/store_data/finaldata
#
# OUTPUT:   ROOT/store_data/VOI_CFG_10.R, VOI_CFG_20.R.
#
# NOTES:    Libraries to load: mcclust, igraph
#
###############################################################################################################################
###############################################################################################################################

library(mcclust)
library(igraph)

load(file = paste(ROOT, "/store_data/net_gc", sep = ""), envir = .GlobalEnv)
load(file = paste(ROOT, "/store_data/finaldata", sep = ""), envir = .GlobalEnv)

set.seed(seednum)

# Extract the community structure under CFG for the giant component
Xreal = as.matrix(get.adjacency(net_gc))
Xrealg = graph.adjacency(as.matrix(Xreal), mode = "undirected")
RealComm = cluster_fast_greedy(Xrealg, merges = T, modularity = T, membership = T, weights = E(Xrealg)$weight)$membership

# Subset main final dataset to IDs and CFG communities
finaldata = subset(x = finaldata[as.numeric(as.character(finaldata$clusterCFG))>200,], select = c("id", "clusterCFG"))

# Assign row numbers as new IDs for easier random selection of nodes
finaldata$newid = 1:nrow(finaldata)

# 10% and 20% of participants to reassign
pcts = as.list(c(round(nrow(finaldata)*.10,0), round(nrow(finaldata)*.20,0)))

# A list to contain the two VOIs (10% and 20% reassignment)
VOI_CFG = vector("list", length = 2)

for (i in 1:2){
# Randomly select 10% or 20% of participants to have communities reassigned and put in separate data frame
selected = as.data.frame(cbind(sample(finaldata$newid, pcts[[i]], replace = F)))
selected$V2 = "Y"
colnames(selected) = c("newid", "Reassign")

# Merge "Reassign" flag into main dataset.
# Make community names into numeric version for easier subsetting
altereddata = merge(x = finaldata, y = selected, by = c("newid"), all = TRUE)
altereddata$clusterCFG = as.numeric(as.character(altereddata$clusterCFG))

# Define new community variable for after reassignment
altereddata$cfgnew = NA

# For each participant that was selected, randomly choose a new community assignment,
# making sure not to select the community they are already in.
ids = as.list(altereddata$newid[!is.na(altereddata$Reassign)])
for (j in 1:pcts[[i]]){
altereddata$cfgnew[altereddata$newid==ids[[j]]] =  sample(
  unique(
    as.vector(
      altereddata$clusterCFG[altereddata$clusterCFG != altereddata$clusterCFG[altereddata$newid==ids[[j]]]]
      )
    ), 1, replace = F)
}

# For the participants not selected for reassignment, keep their original community the same.
altereddata$cfgnew[is.na(altereddata$Reassign)] = altereddata$clusterCFG[is.na(altereddata$Reassign)]

# Subtract 200 from the community labels so they match the format of the community labels for original network.
Altercoms = as.vector((altereddata$cfgnew)-200)
table(Altercoms)
table(RealComm)

VOI_CFG[[i]] = round(vi.dist(cl1 = RealComm, cl2 = Altercoms, parts = F, base = 2)/log2(vcount(Xrealg)),2)
}

VOI_CFG_10 = unlist(VOI_CFG)[1]
VOI_CFG_20 = unlist(VOI_CFG)[2]

save(VOI_CFG_10, file = paste(ROOT, "/store_data/VOI_CFG_10", sep = ""))
save(VOI_CFG_20, file = paste(ROOT, "/store_data/VOI_CFG_20", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

