###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/make_data/md_interim2.R
#
# PURPOSE:  Create second intermediate dataset with communities. 
#           Create network objects - full network, and largest connected component.
#
# INPUT:    DATA_SOURCE/TRIP_edgespublic_randomID_2022-11-14.csv
#           ROOT//store_data/interim1
#
# OUTPUT:   ROOT/store_data/interim2
#           ROOT/store_data/net
#           ROOT/store_data/net_gc
#
# NOTES:    Libraries to load: igraph
#
###############################################################################################################################
###############################################################################################################################

library(igraph)

set.seed(seednum)

#############################
######### LOAD DATA #########
#############################

load(file = paste(ROOT, "/store_data/interim1", sep = ""), envir = .GlobalEnv)

in.edges = read.csv(paste(DATA_SOURCE, '/TRIP_edgespublic_randomID_2022-11-14.csv', sep = ""))

##################################
######### CREATE NETWORK #########
##################################

interim1$clusterCFG = NA # cluster variable
interim1$clusterLEC = NA # cluster variable
fulltrip <- graph_from_data_frame(d = in.edges, vertices = interim1, directed = F)
vcount(fulltrip) # 356 nodes
ecount(fulltrip) # 829 edges
net0 <- fulltrip # same graph
net1 = delete.vertices(net0, degree(net0)==0) # delete isolated nodes
net = simplify(net1, remove.multiple = TRUE, remove.loops = TRUE) # remove loops and multiple edges
net.adj<- get.adjacency(net)
vcount(net) # 277 nodes
ecount(net) # 542 edges

#############################
######### Components ########
#############################
dec = decompose.graph(net)
decsizes = vector("numeric",length(dec))
for(i in 1:length(dec)){
  decsizes[i] = length(V(dec[[i]]))
}
decngc = which(decsizes!=max(decsizes))
dec0 = dec[[decngc[[1]]]]
for( i in 2:length(which(decsizes!=max(decsizes)))){
  dec1 = union(dec0, dec[[decngc[[i]]]])
  dec0 = dec1
}
net_ngc = dec0 # Non-GC
net_gc = dec[[which(decsizes==max(decsizes))]] # GC

#############################
#### Community Detection ####
#############################

#############################
############ CFG ############ 
#############################

clust1.fg = cluster_fast_greedy(net_ngc, merges = T, modularity = T, membership = T, 
                                weights = E(net)$weight) # community detection in non-GC

C1 = vector("list", length(clust1.fg))

for(i in 1:length(clust1.fg)){
  C1[[i]] = as.vector(as.numeric(unlist(clust1.fg[i]))) 
}

#Assign 100+i as clusterCFG number to nodes which belong to clusterCFG C1[[i]]
for(i in 1:length(clust1.fg)){
  interim1$clusterCFG[match(C1[[i]],interim1$id)] <- 100+i  
}

clust2.fg = cluster_fast_greedy(net_gc, merges = T, modularity = T, membership = T,
                                weights = E(net)$weight) # community detection in GC

C2 = vector("list", length(clust2.fg))

# clusterCFGs of the GC
for(i in 1:length(clust2.fg)){
  C2[[i]] = as.vector(as.numeric(unlist(clust2.fg[i]))) 
}

#Assign 200+i as clusterCFG number to nodes which belong to clusterCFG C[[i]]
for(i in 1:length(clust2.fg)){
  interim1$clusterCFG[match(C2[[i]],interim1$id)] <- 200+i  
}

#############################
#### Community Detection ####
#############################

#############################
############ LEC ############ 
#############################
set.seed(101)
clust1.le = leading.eigenvector.community(net_ngc, weights = NULL) # community detection in non-GC

C3 = vector("list", length(clust1.le))

for(i in 1:length(clust1.le)){
  C3[[i]] = as.vector(as.numeric(unlist(clust1.le[i]))) 
}

#Assign 100+i as clusterLEC number to nodes which belong to clusterLEC C1[[i]]
for(i in 1:length(clust1.le)){
  interim1$clusterLEC[match(C3[[i]],interim1$id)] <- 100+i  
}
set.seed(101)
clust2.le = leading.eigenvector.community(net_gc, weights = NULL, options = list(maxiter=10000)) # community detection in GC

C4 = vector("list", length(clust2.le))

# Clusters of the GC
for(i in 1:length(clust2.le)){
  C4[[i]] = as.vector(as.numeric(unlist(clust2.le[i])))
}

#Assign 200+i as clusterLEC number to nodes which belong to clusterLEC C[[i]]
for(i in 1:length(clust2.le)){
  interim1$clusterLEC[match(C4[[i]],interim1$id)] <- 200+i  
}

interim1$clusterCFG = as.factor(interim1$clusterCFG)
interim1$clusterLEC = as.factor(interim1$clusterLEC)

interim2 = interim1[!is.na(interim1$clusterCFG) | !is.na(interim1$clusterLEC),]

#####################################################
################### SAVE OUTPUTS ####################
#####################################################

save(interim2, file = paste(ROOT, "/store_data/interim2", sep = ""))
save(net, file = paste(ROOT, "/store_data/net", sep = ""))
save(net_gc, file = paste(ROOT, "/store_data/net_gc", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))
