###############################################################################################################################
###############################################################################################################################
#
# PROGRAM:  ROOT/analysis/perturbations.R
#
# PURPOSE:  Create VOI datasets for both community structures.
#
# INPUT:    ROOT/store_data/net_gc
#
# OUTPUT:   ROOT/store_data/LEC_voi, CFG_voi
#
# NOTES:    Libraries to load: perturbR, aricode, lpSolve, mcclust, igraph. 
#           Code is adapted from the 'perturbR' to allow for
#           cluster-fast-greedy (CFG) and leading-eigenvector (LEC).
###############################################################################################################################
###############################################################################################################################

library(perturbR)
library(aricode)
library(lpSolve)
library(mcclust)
library(igraph)

PertFunLECnull = function(NetReal, reps, resolution, nlines){
  Xreal = as.matrix(get.adjacency(NetReal))
  Xrealg = graph.adjacency(as.matrix(Xreal), mode = "undirected")
  
  
  n.elements <- length(Xreal[, 1]) * (length(Xreal[,1]) - 1)/2
  percent <- seq(from = 0, to = n.elements, by = max(round(resolution * (n.elements)), 1))
  iters <- seq(1, length(percent))
  
  vid = cbind(rep(NA, reps))
  vidR = cbind(rep(NA, reps))
  m.vid = matrix(cbind(rep(NA, length(round(percent/max(percent),2)))))
  m.vidR = matrix(cbind(rep(NA, length(round(percent/max(percent),2)))))
  Mvid = vector(mode = "list", length = nlines)
  rownames(m.vid) = round(percent/max(percent),2)
  NullGraphs = vector(mode = "list", length = nlines)
  
  for(k in 1:nlines){
    for(j in 1:length(percent)){
      for(i in 1:reps){
        
        NullGraphs[[k]] = degree.sequence.game(degree(Xrealg), method = c("vl"))
        Xnull = as.matrix(get.adjacency(NullGraphs[[k]]))
        NullComm = leading.eigenvector.community(NullGraphs[[k]], weights = E(NullGraphs[[k]])$weight, options = list(maxiter=10000))$membership
        RealComm = leading.eigenvector.community(Xrealg, weights = E(Xrealg)$weight, options = list(maxiter=10000))$membership
        
        Xnullnew <- as.matrix(rewireR(Xnull, nperturb = percent[iters[j]], 
                                      dist = "NegBinom"))
        Xnullnewg <- graph.adjacency(Xnullnew, mode = "undirected", 
                                     weighted = TRUE)
        Xrealnew <- as.matrix(rewireR(Xreal, nperturb = percent[iters[j]], 
                                      dist = "NegBinom"))
        Xrealnewg <- graph.adjacency(Xrealnew, mode = "undirected", 
                                     weighted = TRUE)
        
        ReNullComm = leading.eigenvector.community(Xnullnewg, weights = E(Xnullnewg)$weight, options = list(maxiter=10000))$membership
        ReRealComm = leading.eigenvector.community(Xrealnewg, weights = E(Xrealnewg)$weight, options = list(maxiter=10000))$membership
        
        vid[i] = vi.dist(cl1 = ReNullComm, cl2 = NullComm, parts = F, base = 2)
        vidR[i] = vi.dist(cl1 = ReRealComm, cl2 = RealComm, parts = F, base = 2)
      }
      
      m.vid[j] = mean(vid)/log2(vcount(Xrealg))
      m.vidR[j] = mean(vidR)/log2(vcount(Xrealg))
    }
    Mvid[[k]] = m.vid
    
    plot(Mvid[[k]] ~ rownames(m.vid), ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
         type = "l", lwd=3, lty=1, col = gray(.65),);par(new=T)
  }
  plot(m.vidR ~ rownames(m.vid), ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
       type = "l", lwd=3, lty=1, col = "red",)
  return(Mvid)
}

PertFunCFGnull = function(NetReal, reps, resolution, nlines){
  Xreal = as.matrix(get.adjacency(NetReal))
  Xrealg = graph.adjacency(as.matrix(Xreal), mode = "undirected")
  
  
  n.elements <- length(Xreal[, 1]) * (length(Xreal[,1]) - 1)/2
  percent <- seq(from = 0, to = n.elements, by = max(round(resolution * (n.elements)), 1))
  iters <- seq(1, length(percent))
  
  vid = cbind(rep(NA, reps))
  vidR = cbind(rep(NA, reps))
  m.vid = matrix(cbind(rep(NA, length(round(percent/max(percent),2)))))
  m.vidR = matrix(cbind(rep(NA, length(round(percent/max(percent),2)))))
  Mvid = vector(mode = "list", length = nlines)
  rownames(m.vid) = round(percent/max(percent),2)
  NullGraphs = vector(mode = "list", length = nlines)
  
  for(k in 1:nlines){
    for(j in 1:length(percent)){
      for(i in 1:reps){
        
        NullGraphs[[k]] = degree.sequence.game(degree(Xrealg), method = c("vl"))
        Xnull = as.matrix(get.adjacency(NullGraphs[[k]]))
        NullComm = cluster_fast_greedy(NullGraphs[[k]], merges = T, modularity = T, membership = T, weights = E(NullGraphs[[k]])$weight)$membership
        RealComm = cluster_fast_greedy(Xrealg, merges = T, modularity = T, membership = T, weights = E(Xrealg)$weight)$membership
        
        
        Xnullnew <- as.matrix(rewireR(Xnull, nperturb = percent[iters[j]], 
                                      dist = "NegBinom"))
        Xnullnewg <- graph.adjacency(Xnullnew, mode = "undirected", 
                                     weighted = TRUE)
        Xrealnew <- as.matrix(rewireR(Xreal, nperturb = percent[iters[j]], 
                                      dist = "NegBinom"))
        Xrealnewg <- graph.adjacency(Xrealnew, mode = "undirected", 
                                     weighted = TRUE)
        
        ReNullComm = cluster_fast_greedy(Xnullnewg, merges = T, modularity = T, membership = T, 
                                         weights = E(Xnullnewg)$weight)$membership # community detection
        ReRealComm = cluster_fast_greedy(Xrealnewg, merges = T, modularity = T, membership = T, 
                                         weights = E(Xrealnewg)$weight)$membership
        
        
        vid[i] = vi.dist(cl1 = ReNullComm, cl2 = NullComm, parts = F, base = 2)
        vidR[i] = vi.dist(cl1 = ReRealComm, cl2 = RealComm, parts = F, base = 2)
        
        
      }
      
      m.vid[j] = mean(vid)/log2(vcount(Xrealg))
      m.vidR[j] = mean(vidR)/log2(vcount(Xrealg))
      
    }
    Mvid[[k]] = m.vid
    
    plot(Mvid[[k]] ~ rownames(m.vid), ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
         type = "l", lwd=3, lty=1, col = gray(.65),);par(new=T)
  }
  plot(m.vidR ~ rownames(m.vid), ylim = range(c(0, .825)), ylab="", xlab="", axes = F,
       type = "l", lwd=3, lty=1, col = "red",)
  return(Mvid)
}

PertFunLECreal = function(NetReal, reps, resolution){
  Xreal = as.matrix(get.adjacency(NetReal))
  Xrealg = graph.adjacency(as.matrix(Xreal), mode = "undirected")
  Xnullg = degree.sequence.game(degree(Xrealg), method = c("vl"))
  Xnull = as.matrix(get.adjacency(Xnullg))
  
  
  n.elements <- length(Xreal[, 1]) * (length(Xreal[,1]) - 1)/2
  percent <- seq(from = 0, to = n.elements, by = max(round(resolution * (n.elements)), 1))
  iters <- seq(1, length(percent))
  
  NullComm = leading.eigenvector.community(Xnullg, weights = E(Xnullg)$weight, options = list(maxiter=10000))$membership
  RealComm = leading.eigenvector.community(Xrealg, weights = E(Xrealg)$weight, options = list(maxiter=10000))$membership
  
  vid = cbind(cbind(rep(NA, reps)), cbind(rep(NA, reps)))
  m.vid = as.matrix(x = cbind(cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent)))))
  colnames(m.vid) = c("VOI_con", "VOI_real", "LB_con", "UB_con", "LB_real", "UB_real")
  rownames(m.vid) = round(percent/max(percent),2)
  
  for(j in 1:length(percent)){
    for(i in 1:reps){
     # repeat{
      Xnullnew <- as.matrix(rewireR(Xnull, nperturb = percent[iters[j]], 
                                    dist = "NegBinom"))
      Xnullnewg = graph.adjacency(as.matrix(Xnullnew), mode = "undirected")
      
     # if(!any(degree(Xnullnewg)==0)==TRUE){break}
     # }

      ReNullComm = leading.eigenvector.community(Xnullnewg, weights = E(Xnullnewg)$weight, options = list(maxiter=100000, NCV=6))$membership

      
     # repeat{
      Xrealnew <- as.matrix(rewireR(Xreal, nperturb = percent[iters[j]], 
                                    dist = "NegBinom"))
      Xrealnewg = graph.adjacency(as.matrix(Xrealnew), mode = "undirected")
      
     # if(!any(degree(Xrealnewg)==0)==TRUE){break}
     # }

      ReRealComm = leading.eigenvector.community(Xrealnewg, weights = E(Xrealnewg)$weight, options = list(maxiter=100000, NCV=6))$membership

      
      vid[i,1] = vi.dist(cl1 = ReNullComm, cl2 = NullComm, parts = F, base = 2)
      vid[i,2] = vi.dist(cl1 = ReRealComm, cl2 = RealComm, parts = F, base = 2)
      
    }
    
    m.vid[j,1] = mean(vid[,1])/log2(vcount(Xrealg))
    m.vid[j,2] = mean(vid[,2])/log2(vcount(Xrealg))
    m.vid[j,3] = quantile(vid[,1], c(0,1))[[1]]/log2(vcount(Xrealg))
    m.vid[j,4] = quantile(vid[,1], c(0,1))[[2]]/log2(vcount(Xrealg))
    m.vid[j,5] = quantile(vid[,2], c(0,1))[[1]]/log2(vcount(Xrealg))
    m.vid[j,6] = quantile(vid[,2], c(0,1))[[2]]/log2(vcount(Xrealg))
    
  }
  return(as.data.frame(m.vid))
}

PertFunCFGreal = function(NetReal, reps, resolution){
  Xreal = as.matrix(get.adjacency(NetReal))
  Xrealg = graph.adjacency(as.matrix(Xreal), mode = "undirected")
  Xnullg = degree.sequence.game(degree(Xrealg), method = c("vl"))
  Xnull = as.matrix(get.adjacency(Xnullg))
  
  
  n.elements <- length(Xreal[, 1]) * (length(Xreal[,1]) - 1)/2
  percent <- seq(from = 0, to = n.elements, by = max(round(resolution * (n.elements)), 1))
  iters <- seq(1, length(percent))
  
  NullComm = cluster_fast_greedy(Xnullg, merges = T, modularity = T, membership = T, weights = E(Xnullg)$weight)$membership
  RealComm = cluster_fast_greedy(Xrealg, merges = T, modularity = T, membership = T, weights = E(Xrealg)$weight)$membership
  
  vid = cbind(cbind(rep(NA, reps)), cbind(rep(NA, reps)))
  m.vid = as.matrix(x = cbind(cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent))), cbind(rep(NA, length(percent)))))
  colnames(m.vid) = c("VOI_con", "VOI_real", "LB_con", "UB_con", "LB_real", "UB_real")
  rownames(m.vid) = round(percent/max(percent),2)
  
  for(j in 1:length(percent)){
    for(i in 1:reps){
      Xnullnew <- as.matrix(rewireR(Xnull, nperturb = percent[iters[j]], 
                                    dist = "NegBinom"))
      Xnullnewg <- graph.adjacency(Xnullnew, mode = "undirected", 
                                   weighted = TRUE)
      ReNullComm = cluster_fast_greedy(Xnullnewg, merges = T, modularity = T, membership = T, 
                                       weights = E(Xnullnewg)$weight)$membership # community detection
      
      Xrealnew <- as.matrix(rewireR(Xreal, nperturb = percent[iters[j]], 
                                    dist = "NegBinom"))
      Xrealnewg <- graph.adjacency(Xrealnew, mode = "undirected", 
                                   weighted = TRUE)
      ReRealComm = cluster_fast_greedy(Xrealnewg, merges = T, modularity = T, membership = T, 
                                       weights = E(Xrealnewg)$weight)$membership # community detection
      
      
      vid[i,1] = vi.dist(cl1 = ReNullComm, cl2 = NullComm, parts = F, base = 2)
      vid[i,2] = vi.dist(cl1 = ReRealComm, cl2 = RealComm, parts = F, base = 2)
      
    }
    
    m.vid[j,1] = mean(vid[,1])/log2(vcount(Xrealg))
    m.vid[j,2] = mean(vid[,2])/log2(vcount(Xrealg))
    m.vid[j,3] = quantile(vid[,1], c(0,1))[[1]]/log2(vcount(Xrealg))
    m.vid[j,4] = quantile(vid[,1], c(0,1))[[2]]/log2(vcount(Xrealg))
    m.vid[j,5] = quantile(vid[,2], c(0,1))[[1]]/log2(vcount(Xrealg))
    m.vid[j,6] = quantile(vid[,2], c(0,1))[[2]]/log2(vcount(Xrealg))
    
  }
  return(as.data.frame(m.vid))
}

FinalLEC = function(NetReal, reps, resolution, style, nlines){
  
  if(style == 1){
    return(PertFunLECreal(NetReal = NetReal, reps = reps, resolution = resolution))
  }
  if(style == 2){
    return(PertFunLECnull(NetReal = NetReal, reps = reps, resolution = resolution, nlines = nlines))
  }
  
}

FinalCFG = function(NetReal, reps, resolution, style, nlines){
  
  if(style == 1){
    return(PertFunCFGreal(NetReal = NetReal, reps = reps, resolution = resolution))
  }
  if(style == 2){
    return(PertFunCFGnull(NetReal = NetReal, reps = reps, resolution = resolution, nlines = nlines))
  }
  
}


PertThatGraph = function(NetReal, reps, resolution, ComDecAlg, style, nlines){
  
  if(ComDecAlg == "CFG"){
    return(FinalCFG(NetReal = NetReal, reps = reps, resolution = resolution, style = style, nlines = nlines))
  }
  if(ComDecAlg == "LEC"){
    return(FinalLEC(NetReal = NetReal, reps = reps, resolution = resolution, style = style, nlines = nlines))
  }
}


############################################
#### Make datasets for making VOI plots ####
############################################


# Giant component of the network graph.
load(file = paste(ROOT, "/store_data/net_gc", sep = ""), envir = .GlobalEnv)

set.seed(seednum)

# Make the VOI dataset for CFG.
CFG_voi = PertThatGraph(NetReal = net_gc, reps = 30, resolution = .05, style = 1, ComDecAlg = "CFG")
save(CFG_voi, file = paste(ROOT, "/store_data/CFG_voi", sep = ""))


# Make the VOI dataset for LEC.
LEC_voi = PertThatGraph(NetReal = net_gc, reps = 30, resolution = .05, style = 1, ComDecAlg = "LEC")
save(LEC_voi, file = paste(ROOT, "/store_data/LEC_voi", sep = ""))

rm(list=setdiff(ls(),c("ROOT", "seednum", "vars")))

