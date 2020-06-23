# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Recover vertices with Partial Knowledge:             ====== !
# ===== ------------------ (1) Maximum Degree                           ====== !
# ===== ------------------ (1) Minimum Maximum Subgraph                 ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !

# Load libraries ------------------------------------------------------------- !
library(tidyverse); library(igraph); library(sna)

# ============================================================================ !
# ----- Fn to get Recovery Rate ---------------------------------------------- !
# -----                                                                 ------ !
recover.vert.n <- function(nbhd.list, deg.phi.df, K.vector, 
                           known.vert, vert.rec) { # ---- START Fn
  
  # Input:
  # ------- deg.phi.df: list of 2 tables:
  # ------------------- (1) degree table
  # ------------------- (2) phi_value table
  # ------- K.vector:   vector of different K values
  # ------- nbhd.list:  list of neighbors of each of the first 10 vertices
  # ------- known.vert: vector of known vertices
  # ------- vert.rec:   vertex to recover
  # Output: 
  # ------- recovery.rate: vector of proportion of times 1 is
  # ---------------------- recovered for each K per method
  
  # TODO
  # ---- 1. Prepare data:
  # ------- 1.1. Get each df_deg & df_phi
  # ------- 1.2. Initialize rec.deg.v and rec.phi.v to track the recovery
  # ---- 2. Loop over each K = #(return)
  # ------- 2.1. Loop over each MC simulation
  # ------------ 2.1.1. Get common neighbors of known vertices
  # ------------ 2.1.2. Check for #(common neighbors)
  # ------------------- 2.1.2.1. If only 1, return that
  # ------------------- 2.1.2.2. If more, loop over each K and return top K vertices
  # ---- 3. Calculate the recovery rate
  
  # TODO Step 1 ---------------------------------------------------- !
  # ---- 1.1. Get each df_deg & df_phi
  df_deg <- deg.phi.df[[1]]
  df_phi <- deg.phi.df[[2]]
  # ---- 1.2. Initialize rec.deg.v and rec.phi.v
  rec.deg.v <- matrix(0, nrow = length(K.vector), ncol = 1000)
  rec.phi.v <- matrix(0, nrow = length(K.vector), ncol = 1000)
  # ----------------------------------------------------- End Step 1 !
  
  
  # TODO Step 2 ---------------------------------------------------- !
  # ---- 2.1. Loop over each MC simulation
  for (i in 1:1000) {
    
    graph.nbhd <- nbhd.list[[i]]    # Extract neighbor list of current simulation
    
    # ---- 2.1.1. Get common neighbors of known vertices
    common.nbh <- c()
    for (u in 1:length(known.vert)) {  # Loop over each known vertices
      known.v <- known.vert[u]
      nbh.v <- unlist(graph.nbhd[known.v] )
      common.nbh <- union(nbh.v, common.nbh)
    }
    common.nbh <- common.nbh[!(common.nbh %in% known.vert)]
    # ------------------------------------------- ENd Substep 2.1.1. !
    
    # ---- 2.1.2. Check for #(common neighbors)
    # -------- 2.1.2.1. If only 1, return that
    if (length(common.nbh) == 1) {
      returned.vert <- common.nbh
      rec.deg.v[,i] <- vert.rec %in% returned.vert
      rec.phi.v[,i] <- vert.rec %in% returned.vert
    } 
    # -------- 2.1.2.2. If more, loop over each K and return top K vertices
    else {
      
      # ---- via Degree: Max(Degree)
      tmp.df <- df_deg[common.nbh, c(1, i+1)]
      tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Degrees
      # -------- Loop over each K
      for (k in 1:length(K.vector)) {
        K <- K.vector[k]
        returned.vert <- tail(tmp.df[,1], min(K, nrow(tmp.df)))
        rec.deg.v[k,i] <- vert.rec %in% returned.vert
      }
      
      # ---- via Subgraphs: Min(Max. Subgraph)
      tmp.df <- df_phi[common.nbh, c(1, i+1)]
      tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Degrees
      # -------- Loop over each K
      for (k in 1:length(K.vector)) {
        K <- K.vector[k]
        returned.vert <- head(tmp.df[,1], min(K, nrow(tmp.df)))
        rec.phi.v[k,i] <- vert.rec %in% returned.vert
      }
      
    }
    
  }
  # ----------------------------------------------------- End Step 2 !
  
  
  # TODO Step 3 ---------------------------------------------------- !
  recovery.rate.deg <- c(); recovery.rate.phi <- c()
  for (k in 1:length(K.vector)) {
    recovery.rate.deg[k] <- sum(rec.deg.v[k,]) / 1000
    recovery.rate.phi[k] <- sum(rec.phi.v[k,]) / 1000
  }
  # ----------------------------------------------------- End Step 3 !
  
  # Prepare return results
  recovery.rate <- list(recovery.rate.deg, recovery.rate.phi)
  names(recovery.rate) <- c('via_Degrees', 'via_Subgraphs')
  return(recovery.rate)
  
} # ---------------------------------------------------------------- !
