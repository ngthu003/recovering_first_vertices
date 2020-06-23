# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Recover L vertices:                                  ====== !
# ===== ------------------ Local search: K: constant                    ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !

library(tidyverse); library(igraph); library(sna)

# ============================================================================ !
# ----- Fn to get Returned Vertices ------------------------------------------ !
# -----                                                                 ------ !
local.search.1.basket <- function(nbhd.list, deg.phi.df, 
                                  known.vertices, K, L.steps) { # ---- START Fn
  
  # Input:
  # ------- deg.phi.df:   list of 2 tables:
  # -------------------     (1) degree table (not used in this case)
  # -------------------     (2) phi_value table
  # ------- nbhd.list:      list of neighbors of each of the first vertices
  # ------- known.vertices: vector of known vertices (default to empty)
  # ------- K:              #(vertices) to recover per step (constant in this case)
  # ------- L.steps:        #(searches) (ie. no. of vertices we want recovered)
  
  # Output: 
  # ------- list.returned.vertices:
  # ------------------------------- list of vectors of returned vertices
  # ------------------------------- each item in list is for each simulation
  # ------------------------------- to be checked for accuracy later
  
  # TODO
  # ---- 1. Prepare data:
  # ------- 1.1. Get df_phi
  # ------- 1.2. returned.vertices
  # ---- 2. Loop over each MC simulation
  # ------- 2.1. Initialize known vertices for current simulation
  # ------- 2.2. Loop over each step
  # ------------ 2.2.1. If No known vertices: get top K vertices w/ smallest phi values
  # ------------ 2.2.2. If alr. have known vertices:
  # ------------------- 2.2.2.1. Get common neighbors, group all together
  # ------------------- 2.2.2.2. Among common neighbors, get K vertices w/ smallest phi
  # ------------ 2.2.3. Update known.vertices with those K vertices
  
  
  
  # TODO Step 1 ---------------------------------------------------- !
  # ----------- Prepare data
  df_phi <- deg.phi.df[[2]]
  list.returned.vertices <- list()
  # ----------------------------------------------------- End Step 1 !
  
  
  # TODO Step 2 ---------------------------------------------------- !
  # ----------- Loop over each simulation
  for (i in 1:1000) {
    
    # ---- 2.1. For current simulation, initialize:
    graph.nbhd <- nbhd.list[[i]]          # Neighbor lists
    returned.vertices <- known.vertices   # Known vertices at the beginning
    
    # ---- 2.2. Loop over each step
    for (l in 1:L.steps) {
      
      # Check for if there are No known vertices, if Yes, look for top K vertices like usual
      if (length(returned.vertices) == 0) {
        
        # ---- 2.2.1. No known vertices 
        tmp.df <- df_phi[,c(1,i+1)]
        head(tmp.df)
        tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Max(Subgraphs)
        returned.vertices.step <- head(tmp.df[,1], K)      # Return via Min(Max(Subgraphs))
        
      } else {
        
        # ---- 2.2.2. Have known vertices
        # ---- 2.2.2.1. Get common neighbors
        common.nbh <- c()
        for (u in 1:length(returned.vertices)) {  
          known.v <- returned.vertices[u]
          nbh.v <- unlist(graph.nbhd[known.v] )
          common.nbh <- union(nbh.v, common.nbh)
        }
        common.nbh <- common.nbh[!(common.nbh %in% returned.vertices)]
        
        # ---- 2.2.2.2. Get returned vertices
        tmp.df <- df_phi[common.nbh, c(1, i+1)]
        head(tmp.df)
        tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Degrees
        returned.vertices.step <- head(tmp.df[,1], min(K, nrow(tmp.df)))
        
      }
      
      # ---- 2.2.3. Update known vertices
      returned.vertices <- append(returned.vertices, returned.vertices.step)
      
    }
    
    # Append to list of known vertices
    list.returned.vertices[[i]] <- returned.vertices
    
  }
  # ----------------------------------------------------- End Step 2 !
  
  return(list.returned.vertices)
  
} # ---------------------------------------------------------------- !
