library(tidyverse); library(igraph); library(sna)

pref.attm.tree.model <- function(n, seed) { # --------------- START Fn
  
  # Input:
  # ------- n: size of graph
  # ------- seed: to control sampling
  # Output: 
  # ------- adj.mat: adjacency list
  # ---------------- of size (n-1)x2, where n = |V_G|
  
  # TODO
  # ---- 1. Initialize v1, v2, all_vt
  # ---- 2. Loop over each new iteration t:
  # ------- 2.1. Sample from current all_vt array
  # ------- 2.2. Update all_vt
  
  # Step 1 --------------------------------------------------------- !
  v1 <- seq(2,n, by = 1)      # Vector of each new vertex
  v2 <- c(1, rep(-1, n-2))    # Vector of sampled vertices
  all_vt <- c(v1[1], v2[1])   # Vector to store all end vertices
  # ----------------------------------------------------- End Step 1 !
  
  # Growing trees 
  # Step 2 --------------------------------------------------------- !
  set.seed(seed)              # Set randomization control
  for (t in c(3:n)) {                          
    # Sample from existing vertices
    rdm_vt <- sample(all_vt, 1, prob = rep(1, length(all_vt)))
    v2[t-1] <- rdm_vt
    # Update vector to store all edges' end vertices
    all_vt <- append(all_vt, c(v1[t-1], rdm_vt))
  }
  # ----------------------------------------------------- End Step 2 !
  
  return(cbind(v1, v2))       # Return adjacency matrix
} # ---------------------------------------------------------------- !
