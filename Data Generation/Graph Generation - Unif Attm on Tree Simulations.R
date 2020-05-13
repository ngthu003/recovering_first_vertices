library(tidyverse); library(igraph); library(sna)

unif.attm.tree.model <- function(n, seed) { # --------------- START Fn
  
  # Input:
  # ------- n: size of graph
  # ------- seed: to control sampling
  # Output: 
  # ------- inc.mat: incidence matrix of random graph
  # ---------------- of size (n-1)x2, where n = |V_G|
  
  # TODO
  # ---- 1. Initialize 0 matrix of size n x n
  # ---- 2. Loop over each new iteration t:
  # ------- 2.1. Sample from existing vertices and add new edge
  
  
  # Step 1 --------------------------------------------------------- !
  v1 <- seq(2,n, by = 1)      # Vector of each new vertex
  v2 <- rep(-1, n-1)          # Vector of sampled vertices
  # ----------------------------------------------------- End Step 1 !
  
  # Growing trees 
  # Step 2 --------------------------------------------------------- !
  for (t in c(2:n)) {                          # Loop over each new t
    set.seed(seed + t + n)                     # Set Random Ctrl
    random.vertex <- sample(seq(1,t-1,by=1),1) # Pick existing vertex
    v2[t-1] <- random.vertex                   # Add that sampled vt.
  }
  # ----------------------------------------------------- End Step 2 !
  
  inc.matrix <- cbind(v1, v2)
  return(inc.matrix)
} # ---------------------------------------------------------------- !
