# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Get the Recovery Rate:                               ====== !
# ===== ------------------ conditioned on largest maximum degrees       ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !

# Load libraries ------------------------------------------------------------- !
library(tidyverse); library(igraph); library(sna)
# ---------------------------------------------------------------------------- !

# ============================================================================ !
# ----- Fn to get Recovery Rate ---------------------------------------------- !
# -----                                                                 ------ !
recover.1.phi.on.deg <- function(deg.phi.df, K.vector, N.largest) { # ---- START Fn
  
  # Input:
  # ------- deg.phi.df: list of 2 tables:
  # ------------------- (1) degree table
  # ------------------- (2) phi_value table
  # ------- K.vector:   different K values
  # ------- N.largest:  #(vertices) with largest degrees
  # Output: 
  # ------- recovered.1: vector of proportion of times 1 is
  # -------------------- recovered for each K
  
  # TODO
  # ---- 1. Prepare data:
  # ------- 1.1. Get each `df_deg` & `df_phi`
  # ------- 1.2. Initialize vector `recovered.1` to store rates
  # ---- 2. Loop over each K = #(return)
  # ------- 2.1. Loop over each MC simulation
  # ------------ 2.1.1. Get label of `N.largest` vertices with largest degrees
  # ------------ 2.1.2. Conditioned on those vertices, find K with smallest phi
  # ------------ 2.1.3. Calculate recovery rate per graph per K
  
  
  # ------------------------------------------------------------------------ !
  # -------------------------------------------------------------------- !
  # TODO Step 1 ---------------------------------------------------- !
  # ---- 1. Prepare data:
  # ------- 1.1. Get each `df_deg` & `df_phi`
  df_deg <- deg.phi.df[[1]]
  df_phi <- deg.phi.df[[2]]
  # ------- 1.2. Initialize vector `recovered.1` to store rates
  recovered.1 <- rep(0, 1+length(K.vector)) # Initialize vector
  recovered.1[1] <- nrow(df_deg)            # Label with Graph size
  # ----------------------------------------------------- End Step 1 !
  # -------------------------------------------------------------------- !
  # ------------------------------------------------------------------------ !
  # -------------------------------------------------------------------- !
  # TODO Step 2 ---------------------------------------------------- !
  # ---- 2. Loop over each K = #(return)
  for (j in 1:length(K.vector)) {               # Loop over each K
    K <- K.vector[j]                            # Get current K
    # ------- 2.1. Loop over each MC simulation
    tmp.rec.1.sim <- c()
    for (i in 2:ncol(df_deg)) {          
      # ------------ 2.1.1. Get label of `N.largest` vertices with largest degrees
      tmp.df <- df_deg[,c(1,i)]           
      tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Degrees
      N.vert <- tail(tmp.df[,1], N.largest)     # Return Label of N.largest vertices
      # ------------ 2.1.2. Conditioned on those vertices, find K with smallest phi
      tmp.df <- df_phi[N.vert,c(1,i)]
      tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Max(Subgraphs)
      returned.vert <- head(tmp.df[,1], K)      # Return via Min(Max(Subgraphs))
      tmp.rec.1.sim[i-1] <- 1 %in% returned.vert# Check for if 1 is returned
    }
    # ------------ 2.1.3. Calculate recovery rate per graph per K
    recovered.1[j+1] <- sum(tmp.rec.1.sim)/1000
  }
  # ----------------------------------------------------- End Step 2 !
  # -------------------------------------------------------------------- !
  # ------------------------------------------------------------------------ !
  return(recovered.1)
} # ---------------------------------------------------------------- !
