# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Recover L vertices:                                  ====== !
# ===== ------------------ Simple search: Vertices with smallest phi    ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !

# Load libraries ------------------------------------------------------------- !
library(tidyverse); library(igraph); library(sna)

# ============================================================================ !
# ----- Fn to get Recovery Rate ---------------------------------------------- !
# -----                                                                 ------ !
get.recovered.L <- function(deg.phi.df, K.vector, L) { # ---- START Fn
  
  # Input:
  # ------- ua.deg.phi.MCsim.5k: list of 2 tables:
  # ---------------------------- (1) degree table
  # ---------------------------- (2) phi_value table
  # ------- K.vector: vector of different K values
  # ------- L:        vertices to be recovered
  # Output: 
  # ------- recovered.1.vector: vector of proportion of times 1 is
  # --------------------------- recovered for each K
  
  # TODO
  # ---- 1. Prepare data:
  # ------- 1.1. Get each df_deg & df_phi
  # ------- 1.2. Initialize recovered.1 vector to store rates
  # ---- 2. Loop over each K = #(return)
  # ------- 2.1. Loop over each MC simulation
  # ------------ 2.1.1. Calculate recovery rate per graph per K
  
  # TODO Step 1 ---------------------------------------------------- !
  # ---- 1.1.
  df_deg <- deg.phi.df[[1]]
  df_phi <- deg.phi.df[[2]]
  # ---- 1.2.
  recovered.L.deg <- rep(0, 1+length(K.vector)) # Initialize vector
  recovered.L.deg[1] <- nrow(df_deg)  # Recovery rate via Max(Degree)
  recovered.L.phi <- recovered.L.deg  # ............. via Min(Max(Subgraph))
  # ----------------------------------------------------- End Step 1 !
  
  # TODO Step 2 ---------------------------------------------------- !
  for (j in 1:length(K.vector)) {               # Loop over each K
    
    K <- K.vector[j]                            # Get current K
    rec.L.deg.v <- c(); rec.L.phi.v <- c()      # to store rates of this K
    
    # Check for if we return enough vertices
    if (K < length(L)) {
      rec.L.deg.v <- rep(FALSE, (ncol(df_deg)-1))
      rec.L.phi.v <- rep(FALSE, (ncol(df_phi)-1))
    } else {
      # ---- 2.1.
      for (i in 2:ncol(df_deg)) {          # Loop over each MC sim.
        # ---- 2.1.1. via Degree
        tmp.df <- df_deg[,c(1,i)]
        tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Degrees
        returned.vert <- tail(tmp.df[,1], K)      # Return via Max(Deg)
        rec.L.deg.v[i-1] <- all(L %in% returned.vert)  # Check for if 1 is returned
        # ---- 2.1.1. via Phi value
        tmp.df <- df_phi[,c(1,i)]
        tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Max(Subgraphs)
        returned.vert <- head(tmp.df[,1], K)      # Return via Min(Max(Subgraphs))
        # rec.1.phi.v[i-1] <- 1 %in% returned.vert  # Check for if 1 is returned
        rec.L.phi.v[i-1] <- all(L %in% returned.vert)  # Check for if 1 is returned
      }
    }
    
    recovered.L.deg[j+1] <- sum(rec.L.deg.v)/1000  # Get recoery rates
    recovered.L.phi[j+1] <- sum(rec.L.phi.v)/1000
  }
  # ----------------------------------------------------- End Step 2 !
  
  recovered.L <- list(recovered.L.deg, recovered.L.phi)   # Recovery rates
  names(recovered.L) <- c('via_Degrees', 'via_Subgraphs') # for all K
  
  return(recovered.L)
} # ---------------------------------------------------------------- !
