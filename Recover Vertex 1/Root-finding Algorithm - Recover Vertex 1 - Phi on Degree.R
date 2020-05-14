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
# ============================================================================ !


# ============================================================================ !
# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- c(seq(1, 9, by = 1), seq(10, 100, by = 5))
print('Different values of K:'); print(K.vector)
print(paste0('In total, we will test over ', length(K.vector), ' values of K.'))
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !




# ============================================================================ !
# ----- Time tracking -------------------------------------------------------- !
# -----                                                                 ------ !
time.tracking <- data.frame(Graph_Size = seq(5000, 75000, by = 5000))
time.tracking$Time <- rep(0, nrow(time.tracking))
time.tracking
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !




# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 5,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 5k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.5k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.5k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.5k, K.vector, 100)
result.5k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.5k, K.vector, 500)
result.5k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.5k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[1] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.5k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 10,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 10k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.10k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.10k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.10k, K.vector, 100)
result.10k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.10k, K.vector, 500)
result.10k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.10k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[2] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.10k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 15,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 15k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.15k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.15k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.15k, K.vector, 100)
result.15k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.15k, K.vector, 500)
result.15k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.15k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[3] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.15k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 20,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 20k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.20k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.20k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.20k, K.vector, 100)
result.20k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.20k, K.vector, 500)
result.20k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.20k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[4] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.20k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 25,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 25k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.25k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.25k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.25k, K.vector, 100)
result.25k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.25k, K.vector, 500)
result.25k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.25k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[5] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.25k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 30,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 30k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.30k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.30k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.30k, K.vector, 100)
result.30k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.30k, K.vector, 500)
result.30k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.30k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[6] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.30k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 35,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 35k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.35k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.35k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.35k, K.vector, 100)
result.35k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.35k, K.vector, 500)
result.35k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.35k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[7] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.35k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 40,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 40k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.40k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.40k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.40k, K.vector, 100)
result.40k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.40k, K.vector, 500)
result.40k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.40k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[8] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.40k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 45,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 45k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.45k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.45k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.45k, K.vector, 100)
result.45k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.45k, K.vector, 500)
result.45k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.45k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[9] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.45k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 50,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 50k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.50k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.50k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.50k, K.vector, 100)
result.50k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.50k, K.vector, 500)
result.50k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.50k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[10] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.50k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 55,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 55k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.55k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.55k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.55k, K.vector, 100)
result.55k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.55k, K.vector, 500)
result.55k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.55k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[11] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.55k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 60,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 60k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.60k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.60k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.60k, K.vector, 100)
result.60k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.60k, K.vector, 500)
result.60k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.60k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[12] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.60k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 65,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 65k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.65k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.65k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.65k, K.vector, 100)
result.65k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.65k, K.vector, 500)
result.65k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.65k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[13] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.65k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 70,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 70k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.70k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.70k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.70k, K.vector, 100)
result.70k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.70k, K.vector, 500)
result.70k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.70k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[14] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.70k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 75,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 75k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.75k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.75k.1 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.75k, K.vector, 100)
result.75k.5 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.75k, K.vector, 500)
result.75k.10 <- recover.1.phi.on.deg(pa.deg.phi.MCsim.75k, K.vector, 1000)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[15] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.75k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !













# -------------------------------------------------------------------------------

time.tracking
sum(time.tracking$Time[1:12])



# ======================================================================================== !
# ================================================================================== !
# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# ----- Top 100 vertices                                                --------
# ----- 1. Initialize df to store recovery rates per method
pa.recovered.1.phi.on.deg.1 <- data.frame(matrix(ncol = 1+length(K.vector)))
colnames(pa.recovered.1.phi.on.deg.1) <- c('Graph_Size', 
                                           paste0('K.', K.vector))   # Rename df
all.results.1 <- list( result.5k.1, result.10k.1, result.15k.1, result.20k.1, 
                      result.25k.1, result.30k.1, result.35k.1, result.40k.1,
                      result.45k.1, result.50k.1, result.55k.1, result.60k.1,
                      result.65k.1, result.70k.1, result.75k.1
)
# ----- 2. Get rate: Phi on Degree - top 100
for (i in 1:length(all.results.1)) {
  current.result <- all.results.1[[i]]
  pa.recovered.1.phi.on.deg.1[i,] <- current.result
}
print('Recovery rate: via Phi on Degree: Top 100 Vertices'); 
pa.recovered.1.phi.on.deg.1
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !
# ======================================================================================== !
# ================================================================================== !
# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# ----- Top 500 vertices                                                --------
# ----- 1. Initialize df to store recovery rates per method
pa.recovered.1.phi.on.deg.5 <- data.frame(matrix(ncol = 1+length(K.vector)))
colnames(pa.recovered.1.phi.on.deg.5) <- c('Graph_Size', 
                                           paste0('K.', K.vector))   # Rename df
all.results.5 <- list( result.5k.5, result.10k.5, result.15k.5, result.20k.5, 
                       result.25k.5, result.30k.5, result.35k.5, result.40k.5,
                       result.45k.5, result.50k.5, result.55k.5, result.60k.5,
                       result.65k.5, result.70k.5, result.75k.5
)
# ----- 2. Get rate: Phi on Degree - top 100
for (i in 1:length(all.results.5)) {
  current.result <- all.results.5[[i]]
  pa.recovered.1.phi.on.deg.5[i,] <- current.result
}
print('Recovery rate: via Phi on Degree: Top 500 Vertices'); 
pa.recovered.1.phi.on.deg.5
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !
# ======================================================================================== !
# ================================================================================== !
# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# ----- Top 1000 vertices                                                -------
# ----- 1. Initialize df to store recovery rates per method
pa.recovered.1.phi.on.deg.10 <- data.frame(matrix(ncol = 1+length(K.vector)))
colnames(pa.recovered.1.phi.on.deg.10) <- c('Graph_Size', 
                                           paste0('K.', K.vector))   # Rename df
all.results.10 <- list( result.5k.10, result.10k.10, result.15k.10, result.20k.10, 
                       result.25k.10, result.30k.10, result.35k.10, result.40k.10,
                       result.45k.10, result.50k.10, result.55k.10, result.60k.10,
                       result.65k.10, result.70k.10, result.75k.10
)
# ----- 2. Get rate: Phi on Degree - top 100
for (i in 1:length(all.results.10)) {
  current.result <- all.results.10[[i]]
  pa.recovered.1.phi.on.deg.10[i,] <- current.result
}
print('Recovery rate: via Phi on Degree: Top 1000 Vertices'); 
pa.recovered.1.phi.on.deg.10
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !








# ============================================================================ !
# ----- Save df -------------------------------------------------------------- !
write.csv(pa.recovered.1.phi.on.deg.1,
          file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 100.csv', 
          row.names = TRUE)
save(pa.recovered.1.phi.on.deg.1, 
     file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 100.RData')
# ----- Save df -------------------------------------------------------------- !
write.csv(pa.recovered.1.phi.on.deg.5,
          file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 500.csv', 
          row.names = TRUE)
save(pa.recovered.1.phi.on.deg.5, 
     file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 500.RData')
# ----- Save df -------------------------------------------------------------- !
write.csv(pa.recovered.1.phi.on.deg.10,
          file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 1000.csv', 
          row.names = TRUE)
save(pa.recovered.1.phi.on.deg.10, 
     file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 1000.RData')




















