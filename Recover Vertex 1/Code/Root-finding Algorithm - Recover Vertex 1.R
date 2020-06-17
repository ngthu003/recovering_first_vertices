# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Get the Recovery Rate:                               ====== !
# ===== ------------------ (1) Maximum Degree                           ====== !
# ===== ------------------ (1) Minimum Maximum Subgraph                 ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !


library(tidyverse); library(igraph); library(sna)

# ============================================================================ !
# ----- Fn to get Recovery Rate ---------------------------------------------- !
# -----                                                                 ------ !
get.recovered.1 <- function(deg.phi.df, K.vector) { # ---- START Fn
  
  # Input:
  # ------- ua.deg.phi.MCsim.5k: list of 2 tables:
  # ---------------------------- (1) degree table
  # ---------------------------- (2) phi_value table
  # ------- K.vector: vector of different K values
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
  recovered.1.deg <- rep(0, 1+length(K.vector)) # Initialize vector
  recovered.1.deg[1] <- nrow(df_deg)  # Recovery rate via Max(Degree)
  recovered.1.phi <- recovered.1.deg  # ............. via Min(Max(Subgraph))
  # ----------------------------------------------------- End Step 1 !
  
  # TODO Step 2 ---------------------------------------------------- !
  for (j in 1:length(K.vector)) {               # Loop over each K
    
    K <- K.vector[j]                            # Get current K
    rec.1.deg.v <- c(); rec.1.phi.v <- c()      # to store rates of this K
    
    # ---- 2.1.
    for (i in 2:ncol(df_deg)) {          # Loop over each MC sim.
      # ---- 2.1.1. via Degree
      tmp.df <- df_deg[,c(1,i)]
      tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Degrees
      returned.vert <- tail(tmp.df[,1], K)      # Return via Max(Deg)
      rec.1.deg.v[i-1] <- 1 %in% returned.vert  # Check for if 1 is returned
      # ---- 2.1.1. via Phi value
      tmp.df <- df_phi[,c(1,i)]
      tmp.df <- tmp.df[order(tmp.df[,2]),]      # Sort via Max(Subgraphs)
      returned.vert <- head(tmp.df[,1], K)      # Return via Min(Max(Subgraphs))
      rec.1.phi.v[i-1] <- 1 %in% returned.vert  # Check for if 1 is returned
    }
    
    recovered.1.deg[j+1] <- sum(rec.1.deg.v)/1000  # Get recoery rates
    recovered.1.phi[j+1] <- sum(rec.1.phi.v)/1000
  }
  # ----------------------------------------------------- End Step 2 !
  
  recovered.1 <- list(recovered.1.deg, recovered.1.phi)   # Recovery rates
  names(recovered.1) <- c('via_Degrees', 'via_Subgraphs') # for all K
  
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
result.5k <- get.recovered.1(pa.deg.phi.MCsim.5k, K.vector)
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
result.10k <- get.recovered.1(pa.deg.phi.MCsim.10k, K.vector)
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
result.15k <- get.recovered.1(pa.deg.phi.MCsim.15k, K.vector)
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
result.20k <- get.recovered.1(pa.deg.phi.MCsim.20k, K.vector)
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
result.25k <- get.recovered.1(pa.deg.phi.MCsim.25k, K.vector)
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
result.30k <- get.recovered.1(pa.deg.phi.MCsim.30k, K.vector)
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
result.35k <- get.recovered.1(pa.deg.phi.MCsim.35k, K.vector)
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
result.40k <- get.recovered.1(pa.deg.phi.MCsim.40k, K.vector)
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
result.45k <- get.recovered.1(pa.deg.phi.MCsim.45k, K.vector)
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
result.50k <- get.recovered.1(pa.deg.phi.MCsim.50k, K.vector)
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
result.55k <- get.recovered.1(pa.deg.phi.MCsim.55k, K.vector)
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
result.60k <- get.recovered.1(pa.deg.phi.MCsim.60k, K.vector)
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
result.65k <- get.recovered.1(pa.deg.phi.MCsim.65k, K.vector)
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
result.70k <- get.recovered.1(pa.deg.phi.MCsim.70k, K.vector)
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
result.75k <- get.recovered.1(pa.deg.phi.MCsim.75k, K.vector)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[15] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.75k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 80,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 80k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.80k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.80k <- get.recovered.1(pa.deg.phi.MCsim.80k, K.vector)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[16] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.80k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !












# -------------------------------------------------------------------------------






# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# ----- 1. Initialize df to store recovery rates per method
pa.recovered.1.deg <- data.frame(matrix(ncol = 1+length(K.vector)))
colnames(pa.recovered.1.deg) <- c('Graph_Size', paste0('K.', K.vector))   # Rename df
pa.recovered.1.phi <- pa.recovered.1.deg
all.results <- list( result.5k, result.10k, result.15k, result.20k, 
                    result.25k, result.30k, result.35k, result.40k,
                    result.45k, result.50k, result.55k, result.60k,
                    result.65k, result.70k, result.75k, result.80k
                    )
# ----- 2. Get rate: Maximum Degree
for (i in 1:length(all.results)) {
  current.result <- all.results[[i]]
  pa.recovered.1.deg[i,] <- current.result['via_Degrees'][[1]]
}
print('Recovery rate: via Degree'); pa.recovered.1.deg
# ----- 3. Get rate: Minimum Maximum Subgraphs
for (i in 1:length(all.results)) {
  current.result <- all.results[[i]]
  pa.recovered.1.phi[i,] <- current.result['via_Subgraphs'][[1]]
}
print('Recovery rate: via Phi'); pa.recovered.1.phi
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !



# ============================================================================ !
# ----- Save df -------------------------------------------------------------- !
write.csv(pa.recovered.1.deg,
          file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Degrees.csv', row.names = TRUE)
write.csv(pa.recovered.1.phi,
          file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi values.csv', row.names = TRUE)

save(pa.recovered.1.deg, file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Degrees.RData')
save(pa.recovered.1.phi, file = 'Pref Attm - Recovery Rate - Vertex 1 - Via Phi values.RData')

















