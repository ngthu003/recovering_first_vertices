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
# ============================================================================ !




# ============================================================================ !
# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- seq(5,100,by=5)
print('Different values of K: '); print(K.vector)
print(paste0('In total, we will test over ', length(K.vector), ' values of K.'))
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !



# ============================================================================ !
# ----- L: diff. #(vertices) to be recovered --------------------------------- !
# -----                                                                 ------ !
L.vector <- seq(1,10,by=1)
print('Different values of L:'); print(L.vector)
print(paste0('In total, we will test over ', length(L.vector), ' values of L.'))
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !

# K.vector
# L.vector
# 
# deg.phi.df <- pa.deg.phi.MCsim.5k
# tmp <- get.recovered.L(pa.deg.phi.MCsim.5k, K.vector, seq(1,L.vector[10]))





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
result.5k <- list()
for (i in 1:length(L.vector)) {
  result.5k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.5k, K.vector, seq(1,L.vector[i]))
  names(result.5k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.10k <- list()
for (i in 1:length(L.vector)) {
  result.10k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.10k, K.vector, seq(1,L.vector[i]))
  names(result.10k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.15k <- list()
for (i in 1:length(L.vector)) {
  result.15k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.15k, K.vector, seq(1,L.vector[i]))
  names(result.15k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.20k <- list()
for (i in 1:length(L.vector)) {
  result.20k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.20k, K.vector, seq(1,L.vector[i]))
  names(result.20k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.25k <- list()
for (i in 1:length(L.vector)) {
  result.25k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.25k, K.vector, seq(1,L.vector[i]))
  names(result.25k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.30k <- list()
for (i in 1:length(L.vector)) {
  result.30k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.30k, K.vector, seq(1,L.vector[i]))
  names(result.30k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.35k <- list()
for (i in 1:length(L.vector)) {
  result.35k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.35k, K.vector, seq(1,L.vector[i]))
  names(result.35k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.40k <- list()
for (i in 1:length(L.vector)) {
  result.40k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.40k, K.vector, seq(1,L.vector[i]))
  names(result.40k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.45k <- list()
for (i in 1:length(L.vector)) {
  result.45k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.45k, K.vector, seq(1,L.vector[i]))
  names(result.45k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.50k <- list()
for (i in 1:length(L.vector)) {
  result.50k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.50k, K.vector, seq(1,L.vector[i]))
  names(result.50k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.55k <- list()
for (i in 1:length(L.vector)) {
  result.55k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.55k, K.vector, seq(1,L.vector[i]))
  names(result.55k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.60k <- list()
for (i in 1:length(L.vector)) {
  result.60k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.60k, K.vector, seq(1,L.vector[i]))
  names(result.60k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.65k <- list()
for (i in 1:length(L.vector)) {
  result.65k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.65k, K.vector, seq(1,L.vector[i]))
  names(result.65k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.70k <- list()
for (i in 1:length(L.vector)) {
  result.70k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.70k, K.vector, seq(1,L.vector[i]))
  names(result.70k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
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
result.75k <- list()
for (i in 1:length(L.vector)) {
  result.75k[[i]] <- get.recovered.L(pa.deg.phi.MCsim.75k, K.vector, seq(1,L.vector[i]))
  names(result.75k)[i] <- paste0('Recover.top.', seq(1,length(L.vector))[i])
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[15] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.75k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !













# -------------------------------------------------------------------------------






# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# ----- 1. Initialize df to store recovery rates per method
pa.recovered.L.deg.naive <- data.frame(matrix(ncol = 1+length(K.vector)))
colnames(pa.recovered.L.deg.naive) <- c('Graph_Size', paste0('K.', K.vector))   # Rename df
pa.recovered.L.phi.naive <- pa.recovered.L.deg.naive
all.results <- list( result.5k, result.10k, result.15k, result.20k, 
                     result.25k, result.30k, result.35k, result.40k,
                     result.45k, result.50k, result.55k, result.60k,
                     result.65k, result.70k, result.75k
)

# ----- 2. Get rate: Maximum Degree
for (l in 1:length(L.vector)) {
  for (i in 1:length(all.results)) {
    current.result <- all.results[[i]][[l]]
    pa.recovered.L.deg.naive[length(all.results)*(l-1) + i,] <- current.result['via_Degrees'][[1]]
  }
}
pa.recovered.L.deg.naive$Vertex.to.Recover <- rep(L.vector, each = length(all.results))
print('Recovery rate: via Degree'); pa.recovered.1.deg

# ----- 3. Get rate: Minimum Maximum Subgraphs
for (l in 1:length(L.vector)) {
  for (i in 1:length(all.results)) {
    current.result <- all.results[[i]][[l]]
    pa.recovered.L.phi.naive[length(all.results)*(l-1) + i,] <- current.result['via_Subgraphs'][[1]]
  }
}
pa.recovered.L.phi.naive$Vertex.to.Recover <- rep(L.vector, each = length(all.results))
print('Recovery rate: via Phi'); pa.recovered.1.phi
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !



# ============================================================================ !
# ----- Save df -------------------------------------------------------------- !
write.csv(pa.recovered.L.deg.naive,
          file = 'Pref Attm - Recovery Rate - Vertices 1-10 - Naive Approach - Via Degrees.csv', row.names = TRUE)
write.csv(pa.recovered.L.phi.naive,
          file = 'Pref Attm - Recovery Rate - Vertices 1-10 - Naive Approach - Via Phi values.csv', row.names = TRUE)

save(pa.recovered.L.deg.naive, file = 'Pref Attm - Recovery Rate - Vertices 1-10 - Naive Approach - Via Degrees.RData')
save(pa.recovered.L.phi.naive, file = 'Pref Attm - Recovery Rate - Vertices 1-10 - Naive Approach - Via Phi values.RData')

 
















