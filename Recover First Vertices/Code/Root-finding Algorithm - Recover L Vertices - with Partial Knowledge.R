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
# ============================================================================ !











# ============================================================================ !
# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- c(seq(1, 9, by = 1), seq(10, 20, by = 2))
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




# ============================================================================ !
# ----- Recover 2 w/ Partial Knowledge -----------------------------------------
# -----                                                                 ------ !
known.and.to.recover.vertices <- list(
  list(c(1),2), 
  list(c(1,3),2), 
  list(c(1,5),2),
  # list(c(1,3,5,7),2), 
  # list(c(1,3,5,7,9),2),
  # list(c(1,seq(3,10)), 2),
  list(c(1,5,10),2)
  # list(seq(1,1), 2), list(seq(1,2), 3),
  # list(seq(1,3), 4), list(seq(1,4), 5),
  # list(seq(1,5), 6), list(seq(1,6), 7),
  # list(seq(1,7), 8), list(seq(1,8), 9),
  # list(seq(1,9), 10), list(seq(1,10), 11)
)
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !




# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 5,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 5k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 5k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.5k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.5k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.5k.all[[i]] <- recover.vert.n(pref.attm.nbhd.5k, pa.deg.phi.MCsim.5k, K.vector, 
                                       known.and.to.recover.vertices[[i]][[1]],
                                       known.and.to.recover.vertices[[i]][[2]])
  names(result.5k.all)[i] <- paste0('rec.2.known.', 
                                    paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[1] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.5k, pref.attm.nbhd.5k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 10,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 10k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 10k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.10k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.10k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.10k.all[[i]] <- recover.vert.n(pref.attm.nbhd.10k, pa.deg.phi.MCsim.10k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.10k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[2] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.10k, pref.attm.nbhd.10k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 15,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 15k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 15k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.15k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.15k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.15k.all[[i]] <- recover.vert.n(pref.attm.nbhd.15k, pa.deg.phi.MCsim.15k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.15k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[3] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.15k, pref.attm.nbhd.15k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 20,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 20k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 20k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.20k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.20k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.20k.all[[i]] <- recover.vert.n(pref.attm.nbhd.20k, pa.deg.phi.MCsim.20k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.20k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[4] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.20k, pref.attm.nbhd.20k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 25,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 25k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 25k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.25k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.25k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.25k.all[[i]] <- recover.vert.n(pref.attm.nbhd.25k, pa.deg.phi.MCsim.25k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.25k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[5] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.25k, pref.attm.nbhd.25k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 30,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 30k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 30k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.30k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.30k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.30k.all[[i]] <- recover.vert.n(pref.attm.nbhd.30k, pa.deg.phi.MCsim.30k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.30k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[6] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.30k, pref.attm.nbhd.30k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 35,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 35k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 35k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.35k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.35k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.35k.all[[i]] <- recover.vert.n(pref.attm.nbhd.35k, pa.deg.phi.MCsim.35k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.35k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[7] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.35k, pref.attm.nbhd.35k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 40,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 40k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 40k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.40k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.40k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.40k.all[[i]] <- recover.vert.n(pref.attm.nbhd.40k, pa.deg.phi.MCsim.40k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.40k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[8] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.40k, pref.attm.nbhd.40k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 45,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 45k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 45k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.45k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.45k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.45k.all[[i]] <- recover.vert.n(pref.attm.nbhd.45k, pa.deg.phi.MCsim.45k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.45k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[9] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.45k, pref.attm.nbhd.45k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 50,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 50k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 50k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.50k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.50k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.50k.all[[i]] <- recover.vert.n(pref.attm.nbhd.50k, pa.deg.phi.MCsim.50k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.50k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[10] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.50k, pref.attm.nbhd.50k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 55,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 55k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 55k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.55k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.55k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.55k.all[[i]] <- recover.vert.n(pref.attm.nbhd.55k, pa.deg.phi.MCsim.55k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.55k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[11] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.55k, pref.attm.nbhd.55k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 60,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 60k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 60k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.60k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.60k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.60k.all[[i]] <- recover.vert.n(pref.attm.nbhd.60k, pa.deg.phi.MCsim.60k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.60k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[12] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.60k, pref.attm.nbhd.60k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 65,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 65k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 65k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.65k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.65k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.65k.all[[i]] <- recover.vert.n(pref.attm.nbhd.65k, pa.deg.phi.MCsim.65k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.65k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[13] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.65k, pref.attm.nbhd.65k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 70,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 70k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 70k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.70k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.70k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.70k.all[[i]] <- recover.vert.n(pref.attm.nbhd.70k, pa.deg.phi.MCsim.70k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.70k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[14] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.70k, pref.attm.nbhd.70k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 75,000 -----------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 75k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 75k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.75k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.75k.all <- list()
for (i in 1:length(known.and.to.recover.vertices)) {
  result.75k.all[[i]] <- recover.vert.n(pref.attm.nbhd.75k, pa.deg.phi.MCsim.75k, K.vector, 
                                        known.and.to.recover.vertices[[i]][[1]],
                                        known.and.to.recover.vertices[[i]][[2]])
  names(result.75k.all)[i] <- paste0('rec.2.known.', 
                                     paste0(known.and.to.recover.vertices[[i]][[1]], collapse = '.'))
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[15] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.75k, pref.attm.nbhd.75k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !
# ======================================================================================== !













































# -------------------------------------------------------------------------------




# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# Super iist to contain recovery rates 
# ------------- of each of first 2-11 vertices
# ------------- over each graph size
rec.2.partial.knowledge <- list()
all.results <- list( result.5k.all, result.10k.all, result.15k.all, result.20k.all, 
                     result.25k.all, result.30k.all, result.35k.all, result.40k.all,
                     result.45k.all, result.50k.all, result.55k.all, result.60k.all,
                     result.65k.all, result.70k.all, result.75k.all
)
graph_size <- seq(5000, 75000, by = 5000)
# Loop over each vertex to be recovered
for (j in 1:4) {
  
  # ----- 1. Initialize temp. df
  tmp_df.deg <- data.frame(matrix(ncol = 1+length(K.vector)))
  colnames(tmp_df.deg) <- c('Graph_Size', paste0('K.', K.vector))   # Rename df
  tmp_df.phi <- tmp_df.deg
  
  # ----- 2. Get rate: Maximum Degree
  for (i in 1:length(all.results)) {
    current.result <- all.results[[i]][[j]]
    tmp_df.deg[i,] <- c(graph_size[i], current.result['via_Degrees'][[1]])
  }
  # ----- 3. Get rate: Minimum Maximum Subgraphs
  for (i in 1:length(all.results)) {
    current.result <- all.results[[i]][[j]]
    tmp_df.phi[i,] <- c(graph_size[i], current.result['via_Subgraphs'][[1]])
  }
  # -----                                                                 ------ !
  # ---------------------------------------------------------------------------- !
  
  # Return result
  rec.2.partial.knowledge[[j]] <- list(tmp_df.deg, tmp_df.phi)
  # Rename result
  names(rec.2.partial.knowledge)[j] <- paste0('rec.2.known.', 
                                              paste0(known.and.to.recover.vertices[[j]][[1]], 
                                                     collapse = '.'))
  names(rec.2.partial.knowledge[[j]]) <- c('via_Degree', 'via_Subgraph')
}
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !




# ============================================================================ !
# ----- Save df ----------------------------------------------------------------
save(rec.2.partial.knowledge, 
     file = 'Pref Attm - Recovery Rate - Vertex 2 - Partial Knowledge - Both Methods.RData')























