# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Recover L vertices:                                  ====== !
# ===== ------------------ Local search: all in 1 basket                ====== !
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
# ============================================================================ !





# ============================================================================ !
# ----- Fn to get Recovery Rate ---------------------------------------------- !
# -----                                                                 ------ !
get.recovery.rate <- function(L.vertices, list.returned.vertices) {
  
  # Input:
  # ------- list.returned.vertices: list of returned vertices per simulation
  # ------- L.vertices:             first L vertices to be recovred
  
  # Output: 
  # ------- recovery.rate:
  
  recovery.rate <- c()  # Initialize recovery rate
  for (i in 1:1000) {
    # Check for recovery per simulation
    recovery.rate[i] <- all(seq(1, L.vertices) %in% list.returned.vertices[[i]])
  }
  return( sum(recovery.rate)/1000 )
} # ---------------------------------------------------------------- !
# ============================================================================ !







# ============================================================================ !
# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- seq(2,40,2)
L.steps <- c(5,10,15,20)
known.vertices <- c()
print(paste0('Number of vertices returned per step: ', paste0(K.vector, collapse = ',')))
print(paste0('Number of vertices to be recovered: ', paste0(L.steps, collapse = ',')))
print(paste0('We assume ', length(known.vertices), ' known vertices'))
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !




# ============================================================================ !
# ----- Time tracking -------------------------------------------------------- !
# -----                                                                 ------ !
time.tracking <- data.frame(Graph_Size = seq(5000, 80000, by = 5000))
time.tracking$Time <- rep(0, nrow(time.tracking))
time.tracking
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
result.5k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.5k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.5k, pa.deg.phi.MCsim.5k, 
                                                    known.vertices, K, L)
    result.5k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.5k[[l]] <- result.5k.tmp
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
# ----- Graph size: 10,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 10k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 10k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.10k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.10k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.10k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.10k, pa.deg.phi.MCsim.10k, 
                                                    known.vertices, K, L)
    result.10k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.10k[[l]] <- result.10k.tmp
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
# ----- Graph size: 15,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 15k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 15k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.15k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.15k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.15k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.15k, pa.deg.phi.MCsim.15k, 
                                                    known.vertices, K, L)
    result.15k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.15k[[l]] <- result.15k.tmp
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
# ----- Graph size: 20,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 20k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 20k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.20k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.20k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.20k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.20k, pa.deg.phi.MCsim.20k, 
                                                    known.vertices, K, L)
    result.20k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.20k[[l]] <- result.20k.tmp
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
# ----- Graph size: 25,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 25k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 25k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.25k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.25k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.25k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.25k, pa.deg.phi.MCsim.25k, 
                                                    known.vertices, K, L)
    result.25k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.25k[[l]] <- result.25k.tmp
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
# ----- Graph size: 30,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 30k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 30k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.30k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.30k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.30k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.30k, pa.deg.phi.MCsim.30k, 
                                                    known.vertices, K, L)
    result.30k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.30k[[l]] <- result.30k.tmp
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
# ----- Graph size: 35,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 35k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 35k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.35k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.35k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.35k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.35k, pa.deg.phi.MCsim.35k, 
                                                    known.vertices, K, L)
    result.35k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.35k[[l]] <- result.35k.tmp
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
# ----- Graph size: 40,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 40k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 40k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.40k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.40k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.40k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.40k, pa.deg.phi.MCsim.40k, 
                                                    known.vertices, K, L)
    result.40k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.40k[[l]] <- result.40k.tmp
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
# ----- Graph size: 45,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 45k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 45k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.45k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.45k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.45k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.45k, pa.deg.phi.MCsim.45k, 
                                                    known.vertices, K, L)
    result.45k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.45k[[l]] <- result.45k.tmp
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
# ----- Graph size: 50,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 50k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 50k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.50k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.50k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.50k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.50k, pa.deg.phi.MCsim.50k, 
                                                    known.vertices, K, L)
    result.50k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.50k[[l]] <- result.50k.tmp
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
# ----- Graph size: 55,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 55k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 55k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.55k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.55k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.55k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.55k, pa.deg.phi.MCsim.55k, 
                                                    known.vertices, K, L)
    result.55k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.55k[[l]] <- result.55k.tmp
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
# ----- Graph size: 60,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 60k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 60k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.60k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.60k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.60k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.60k, pa.deg.phi.MCsim.60k, 
                                                    known.vertices, K, L)
    result.60k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.60k[[l]] <- result.60k.tmp
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
# ----- Graph size: 65,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 65k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 65k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.65k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.65k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.65k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.65k, pa.deg.phi.MCsim.65k, 
                                                    known.vertices, K, L)
    result.65k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.65k[[l]] <- result.65k.tmp
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
# ----- Graph size: 70,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 70k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 70k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.70k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.70k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.70k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.70k, pa.deg.phi.MCsim.70k, 
                                                    known.vertices, K, L)
    result.70k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.70k[[l]] <- result.70k.tmp
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
# ----- Graph size: 75,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 75k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 75k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.75k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.75k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.75k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.75k, pa.deg.phi.MCsim.75k, 
                                                    known.vertices, K, L)
    result.75k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.75k[[l]] <- result.75k.tmp
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
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 75,000 ------------------------------------------------------
# -----                                                                 ------ !
# ----- 1. Load data
load('Pref Attm - List of neighboring vertices - 80k.RData')
load('Pref Attm - Degree - Phi - 1000 MC sim - Graph size - 80k.RData')
print(paste0('Graph size: ', nrow(pa.deg.phi.MCsim.80k[[1]])))
# ----- 2. Ger recovery rates
start.time <- Sys.time()        # Start timer for current size
result.80k <- list()
for (l in 1:length(L.steps)) {
  L <- L.steps[l]    # Get L vertices to be recovered
  result.80k.tmp <- c()
  for (k in 1:length(K.vector)) {
    K <- K.vector[k]
    list.returned.vertices <- local.search.1.basket(pref.attm.nbhd.80k, pa.deg.phi.MCsim.80k, 
                                                    known.vertices, K, L)
    result.80k.tmp[k] <- get.recovery.rate(L, list.returned.vertices)
  }
  result.80k[[l]] <- result.80k.tmp
}
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
# ----- 3. Update time tracking
time.tracking$Time[16] <- round(run.time,2)
# -----                                                                 ------ !
rm(pa.deg.phi.MCsim.80k, pref.attm.nbhd.80k)
# ---------------------------------------------------------------------------- !
# ---------------------------------------------------------------------------------- !


time.tracking
sum(time.tracking$Time)






# ============================================================================ !
# ----- Gather Recovery rate into df ----------------------------------------- !
# -----                                                                 ------ !
# ----- 1. Initialize df to store recovery rates per method
pa.recovered.L.local.search <- data.frame(matrix(ncol = 2+length(K.vector)))
colnames(pa.recovered.L.local.search) <- c('Graph_Size', 'L',
                                        paste0('K.', K.vector))   # Rename df
all.results <- list( result.5k, result.10k, result.15k, result.20k, 
                     result.25k, result.30k, result.35k, result.40k,
                     result.45k, result.50k, result.55k, result.60k,
                     result.65k, result.70k, result.75k
)
graph.sizes <- seq(5000, 75000, by = 5000)
# ----- 2. Get rate: Minimum Maximum Subgraph
for (l in 1:length(L.steps)) {
  for (i in 1:length(all.results)) {
    current.result <- all.results[[i]][[l]]
    pa.recovered.L.local.search[length(all.results)*(l-1) + i,] <- c(graph.sizes[i], L.steps[l], current.result) 
  }
}
print('Recovery of L vertices rates: via Phi values'); pa.recovered.L.local.search

# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !




# ============================================================================ !
# ----- Save df -------------------------------------------------------------- !
write.csv(pa.recovered.L.local.search,
          file = 'Pref Attm - Recovery Rate - Vertices 5-10 - Local Search - Constant K - Via Phi values.csv', row.names = TRUE)
save(pa.recovered.L.local.search, 
     file = 'Pref Attm - Recovery Rate - Vertices 5-10 - Local Search - Constant K - Via Phi values.RData')



















