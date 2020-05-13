library(tidyverse); library(igraph); library(sna)







# 1. fn to grow Unif.Attm graph 
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







# 2. 1000 simulations each for different n ---------------------------

# n.list <- seq(0, 4 * 10^4, by = 5000)[-1]
# n.list <- seq(4 * 10^4, 5 * 10^4, by = 5000)[-1]
n.list <- seq(5 * 10^4, 6 * 10^4, by = 5000)[-1]

no.simulations <- 1000
# no.simulations <- 5

# List to store graphs of size up to 25k
unif.attm.simulation.list <- list()

start.time <- Sys.time()        # Start timer for current size

for (i in 1:length(n.list)) { # --------- Choose graph sizes from n.list() above
  
  # TODO
  # ---- Build graph of current size #(n.MC) times
  
  start.time.n <- Sys.time(); print(paste0('Graph size: ', n.list[i]))
  tmp.mc.sim.list <- list()     # List to store simulations of Current graph size n
  
  for (j in 1:no.simulations) {         # Build graph of current size #(n.MC) times
    
    # Grow graphs ------------------------------------------------
    tmp.inc.matrix <- unif.attm.tree.model(n.list[i], j)
    # -------------------------------------------------------- End
    
    tmp.mc.sim.list[[j]] <- tmp.inc.matrix               # Current simulation
  }
  
  unif.attm.simulation.list[[i]] <- tmp.mc.sim.list      # Add all simulations to list of n
  names(unif.attm.simulation.list)[i] <- n.list[i]       # Rename according graph size
  
  end.time.n <- Sys.time(); run.time.n <- difftime(end.time.n, start.time.n, units='mins') 
  print(paste0('------------------ Run time: ', round(run.time.n,4), ' mins')) # Print timer for current size
  
} # ---------------------------------------------------------------- !


end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins'); # End timer
print(paste0('Total run time: ', round(run.time,4), ' mins'))                     # Print timer









rm(tmp.inc.matrix, tmp.mc.sim.list, i, j,
   start.time, start.time.n, end.time, end.time.n, run.time, run.time.n)

View(unif.attm.simulation.list)
print(object.size(unif.attm.simulation.5k), units = 'Mb')
save(unif.attm.simulation.list, file = 'UA on Tree - 1000 MC Simulations - n - 5k-40k.RData')




# Save each graph size simulations

names(unif.attm.simulation.list)

# # n = 5,000
# unif.attm.simulation.5k <- unif.attm.simulation.list[[1]]
# View(unif.attm.simulation.5k)
# print(object.size(unif.attm.simulation.5k), units = 'Mb')
# save(unif.attm.simulation.5k, file = 'UA on Tree - 1000 MC Simulations - n = 5k.RData')
# 
# # n = 10,000
# unif.attm.simulation.10k <- unif.attm.simulation.list[[2]]
# View(unif.attm.simulation.10k)
# print(object.size(unif.attm.simulation.10k), units = 'Mb')
# save(unif.attm.simulation.10k, file = 'UA on Tree - 1000 MC Simulations - n = 10k.RData')
# 
# # n = 15,000
# unif.attm.simulation.15k <- unif.attm.simulation.list[[3]]
# print(object.size(unif.attm.simulation.15k), units = 'Mb')
# save(unif.attm.simulation.15k, file = 'UA on Tree - 1000 MC Simulations - n = 15k.RData')
# 
# # n = 15,000
# unif.attm.simulation.15k <- unif.attm.simulation.list[[3]]
# print(object.size(unif.attm.simulation.15k), units = 'Mb')
# save(unif.attm.simulation.15k, file = 'UA on Tree - 1000 MC Simulations - n = 15k.RData')
# 
# # n = 20,000
# unif.attm.simulation.20k <- unif.attm.simulation.list[[4]]
# print(object.size(unif.attm.simulation.20k), units = 'Mb')
# save(unif.attm.simulation.20k, file = 'UA on Tree - 1000 MC Simulations - n = 20k.RData')
# 
# # n = 25,000
# unif.attm.simulation.25k <- unif.attm.simulation.list[[5]]
# print(object.size(unif.attm.simulation.25k), units = 'Mb')
# save(unif.attm.simulation.25k, file = 'UA on Tree - 1000 MC Simulations - n = 25k.RData')
# 
# # n = 30,000
# unif.attm.simulation.30k <- unif.attm.simulation.list[[6]]
# print(object.size(unif.attm.simulation.30k), units = 'Mb')
# save(unif.attm.simulation.30k, file = 'UA on Tree - 1000 MC Simulations - n = 30k.RData')
# 
# # n = 35,000
# unif.attm.simulation.35k <- unif.attm.simulation.list[[7]]
# print(object.size(unif.attm.simulation.35k), units = 'Mb')
# save(unif.attm.simulation.35k, file = 'UA on Tree - 1000 MC Simulations - n = 35k.RData')
# 
# # n = 40,000
# unif.attm.simulation.40k <- unif.attm.simulation.list[[8]]
# print(object.size(unif.attm.simulation.40k), units = 'Mb')
# save(unif.attm.simulation.40k, file = 'UA on Tree - 1000 MC Simulations - n = 40k.RData')

# n = 55,000
unif.attm.simulation.55k <- unif.attm.simulation.list[[1]]
print(object.size(unif.attm.simulation.55k), units = 'Mb')
save(unif.attm.simulation.55k, file = 'UA on Tree - 1000 MC Simulations - n = 55k.RData')

# n = 60,000
unif.attm.simulation.60k <- unif.attm.simulation.list[[2]]
print(object.size(unif.attm.simulation.60k), units = 'Mb')
save(unif.attm.simulation.60k, file = 'UA on Tree - 1000 MC Simulations - n = 60k.RData')







































