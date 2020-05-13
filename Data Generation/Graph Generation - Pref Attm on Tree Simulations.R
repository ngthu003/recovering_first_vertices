library(tidyverse); library(igraph); library(sna)







# 1. fn to grow Unif.Attm graph --------------------------------------
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










# 2. Generating graphs: Time tracking --------------------------------

graph.sizes <- seq(0, 80000, by = 5000)[-1]
MC.simulation <- 5

pref.attm.Simulation.list <- list()

time.tracking <- data.frame(Graph_Size = graph.sizes, 
                            Number_Simulation = rep(MC.simulation, length(graph.sizes)),
                            Time = rep(0, length(graph.sizes)))

for (s in 1:length(graph.sizes)) {
  
  size <- graph.sizes[s]      # Get current graph size
  
  # Start time tracking
  start.time.n <- Sys.time(); print(paste0('Graph size: ', size))
  
  # Initialize list to store graphs of current size
  current.Simulation.list <- list()
  # Monte Carlo simulation given current graph size
  for (j in 1:MC.simulation) {
    # Generate graphs =============================== !
    current.adj.matrix <- pref.attm.tree.model(size, j)
    # ----------------------------------------------- !
    # Append to list of same graph size ============= !
    current.Simulation.list[[j]] <- current.adj.matrix
    # ----------------------------------------------- !
  }
    
  # Append to comprehensive list of all sizes ======= !
  pref.attm.Simulation.list[[s]] <- current.Simulation.list
  names(pref.attm.Simulation.list)[s] <- size
  # ------------------------------------------------- !
  
  # End time tracking
  end.time.n <- Sys.time(); run.time.n <- difftime(end.time.n, start.time.n, units='mins') 
  print(paste0('------------------ Run time: ', round(run.time.n,4), ' mins')) # Print timer for current size
  # Update time tracking
  time.tracking$Time[s] <- round(run.time.n,4)
  
}



# Predict time for 1,000 simulations each size
time.tracking$Prediction_1000 <- round((time.tracking$Time * 200)/60,2)
write.csv(time.tracking,
          'PrefAttm - Time tracking for 5 simulation per graph size.csv',
          row.names = FALSE)
write.csv(time.tracking,
          'PrefAttm - Time tracking for 5 simulation per graph size - with Row Names.csv',
          row.names = TRUE)


plot(x = time.tracking$Graph_Size, y = time.tracking$Prediction_1000,
     pch = 16, cex = 2, col = '#336699',
     bty = 'n',
     xlim = c(0, 80000), ylim = c(0, 30),
     xlab = 'Graph size', ylab = 'Hours',
     main = 'Prediction time for 1,000 simulations for Preferential Attachment model')
abline(h = 5, col = 'black', lty = 2)
abline(h = 10, col = 'black', lty = 2)
abline(h = 15, col = 'black', lty = 2)
abline(h = 20, col = 'black', lty = 2)
abline(h = 25, col = 'black', lty = 2)



























