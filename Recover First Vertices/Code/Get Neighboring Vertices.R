# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Get the list of neighboring vertices                 ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !


library(tidyverse); library(igraph); library(sna)


get.neighbors.graphs <- function(inc_matrix_list, n) {
  pref.attm.nbhd <- list()
  # Get current graph
  for (i in 1:length(inc_matrix_list)) {
    graph <- inc_matrix_list[[i]]
    graph <- graph.edgelist(graph, directed = FALSE)
    nbhd.list <- list()
    # Get neighbors of vertices from 1 to n
    for (u in 1:n) {
      nbhd.list[[u]] <- neighbors(graph, u)
    }
    pref.attm.nbhd[[i]] <- nbhd.list
  }  
  return(pref.attm.nbhd)
}




# ============================================================================ !
# ----- n: top n vertices to get neighbors from ------------------------------ !
# -----                                                                 ------ !
n <- 1000
print(paste0('We are getting neighbors from top ', n, ' vertices'))
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
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 5k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.5k[[1]]) + 1))
pref.attm.nbhd.5k <- get.neighbors.graphs(pref.attm.simulation.5k, n)
save(pref.attm.nbhd.5k, 
     file = 'Pref Attm - List of neighboring vertices - 5k.RData')
rm(pref.attm.simulation.5k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[1] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 10,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 10k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.10k[[1]]) + 1))
pref.attm.nbhd.10k <- get.neighbors.graphs(pref.attm.simulation.10k, n)
save(pref.attm.nbhd.10k, 
     file = 'Pref Attm - List of neighboring vertices - 10k.RData')
rm(pref.attm.simulation.10k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[2] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 15,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 15k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.15k[[1]]) + 1))
pref.attm.nbhd.15k <- get.neighbors.graphs(pref.attm.simulation.15k, n)
save(pref.attm.nbhd.15k, 
     file = 'Pref Attm - List of neighboring vertices - 15k.RData')
rm(pref.attm.simulation.15k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[3] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 20,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 20k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.20k[[1]]) + 1))
pref.attm.nbhd.20k <- get.neighbors.graphs(pref.attm.simulation.20k, n)
save(pref.attm.nbhd.20k, 
     file = 'Pref Attm - List of neighboring vertices - 20k.RData')
rm(pref.attm.simulation.20k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[4] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 25,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 25k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.25k[[1]]) + 1))
pref.attm.nbhd.25k <- get.neighbors.graphs(pref.attm.simulation.25k, n)
save(pref.attm.nbhd.25k, 
     file = 'Pref Attm - List of neighboring vertices - 25k.RData')
rm(pref.attm.simulation.25k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[5] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 30,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 30k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.30k[[1]]) + 1))
pref.attm.nbhd.30k <- get.neighbors.graphs(pref.attm.simulation.30k, n)
save(pref.attm.nbhd.30k, 
     file = 'Pref Attm - List of neighboring vertices - 30k.RData')
rm(pref.attm.simulation.30k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[6] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 35,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 35k.RData')
print(paste0('Graph size: ', nrow(pref.attm.simulation.35k[[1]]) + 1))
pref.attm.nbhd.35k <- get.neighbors.graphs(pref.attm.simulation.35k, n)
save(pref.attm.nbhd.35k, 
     file = 'Pref Attm - List of neighboring vertices - 35k.RData')
rm(pref.attm.simulation.35k)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[7] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 40,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 40k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 40k Part 2.RData')
mylist2 <- pref.attm.simulation.40k.2[which(!sapply(pref.attm.simulation.40k.2, is.null))]
pref.attm.simulation.40k <- append(pref.attm.simulation.40k, mylist2)
print(paste0('Graph size: ', nrow(pref.attm.simulation.40k[[1]]) + 1))
pref.attm.nbhd.40k <- get.neighbors.graphs(pref.attm.simulation.40k, n)
save(pref.attm.nbhd.40k, 
     file = 'Pref Attm - List of neighboring vertices - 40k.RData')
rm(pref.attm.simulation.40k, pref.attm.simulation.40k.2)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[8] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 45,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 45k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 45k Part 2.RData')
mylist2 <- pref.attm.simulation.45k.2[which(!sapply(pref.attm.simulation.45k.2, is.null))]
pref.attm.simulation.45k <- append(pref.attm.simulation.45k, mylist2)
print(paste0('Graph size: ', nrow(pref.attm.simulation.45k[[1]]) + 1))
pref.attm.nbhd.45k <- get.neighbors.graphs(pref.attm.simulation.45k, n)
save(pref.attm.nbhd.45k, 
     file = 'Pref Attm - List of neighboring vertices - 45k.RData')
rm(pref.attm.simulation.45k, pref.attm.simulation.45k.2)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[9] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 50,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 50k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 50k Part 2.RData')
mylist2 <- pref.attm.simulation.50k.2[which(!sapply(pref.attm.simulation.50k.2, is.null))]
pref.attm.simulation.50k <- append(pref.attm.simulation.50k, mylist2)
print(paste0('Graph size: ', nrow(pref.attm.simulation.50k[[1]]) + 1))
pref.attm.nbhd.50k <- get.neighbors.graphs(pref.attm.simulation.50k, n)
save(pref.attm.nbhd.50k, 
     file = 'Pref Attm - List of neighboring vertices - 50k.RData')
rm(pref.attm.simulation.50k, pref.attm.simulation.50k.2)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[10] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 55,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 55k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 55k Part 2.RData')
load('PA on Tree - 1000 MC Simulations - n = 55k Part 3.RData')
mylist2 <- pref.attm.simulation.55k.2[which(!sapply(pref.attm.simulation.55k.2, is.null))]
pref.attm.simulation.55k <- append(pref.attm.simulation.55k, mylist2)
mylist3 <- pref.attm.simulation.55k.3[which(!sapply(pref.attm.simulation.55k.3, is.null))]
pref.attm.simulation.55k <- append(pref.attm.simulation.55k, mylist3)
print(paste0('Graph size: ', nrow(pref.attm.simulation.55k[[1]]) + 1))
pref.attm.nbhd.55k <- get.neighbors.graphs(pref.attm.simulation.55k, n)
save(pref.attm.nbhd.55k, 
     file = 'Pref Attm - List of neighboring vertices - 55k.RData')
rm(pref.attm.simulation.55k, pref.attm.simulation.55k.2, pref.attm.simulation.55k.3)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[11] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 60,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 60k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 60k Part 2.RData')
load('PA on Tree - 1000 MC Simulations - n = 60k Part 3.RData')
mylist2 <- pref.attm.simulation.60k.2[which(!sapply(pref.attm.simulation.60k.2, is.null))]
pref.attm.simulation.60k <- append(pref.attm.simulation.60k, mylist2)
mylist3 <- pref.attm.simulation.60k.3[which(!sapply(pref.attm.simulation.60k.3, is.null))]
pref.attm.simulation.60k <- append(pref.attm.simulation.60k, mylist3)
print(paste0('Graph size: ', nrow(pref.attm.simulation.60k[[1]]) + 1))
pref.attm.nbhd.60k <- get.neighbors.graphs(pref.attm.simulation.60k, n)
save(pref.attm.nbhd.60k, 
     file = 'Pref Attm - List of neighboring vertices - 60k.RData')
rm(pref.attm.simulation.60k, pref.attm.simulation.60k.2, pref.attm.simulation.60k.3)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[12] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 65,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 65k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 65k Part 2.RData')
load('PA on Tree - 1000 MC Simulations - n = 65k Part 3.RData')
load('PA on Tree - 1000 MC Simulations - n = 65k Part 4.RData')
mylist2 <- pref.attm.simulation.65k.2[which(!sapply(pref.attm.simulation.65k.2, is.null))]
pref.attm.simulation.65k <- append(pref.attm.simulation.65k, mylist2)
mylist3 <- pref.attm.simulation.65k.3[which(!sapply(pref.attm.simulation.65k.3, is.null))]
pref.attm.simulation.65k <- append(pref.attm.simulation.65k, mylist3)
mylist4 <- pref.attm.simulation.65k.4[which(!sapply(pref.attm.simulation.65k.4, is.null))]
pref.attm.simulation.65k <- append(pref.attm.simulation.65k, mylist4)
print(paste0('Graph size: ', nrow(pref.attm.simulation.65k[[1]]) + 1))
pref.attm.nbhd.65k <- get.neighbors.graphs(pref.attm.simulation.65k, n)
save(pref.attm.nbhd.65k, 
     file = 'Pref Attm - List of neighboring vertices - 65k.RData')
rm(pref.attm.simulation.65k, pref.attm.simulation.65k.2, 
   pref.attm.simulation.65k.3, pref.attm.simulation.65k.4)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[13] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 70,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 70k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 70k Part 2.RData')
load('PA on Tree - 1000 MC Simulations - n = 70k Part 3.RData')
load('PA on Tree - 1000 MC Simulations - n = 70k Part 4.RData')
mylist2 <- pref.attm.simulation.70k.2[which(!sapply(pref.attm.simulation.70k.2, is.null))]
pref.attm.simulation.70k <- append(pref.attm.simulation.70k, mylist2)
mylist3 <- pref.attm.simulation.70k.3[which(!sapply(pref.attm.simulation.70k.3, is.null))]
pref.attm.simulation.70k <- append(pref.attm.simulation.70k, mylist3)
mylist4 <- pref.attm.simulation.70k.4[which(!sapply(pref.attm.simulation.70k.4, is.null))]
pref.attm.simulation.70k <- append(pref.attm.simulation.70k, mylist4)
print(paste0('Graph size: ', nrow(pref.attm.simulation.70k[[1]]) + 1))
pref.attm.nbhd.70k <- get.neighbors.graphs(pref.attm.simulation.70k, n)
save(pref.attm.nbhd.70k, 
     file = 'Pref Attm - List of neighboring vertices - 70k.RData')
rm(pref.attm.simulation.70k, pref.attm.simulation.70k.2, 
   pref.attm.simulation.70k.3, pref.attm.simulation.70k.4)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[14] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 75,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 75k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 75k Part 2.RData')
load('PA on Tree - 1000 MC Simulations - n = 75k Part 3.RData')
load('PA on Tree - 1000 MC Simulations - n = 75k Part 4.RData')
mylist2 <- pref.attm.simulation.75k.2[which(!sapply(pref.attm.simulation.75k.2, is.null))]
pref.attm.simulation.75k <- append(pref.attm.simulation.75k, mylist2)
mylist3 <- pref.attm.simulation.75k.3[which(!sapply(pref.attm.simulation.75k.3, is.null))]
pref.attm.simulation.75k <- append(pref.attm.simulation.75k, mylist3)
mylist4 <- pref.attm.simulation.75k.4[which(!sapply(pref.attm.simulation.75k.4, is.null))]
pref.attm.simulation.75k <- append(pref.attm.simulation.75k, mylist4)
print(paste0('Graph size: ', nrow(pref.attm.simulation.75k[[1]]) + 1))
pref.attm.nbhd.75k <- get.neighbors.graphs(pref.attm.simulation.75k, n)
save(pref.attm.nbhd.75k, 
     file = 'Pref Attm - List of neighboring vertices - 75k.RData')
rm(pref.attm.simulation.75k, pref.attm.simulation.75k.2, 
   pref.attm.simulation.75k.3, pref.attm.simulation.75k.4)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[15] <- round(run.time,2)
# ---------------------------------------------------------------------------------- !
# ----- Graph size: 80,000 -----------------------------------------------------
# -----                                                                 ------ !
start.time <- Sys.time()        # Start timer for current size
load('PA on Tree - 1000 MC Simulations - n = 80k Part 1.RData')
load('PA on Tree - 1000 MC Simulations - n = 80k Part 2.RData')
mylist2 <- pref.attm.simulation.80k.2[which(!sapply(pref.attm.simulation.80k.2, is.null))]
pref.attm.simulation.80k <- append(pref.attm.simulation.80k, mylist2)
print(paste0('Graph size: ', nrow(pref.attm.simulation.80k[[1]]) + 1))
pref.attm.nbhd.80k <- get.neighbors.graphs(pref.attm.simulation.80k, n)
save(pref.attm.nbhd.80k, 
     file = 'Pref Attm - List of neighboring vertices - 80k.RData')
rm(pref.attm.simulation.80k, pref.attm.simulation.80k.2)
end.time <- Sys.time(); run.time <- difftime(end.time, start.time, units='mins');
print(paste0('Total run time: ', round(run.time,4), ' mins'))
time.tracking$Time[16] <- round(run.time,2)










































