# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Given each graph:                                    ====== !
# ===== ------------------ Get vertex degree & vertex phi value         ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !


# Load libraries ------------------------------------------------------------- !
library(tidyverse); library(igraph); library(sna)




# ============================================================================ !
# ----- Fn to get stats per single graph ------------------------------------- !
# -----                                                                 ------ !
get_degree_phi_1_graph <- function(inc_matrix) {
  
  # Input:
  # ------- inc_matrix:   incidence matrix (of 1 single graph)
  # Output: 
  # ------- df_deg_phi:   matrix of 3 columns:
  # --------------------- (1): True label
  # --------------------- (2): Degree
  # --------------------- (3): Phi value
  
  # FIXME
  # ----- Time Complexity: not going to compute Phi for all vertices
  # ---------------------- compute phi of the first 5,000 vertices
  # ------------------------------ all else: assign Phi(u) = n-1 <=> u: a leaf
  
  # TODO
  # ---- 1. Convert: inc_matrix => graph and get |V|
  # ---- 2. Initialize:
  # ------- 2.1. df_deg_phi
  # ---- 3. For each vertex, calculate:
  # ------- 3.1. Degree
  # ------- 3.2. Phi_value
  
  # TODO Step 1 ---------------------------------------------------- !
  graph <- graph.edgelist(inc_matrix, directed = FALSE);  # convert to graph
  n <- nrow(inc_matrix) + 1                               # get |V|
  # ----------------------------------------------------- End Step 1 !
  
  # TODO Step 2 ---------------------------------------------------- !
  # ---- 2.1.
  df_deg_phi <- matrix(data = c(seq(1,n,by=1), 
                                rep(0,n * 2)), 
                       ncol = 3, byrow = FALSE)           # Initialize df
  colnames(df_deg_phi) <- c('True_Label', 'Degree', 'Phi_value')
  # ----------------------------------------------------- End Step 2 !
  
  # TODO Step 3 ---------------------------------------------------- !
  # ---- 3.1.
  tmp.deg <- igraph::degree(graph)
  df_deg_phi[, 'Degree'] <- tmp.deg
  # ---- 3.2.
  tmp.phi <- rep(n-1,n)
  for (u in 1:5000) {
    if (tmp.deg[u] != 1) {   # Check for if deg(u) = 1, if not compute phi(u)
      tmp.graph <- delete_vertices(graph, u)       # Subgraph w/Out u
      tmp.phi[u] <- max(clusters(tmp.graph)$csize) # Max. subgraph  
    }
  }
  df_deg_phi[, 'Phi_value'] <- tmp.phi
  # ----------------------------------------------------- End Step 3 !
  
  return(df_deg_phi)
  
} # ---------------------------------------------------------------- !




# ============================================================================ !
# ----- Fn to get stats for all simulations per graph size ------------------- !
# -----                                                                 ------ !
get_degree_phi_Multiple_graphs <- function(inc_matrix_list) {
  
  # Input:
  # ------- inc_matrix_list:  list of multiple incidence matrices
  # Output: 
  # ------- df_deg_all_graphs: matrix of 3 columns:
  # -------------------------- (1): True label
  # -------------------------- (2:-): Degree(u) for all vertices (row)
  # ----------------------------------------------- all simulations (col)
  # ------- df_phi_all_graphs: similar, but for Phi_value
  
  # TODO
  # ---- 1. Initialize: df_deg_all_graphs, df_phi_all_graphs,
  # ------------------- n: graph size, no_sim: #(simulations)
  # ---- 2. For each simulation, calculate on entire graph:
  # ------- 2.1. Degree 
  # ------- 2.2. Phi_value
  
  # Require:
  # -------- get_degree_phi_1_graph(inc_matrix)
  
  # TODO Step 1 ---------------------------------------------------- !
  no_sim <- length(inc_matrix_list)
  # no_sim <- 5
  n <- nrow(inc_matrix_list[[1]]) + 1
  df_deg_all_graphs <- matrix(data = c(seq(1,n,by=1), rep(0,n*no_sim)), 
                              ncol = no_sim + 1, byrow = FALSE)           # Initialize df
  colnames(df_deg_all_graphs) <- c('True_Label', seq(1, no_sim, by = 1))
  df_phi_all_graphs <- df_deg_all_graphs
  # ----------------------------------------------------- End Step 1 !
  
  
  # TODO Step 2 ---------------------------------------------------- !
  for (i in 1:no_sim) {     # Loop over each simulation
    
    if (i %% 100 == 0) { print(paste0('Current batch: ', i)) }               # to track i = 100, 200, ...
    
    current_inc_matrix <- inc_matrix_list[[i]]    # Get current graph
    tmp.degree_phi <- get_degree_phi_1_graph(current_inc_matrix) # Cal. degree & Phi
    # ---- 3.1. Degree
    df_deg_all_graphs[, i+1] <- tmp.degree_phi[, 'Degree']
    # ---- 3.1. Phi value
    df_phi_all_graphs[, i+1] <- tmp.degree_phi[, 'Phi_value']
  }
  # ----------------------------------------------------- End Step 2 !
  
  return(list(df_deg_all_graphs, df_phi_all_graphs))
  
} # ---------------------------------------------------------------- !
