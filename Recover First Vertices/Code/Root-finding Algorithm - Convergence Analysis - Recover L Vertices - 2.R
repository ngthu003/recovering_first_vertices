# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Convergence Analysis on Recovery Rate                ====== !
# ===== ---------- Recover L vertices:                                  ====== !
# ===== ------------------ Local search: all in 1 basket                ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !


# Load libraries ------------------------------------------------------------- !
library(ggplot2); library(ggthemes); library(tidyverse); 
library(reshape2); library(ggrepel)


# Load Recovery Rates data --------------------------------------------------- !
load('Pref Attm - Recovery Rate - Vertices 5-10 - Local Search - Constant K - Via Phi values.RData')
load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi values.RData')
load('Pref Attm - Recovery Rate - Vertices 1-10 - Naive Approach - Via Phi values.RData')


# ============================================================================ !
# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- seq(2,50,2)
L.steps <- c(5,10)
print(paste0('Number of vertices returned per step: ', paste0(K.vector, collapse = ',')))
print(paste0('Number of vertices to be recovered: ', paste0(L.steps, collapse = ',')))
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !



# ============================================================================ !
# ----- helper fn to correct K ----------------------------------------------- !
# -----                                                                 ------ !
replace.K <- function(x) {
  return(as.numeric(gsub('K', '', x)))
} # -----                                                               ------ !
# ---------------------------------------------------------------------------- !


# ============================================================================ !
# ---------------------------------------------------------------------------- !
# ----- 1) n = 50,000                                                   --------
# Get data
df <- pa.recovered.L.local.search[c(10,26),c(1,2,seq(3,27, by = 2))]
# Prepare data
K.each.step <- rep(seq(2,50,2), each = 4)
K.vector.reduced <- seq(2,50, by = 4)
rownames(df) <- seq(1,nrow(df))
colnames(df) <- c('Graph_Size', 'L', paste0('K', K.vector.reduced))
df <- df[,-1]
# Melt data for plotting
df.melted <- melt(df, id.vars = 'L')
df.melted$variable <- sapply(df.melted$variable, replace.K)
point.labels <- df.melted$variable * df.melted$L
df.melted$L <- factor(c(5,10), levels = c(5,10))
df.melted$Labels <- point.labels
# Plot
K.s <- seq(2, 50, by=4)
proportion.cuts <- seq(0, 1, by = .1)
df.melted %>%
  ggplot(aes(x = variable, y = value, label = Labels)) +
  geom_point(aes(color = L), size = 5) +
  geom_label_repel(point.padding = 1, direction = 'y',
                   nudge_y = .01, nudge_x = .1) +
  geom_line(aes(color = L, group = L), size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(0, 50), labels = K.s, breaks = K.s) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Graph size: 50,000; Over different K vertices returned at each of the L vertices to be recovered; 
       Also labeled are the total number of vertices returned at the end of recovering those L vertices.',
       title = 'Pref. Attm. Model on Tree:
                Recovery of The First L Vertices
                Method: Local Search with Constant K per Step',
       x = 'K (per step)', y = 'Proportion',
       color = 'First L vertices to recover') +
  scale_color_hue(labels = c('First 1', 'First 5', 'First 10')) +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_text(size = 16),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.text = element_text(size = 12),
        axis.title = element_text(face = 'bold', size = 16)
  )


# ============================================================================ !
# ---------------------------------------------------------------------------- !
# ----- 2) L = 5                                                        --------
# Get data
df <- pa.recovered.L.local.search[c(1:16),c(1,2,7,12,17,22,27)]
# Prepare data
K.each.step <- rep(seq(10,50,10), each = 4)
K.vector.reduced <- seq(10,50, by = 10)
rownames(df) <- seq(1,nrow(df))
colnames(df) <- c('Graph_Size', 'L', paste0('K', K.vector.reduced))
df <- df[,-2]
# Melt data for plotting
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted$variable <- sapply(df.melted$variable, replace.K)
df.melted$variable <- factor(df.melted$variable,
                             levels = seq(10,50,by=10))
# Plot
graph_sizes <- df$Graph_Size
proportion.cuts <- seq(0, 1, by = .1)
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value)) +
  geom_point(aes(color = variable), size = 5) +
  geom_line(aes(color = variable, group = variable), size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(0, 80000), labels = graph_sizes, breaks = graph_sizes) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Over different graph size; Over different K vertices returned at each of the L vertices to be recovered;',
       title = 'Pref. Attm. Model on Tree:
                Recovery of The First 5 Vertices
                Method: Local Search with Constant K per Step',
       x = 'Graph Size', y = 'Proportion',
       color = 'K (per step)') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_text(size = 16),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.text = element_text(size = 12),
        axis.title = element_text(face = 'bold', size = 16)
  )



# ============================================================================ !
# ---------------------------------------------------------------------------- !
# ----- 3) L = 10                                                       --------
# Get data
df <- pa.recovered.L.local.search[c(17:32),c(1,2,7,12,17,22,27)]
# Prepare data
K.each.step <- rep(seq(10,50,10), each = 4)
K.vector.reduced <- seq(10,50, by = 10)
rownames(df) <- seq(1,nrow(df))
colnames(df) <- c('Graph_Size', 'L', paste0('K', K.vector.reduced))
df <- df[,-2]
# Melt data for plotting
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted$variable <- sapply(df.melted$variable, replace.K)
df.melted$variable <- factor(df.melted$variable,
                             levels = seq(10,50,by=10))
# Plot
graph_sizes <- df$Graph_Size
proportion.cuts <- seq(0, 1, by = .1)
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value)) +
  geom_point(aes(color = variable), size = 5) +
  geom_line(aes(color = variable, group = variable), size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(0, 80000), labels = graph_sizes, breaks = graph_sizes) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Over different graph size; Over different K vertices returned at each of the L vertices to be recovered;',
       title = 'Pref. Attm. Model on Tree:
                Recovery of The First 10 Vertices
                Method: Local Search with Constant K per Step',
       x = 'Graph Size', y = 'Proportion',
       color = 'K (per step)') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_text(size = 16),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.text = element_text(size = 12),
        axis.title = element_text(face = 'bold', size = 16)
  )


# ============================================================================ !
# ---------------------------------------------------------------------------- !
# ----- 4) L = 5,10                                                     --------
# Get data
df <- pa.recovered.L.local.search %>%
  select(Graph_Size, L, K.10, K.30, K.50)
# Prepare data
K.each.step <- rep(c(10,50,10), each = 2)
rownames(df) <- seq(1,nrow(df))
colnames(df) <- c('Graph_Size', 'L', paste0('K', c(10, 30, 50)))
# Melt data for plotting
df.melted <- melt(df, id.vars = c('Graph_Size', 'L'))
df.melted$variable <- sapply(df.melted$variable, replace.K)
df.melted$L <- factor(df.melted$L, levels = c(5,10))
df.melted$variable <- factor(df.melted$variable,
                             levels = c(seq(10,50,20)))
# Plot
graph_sizes <- df$Graph_Size
proportion.cuts <- seq(0, 1, by = .1)
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, color = variable)) +
  geom_point(size = 5) +
  geom_line(aes(linetype = L), size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(0, 80000), labels = graph_sizes, breaks = graph_sizes) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Over different graph size; Over different K vertices returned at each of the L vertices to be recovered;',
       title = 'Pref. Attm. Model on Tree:
                Recovery of The First 5 and 10 Vertices
                Method: Local Search with Constant K per Step',
       x = 'Graph Size', y = 'Proportion',
       color = 'K (per step)',
       linetype = 'First L vertices to recover') +
  scale_linetype_manual(values = c(1,2), labels = c('First 5', 'First 10')) +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_text(size = 16),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.text = element_text(size = 12),
        legend.key.size = unit(5,"line"),
        axis.title = element_text(face = 'bold', size = 16)
  )
