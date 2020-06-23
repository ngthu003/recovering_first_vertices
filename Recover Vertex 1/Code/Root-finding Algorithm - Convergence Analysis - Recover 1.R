# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Convergence Analysis on Recovery Rate                ====== !
# ===== ------------------ (1) Maximum Degree                           ====== !
# ===== ------------------ (1) Minimum Maximum Subgraph                 ====== !
# =====                                                                 ====== !
# ============================================================================ !
# ============================================================================ !
# ============================================================================ !


# Statistical Analysis on Convergence of Recovery Rate

library(ggplot2); library(ggthemes); library(tidyverse); library(reshape2)

load('Pref Attm - Recovery Rate - Vertex 1 - Via Degrees.RData')
load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi values.RData')


# ============================================================================ !
# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- c(seq(1, 9, by = 1), seq(10, 100, by = 5))
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !





pa.recovered.1.deg



# ============================================================================ !
# ----- A. Subgraphs - Phi values ----------------------------------------------
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !
# ----- 1) n = 50,000                                                   --------
df.t <- as.data.frame(t(pa.recovered.1.phi))
colnames(df.t) <- paste0('V', seq(1,ncol(df.t),by=1))      # Rename df
proportion.cuts <- seq(0, 1, by = .1)
graph_size <- pa.recovered.1.phi$Graph_Size
K.s <- seq(10, 100, by = 10)
df.t %>%
  slice(2:n()) %>%
  ggplot(aes(x = K.vector, y = V10)) +
  geom_point(size = 5, color = '#336699') +
  geom_line(color = '#336699', size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(1,100), labels = K.s, breaks = K.s) +
  scale_y_continuous(limits = c(0, 1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = paste0('Proportion of times vertex 1 was recovered on graph of size n = ', graph_size[10]),
       title = 'Pref. Attm. Model on Tree - Method: Minimum Maximum Connected Components',
       x = 'K', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 2) K = 20                                                       --------
graph_size <- pa.recovered.1.phi$Graph_Size
proportion.cuts.s <- seq(0.87, .92, by = .01)
colnames(pa.recovered.1.phi) <- c('Graph_Size', paste0('K', K.vector))    # Rename df

pa.recovered.1.phi %>%
  ggplot(aes(x = Graph_Size, y = K20)) +
  geom_point(size = 5, color = '#336699') +
  geom_line(color = '#336699', size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0.87,.92), labels = proportion.cuts.s, breaks = proportion.cuts.s) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered when returning K = 20 vertices',
       title = 'Pref. Attm. Model on Tree - Method: Minimum Maximum Connected Components',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 3) K = 1,2,3,4,5                                                --------
df <- pa.recovered.1.phi %>%
  select(Graph_Size, 
         K1, K2, K3, K4, K5)
proportion.cuts <- seq(0, 1, by = .1)
graph_size <- pa.recovered.1.phi$Graph_Size
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered over K = 1, 2, 3, 4, 5',
       title = 'Pref. Attm. Model on Tree - Method: Minimum Maximum Connected Components',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 4) K = 5, 25, 50, 100                                           --------
df <- pa.recovered.1.phi %>%
  select(Graph_Size, 
         K5, K25, K50, K100)
proportion.cuts <- seq(0.75, 1, by = .05)
graph_size <- pa.recovered.1.phi$Graph_Size
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0.75,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered over K = 5, 25, 50, 100',
       title = 'Pref. Attm. Model on Tree - Method: Minimum Maximum Connected Components',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
rm(df.t, df.melted, 
   proportion.cuts, proportion.cuts.s)








graph_size <- pa.recovered.1.deg$Graph_Size
# ============================================================================ !
# ----- B. Degrees                ----------------------------------------------
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !
# ----- 1) n = 50,000                                                   --------
df.t <- as.data.frame(t(pa.recovered.1.deg))
colnames(df.t) <- paste0('V', seq(1,ncol(df.t),by=1))      # Rename df
proportion.cuts <- seq(0, 1, by = .1)
K.s <- seq(10, 100, by = 10)
df.t %>%
  slice(2:n()) %>%
  ggplot(aes(x = K.vector, y = V10)) +
  geom_point(size = 5, color = '#336699') +
  geom_line(color = '#336699', size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(1,100), labels = K.s, breaks = K.s) +
  scale_y_continuous(limits = c(0, 1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = paste0('Proportion of times vertex 1 was recovered on graph of size n = ', graph_size[10]),
       title = 'Pref. Attm. Model on Tree - Method: Maximum Degree',
       x = 'K', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 2) K = 20                                                       --------
graph_size <- pa.recovered.1.deg$Graph_Size
proportion.cuts.s <- seq(0.5, 1, by = .05)
colnames(pa.recovered.1.deg) <- c('Graph_Size', paste0('K', K.vector))    # Rename df

pa.recovered.1.deg %>%
  ggplot(aes(x = Graph_Size, y = K20)) +
  geom_point(size = 5, color = '#336699') +
  geom_line(color = '#336699', size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0.5,1), labels = proportion.cuts.s, breaks = proportion.cuts.s) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered when returning K = 20 vertices',
       title = 'Pref. Attm. Model on Tree - Method: Maximum Degree',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 3) K = 1,2,3,4,5                                                --------
df <- pa.recovered.1.deg %>%
  select(Graph_Size, 
         K1, K2, K3, K4, K5)
proportion.cuts <- seq(0, 1, by = .1)
graph_size <- pa.recovered.1.deg$Graph_Size
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered over K = 1, 2, 3, 4, 5',
       title = 'Pref. Attm. Model on Tree - Method: Maximum Degree',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 4) K = 5, 25, 50, 100                                           --------
df <- pa.recovered.1.deg %>%
  select(Graph_Size, 
         K5, K25, K50, K100)
proportion.cuts <- seq(0.5, 1, by = .05)
graph_size <- pa.recovered.1.deg$Graph_Size
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0.5,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered over K = 5, 25, 50, 100',
       title = 'Pref. Attm. Model on Tree - Method: Maximum Degree',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
rm(df.t, df.melted, 
   proportion.cuts, proportion.cuts.s)







































K.vector <- c(seq(1, 9, by = 1), seq(10, 100, by = 5))
# ============================================================================ !
# ----- C. Comparison                -------------------------------------------
# -----                                                                 ------ !
# ---------------------------------------------------------------------------- !
# ----- 1) Subgraphs vs. Degrees                                        --------
colnames(pa.recovered.1.deg) <- c('Graph_Size', paste0('K', K.vector))
# Get relevant data frame
df <- pa.recovered.1.deg
df <- rbind(df, pa.recovered.1.phi)
df$Method <- c(rep('Degree', nrow(pa.recovered.1.deg)),
               rep('Subgraphs', nrow(pa.recovered.1.phi)))
colnames(df) <- c('Graph_Size', paste0('K', K.vector), 'Method')    # Rename df

df <- df %>%
  select(Graph_Size, 
         K1, K5, K20, Method)
graph_size <- df$Graph_Size
proportion.cuts <- seq(0, 1, by = .1)
df.melted <- melt(df, id.vars = c('Graph_Size', 'Method'))
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = Method, color = variable)) +
  geom_point(aes(shape = Method), size = 5) +
  geom_line(aes(linetype = Method), size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered over graph sizes and K = 1,5,20',
       title = 'Pref. Attm. Model on Tree - Comparison: 
       Minimum Maximum Connected Components vs. Maximum Degree',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        legend.direction = 'horizontal',
        legend.position = 'bottom',
        legend.box = 'horizontal',
        legend.key.size = unit(5,"line"),
        axis.title = element_text(face = 'bold', size = 16))







load('Pref Attm - Recovery Rate - Vertex 1 - Via Degrees.RData')
load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi values.RData')
K.vector <- c(seq(1, 9, by = 1), seq(10, 100, by = 5))
graph_size <- pa.recovered.1.phi$Graph_Size
# ============================================================================ !
# ----- C. Comparison                -------------------------------------------
# -----                                                                 ------ !

pa.recovered.1.deg
pa.recovered.1.phi

diff.phi.deg <- pa.recovered.1.phi
diff.phi.deg[,c(2:29)] <- diff.phi.deg[,c(2:29)] - pa.recovered.1.deg[,c(2:29)]
colnames(diff.phi.deg) <- c('Graph_Size', paste0('K', K.vector))
diff.phi.deg
# ---------------------------------------------------------------------------- !
# ----- 1) K = 1,2,3,4,5                                                --------
df <- diff.phi.deg %>%
  select(Graph_Size, 
         K1, K2, K3, K4, K5)
proportion.cuts <- seq(0, .2, by = .02)
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0,.2), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times 1 was recovered over K = 1, 2, 3, 4, 5',
       title = 'Pref. Attm. Model on Tree - Comparison: 
       Minimum Maximum Connected Components vs. Maximum Degree',
       x = 'Graph Size', y = 'Difference in Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 2) K = 5, 25, 50, 100                                           --------
df <- diff.phi.deg %>%
  select(Graph_Size, 
         K5, K25, K50, K100)
proportion.cuts <- seq(0, .2, by = .02)
df.melted <- melt(df, id.vars = 'Graph_Size')
df.melted %>%
  ggplot(aes(x = Graph_Size, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(5000,80000), labels = graph_size, breaks = graph_size) +
  scale_y_continuous(limits = c(0,.2), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times 1 was recovered over K = 5, 25, 50, 100',
       title = 'Pref. Attm. Model on Tree - Comparison: 
       Minimum Maximum Connected Components vs. Maximum Degree',
       x = 'Graph Size', y = 'Difference in Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=45),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !


















load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi values.RData')
load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 100.RData')
load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 500.RData')
load('Pref Attm - Recovery Rate - Vertex 1 - Via Phi on Degree - Top 1000.RData')
K.vector <- c(seq(1, 9, by = 1), seq(10, 100, by = 5))
graph_size <- pa.recovered.1.phi$Graph_Size
K.s <- seq(10, 100, by = 10)
# ============================================================================ !
# ----- D. Comparison: Phi on Degree -------------------------------------------
# -----                                                                 ------ !
df <- pa.recovered.1.phi[10,]
df[2,] <- pa.recovered.1.phi.on.deg.10[10,]
df[3,] <- pa.recovered.1.phi.on.deg.5[10,]
df[4,] <- pa.recovered.1.phi.on.deg.1[10,]
df$Method <- c('Phi', 
               'Phi on Degrees: Top 100',
               'Phi on Degrees: Top 500',
               'Phi on Degrees: Top 1000')
colnames(df) <- c('Graph_Size', paste0('K', K.vector), 'Method')    # Rename df
# ---------------------------------------------------------------------------- !
# ----- 1) K = 1,2,3,4,5                                                --------
df.tmp <- df %>%
  select(Method, K1, K2, K3, K4, K5)
proportion.cuts <- seq(0, 1, by = .1)
df.melted <- melt(df.tmp, id.vars = 'Method')
df.melted %>%
  ggplot(aes(x = Method, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(aes(group = variable, color = variable), size = 2) +
  theme_fivethirtyeight() +
  scale_x_discrete(labels = c('M = n', 'M =  1,000', 'M = 500', 'M = 100')) +
  scale_y_continuous(limits = c(0,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times vertex 1 was recovered over K = 1, 2, 3, 4, 5',
       title = 'Pref. Attm. Model on Tree - Method: 
       Minimum Maximum Subgraph among Top M Vertices of Maximum Degrees',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !
# ---------------------------------------------------------------------------- !
# ----- 1) K = 5,25,50,100                                              --------
df.tmp <- df %>%
  select(Method, K5, K25, K50, K100)
proportion.cuts <- seq(0.5, 1, by = .05)
df.melted <- melt(df.tmp, id.vars = 'Method')
df.melted %>%
  ggplot(aes(x = Method, y = value, fill = variable, color = variable)) +
  geom_point(size = 5) +
  geom_line(aes(group = variable, color = variable), size = 2) +
  theme_fivethirtyeight() +
  scale_x_discrete(labels = c('All vertices', 'Top 1,000 vertices',
                              'Top 500 vertices', 'Top 100 vertices')) +
  scale_y_continuous(limits = c(0.5,1), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Proportion of times 1 was recovered over K = 5, 25, 50, 100',
       title = 'Pref. Attm. Model on Tree - Method: 
       Minimum Maximum Subgraph among Top M Vertices of Maximum Degrees',
       x = 'Graph Size', y = 'Recovery Rate') +
  theme(plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        axis.text.x = element_text(size=12, angle=0),
        axis.text.y = element_text(size=12, angle=0),
        legend.background = element_rect(fill = "white"),
        legend.key = element_rect(fill = 'white'),
        legend.title = element_blank(),
        axis.title = element_text(face = 'bold', size = 16))
# ------------------------------------------------------------------ !






