# ============================================================================ !
# ============================================================================ !
# ============================================================================ !
# =====                                                                 ====== !
# ===== Objective: ---------------------------------------------------- ====== !
# ===== ---------- Convergence Analysis on Recovery Rate                ====== !
# ===== ---------- Recover L vertices:                                  ====== !
# ===== ------------------ Local search: Constant K                     ====== !
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


# ----- K: diff. #(vertices) ------------------------------------------------- !
# -----                                                                 ------ !
K.vector <- seq(2,50,2)
L.steps <- c(5,10)
print(paste0('Number of vertices returned per step: ', paste0(K.vector, collapse = ',')))
print(paste0('Number of vertices to be recovered: ', paste0(L.steps, collapse = ',')))
# ---------------------------------------------------------------------------- !



# ----- helper fn to correct K ----------------------------------------------- !
# -----                                                                 ------ !
replace.K <- function(x) {
  return(as.numeric(gsub('K', '', x)))
} # -----                                                               ------ !
# ---------------------------------------------------------------------------- !


# ============================================================================ !
# ---------------------------------------------------------------------------- !
# ----- Comparison: Local Search vs. Naive                              --------
# ---------------------------------------------------------------------------- !
# ============================================================================ !


# ---------------------------------------------------------------------------- !
# ----- 1) n = 50,000                                                   --------
# Get data
df <- pa.recovered.L.local.search[c(10,25),]
tmp <- pa.recovered.L.phi.naive %>%
  filter(Graph_Size == 50000, Vertex.to.Recover %in% c(5,10))
df[3,] <- c(50000, 5, tmp[1,seq(2,11,by=1)])
df[4,] <- c(50000, 10, tmp[2,seq(3,21,by=2)])
df$Method <- rep(c('Local_Search', 'Simple_Search'), each = 2)

# Prepare data
rownames(df) <- seq(1,nrow(df))
colnames(df) <- c('Graph_Size', 'L', paste0('K', K.vector), 'Method')
df <- df[,-1]
# Melt data for plotting
df.melted <- melt(df, id.vars = c('L', 'Method'))
df.melted$variable <- sapply(df.melted$variable, replace.K) * df$L
point.labels <- df.melted$variable
df.melted$L <- factor(df.melted$L, levels = c(5,10))
df.melted$Method <- factor(df.melted$Method, levels = c('Local_Search', 'Simple_Search'))
df.melted$Labels <- point.labels
# Plot
K.s <- seq(5, 100, by=5)
proportion.cuts <- seq(0, 1, by = .1)
df.melted %>%
  ggplot(aes(x = variable, y = value, color = Method)) +
  geom_point(size = 5) +
  geom_line(aes(linetype = L), size = 2) +
  theme_fivethirtyeight() +
  scale_x_continuous(limits = c(0, 100), labels = K.s, breaks = K.s) +
  scale_y_continuous(limits = c(0,.6), labels = proportion.cuts, breaks = proportion.cuts) +
  labs(subtitle = 'Graph size: 50,000; Over different K vertices returned in total;',
       title = 'Pref. Attm. Model on Tree:
                Recovery of The First 5 and 10 Vertices
                Comparison: Local Search vs. Simple Search',
       x = 'K (total)', y = 'Recovery Rate',
       color = 'Method',
       linetype = 'Vertices to recover') +
  scale_color_hue(labels = c('Local Search', 'Simple Search')) +
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
