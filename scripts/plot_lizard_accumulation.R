# Purpose ----
# To plot lineage accumulation in the deserts only for lizards 
# (squamates excluding snakes)

# Working directory and packages ----
setwd("~/Dropbox/HECTOR/Dan/")

libs <- c("treeio", "phytools", "geiger", "tidytree", "dplyr", "tidyverse", "doParallel",
          "RColorBrewer", "ggtree", "scico", "ggchicklet")
lapply(libs, require, character.only = TRUE)

# Import tree and species dataframe ----
tonini_dna_desert <- readRDS("objects/Ancestral_reconst_deserts/tonini_dna_desert.rds")
deserts_genet <- readRDS("objects/Ancestral_reconst_deserts/deserts_genet.rds")
names(deserts_genet)
levels(deserts_genet$group)

# Import node probability and age table ----
node_prob_age <- readRDS("objects/Ancestral_reconst_deserts/node_prob_age.rds")

# Get the MRCA node number of the snakes ----
snake_df <- deserts_genet %>%
  filter(group == "snake")
snake_species <- as.character(snake_df$sp_tree)
length(snake_species)

snake_node <- getMRCA(tonini_dna_desert@phylo, snake_species)

# Plot tree with node numbers ----
branch_color <- "gray40"
branch_size <- 0.3

t <- ggtree(tonini_dna_desert, layout="fan", color=branch_color, size=branch_size) 
plot_tree_nodes <- t + 
  geom_nodepoint(color="lightblue", size=0.5) +
  geom_text2(aes(label=node), size=0.5) +
  geom_tiplab2(size=0.3, offset=2.5)

pdf("plots/tree_node_numbers.pdf", width=20, height=20)
plot_tree_nodes
dev.off()



# Get all the node numbers in the snakes clade ----
snake_clade_nodes <- getDescendants(tonini_dna_desert@phylo, snake_node)

# Filter out snake nodes in the node probability and age table ----
node_prob_age0 <- node_prob_age
node_prob_age <- node_prob_age0 %>% 
  mutate(node_number = as.numeric(rownames(node_prob_age)))

node_prob_age_lizards <- node_prob_age %>%
  filter(!node_number %in% snake_clade_nodes)

saveRDS(node_prob_age_lizards, "objects/Ancestral_reconst_deserts/node_prob_age_lizards.rds")


# PLOT LIZARD LINEAGE ACCUMULATION ----
z <- readRDS("objects/Ancestral_reconst_deserts/node_prob_age_lizards.rds")
time_vec <- seq(0.001, 100, length.out=200) # Time axis (Millions of years)

rmat <- matrix(NA, ncol = 8, nrow=length(time_vec))
colnames(rmat) <- c("American", "Atacama", "Asian", "Gobi", "Persian", "Saharab", "Kalahari", "Australian")

xx <- z
#xx$American <- xx$American + xx$Atacama_American / 2
#xx$Atacama <- xx$Atacama + xx$Atacama_American / 2
xx$Asian   <- xx$Asian + xx$Asian_Gobi / 2 + xx$Asian_Gobi_Persian / 3 + xx$Asian_Persian / 2 + xx$Asian_Persian_Saharab / 3
xx$Gobi    <- xx$Gobi + xx$Asian_Gobi / 2 + xx$Asian_Gobi_Persian / 3
xx$Persian <- xx$Persian + xx$Asian_Gobi_Persian / 3 + xx$Asian_Persian / 2 + xx$Asian_Persian_Saharab / 3 +  xx$Persian_Saharab / 2
xx$Saharab <- xx$Saharab + xx$Asian_Persian_Saharab / 3 + xx$Persian_Saharab / 2


# Estimate lineage totals that were present in a region by a particular time 

for (ii in 1:length(time_vec)){
  
  tmp <- xx[xx$age >= time_vec[ii], ]
  rmat[ii, "American"] <- sum(tmp$American)
  rmat[ii, "Atacama"]  <- sum(tmp$Atacama)
  rmat[ii, "Asian"]    <- sum(tmp$Asian)
  rmat[ii, "Gobi"]     <- sum(tmp$Gobi)
  rmat[ii, "Persian"]  <- sum(tmp$Persian)
  rmat[ii, "Saharab"]  <- sum(tmp$Saharab)
  rmat[ii, "Kalahari"] <- sum(tmp$Kalahari)
  rmat[ii, "Australian"] <- sum(tmp$Australian)
  
}
# rmat is the cumulative node probability over time for each desert.

quartz.options(height=5, width= 6)
par(mar=c(5,5,1,1), oma=c(1,1,1,1))

#dir.create("objects/lineages_time")
pdf("objects/lineages_time/lineages_time_lizards.pdf")
plot.new()
plot.window(xlim=c(60,0), ylim=c(0,250))
lines(x = time_vec, y = rmat[ , "Australian"], lwd=4, col = "gold")
lines(x = time_vec, y = rmat[ , "Saharab"], lwd=4, col = "orange")
lines(x = time_vec, y = rmat[ , "Kalahari"], lwd=4, col = "brown")
lines(x = time_vec, y = rmat[ , "American"], lwd=4, col = "lightblue")
#lines(x = time_vec, y = rmat[ , "Atacama"], lwd=4, col = "red")
#lines(x = time_vec, y = rmat[ , "Asian"], lwd=4, col = "green")
#lines(x = time_vec, y = rmat[ , "Gobi"], lwd=4, col = "purple")
#lines(x = time_vec, y = rmat[ , "Persian"], lwd=4, col = "darkblue")


segments(x0=100, x1=95, y0=c(110,120,130,140), y1=c(110, 120, 130, 140), col=c("gold", "orange", "brown", "lightblue"), lwd=4)
text(x=95, y=c(110,120,130,140), labels = c("Australia", "Saharab", "Kalahari", "American"), cex=1, pos = 4)

axis(1)
axis(2)
mtext("Time before present (Ma)", side= 1, line = 2.5, cex=1)
mtext("Cumulative node probability in system", side= 2, line = 2.5, cex=1)
dev.off()

######
######

desert_colors <- c("lightblue", "green", "red", "gold", "purple", 
                   "brown", "darkblue", "orange")
names(desert_colors) <- c("American", "Asian", "Atacama", "Australian", 
                          "Gobi", "Kalahari", "Persian", "Saharab")

# Plot cumulative probability in each desert with ggplot2 ----

# Set the theme 
theme.clean <- function(){
  theme_minimal() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
#         panel.grid.major.x = element_blank(),                                          
#         panel.grid.minor.x = element_blank(),
#         panel.grid.minor.y = element_blank(),
#         panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

# Set the color for the deserts
desert_colors <- c("lightblue", "green", "red", "gold", "purple", 
                   "brown", "darkblue", "orange")
names(desert_colors) <- c("American", "Asian", "Atacama", "Australian", 
                          "Gobi", "Kalahari", "Persian", "Saharab")

# Alternative colors
green <- '#6A8C1F'
yellow <- '#F2DD72'
orange1 <- '#F29E38'
orange2 <- '#F25116'
brown <- '#400C07'
blue <- '#03A688'
red <- '#A62103'
purple <- '#b65cff'
blue2 <- '#7a97ff'
desert_colors <- c(green, yellow, orange1, orange2, brown, blue, red, blue2)
desert_names <- c("Persian", "Gobi", "Saharab", "Australian", "Kalahari", "American", 
                  "Atacama", "Asian")
names(desert_colors) <- desert_names
plot(1:8, 1:8, cex= 10, pch=16, col = desert_colors)

# Create the table of time and cumulative probabilities for each desert
cum_prob_time <- cbind(time = time_vec, as.data.frame(rmat))
cum_prob_time_long <- cum_prob_time %>%
  gather(key = desert, value = prob, American:Australian, factor_key = TRUE)

# Alternative to gather: pivot_longer (new function)
cum_prob_time_long2 <- cum_prob_time %>%
  pivot_longer(cols = c(American:Australian), names_to = 'desert', values_to = 'prob', 
               names_transform = list(desert = as.factor))

cum_prob_plot <- cum_prob_time_long %>%
  filter(desert %in% c("American", "Australian", "Kalahari", "Saharab")) %>%
  ggplot(data = .) +
  geom_line(aes(x = time, y = prob, color = desert), lwd = 1) +
  scale_color_manual(values = desert_colors) +
  labs(x = "Time before present (Ma)", y = "Lineage richness") +
  xlim(80, 0) +
  theme.clean() +
  ggsave("plots/cumulative_node_probability.pdf")

# Combine the cumulative probability plot with the richness colplot: ----
colplot_richness2 <- readRDS("objects/colplot_richness2.rds")

colplot_richness2_2 <- colplot_richness2 + 
  ggtitle("Biogeographic richness") +
  theme(
  panel.grid.major.x = element_blank(),                                          
  panel.grid.minor.x = element_blank(),
  panel.grid.minor.y = element_blank(),
  panel.grid.major.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_text(size = 5),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  plot.title = element_text(size = 10, vjust = 1, hjust = 0.5)
  )

cum_prob_plot + 
  annotation_custom(ggplotGrob(colplot_richness2_2), xmin = -60, xmax = -20,
                    ymin = 90, ymax= 210) +
  ggsave("plots/cumulative_probability_and_richness.pdf", width = 7, height = 5) +
  ggsave("plots/cumulative_probability_and_richness.png", width = 7, height = 5)




