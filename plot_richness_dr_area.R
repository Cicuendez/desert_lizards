# Purpose ----
# Plot biogeographic diversity, speciation rate and geographic area
# This comes from the file: DR_desert_comparison.R

# Directory and libraries ----
setwd("~/Dropbox/HECTOR/Dan")
library(tidyverse)
library(ggchicklet)
library(cowplot)

# Import data ----
desert_areas_richness0 <- readRDS("objects/desert_areas_richness.rds")
desert_areas_richness <- desert_areas_richness0 %>% 
  mutate(area_km = area_m/1e6)
dr_ggplot <- readRDS("objects/dr_rates/dr_ggplot_format.rds")

# Set colors ----
color_richness <- "gray80"
color_area <- "#F08080"

desert_colors <- c("lightblue", "green", "red", "gold", "purple", 
                    "brown", "darkblue", "orange")
names(desert_colors) <- c("American", "Asian", "Atacama", "Australian", 
                           "Gobi", "Kalahari", "Persian", "Saharab")

desert_colors2 <- c("lightblue", color_richness, color_richness, "gold", color_richness, 
                   "brown", color_richness, "orange")
names(desert_colors2) <- c("American", "Asian", "Atacama", "Australian", 
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
plot(1:8, 1:8, cex= 10, pch=16, col = desert_colors2)

desert_colors2 <- c(American = blue, Asian = color_richness, Atacama = color_richness, 
                    Australian = orange2, Gobi = color_richness, Kalahari = brown, 
                    Persian = color_richness, Saharab = orange1)







# Set the theme for ggplot ----
theme.clean <- function(){
  theme_minimal() +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
#          panel.grid.major.x = element_blank(),                                          
#          panel.grid.minor.x = element_blank(),
#          panel.grid.minor.y = element_blank(),
#          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

## Separate plots: desert richness, area and speciation rate ----
## 1. Colplot richness ----
colplot_richness <- ggplot(data = desert_areas_richness, aes(x = desert, y = richness)) +
  geom_col(fill = color_richness, alpha = 0.8) +
#  geom_chicklet(radius = grid::unit(2, 'mm'), fill = color_richness, alpha = 0.8) +
  theme.clean() +
  labs(x = "Desert", y = "Richness") +
  ggsave("plots/colplot_richness.pdf", paper = 'a4r')

## 1.2. Colplot richness highlighting the most diverse deserts ----
desert_areas_richness$desert <- factor(desert_areas_richness$desert)

colplot_richness2 <- desert_areas_richness %>%
  mutate(desert = factor(desert, levels = c("American", "Asian", "Atacama", "Australian", 
                                            "Gobi", "Kalahari", "Persian", "Saharab"))) %>%
  ggplot(data = ., aes(x = desert, y = richness)) +
#  geom_col(aes(fill = desert), alpha = 0.8) +
  geom_chicklet(aes(fill = desert), radius = grid::unit(2, 'mm'), alpha = 0.8) +
  theme.clean() +
  scale_fill_manual(values = desert_colors2, breaks = c("American", "Australian", 
                                                        "Kalahari", "Saharab")) +
  labs(x = "Desert", y = "Richness") +
  theme(legend.position = 'none') +
  ggsave("plots/colplot_richness2.pdf", paper = 'a4') +
  ggsave("plots/colplot_richness2.png", height = 5, width = 5)
saveRDS(colplot_richness2, "objects/colplot_richness2.rds")

## 2. Colplot area ----
colplot_area <- ggplot(data = desert_areas_richness, aes(x = desert, y = area_km)) +
#  geom_chicklet(radius = grid::unit(2, 'mm'), fill = color_area, alpha = 0.8) +
  geom_col(fill = color_area, alpha = 0.8) +
  theme.clean() +
  labs(x = "Desert", y = "Area km") +
  ylab(expression(Area~(km^2))) + 
  ggsave("plots/colplot_area.pdf", paper = "a4r")


## 3. Violin plot and Boxplot logDR ----
?scale_fill_manual

violin_logDR <- dr_ggplot %>%
  mutate(desert = factor(desert, levels = c("American", "Asian", "Atacama", "Australian", 
                                            "Gobi", "Kalahari", "Persian", "Saharab"))) %>% 
  ggplot(aes(x = desert, y = log(dr))) +
  geom_violin(aes(fill = desert), color = 'black', lwd = 0.2, alpha = 0.8) +
  scale_fill_manual(values = desert_colors) +
  theme.clean() +
  theme(legend.position = 'none') +
  labs(x = "Desert", y = "log(DR)") +
  ggsave("plots/boxplotsDR/violin_logDR.pdf", paper="a4r")

boxplot_logDR <- dr_ggplot %>%
  mutate(desert = factor(desert, levels = c("American", "Asian", "Atacama", "Australian", 
                                            "Gobi", "Kalahari", "Persian", "Saharab"))) %>% 
  ggplot(aes(x = desert, y = log(dr))) +
  geom_boxplot(aes(fill = desert), color = "black", alpha = 0.7, lwd = 0.2, fatten = 4) +
  scale_fill_manual(values = desert_colors) +
# geom_jitter(color="black", size=0.1, alpha=0.7) +
  theme.clean() +
  theme(legend.position = 'none') +
  labs(x = "Desert", y = "log(DR)") +
  ggsave("plots/boxplotsDR/boxplot_logDR.pdf", paper="a4r")


# Grid with the tree plots ----
richness_dr_area_plotlist <- list(colplot_richness, boxplot_logDR, colplot_area)

three_plot <- cowplot::plot_grid(plotlist = richness_dr_area_plotlist, ncol = 1, labels = 'auto')
ggsave("plots/richness_dr_area.pdf", three_plot, paper='a4', height = 10, width = 5)
ggsave("plots/richness_dr_area.png", three_plot, height = 10, width = 5)

three_plot_horizontal <- cowplot::plot_grid(plotlist = richness_dr_area_plotlist, ncol=3, labels = 'auto')
ggsave("plots/richness_dr_area_horizontal.pdf", three_plot_horizontal, paper='a4r', height = 5, width = 10)
ggsave("plots/richness_dr_area_horizontal.png", three_plot_horizontal, height = 5, width = 10)


three_plot_horizontal_round <- cowplot::plot_grid(plotlist = richness_dr_area_plotlist, ncol=3, labels = 'auto')
ggsave("plots/richness_dr_area_horizontal_round.pdf", three_plot_horizontal_round, paper='a4r', height = 5, width = 10)
ggsave("plots/richness_dr_area_horizontal_round.png", three_plot_horizontal_round, height = 5, width = 10)

#####
#####
#####


# Scatterplot area-richness ----
area_richness <- ggplot(data = desert_areas_richness, aes(x = log(area_km), y = richness)) +
  geom_point(aes(color = desert), alpha = 1, size = 5) +
  scale_color_manual(values = desert_colors) +
  theme.clean() +
  labs(x = "log(area)", y = "richness") +
  ggsave("plots/scatterplot_area_richness.pdf", paper = "a4")



  


