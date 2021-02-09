## Intro ----
# Compare average DR rates (for LIZARDS) among deserts.
# Barplot species richness. 
# Relationship between richness, DR rates and desert area.
# Date: 2020-04-14

setwd("~/Dropbox/HECTOR/Dan")
library(tidyverse)

## Import mean DR values ----
# dr_mean is a dataframe with the average DR rates for all species in Tonini tree.
# Therefore, it has the species names of the tree: sp_tree
dr_mean <- readRDS("objects/dr_rates/dr_mean.rds")
head(dr_mean)

# Create a dataframe with species name in one column and mean DR in other
dr_mean_df <- data.frame(sp_tree=rownames(dr_mean), dr=dr_mean$dr)
head(dr_mean_df)

## Import reptile species in the deserts ----
reptiles_deserts <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_15_15.rds")
head(reptiles_deserts)

## Merge the dataframes ----
dr_deserts <- merge(dr_mean_df, reptiles_deserts)
head(dr_deserts)

?merge
?join

dr_deserts2 <- inner_join(dr_mean_df, reptiles_deserts, by="sp_tree")

## Extract lizards ----
lizards_dr_deserts <- dr_deserts %>%
  filter(group == "lizard" | group == "worm lizard")

head(lizards_dr_deserts)

## Create list, each element is a vector of DR values for each desert ----
# This way, it will be easier to make the boxplot of DR per desert
desert_names <- colnames(lizards_dr_deserts[, 9:16])
dr_list <- vector("list", length=length(desert_names))
names(dr_list) <- desert_names


for (i in desert_names){
  dr_list[[i]] <- lizards_dr_deserts$dr[lizards_dr_deserts[, i] == 1]
}

# For ggplot, create a list of dataframes
dr_list_ggplot <- vector("list", length=length(desert_names))
names(dr_list_ggplot) <- desert_names
for (i in desert_names){
  dr_list_ggplot[[i]] <- data.frame(desert=factor(i, levels = desert_names), 
                                    dr=lizards_dr_deserts$dr[lizards_dr_deserts[, i] == 1])
}

# Stack the dataframes
dr_ggplot <- do.call(rbind, dr_list_ggplot)
saveRDS(dr_ggplot, "objects/dr_rates/dr_ggplot_format.rds")

## Boxplot ----
desert_colors <- c("red", "gold", "green", "purple", "brown", "lightblue", "darkblue", "orange")
desert_names <- c("Atacama", "Australian", "Central Asian", "Gobi", "Kalahari", "North American",
                  "Persian", "Saharoarab")
names(desert_colors) <- desert_names

boxplot(dr_list, las=2, col=desert_colors)

# With ggplot
theme.clean <- function(){
  theme_bw()+
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),
          axis.text.y = element_text(size = 12),
          axis.title.x = element_text(size = 14, face = "plain"),             
          axis.title.y = element_text(size = 14, face = "plain"),             
          panel.grid.major.x = element_blank(),                                          
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),  
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
          plot.title = element_text(size = 20, vjust = 1, hjust = 0.5),
          legend.text = element_text(size = 12, face = "italic"),          
          legend.position = "right")
}

ggplot(data=dr_ggplot, aes(x = desert, y = log(dr))) +
#  geom_boxplot(color=desert_colors) +
#  geom_boxplot(fill = "gray80", alpha = 0.8, colour = "gray70") +
  geom_boxplot(fill = desert_colors, color = "black", alpha = 0.5) +
#  theme_classic() +
  theme.clean() +
  labs(x = "Desert", y = "log(DR)")
#  xlab("Desert") +
#  ylab("log(DR)")


## Density plot ----
ggplot(data=dr_ggplot, aes(x = log(dr), color=desert)) +
  geom_density() +
  theme_classic() +
  xlab("log(DR)")

## Barplot species richness ----
barplot_richness <- ggplot(data = dr_ggplot, aes(x = desert)) +
  geom_bar(fill = "gray80") +
#  geom_bar(fill = desert_colors, alpha = 0.8) +
  theme.clean() + 
  labs(x = "Desert", y = "Richness")


desert_richness <- colSums(lizards_dr_deserts[, 9:16])
barplot(desert_richness)

## Species richness and speciation rate (barplot + boxplot)
ggplot(data = dr_ggplot) +
  geom_bar(aes(x = desert), fill = "gray92", alpha = 0.8) + 
  geom_boxplot(aes(x = desert, y = log(dr)*100), color = desert_colors, fill = "transparent") +
  theme.clean() +
  labs(x = "Desert", y = "", 
       title = "Desert species richness and speciation rates")

?ggtitle
?geom_bar
log(dr_ggplot$dr)*100

## Colplot desert area ----
desert_areas <- readRDS("objects/desert_areas.rds")
ggplot(data=desert_areas, aes(x = desert, y = area_m/10000000000)) +
  geom_col(fill = "gray92") +
  theme.clean() +
  labs(x = "Desert", y = "area", 
       title = "Desert area")

#log(desert_areas$area_m)
desert_areas$desert <- levels(dr_ggplot$desert)
desert_richness
desert_areas_richness <- desert_areas
desert_areas_richness$richness <- desert_richness

saveRDS(desert_areas_richness, "objects/desert_areas_richness.rds")

## Colplot desert area + boxplot speciation rate ----
desert_colors
desert_colors2 <- c("lightblue", "green", "red", "gold", "purple", 
                    "brown", "darkblue", "orange")
names(desert_colors2) <- c("American", "Asian", "Atacama", "Australian", 
                           "Gobi", "Kalahari", "Persian", "Saharab")
desert_areas_richness$desert

## Three way plot: desert richness, area and speciation rate ----
threeway_plot <- ggplot() +
  geom_col(data = desert_areas_richness, aes(x = desert, y = richness), fill = "gray92") +
  geom_col(data = desert_areas_richness, aes(x = desert, y = -area_m/50000000000), fill = "#F08080", alpha = 0.3) +
  geom_boxplot(data=dr_ggplot, aes(x = desert, y = log(dr)*100+220), 
               color = "black", fill = desert_colors2, alpha=0.5) +
  theme.clean() +
  labs(x = "Desert", y = "Area / Richness", 
       title = "Speciation rate / Area / Richness") +
  theme(axis.text.y = element_blank())

pdf("temp/00threeway_plot.pdf")
threeway_plot
dev.off()

## Separate plots: desert richness, area and speciation rate ----
## 1. Colplot richness ----
colplot_richness <- ggplot(data = desert_areas_richness, aes(x = desert, y = richness)) +
  geom_col(fill = "gray90") +
  theme.clean() +
  labs(x = "Desert", y = "Richness")

pdf("plots/colplot_richness.pdf", paper = "a4r")
colplot_richness
dev.off()

## 2. Colplot area ----
colplot_area <- ggplot(data = desert_areas_richness, aes(x = desert, y = -area_m/1000000)) +
  geom_col(fill = "#F08080", alpha = 0.7) +
  theme.clean() +
  labs(x = "Desert", y = "Area")

pdf("plots/colplot_area.pdf", paper = "a4r")
colplot_area
dev.off()
  
## 3. Boxplot logDR ----
boxplot_logDR <- ggplot(data=dr_ggplot, aes(x = desert, y = log(dr))) +
  geom_boxplot(fill = desert_colors, color = "black", alpha = 0.5) +
  theme.clean() +
  labs(x = "Desert", y = "log(DR)")
#  xlab("Desert") +
#  ylab("log(DR)")

pdf("plots/boxplotsDR/boxplot_logDR.pdf", paper="a4r")
boxplot_logDR
dev.off()






## STATISTICAL ANALYSES ----
# Linear model (ANOVA) ----
dr.desert.lm <- lm(dr~desert, data=dr_ggplot)
summary(dr.desert.lm)
anova(dr.desert.lm)

# Checking that the residuals are normally distributed
dr.desert.resid <- resid(dr.desert.lm)              # Extracting the residuals
shapiro.test(dr.desert.resid)                   # Using the Shapiro-Wilk test
# The null hypothesis of normal distribution is rejected: there is significant difference (p < 0.05) from a normal distribution

# Checking for homoscedasticity
bartlett.test(dr~desert, data=dr_ggplot) 
# The null hypothesis of homoscedasticity is rejected

plot(dr.desert.lm)

# Non-parametric alternative: Kruskal-Wallis test ----
# Since the assumptions of ANOVA are not met, we compute the non-parametric alternative.
kruskal.test(dr ~ desert, data = dr_ggplot)

## Posthoc tests: Wilcoxon test ----
pairwise_dif <- pairwise.wilcox.test(dr_ggplot$dr, dr_ggplot$desert,
                     p.adjust.method = "bonferroni")
saveRDS(pairwise_dif, "objects/dr_rates/posthoc_tests_DR.rds")
class(pairwise_dif$p.value)
write.table(pairwise_dif$p.value, "tables/Table_S1_posthoc_DR.csv", quote = FALSE, 
            sep = ";", dec = ".", row.names = TRUE, col.names = NA)
as.data.frame(pairwise_dif$p.value)
as.data.frame(pairwise_dif)
?pairwise.wilcox.test








