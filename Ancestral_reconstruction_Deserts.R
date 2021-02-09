##### ANCESTRAL RECONSTRUCTION OF PRESENCE IN THE DESERT #####
setwd("~/Dropbox/HECTOR/Dan/")

libs <- c("treeio", "phytools", "geiger", "tidytree", "dplyr", "tidyverse", "doParallel",
          "RColorBrewer", "ggtree", "scico")
lapply(libs, require, character.only = TRUE)

# Import phylogeny
tonini_dna <- read.newick("phylogeny/tonini_dna.tre")
tonini_dna_treedata <- as.treedata(tonini_dna)

# Import desert categorization
categorization_reptiles_deserts_15_15 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_15_15.rds")
x <- categorization_reptiles_deserts_15_15



species_dna <- data.frame(sp_tree=tonini_dna_treedata@phylo$tip.label)
nrow(species_dna)
nrow(categorization_reptiles_deserts_15_15)

deserts_genet <- merge(species_dna, categorization_reptiles_deserts_15_15)
rownames(deserts_genet) <- deserts_genet$sp_tree
name.check(tonini_dna, deserts_genet)

x$sp_tree[which(x$sp_tree=="Acanthocercus_annectans")] <- "Acanthocercus_annectens"
x["Anolis_rodriguezii",]$sp_tree <- "Anolis_rodriguezi"
x$sp_tree[which(x$sp_tree=="Psammophis_phillipsii")] <- "Psammophis_phillipsi"
x$sp_tree[which(x$sp_tree=="Malpolon_moilensis")] <- "Rhagerhis_moilensis"
x$sp_tree[which(x$sp_tree=="Tropiocolotes_scorteccii")] <- "Tropiocolotes_scortecci"

deserts_genet <- merge(species_dna, x)
rownames(deserts_genet) <- deserts_genet$sp_tree
name.check(tonini_dna, deserts_genet)

deserts_genet$desert <- "Out"
for (i in 1:nrow(deserts_genet)){
  if (sum(deserts_genet[i,8:15])==1){
    deserts_genet$desert[i] <- colnames(deserts_genet[i,8:15][which(deserts_genet[i,8:15]==1)])
  }
  if (sum(deserts_genet[i,8:15])>1){
    deserts_genet$desert[i] <- paste0(colnames(deserts_genet[i,8:15][which(deserts_genet[i,8:15]==1)]), collapse="_")
  }
}
levels(as.factor(deserts_genet$desert))

ncheck <- name.check(tonini_dna, deserts_genet)
tonini_dna_desert <- treeio::drop.tip(tonini_dna_treedata, ncheck$tree_not_data)

#dir.create("objects/Ancestral_reconst_deserts/")
saveRDS(tonini_dna_desert, "objects/Ancestral_reconst_deserts/tonini_dna_desert.rds")
saveRDS(deserts_genet, "objects/Ancestral_reconst_deserts/deserts_genet.rds")

##### Make simmap #####
tonini_dna_desert <- readRDS("objects/Ancestral_reconst_deserts/tonini_dna_desert.rds")
deserts_genet <- readRDS("objects/Ancestral_reconst_deserts/deserts_genet.rds")
desert_data <- deserts_genet$desert
names(desert_data) <- rownames(deserts_genet)
registerDoParallel(cores=4)
desert_simmap100 <- make.simmap(tonini_dna_desert@phylo, desert_data, model="ER", nsim=100)
desert_simmap <- describe.simmap(desert_simmap100)
saveRDS(desert_simmap, "objects/Ancestral_reconst_deserts/desert_simmap.rds")

desert_simmap <- readRDS("objects/Ancestral_reconst_deserts/desert_simmap.rds")

levels(as.factor(deserts_genet$desert))

desert_colors <- c("lightblue", "green", "darkgreen", "blue", "lightgreen", "yellow", 
                   "red", "gold", "purple", "brown", "grey", 
                   "navy", "violet", "orange")
names(desert_colors) <- levels(as.factor(deserts_genet$desert))

nodes_simmap <- as.data.frame(desert_simmap$ace)
node_desert <- as.numeric(rownames(nodes_simmap[nodes_simmap$Out<.80,]))
nodes_simmap_matrix <- as.matrix(nodes_simmap)
tips_simmap <- as.data.frame(desert_simmap$tips)
tip_desert <- as.numeric(which(tips_simmap$Out!=1))
tips_simmap_matrix <- as.matrix(tips_simmap)
tonini_dna_desert@phylo$tip.label

#dir.create("plots/Ancestral_reconstruction_deserts")
# Plot all
pdf("plots/Ancestral_reconstruction_deserts/reconstruction.pdf")
plotTree(tonini_dna_desert@phylo, fsize=0.01, lwd=0.1, type="fan", color="gray")
tiplabels(pie=desert_simmap$tips, piecol=desert_colors, cex=0.1, lty=par(lty="blank"))
nodelabels(pie=desert_simmap$ace, piecol=desert_colors, cex=0.1, lty=par(lty="blank"))
add.simmap.legend(colors=desert_colors, fsize=0.5, prompt = F, x=20, y=-25)
dev.off()

# Plot all big
pdf("plots/Ancestral_reconstruction_deserts/reconstruction_big.pdf", height=30, width=30)
plotTree(tonini_dna_desert@phylo, fsize=0.1, lwd=0.1, type="fan", color="gray")
tiplabels(pie=desert_simmap$tips, piecol=desert_colors, cex=0.01, lty=par(lty="blank"))
nodelabels(pie=desert_simmap$ace, piecol=desert_colors, cex=0.01, lty=par(lty="blank"))
add.simmap.legend(colors=desert_colors, fsize=1.8, prompt = F, x=30, y=104)
dev.off()

# Plot only desert
pdf("plots/Ancestral_reconstruction_deserts/desert_reconstruction.pdf", height=20, width=20)
plotTree(tonini_dna_desert@phylo, fsize=0.1, ftype='i', lwd=0.1, type="fan", color="gray", offset=20)
tiplabels(tip=tip_desert, pie=tips_simmap_matrix[tip_desert,], piecol=desert_colors, cex=0.1, lty=par(lty="blank"))
nodelabels(node=node_desert, pie=nodes_simmap_matrix[as.character(node_desert),], piecol=desert_colors, cex=0.1, lty=par(lty="blank"))
add.simmap.legend(colors=desert_colors, fsize=1.5, prompt = F, x=40, y=120)
axisPhylo()
dev.off()

# Plot only desert big (small piecharts)
pdf("plots/Ancestral_reconstruction_deserts/desert_reconstruction_big.pdf", height=30, width=30)
plotTree(tonini_dna_desert@phylo, fsize=0.1, ftype='i', lwd=0.1, type="fan", color="gray", offset=20)
tiplabels(tip=tip_desert, pie=tips_simmap_matrix[tip_desert,], piecol=desert_colors, cex=0.01, lty=par(lty="blank"))
nodelabels(node=node_desert, pie=nodes_simmap_matrix[as.character(node_desert),], piecol=desert_colors, cex=0.01, lty=par(lty="blank"))
add.simmap.legend(colors=desert_colors, fsize=1.8, prompt = F, x=30, y=104)
axisPhylo()
dev.off()

# Divergence times
node.depth.edgelength(tonini_dna_desert@phylo)
divtimes <- node.depth.edgelength(tonini_dna_desert@phylo)[1] - node.depth.edgelength(tonini_dna_desert@phylo)
range(divtimes)
divtimes[7150]
divtimes[as.numeric(rownames(nodes_simmap))]
node_prob_age <- cbind(age=divtimes[as.numeric(rownames(nodes_simmap))], nodes_simmap)
saveRDS(node_prob_age, "objects/Ancestral_reconst_deserts/node_prob_age.rds")


##################################
##################################
##################################


