setwd("~/Dropbox/HECTOR/Dan/")

categorization_reptiles_deserts_10_10 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_10_10.rds")
categorization_reptiles_deserts_10_15 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_10_15.rds")
categorization_reptiles_deserts_10_20 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_10_20.rds")
categorization_reptiles_deserts_15_10 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_15_10.rds")
categorization_reptiles_deserts_15_15 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_15_15.rds")
categorization_reptiles_deserts_15_20 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_15_20.rds")
categorization_reptiles_deserts_20_10 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_20_10.rds")
categorization_reptiles_deserts_20_15 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_20_15.rds")
categorization_reptiles_deserts_20_20 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_20_20.rds")
categorization_reptiles_deserts_30_30 <- readRDS("objects/desert_categorization/categorization_reptiles_deserts_30_30.rds")

categorization_list_objects <- list(categorization_reptiles_deserts_10_10, categorization_reptiles_deserts_10_15,
                                    categorization_reptiles_deserts_10_20, categorization_reptiles_deserts_15_10,
                                    categorization_reptiles_deserts_15_15, categorization_reptiles_deserts_15_20,
                                    categorization_reptiles_deserts_20_10, categorization_reptiles_deserts_20_15,
                                    categorization_reptiles_deserts_20_20, categorization_reptiles_deserts_30_30)

desert_numbers_list <- vector("list", length=length(categorization_list_objects))
names(desert_numbers_list) <- c("10_10", "10_15", "10_20", "15_10", "15_15", "15_20", "20_10", "20_15", "20_20", "30_30")

for (i in 1:length(categorization_list_objects)){
  desert_numbers_list[[i]] <- colSums(categorization_list_objects[[i]][,8:ncol(categorization_list_objects[[i]])])
}

#dir.create("plots/barplots_sensitiviy")
pdf("plots/barplots_sensitiviy/sensitivity_species_count.pdf", paper="a4", height=20, width=20)
par(mfrow=c(5,2), mar=c(4,4,4,4))
for (i in 1:length(desert_numbers_list)){
  barplot(desert_numbers_list[[i]], las=2, main=names(desert_numbers_list)[i], col="gray", border=NA)
}
dev.off()

