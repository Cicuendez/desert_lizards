
##### CATEGORIZATION OF REPTILES IN THE DESERTS #####

# Every species with at least *dist_percent* percent of its distribution within a desert OR 
# present in at least *desert_percent* percent of the area of a desert 
# will be assigned to that desert. 
dist_percent <- 15
desert_percent <- 15

# Import the master table
reptiles_deserts_raw <- readRDS("objects/reptiles_deserts_raw.rds")

# Set up the table for categorization
reptiles_deserts <- reptiles_deserts_raw[,1:7]
reptiles_deserts <- cbind(reptiles_deserts, Atacama=0, Australian=0, Asian=0, Gobi=0, Kalahari=0,
                          American=0, Persian=0, Saharab=0)


for (i in 1:nrow(reptiles_deserts_raw)){
  if (reptiles_deserts_raw$percent_in_atacama[i] >= dist_percent | reptiles_deserts_raw$percent_of_atacama[i] >= desert_percent){
    reptiles_deserts$Atacama[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_australian[i] >= dist_percent | reptiles_deserts_raw$percent_of_australian[i] >= desert_percent){
    reptiles_deserts$Australian[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_asian[i] >= dist_percent | reptiles_deserts_raw$percent_of_asian[i] >= desert_percent){
    reptiles_deserts$Asian[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_gobi[i] >= dist_percent | reptiles_deserts_raw$percent_of_gobi[i] >= desert_percent){
    reptiles_deserts$Gobi[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_kalahari[i] >= dist_percent | reptiles_deserts_raw$percent_of_kalahari[i] >= desert_percent){
    reptiles_deserts$Kalahari[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_american[i] >= dist_percent | reptiles_deserts_raw$percent_of_american[i] >= desert_percent){
    reptiles_deserts$American[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_persian[i] >= dist_percent | reptiles_deserts_raw$percent_of_persian[i] >= desert_percent){
    reptiles_deserts$Persian[i] <- 1
  }
  if (reptiles_deserts_raw$percent_in_saharab[i] >= dist_percent | reptiles_deserts_raw$percent_of_saharab[i] >= desert_percent){
    reptiles_deserts$Saharab[i] <- 1
  }
}

saveRDS(reptiles_deserts, "objects/categorization_reptiles_deserts.rds")
