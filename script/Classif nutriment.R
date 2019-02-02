# chargement du jeu de donnee
nutriment <- don[,which(colnames(don)=="Energy_kcal"):which(colnames(don)=="Moisture_gm")]

# ACP
acp <- PCA(nutriment,scale.unit=T,graph = F)
var <- get_pca_var(acp)

# Kmeans
partition <- 1:15
for (i in 1:15){
  kmk <- kmeans(var$coord,centers = i, nstart = 10)
  partition[i]= kmk$tot.withinss/kmk$totss*100
}

km <- kmeans(var$coord, center=4, nstart = 25)
grp <- as.factor(km$cluster)
