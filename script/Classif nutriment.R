# chargement du jeu de donnee
nutriment <- don[,which(colnames(don)=="Energy_kcal"):which(colnames(don)=="Moisture_gm")]

# ACP
acp <- PCA(nutriment,scale.unit=T,graph = F)
var <- get_pca_var(acp)
ind <- get_pca_ind(acp)

# Kmeans
partition <- 1:15
for (i in 1:15){
  kmk <- kmeans(var$coord,centers = i, nstart = 10)
  partition[i]= kmk$tot.withinss/kmk$totss*100
}

partitionInd <- 1:15
for (i in 1:15){
  kmk <- kmeans(ind$coord,centers = i, nstart = 10)
  partitionInd[i]= kmk$tot.withinss/kmk$totss*100
}

km <- kmeans(var$coord, center=4, nstart = 25)
grp <- as.factor(km$cluster)

kmInd <- kmeans(ind$coord, center=4, nstart = 25)
grpInd <- as.factor(kmInd$cluster)


fviz_pca_ind(
  acp,
  col.ind = grpInd,
  palette = c("black", "Blue", "red", "orange"),
  repel = TRUE
)

unique(grpInd)
