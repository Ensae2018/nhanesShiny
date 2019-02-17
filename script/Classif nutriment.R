# chargement du jeu de donnee
nutriment <- donHyp[,which(colnames(donHyp)=="Energy_kcal"):which(colnames(donHyp)=="Moisture_gm")]

# ACP
acp <- PCA(nutriment,scale.unit=T,graph = F)
var <- get_pca_var(acp)

# Kmeans
partition <- 1:15
for (i in 1:15){
  kmk <- kmeans(var$coord,centers = i, nstart = 10)
  partition[i]= kmk$tot.withinss/kmk$totss*100
}

plot(acp,choix="var")
