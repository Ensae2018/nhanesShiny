# chargement du jeu de donnee
nutriment <- donHyp[,c(which(colnames(donHyp)=="high_cholesterol_level"),
                       which(colnames(donHyp)=="Doctor_told_you_have_diabetes"),
                       which(colnames(donHyp)=="Y"),
                       which(colnames(donHyp)=="Age_in_years_at_screening"),
                       which(colnames(donHyp)=="Sleep_hours"),
                       which(colnames(donHyp)=="Energy_kcal"):which(colnames(donHyp)=="Moisture_gm"))]
names(nutriment)[3] <- "hypertension"

# ACP
acp <- PCA(nutriment,scale.unit=T,graph = F, quali.sup = 1:3, quanti.sup = 4:5)
var <- get_pca_var(acp)
ind <- get_pca_ind(acp)

# Kmeans
#selon la conso des nutriments
partition <- 1:15
for (i in 1:15){
  kmk <- kmeans(var$coord,centers = i, nstart = 10)
  partition[i]= kmk$tot.withinss/kmk$totss*100
}

#selon le comportement des individus 
partitionbis <- 1:15
for (i in 1:15){
  kmk <- kmeans(ind$coord,centers = i, nstart = 10)
  partitionbis[i]= kmk$tot.withinss/kmk$totss*100
}
