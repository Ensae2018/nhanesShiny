# On précise les packages necessaires pour le projet et ses utilisations
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel
library(shinythemes)#Choix theme Shiny
library(ggplot2)#graphique plus joli
library(FactoMineR)# ACP
library(factoextra)# graphique ACP
library(DT)#utile pour la table interactive
library(pROC)#Etude courbe ROC et AUC
library(shinyWidgets)#widgets avancés pour Shiny
library(igraph) # des graphe type phylogenic 
library(plotly)#graphes interactifs

#options(error=recover)

# on importe les donnees necessaire pour le projet
donHyp <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)# donnee hypertension transcodifie et avec imputation mice

# on importe les donnees necessaire pour le projet
donChol <- read.csv("data/nhanes_chol_mice_finale.csv", row.names = 1)# donnee cholesterol transcodifie et avec imputation mice
# A TRAITER
#don2Chol <- read.csv("data/nhanes_chol_mice_finale_transco.csv", header=T, sep=";", dec=".", row.names = 1)
#str(donChol)

# on importe les donnees necessaire pour le projet: partie diabete
donDia <- read.csv2("data/nhanes_dia_avant_transco.csv", sep=",",dec=".",row.names = 1)# donnee diabete transcodifie et avec imputation mice

# Transformer toutes les variables avec dif levels en FACTOR
#RIDSTATR_demo  : Factor w/ 1 level "2": 1
#donChol$RIDSTATR_demo <- as.factor(don$RIDSTATR_demo)
donChol$RIAGENDR_demo <- as.factor(donChol$RIAGENDR_demo)
#donChol$INDHHIN2_demo <- as.factor(donChol$INDHHIN2_demo) #contexte
#donChol$INDFMIN2_demo <- as.factor(donChol$INDFMIN2_demo) #contexte
donChol$BPQ020_bpq <- as.factor(donChol$BPQ020_bpq)
donChol$DIQ010_diq <- as.factor(donChol$DIQ010_diq)
donChol$HEQ010_heq <- as.factor(donChol$HEQ010_heq)
donChol$HEQ030_heq <- as.factor(donChol$HEQ030_heq)
donChol$HOQ065_hoq <- as.factor(donChol$HOQ065_hoq)
donChol$HIQ011_hiq <- as.factor(donChol$HIQ011_hiq)
#donChol$IMQ020_imq <- as.factor(donChol$IMQ020_imq)
#donChol$IMQ011_imq <- as.factor(donChol$IMQ011_imq)
donChol$MCQ010_mcq <- as.factor(donChol$MCQ010_mcq)
donChol$MCQ080_mcq <- as.factor(donChol$MCQ080_mcq)
donChol$OHAREC_ohxref <- as.factor(donChol$OHAREC_ohxref)
donChol$PAQ605_paq <- as.factor(donChol$PAQ605_paq)
donChol$PAQ620_paq <- as.factor(donChol$PAQ620_paq)
donChol$PAQ635_paq <- as.factor(donChol$PAQ635_paq)
donChol$PAQ650_paq <- as.factor(donChol$PAQ650_paq)
donChol$PAQ665_paq <- as.factor(donChol$PAQ665_paq)
donChol$SLQ050_slq <- as.factor(donChol$SLQ050_slq)
#DRABF_dr1tot   : Factor w/ 1 level "2": 1
#donChol$DRABF_dr1tot <- as.factor(donChol$DRABF_dr1tot)
donChol$DRQSDIET_dr1tot <- as.factor(donChol$DRQSDIET_dr1tot)


# #RIDSTATR_demo  : Factor w/ 1 level "2": 1
# #donChol$RIDSTATR_demo <- as.factor(don$RIDSTATR_demo)
# donChol$"Gender" <- as.factor(donChol$"Gender")
# #donChol$INDHHIN2_demo <- as.factor(donChol$INDHHIN2_demo) #contexte
# #donChol$INDFMIN2_demo <- as.factor(donChol$INDFMIN2_demo) #contexte
# donChol$"Ever told you had high blood pressure" <- as.factor(donChol$"Ever told you had high blood pressure")
# donChol$"Doctor told you have diabetes" <- as.factor(donChol$"Doctor told you have diabetes")
# donChol$"Ever told you have Hepatitis B?" <- as.factor(donChol$"Ever told you have Hepatitis B?")
# donChol$"Ever told you have Hepatitis C?" <- as.factor(donChol$"Ever told you have Hepatitis C?")
# donChol$"Home owned, bought, rented, other" <- as.factor(donChol$"Home owned, bought, rented, other")
# donChol$"Covered by health insurance" <- as.factor(donChol$"Covered by health insurance")
# #donChol$IMQ020_imq <- as.factor(donChol$IMQ020_imq)
# #donChol$IMQ011_imq <- as.factor(donChol$IMQ011_imq)
# donChol$"Ever been told you have asthma" <- as.factor(donChol$"Ever been told you have asthma")
# donChol$"Doctor ever said you were overweight" <- as.factor(donChol$"Doctor ever said you were overweight")
# donChol$"Overall recommendation for care" <- as.factor(donChol$"Overall recommendation for care")
# donChol$"Vigorous work activity" <- as.factor(donChol$"Vigorous work activity")
# donChol$"Moderate work activity" <- as.factor(donChol$"Moderate work activity")
# donChol$"Walk or bicycle" <- as.factor(donChol$"Walk or bicycle")
# donChol$"Vigorous recreational activities" <- as.factor(donChol$"Vigorous recreational activities")
# donChol$"Moderate recreational activities" <- as.factor(donChol$"Moderate recreational activities")
# donChol$"Ever told doctor had trouble sleeping?" <- as.factor(donChol$"Ever told doctor had trouble sleeping?")
# #DRABF_dr1tot   : Factor w/ 1 level "2": 1
# #donChol$DRABF_dr1tot <- as.factor(donChol$DRABF_dr1tot)
# donChol$"On special diet?" <- as.factor(donChol$"On special diet?")

#Conversion en facteurs de variables diabete
donDia$RIAGENDR_demo<-factor(donDia$RIAGENDR_demo)
donDia$DIQ010_diq<-factor(donDia$DIQ010_diq)
donDia$BPQ020_bpq<-as.factor(donDia$BPQ020_bpq)
donDia$BPQ080_bpq<-as.factor(donDia$BPQ080_bpq)
donDia$MCQ080_mcq<-as.factor(donDia$MCQ080_mcq)
donDia$MCQ160A_mcq<-as.factor(donDia$MCQ160A_mcq)
donDia$MCQ160B_mcq<-as.factor(donDia$MCQ160B_mcq)
donDia$MCQ160C_mcq<-as.factor(donDia$MCQ160C_mcq)
donDia$MCQ160D_mcq<-as.factor(donDia$MCQ160D_mcq)
donDia$MCQ160E_mcq<-as.factor(donDia$MCQ160E_mcq)
donDia$MCQ160F_mcq<-as.factor(donDia$MCQ160F_mcq)
donDia$MCQ160G_mcq<-as.factor(donDia$MCQ160G_mcq)
donDia$MCQ160M_mcq<-as.factor(donDia$MCQ160M_mcq)
donDia$MCQ160N_mcq<-as.factor(donDia$MCQ160N_mcq)
donDia$SLQ050_slq<-as.factor(donDia$SLQ050_slq)
donDia$HEQ010_heq<-as.factor(donDia$HEQ010_heq)
donDia$HEQ030_heq<-as.factor(donDia$HEQ030_heq)
donDia$DIQ010_diq<-factor(donDia$DIQ010,levels=c(0,1),labels=c("Sains","Malades"))


donDia_transco<-donDia
names(donDia_transco)<-c("SEQN","Gender", "Age.in.years.at.screening.", "Total.number.of.people.in.the.Household", 
                         "Annual.household.income", "Ratio.of.family.income.to.poverty", 
                         "Weight..kg.", "Standing.Height..cm.", "Body.Mass.Index..kg.m..2.", 
                         "Ever.told.you.had.high.blood.pressure", "high.cholesterol.level", 
                         "Doctor.told.you.have.diabetes", "Ever.told.you.have.Hepatitis.B.", 
                         "Ever.told.you.have.Hepatitis.C.", "Number.of.rooms.in.home", 
                         "Doctor.ever.said.you.were.overweight", "Doctor.ever.said.you.had.arthritis", "Doctor.ever.told.you.that.you.had.gout", 
                         "Ever.told.had.congestive.heart.failure", "Ever.told.you.had.coronary.hart.disease", "Ever.told.you.had.angina.pectoris", "Ever.told.you.had.heart.attack", "Ever.told.you.had.a .stroke", 
                         "Ever.told.you.had.emphysema", "Ever.told.you.had.thyroid.problem", "Sleep.hours.", "Ever.told.doctor.had.trouble.sleeping.", 
                         "Energy..kcal.", "Protein..gm.", "Carbohydrate..gm.", "Total.sugars..gm.", 
                         "Dietary.fiber..gm.", "Total.fat..gm.", "Total.saturated.fatty.acids..gm.", 
                         "Total.monounsaturated.fatty.acids..gm.", "Total.polyunsaturated.fatty.acids..gm.", 
                         "Cholesterol..mg.", "tocopherol..mg.", "tocopherol..Vitamin.E...mg.", 
                         "Retinol..mcg.", "Vitamin.A..RAE..mcg.", "carotene..mcg.", "carotene..mcg..1", 
                         "cryptoxanthin..mcg.", "Lycopene..mcg.", "Lutein...zeaxanthin..mcg.", 
                         "Thiamin..Vitamin.B1...mg.", "Riboflavin..Vitamin.B2...mg.", 
                         "Niacin..mg.", "Vitamin.B6..mg.", "Total.folate..mcg.", "Folic.acid..mcg.", 
                         "Food.folate..mcg.", "Folate..DFE..mcg.", "Total.choline..mg..", 
                         "Vitamin.B12..mcg.", "Added.vitamin.B12..mcg.", "Vitamin.C..mg.", 
                         "Vitamin.D..D2...D3...mcg.", "Vitamin.K..mcg.", "Calcium..mg.", 
                         "Phosphorus..mg.", "Magnesium..mg.", "Iron..mg.", "Zinc..mg.", 
                         "Copper..mg.", "Sodium..mg.", "Potassium..mg.", "Selenium..mcg.", 
                         "Caffeine..mg.", "Theobromine..mg.", "Alcohol..gm.", "Moisture..gm.", 
                         "Total.plain.water.drank.yesterday..gm", "VAR_EDUCATION", "VAR_TRAVAIL", "VAR_DEPRESSION", 
                         "VAR_SITUATION", "VAR_COFUMEUR", "VAR_TENSIONSY", "VAR_TENSIONDI", 
                         "VAR_ARGENTALIM", "VAR_DENTISTE", "VAR_ASSURE", "VAR_ACTIVITE")





# Conversion du facteur Yes/No vers 1/0 pour Y
levels(donHyp$Y) <- c(0,1)
donHyp$Y<-factor(donHyp$Y,levels=c(0,1),labels=c("Sains","Malades"))

#levels(donChol$Y) <- c(0,1) #pas besoin pour jdd cholesterol
#donChol$Y <- as.numeric(donChol$Y) #si jamais
#donChol$Y <- as.factor(donChol$Y)
donChol$Y<-factor(donChol$Y,levels=c(0,1),labels=c("Sains","Malades"))

#levels(donDiab$Y) <- c(0,1)#pas besoin pour diabete, Y=DIQ010_diq est déjà 0/1 en integer

# algorithme de prediction hypertension
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
modHyp <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
                high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
                Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
                Sodium_mg,data=donHyp,family="binomial")

#onglet coefficients pour visualiser les coefficnets de la logistique
coefhyp<-data.frame(Variable=rownames(summary(modHyp)$coefficients),Estimate=summary(modHyp)$coefficients[,1],
                    StdEr=summary(modHyp)$coefficients[,2],
                    StatTest=summary(modHyp)$coefficients[,3],
                    pValue=summary(modHyp)$coefficients[,4])

# algorithme de prediction CHOLESTEROL
## Utilisation du modele logistique avec le choix d'une dizaine de variables prépondérantes
# finalement les 15 var retenues avec le choix var de Log90
modChol <- glm(Y~RIDAGEYR_demo+
                 RIAGENDR_demo+
                 #INDFMPIR_demo+
                 Var_TRAVAIL+
                 BMXHT_bmx+
                 BMXWT_bmx+
                 #BMXBMI_bmx+#je desactive pq conflit visuel dans les param de linterface prediction
                 BPQ020_bpq+
                 MCQ080_mcq+
                 DIQ010_diq+
                 #BPXDI2_bpx+
                 #BPXSY3_bpx+
                 SLQ050_slq+
                 #OHAREC_ohxref+
                 DRQSDIET_dr1tot+
                 DR1TFIBE_dr1tot+
                 #DR1TALCO_dr1tot+
                 #DR1TFF_dr1tot+
                 #DR1.320Z_dr1tot,
                 #HOD050_hoq+
                 #PAQ635_paq+
                 #DR1TLZ_dr1tot+
                 #DR1TVC_dr1tot+
                 #DR1TMOIS_dr1tot,
                 DR1TVB6_dr1tot+
                 DR1TCHOL_dr1tot+
                 DR1TB12A_dr1tot,data=donChol,family="binomial")

#onglet coefficients pour visualiser les coefficnets de la logistique
coefcho<-data.frame(Variable=rownames(summary(modChol)$coefficients),Estimate=summary(modChol)$coefficients[,1],
                    StdEr=summary(modChol)$coefficients[,2],
                    StatTest=summary(modChol)$coefficients[,3],
                    pValue=summary(modChol)$coefficients[,4])

# algorithme de prediction DIABETES
## Utilisation du modele logistique step réduit aux 15 variables d'importance

donDiapred<-donDia
donDiapred<-donDia[,-1]
donDiapred$DIQ010_diq<-as.numeric(donDiapred$DIQ010_diq)-1

modDiab <- glm(DIQ010_diq~RIDAGEYR_demo+
                 BPQ080_bpq+
                 MCQ080_mcq+
                 BPQ020_bpq+
                 DR1TSUGR_dr1tot+
                 DR1TKCAL_dr1tot+
                 DR1TCARB_dr1tot+
                 DR1TTFAT_dr1tot+
                 DR1TMOIS_dr1tot+
                 INDFMPIR_demo+
                 RIAGENDR_demo+
                 DR1TCHOL_dr1tot+
                 DR1.320Z_dr1tot+
                 Var_TENSIONDI+
                 Var_ACTIVITE,
               data=donDiapred,family="binomial")

#onglet coefficients pour visualiser les coefficnets de la logistique
coefdia<-data.frame(Variable=rownames(summary(modDiab)$coefficients),Estimate=summary(modDiab)$coefficients[,1],
                    StdEr=summary(modDiab)$coefficients[,2],
                    StatTest=summary(modDiab)$coefficients[,3],
                    pValue=summary(modDiab)$coefficients[,4])

# Le résulatat de comparaison des méthodes de prédiction Hypertension
res_hyp <- read.csv2("data/res_hyp.csv")

# Le résulatat de comparaison des méthodes de prédiction Cholesterol
res_chol <- read.csv2("data/res_chol.csv")

# Le résulatat de comparaison des méthodes de prédiction Diabète
res_dia <- read.csv("data/res_dia.csv",header=TRUE,sep=",",dec=".",row.names=1)


# on charge la table de selection des variable pour l'Hypertension
tabselvar_hyp <- read.table("data/choix_var.csv", header=T, sep=";",row.names = NULL)
max_val_hyp <- apply(tabselvar_hyp[,-1],2,function(x) rank(-x,na.last = T,ties.method = "first"))

# on charge la table de selection des variable pour le Cholesterol => Yfan format CSV specifique à reproduire
#tabselvar_chol <- read.table("data/choix_var_chol.csv", header=T, sep=";",dec=".",row.names = NULL)
tabselvar_chol <- read.table("data/choix_var_chol_transco.csv", header=T, sep=";",dec=".",row.names = NULL)
max_val_chol <- apply(tabselvar_chol[,-1],2,function(x) rank(-x,na.last = T,ties.method = "first"))
tabselvar_chol[,-1]<-round(tabselvar_chol[,-1],3) #pour regler le problème de yellow avec les rangs

# on charge la table de selection des variable pour le diabete
tabselvar_dia <- read.table("data/choix_var_dia.csv", header=T, sep=";",dec=",",row.names = NULL)
max_val_dia <- apply(tabselvar_dia[,-1],2,function(x) rank(-x,na.last = T,ties.method = "first"))
tabselvar_dia[,-1]<-round(tabselvar_dia[,-1],3)

# chargement des scripts
source("script/Classif nutriment.R") #utile pour la classification des nutriments

seuil_hyp <- round(coords(roc(res_hyp[,1],res_hyp[,2]), "best", best.method = "closest.topleft")[1],3)
seuil_dia <- round(coords(roc(res_dia[,1],res_dia[,2]), "best", best.method = "closest.topleft")[1],3)
seuil_chol <-round(coords(roc(res_chol[,1],res_chol[,2]), "best", best.method = "closest.topleft")[1],3)


options(shiny.trace=FALSE)