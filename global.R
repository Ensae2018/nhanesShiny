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

# on importe les donnees necessaire pour le projet
donHyp <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)# donnee hypertension transcodifie et avec imputation mice

# on importe les donnees necessaire pour le projet
#donChol <- read.csv("data/nhanes_chol_mice_faux.csv", row.names = 1)# donnee cholesterol transcodifie et avec imputation mice
donChol <- read.csv("data/nhanes_chol_mice_step_finale.csv", row.names = 1)# donnee cholesterol transcodifie et avec imputation mice

# on importe les donnees necessaire pour le projet: partie diabete
donDia <- read.csv("data/nhanes_dia_mice_apres.csv", sep=",",dec=".",row.names=1)# donnee diabete transcodifie et avec imputation mice

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

# Conversion du facteur Yes/No vers 1/0 pour Y
levels(donHyp$Y) <- c(0,1)
#levels(donChol$Y) <- c(0,1) #pas besoin pour jdd cholesterol
donChol$nhanes.y <- as.numeric(donChol$nhanes.y)
#levels(donDiab$Y) <- c(0,1)#pas besoin pour diabete, Y=DIQ010_diq est déjà 0/1 en integer

# algorithme de prediction hypertension
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
modHyp <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
                high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
                Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
                Sodium_mg,data=donHyp,family="binomial")

# algorithme de prediction CHOLESTEROL
## Utilisation du modele logistique avec le choix d'une dizaine de variables prépondérantes
# modChol <- glm(nhanes.y~.,data=donChol,family="binomial")
modChol <- glm(nhanes.y~RIDAGEYR_demo+RIAGENDR_demo+INDFMPIR_demo+Var_TRAVAIL+
                 BMXHT_bmx+BMXWT_bmx+BMXBMI_bmx+BPQ020_bpq+MCQ080_mcq+DIQ010_diq+
                 BPXDI2_bpx+BPXSY3_bpx+SLQ050_slq+OHAREC_ohxref+
                 DRQSDIET_dr1tot+DR1TFIBE_dr1tot+DR1TALCO_dr1tot+DR1TFF_dr1tot+DR1.320Z_dr1tot,data=donChol,family="binomial")

# algorithme de prediction DIABETES
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
# modDiab <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
#              high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
#              Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
#              Sodium_mg,data=donDiab,family="binomial")

# Le résulatat de comparaison des méthodes de prédiction Hypertension
res_hyp <- read.csv2("data/res_hyp.csv")

# Le résulatat de comparaison des méthodes de prédiction Cholesterol
res_chol <- read.csv2("data/res_chol.csv")

# Le résulatat de comparaison des méthodes de prédiction Diabète
res_dia <- read.csv("data/res_dia.csv",header=TRUE,sep=",",dec=".",row.names=1)
str(res_dia)

# on charge la table de selection des variable pour l'Hypertension
tabselvar_hyp <- read.table("data/choix_var.csv", header=T, sep=";",row.names = NULL)
max_val_hyp <- apply(tabselvar_hyp[,-1],2,function(x) rank(-x,na.last = T,ties.method = "first"))

# on charge la table de selection des variable pour le Cholesterol => Yfan format CSV specifique à reproduire
#tabselvar_chol <- read.csv2("data/choix_var_chol.csv",row.names = NULL)
#tabselvar_chol <- read.table("data/choix_var_chol.csv", header=T, sep=";",row.names = NULL)
tabselvar_chol <- read.table("data/choix_var_chol.csv", header=T, sep=";",dec=".",row.names = NULL)

#tabselvar_chol <- read.table("data/choix_var_chol_faux.csv", header=T, sep=";",row.names = NULL) 
max_val_chol <- apply(tabselvar_chol[,-1],2,function(x) rank(-x,na.last = T,ties.method = "first"))

# on charge la table de selection des variable pour le diabete
tabselvar_dia <- read.table("data/choix_var_dia.csv", header=T, sep=";",dec=",",row.names = NULL)
max_val <- apply(tabselvar_hyp[,-1],2,function(x) rank(-x,na.last = T,ties.method = "first"))



# chargement des scripts
source("script/Classif nutriment.R") #utile pour la classification des nutriments

options(shiny.trace=FALSE)
