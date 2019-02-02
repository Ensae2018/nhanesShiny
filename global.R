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

# test diff avec github
#test pour JV

# on importe les donnees necessaire pour le projet
donHyp <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)# donnee hypertension transcodifie et avec imputation mice

# on importe les donnees necessaire pour le projet
donChol <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)# donnee cholesterol transcodifie et avec imputation mice

# on importe les donnees necessaire pour le projet
donDiab <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)# donnee daibetes transcodifie et avec imputation mice

# algorithme de prediction hypertension
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
modHyp <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
             high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
             Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
             Sodium_mg,data=donHyp,family="binomial")

# algorithme de prediction CHOLESTEROL
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
modChol <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
             high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
             Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
             Sodium_mg,data=donChol,family="binomial")

# algorithme de prediction DIABETES
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
modDiab <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
             high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
             Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
<<<<<<< HEAD
             Sodium_mg,data=don,family="binomial")

# chargement des scripts
source("script/Classif nutriment.R")
=======
             Sodium_mg,data=donDiab,family="binomial")
>>>>>>> 36a3c249128b886756494057779fab3bbc280266
