# On précise les packages necessaires pour le projet et ses utilisations
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel
library(shinythemes)#☺ Choix theme Shiny

# on importe les donnees necessaire pour le projet
don <- read.csv("data/nhanes_hyper_mice.csv", row.names = 1)# donnee daibete transcodifie et avec imputation mice

# algorithme de prediction hypertension
## Utilisation du modele logistique avec le choix des 10 variables prépondérantes
mod <- glm(Y~Age_in_years_at_screening+Systolic_Blood_pres_2nd_rdg_mm_Hg+
             high_cholesterol_level+Body_Mass_Index_kg_m_2+Doctor_ever_said_you_were_overweight+
             Ever_told_doctor_had_trouble_sleeping+Phosphorus_mg+Diastolic_Blood_pres_1st_rdg_mm_Hg+
             Sodium_mg,data=don,family="binomial")