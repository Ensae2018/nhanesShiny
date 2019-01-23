# On précise les packages necessaires pour le projet et ses utilisations
library(glmnet)#contrainte
library(randomForest)#Foret
library(gbm)#Boosting
library(e1071)#SVM
library(rpart)#Arbre
library(foreach)#parallel
library(shinythemes)#☺ Choix theme Shiny

# on importe les donnees necessaire pour le projet
don <- read.csv("data/nhanes_hyper_mice.csv")# donnee daibete transcodifie et avec imputation mice