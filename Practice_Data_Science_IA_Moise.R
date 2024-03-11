#--------------------------------------------------------------------------#
#                   FORMATION DATA SCIENCE ET IA                           #
#--------------------------------------------------------------------------#

# PROJET 1 : developpement d'un modèle arbre de de decision

## Chargement des packages

library(rpart)

## Chargement des jeux de données et structure des données

achat <- read.csv("D:/DOCUMENTS/FORMATIONS COMPLEMENTAIRES EN LOG STAT/MOOC DATA SCIENCE ET IA/MODULE_1/Data_achat.csv", header = TRUE, stringsAsFactors = TRUE)
head(achat)
str(achat) # structure

## Afficher la distribution de la variable achat

table(achat$Achat)

## Echantillon apprentissage et test: achat_EA et achat_ET

achat_EA <- achat[1:400, ]
achat_ET <- achat[401:600, ]
achat_EA <- subset(achat_EA, select = -ID)
summary(achat_EA)
summary(achat_ET)

## Apprentissage par arbre de décision

tree1 <- rpart(Achat ~ ., achat_EA)
x11()
plot(tree1)
text(tree1, pretty = 0 )

## Evaluation de la performance du modèle en appliquant l'arbre construit sur jeu de données test

test_tree1 <- predict(tree1, achat_ET, type = "class")
test_tree1
 
## Ajouter la colonne test_tree1 au jeu de données achat_ET

achat_ET$prediction <- test_tree1
View(achat_ET)

## Calcul du taux de succès et d'échec
taux_succes <- round((nrow(achat_ET[achat_ET$Achat==achat_ET$prediction, ])/nrow(achat_ET))*100, 2)
taux_succes

taux_echec <- 100-taux_succes
taux_echec
## ou
taux_echec_bis <- round((nrow(achat_ET[achat_ET$Achat!=achat_ET$prediction, ])/nrow(achat_ET))*100, 2)
taux_echec_bis

## Prédiction sur un jeu de données
### Importation du jeu de données achat_pro

achat_pro <- read.csv("D:/DOCUMENTS/FORMATIONS COMPLEMENTAIRES EN LOG STAT/MOOC DATA SCIENCE ET IA/MODULE_1/Data_pro.csv", header = TRUE, stringsAsFactors = TRUE)
View(achat_pro)
str(achat_pro)

### Prédiction 

pred_tree1 <- predict(tree1, achat_pro, type = "class")
table(pred_tree1)
achat_pro$prediction <- pred_tree1
head(achat_pro)
achat_pred_oui <- achat_pro[achat_pro$prediction=="Oui", ]
head(achat_pred_oui)
achat_pred_non <- achat_pro[achat_pro$prediction=="Non", ]
head(achat_pred_non)



# PROJET 2: contruire un reseau de neurones deep learning
## Installation et importation du package tensorflow

devtools::install_github("rstudio/tfestimators")
library(tfestimators)
library(tensorflow)
install_tensorflow()


## Importation du jeu de données
library(readr)
library(dplyr)
library(tidyverse)

donor_data <- read.csv("D:/DOCUMENTS/FORMATIONS COMPLEMENTAIRES EN LOG STAT/MOOC DATA SCIENCE ET IA/MODULE_3/DonorSampleDataCleaned.csv", header = TRUE)
View(donor_data)
str(donor_data)
    
## Nettoyage du jeu de données des valeurs manquantes : on remplace les valeurs 
## valeurs manquantes par le mode si la variable est quali et par la mediane
## si elle est quanti

## Créeons la fonction mode

my_mode <- function(x){
  
  x_no_na <- x[!is.na(x)]
  if (length(x_no_na) == 0) {
    return(NA)
  }
  ux <- unique(x_no_na)
  ux[which.max(tabulate(match(x_no_na, ux)))]
}

## test
vect <- c(2, 3,3,6,7,9)
my_mode(vect)

donor_data_copy <- donor_data %>% 
  mutate_if(is.character, as.factor) # copie du dataset

donor_data_copy <- donor_data_copy %>% 
  mutate_if(is.numeric, ~if_else(is.na(.), median(., na.rm =  TRUE), .)) %>% 
  mutate_if(is.factor, ~if_else(is.na(.), my_mode(.), .))

## Selection des predicteurs

vars_pred <- c("AGE", "MARITAL_STATUS", "ALUMNUS_IND", "GENDER", 
               "PARENT_IND", "WEALTH_RATING", "PREF_ADDRESS_TYPE")

str(donor_data_copy[, c("AGE", "MARITAL_STATUS", "ALUMNUS_IND", "GENDER", "PARENT_IND", "WEALTH_RATING", "PREF_ADDRESS_TYPE")])

## Indiquer les types de variables a tensorflow et convertir les variables en 0 et 1

 feature_cols <- feature_columns(
   column_indicator(
     column_categorical_with_vocabulary_list(
       "MARITAL_STATUS",
       vocabulary_list = unique(donor_data_copy$MARITAL_STATUS))),
   column_indicator(
     column_categorical_with_vocabulary_list(
       "GENDER",
       vocabulary_list = unique(donor_data_copy$GENDER))),
   column_indicator(
     column_categorical_with_vocabulary_list(
       "ALUMNUS_IND",
       vocabulary_list = unique(donor_data_copy$ALUMNUS_IND))),
   column_indicator(
     column_categorical_with_vocabulary_list(
       "PARENT_IND",
        vocabulary_list = unique(donor_data_copy$PARENT_IND))),
   column_indicator(
     column_categorical_with_vocabulary_list(
       "WEALTH_RATING",
       vocabulary_list = unique(donor_data_copy$WEALTH_RATING))),
   column_indicator(
     column_categorical_with_vocabulary_list(
       "PREF_ADDRESS_TYPE",
       vocabulary_list = unique(donor_data_copy$PREF_ADDRESS_TYPE))),
   column_numeric("AGE")
 )

## Separation du jeu en data_train et data_test
 
 row_indices <- sample(1:nrow(donor_data_copy), 
                       size = 0.8*nrow(donor_data_copy)) # 80% des individus
donor_data_train <- donor_data_copy[row_indices, ]
donor_data_test <- donor_data_copy[-row_indices, ]

## Création d'une fonction d'entrée

donor_pred_fun <- function(data){
  input_fn(data,
    features = vars_pred,
    response = "DONOR_IND"
  )
}

## Contructeur d'un classifieur

classifieur <- dnn_classifier(
  feature_columns = feature_cols,
  hidden_units = c(80, 40, 30),
  n_classes = 2,
  label_vocabulary = c("N", "Y"))

 
 
 
 
 
 
 
 
 
 

