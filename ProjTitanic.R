library(tidyr)

## Exemple d'application
fichierTitanic <- "C:/Users/Utilisateur/Documents/Colombe/Brief_Projet_2/train.csv" 
Titanic <- read.csv(fichierTitanic) #, sep=",")
summary(Titanic)


####### Traitement des valeurs manquantes


# On compte le nombre de valeurs manquantes dans chaque colonne en pourcentage
a <- sapply(Titanic, function(Titanic) (sum(is.na(Titanic))/n)*100)
a

#Faire un test d'hypothèse pour savoir si oui ou non, les enfants ont été privilégiés lors du naufrage.

# Ensuite je supprime toutes les valeurs manquantes qui restent dans la base et de la colonne Cabin
Titanic <- na.omit(Titanic)
Titanic <- Titanic[,-11]
Titanic

Titanic <- subset(Titanic, PassengerId != 62 & PassengerId != 830)
Titanic


#Recuperons les données des passagers de moins de 18 ans
TitaEnfant <- subset(Titanic, Age < 18)

#Je récupère les caractèristiques de age et de survived
TitaEnfant = TitaEnfant[, c('Survived', 'Age')]
names(TitaEnfant)

#Afficher les caractèristiques des colonnes
summary(TitaEnfant) #la moyenne d'age des enfants est 9.04

sapply(TitaEnfant, sd) #ecart type de l'age des enfants = 6.0304077 soit environ 6.03


#Je récupère les caractèristiques de age et de survived des enfants qui ont survecu
TitaEnfantSuv = subset(TitaEnfant, Survived == 1) 
#names(TitaEnfantSur)

#Afficher les caractèristiques des colonnes
summary(TitaEnfantSuv) #la moyenne d'age des enfants est 7.888 environ 7,9

sapply(TitaEnfantSuv, sd) #ecart type de l'age des enfant = 6.129831 environ 6,13


# H0 les enfants ne sont pas privilégiés.
# H1 les enfants sont privilégiés.

# Réalisation du test de khi-deux
#Test de Khideux
test <- chisq.test(TitaEnfantSuv) # TitaEnfantSuv = tableau de contingence


#Test de Khideux
test1 <- chisq.test(TitaEnfant) # TitaEnfantSuv = tableau de contingence

#Accès aux différents objets du test:
test$statistic #: la statistique du Chi2.
test$parameter #: le nombre de degrés de libertés.
test$p.value #: la p-value = 0.372411
test$observed #: la matrice observée de départ.
test$expected #: la matrice attendue sous l'hypothèse nulle d'absence de biais.


#Faire un test d'hypothèse pour vérifier si oui ou non, les femmes ont été privilégiées lors du naufrage.
#Recuperons les données des passagers femmes
TitaFemme <- subset(Titanic, Sex == "female") # Il y a au total 261 femmes à bord

#Je récupère les caractèristiques de age et de survived
TitaFemme = TitaFemme[, c('Survived', 'Sex')]
names(TitaFemme)

#Afficher les caractèristiques des colonnes
#summary(TitaFemme) #la moyenne d'age des enfants est 9.04

#sapply(TitaFemme, sd) #ecart type de l'age des enfants = 6.0304077 soit environ 6.03


#Je récupère les caractèristiques de age et de survived des enfants qui ont survecu
TitaFemmeSuv = subset(TitaFemme, Survived == 1) # 197 femmes ont survécu.
#names(TitaEnfantSur)

#Afficher les caractèristiques des colonnes
#summary(TitaFemmeSuv) #la moyenne d'age des enfants est 7.888 environ 7,9

#sapply(TitaFemmeSuv, sd) #ecart type de l'age des enfant = 6.129831 environ 6,13
Mort <- subset(Titanic, Survived == 0)
Mort

# Réalisation du test de khi-deux
#Test de Khideux
test1 <- chisq.test(TitaFemmeSuv) # TitaEnfantSuv = tableau de contingence
#Accès aux différents objets du test:
test1$statistic #: la statistique du Chi2.
test1$parameter #: le nombre de degrés de libertés.
test1$p.value #: la p-value.
test1$observed #: la matrice observée de départ.
test1$expected #: la matrice attendue sous l'hypothèse nulle d'absence de biais.


#Faire un test d'hypothèse pour savoir si oui ou non, les enfants ont été privilégiés lors du naufrage.
M <- as.table(rbind(c(61, 229), c(52, 327))) # création d'une table 2 lignes/2 colonnes
dimnames(M) <- list(Vivant=c("Oui","Non"), Enfant = c("Oui","Non"))# entête colonne et ligne
test <- chisq.test(M) # affichage des résultats du test

test$statistic #: la statistique du Chi2.
test$parameter #: le nombre de degrés de libertés.
test$p.value #: la p-value.
test$observed #: la matrice observée de départ.
test$expected #: la matrice attendue sous l'hypothèse nulle d'absence de biais.


#Faire un test d'hypothèse pour vérifier si oui ou non, les femmes ont été privilégiées lors du naufrage.
Mat <- as.table(rbind(c(197, 93), c(64, 360))) # création d'une table 2 lignes/2 colonnes
dimnames(Mat) <- list(Vivant=c("Oui","Non"), Femmes = c("Oui","Non"))# entête colonne et ligne
test1 <- chisq.test(Mat) # affichage des résultats du test

test1$statistic #: la statistique du Chi2.
test1$parameter #: le nombre de degrés de libertés.
test1$p.value #: la p-value.
test1$observed #: la matrice observée de départ.
test1$expected #: la matrice attendue sous l'hypothèse nulle d'absence de biais.

#On va récuperer les prix des billets des survivants
TitSuvFare = subset(Titanic, Survived == 1) 
#Je récupère les caractèristiques de Fare et de survived
TitSuvFare2 = TitSuvFare[, c('Fare', 'Survived')]
names(TitSuvFare2)
summary(TitSuvFare2) #la moyenne est 51.84

#On va récuperer les prix des billets des non survivants
TitMortFare = subset(Titanic, Survived == 0) 
#Je récupère les caractèristiques de Fare et de survived
TitMortFare2 = TitMortFare[, c('Fare', 'Survived')]
names(TitMortFare2)
summary(TitMortFare2) #La moyenne est 22.965

#Construction des tranches de prix des survivants
trancheSuvFare1 = subset(TitSuvFare2, Fare < 100) 
trancheSuvFare2 = subset(TitSuvFare2, 100 <= Fare & Fare < 200) 
trancheSuvFare3 = subset(TitSuvFare2, 200 <= Fare & Fare < 300) 
trancheSuvFare4 = subset(TitSuvFare2, 300 <= Fare & Fare < 400) 
trancheSuvFare5 = subset(TitSuvFare2, 400 <= Fare & Fare < 500) 
trancheSuvFare6 = subset(TitSuvFare2, 500 <= Fare & Fare < 600) 


#Construction des tranches de prix des non survivants
trancheMortFare1 = subset(TitMortFare2, Fare < 100) 
trancheMortFare2 = subset(TitMortFare2, 100 <= Fare & Fare < 200) 
trancheMortFare3 = subset(TitMortFare2, 200 <= Fare & Fare < 300) 
trancheMortFare4 = subset(TitMortFare2, 300 <= Fare & Fare < 400) 
trancheMortFare5 = subset(TitMortFare2, 400 <= Fare & Fare < 500) 
trancheMortFare6 = subset(TitMortFare2, 500 <= Fare & Fare < 600) 

# Créations des vecteurs correspondant aux 2 catégories :
Oui = c(254, 22, 14)
Non = c(412, 8, 4)
# Création d'une matrice comparative :
tableau = matrix(c(Oui, Non), 2, 3, byrow=T) # (2 : nombre de lignes et 6 nombres de colonnes (tranches de prix))
# Réalisation du test khi-deux - les résultats sont sauvegardés dans "khi_test"
khi_test = chisq.test(tableau)
khi_test # affiche le résultat du test

