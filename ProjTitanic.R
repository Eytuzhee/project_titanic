library(tidyr)

## Exemple d'application
fichierTitanic <- "C:/Users/Utilisateur/Documents/Colombe/Brief_Projet_2/train.csv" 
Titanic <- read.csv(fichierTitanic) #, sep=",")
summary(Titanic)


####### Traitement des valeurs manquantes


# On compte le nombre de valeurs manquantes dans chaque colonne en pourcentage
a <- sapply(Titanic, function(Titanic) (sum(is.na(Titanic))/n)*100)
a

#Faire un test d'hypoth�se pour savoir si oui ou non, les enfants ont �t� privil�gi�s lors du naufrage.

# Ensuite je supprime toutes les valeurs manquantes qui restent dans la base et de la colonne Cabin
Titanic <- na.omit(Titanic)
Titanic <- Titanic[,-11]
Titanic

Titanic <- subset(Titanic, PassengerId != 62 & PassengerId != 830)
Titanic


#Recuperons les donn�es des passagers qui ont surv�cu
TitaSurvived <- subset(Titanic, Survived == 1)
#288 passagers ont surv�cu lors du naufrage

#Recuperons les donn�es des passagers de moins de 18 ans
TitaEnfant <- subset(Titanic, Age < 18)

#Je r�cup�re les caract�ristiques de age et de survived
TitaEnfant = TitaEnfant[, c('Survived', 'Age')] # Il y a 113 enfants au total parmis les passagers
names(TitaEnfant)

#Afficher les caract�ristiques des colonnes
summary(TitaEnfant) #la moyenne d'age des enfants est 9.04

sapply(TitaEnfant, sd) #ecart type de l'age des enfants = 6.0304077 soit environ 6.03


#Je r�cup�re les caract�ristiques de age et de survived des enfants qui ont survecu
TitaEnfantSuv = subset(TitaEnfant, Survived == 1) # Il y a 61 enfants qui ont surv�cu lors du naufrage et donc 52 qui n'ont pas surv�cu
#names(TitaEnfantSur)

#Afficher les caract�ristiques des colonnes
summary(TitaEnfantSuv) #la moyenne d'age des enfants est 7.888 environ 7,9

sapply(TitaEnfantSuv, sd) #ecart type de l'age des enfant = 6.129831 environ 6,13


#Faire un test d'hypoth�se pour v�rifier si oui ou non, les femmes ont �t� privil�gi�es lors du naufrage.
#Recuperons les donn�es des passagers femmes
TitaFemme <- subset(Titanic, Sex == "female") # Il y a au total 259 femmes � bord


#Je r�cup�re les caract�ristiques de age et de survived
TitaFemme = TitaFemme[, c('Survived', 'Sex')]
names(TitaFemme)


#Je r�cup�re les caract�ristiques de age et de survived des femmes qui ont survecu
TitaFemmeSuv = subset(TitaFemme, Survived == 1) # 195 femmes ont surv�cu.
#names(TitaEnfantSur)


#sapply(TitaFemmeSuv, sd) #ecart type de l'age des enfant = 6.129831 environ 6,13
Mort <- subset(Titanic, Survived == 0)
Mort



#Faire un test d'hypoth�se pour savoir si oui ou non, les enfants ont �t� privil�gi�s lors du naufrage.
M <- as.table(rbind(c(61, 227), c(52, 372))) # cr�ation d'une table 2 lignes/2 colonnes
dimnames(M) <- list(Vivant=c("Oui","Non"), Enfant = c("Oui","Non"))# ent�te colonne et ligne
test <- chisq.test(M) # affichage des r�sultats du test

test$statistic #: la statistique du Chi2.
test$parameter #: le nombre de degr�s de libert�s.
test$p.value #: la p-value.
test$observed #: la matrice observ�e de d�part.
test$expected #: la matrice attendue sous l'hypoth�se nulle d'absence de biais.
#.la p-value = 0.001993878, donc < 0.05, on accepte H0

#Faire un test d'hypoth�se pour v�rifier si oui ou non, les femmes ont �t� privil�gi�es lors du naufrage.
Mat <- as.table(rbind(c(195, 93), c(64, 360))) # cr�ation d'une table 2 lignes/2 colonnes
dimnames(Mat) <- list(Vivant=c("Oui","Non"), Femmes = c("Oui","Non"))# ent�te colonne et ligne
test1 <- chisq.test(Mat) # affichage des r�sultats du test

test1$statistic #: la statistique du Chi2.
test1$parameter #: le nombre de degr�s de libert�s.
test1$p.value #: la p-value.
test1$observed #: la matrice observ�e de d�part.
test1$expected #: la matrice attendue sous l'hypoth�se nulle d'absence de biais.

#On va r�cuperer les prix des billets des survivants
TitSuvFare = subset(Titanic, Survived == 1) 


#Je r�cup�re les caract�ristiques de Fare et de survived
TitSuvFare2 = TitSuvFare[, c('Fare', 'Survived')]
names(TitSuvFare2)
summary(TitSuvFare2) #la moyenne est 51.84

#On va r�cuperer les prix des billets des non survivants
TitMortFare = subset(Titanic, Survived == 0) 

#Je r�cup�re les caract�ristiques de Fare et de survived
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

# Cr�ations des vecteurs correspondant aux 2 cat�gories :
Oui = c(254, 22, 14)
Non = c(412, 8, 4)
# Cr�ation d'une matrice comparative :
tableau = matrix(c(Oui, Non), 2, 3, byrow=T) # (2 : nombre de lignes et 6 nombres de colonnes (tranches de prix))
# R�alisation du test khi-deux - les r�sultats sont sauvegard�s dans "khi_test"
khi_test = chisq.test(tableau)
khi_test # affiche le r�sultat du test

