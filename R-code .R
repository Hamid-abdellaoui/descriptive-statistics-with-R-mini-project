
################## CODE en R de projet stat descriptif ######################### 
################## ABDELLAOUI HAMID   #########################


######### importation et lecture du fichier en R studio  #########
library(readxl)
Salaire_entreprise <- read_excel("sources/Salaire_entreprise.xlsx")
View(Salaire_entreprise) 
#################################################################################################################################
################## Installation des packages  ######################
					#que j'aurai besoin, je vais l'appeler seulement dans les codes qui les necessitent

install.packages(psych)   # istallation de package 'PSYCH' specifié au mesures statistiques
install.packages("ggpubr")  # installation du bibliothèque ggpubr specifié au création du graphes
install.packages("dplyr")      # ,,           ,,            ,,              ,, dplyr      ,,          ,, modification et ajoute des données dans les tables
                                         # notamment contient le fonction mutate(), cependant cette modification n'est pas permanente
install.packages("tibble")    # il contient la fonction add_column() qui permet l'ajout d'un colonne
install.packages("dplyr")     # cette library donne plusieurs fonctions qui donnenent des mesures de statistique descriptive par groupes


#################################################################################################################################
################### Préparation de données on modifie et on ajoute quelque colonnes pour faciliter la manipulation ##########################

#modification de la colonne Niveau d'etude en changeant son nom ainsi en remplacant 
#ses valeurs par un équivalent manipulable afin de l'utiliser dans les graphes 

> names(Salaire_entreprise)[names(Salaire_entreprise) == "Niveau_d’étude"] <- "Nive"
> Salaire_entreprise$Nive[Salaire_entreprise$Nive %in% c("BAC")]<-1
> Salaire_entreprise$Nive[Salaire_entreprise$Nive %in% c("BAC+2")]<-2
> Salaire_entreprise$Nive[Salaire_entreprise$Nive %in% c("BAC+3")]<-3
> Salaire_entreprise$Nive[Salaire_entreprise$Nive %in% c("BAC+4")]<-4
> Salaire_entreprise$Nive[Salaire_entreprise$Nive %in% c("BAC+5")]<-5


# on ajoute un colonne selone le sexe d'employé afin de l'utiliser dans le dessin  de quelques graphes:
library(dplyr)
library(tibble)
Salaire_entreprise <- Salaire_entreprise %>%
  add_column(gender = case_when(
    endsWith(Sexe, "H") ~ "1",
    endsWith(Sexe, "F") ~ "0"
    ))
####### decoupage des varibles anciennete et salaire net en intervales  #######

> Salaire_entreprise$sal_int<-cut(Salaire_entreprise$Salaire_net_en_MAD, seq(0,12000,1500))   # decoupage en intervalles de longuer 1500
> Salaire_entreprise$experience<-cut(Salaire_entreprise$anciennete, seq(0,10,1))                         # decoupage en intervalles de longuer  1

#################################################################################################################################
############## stat sur les variables ##############

# calcul de la moyen , median , mode, boxplot ...

# pour l'ancienneté /annees d'experience
> library(psych)
> describe(Salaire_entreprise$'Nb_année_d’expérience')
> quantile(Salaire_entreprise$'Nb_année_d’expérience')
> IQR(Salaire_entreprise$'Nb_année_d’expérience')
> library(ggpubr)       # pour dessin de boite a moustache
> ggboxplot(Salaire_entreprise, y="Nb_année_d’expérience",main="boite a moustache de ancienneté", width = 0.25, xlab=FALSE,fill = "yellow",
                   col = "blue",
                  notch = TRUE)

#pour les salaires net
> library(psych)
> describe(Salaire_entreprise$'Salaire_net_en_MAD')
> quantile(Salaire_entreprise$'Salaire_net_en_MAD')
> mad(Salaire_entreprise$'Salaire_net_en_MAD')
> IQR(Salaire_entreprise$'Salaire_net_en_MAD')
> library(ggpubr)     # pour dessin de boite a moustache
> ggboxplot(Salaire_entreprise, y="Salaire_net_en_MAD",main="boite a moustache des salaires des employes", width = 0.25, xlab=FALSE,
                    col = "blue",
                   notch = TRUE)



### Dessin des histogrammes
> gghistogram(Salaire_entreprise,main="histogrammes des années d'experience", x = "anciennete",ylab = "frequence", bins = 10,fill ='orange', color = 'blue', size=1.5, add = "mean") # histo de ancienneté
> gghistogram(Salaire_entreprise,main="histogrammes des salaires net", x = "Salaire_net_en_MAD",ylab = "frequence", bins = 10,fill ='orange', color = 'blue', size=1.5,   add = "mean") # histo de salaires net , on divisé sur 10 donc chaque intervalle correspont a 1250 MAD


## quelques Statistiques sur niveau d'etudes
> library(psych)
> describe(as.numeric(Salaire_entreprise$Nive))  ##besion de convertire vers le type number because les valeurs ont un type character   
> quantile(as.numeric(Salaire_entreprise$Nive))
> IQR(as.numeric(Salaire_entreprise$Nive))
> hist(as.numeric(Salaire_entreprise$Nive), main = "niveau des etudes",col = 'orange')    # histogramme de niveaux d'études

######################################################################################################################### 
#########################  stat bivarié de deux variables ou plus ######################### 
# les deux boxplot  de salaires net des femmes et hommes  dans le meme plot pour comparaison
> library(dplyr)
> ggboxplot(Salaire_entreprise, x = "Sexe", y = "Salaire_net_en_MAD",   
         color = "Sexe",
         palette = c("#00AFBB", "#E7B800", "#FC4E07"))

# les deux boxplot  de ancienneté/années d'expérience des femmes et hommes  dans le meme plot pour comparaison
> ggboxplot(Salaire_entreprise, x = "Sexe", y = "Nive",           
        color = "Sexe", 
	palette = c("#00AFBB", "#E7B800", "#FC4E07"))


################ pour etablire les tableaux de fréquences.effectifs on crée des DATAFRAMES ( sont comme des extrait du tableau) ##################### 

> mydf1=data.frame(Salaire_entreprise$gender,Salaire_entreprise$Nive)      # DataFrame de Sexe(gender) * Niveau d'étude(NIVE)
> mydf2=data.frame(Salaire_entreprise$gender,Salaire_entreprise$Sal_int)    # DataFrame de Sexe(gender) * intervalles des salaires(SAL_INT)
> mydf3=data.frame(Salaire_entreprise$gender,Salaire_entreprise$Sal_int,Salaire_entreprise$Nive)    # DataFrame de Sexe(gender) * intervalles des salaires(SAL_INT)* Niveau d'étude(NIVE)
> mydf4=data.frame(Salaire_entreprise$Nive,Salaire_entreprise$Sal_int)

> tab1= table(mydf1)
> addmargins(tab1)    # la fonction addmargins pour ajouer un  colonnes  et une lignes des sommes , on affiche ensuite la table, et on repete meme choses pour les autres tableaux

> tab3= table(mydf2)
> addmargins(tab2)

> tab3= table(mydf3)
> addmargins(tab3)

> tab4= table(mydf34)
> addmargins(tab4)

############   dessin des secteurs ######################### 
> prop1=c(87,106)    # vecteur nombre des femmes et nb des hommes
> noms=c("femmes","hommes")   
> pie(prop1,noms,col =c("pink","blue") ,main="repartition selon sexe")    # dessin des secteur 



> prop2=c(17,23,14,26,7)   # vecteur des femmes dans chaque niveau de bac
> noms2=c("bac","bac+2","bac+3","bac+4","bac+5")
> pie(prop2,noms2,main = "repartition des femmes selon niveau etude")

> prop3=c(19 ,19,  19,  37,  12)     # vecteur des hommes dans chaque niveau
> pie(prop3,noms2,main = "repartition des hommes selon niveau etude")

# le dernier  graphique  representant les personnes comme des pointsen fonction de leur salaire et niveau d'étude
> plot(x = Salaire_entreprise$Nive, y=Salaire_entreprise$Salaire_net_en_MAD, sub = "salaire net en fonctionde niveau d'etude")

