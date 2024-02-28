# Homework2

#1- Créer une base de données fictive ayant au moins 5 variables avec 
#--les types numérique, caracters, facteurs...
Ma_base_mbb <- data.frame(Nom = c("BOUSSO","COULIBALY","DIAKHATE","DIAW","DIENG"), Prénom= c("Mame Balla","Khadidiatou", "Khadidiatou","Awa","Samba"),Age =c("23 ans","19 ans","19 ans","20 ans","19 ans"),Menstion_ISEP1=factor(c("Bien","Excellent","Tbien","Tbien","Bien")),Taille=c(185,163,195,180,175))
View(Ma_base_mbb)

#2- Créer une matrice a partir de vos données; renommer les lignes
Matrice_mbb<- as.matrix(Ma_base_mbb)
rownames(Matrice_mbb)<- c("MBB","KC","KD","DIVA","BATHIE")
#3- Faites des statistiques descriptives : mean, max, quartiles
summary(Ma_base_mbb)


attach(Ma_base_mbb)
#Histogramme

library(ggplot2)
ggplot(Ma_base_mbb, aes(x = Nom, y = Taille, fill = Nom)) +
  geom_bar(stat="identity", position="dodge") +
  labs(title="Histogramme de la Taille par Nom",
       x="Nom",
       y="Taille") +
  theme_minimal()

#Camembert
ggplot(Ma_base_mbb, aes(x = "", y = , fill = Nom)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar(theta="y") +
  labs(title="Diagramme circulaire de la Taille par Nom") +
  theme_minimal()


detach(Ma_base_mbb)

#  Problème d'optimisation
#exemple de donnée
#vecteur de coût
c <- c(-8, -4, 0, 0, 0)

#Matrices de restriction
A<-matrix(nrow=3,ncol=5)
A[1,] <- c(5, -2, 1, 0, 0)
A[2,] <- c(8, -2, 0, 1, 0)
A[3,] <- c(8,  1, 0, 0, 1)

# côtés
b<-c(0,1,2)

#solution initiales
B<-matrix(nrow=3,ncol=3)
B[1,] <- c(1, 0, 0)
B[2,] <- c(0, 1, 0)
B[3,] <- c(0, 0, 1)
solIndexes <- c(3,4,5)

#intialisation
simplex <- function(c,A,b,B,solIndexes){
  i = 0
  j = 1
  sum = 0
  max = -1
  min = 1000000
  entryVariable = -1
  exitVariable = -1
  entryVariable.relative = -1
  exitVariable.relative = -1
  cb <- c()
  entryCriterion <- c()
  
  #Etape 1: initialisation
  invB=solve(B)               #inversion de la matrice
  xb <- invB %*% b            #tableau de solution initiale
  for(i in solIndexes){       #tableau 
    cb <- c(cb, c[i])
  }
  cb[is.na(cb)] <- 0
  
  noSolIndexes <- c()         #indexes des variables candidats
  for(i in 1:5){
    if(!i %in% solIndexes){
      noSolIndexes <- c(noSolIndexes,i)
    }
  }
  
  #itération par l'algorithme
  while(TRUE){
    #Étape 2 : critère d'entrée 
    for(i in noSolIndexes){     #on obtient le critère pour décider quelle variable va entrer dans la solution
      ac <- A[,i]
      y  <- invB %*% ac
      
      candidateVariableCost = c[i]
      if(is.na(candidateVariableCost))  candidateVariableCost = 0
      entryCriterion <- c(entryCriterion, cb %*% y - candidateVariableCost)
    }
    
    for(i in entryCriterion){  #maximum (la variable qui va entrer est obtenue)
      if(i<=0){
        sum = sum+1
      }
      else if(i > max){
        max = i
        entryVariable.relative = j
      }
      j = j + 1
    }
    
    if(sum == length(entryCriterion)){ #une solution optimale a été trouvée
      print("[ Optimal solution ]")
      break
    }
    
    entryVariable = noSolIndexes[entryVariable.relative] #l'index de la variable d'entrée est obtenu
    
    
    
    #Étape 3 : critère de sortie
    y <- c()
    sum = 0
    j=1
    y <- invB %*% A[,entryVariable]
    for(i in y){
      if(i <= 0){
        sum = sum + 1
      }else if(xb[j]/i < min){
        min = xb[j]/i
        exitVariable.relative = j
      }
      j = j + 1
    }
    
    exitVariable = solIndexes[exitVariable.relative]
    
    
    if(sum == length(A[,entryVariable])){
      return("[ Unbounded problem ]")
    }
    
    
    #Étape 4 : la solution est recalculée
    B[,exitVariable.relative] = A[,entryVariable]
    
    invB=solve(B)               #inverse de la matrice B
    xb <- invB %*% b            #la solution est obtenue
    solIndexes[exitVariable.relative] = entryVariable 
    noSolIndexes[which(noSolIndexes==entryVariable)] = exitVariable
    cb[exitVariable.relative] = c[entryVariable]
    if(is.na(cb[exitVariable.relative]))  cb[exitVariable.relative] = 0
    
    #les variables temporaires sont nettoyées
    i = 0
    j = 1
    sum = 0
    max = -1
    min = 1000000
    entryVariable = -1
    exitVariable = -1
    entryVariable.relative = -1
    exitVariable.relative = -1
    entryCriterion <- c()
  }
  
  #retour des valeurs
  z = cb[i]%*%xb[i]
  return(list("Valeur des variables" = xb, "coûts minimal" = z, "Base" = solIndexes))
}
#vérification
simplex(c,A,b,B,solIndexes)
