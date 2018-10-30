if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra,MASS,car,alr3)


# Analyse préliminaire des données ----
data <- read.csv("AutoBodyInjury.csv",sep = ";")

## Correction des erreurs dans MARITAL
correction <- gsub("ma.*","married",data$MARITAL)
correction <- gsub("si.*","single",correction)
data$MARITAL <- factor(correction)

## Correction des erreurs dans CLMSEX
correction <- gsub("male*","M",data$CLMSEX)
data$CLMSEX <- factor(correction)

## Correction des erreurs dans CLMAGE (retrait de la ligne de l'âge 610)
data[66,7] <- 61

# Conversion en factor de ATTORNEY,CLIMINSUR & SEATBELT
for(i in c("ATTORNEY","CLMINSUR","SEATBELT")){
    data[[i]] <- as.factor(data[[i]])
}

#sapply(data,class)

# Création du plot des fréquences ----
df_list <- list()
for(i in c("ATTORNEY","CLMSEX","MARITAL","CLMINSUR","SEATBELT") ){
    tab <- table(data[[i]])
    df <- as.data.frame(tab)
    df$Var1 <- levels(data[[i]])
    names(df)[names(df)=="Var1"] <- i
    df_list[[i]] <- df
}
p1 <- ggplot(data=df_list[[1]], aes(x=ATTORNEY,y = Freq,color=ATTORNEY)) + geom_bar(stat="identity",fill = "lightgrey") +theme_classic()
p2 <- ggplot(data=df_list[[2]], aes(x=CLMSEX,y = Freq,color=CLMSEX)) + geom_bar(stat="identity",fill = "lightgrey") +theme_classic()
p3 <- ggplot(data=df_list[[3]], aes(x=MARITAL,y = Freq,color=MARITAL)) + geom_bar(stat="identity",fill = "lightgrey") +theme_classic()
p4 <- ggplot(data=df_list[[4]], aes(x=CLMINSUR,y = Freq,color=CLMINSUR)) + geom_bar(stat="identity",fill = "lightgrey") +theme_classic()
p5 <- ggplot(data=df_list[[5]], aes(x=SEATBELT,y = Freq,color=SEATBELT)) + geom_bar(stat="identity",fill = "lightgrey") +theme_classic()
grid.arrange(p1,p2,p3,p4,p5, nrow = 2)


#################################### Modèle #################################
summary(data$LOSS)

## Développement pour les transformations à faire : 

##test pour respecter les hypothèses de base avec la seule variable continue
temp <- lm(LOSS ~ CLMAGE ,data=data)
par(mfrow = c(1,3))
plot(data$CLMAGE,data$LOSS)
plot(temp$fitted.values,rstudent(temp))
plot(data$CLMAGE,rstudent(temp))
## Avec ces graphiques là on voit qu'il y a des problèmes avec les postulats de base de la régression
# Ici, on fait box cox pour trouver la transformation à appliquer sur Loss..

par(mfrow = c(1,1))
bcox <- lm(LOSS ~ CLMAGE,data = data) # On a omis les variables qualitatives pour en faire un modèle simple
# avec les variables qualitatives incluts dans B0
boxcox(bcox) # Boxcox indique lambda = 0 à 95% -> la transformation a Y à faire est log(Y)

# On trouve que la meilleur transformation est la logarithme:
# Et on voit que les postulats sont respectés
par(mfrow = c(1,3))
temp <- lm(log(LOSS) ~ CLMAGE ,data=data)
plot(data$CLMAGE,log(data$LOSS))
plot(temp$fitted.values,rstudent(temp))
plot(data$CLMAGE,rstudent(temp))

## Mon idée: Enlevé les interactions et variables du modèle complet plus bas jusqu'à ce qu'on ait
## un modèle avec de bons vifs et ensuite faire la méthode algorithmique pour trouver le vrai modèle
## adéquat


#Toutes les interactions possibles...
fit <- lm(log(LOSS)~CLMAGE + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
            + CLMAGE*ATTORNEY + CLMAGE*CLMSEX + CLMAGE*MARITAL + CLMAGE*CLMINSUR + CLMAGE*SEATBELT
            + ATTORNEY*CLMSEX + ATTORNEY*MARITAL + ATTORNEY*CLMINSUR + ATTORNEY*SEATBELT
            + CLMSEX*MARITAL + CLMSEX*CLMINSUR + CLMSEX*SEATBELT
            + MARITAL*CLMINSUR + MARITAL*SEATBELT
            +CLMINSUR*SEATBELT,data=data)

## Le modele avec des vifs correctent est donc le suivant:
fit2 <- lm(log(LOSS)~CLMAGE + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
           + CLMAGE*ATTORNEY + CLMAGE*CLMSEX  + CLMAGE*SEATBELT
           + ATTORNEY*CLMSEX  + ATTORNEY*SEATBELT
            + CLMSEX*SEATBELT
           ,data=data)
   
vif(fit2)

## Ensuite on applique les méthodes algorithmiques et on trouve que le vrai modèle est le suivant:
formula(stepAIC(fit2,direction="both")) 
formula(stepAIC(fit2, direction="backward"))# remarqué que ca donne la mm chose selon les deux méthodes...


## Notre modèle est donc :
modele <- lm(log(LOSS)~CLMAGE + ATTORNEY + MARITAL + SEATBELT + 
                    CLMAGE:ATTORNEY,data=data)
vif(modele) # meilleur modèle pour les VIFS

#Analyse vif est gucci

par(mfrow = c(1,2))
plot(data$CLMAGE,rstudent(modele),xlab="CLMAGE",main="Student")
qqnorm(rstudent(modele))
qqline(as.numeric(rstudent(modele))) ## pt  que les résidus sont pas normaux .. :/ 


pureErrorAnova(modele)



m <- summary(modele)
coef <- m$coefficients[,1]
std <- m$coefficients[,2]
n_p <- m$df[2]
coef[1] + std[1]*qt(0.975,n_p)
sapply(c(-1,1),function(i) coef + i*std*qt(0.975,n_p))

confidence <- as.data.frame(confint(modele))
confidence 
#
summary(modele)
anova(modele)
