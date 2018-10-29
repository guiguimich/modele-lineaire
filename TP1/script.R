if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra,MASS,car)


# Analyse préliminaire des données ----
data <- read.csv("AutoBodyInjury.csv",sep = ";")
tail(sort(data$CLMAGE),5) 
tail(sort(data$LOSS),5)
str(data)
summary(data[,7:8])
## Correction des erreurs dans MARITAL
boxplot(data$LOSS)
p7 <- ggplot(data, aes(x = Month, y = Ozone)) +
    geom_boxplot()
par(mfrow = c(1,2))
t <- head(sort(data$LOSS),-1)
boxplot(t)
hist(log(t))
hist(log(data$LOSS))
correction <- gsub("ma.*","married",data$MARITAL)
correction <- gsub("si.*","single",correction)
data$MARITAL <- factor(correction)

## Correction des erreurs dans CLMSEX
correction <- gsub("male*","M",data$CLMSEX)
data$CLMSEX <- factor(correction)


## Correction des erreurs dans CLMAGE (retrait de la ligne de l'âge 610)
data <- data[-66,]


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


# Modèle ----
summary(data$LOSS)

boxplot(data$CLMAGE)# un age de 61 : peut être problématique
boxplot(data$LOSS) # Loss de 1m très éloignée des autres, va être à regarder
hist(log(1+data$CLMAGE)) # Un log(1+age)(car il y a des valeurs de 0) pourrait être utilisé ,car fortement asymétrique
hist(log(data$LOSS)) ## Un log(1+loss) pourrait être utilisé ,car fortement asymétrique

## Développement pour les transformations à faire : 

which(data$LOSS > 1000)

#data[-859,] peut être a enlevé à cause de la haute valeur
#### Analyse des résidus
##test pour respecter les hypothèses
temp <- lm(LOSS ~ CLMAGE ,data=data)
plot(data$CLMAGE,data$LOSS)
plot(temp$fitted.values,rstudent(temp))
plot(data$CLMAGE,rstudent(temp))
## Il y a donc des problèmes avec les postulats de base, il faut donc faire une transformation.

## Insérer ici la méthode box-Cox


# On trouve que la meilleur transformation est la logarithme:
# Et on voit que les postulats sont respectés
temp <- lm(log(LOSS) ~ CLMAGE ,data=data)
plot(data$CLMAGE,log(data$LOSS))
plot(temp$fitted.values,rstudent(temp))
plot(data$CLMAGE,rstudent(temp))



## Mon idée: faire la méthode forward ou backward sur le modèle complet plus haut pour obtenir le
## bon modèle à utiliser 
## On remarque que tant et aussi longtemps qu'on a des interactions, il y a multicolinéarité 
## donc le modèle semble ne pas avoir d'interactions
#Toutes les interactions possibles...
fit <- lm(log(LOSS)~CLMAGE + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
            + CLMAGE*ATTORNEY + CLMAGE*CLMSEX + CLMAGE*MARITAL + CLMAGE*CLMINSUR + CLMAGE*SEATBELT
            + ATTORNEY*CLMSEX + ATTORNEY*MARITAL + ATTORNEY*CLMINSUR + ATTORNEY*SEATBELT
            + CLMSEX*MARITAL + CLMSEX*CLMINSUR + CLMSEX*SEATBELT
            + MARITAL*CLMINSUR + MARITAL*SEATBELT
            +CLMINSUR*SEATBELT,data=data)
step1 <- stepAIC(fit, direction="both")
step2 <- stepAIC(fit, direction="backward")


formula(step1) # mm affaire
formula(step2)# On enleve des variables jusqu'à ce que les vifs soient bons...


# Aorès avoir fait la méthode algorithmique on trouve que le meilleur modèle avec des vifs bons est 
#suivant :
analyse_vif <- lm(log(LOSS)~CLMAGE + ATTORNEY + MARITAL + CLMINSUR + SEATBELT + 
                    CLMAGE:ATTORNEY,data=data)

vif(analyse_vif) # meilleur modèle pour les VIFS
# les interactions causent des problèmes de VIFs
step4 <- stepAIC(analyse_vif, direction="both")
step5 <- stepAIC(analyse_vif, direction="backward")

# donc analyse vif est gucci
