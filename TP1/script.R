if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra,MASS,car)

# Analyse préliminaire des données ----
data <- read.csv("AutoBodyInjury.csv",sep = ";")
tail(sort(data$CLMAGE),5) 
tail(sort(data$LOSS),5)
str(data)
## Correction des erreurs dans MARITAL
correction <- gsub("ma.*","married",data$MARITAL)
correction <- gsub("si.*","single",correction)
data$MARITAL <- factor(correction)

## Correction des erreurs dans CLMSEX
correction <- gsub("male*","M",data$CLMSEX)
data$CLMSEX <- factor(correction)


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
boxplot(data$CLMAGE)# un age de 610 et des ages de 0 : peut être problématique
boxplot(data$LOSS) # Loss de 1m très éloignée des autres, va être à regarder
hist(data$CLMAGE) # Un log(1+age)(car il y a des valeurs de 0) pourrait être utilisé ,car fortement asymétrique
hist(data$LOSS) ## Un log(1+loss) pourrait être utilisé ,car fortement asymétrique

## Voici les 4 principaux modèles comportants toutes les interactions
(fit1 <- lm(I(log(LOSS))~I(log(1+CLMAGE)) + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
                + I(log(1+CLMAGE))*ATTORNEY + I(log(1+CLMAGE))*CLMSEX + I(log(1+CLMAGE))*MARITAL + I(log(1+CLMAGE))*CLMINSUR + I(log(1+CLMAGE))*SEATBELT
                + ATTORNEY*CLMSEX + ATTORNEY*MARITAL + ATTORNEY*CLMINSUR + ATTORNEY*SEATBELT
                + CLMSEX*MARITAL + CLMSEX*CLMINSUR + CLMSEX*SEATBELT
                + MARITAL*CLMINSUR + MARITAL*SEATBELT
                +CLMINSUR*SEATBELT,data=data))
(fit2 <- lm(LOSS~I(log(1+CLMAGE)) + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
            + I(log(1+CLMAGE))*ATTORNEY + I(log(1+CLMAGE))*CLMSEX + I(log(1+CLMAGE))*MARITAL + I(log(1+CLMAGE))*CLMINSUR + I(log(1+CLMAGE))*SEATBELT
            + ATTORNEY*CLMSEX + ATTORNEY*MARITAL + ATTORNEY*CLMINSUR + ATTORNEY*SEATBELT
            + CLMSEX*MARITAL + CLMSEX*CLMINSUR + CLMSEX*SEATBELT
            + MARITAL*CLMINSUR + MARITAL*SEATBELT
            +CLMINSUR*SEATBELT,data=data))
(fit3 <- lm(LOSS~CLMAGE + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
            + CLMAGE*ATTORNEY + CLMAGE*CLMSEX + CLMAGE*MARITAL + CLMAGE*CLMINSUR + CLMAGE*SEATBELT
            + ATTORNEY*CLMSEX + ATTORNEY*MARITAL + ATTORNEY*CLMINSUR + ATTORNEY*SEATBELT
            + CLMSEX*MARITAL + CLMSEX*CLMINSUR + CLMSEX*SEATBELT
            + MARITAL*CLMINSUR + MARITAL*SEATBELT
            +CLMINSUR*SEATBELT,data=data))
(fit4 <- lm(log(LOSS)~CLMAGE + ATTORNEY + CLMSEX + MARITAL + CLMINSUR + SEATBELT 
            + CLMAGE*ATTORNEY + CLMAGE*CLMSEX + CLMAGE*MARITAL + CLMAGE*CLMINSUR + CLMAGE*SEATBELT
            + ATTORNEY*CLMSEX + ATTORNEY*MARITAL + ATTORNEY*CLMINSUR + ATTORNEY*SEATBELT
            + CLMSEX*MARITAL + CLMSEX*CLMINSUR + CLMSEX*SEATBELT
            + MARITAL*CLMINSUR + MARITAL*SEATBELT
            +CLMINSUR*SEATBELT,data=data))



#l'interaction MARITAL*SEATBELT semble ne pas fonctionner
## Mon idée: faire la méthode forward ou backward sur le modèle complet plus haut pour obtenir le
## bon modèle à utiliser 
## On remarque que tant et aussi longtemps qu'on a des interactions, il y a multicolinéarité 
## donc le modèle semble ne pas avoir d'interactions


step1 <- stepAIC(fit1, direction="both")
step2 <- stepAIC(fit1, direction="forward")
step3 <- stepAIC(fit1, direction="backward")
debut_fit <- lm(I(log(LOSS)) ~ I(log(1 + CLMAGE)) + ATTORNEY + MARITAL + CLMINSUR + 
                  SEATBELT ,data=data) 

step1 <- stepAIC(debut_fit, direction="both")
step2 <- stepAIC(debut_fit, direction="forward")
step3 <- stepAIC(debut_fit, direction="backward")

test1 <- lm(formula(step1),data=data)
test2 <- lm(formula(step2),data=data)

summary(test1)
summary(test2)### regarder si ca vaut la peine de garder CLMINSUR

library(car)
vif(good_fit1) # meilleur modèle pour les VIFS
# les interactions causent des problèmes de VIFs


