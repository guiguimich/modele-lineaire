if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2,gridExtra,MASS,car)


# Analyse préliminaire des données ----
data <- read.csv("AutoBodyInjury.csv",sep = ";")

summary(data[,7:8])
## Correction des erreurs dans MARITAL
correction <- gsub("ma.*","married",data$MARITAL)
correction <- gsub("si.*","single",correction)
data$MARITAL <- factor(correction)

## Correction des erreurs dans CLMSEX
correction <- gsub("male*","M",data$CLMSEX)
data$CLMSEX <- factor(correction)

## Correction des erreurs dans CLMAGE
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
bcox <- lm(LOSS ~ CLMAGE,data = data) # On a omis les variables qualitatives pour en faire un modèle simlpe
# avec les variables qualitatives incluts dans B0
boxcox(bcox) # Boxcox indique lambda = 0 à 95% -> la transformation a Y à faire est log(Y)

modele1 <- lm(I(log(LOSS)) ~ CLMAGE + ATTORNEY  + SEATBELT + MARITAL + CLMSEX + CLMINSUR,data = data)
par(mfrow = c(1,1))
plot(data$CLMAGE,rstudent(modele1),xlab="Me lost",main="Please, mean = 0 ")
summary(modele1)
vif(modele1)
AIC_m1 <- stepAIC(modele1, direction="both")
modele1_mod <- lm(formula(AIC_m1),data = data)
summary(modele1_mod)
plot(data$CLMAGE,rstudent(modele1_mod),xlab="CLMAGE",main="Résidus studentisé en fonction de CLMAGE")
qqnorm(rstudent(modele1_mod))
qqline(as.numeric(rstudent(modele1_mod)))

modele2 <- lm(log(LOSS) ~ CLMAGE + ATTORNEY + MARITAL + SEATBELT + CLMAGE:ATTORNEY,data = data)
par(mfrow = c(1,1))
summary(modele2)
vif(modele2)
AIC_m2 <- stepAIC(modele2, direction="both")
modele2_mod <- lm(formula(AIC_m2),data = data)
summary(modele2_mod)
plot(data$CLMAGE,rstudent(modele2_mod),xlab="CLMAGE",main="Résidus studentisé en fonction de CLMAGE")
qqnorm(rstudent(modele2_mod))
qqline(as.numeric(rstudent(modele2_mod)))


verif <- function(modele){
    par(mfrow = c(1,2))
    plot(data$CLMAGE,rstudent(modele),xlab="CLMAGE",main="Student")
    qqnorm(rstudent(modele))
    qqline(as.numeric(rstudent(modele)))
    summary(modele)
}
verif(lm(log(LOSS) ~ CLMAGE + ATTORNEY + MARITAL + SEATBELT + CLMAGE:ATTORNEY,data = data))
