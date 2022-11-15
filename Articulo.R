suppressPackageStartupMessages({
  suppressWarnings({
    suppressMessages({
      library(lmtest)
      library(lrmest)
      library(corrplot)
      library(tseries)
      library(nortest)
      library(car)
      library(sandwich)
      library(lattice)
      library(viridisLite)
      library(leaps)
      library(agricolae)
      library(faraway)
      library(stargazer)
      library(dplyr)
    })
  })
})

## Cargar datos
setwd("C:/Users/jochoa/Desktop/Bases de datos Doctorado/Lima 2022-2") # dependiendo de quien trabaja
mesh2 <- read.csv("mesh.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "unknown")                           # lectura del archivo eventualmente read.table con sus opciones

#sacar de la data los valores 0
mesh2[mesh2==0] <- NA                                     # cambiar ceros en NA
mesh1         <- mesh2[complete.cases(mesh2),]            # tirar datos incompletos


attach(mesh1)

# chequeo de los datos iniciales
print(head(mesh1, 10))                                  # encabezamiento
print(str(mesh1))                                       # structura del data.frame
print(summary(mesh1))                                   # estadísticas somárias

# Conocer las variables categorizantes


table(as.factor(MXN))

table(as.factor(TXDURATION))

table(as.factor(M_STEP))

table(as.factor(M_PACKET))

table(as.factor(INTERFACES))

table(as.factor(PDF))

table(as.factor(TX))

table(as.factor(TXX))

table(as.factor(L4))
plot(FAILAVG,METRIC)
        # diagrama de dispersión entre caracteres cualitativos

boxplot(METRIC~MXN)           # diagramas de caja y bigotes entre un caracter cuantitativo y cualitativos
boxplot(METRIC~TXDURATION) 
boxplot(METRIC~M_STEP) 
boxplot(METRIC~M_PACKET) 
boxplot(METRIC~INTERFACES) 
boxplot(METRIC~PDF) 
boxplot(METRIC~TX) 
boxplot(METRIC~TXX) 

plot(mesh1)                             # gráfico cruzando todas las variables

#Definimos regresores como factor

M_STEP=factor(M_STEP)
M_PACKET=factor(M_PACKET)
INTERFACES=factor(INTERFACES)
PDF=factor(PDF)
MXN=factor(MXN)
TXDURATION=factor(TXDURATION)

L4=factor(L4)
TXX=factor(TXX)


##Modelo lineal 1 IEEE 802.11s HWMP Original
lmesh1  <- lm(log(METRIC)~FAILAVG, data=mesh1)
summary(lmesh1)
anova(lmesh1)
par(mfcol = c(2, 2))
plot(lmesh1)

boxplot(lmesh1[['residuals']],main='Boxplot: Residuals',ylab='residual value')


#MODELO 2
lmesh2=lm(log(METRIC)~FAILAVG+I(FAILAVG^2)+I(FAILAVG^3)+MXN+M_STEP+M_PACKET+INTERFACES+L4, data=mesh1)
summary(lmesh2)
anova(lmesh2)
par(mfcol = c(2, 2))
plot(lmesh2)

boxplot(lmesh2$residuals,main='Boxplot: Residuals',ylab='residual value')


##Resumen de los modelos 

stargazer(lmesh1,lmesh2, type="text", df=FALSE)

anova(lmesh1,lmesh2)

## Forward

lm20 <- lm(log(METRIC)~1,data=mesh1)
lm2f <- step(lm20, scope=list(lower=lm20, upper=lmesh2), direction="forward"); 
summary(lm2f)
lm2f$anova

modback<-stepAIC(lmesh2, trace=TRUE, direction="backward")
modback$anova
summary(modback)

modforw <- stepAIC(lm20, trace=FALSE, direction="forward", scope=list(lower=lm20, upper=lmesh2));
modforw$anova
summary(modforw)



par(mfrow=c(1, 2))
plot(modback, main="Backward", pch=19, cex=1, which=1)
plot(modforw, main="Forward", pch=19, cex=1, which=1)


#Rehacemos todo lm con comp

COMP=1/(1-FAILAVG)

lmesh3=lm(METRIC~COMP+I(COMP^2)+I(COMP^3)+MXN+M_STEP+M_PACKET+INTERFACES+L4, data=mesh1)
summary(lmesh3)
anova(lmesh3)
par(mfcol = c(2, 2))
plot(lmesh3)




##Modelos generalizados

setwd("C:/Users/jochoa/Desktop/Bases de datos Doctorado/Lima 2022-2") # dependiendo de quien trabaja
mesh3 <- read.csv("mesh_udp_2.csv", header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", encoding = "unknown")                           # lectura del archivo eventualmente read.table con sus opciones

attach(mesh3)
print(head(mesh3, 10))                                  # encabezamiento
print(str(mesh3))                                       # structura del data.frame
print(summary(mesh3))   



#Corremos el glm con distribucion POISSON

glmesh1=glm(formula=METRIC~ï..FAILAVG+MXN+M_STEP+M_PACKET+INTERFACES+TXDURATION+PDF,family=poisson(link="log"),data=mesh3)
summary(glmesh1)

par(mfcol = c(2, 2))
plot(glmesh1)


glmesh2=glm(formula=TXX~METRIC+ï..FAILAVG+MXN+M_STEP+M_PACKET+INTERFACES+TXDURATION,family=binomial(link="logit"),data=mesh3)
summary(glmesh2)
par(mfcol = c(2, 2))
plot(glmesh2)
anova(glmesh6,test = "Chi")

## ANCOVA

M_STEP=factor(M_STEP)
M_PACKET=factor(M_PACKET)
INTERFACES=factor(INTERFACES)
PDF=factor(PDF)
MXN=factor(MXN)
TXDURATION=factor(TXDURATION)

L4=factor(L4)
TXX=factor(TXX)

leveneTest(mesh3$TXX, mesh3$MXN, center=median)
leveneTest(mesh3$TXX, mesh3$ï..FAILAVG, center=median)
leveneTest(mesh3$TXX, mesh3$M_STEP, center=median)
leveneTest(mesh3$TXX, mesh3$INTERFACES, center=median)
leveneTest(mesh3$TXX, mesh3$TXDURATION, center=median)
leveneTest(mesh3$TXX, mesh3$M_PACKET, center=median)


##PASO 2

lmeshI1=aov(METRIC~M_PACKET,data=mesh3)
summary(lmeshI1)

lmeshI2=aov(METRIC~MXN,data=mesh3)
summary(lmeshI2)

lmeshI3=aov(METRIC~M_STEP,data=mesh3)
summary(lmeshI3)

lmeshI4=aov(METRIC~INTERFACES,data=mesh3)
summary(lmeshI4)

lmeshI5=aov(METRIC~TXDURATION,data=mesh3)
summary(lmeshI5)

lmeshI=aov(METRIC~MXN+M_STEP+M_PACKET+INTERFACES+TXDURATION,data=mesh3)
summary(lmeshI)

#PASO 3: Homogeneidad de las pendientes de regresión 

lmeshH1=aov(TXX~METRIC*MXN,data=mesh3) ##Ancova con M_STEP
summary(lmeshH1)

lmeshH2=aov(TXX~METRIC*M_PACKET,data=mesh3) ##Ancova con M_STEP
summary(lmeshH2)

lmeshH3=aov(TXX~METRIC*M_STEP,data=mesh3) ##Ancova con M_STEP
summary(lmeshH3)

lmeshH4=aov(TXX~METRIC*INTERFACES,data=mesh3) ##Ancova con M_STEP
summary(lmeshH4)

lmeshH5=aov(TXX~METRIC*TXDURATION,data=mesh3) ##Ancova con M_STEP
summary(lmeshH5)

lmeshH5=aov(TXX~METRIC*ï..FAILAVG,data=mesh3) ##Ancova con M_STEP
summary(lmeshH5)
