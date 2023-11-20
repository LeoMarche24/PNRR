setwd("C:/Users/leoma/OneDrive/Documents/PNRR")

library(readr)
rm(list=ls())
graphics.off()

tosc <- read_delim("Toscana.Progetti.csv", 
                               delim = ";", escape_double = FALSE, col_types = cols(geo_map_lat = col_number(), 
                               geo_map_long = col_number()), trim_ws = TRUE)
tosc <- tosc[, 1:12]
length(na.omit((tosc$provincia)))
r <- is.na(tosc$missione)
toscana <- tosc[which(r == FALSE) ,]
r <- is.na(toscana$provincia)
toscana <- toscana[which(r == FALSE) ,]
r <- is.na(toscana$comune)
toscana <- toscana[which(r == FALSE) ,]
r <- is.na(toscana$importo_progetto)
toscana <- toscana[which(r == FALSE) ,]
save(toscana, file = 'dati toscana puliti')
####Inizio lavoro####
load('dati toscana puliti')

####Geostat####
library(sp)           ## Data management
library(lattice)      ## Data management
library(gstat)        ## Geostatistics
library(geoR)
plot(toscana$geo_map_lat, toscana$geo_map_long)
coordinates(toscana) <- c('geo_map_lat', 'geo_map_long')
svgm <- variogram(importo_progetto ~ 1, toscana)     #the notation "~ 1" stands for a single constant predictor
plot(svgm, main = 'Sample Variogram',pch=19)

#Prendo i valori popolazione
data <- read_delim("pop.tosc.csv", delim = ";", 
                   escape_double = FALSE, trim_ws = TRUE)
data$Territorio <- toupper(data$Territorio)

####
comun <- sort(unique(toscana$comune))
inv_per_com <- rep(0, length(comun))
ab_per_com <- rep(0, length(comun))

for (i in 1:length(comun))
{
  row <- which(toscana$comune==comun[i])
  for (j in 1:length(row))
  {
    inv_per_com[i] <- inv_per_com[i] + toscana$importo_progetto[row[j]]
  }
}

dati.t <- data.frame(comune = comun, investimento = inv_per_com, popolazione = as.numeric(data$Value))
plot(dati.t$popolazione, dati.t$investimento)

coef <- dati.t$investimento/dati.t$popolazione
View(data.frame(comun, coef))
length(which(coef>coef[which(comun=='Pistoia')]))
length(which(coef>coef[which(comun=='Pistoia')]))/length(comun)

prov <- NULL
for(i in comun)
{
  row <- which(toscana$comune==i)[1]
  prov <- c(prov, toscana$provincia[row])
}
prov <- as.character(prov)
prov <- as.factor(prov)

pr <- tapply(dati.t$investimento, prov, sum)
c(dati.t$investimento[which(toupper(dati.t$comune)==prov)]/pr[-6], 
  dati.t$investimento[which(dati.t$comune=="Massa")]/pr[6])

dati.t <- data.frame(dati.t, provincia = prov)
library(lme4)
library(insight)
library(caret)
fit.lm <- lmer(investimento ~ -1 + popolazione + (1|provincia), data=dati.t)
summary(fit.lm)
re <- get_variance_residual(fit.lm) 
ra <- get_variance_random(fit.lm) 
ra/(ra+re)
dotplot(ranef(fit.lm))

##################################################################
#Provincie ma con differenti missioni
#############################################################

inv_per_com <- data.frame(matrix(rep(0, length(comun)*6), nrow = length(comun), ncol = 6))
names(inv_per_com) <- c("M1", "M2", "M3", "M4", "M5", "M6")
ab_per_com <- rep(0, length(comun))

#Assegno gli investimenti provando a dividerli anche per Missione

for (i in 1:dim(toscana)[1])
{
  com <- toscana$comune[i]
  mis <- toscana$missione[i]
  inv_per_com[which(comun==com), mis] <- inv_per_com[which(comun==com), mis] + toscana$importo_progetto[i]
}

investimenti <- as.matrix(inv_per_com)

##############################################################

pr <- princomp(investimenti)
summary(pr)
pr$loadings
View(pr$scores)

pr <- princomp(scale(investimenti[, -3]))
summary(pr)
pr$loadings

plot(pr$scores[, 1:2], col=ifelse(comun=='Pistoia', 'red', 'black'), pch=16)

#Fare un anova mettendo in un unica colonna i vari investimenti con accanto la missione
tutti_inv <- NULL
for (i in 1:6)
{
  tutti_inv <- rbind(tutti_inv, cbind(investimenti[, i], rep(paste0("M", i), 273)))
}
tutti_inv <- data.frame(tutti_inv)
names(tutti_inv) <- c("inv", "missione")
summary(aov(inv ~ missione, data=tutti_inv))

summary(lm(inv ~ missione, data=tutti_inv))

colori = ifelse(prov=="Pistoia", 'red', 'black')

#Vedere la moda degli investimenti
moda <- NULL
for (i in 1:273)
{
  moda <- c(moda, which.max(investimenti[i ,]))
}
moda <- data.frame(comune = comun, moda=moda)

length(which(moda[, 2]==1))
length(which(moda[, 2]==2))
length(which(moda[, 2]==3))
length(which(moda[, 2]==4))
length(which(moda[, 2]==5))
length(which(moda[, 2]==6))

plot(dati.t$popolazione, investimenti[, 1])
data_sc <- cbind(dati.t$popolazione, investimenti[, 1])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(comun=="Pistoia")]))
length(which(coef_ang>coef_ang[which(comun=="Pistoia")]))/length(comun)

plot(dati.t$popolazione, investimenti[, 2], col=colori)
data_sc <- cbind(dati.t$popolazione, investimenti[, 2])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(comun=="Pistoia")]))
length(which(coef_ang>coef_ang[which(comun=="Pistoia")]))/length(comun)

plot(dati.t$popolazione, investimenti[, 4], col=colori)
data_sc <- cbind(dati.t$popolazione, investimenti[, 4])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(comun=="Pistoia")]))
length(which(coef_ang>coef_ang[which(comun=="Pistoia")]))/length(comun)
View(data.frame(comun, dati.t$popolazione, coef_ang))

plot(dati.t$popolazione, investimenti[, 5], col=colori)
data_sc <- cbind(dati.t$popolazione, investimenti[, 5])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="Pistoia")]))
length(which(coef_ang>coef_ang[which(prov=="Pistoia")]))/length(prov)

plot(dati.t$popolazione, investimenti[, 6], col=colori)
data_sc <- cbind(dati.t$popolazione, investimenti[, 6])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="Pistoia")]))
length(which(coef_ang>coef_ang[which(prov=="Pistoia")]))/length(prov)
View(data.frame(prov, coef_ang))
