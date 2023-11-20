setwd("C:/Users/leoma/OneDrive/Documents/PNRR")

library(readr)
rm(list=ls())
graphics.off()
data <- read_delim("AZ1.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)

data$`Finanziamento PNRR...9` <- data$`Finanziamento PNRR...9`/100
data$`Finanziamento PNRR...12`<-data$`Finanziamento PNRR...12`/100
data$`Finanziamento Totale`<-data$`Finanziamento Totale`/100
data$`Finanziamento Totale Pubblico`<-data$`Finanziamento Totale Pubblico`/100
data$`Finanziamento Totale Pubblico Netto`<-data$`Finanziamento Totale Pubblico Netto`/100
sum(na.omit(data$`Finanziamento PNRR...9`))
sum(na.omit(data$`Finanziamento PNRR...12`))
sum(na.omit(data$`Finanziamento Totale`))
sum(na.omit(data$`Finanziamento Totale Pubblico`))
sum(na.omit(data$`Finanziamento Totale Pubblico Netto`))
###############################################################
###Estrazione dati
###############################################################
names(data)
length(data$CUP...8)
length(na.omit(data$CUP...8))
length(na.omit(unique(data$CUP...8)))
length(na.omit(data$`Finanziamento PNRR`))
length(na.omit(unique(data$CUP...1)))
length(na.omit((data$CUP...1)))

sum(na.omit(data$`Finanziamento PNRR`))
sum(na.omit(data$`Finanziamento Totale Pubblico Netto`))
length(which(data$`Finanziamento PNRR`!=data$`Finanziamento Totale Pubblico Netto`))

#Come prima cosa bisogna andare a unire quei CUP che sono citati due volte, sommando
# l'investimento relativo a ogni riga con lo stesso codice

finanziamenti <- na.omit(data$CUP...8)
money <- as.numeric(na.omit(data$`Finanziamento Totale`))

a<-0
i <- 1
while (!is.na(finanziamenti[i]))
{
  j <- i+1
  while (!is.na(finanziamenti[j+1]))
  {
    if(finanziamenti[i]==finanziamenti[j])
    {
      finanziamenti <- finanziamenti[-j]
      money[i] <- money[i]+money[j]
      money <- money[-j]
      j <- j-1
    }
    j <- j+1
  }
  i <- i+1
}

da <- cbind(finanziamenti,money)

save(da, file = "sorted cup")
load("sorted cup")

finanziamenti <- da[, 1]
money <- as.numeric(da[, 2])
rm(da)

#Adesso CUP e rispettivi finanziamenti sono ordinati e unici, questo è il punto di partenza per l'analisi

pop <- read.csv("pop.csv", sep = ";")
pop <- pop[, 1:4]
pop <- pop[, -3]
names(pop) <- c("provincie", "abitanti", "partito")
pop <- pop[1:107 ,]
partito <- as.factor(pop[, 3])

prov <- sort(unique(data$`Descrizione Provincia`))
prov <- prov[-99]
prov <- prov[-3]
###########################################
#Ciclo for con cui sono stati estratti
##############################################
inv_per_prov <- rep(0, length(prov))
ab_per_prov <- rep(0, length(prov))

for (i in 1:length(finanziamenti))
{
  row <- which(data$CUP...1 == finanziamenti[i])
  provincia <- data$`Descrizione Provincia`[row]
  for (j in 1:length(provincia))
  {
    which_prov <- which(prov==provincia[j])
    inv_per_prov[which_prov] <- inv_per_prov[which_prov] + money[i]/length(provincia)
  }
}
ab_per_prov <- pop[, 2]

dati_ini <- data.frame(provincia=prov, abitanti = ab_per_prov, investimenti=inv_per_prov)
######################################################

save(dati_ini, file = "Dati totali per provincia")
load("Dati totali per provincia")
dati_ini$investimenti <- dati_ini$investimenti/100

#dati_ini rappresenta la prima analisi possibile, con la matrice che rappresenta per ogni provincia
# i rispettivi abitanti e l'ammontare di denaro investito dal pnrr.

attach((dati_ini))
ab_per_prov <- as.numeric(abitanti)
inv_per_prov <- as.numeric(investimenti)
detach((dati_ini))

View(cbind(prov, inv_per_prov/ab_per_prov))

###########################################################
###Analisi per provincie sul totale degli investimenti
##############################################################
plot(ab_per_prov, inv_per_prov)

pt <- which(prov=="PISTOIA")
colori = ifelse(prov=="PISTOIA", 'red', 'black')

plot(ab_per_prov, inv_per_prov, col=colori, lty=2, pch=16)
points(ab_per_prov[pt], inv_per_prov[pt], col='red', pch=16)

#Primo grafico possibile, qualitativamente Pistoia è sotto la media e non fa benissimo

model <- lm(inv_per_prov ~ ab_per_prov)
summary(model)
#Ad ogni abitante in più è associato un investimento in più pari a 606.2€ in più

##prova a ottenere un indice di inv per abitante

#PRIMO MODO
data_sc <- dati_ini[, 2:3]
coef_ang <- data_sc[, 2]/data_sc[, 1]

length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)

coef_ang[which(prov=="PISTOIA")]
View(data.frame(prov, coef_ang))

summary(aov(inv_per_prov ~ partito))
summary(aov(coef_ang ~ partito))

plot(partito, coef_ang)

########Approccio lmm####
reg <- NULL
for(i in prov)
{
  row <- which(data[, 3]==i)[1]
  reg <- c(reg, data[row, 2])
}

reg <- as.character(reg)
reg <- as.factor(reg)

dati <- data.frame(provincia=prov, regione=reg, investimenti=inv_per_prov, abitanti=ab_per_prov)
fit <- lm(investimenti ~ abitanti, data=dati)
summary(fit)
x11()
par(mfrow=c(2,2))
plot(fit)

plot(dati$abitanti)
inx <- which(dati$abitanti<2e+6)
plot(dati$investimenti)
abline(h=1.5e+9)
inx <- which(dati$investimenti<1.5e+9)
dati1 <- dati[inx ,]
fit.wo <- lm(investimenti ~ -1 + abitanti, data=dati1)
summary(fit.wo)
x11()
plot(dati1$abitanti, dati1$investimenti)

x11()
par(mfrow=c(2,2))
plot(fit.wo)

x11()
plot(dati1$investimenti, fit.wo$residuals, col=dati$regione)

library(lme4)
library(insight)
library(caret)
fit.lm <- lmer(investimenti ~ abitanti + (1|regione), data=dati)
summary(fit.lm)
re <- get_variance_residual(fit.lm) 
ra <- get_variance_random(fit.lm) 
ra/(ra+re)
x11()
dotplot(ranef(fit.lm))
x11()
View(t(sort(tapply(dati$investimenti/dati$abitanti, dati$regione, sum))))
data <- sort(tapply(dati$investimenti/dati$abitanti, dati$regione, sum))

x11()
plot(dati1$abitanti, fit.wo$residuals)

dati.log <- dati
dati.log$abitanti <- log(dati.log$abitanti)
dati.log$investimenti <- log(dati.log$investimenti)

x11()
plot(dati.log$abitanti, dati.log$investimenti)

fit.l <- lm(investimenti ~ abitanti, data=dati.log)
summary(fit.l)
x11()
par(mfrow=c(2,2))
plot(fit.l)

fit.lm.l <- lmer(investimenti ~ abitanti + (1|regione), data=dati.log)
summary(fit.lm.l)
re <- get_variance_residual(fit.lm.l) 
ra <- get_variance_random(fit.lm.l) 
ra/(ra+re)
x11()
dotplot(ranef(fit.lm.l))

shapiro.test(residuals(fit.lm.l))

x11()
plot(dati.log$abitanti, residuals(fit.lm.l), col=dati.log$regione)

#######################################################
#Prova con comuni  ---> da rifare
###########################################################
comuni <- read_delim("comuni.csv", delim = ";", 
                         escape_double = FALSE, na = "0", trim_ws = TRUE, )

comuni <- cbind(nome=(comuni[, 1]), abitanti=comuni[, 5])

comuni[3302, 1] <-"H NE"
comuni[3385, 1] <-"JOVEN AN"

comuni[, 1] <- toupper(comuni[, 1])

#Ciclo for per estrarre i finanziamenti per ogni comune
#######################################################

comun <- comuni[, 1]

inv_per_com <- rep(0, length(comun))

for (i in 1:length(finanziamenti))
{
  row <- which(data$CUP...1 == finanziamenti[i])
  com <- data$`Descrizione Comune`[row]
  for(j in 1:length(com))
  {
    which_com <- which(comun==com[j])
    inv_per_com[which_com] <- inv_per_com[which_com] + money[i]/length(com)
  }
}

ab <- comuni[, 2]

comuni_init <- data.frame(comun, ab, inv_per_com)
View(comuni_init[which(comuni_init$inv_per_com==0) ,])
##########################################################

save(comuni_init, file="Comuni abitanti e investimenti totali")

#Prendo per buona questa distribuzione di investimenti
load("Comuni abitanti e investimenti totali")

lines <- which(comuni_init$inv_per_com>0)

comuni_init <- comuni_init[lines ,]

plot(comuni_init$ab, comuni_init$inv_per_com)

lines <- which(comuni_init$ab < 500000)

pt <- which(comuni_init$comun=="PISTOIA")

x11()
plot(comuni_init$ab[lines], comuni_init$inv_per_com[lines])
points(comuni_init$ab[pt], comuni_init$inv_per_com[pt], col='red', pch=18)

range <- which(comuni_init$ab>50000 & comuni_init$ab<150000)

comuni_init <- comuni_init[range ,]

plot(comuni_init$ab, comuni_init$inv_per_com)

pt <- which(comuni_init$comun=="PISTOIA")
points(comuni_init$ab[pt], comuni_init$inv_per_com[pt], col='red', pch=18)

##prova a ottenere un indice di inv per abitante

data_sc <- scale(comuni_init[, 2:3], center=TRUE)
data_sc <- comuni_init[, 2:3]
lines1<-which(is.na(data_sc[,1]))
data_sc <- data_sc[-which(is.na(data_sc[,1])) ,]
data_sc[, 1] <-data_sc[, 1]+abs(min(data_sc[, 1])) 
data_sc[, 2] <-data_sc[, 2]+abs(min(data_sc[, 2]))
lines <- which(data_sc[, 1]>0 & data_sc[, 2]>0)
data_sc <- data_sc[lines ,]
comuni_sc <- comun[-lines1]
comuni_sc <- comuni_sc[lines]
pt <- which(comuni_sc=="PISTOIA")

data_sc <- comuni_init[2:3]
pt <- which(comuni_init$comun[lines]=="PISTOIA")

coef_ang <- data_sc[, 2]/data_sc[, 1]

dataframes <- data.frame(comuni_init$comun[lines], coef_ang)
dataframes <- dataframes[which(comuni_init$inv_per_com>0) ,]

length(which(coef_ang>coef_ang[pt]))
length(which(coef_ang>coef_ang[pt]))/length(comuni_init$comun)

###Obbiettivo : vedere quanto gli investimenti del capoluogo di provincia influenzano quelli dei comuni

#Primo indice:percentuale su tutti gli investimenti (si può fare solo su alcune provincie -> quelle che hanno 
# per capoluogo un comune e che si chiamano così)
prov_prova <- prov
prov_prova <- prov_prova[-102]
prov_prova <- prov_prova[-99]
prov_prova <- prov_prova[-89]
prov_prova <- prov_prova[-67]
prov_prova[57] <- "MONZA"
prov_prova <- prov_prova[-52]
prov_prova <- prov_prova[-35]
prov_prova[15] <- "BOLZANO"
prov_prova <- prov_prova[-9]

inv_comunali <- comuni_init[, 3]
new_data <- NULL
for(i in prov_prova)
{
  names_new <- sort(unique(data$`Descrizione Comune`[which(data$`Descrizione Provincia`==i)]))
  for (j in 1:length(names_new))
  inv_totali <- inv_totali + inv_comunali[which(comuni_init[, 1] == names_new[j])]
}

for(i in prov_prova)
{
  perc <- inv_comunali[which(comuni_init[, 1] == i)]/inv_per_prov[prov==i]
  new_data <- c(new_data, perc)
}
new_data <- rbind(prov_prova, new_data)
new_data <- data.frame(t(new_data))
mean(as.numeric(new_data[, 2]))

##################################################################
#Provincie ma con differenti missioni
#############################################################

inv_per_prov <- data.frame(matrix(rep(0, length(prov)*6), nrow = length(prov), ncol = 6))
names(inv_per_prov) <- c("M1", "M2", "M3", "M4", "M5", "M6")
ab_per_prov <- rep(0, length(prov))

#Assegno gli investimenti provando a dividerli anche per Missione

for (i in 1:length(finanziamenti))
{
  cup <- finanziamenti[i]
  row <- which(data$CUP...1 == cup)
  provincia <- data$`Descrizione Provincia`[row]
  rowmis <- which(data$CUP...8 == cup)[1]
  mis <- which(names(inv_per_prov)==data$Missione[rowmis])[1]
  for (j in 1:length(provincia))
  {
    which_prov <- which(prov==provincia[j])
    inv_per_prov[which_prov, mis] <- inv_per_prov[which_prov, mis] + money[i]/length(provincia)
  }
}
popolazione <- pop[1:107 ,]
ab_per_prov <- as.numeric(popolazione[, 2])

investimenti <- as.matrix(inv_per_prov)

row.names(investimenti) <- prov
##############################################################

save(investimenti, file = "Risult")
load("Risult")
investimenti <- investimenti/100

boxplot(investimenti)
abline(h=1e+9)
out <- which(investimenti < 1e+9)
box <- investimenti[-out ,]
boxplot(box)

pr <- princomp(investimenti)
summary(pr)
pr$loadings
View(pr$scores)

pr <- princomp(scale(investimenti[, -3]))
summary(pr)
pr$loadings

View(cbind(prov, ifelse(pr$scores[, 2]<0, "school", "green")))

plot(pr$scores[, 1:2], col=ifelse(prov=='PISTOIA', 'red', 'black'), pch=16)
points(pr$scores[71, 1], pr$scores[71, 2], col='red', pch=16)

#Fare un anova mettendo in un unica colonna i vari investimenti con accanto la missione
tutti_inv <- NULL
for (i in 1:6)
{
  tutti_inv <- rbind(tutti_inv, cbind(investimenti[, i], rep(paste0("M", i), 107)))
}
tutti_inv <- data.frame(tutti_inv)
names(tutti_inv) <- c("inv", "missione")
row.names(tutti_inv) <- 1:642
summary(aov(inv ~ missione, data=tutti_inv))
prova_anova <- tutti_inv[-((2*107+1):(3*107)) ,]
prova_anova <- prova_anova[-((3*107+1):(4*107)) ,]
summary(aov(inv ~ missione, data=prova_anova))

summary(lm(inv ~ missione, data=tutti_inv))

colori = ifelse(prov=="PISTOIA", 'red', 'black')

model <- manova(investimenti ~ pop$partito)
summary(model)

#Vedere la moda degli investimenti
moda <- NULL
for (i in 1:107)
{
  moda <- c(moda, which.max(investimenti[i ,]))
}
moda <- data.frame(provincia=prov, moda=moda)
  
length(which(moda[, 2]==1))
length(which(moda[, 2]==2))
length(which(moda[, 2]==3))
length(which(moda[, 2]==4))
length(which(moda[, 2]==5))
length(which(moda[, 2]==6))

plot(ab_per_prov, investimenti[, 1])
data_sc <- cbind(ab_per_prov, investimenti[, 1])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)


plot(ab_per_prov, investimenti[, 2], col=colori)
data_sc <- cbind(ab_per_prov, investimenti[, 2])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)

plot(ab_per_prov, investimenti[, 4], col=colori)
data_sc <- cbind(ab_per_prov, investimenti[, 4])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)
View(data.frame(prov, ab_per_prov, coef_ang))

plot(ab_per_prov, investimenti[, 5], col=colori)
data_sc <- cbind(ab_per_prov, investimenti[, 5])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)

plot(ab_per_prov, investimenti[, 6], col=colori)
data_sc <- cbind(ab_per_prov, investimenti[, 6])
coef_ang <- data_sc[, 2]/data_sc[, 1]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)
View(data.frame(prov, coef_ang))

################################################################
###Vedere le percentuali di investimenti nelle varie missioni
##################################################################

percentuali <- investimenti
for (j in 1:107)
{
  for(i in 1:6)
  {
    percentuali[j, i] <- investimenti[j, i]/sum(investimenti[j ,])
  }
}
View(cbind(prov, percentuali))
tutti_inv <- NULL
for (i in 1:6)
{
  tutti_inv <- rbind(tutti_inv, cbind(percentuali[, i], rep(paste0("M", i), 107)))
}
prova_anova <- tutti_inv[-((2*107+1):(3*107)) ,]
prova_anova <- data.frame(prova_anova[-((3*107+1):(4*107)) ,])
names(prova_anova) <- c("inv", "missione")
summary(aov(inv ~ missione, data=data.frame(prova_anova)))


percentuali <- percentuali[, -3]

boxplot(percentuali)
ab_per_prov <- pop[, 2]
colori <- ifelse(prov=='PISTOIA', 'red', 'green')

plot(ab_per_prov, percentuali[, 1], col=colori)
data_sc <- cbind(ab_per_prov, percentuali[, 1])
cor(data_sc)
coef_ang <- data_sc[, 2]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)


plot(ab_per_prov, percentuali[, 2], col=colori)
data_sc <- cbind(ab_per_prov, percentuali[, 2])
cor(data_sc)
coef_ang <- data_sc[, 2]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)

plot(ab_per_prov, percentuali[, 3], col=colori)
data_sc <- cbind(ab_per_prov, percentuali[, 3])
cor(data_sc)
coef_ang <- data_sc[, 2]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)
View(data.frame(prov, ab_per_prov, coef_ang))

plot(ab_per_prov, percentuali[, 4], col=colori)
data_sc <- cbind(ab_per_prov, percentuali[, 4])
cor(data_sc)
coef_ang <- data_sc[, 2]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)

plot(ab_per_prov, percentuali[, 5], col=colori)
data_sc <- cbind(ab_per_prov, percentuali[, 5])
cor(data_sc)
coef_ang <- data_sc[, 2]
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))
length(which(coef_ang>coef_ang[which(prov=="PISTOIA")]))/length(prov)
View(data.frame(prov, coef_ang))

##Prova a vedere l'influenza della politica nella natura delle percentuali

plot(partito, percentuali[, 1])
lines <- 1:107
perc1 <- percentuali[lines ,]
perc1 <- perc1[, 1]
summary(aov(perc1 ~ partito[lines]))

plot(partito, percentuali[, 2])
lines <- which(partito != "altro")
perc2 <- percentuali[lines ,]
perc2 <- perc2[, 2]
summary(aov(perc2 ~ partito[lines]))

plot(partito, percentuali[, 3])
perc4 <- percentuali[, 4]
summary(aov(perc4 ~ partito))
tapply(perc4, INDEX = partito, FUN = mean)
lines <- which(partito != "altro")
perc4 <- perc4[lines]
summary(aov(perc4 ~ partito[lines]))

plot(partito, percentuali[, 4])
lines <- 1:107
perc5 <- percentuali[, 5]
summary(aov(perc5 ~ partito))

plot(partito, percentuali[, 4])
lines <- 1:107
perc6 <- percentuali[lines ,]
perc6 <- perc6[, 6]
summary(aov(perc6 ~ partito[lines]))
tapply(perc6, INDEX = partito, FUN = mean)

####Indici di centralità di investimento####
money <- rep(0, length(prov))
for (i in 1:length(prov))
{
  row <- which(data$`Descrizione Comune` == prov[i])
  cup <- data$CUP...1[row]
  if (length(cup>0))
  {
    for (j in 1:length(cup))
    {
      money[i] <- money[i] + as.numeric(data$`Finanziamento PNRR...9`[which(data$CUP...8==cup[j])])
    }
  }
}

money/dati_ini$investimenti

perc.prov <- data.frame(provincia = prov, percentuale = money/dati_ini$investimenti)
hist(perc.prov$percentuale)
View(perc.prov)
