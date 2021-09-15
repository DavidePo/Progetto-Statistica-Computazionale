setwd('F:/Università/Lovaglio/stat_comp/progetto')
data <- read.csv('crime.txt', header = F)
colnames(data) <- c('communityname', 'State', 'countyCode', 'communityCode', 'fold', 'pop', 'perHoush', 'pctBlack', 'pctWhite', 'pctAsian', 
                    'pctHisp', 'pct12-21', 'pct12-29', 'pct16-24', 'pct65up', 'persUrban', 'pctUrban', 'medIncome', 'pctWwage', 'pctWfarm', 
                    'pctWdiv', 'pctWsocsec', 'pctPubAsst', 'pctRetire', 'medFamIncome', 'perCapInc', 'whitePerCap', 'blackPerCap', 'NAperCap', 
                    'asianPerCap', 'otherPerCap', 'hispPerCap', 'persPoverty', 'pctPoverty', 'pctLowEdu', 'pctNotHSgrad', 'pctCollGrad', 'pctUnemploy', 
                    'pctEmploy', 'pctEmployMfg', 'pctEmployProfServ', 'pctOccupManu', 'pctOccupMgmt', 'pctMaleDivorc', 'pctMaleNevMar', 'pctFemDivorc', 
                    'pctAllDivorc', 'persPerFam', 'pct2Par', 'pctKids2Par', 'pctKids-4w2Par', 'pct12-17w2Par', 'pctWorkMom-6', 'pctWorkMom-18', 
                    'kidsBornNevrMarr', 'pctKidsBornNevrMarr', 'numForeignBorn', 'pctFgnImmig-3', 'pctFgnImmig-5', 'pctFgnImmig-8', 'pctFgnImmig-10', 
                    'pctImmig-3', 'pctImmig-5', 'pctImmig-8', 'pctImmig-10', 'pctSpeakOnlyEng', 'pctNotSpeakEng', 'pctLargHousFam', 'pctLargHous', 
                    'persPerOccupHous', 'persPerOwnOccup', 'persPerRenterOccup', 'pctPersOwnOccup', 'pctPopDenseHous', 'pctSmallHousUnits', 'medNumBedrm', 
                    'houseVacant', 'pctHousOccup', 'pctHousOwnerOccup', 'pctVacantBoarded', 'pctVacant6up', 'medYrHousBuilt', 'pctHousWOphone', 
                    'pctHousWOplumb', 'ownHousLowQ', 'ownHousMed', 'ownHousUperQ', 'ownHousQrange', 'rentLowQ', 'rentMed', 'rentUpperQ', 'rentQrange', 
                    'medGrossRent', 'medRentpctHousInc', 'medOwnCostpct', 'medOwnCostPctWO', 'persEmergShelt', 'persHomeless', 'pctForeignBorn', 
                    'pctBornStateResid', 'pctSameHouse-5', 'pctSameCounty-5', 'pctSameState-5', 'numPolice', 'policePerPop', 'policeField', 
                    'policeFieldPerPop', 'policeCalls', 'policCallPerPop', 'policCallPerOffic', 'policePerPop2', 'racialMatch', 'pctPolicWhite', 
                    'pctPolicBlack', 'pctPolicHisp', 'pctPolicAsian', 'pctPolicMinority', 'officDrugUnits', 'numDiffDrugsSeiz', 'policAveOT', 'landArea', 
                    'popDensity', 'pctUsePubTrans', 'policCarsAvail', 'policOperBudget', 'pctPolicPatrol', 'gangUnit', 'pctOfficDrugUnit', 
                    'policBudgetPerPop', 'murders', 'murdPerPop', 'rapes', 'rapesPerPop', 'robberies', 'robbbPerPop', 'assaults', 'assaultPerPop', 
                    'burglaries', 'burglPerPop', 'larcenies', 'larcPerPop', 'autoTheft', 'autoTheftPerPop', 'arsons', 'arsonsPerPop', 'violentPerPop', 
                    'nonViolPerPop')

#gli na son segnati con ?
data[data=='?'] <- NA
str(data, list.len=ncol(data))

#covariate
covariate <- data[ , -c(1,3,4,5)]

#trasformo in numeric le variabili (tranne gangUnit)
for (i in (2:122)) {covariate[,i] <- as.numeric(covariate[,i])}
for (i in (124:143)) {covariate[,i] <- as.numeric(covariate[,i])}

#ricodifico gangUnit
covariate$temp[covariate$gangUnit==0]='no'
covariate$temp[covariate$gangUnit==10]='yes'
covariate$temp[covariate$gangUnit==5]='part_time'
covariate$gangUnit <- as.factor(covariate$temp)
covariate$temp <- NULL

#tolgo le covariate inutili
covariate[ , c('murders', 'rapes', 'rapesPerPop', 'robberies', 'assaults','burglaries', 'burglPerPop', 'larcenies', 
               'larcPerPop', 'autoTheft', 'autoTheftPerPop', 'arsons', 'arsonsPerPop', 'violentPerPop')] <- NULL

covariate[ , c('pct12-21', 'pct12-29', 'persUrban', 'persPoverty', 'pctOccupManu', 'pct2Par', 'pctKids-4w2Par', 'pctMaleDivorc', 'pctFemDivorc', 'pctWorkMom-6','kidsBornNevrMarr', 'numForeignBorn',
               "pctFgnImmig-3", "pctFgnImmig-5", "pctFgnImmig-8", "pctFgnImmig-10", 'pctLargHousFam', 'persPerOwnOccHous', 'persPerRenterOccup', 'pctPersOwnOccup', 
               'houseVacant', "ownHousLowQ", "ownHousMed", "ownHousUperQ", "ownHousQrange", "rentLowQ", "rentMed", "rentUpperQ", "rentQrange", 
               "medGrossRent", "medRentpctHousInc", "medOwnCostpct", "medOwnCostPctWO", "numPolice", "policeField", "policeCalls", 'persPerOwnOccup', 'persPerOccupHous')] <- NULL

covariate$pctEmergShelt <- covariate$persEmergShelt/covariate$pop
covariate$persEmergShelt <- NULL

covariate$pctHomeless <- covariate$persHomeless/covariate$pop
covariate$persHomeless <- NULL

covariate$pctOfficDrugUnit <- NULL #è l'unica var poliziesca rimasta, non ha senso tenerla

plot(covariate$pctUrban)
covariate$pctUrban <- NULL #vedi plot

#non c'è alcun tipo di pattern tra pop e nonViolPerPop
plot(covariate$pop[which(covariate$pop<500000)], covariate$nonViolPerPop[which(covariate$pop<500000)])
plot(covariate$pop[which(covariate$pop<100000)], covariate$nonViolPerPop[which(covariate$pop<100000)])
plot(covariate$pop[which(covariate$pop<50000)], covariate$nonViolPerPop[which(covariate$pop<50000)])
plot(covariate$pop[which(covariate$pop<30000)], covariate$nonViolPerPop[which(covariate$pop<30000)])
covariate$pop <-NULL #vedi i plot

#non c'è alcun tipo di pattern tra landArea e nonViolPerPop
plot(covariate$landArea, covariate$nonViolPerPop)
plot(covariate$landArea[which(covariate$landArea<1000)], covariate$nonViolPerPop[which(covariate$landArea<1000)])
plot(covariate$landArea[which(covariate$landArea<500)], covariate$nonViolPerPop[which(covariate$landArea<500)])
plot(covariate$landArea[which(covariate$landArea<250)], covariate$nonViolPerPop[which(covariate$landArea<250)])
plot(covariate$landArea[which(covariate$landArea<100)], covariate$nonViolPerPop[which(covariate$landArea<100)])
plot(covariate$landArea[which(covariate$landArea<60)], covariate$nonViolPerPop[which(covariate$landArea<60)])
covariate$landArea <-NULL #vedi i plot

#non c'è alcun tipo di pattern tra robbbPerPop e nonViolPerPop
plot(covariate$robbbPerPop, covariate$nonViolPerPop)
covariate$robbbPerPop <- NULL


#tolgo dal df unità con target (nonViolPerPop) NA
sum(is.na(covariate$nonViolPerPop)) #ci sono 97 NA per nonViolPerPop
covariate <- covariate[is.na(covariate$nonViolPerPop)==F,]

#NA count
na <- sapply(covariate, function(x) sum(is.na(x))/nrow(covariate))
na
na_20 <- sapply(na, function(x) (x>0.2))
na_80 <- sapply(na, function(x) (x>0.8))
table(na_20==na_80) #tutte le covariate che hanno almeno il 20% di NA in realtà ne ha almeno l'80%
to_keep <- as.array(names(na_20[na_20==F]))
covariate <- covariate[ , to_keep]

#cerchiamo se ci sono variabili a varianza zero
table(data$persHomeless)['0']
table(data$persEmergShelt)['0']
#rimuovo pctEmergShelt e Homeless perché hanno quasi tutti valori 0 (o son stati segnati gli NA come 0 o non c'è varianza)
covariate$pctHomeless <- NULL
covariate$pctEmergShelt <- NULL
var <- sapply(covariate[,3:66], function(x) var(x, na.rm = TRUE))
var[var<0.1]

#la variabile persPerFam ha varianza bassa e non si nota nessun legame funzionale con nonViolPerPop
plot(covariate$persPerFam, covariate$nonViolPerPop)
covariate$persPerFam <- NULL

#ricodifica di state in regions
lvl <- levels(covariate$State)
states <- sort(c('CT', 'ME', 'MA', 'NH', 'RI', 'VT', 'NJ', 'NY', 'DC', 'DE', 'MD', 'PA', 'VA', 'WV', 'AL', 'FL', 'GA', 'KY', 'MS', 
                 'NC', 'SC', 'TN', 'IL', 'IN', 'MI', 'MN', 'OH', 'WI', 'AR', 'LA', 'NM', 'OK', 'TX', 'IA', 'KS', 'MO', 'CO', 'ND', 
                 'SD', 'UT', 'WY', 'AZ', 'CA', 'NV', 'AK', 'ID', 'OR', 'WA'))
str(lvl)
str(states)
lvl==states #VERO

covariate$temp[covariate$State=='NJ']='Region1'
covariate$temp[covariate$State=='AK']='Region9'
covariate$temp[covariate$State=='AL']='Region3'
covariate$temp[covariate$State=='AR']='Region5'
covariate$temp[covariate$State=='AZ']='Region8'
covariate$temp[covariate$State=='CA']='Region8'
covariate$temp[covariate$State=='CO']='Region7'
covariate$temp[covariate$State=='CT']='Region1'
covariate$temp[covariate$State=='DC']='Region2'
covariate$temp[covariate$State=='DE']='Region2'
covariate$temp[covariate$State=='FL']='Region3'
covariate$temp[covariate$State=='GA']='Region3'
covariate$temp[covariate$State=='IA']='Region6'
covariate$temp[covariate$State=='ID']='Region9'
covariate$temp[covariate$State=='IL']='Region4'
covariate$temp[covariate$State=='IN']='Region4'
covariate$temp[covariate$State=='KS']='Region6'
covariate$temp[covariate$State=='KY']='Region3'
covariate$temp[covariate$State=='LA']='Region5'
covariate$temp[covariate$State=='MA']='Region1'
covariate$temp[covariate$State=='MD']='Region2'
covariate$temp[covariate$State=='ME']='Region1'
covariate$temp[covariate$State=='MI']='Region4'
covariate$temp[covariate$State=='MN']='Region4'
covariate$temp[covariate$State=='MO']='Region6'
covariate$temp[covariate$State=='MS']='Region3'
covariate$temp[covariate$State=='NC']='Region3'
covariate$temp[covariate$State=='ND']='Region7'
covariate$temp[covariate$State=='NH']='Region1'
covariate$temp[covariate$State=='NM']='Region5'
covariate$temp[covariate$State=='NV']='Region8'
covariate$temp[covariate$State=='NY']='Region1'
covariate$temp[covariate$State=='OH']='Region4'
covariate$temp[covariate$State=='OK']='Region5'
covariate$temp[covariate$State=='OR']='Region9'
covariate$temp[covariate$State=='PA']='Region2'
covariate$temp[covariate$State=='RI']='Region1'
covariate$temp[covariate$State=='SC']='Region3'
covariate$temp[covariate$State=='SD']='Region7'
covariate$temp[covariate$State=='TN']='Region3'
covariate$temp[covariate$State=='TX']='Region5'
covariate$temp[covariate$State=='UT']='Region7'
covariate$temp[covariate$State=='VA']='Region2'
covariate$temp[covariate$State=='VT']='Region1'
covariate$temp[covariate$State=='WA']='Region9'
covariate$temp[covariate$State=='WI']='Region4'
covariate$temp[covariate$State=='WV']='Region2'
covariate$temp[covariate$State=='WY']='Region7'
covariate$State <- as.factor(covariate$temp)
covariate$temp <- NULL


#plot target con singole possibili covariate
par(mfrow=c(3,3))
for (i in (1:65)) {
  plot(covariate[,i], covariate$nonViolPerPop)
}
par(mfrow=c(1,1))

for (i in c(22, 30, 48, 52, 53, 57, 65)) {print(colnames(covariate)[i])}

# "otherPerCap"
# "pctEmployMfg"
# "medNumBedrm"
# "pctVacant6up"
# "medYrHousBuilt"
# "pctBornStateResid"
# "assaultPerPop"

plot(covariate$otherPerCap, covariate$nonViolPerPop) #NO PATTERN
plot(covariate$pctEmployMfg, covariate$nonViolPerPop) #NO PATTERN
plot(covariate$medNumBedrm, covariate$nonViolPerPop) #NO PATTERN
plot(covariate$otherPerCap, covariate$nonViolPerPop) #NO PATTERN E SEPARAZIONE
plot(covariate$pctVacant6up, covariate$nonViolPerPop) #NO PATTERN
plot(covariate$medYrHousBuilt, covariate$nonViolPerPop) #NO PATTERN E VALORE 1939 ANOMALO
plot(covariate$pctBornStateResid, covariate$nonViolPerPop) #NO PATTERN
plot(covariate$assaultPerPop, covariate$nonViolPerPop) #NO PATTERN

list_name <- c("otherPerCap", "pctEmployMfg", "medNumBedrm", "pctVacant6up", 
               "medYrHousBuilt", "pctBornStateResid", "assaultPerPop")
for (i in list_name){covariate[, i] <- NULL}

#la variabile murdPerPop non è a varianza 0 ma ha tanti valori pari a 0.00
nrow(covariate[which(covariate$murdPerPop==0),])
#metà dataset ha merdPerPop=0, non è utilizzabile come variabile
covariate$murdPerPop <- NULL
covariate$pctTeenW2Par <- covariate$`pct12-17w2Par`
covariate$`pct12-17w2Par` <- NULL
covariate$pctImmig5 <- covariate$`pctImmig-5`
covariate$`pctImmig-5` <- NULL


# create a binary target
summary(covariate$nonViolPerPop)
m <- mean(covariate$nonViolPerPop)
covariate$target_dummy = ifelse(covariate$nonViolPerPop>m,1,0)
table(covariate$target_dummy)
prop.table(table(covariate$target_dummy))

# lm finale modello robusto State + perHoush + pctWhite + I(pctWhite^2) + pctWfarm + 
# perCapInc + pctLowEdu + pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5

# white, capinc, housh, state, teenw2par, lowedu,  samehouse5, farm,   immig5 

covariate$IncMil <- covariate$perCapInc/1000 #per rendere più leggibile l'OR

#first model
log1 <- glm(target_dummy ~ State + pctWhite + IncMil , data=covariate, family=binomial)
summary(log1)

# coefficients
library(coefplot)
coefplot(log1, intercept=FALSE)

# OR
a1=round(exp(cbind(OR=coef(log1), confint(log1))), digits = 2)
a1
library(forestmodel)
print(forest_model(log1),text_size = 5)

# fit
drop1(log1, test="LRT")
summary(log1)


# deviance and R2
ls(log1)
log1$deviance
log1$null.deviance
R2_1=1-(log1$deviance/log1$null.deviance)
R2_1


#second model
log2 <- glm(target_dummy ~ pctWhite + IncMil + perHoush, data=covariate, family=binomial)
summary(log2)

# coefficients
library(coefplot)
coefplot(log2, intercept=FALSE)

# OR
a2=round(exp(cbind(OR=coef(log2), confint(log2))), digits = 2)
a2
library(forestmodel)
print(forest_model(log2), text_size = 5)

# fit
drop1(log2, test="LRT")
summary(log2)

# deviance and R2
log2$deviance
log2$deviance-log1$deviance
R2_2=1-(log2$deviance/log2$null.deviance)
R2_2

#third model
log3 <- glm(target_dummy ~ State + pctWhite + perHoush , data=covariate, family=binomial)
summary(log3)

# coefficients
library(coefplot)
coefplot(log3, intercept=FALSE)

# OR
a3=round(exp(cbind(OR=coef(log3), confint(log3))), digits = 2)
a3
library(forestmodel)
print(forest_model(log3),text_size = 5)

# fit
drop1(log3, test="LRT")
summary(log3)

# deviance and R2
log3$deviance
log3$null.deviance
R2_3=1-(log3$deviance/log3$null.deviance)
R2_3

#fourth model
log4 <- glm(target_dummy ~ State + pctWhite + perHoush + IncMil, data=covariate, family=binomial)
summary(log4)

# coefficients
library(coefplot)
coefplot(log4, intercept=FALSE)

# OR
a4=round(exp(cbind(OR=coef(log4), confint(log4))), digits = 2)
a4
library(forestmodel)
print(forest_model(log4),text_size = 5)

# fit
drop1(log4, test="LRT")
summary(log4)

# deviance and R2
log4$deviance
log4$null.deviance
R2_4=1-(log4$deviance/log4$null.deviance)
R2_4


#fifth model
log5 <- glm(target_dummy ~ State + pctWhite + perHoush + IncMil + pctLowEdu, data=covariate, family=binomial)
summary(log5)

# coefficients
library(coefplot)
coefplot(log5, intercept=FALSE)

# OR
a5=round(exp(cbind(OR=coef(log5), confint(log5))), digits = 2)
a5
library(forestmodel)
print(forest_model(log5),text_size = 5)

# fit
drop1(log5, test="LRT")
summary(log5)

# deviance and R2
log5$deviance
log5$null.deviance
R2_5=1-(log5$deviance/log5$null.deviance)
R2_5

#non vale la pena aggiungere una quinta variabile
#teniamo log4

# separation
table(covariate$State, covariate$target_dummy) #non sembra esserci separazione
plot(covariate$target_dummy, covariate$State) #non sembra esserci separazione

covariate_tar0 <- covariate[which(covariate$target_dummy==0), c('pctWhite', 'perHoush', 'IncMil')]
covariate_tar1 <- covariate[which(covariate$target_dummy==1), c('pctWhite', 'perHoush', 'IncMil')]
summary(covariate_tar0)
summary(covariate_tar1)
#non c'è separazione neanche tra le quantitative, infatti i range delle quantitative nei due gruppi (target 0 o 1)
#sono pressoché identici, sicuramente non sono separati

#prediction

predicted_p <- predict(log4, covariate, type="response")  #così ci dà le probabilità previste di 1 al variare delle x
head(predicted_p)

predicted_p2 <- predict(log4, covariate) #così ci dà i logit previsti  
head(predicted_p2)

# add to the data
good_cov <- covariate[,c('target_dummy', 'State', 'pctWhite', 'perHoush', 'IncMil')]
good_cov$predicted_p <- predicted_p
tail(good_cov)

# predicted target
good_cov$predicted_y <- ifelse(good_cov$predicted_p > 0.5,1,0)
tail(good_cov)

# classification
table(observed=good_cov$target_dummy,predicted=good_cov$predicted_y)
table(observed=good_cov$target_dummy,predicted=good_cov$predicted_y)/nrow(good_cov)
#quasi 1 unità su 3 misclassificata, non è buono

# accuracy and error rate
t=table(observed=good_cov$target_dummy,predicted=good_cov$predicted_y)/nrow(good_cov)

acc=t[1,1]+t[2,2]
acc

err_rate=1-acc
err_rate

#modello con interazioni
#abbiamo provato diversi modelli con interazioni ma nessuno produce variazioni significative, il migliore è il seguente
log6 <- glm(target_dummy ~ State*pctWhite + perHoush + IncMil, data=covariate, family=binomial)
summary(log6)
drop1(log6, test="LRT")

# deviance and R2
log6$deviance
log6$null.deviance
R2_6=1-(log6$deviance/log6$null.deviance)
R2_6
library(ggeffects)
p=ggpredict(log6,c("State", "pctWhite"))
plot(p)
p2=ggpredict(log4,c("State", "pctWhite"))
plot(p2)

head(good_cov)

library(ResourceSelection)
hoslem.test(log4$y, fitted(log4), g=10)
hoslem.test(log6$y, fitted(log6), g=10)

#nonostante la devianza spiegata non cresca particolarmente, si può notare nel grafico congiunto come cambino le probabilità
#previste con l'aggiunta dell'interazione