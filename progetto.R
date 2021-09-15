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

table(sapply(covariate, function(x) is.numeric(x))) #le covariate rimaste son tutte numeriche, tranne (giustamente) una, che è State

#modello con tutte 56 le covariate rimaste
lm_start <- lm(nonViolPerPop ~ ., covariate)
summary(lm_start)
drop1(lm_start, test='F')
par(mfrow=c(2,2))
plot(lm_start)
par(mfrow=c(1,1))

#collinearity
k=0
for (i in (2:57)) {
  for (j in (2:57)){
    if (i!=j){
      if (abs(cor(covariate[,i], covariate[,j], use='complete.obs'))>0.7) {
        print(c(cor(covariate[,i], covariate[,j]), colnames(covariate)[i], colnames(covariate)[j]))
        k=k+1                                                                  }
    }
  }
}
k #198 coppie di variabili hanno correlazione in modulo maggiore di 0.7, bisogna fare bene analisi collinearità


#fare VIF e TOL per tutte le esplicative sarebbe lungo, prima faccio model selection

#Model Selection
selectedMod <- step(lm_start, direction="both")
drop1(selectedMod, test='F')

#nonViolPerPop ~ State + perHoush + pctWhite + pctHisp + `pct16-24` + pct65up + medIncome + pctWwage + 
#pctWfarm + perCapInc + blacKPerCap + asianPerCap + pctLowEdu + pctCollGrad + pctOccupMgmt + 
#pctAllDivorc + pctKids2Par + `pct12-17w2Par` + pctKidsBornNevrMarr + 
#`pctImmig-5` + `pctImmig-8` + pctLargHous + pctPopDenseHous + pctSmallHousUnits + pctHousOccup + 
#pctHousWOphone + pctHousWOplumb + pctForeignBorn + `pctSameHouse-5`


#seleziono variabili nel modello selezionato con AIC
new_cov=selectedMod$model
lm_1 <- lm(nonViolPerPop ~ ., new_cov)
summary(lm_1)
drop1(lm_1, test='F')

#faccio ora TOL e VIF solo sulle variabili seleziona
y <- new_cov[,'nonViolPerPop']
X <- new_cov[, (3:30)] #le prime due sono la target e la categoriale
X=as.matrix(X)
library(mctest)
imcdiag(X,y, method = 'TOL')

k=0
for (i in (3:30)) {
  for (j in (3:30)){
    if (i!=j){
      if (abs(cor(new_cov[,i], new_cov[,j], use='complete.obs'))>0.7) {
        print(c(cor(new_cov[,i], new_cov[,j]), colnames(new_cov)[i], colnames(new_cov)[j]))
        k=k+1                                                                  }
    }
  }
}
k #50

library(corrplot)
corrplot(cor(new_cov[,3:30]))

#pctKids2Par ha TOL molto basso, alta correlazione con più variabili e ha pct12-18w2Par che è molto simile --> tolgo
new_cov$pctKids2Par <- NULL

X <- new_cov[, (3:29)] #le prime due sono la target e la categoriale
imcdiag(X,y, method = 'TOL')

#pctImmig-8 ha il TOL più basso ed è altamente correlato con le altre variabili di immigrazione. Tra le 2 var
#di immigrazione togliamo questa perché supponiamo che immigrati da più tempo abbiano trovato lavoro più facilmente
new_cov$`pctImmig-8` <- NULL

X <- new_cov[, (3:28)] #le prime due sono la target e la categoriale
imcdiag(X,y, method = 'TOL')

corrplot(cor(new_cov[,3:28]))

#perCapInc e medIncome sono molto simili e hanno entrambe TOL basso; togliendone una dovrebbe alzarsi notevolmente
#il TOL dell'altra. Togliamo medIncome perché ha TOL inferiore
new_cov$medIncome <- NULL
X <- new_cov[, (3:27)] #le prime due sono la target e la categoriale
imcdiag(X,y, method = 'TOL')


#come con Immig-8 ma con pctForeignBorn (teniamo Immig-5)
new_cov$pctForeignBorn <- NULL
X <- new_cov[, (3:26)] #le prime due sono la target e la categoriale
imcdiag(X,y, method = 'TOL')


#tolgo pctCollGrad perché ha un TOL basso, è altamente correlata con perCapInc e per l'educazione preferiamo tenere pctLowEdu
cor(new_cov$pctCollGrad, new_cov$perCapInc)
new_cov$pctCollGrad <- NULL

X <- new_cov[, (3:25)] #le prime due sono la target e la categoriale
imcdiag(X,y, method = 'TOL')
corrplot(cor(new_cov[,3:25]))


#pct65up ha il secondo TOL più basso ed è quasi perfettamente correlato con pctWWage
cor(new_cov$pct65up, new_cov$pctWwage)
new_cov$pct65up <- NULL

X <- new_cov[, (3:24)] #le prime due sono la target e la categoriale
imcdiag(X,y)

corrplot(cor(new_cov[,3:24]))

#pctPopDenseHous rimane l'unica con VIF>10 e inoltre ha lo stesso significato di pctLargHous (valuta sovraffollamento) --> tolgo
cor(new_cov$pctPopDenseHous, new_cov$pctLargHous)
new_cov$pctPopDenseHous <- NULL

X <- new_cov[, (3:23)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:23]))


#tolgo pctKidsBornNevrMarr perché ha un TOL basso ed è altamente correlata con pct12-17w2Par e pctWhite
cor(new_cov$pctKidsBornNevrMarr, new_cov$pctWhite)
cor(new_cov$pctKidsBornNevrMarr, new_cov$`pct12-17w2Par`)
new_cov$pctKidsBornNevrMarr <- NULL

X <- new_cov[, (3:22)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:22]))

#la variabile pctLargHous ha un TOL basso ed è molto correlato con perHoush
cor(new_cov$pctLargHous, new_cov$perHoush)
new_cov$pctLargHous <- NULL

X <- new_cov[, (3:21)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:21]))

#la variabile pctOccupMgmt ha un TOL basso, è altamente correlata con perCapInc e
#pctLowEdu ed è meno interessante di queste due, quindi la tolgo
new_cov$pctOccupMgmt <- NULL

X <- new_cov[, (3:20)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:20]))


#tolgo pctHousWOphone perché ha un TOL basso e non è troppo significativo come
#indicatore di ricchezza e c'è già perCapInc
new_cov$pctHousWOphone <- NULL

X <- new_cov[, (3:19)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:19]))

#la variabile pctSmallHousUnits (% case con meno di 3 camere da letto) ha un TOL basso
#e non riteniamo essere un indicatore molto affidabile, visto anche che poco meno
#della metà delle comunità del dataset ha il 50% di case di quel tipo
new_cov$pctSmallHousUnits <- NULL

X <- new_cov[, (3:18)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:18]))


#la variabile pctAllDivorc ha un TOL basso ed una correlazione piuttosto elevata con pct12-17w2Par (-0.7).
#Inoltre i divorzi sono un sottoinsieme dei casi di bambini senza entrambi i genitori in casa.
cor(new_cov$`pct12-17w2Par`, new_cov$pctAllDivorc)
new_cov$pctAllDivorc <- NULL

X <- new_cov[, (3:17)] #le prime due sono la target e la categoriale
imcdiag(X,y)
corrplot(cor(new_cov[,3:17]))


#grafici diagnostica
lm_2 <- lm(nonViolPerPop ~ ., new_cov)
drop1(lm_2, test = 'F')
summary(lm_2)
par(mfrow=c(2,2))
plot(lm_2)
par(mfrow=c(1,1))


#symbox
library(car)
symbox(new_cov$nonViolPerPop, powers=c(-1, -.5, 0, .5, 1, 2), start=0) #migliore è non trasformato

#boxcox
library(MASS)
boxcox1 <- boxcox(lm_2)
lambda=boxcox1$x[which.max(boxcox1$y)]
lambda #non conviene trasformare

# Evaluate homoscedasticity
library(car)
# non-constant error variance test
ncvTest(lm_2) #rifiuto H0 di omosch.

#per vedere quali covariate non sono lineari
par(mfrow=c(2,2))
crPlots(lm_2)
return
return
par(mfrow=c(1,1)) 

#GAM sulle x
library(mgcv)
new_cov$pctYoung <- new_cov$`pct16-24`
new_cov$`pct16-24` <- NULL
new_cov$pctTeenW2Par <- new_cov$`pct12-17w2Par`
new_cov$`pct12-17w2Par` <- NULL
new_cov$pctImmig5 <- new_cov$`pctImmig-5`
new_cov$`pctImmig-5` <- NULL
new_cov$pctSameHouse5 <- new_cov$`pctSameHouse-5`
new_cov$`pctSameHouse-5` <- NULL

paste(colnames(new_cov)[3:17], collapse=") + s(")
#s(perHoush) + s(pctWhite) + s(pctHisp) + s(pctWwage) + s(pctWfarm) + s(perCapInc) + s(blackPerCap) + s(asianPerCap) + s(pctLowEdu) + s(pctHousOccup) + s(pctHousWOplumb) + s(pctYoung) + s(pctTeenW2Par) + s(pctImmig5) + s(pctSameHouse5)
lm_gam <- gam(nonViolPerPop ~ State+s(perHoush) + s(pctWhite) + s(pctHisp) + s(pctWwage) + s(pctWfarm) + s(perCapInc) + s(blackPerCap) + s(asianPerCap) + s(pctLowEdu) + s(pctHousOccup) + s(pctHousWOplumb) + s(pctYoung) + s(pctTeenW2Par) + s(pctImmig5) + s(pctSameHouse5), data=new_cov)
summary(lm_gam)

#guardando i df della seconda tabella si nota che sono lineari: perHoush, blackPerCap, pctLowEdu, pctHousWOplumb,
#pctSameHouse5

anova(lm_2, lm_gam, test='Chisq')

par(mfrow=c(2,4))
plot(lm_gam)
return()
par(mfrow=c(1,1))

#proviamo a diminuire i gradi di libertà per le covariate con un plot non interpretabile

lm_gam1 <- gam(nonViolPerPop ~ State+s(perHoush) + s(pctWhite) + s(pctHisp) + s(pctWwage) + s(pctWfarm, k=5) + s(perCapInc, k=5) +
                s(blackPerCap) + s(asianPerCap) + s(pctLowEdu) + s(pctHousOccup, k=4) + s(pctHousWOplumb) + s(pctYoung, k=5) + s(pctTeenW2Par) +
                s(pctImmig5, k=5) + s(pctSameHouse5), data=new_cov)

par(mfrow=c(2,4))
plot(lm_gam1)
return()
par(mfrow=c(1,1))

summary(lm_gam1)

lm_3 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + pctHisp + pctWwage + 
             + pctWfarm + perCapInc + blackPerCap + log(asianPerCap+1) + pctLowEdu + 
             pctHousOccup + pctHousWOplumb + pctYoung + log(pctTeenW2Par) +pctImmig5 + pctSameHouse5, data=new_cov)
lm_4 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + pctHisp + pctWwage + 
             pctWfarm + perCapInc + blackPerCap + asianPerCap + I(asianPerCap^2) + pctLowEdu + 
             pctHousOccup + pctHousWOplumb + pctYoung + I(pctYoung^2) + 
             pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)
summary(lm_3)
drop1(lm_3, test='F')
anova(lm_2, lm_3, lm_4, lm_gam, test='Chisq')
summary(lm_4)
drop1(lm_4, test='F')

#teniamo lm_4

#Opscore:proviamo a trasformare State
table(new_cov$State)
library(lsmeans)
ls1=lsmeans(lm_4, pairwise ~ State, adjust="tukey")
ls1$lsmeans
plot(ls1$lsmeans, alpha = .05)
new_cov$State_opscore <- 0
new_cov[new_cov$State=='Region1',]$State_opscore <- 'Region_op1'
new_cov[new_cov$State=='Region2',]$State_opscore <- 'Region_op2'
new_cov[new_cov$State=='Region3',]$State_opscore <- 'Region_op3'
new_cov[new_cov$State=='Region4',]$State_opscore <- 'Region_op4'
new_cov[new_cov$State=='Region5',]$State_opscore <- 'Region_op5'
new_cov[new_cov$State=='Region6',]$State_opscore <- 'Region_op5'
new_cov[new_cov$State=='Region7',]$State_opscore <- 'Region_op6'
new_cov[new_cov$State=='Region8',]$State_opscore <- 'Region_op7'
new_cov[new_cov$State=='Region9',]$State_opscore <- 'Region_op6'

lm_4_op <- lm(nonViolPerPop ~ State_opscore + perHoush + pctWhite + I(pctWhite^2) + pctHisp + pctWwage + 
             pctWfarm + perCapInc + blackPerCap + asianPerCap + I(asianPerCap^2) + pctLowEdu + 
             pctHousOccup + pctHousWOplumb + pctYoung + I(pctYoung^2) + 
             pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)

summary(lm_4)
summary(lm_4_op)
anova(lm_4, lm_4_op, test='Chisq')
#non c'è un miglioramento significativo, teniamo State non aggregata con Opscore

#ci sono variabili non significative, bisogna fare model selection.
#Piuttosto che selezionare con AIC a questo punto preferisco selezionare quelle significative.

#Valuto l'omoschedasticità per sapere se i p-value sono affidabili
library(car)
ncvTest(lm_4) #c'è eterosch quindi i p-value non sono affidabili, quindi per
              #eliminare le variabili in base alla significatività devo usare White

library(lmtest)
library(sandwich)
coeftest(lm_4, vcov. = vcovHC(lm_4))

#tolgo pctHousOccup perché è la meno significativa
lm_5_1 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + pctHisp + pctWwage + 
             pctWfarm + perCapInc + blackPerCap + asianPerCap + I(asianPerCap^2) + pctLowEdu + 
             pctHousWOplumb + pctYoung + I(pctYoung^2)+ pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)
ncvTest(lm_5_1)
coeftest(lm_5_1, vcov. = vcovHC(lm_5_1))

#asianPerCap è la meno significativa e perciò tolgo quella e il suo quadrato
lm_5_2 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + pctHisp + pctWwage + 
               pctWfarm + perCapInc + blackPerCap + pctLowEdu + 
               pctHousWOplumb + pctYoung+ I(pctYoung^2) + pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)
ncvTest(lm_5_2)
coeftest(lm_5_2, vcov. = vcovHC(lm_5_2))

#tolgo pctHousWOplumb perché è la meno significativa
lm_5_3 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + pctHisp + pctWwage + 
               pctWfarm + perCapInc + blackPerCap + pctLowEdu + 
               pctYoung + I(pctYoung^2) + pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)
ncvTest(lm_5_3)
coeftest(lm_5_3, vcov. = vcovHC(lm_5_3))


#tolgo pctHisp perché è la meno significativa
lm_5_4 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2)+ pctWwage + 
               pctWfarm + perCapInc + blackPerCap + pctLowEdu + 
               pctYoung + I(pctYoung^2) + pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)
ncvTest(lm_5_4)
coeftest(lm_5_4, vcov. = vcovHC(lm_5_4))


#tolgo blackPerCap perché è la meno significativa
lm_5_5 <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + 
               pctWfarm + perCapInc + pctWwage + pctLowEdu + 
               pctYoung + I(pctYoung^2) + pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=new_cov)
ncvTest(lm_5_5)
coeftest(lm_5_5, vcov. = vcovHC(lm_5_5))
summary(lm_5_5)

new_cov2 <- lm_5_5$model
new_cov2[,c('I(pctWhite^2)', 'I(pctYoung^2)', 'I(pctTeenW2Par^2)')] <- NULL

#Trattamento dei punti influenti

#grafico con leverage e outliers per identificare 
library(car)
influencePlot(lm_5_5,  main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )

new_cov2$cooksd <- cooks.distance(lm_5_5)
cutoff <- 4/(nrow(new_cov2)-length(lm_5_5$coefficients)-2)
cutoff
plot(lm_5_5, which=4, cook.levels=cutoff) #which=4 seleziona il grafico delle dist di Cook
abline(h=cutoff, col='red')
cov_noInf <- new_cov2[which(new_cov2$cooksd<cutoff),]

lm_noInf <- lm(nonViolPerPop ~ State + perHoush + pctWhite + I(pctWhite^2) + 
                 pctWfarm + perCapInc + pctWwage + pctLowEdu + 
                 pctYoung + I(pctYoung^2) + pctTeenW2Par + I(pctTeenW2Par^2) +pctImmig5 + pctSameHouse5, data=cov_noInf)

ncvTest(lm_noInf) #l'eterosch è diminuita parecchio, ma è comunque significativa
coeftest(lm_noInf, vcov. = vcovHC(lm_noInf))
summary(lm_noInf)


#our final model
lm_fin <- lm_noInf

drop1(lm_fin, test='F')
library(lsmeans)
lsmeans(lm_fin, pairwise ~ State, adjust="tukey")$contrasts

par(mfrow=c(2,2))
plot(lm_fin)
par(mfrow=c(1,1))

#Omoschedasticità
library(car)
# non-constant error variance test
ncvTest(lm_noInf)
#l'eteroschedasticità è nettamente diminuita, ma rimane significativa, quindi bisogna
#usare la correzione di White

#Shapiro-Wilk test per la normalità dei residui
shapiro.test(lm_fin$residuals)


plot(lm_fin$fitted.values , cov_noInf$nonViolPerPop)
cor(lm_fin$fitted.values , cov_noInf$nonViolPerPop)
summary(lm_fin)$adj.r.squared

#our starting model
par(mfrow=c(1,2))
plot(lm_start$fitted.values , covariate$nonViolPerPop); plot(lm_fin$fitted.values , cov_noInf$nonViolPerPop)
cor (lm_start$fitted.values , covariate$nonViolPerPop); cor(lm_fin$fitted.values , cov_noInf$nonViolPerPop)
summary(lm_start)$adj.r.squared; summary(lm_fin)$adj.r.squared
#nel grafico ci sono dei modesti miglioramenti, l'R2adj cresce di 0.22 e la correlazione
#tra fitted e osservati di 0.16

par(mfrow=c(2,2))
plot(lm_start)
plot(lm_fin)


library(coefplot)
coefplot(lm_fin, decreasing = TRUE, sort = "magnitude",intercept=FALSE)
#########################################################################################

