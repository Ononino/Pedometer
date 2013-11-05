Pedometer
=========

Pedometer
library("ggplot2")

## Graf 1: Povprečno število opravljenih kilometrov v različnih izmenah
d1 <- read.csv(file="meritve.csv",head=TRUE,sep=",")
d2 <- read.csv(file="podatki2.csv",head=TRUE,sep=",")

dAll <- merge(d1,d2,by="Pedometer")

dAll$Km <- as.numeric(as.character(dAll$Km)) 

aggdata <- aggregate(dAll$Km, by=list(dAll$Shifts), FUN=mean, na.rm=TRUE)
names(aggdata) <- c("Izmena", "Kilometri")

aggdata[,1] <- factor(c("Dopoldan", "Vikend dnevna", "Tedenska nočna", "Vikend nočna", "Popoldan"), levels = c("Dopoldan", "Popoldan", "Tedenska nočna", "Vikend dnevna", "Vikend nočna"))

ggplot(aggdata, aes(x=Izmena, y=Km, fill=Izmena)) + geom_bar(colour="black")

aggdata$ure <- c(8, 12, 9, 12, 7)
aggdata$kmUro <- aggdata$Km / aggdata$ure
ggplot(data=aggdata, aes(x=Izmena, y=Kilometri, fill=Izmena)) + geom_bar(colour="black") +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
       axis.text.x = theme_text(angle=45,colour="#000000", size=14),
       axis.text.y  = theme_text(colour="#000000", size=14),
       axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
       legend.position="none")


## Graf 2: Povprečno število opravljenih kilometrov na uro v različnih izmenah
d1 <- read.csv(file="meritve.csv",head=TRUE,sep=",")
d2 <- read.csv(file="podatki2.csv",head=TRUE,sep=",")

dAll <- merge(d1,d2,by="Pedometer")

dAll$Km <- as.numeric(as.character(dAll$Km)) 

aggdata <- aggregate(dAll$Km, by=list(dAll$Shifts), FUN=mean, na.rm=TRUE)
names(aggdata) <- c("Izmena", "Km")

aggdata[,1] <- factor(c("Dopoldan", "Vikend dnevna", "Tedenska nočna", "Vikend nočna", "Popoldan"), levels = c("Dopoldan", "Popoldan", "Tedenska nočna", "Vikend dnevna", "Vikend nočna"))

ggplot(aggdata, aes(x=Izmena, y=Km, fill=Izmena)) + geom_bar(colour="black")


aggdata$Hours <- c(8, 12, 9, 12, 7)
aggdata$KilometersHours <- aggdata$Km / aggdata$Hours
ggplot(data=aggdata, aes(x=Izmena, y=KilometersHours, fill=Izmena)) + geom_bar(colour="black") +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
       axis.text.x = theme_text(angle=45,colour="#000000", size=14),
       axis.text.y  = theme_text(colour="#000000", size=14),
       axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
       legend.position="none") + scale_y_continuous('Kilometri / uro')
       

## Graf 3: Povprečno število opravljenih kilometrov v vseh delovnih izmenah glede na posamezen dan v celotnem tednu 
aggDT <-aggregate(dAll$Km, by=list(dAll$Dan), FUN=mean, na.rm=TRUE)
aggDTSort <- aggDT
names(aggDTSort) <- c("Dan", "Kilometri")
aggDTSort[,1] <- factor(c("Četrtek", "Nedelja", "Petek", "Ponedeljek", "Sobota", "Sreda", "Torek"), levels = c("Ponedeljek", "Torek", "Sreda", "Četrtek", "Petek", "Sobota", "Nedelja"))
ggplot(data=aggDTSort, aes(x=Dan, y=Kilometri, fill=Dan)) + 
  geom_bar() +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
       axis.text.x = theme_text(angle=45,colour="#000000", size=14),
       axis.text.y  = theme_text(colour="#000000", size=14),
       axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
       legend.position="none")


## Graf 4: Povprečno število skupno opravljenih korakov v posameznem tednu študije 
aggKT <-aggregate(dAll$Koraki, by=list(dAll$Datum2), FUN=mean, na.rm=TRUE)
aggKT[,1] <- ceiling(aggKT[,1]/7)
names(aggKT) <- c("Teden", "Koraki")
aggKT3 <- aggKT[c(-92),]
aggKT3 <-aggregate(aggKT3$Koraki, by=list(aggKT3$Teden), FUN=sum, na.rm=TRUE)
names(aggKT3) <- c("Teden", "Koraki")
aggKT3[,1] <- as.factor(aggKT3[,1])
aggKT3 <- aggKT3[-1,]
ggplot(data=aggKT3, aes(x=Teden, y=Koraki, group=1)) + geom_point() + geom_smooth(se=FALSE, size=1.2) +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
       axis.text.x = theme_text(angle=0,colour="#000000", size=14),
       axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
       axis.text.y = theme_text(angle=0,colour="#000000", size=14),
       legend.position="none") + scale_y_continuous('Število korakov')


## Graf 5: Število udeležencev glede na njihovo povprečno število opravljenih korakov v različnih izmenah

aggUd <-aggregate(dAll$Koraki, by=list(dAll$Pedometer, dAll$Izmena), FUN=mean, na.rm=TRUE)
names(aggUd) <- c("Udelezenec", "Izmena", "Koraki")
aggUd$Kategorija <- NA
aggUd$Sum <- 1
for(i in 1:nrow(aggUd)) {
  if(aggUd[i,3] < 5000) {
    aggUd[i,4] <- "K1 (< 5000 korakov)"
  } 
  if(aggUd[i,3] >= 5000 && aggUd[i,3] < 7500) {
    aggUd[i,4] <- "K2 (>= 5000 < 7500 korakov)"
  } 
  if(aggUd[i,3] >= 7500 && aggUd[i,3] < 10000) {
    aggUd[i,4] <- "K3 (>= 7500 < 10000 korakov) "
  } 
  if(aggUd[i,3] >= 10000) {
    aggUd[i,4] <- "K4 (>= 10000 korakov)"
  } 
}

aggLocke <-aggregate(aggUd$Sum, by=list(aggUd$Izmena, aggUd$Kategorija), FUN=sum, na.rm=TRUE)
names(aggLocke) <- c("Izmena", "Kategorija", "Koraki")
aggLocke[,1] <- sub("D", "Dopoldan", aggLocke[,1], ignore.case =FALSE, fixed=TRUE)
aggLocke[,1] <- sub("P", "Popoldan", aggLocke[,1], ignore.case =FALSE, fixed=TRUE)
aggLocke[,1] <- sub("N", "Tedenska nočna", aggLocke[,1], ignore.case =FALSE, fixed=TRUE)
aggLocke[,1] <- sub("DopoldanTedenska nočna", "Vikend dnevna", aggLocke[,1], ignore.case =FALSE, fixed=TRUE)
aggLocke[,1] <- sub("Tedenska nočnaN", "Vikend nočna", aggLocke[,1], ignore.case =FALSE, fixed=TRUE)

aggLocke[,1] <- factor(aggLocke[,1], levels = c("Dopoldan", "Popoldan", "Tedenska nočna", "Vikend dnevna", "Vikend nočna"))
ggplot(data=aggLocke, aes(x=Izmena, y=Koraki, fill=Kategorija)) + geom_bar(position=position_dodge(), colour="black") +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
     axis.text.x = theme_text(angle=45,colour="#000000", size=14),
     axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
     axis.text.y = theme_text(angle=0,colour="#000000", size=14)) +
   opts(legend.title = theme_text(colour="#990000", size=20, face="bold")) +
   opts(legend.text = theme_text(colour="#000000", size = 14, face = "bold"))


## Graf 6: Nihanje števila opravljenih kilometrov v različnih izmenah skozi obdobje študije

aggSD2 <-aggregate(dAll$Km, by=list(dAll$Datum2, dAll$Izmena), FUN=mean, na.rm=TRUE)
# Extend the regression lines beyond the domain of the data
names(aggSD2) <- c("Dan", "Izmena", "Kilometri")
aggSD2[,2] <- sub("D", "Dopoldan", aggSD2[,2], ignore.case =FALSE, fixed=TRUE)
aggSD2[,2] <- sub("P", "Popoldan", aggSD2[,2], ignore.case =FALSE, fixed=TRUE)
aggSD2[,2] <- sub("N", "Tedenska nočna", aggSD2[,2], ignore.case =FALSE, fixed=TRUE)
aggSD2[,2] <- sub("DopoldanTedenska nočna", "Vikend dnevna", aggSD2[,2], ignore.case =FALSE, fixed=TRUE)
aggSD2[,2] <- sub("Tedenska nočnaN", "Vikend nočna", aggSD2[,2], ignore.case =FALSE, fixed=TRUE)

aggSD2[,2] <- factor(aggSD2[,2], levels = c("Dopoldan", "Popoldan", "Tedenska nočna", "Vikend dnevna", "Vikend nočna"))


ggplot(aggSD2, aes(x=Dan, y=Kilometri, color=Izmena)) + geom_point(shape=1) +
  scale_colour_hue(l=50) + 
  geom_smooth(se=FALSE,    
              fullrange=T, size=1.2) +
                opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
                     axis.text.x  = theme_text(hjust=1.2, size=16),
                      axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
                       axis.text.y  = theme_text(hjust=1.2, size=16)) +
                        opts(legend.title = theme_text(colour="#990000", size=20, face="bold")) +
                          opts(legend.text = theme_text(colour="#000000", size = 14, face = "bold"))


Graf 7: Povezava med delovno obremenitvijo MS glede na število pacientov skozi obdobje študije

## Dodamo se obremenitev iz workload.csv
pac <- read.csv(file="pacientiPoDnevih.csv",head=TRUE,sep=",")
names(pac) <- c("", "Dan", "Pacienti")

wl <- read.csv(file="workload.csv",head=TRUE,sep=",")
names(wl) <- c("Dan", "Obremenitev")
podatki <- c(wl$Obremenitev, pac$Pacienti[1:92])
dan <- c(1:92,1:92)
tip <- c(rep("Obremenitev", 92), rep("Pacienti", 92))
df <- data.frame(Dan=dan, Tip=tip, Podatki=podatki) 

ggplot(df, aes(x=Dan, y=Podatki)) +
  geom_point(shape=1) +    
  geom_smooth() +
  facet_grid(Tip ~ ., scales="free_y") + 
  scale_colour_hue(l=50) + 
  geom_smooth(se=FALSE, fullrange=T, size=1.2) +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
       axis.text.x  = theme_text(angle=90, hjust=1.2, size=16),
       axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
       axis.text.y  = theme_text(hjust=1.2, size=16)) +
         opts(strip.text.y = theme_text(face="bold", angle=90, colour="#990000", size=16))


## Graf 8: Primerjava dveh različnih meritev za pridobitev podatka o povprečni dolžini koraka
d3 <- c(d2[,7], d2[,8])
d4 <- c(rep("Meritev 1", 20), rep("Meritev 2", 20))
d5 <- c(1:20, 1:20)
d6 <- data.frame(Dolžina=d3, Meritev=d4, Udelezenec=d5)

ggplot(data=d6, aes(x=Meritev, y=Dolžina, group=Udelezenec)) + geom_line() + geom_point() +
  opts(axis.title.x = theme_text(face="bold", colour="#990000", size=20),
       axis.text.x = theme_text(angle=0,colour="#000000", size=14),
       axis.title.y = theme_text(angle=90, face="bold", colour="#990000", size=20),
       axis.text.y = theme_text(angle=0,colour="#000000", size=14),
       legend.position="none")


#####################
## Statisticni testi
#####################

# Posebnost vzorca - visja izobrazba -> visja poraba kalorij.
wilcox.test(Kcal ~ St..izobrazbe, data=dAll)

# H 1: V dopoldanskem casu se opravi bistveno vec km kot v popoldanskem casu.
h3 <- read.csv(file="h3.csv",head=TRUE,sep=",")
for(i in 1:nrow(h3)) {
  if(h3[i,1]=="D") {
    h3[i,2] <- h3[i,2]/8
  } else {
    h3[i,2] <- h3[i,2]/7
  }
}
h3$Izmena <- toupper(h3$Izmena)
h3$Izmena <- as.factor(sub(" ", "", h3$Izmena, ignore.case =FALSE, fixed=TRUE))
#t.test(Km ~ Izmena, h3)
wilcox.test(Km ~ Izmena, h3)  # Mann-Whitney statisticni test

# H2: Bolj izkuseni udeleznci raziskave se gibajo bolj racionalno kot tisti z manj leti delovnih izkusenj.
dAll$DI <- NA
dAll$DI[dAll$Delovna.doba..leta. < 6] <- "Low"
dAll$DI[dAll$Delovna.doba..leta. > 5] <- "High"
wilcox.test(Koraki ~ DI, dAll)


# H3: Večje je število bolnikov večja je obremenitev
worlPac <- read.csv(file="WorkloadPacient.csv",head=TRUE,sep=";")
names(worlPac) <- c("Workload", "Število.bolnikov")
worlPac[,1] <- as.factor(worlPac[,1])
worlPac[,2] <- as.factor(worlPac[,2])
t.test(Workload ~ Število.bolnikov, worlPac)


# H4: Primerjava meritev razdalje
prim <- read.csv(file="primerjavaKm.csv",head=TRUE,sep=",")
#t.test(Km ~ Meritev, prim)
wilcox.test(Km ~ Meritev, prim)
