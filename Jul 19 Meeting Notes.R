library(ggplot2)
library(dplyr)
library(data.table)
library(Hmisc) # cut2
library(forecast)
library(eha)
#library(popler)
library(taxize)
library(nlme)
library(zoo)
library(tidyr)
library(BIEN)
library(rredlist)
#library(red)
library(phytools)
library(ape)
library(geiger)
library(mvMORPH)
library(caper)
library(phylolm)
library(rotl)


ar1fin

cor(x = (abs(ar1fin$ar1)),y = ar1fin$phi)
cor(x = (abs(ar1fin$ma1)),y = ar1fin$ar1)
ggplot(ar1fin, (aes(x= ar1)) )+ geom_histogram()+ ggtitle("ar1 for length(TS) > 14 (scaled)") 
ggplot(ar1fin, (aes(x= ma1)) )+ geom_histogram()+ ggtitle("ma for length(TS) > 14 (scaled)") 



#ar1s4 <- subset(ar1s3, ar1 < 1& ar1 > -1)

#ar1sall <- rbind(ar1s3[,1:7],ar1ab)

plhg0 <-data.frame()
plhg<-data.frame()
#pllist <- c("Botryococcus braunii")
pllist  <- unique(ar1fin$GS)
pllist <- as.data.frame(pllist)
ar1spl <- subset(ar1sall, kingdom == "Viridiplantae")
for (i in pllist$pllist) {
  c <- BIEN_trait_species(i)
  d <- subset(c, trait_name == "maximum whole plant height")
  
  if (NROW(d) == 0){
    b <- (strsplit(unlist(i)," "))
    b <- (unlist(b))[1]
    a <- (BIEN_trait_traitbygenus(b, trait = "maximum whole plant height"))$trait_value
    plhg0 <- mean(as.numeric(a))
    plhg <- rbind(plhg,plhg0)
  }
  else{
    plhg0 <- d$trait_value
    plhg <- rbind(plhg, plhg0)
    print(i)
  }
}

pllist <- cbind(pllist,plhg)
colnames(pllist) <- c("GS","maxh")

#colnames(plhg)<- "maxplh"

ar1fin0 <- merge(x = ar1fin, y = pllist, by.x = "GS", by.y = "GS" )


#ar1spl <- cbind(ar1spl, plhg)
ar1spl <- ar1spl[1:14,]
pltaxa <- tnrs_match_names(unique( ar1spl$GS))
pltree <- tol_induced_subtree(ott_ids = ott_id(pltaxa))
plot(pltree, cex = .8, label.offset = .1, no.margin = F)

subsppBM <- unique(ar1fin$GS)
maminc <- data.frame()
for (i in subsppBM) {
  c <- subset(mamfun, GS == i)
  if(NROW(c) == 0){
    c <- subset(mamfun,genus == unlist(strsplit(unlist(i)," "))[1])
  }
  else{}
  maminc  <- rbind(maminc, c)
}
unique(maminc$GS)

maminc$BodyMass.Value <- log10(as.numeric (maminc$BodyMass.Value))

mamtaxa <- tnrs_match_names(unique( maminc$GS))
mamtree <- tol_induced_subtree(ott_ids = ott_id(mamtaxa))
plot(mamtree, cex = .8, label.offset = .1, no.margin = F)

mamar1s<-data.frame()
for (i in unique(maminc$GS)) {
  mamar1 <- subset(ar1fin,GS == i)
  mamar10 <- mamar1[,c(1,2,5)]
  mamar11 <- mean(mamar1$ar1)
  mamar1[1,]$ar1<- mamar11
  mamar1 <- mamar1[1,]
  mamar1s <- rbind(mamar1s, mamar1)
}

mamfin <- mamar1s
ar1finmam <- ar1fin0
mamar1s <- as.data.frame(mamar1s)
ar1finmam <- merge(x = ar1fin0, y = mamar1s, by.x = "GS", by.y = "GS")

##tree for mammals 

mamar1s <- read.csv( file = "/Users/Apple/Desktop/mamar1s.csv", header = T)
mamar1s <- cbind(mamar1s$GS, maminc$BodyMass.Value)
colnames(mamar1s) <- c("GS", "BodyMass")
ggplot(mamar1s, aes(x = maminc$BodyMass.Value, y = ar1))+ geom_point() + geom_smooth(method = lm) + xlab("log(BodyMass)")+
  ggtitle("Mammal bodymass vs ar1")

mamar1s$type <- "mam"
##birds
subsppBM <- unique(ar1fin0$GS)
birdinc <- data.frame()
for (i in subsppBM) {
  c <- subset(birdfun, GS == i)
  if(NROW(c) == 0){
    c <- subset(birdfun,genus == unlist(strsplit(unlist(i)," "))[1])
  }
  else{}
  birdinc  <- rbind(birdinc, c)
}
birdinc$genus<- as.character(birdinc$genus)
birdinc <- write.csv( file ="/Users/Apple/Desktop/birdinc.csv" , birdinc)

birdinc <- read.csv( file ="/Users/Apple/Desktop/birdinc.csv" , header = T)

birdinc2 <- data.frame()
#subsppBM <- unique(ar1sall$GS)
for (i in subsppBM) {
  c <- subset(birdinc, GS == i)
  if(NROW(c) == 0){
    c <- subset(birdinc,genus == unlist(strsplit(unlist(i)," "))[1])
  }
  if(NROW(c) > 0){
    d <- c(as.numeric( c$BodyMass.SpecLevel))
    d <- mean(d)
    e <- data.frame(d,i)
    birdinc2  <- rbind(birdinc2, e)}
}
birdar1s<-data.frame()
for (i in unique(birdinc2$i)) {
  birdar1 <- subset(ar1fin0,GS == i)
  birdar10 <- birdar1[,c(1,2,5)]
  birdar11 <- mean(birdar1$ar1)
  birdar1[1,]$ar1<- birdar11
  birdar1 <- birdar1[1,]
  birdar1s <- rbind(birdar1s, birdar1)
}
birdar1s <- read.csv( file = "/Users/Apple/Desktop/birdar1s.csv", header = T)
birdar1s <- cbind(birdar1s$GS, birdinc2$d)
birdar1s <- as.data.frame(birdar1s)
birdar1s$V2 <- log10(birdar1s$V2)
birdar1s<- drop_na(birdar1s)
birdar1s$V2 <- as.numeric(birdar1s$V2)
colnames(birdar1s)<- c("GS", "BodyMass")
birdar1s$type <- "bird"
anar1s <-  rbind(mamar1s,birdar1s)

ar1finan <- merge(x = ar1fin0, y = anar1s, by.x = "GS", by.y = "GS")

ggplot(birdar1s, aes(y = ar1, x = bodymass))+ geom_point() + geom_smooth(method = lm) + xlab("log(BodyMass)")+
  ggtitle("bird bodymass vs ar1")

birdar1s$type <- "bird"

summary( pgls(phy.resp ~ trait, data=c.data,  lambda = 1))

birdtaxa <- tnrs_match_names(unique( birdinc2$i))
birdtree <- tol_induced_subtree(ott_ids = ott_id(birdtaxa))
plot(birdtree, cex = .8, label.offset = .1, no.margin = F)

#studies_find_trees()

mbtr <- tol_induced_subtree(ott_id(mambirdtaxa), label="name")

plot(smdphylo)
mambirdtaxa <- tnrs_match_names(c(unique(birdinc2$i), unique(maminc$GS)))
mambirdtree <- tol_induced_subtree(ott_ids = ott_id(mambirdtaxa))
plot(mambirdtree, cex = .8, label.offset = .1, no.margin = F)
is.binary(mambirdtree)
mamar1s <- mamar1s[,2:8]
mbar1s <- rbind(birdar1,mamar1s)
colnames(mamar1s) <- c("ar1","phylum","long","lat","GS","loglike","kingdom","bodymass")
colnames(birdar1s) <- c("ar1","phylum","kingdom" ,"long","lat","GS","loglike","bodymass",'type')
birdar1 <- birdar1s[,-3]
mamar1s <- mamar1s[,-7]

mamar1s $type <- "mam"
##pic and pgls for bird mam 
mbalar <- unlist(mbar1s$ar1)
mbalar0 <- unlist(mbar1s0$ar1)

class(mambirdtree)
compute.brlen(mbtr)

library(datelife)
testchr <- datelife_search(input = mbtr, summary_format = "phylo_sdm")

testdatelife <- get_datelife_result(mbtr)
sdm <- datelife_result_sdm_matrix(testdatelife)
sdmphylo <- summary_matrix_to_phylo(sdm)
plot(sdmphylo)
class(sdmphylo)



length(sdmphylo)
MBpic.phy.response <- pic(mbalar0, sdmphylo, scaled = T )
length(BMpic.phy.response)
trait_bm <- mbar1s0$bodymass

trait_bm <- gsub("NaN", '0',trait_bm)
MBpic.trait <- pic (trait_bm, sdmphylo)
plot(MBpic.phy.response ~ MBpic.trait)
summary(lm(MBpic.phy.response ~ MBpic.trait))
testchr$node.label<-NULL

##pgls
mbdata <- data.frame(species= mbar1s0$GS, resp=mbalar0, trait=mbar1s0$bodymass)



mbdata$species <- sub(" ", "_", mbdata$species)
mbdata$phy.resp <- mbalar0

MBcomdata <- comparative.data(sdmphylo,mbdata,  species)
#class(testchr)
summary(pgls(phy.resp ~ trait, data = MBcomdata, lambda=1))
trdata$phy.resp <-  as.numeric(trdata$phy.resp)
response <- cbind(as.numeric( MBpic.phy.response),as.numeric( MBpic.trait))
colnames(response)<- c("response","trait")
response <- as.data.frame(response)

ggplot(response, aes(x = trait,y = response)) + geom_point()+ geom_smooth(method = lm)+ggtitle("mammal bird response to bodymass")


mbar1s0 <- subset(mbar1s,GS != "Dipodomys merriami" )



## now plot 1) mammal +bird mass vs ar1 (color)
ar1finan$BodyMass <- as.numeric(ar1finan$BodyMass)
ggplot(ar1finan, aes(x = BodyMass, y = ma1, colour = type))+ geom_point()+geom_smooth(method = lm)+
  ggtitle("mam+bird ar1 vs bodymass")+xlab("log(bodymass)")

write.csv(ar1sfin, file = "/Users/Apple/Desktop/ar1finan.csv")
write.csv(ar1fin0, file = "/Users/Apple/Desktop/ar1fin0.csv")
ggplot(ar1finan, (aes(x= ar1,fill = type)) )+ geom_histogram()+ ggtitle(" ar1 for length(TS) > 14 (scaled)") 

ggplot(ar1finan , (aes(x= ma1,y = BodyMass, color = type)) )+ geom_point() + geom_smooth( method = lm) +ggtitle("logbodymass vs ma1")
ar1fin0$maxh <- as.numeric(ar1fin0$maxh)
ggplot(ar1fin0 , (aes(x= ar1,y = maxh)) )+ geom_point() + geom_smooth( method = lm) +ggtitle("maxh vs ar1")
