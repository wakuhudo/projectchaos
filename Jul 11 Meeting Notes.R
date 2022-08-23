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

ar1s4 <- ar1s3 [,1:7]
ar1ab <- ar1ab[,1:7]
ar1sall <- rbind(ar1s4, ar1ab)

plhg0 <-data.frame()
plhg<-data.frame()
#pllist <- c("Botryococcus braunii")
pllist  <- unique(ar1sall$GS)
for (i in ar1s5$GS) {
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
  }
}
colnames(plhg)<- "maxplh"
ar1sall <-cbind(ar1s5,plhg)

fun7 <- function(x){unlist(strsplit(unlist(x)," "))[1]}
ar1sall$genus <- lapply(ar1sall$GS, fun7)
ar1sall$genus <- unlist(ar1sall$genus)
subspp <- unique(ar1sall$GS)
maminc <- data.frame()
for (i in subspp) {
  c <- subset(mamfun, GS == i)
  if(NROW(c) == 0){
    c <- subset(mamfun,genus == unlist(strsplit(unlist(i)," "))[1])
  }
  maminc  <- rbind(maminc, c)
}

ar1s5 <- data.frame()
for (i in pllist) {
  ar1s4mi <- subset(ar1sall, GS == i)
  median <- median(ar1s4mi$ar1)
  ar1s4mo <- subset(ar1s4mi, ar1 == median)
  ar1s5 <- rbind(ar1s5, ar1s4mo)
}
nrow(ar1s5)

birdfun$genus <- lapply(birdfun$GS,fun7)
birdinc <- data.frame()
for (i in subspp) {
  d <- subset(birdfun, GS == i)
  if(NROW(d) == 0){
    d <- subset(birdfun,genus == unlist(strsplit(unlist(i)," "))[1])
    
  }
  birdinc  <- rbind(birdinc, d)
}
birdinc$genus<- as.character(birdinc$genus)
bdinc <- birdinc[,2:1]
birdinc <- read.csv(file ="/Users/Apple/Desktop/birdinc.csv",header = T)
getwd()
mainc <- maminc[,22:25]
mainc1 <- mainc[,1]
mainc2 <- mainc[,4]
mainc <- cbind(mainc2,mainc1)
colnames(mainc) <- c("GS","BodyMass")
colnames(bdinc) <- c("GS","BodyMass")
mainc <- as.data.frame(mainc)
mamar0 <- data.frame()
for (i in mainc$GS) {
  mamar0 <- subset(ar1sall, GS == i)
  mamar1<- rbind(mamar1, mamar0)
}
mamar1 <- mamar1[1:11,]
mamar1 <- cbind(mamar1, mainc$BodyMass)

bdinc <- as.data.frame(bdinc)
mamar0 <- data.frame()
bdar1 <- data.frame()
for (i in bdinc$GS) {
  mamar0 <- subset(ar1sall, GS == i)
 
  bdar1<- rbind(bdar1, mamar0)
}
bdinc <-bdinc[1:68,]
bdinc1 <-data.frame()
bdinc2<- data.frame()
for (i in bdar1$GS) {
  bdinc2 <- subset(bdinc, GS == i)
  bdinc1 <- rbind(bdinc1, bdinc2)
}
NROW(bdinc1)

bdar1 <- cbind(bdar1, bdinc1$BodyMass.SpecLevel)

alltree <- read.nexus(file = "/Users/Apple/Desktop/Soltis_etal_639taxa_8cploci_rooted.dated.tre")
plotTree(alltree)

family<- data.frame()
for (i in plar1$GS) {
  b <- tax_name(query = c(i), get = c("family"), db = "ncbi")
  family <- rbind(family,b$family)
}
family[1,] <-c("Botryococcaceae")

library(V.PhyloMaker)
plar1 <- subset(ar1sall, kingdom == "Viridiplantae")
b<- as.data.frame (b)
example <- cbind(plar1$GS,plar1$genus,family)

colnames(example)<- c("species","genus","family")
# generate the phylogeny presented in Figure 1a

tree.a <- phylo.maker(sp.list = example, tree = GBOTB.extended, nodes = nodes.info.1, scenarios="S3")
class(tree.a)
write.tree(tree.a$scenario.3, "Figure.1a.tre")
treea<- read.tree(file = "/Users/Apple/Desktop/Figure.1a.tre")
plotTree(treea)
# generate the phylogeny presented in Figure 1b

#rel <- bind.relative(sp.list=example, tree=GBOTB.extended, nodes=nodes.info.1)

#tree.b <- phylo.maker(sp.list=rel$species.list, tree=rel$phylo, nodes=rel$nodes.info, scenarios="S3")

#write.tree(tree.b$scenario.3, "Figure.1b.tre")
##comparative 
alar <- plar1$ar1
trait <- plar1$maxplh
trdata <- data.frame(species=plar1$GS, resp=alar, trait=  trait)
c.data <- comparative.data(treea, trdata, species)
#summary(pgls(alar ~ trait, data=c.data, lambda="ML"))
pltree<-collapse.singles(pltree,root.edge=TRUE)
pltree <- root(pltree, outgroup = "'Botryococcus braunii'")
pltree <- multi2di(pltree)
is.binary.tree(treea)
is.rooted(treea)
plot(treea)

trait <- sim.char(treea, .5, model="BM", root=2)[,,1]
class(trait)
View(trait)
#colnamepl <- names(trait)
#trait <- 1:36
#names(trait) <- colnamepl
plar2 <- plar1[2:19,]
pldat <- data.frame()
a <- data.frame()
for (i in plar2$GS) {
  i <- gsub("'", '',i)
  a <- subset(plar1 , GS == i)
  a <- a[, c(1,6,8)]
  a$maxplh <- as.numeric(a$maxplh)
  #a <- c (mean(a$ar1),mean(a$phi),mean(a$maxplh),a$GS)
  pldat <- rbind(pldat , a)
}
ar1s3pl$GS <- gsub("gerardii", "gerardi", ar1s3pl$GS)


colnames(pldat) <- c("ar1","GS","maxplh")
trait <- pldat$maxplh
#names(trait) <- colnamepl
trait <-  as.numeric(trait)
plot(unlist(pldat$ar1) ~ unlist(pldat$maxplh))

alar <- pldat$ar1
trdata <- data.frame(species=pldat$GS, resp=alar, trait= unlist(pldat$maxplh))
c.data <- comparative.data(treea, trdata, species)
#summary(pgls(alar ~ trait, data=c.data, lambda="ML"))
treea<-collapse.singles(treea,root.edge=TRUE)
treea <- root(treea, outgroup = "'Botryococcus braunii'")
treea <- multi2di(treea)
is.binary.tree(pltree)
is.rooted(pltree)
plot(treea) 

pic.phy.response <- pic(alar, treea)
length(pic.phy.response)
as.numeric(pic.phy.response)
trait <- gsub("NaN", '0',trait)
pic.trait <- pic (trait, treea)
plot(pic.phy.response ~ pic.trait)
summary(lm(pic.phy.response ~ pic.trait))
treea$node.label<-NULL
trdata$phy.resp <- alar
trdata$species <- sub(" ", "_", trdata$species)
c.data <- comparative.data(treea, data = trdata, names.col =  species)
summary(pgls(phy.resp ~ trait, data=c.data, lambda="ML"))
trdata$phy.resp <-  as.numeric(trdata$phy.resp)
response <- cbind(as.numeric( pic.phy.response),as.numeric( pic.trait))
colnames(response)<- c("response","trait")
response <- as.data.frame(response)

ggplot(response, aes(x = trait,y = response)) + geom_point()+ geom_smooth(method = lm)+ggtitle("plant response to maxheight")

with(c.data$data, plot(phy.resp ~ trait))

mamar1$type <- "mammal"
colnames(mamar1) <- c("ar1","phylum","kingdom","long","lat","GS","loglike","maxplh",  "genus","BodyMass","type")

bdar1$type <- "bird"
colnames(bdar1) <- c("ar1","phylum","kingdom","long","lat","GS","loglike","maxplh",  "genus","BodyMass","type")

anar1 <- rbind(mamar1,bdar1)
anar1$ar1<-as.numeric(anar1$ar1)
anar1$BodyMass<-as.numeric(anar1$BodyMass)

ggplot(anar1, aes(x=type, y= ar1))+ geom_point()+geom_boxplot(alpha = 0.5)+ggtitle("bird vs mammal ar1")
anar1$BodyMass <- log10(anar1$BodyMass)
ggplot(anar1, aes(x=type, y= BodyMass))+ geom_point()+geom_boxplot(alpha = 0.5)+ggtitle("bird vs mammal Body Mass")

ggplot(anar1, aes(x=BodyMass, y= ar1, color = type))+ geom_point()+ggtitle("ar1 by bodymass")+ geom_smooth(method = lm, level = 0.9)
ggplot(ar1fin0, aes(x=maxh, y= ma1))+ geom_point()+ggtitle("ar1 by maxh")+ geom_smooth(method = lm, level = 0.95)
ggplot(ar1s3pl, aes(x=maxplh, y= ar1))+ geom_point()+ggtitle("ar1 by plant height")+ geom_smooth(method = lm, level = 0.85)+ theme_bw()+ ylim(-1,1)
t.test(ar1s3pl$ar1,ar1s3pl$maxplh, paired = T)
ar1finmam$BodyMass<- as.numeric(ar1finmam$BodyMass)

summary(lm(ar1~BodyMass, data = ar1finmam))
NROW(subset(ar1sall1, type == "mam"))

ggplot(ar1finan, aes(x=BodyMass, y= ma1, color = type))+ geom_point()+scale_color_manual(values = c("#4E84C4", "#FFDB6D"))+  theme(legend.position = "bottom")+
  ggtitle("ma1 by bodymass")+ geom_smooth(method = lm, level = 0.8)+theme_bw()
 
ggplot( ar1sall1, aes(x = phylum))+ geom_bar()

ar1finpl <- subset(ar1fin0, maxh != "NaN")
ar1finpl$BodyMass <- "NaN"
ar1finpl$type <- "plant"
ar1finva <- rbind(ar1finpl,ar1finan)
ggplot(ar1finva, aes(y= ma1,x = type, color = type))+ggtitle("ma1 by type")+ geom_boxplot(alpha = 0.5) +geom_jitter()
