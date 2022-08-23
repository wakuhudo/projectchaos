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
library(Chaos01)
#library(red)
fun4 <- function(x){
  format(round(x,0),nsmall = 2)
}
dated <- read.csv(file = "/Users/Apple/Desktop/dated.csv", header = T)
dated <- subset(dated, X != "6376208")
datedX <- subset(dated, ID_SPECIES == "45907")
dated <- dated[,2:42]
dated <- subset(dated, X != "6953242")
write.csv(dated, file = "/Users/Apple/Desktop/dated.csv")

dated$long <- lapply(dated$LONGITUDE, fun4)
dated$lat <- lapply(dated$LATITUDE, fun4)
dated$lat <- as.numeric(dated$lat)
dated$long <- as.numeric(dated$long)
##create date base on year, month, day 

dateds$Year2 <- paste(dateds$YEAR, dateds$MONTH, dateds$DAY)
dated <- dated %>% drop_na(MONTH)
dated$Year2ym <- transform(dated, ym = as.Date(as.character(Year2)))
dated$date<- within(dated, Date <- sprintf("%1d-%2d-%3d", YEAR, MONTH,DAY))
dated$date <- as.Date(dated$date$Date)

datedts3 <- subset(datedts3, datedts3$sum.allrawdata.BIOMASS <10 )

AICs2 <- data.frame()
Qs1 <- data.frame()
resp1 <- data.frame()
numstudies <- unique(dated$STUDY_ID)
longphits <- data.frame()
#ARcum1 <- data.frame() # dataframe to contain AR values
#Icum1 <- data.frame() #integration values 
#MAcum1 <- data.frame() #MA values stored here 
GS1 <- data.frame()
ar1s <- data.frame()
#longts <- data.frame()
lat <- data.frame()
long <- data.frame()
intercept <- data.frame()
loglik <- data.frame()
plotnum <- data.frame()
glsphi <- data.frame()
ma1 <- data.frame()
Kc <- data.frame()
ss<- data.frame()
# loop 1: seperate by study id )removing this now because we are refining by coordinates 
#for (i in numstudies) {
#  dateds <- subset(dated, STUDY_ID == i)
#  dateds <- subset(dateds, GENUS_SPECIES != "Unknown") 
  sublat <- unique(longtsab$lat)
  #loop2 : seperate by coordinates 
  for (n in sublat){
    dateds1 <-  subset(longtsab, lat == n)
    sublong <- unique(dateds1$long)
    
    for (m in sublong) {
      dateds2 <- subset(dateds1, long == m)
      subplot <- unique(dateds2$PLOT)
      
      #loop 3: separate by plot 
      for (k in subplot) {
      dateds3 <- subset(dateds2, PLOT == k)
      subspp <- unique(dateds3$GENUS_SPECIES)
      
      #loop 4: separate by spp 
      for (j in subspp) {
        
          datedts3 <- subset(dateds2, GENUS_SPECIES == j)
          datedts <- arrange(datedts3, Year2)
        #now isolate Biomass and Year2
          datedts <- cbind( datedts[11], datedts$Year2ym$ym )
          
          datedts$sum.allrawdata.ABUNDANCE<-as.data.frame(datedts$sum.allrawdata.ABUNDANCE)
          datedts$sum.allrawdata.ABUNDANCE <-  as.numeric(unlist(datedts$sum.allrawdata.ABUNDANCE))
        
          datedtsmon <- aggregate(datedts$sum.allrawdata.ABUNDANCE, list(datedts$`datedts$Year2ym$ym`), FUN=mean)
        #scale here 
        #  datedtsmon$x <- lapply(datedtsmon$x, fun2)
       # datedts3 <- subset(dateds3, GENUS_SPECIES == j)
      #  datedts <- arrange(datedts3, Year2)
        #now isolate Biomass and Year2
      #  datedts <- datedts[,c(12,17)]
      #  datedts$sum.allrawdata.BIOMASS<-as.data.frame(datedts$sum.allrawdata.BIOMASS)
        
      #  datedts$Year2ym <- as.data.frame(datedts$Year2ym)
      #  datedts2 <- as.data.frame(datedts$sum.allrawdata.BIOMASS)
      #  datedts2 <- cbind(datedts2, datedts$Year2ym)
        
       # datedtsmon <- aggregate(datedts2$`datedts$sum.allrawdata.BIOMASS`, list(datedts2$Year2), FUN=mean)
        #scale here 
        
        fun10 <- function(x){
          x/max(datedts$ABUNDANCE)
        }
        
        if(nrow(datedtsmon) > 14){
          colnames(datedtsmon) <- c("date", "ABUNDANCE")
          
         # datedtsmon$x <- lapply(datedtsmon$Biomass, fun10)
          
          #make things cleaner 
          #insert NAs to missing months 
        #  colnames(datedtsmon) <- c("date", "Biomass")
          datedtsmon$date <-  as.Date(as.yearmon(datedtsmon$date))
          
          all_dates <- seq(as.Date(as.yearmon(min(datedtsmon$date))), as.Date(as.yearmon(max(datedtsmon$date))), by="month")
          
          datedtsmon$date <-  as.Date(as.yearmon(datedtsmon$date))
          
          ##obtain NA contained ts by month
          datedtsmon2 <- merge(data.frame(date = all_dates), datedtsmon, by.x='date',
                               all.x=T,)
          datedtsmon2$rown<- seq.int(nrow(datedtsmon2)) 
          # now introduce arima test 
          ts0 <- as.numeric(unlist(datedtsmon2$ABUNDANCE)) 
          fit0 <- auto.arima(ts0, method = "ML")
          if(fit0$arma[1] >0){
         # fit1 <- arima(ts0, order = c(0,0,1), method = "ML")
        #  fit2 <- arima(ts0, order = c(1,0,0), method = "ML")
          #  fit <- arima(ts0, method = "ML", order = c(1,0,0))
                ARcum1 <- rbind(fit0$arma[1], ARcum1)
            #    Icum1 <- rbind(fit$arma[6], Icum1)
            #    MAcum1 <- rbind(fit$arma[2], MAcum1)
            AICs2 <- rbind(AICs2, fit0$aic)
            #gls
      #      datedtsmon2$biomass <- unlist(datedtsmon2$Biomass)
      #      datedtsmon2 <- na.omit(datedtsmon2)
            
            
      #      res1 <- checkresiduals(fit1, plot = F)
      #      ar1s <- rbind( ar1s,fit2$coef[1])
      #      ma1 <- rbind(ma1, fit1$coef[1])
            
            #intercept <- rbind(intercept,fit1$coef[2])
            #resp1 <- rbind(res1$p.value, resp1) 
      #      GS1 <- rbind( GS1,datedts3[1,]$GENUS_SPECIES)
      #      lat <- rbind(lat,n)
      #      long <- rbind(long,m)
            loglikab <- rbind(loglikab,fit0$loglik)
      #      plotnum <- rbind(plotnum, k)
      #      longts <- rbind(longts,datedts3)
      
         # Kc0 <-  testChaos01(datedts)

          #Kc<- rbind(Kc, Kc0)
      #    ss0 <- NROW(datedtsmon)
      #    ss <- rbind(ss, ss0)
      #    datedtsmon2$biomass <- unlist(datedtsmon2$Biomass)
      #    datedtsmon2 <- na.omit(datedtsmon2)
      #      if (var((datedtsmon2$biomass)) !=0 ){
             
      #      gls <- gls(biomass ~ rown, correlation=corARMA(p =1),data = datedtsmon2, na.action = na.omit)
            
      #      glsphi <- rbind (glsphi ,gls$modelStruct$corStruct)
      #      longphits <- rbind(longphits, datedts3)
      #      }
      #    else{}
      #    } else{}}
      #    else{}
      }}
      }
    }
  }
}
  colnames(ss) <- "samplesize"
  colnames(Kc) <-"Kc"
  Ar1sbm <- cbind(Ar1sbm, ss)  
  ar1ab <- cbind(ar1ab,ss)
  ar1sall1 <- rbind(Ar1sbm,ar1ab)
  #install.packages("BIEN")
library(BIEN)
library(terra)
  ar1sall1<- ar1sall1[,-11]
  ggplot(ar1sall1,aes( x = Kc)) + geom_histogram() +ggtitle("Kc Distibution")
  ggplot(ar1sall1, aes( x = samplesize)) + geom_histogram()+xlim(0,250) +ggtitle("SampleSize Distribution")

  ggplot(ar1sall1,aes( x = Kc)) + geom_histogram() +ggtitle("Kc Distibution")
  colnames( ARcum1) <- "AR"
  ggplot(ARcum1, aes(x = AR)) + geom_histogram() + ggtitle("AR distribution")
  library(scales)
  
  ggplot(ARcum1, aes(x = as.factor(AR), fill = 1)) +
    geom_bar(aes(y = (..count..)/sum(..count..))) +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
    scale_y_continuous(labels = percent) +
    labs(title = "Best-Fit AR Distribution", y = "Percent", x = "AR Order")+ theme_bw()+ theme(legend.position = "none")
d<- subset(d, ma1 > - 0.75 )  
  d <- as.data.frame(d)

  ARcum1 <- read.csv( file ="/Users/Apple/Desktop/ARcum1.csv" ,header = T)
Ar1sbm <- ar1s
colnames(ar1s) <- "ar1"
ggplot(d, (aes(x= ma1)) )+ geom_histogram()+ ggtitle("BM ar1 for length(TS) > 14 (scaled)")

summary( cor(ar1fin0$ar1, ar1fin0$ma1))
library(ggpubr)
cor.test(ar1fin0$ar1, ar1fin0$ma1, method = "pearson")
 write.csv( file ="/Users/Apple/Desktop/AR1fin.csv" ,ar1fin0)

ggplot(ar1fin0, aes(x = ma1))+ geom_density()+ ggtitle("ma1 Distribution")+
  theme_bw()+theme(legend.position = "none")

library(ggridges)
ggplot(ar1finva, aes(x= ar1, y = type, group = type )) + geom_density_ridges2(fill = "12", alpha = 0.5)+theme_bw()+theme(legend.position = "none")+
  ggtitle("Ar1 distribution by type")+ xlim(-1,1)

phyla <- data_frame()
tax_name(sci = GS1)
colnames(GS1) <- GS
library(taxize)
for (i in GS1) {
b <- tax_name(query = c(i), get = c("phylum"), db = "ncbi")
a <- tax_name(query = c(i), get = c("kingdom"), db = "ncbi")
}
#phyla <-  tax_name ( query = c(GS1), get = c("phylum"), db = "ncbi")
#kindomsm <-  tax_name(c(GS1), get = c("kingdom"), db = "ncbi")
#unlist(GS1)
plot(treea)

ape::write.tree(treea, file='/Users/Apple/Desktop/pltr.txt')

phyla1 <- b$phylum
kingdomsm <-  a$kingdom
ar1s <- cbind(ar1s, phyla1)
ar1s <- cbind(ar1s, kingdomsm)
ar1s <- cbind(ar1s,ma1,lat, long, GS1, loglik, glsphi)
#lm(ar1 ~ lats )
colnames(ar1s) <- c("ar1","ma1","lat","long","GS", "loglike", "phi","phylum","kingdom")
#ar1BM <- ar1s
#longtsBM <- longts
## chordates vs arthropods 
ggplot(ar1s, aes(x = ar1, fill = kingdom))+ geom_histogram()+ ggtitle("BM ar1 by kingdom")
ggplot(ar1s, aes(x = ar1, fill = phylum))+ geom_histogram()+ ggtitle("BM ar1 by phylum")

ggplot(ar1s, aes(x = phylum, y = ar1)) + geom_jitter( position=position_jitter(0.2))+
  geom_boxplot(alpha = 0.5)+ ggtitle("BM ar1 by phylum jitter box")  

ggplot(ar1s, aes(x = kingdom, y = ar1)) + geom_jitter( position=position_jitter(0.2))+
  geom_boxplot(alpha = 0.5)+ ggtitle("BM ar1 by kingdom jitter box")  

ggplot(ar1s, aes(y = ar1, x = long)) + geom_jitter()+ xlim(-180,180)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

ggplot(ar1s, aes(x = ar1, y = phi))+ geom_point()+ylim(-3,5)+geom_smooth()+ggtitle("phi by ar1")
ggplot(ar1s, aes(x = phi, fill = phylum))+ geom_histogram()+ggtitle("phi by phylum")+ xlim(-3,6)

phi <- cbind(glsphi, y)
colnames(phi) <- c("phi","y")
fun6 <- function(y){y^2}

phi$y2 <- lapply (phi$y, fun6)
phi$y2 <- as.numeric(phi$y2)
cor(phi$phi, (phi$y2)^4) 
 0.2714
ggplot(phi,aes(x = y2, y = phi)) + stat_smooth()
 
glsphi <- cbind(glsphi, GS1, lat, long)
colnames(glsphi) <- c("phi","GS","lat","long")

for (i in GS1) {
  b <- tax_name(query = c(i), get = c("phylum","kingdom"), db = "ncbi")
}
phylum <- b$phylum
kingdom<-b$kingdom
glsphi <- cbind(glsphi, GS1, lat, long, phylum,kingdom)
glsphi <- glsphi[,c(1:4,8,9)]
colnames(glsphi) <- c("phi","GS","lat","long","phylum","kingdom")
longtsphi <-  longts

ggplot(glsphi, (aes(x= phi)) )+ geom_histogram()+ ggtitle("BM phi for length(TS) > 14 (scaled)")
ggplot(glsphi, aes(y = phi, x = long)) + geom_jitter()+ xlim(-180,180)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

ggplot(glsphi, aes(y= phi, x = lat)) + geom_jitter()+ #xlim(-90,90)+
  stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1)

ggplot(glsphi1, aes(x = kingdom, y = phi)) + geom_jitter( position=position_jitter(0.2))+
  geom_boxplot(alpha = 0.5)+ ggtitle("BM phi by kingdom jitter box")  

ggplot(glsphi1, aes(x = phylum, y = phi)) + geom_jitter( position=position_jitter(0.2))+
  geom_boxplot(alpha = 0.5)+ ggtitle("BM phi by phylum jitter box")  

glsphi1 <- subset(glsphi, phylum != "Streptophyta")



API <- "https://apiv3.iucnredlist.org"
IUCN_status <- iucn_summary(c("Lynx lynx"), key = API)
unlist(IUCN_status)
iucn_status(IUCN_status)
iucn_id("Lynx lynx", key = API)

ar1sbm <- ar1s

##gls 
gls(sum.allrawdata.BIOMASS ~ date, correlation=corAR1(form=~t), data = longtsBM)

IUCN_REDLIST_KEY<-"https://apiv3.iucnredlist.org"
rl_search("lynx lynx")

##wait for api
mamfun <- mamfun %>% drop_na(Diet.Vend)

mamfun$GS <- paste(mamfun$Scientific, mamfun$MSWFamilyLatin)
mamfun <- mamfun[,c(1,4:27)]
colnames(mamfun) <- c("MSW3_ID","Family",colnames(mamfun)[2:15],colnames(mamfun)[17:25])
##match with GS1 

subsppBM <- unique(ar1BM$GS)
maminc <- data.frame()
for (i in subsppBM) {
  c <- subset(mamfun, GS == i)
  maminc  <- rbind(maminc, c)
}
birdfun <- read.delim(url("https://esapubs.org/archive/ecol/E095/178/BirdFuncDat.txt"),header = T,sep = "")
birdfun$GS <- paste(birdfun$Scientific,birdfun$English)
birdinc <- data.frame()
for(i in subsppBM){
  c <- subset(birdfun, GS == i)
  birdinc <- rbind(birdinc, c)
}
mamar1 <- data.frame()

for (i in unique(maminc$GS)) {
  mamar1 <- rbind(mamar1, subset(ar1BM, GS == i))
  
}

ggplot(mamar1, aes(x = GS, y = ar1, fill= GS))+ geom_point(alpha = 0.8)+geom_boxplot(alpha = 0.5)+
  ggtitle("four mammal spp. included in BIotime")
summary (lm(ar1 ~ BM, data = mamar1))


##bien
d <- BIEN_trait_species("Vernonia baldwinii")
e <- data.frame()
plh <- data.frame()
for (i in ar1BM$GS){
  d <- BIEN_trait_species( i)
  e <- subset(d, trait_name == "maximum whole plant height")
  plh <- rbind(plh, e)
}
phar1 <- data.frame()
for (i in plh$scrubbed_species_binomial){
  d <- subset(ar1BM, GS == i)
  d$maxh <- subset(plh, scrubbed_species_binomial == i)$trait_value
  phar1 <- rbind(phar1,d)
}
phar1$maxh <-as.numeric(phar1$maxh)

summary(lm(ar1~maxh, data = phar1))


fun4 <-function(x){
  format(round(x,0),nsmall = 2)
}
##hows AB doing? 

data01$long <- 1
data01$lat <- 1
data01$long <- lapply(data01$LONGITUDE, fun4)
data01$lat <- lapply(data01$LATITUDE, fun4)
data01$lat <- as.numeric(data01$lat)
data01$long <- as.numeric(data01$long)
##create date base on year, month, day 

#data0s$Year2 <- paste(dateds$YEAR, dateds$MONTH, dateds$DAY)
#data10 <- data0 %>% drop_na(MONTH)
#data0$Year2ym <- transform(data0, ym = as.Date(as.character(Year2)))
#data0$date<- within(data0, Date <- sprintf("%1d-%2d-%3d", YEAR, MONTH,DAY))
#data0$date <- as.Date(data0$date$Date)
#data01 <- data0

#  data0$PLOT[is.na(data0$PLOT)] <- "ABA"
#  data0 <- data01[c(1:6099780),]
data0X <- subset(data0, STUDY_ID == 538)
data0 <- subset(data0, STUDY_ID != 538)
data0 <- subset(data0, X != 7653210)

#AICs1 <- data.frame()
Qsab <- data.frame()
respab <- data.frame()
#numstudies <- unique(dat$STUDY_ID)
#ARcum1 <- data.frame() # dataframe to contain AR values
#Icum1 <- data.frame() #integration values 
#MAcum1 <- data.frame() #MA values stored here 
GSab <- data.frame()
ar1ab <- data.frame()
longtsab <- data.frame()
latab <- data.frame()
longab <- data.frame()
interceptab <- data.frame()
loglikab <- data.frame()
plotnumab <- data.frame()
longphitsab <- data.frame()
glsphi2 <- data.frame()
ma1ab <- data.frame()
# loop 1: seperate by study id )removing this now because we are refining by coordinates 
#for (i in numstudies) {
#  dateds <- subset(dated, STUDY_ID == i)
#  dateds <- subset(dateds, GENUS_SPECIES != "Unknown") 
sublatab <- unique(data0$lat)
#loop2 : seperate by coordinates 
for (n in sublatab){
  dateds1 <-  subset(data0, lat == n)
  sublong <- unique(dateds1$long)
  
  for (m in sublong) {
    dateds2 <- subset(dateds1, long == m)
    subplot <- unique(dateds2$PLOT)
    
    #loop 3: separate by plot 
    for (k in subplot) {
      dateds3 <- subset(dateds2, PLOT == k)
      subspp <- unique(dateds3$GENUS_SPECIES)
      
      #loop 4: separate by spp 
      for (j in subspp) {
        
        #  datedts3 <- subset(dateds2, GENUS_SPECIES == j)
        #  datedts <- arrange(datedts3, Year2)
        #now isolate Biomass and Year2
        #  datedts <- datedts[,c(12,21)]
        #  datedts$sum.allrawdata.BIOMASS<-as.data.frame(datedts$sum.allrawdata.BIOMASS)
        #  datedts$sum.allrawdata.BIOMASS <-  as.numeric(unlist(datedts$sum.allrawdata.BIOMASS))
        
        #  datedtsmon <- aggregate(datedts$sum.allrawdata.BIOMASS, list(datedts$date), FUN=mean)
        #scale here 
        #  datedtsmon$x <- lapply(datedtsmon$x, fun2)
        datedts3 <- subset(dateds3, GENUS_SPECIES == j)
        datedts <- arrange(datedts3, Year2)
        #now isolate Biomass and Year2
        datedts <- datedts[,c(11,17)]
        
        datedts$sum.allrawdata.ABUNDANCE<-as.data.frame(datedts$sum.allrawdata.ABUNDANCE)
        
        datedts$Year2ym <- as.data.frame(datedts$Year2ym$ym)
        datedts2 <- as.data.frame(datedts$sum.allrawdata.ABUNDANCE)
        datedts2 <- cbind(datedts2, datedts$Year2ym)
        datedtsmon <- aggregate(datedts2$`datedts$sum.allrawdata.ABUNDANCE`, list(datedts2$`datedts$Year2ym$ym`), FUN=mean, na.rm = T)
        #scale here 
     
        
        if(nrow(datedtsmon) > 14){
          datedtsmon$x <- lapply(datedtsmon$x, fun2)
          
          #make things cleaner 
          #insert NAs to missing months 
          colnames(datedtsmon) <- c("date", "Abundance")
          #print(datedtsmon)
          datedtsmon$date <-  as.Date(as.yearmon(datedtsmon$date))
          
          all_dates <- seq(as.Date(as.yearmon(min(datedtsmon$date))), as.Date(as.yearmon(max(datedtsmon$date))), by="month")
          
          datedtsmon$date <-  as.Date(as.yearmon(datedtsmon$date))
          
          ##obtain NA contained ts by month
          datedtsmon2 <- merge(data.frame(date = all_dates), datedtsmon, by.x='date',
                               all.x=T,)
          
          datedtsmon2$rown<- seq.int(nrow(datedtsmon2)) 
          
          
          # now introduce arima test 
          ts0 <- as.numeric(unlist(datedtsmon2$Abundance)) 
          fit1 <- auto.arima(ts0, method = "ML")
          
          if(fit1$arma[1] != 0){
         #   fit <- arima(ts0, method = "ML", order = c(1,0,0))
            #    ARcum1 <- rbind(fit$arma[1], ARcum1)
            #    Icum1 <- rbind(fit$arma[6], Icum1)
            #    MAcum1 <- rbind(fit$arma[2], MAcum1)
          fit1  <- arima(ts0, order = c(1,0,0), method = "ML")
          fit2 <- arima(ts0, order = c(0,0,1), method = "ML")
            
            resab <- checkresiduals(fit1, plot = F)
            ar1ab <- rbind( ar1ab,fit1$coef[1])
            ma1ab <- rbind(ma1ab, fit2$coef[1])
            #interceptab <- rbind(interceptab,fit$coef[2])
        #    respab <- rbind(respab$p.value, resp1) 
            GSab <- rbind( GSab,datedts3[1,]$GENUS_SPECIES)
            latab <- rbind(latab,n)
            longab <- rbind(longab,m)
            loglikab <- rbind(loglikab,fit1$loglik)
            plotnumab <- rbind(plotnumab, k)
            longtsab <- rbind(longtsab,datedts3)
          
 
          
          datedtsmon2$abundance <- unlist(datedtsmon2$Abundance)
          datedtsmon2 <- na.omit(datedtsmon2)
          class(datedtsmon2$rown)
          if (var((datedtsmon2$abundance)) !=0 ){
            
            gls <- gls(abundance ~ rown, correlation=corARMA(p =1),data = datedtsmon2, na.action = na.omit)
            
            glsphi2 <- rbind (glsphi2 ,gls$modelStruct$corStruct)
            longphitsab <- rbind(longphitsab, datedts3)
          }
          else{}
        } else{}}
      else{}
      }
    }
  }}

#}
longtssave <- longts 

colnames(ar1ab) <- "ar1"
ggplot(ar1ab, (aes(x= ar1)) )+ geom_histogram()+ ggtitle("AB ar1 for length(TS) > 14 (scaled)") + xlim(-1,1)
colnames(ma1ab) <-"ma1"
ggplot(ma1ab, (aes(x= ma1)) )+ geom_histogram()+ ggtitle("AB ar1 for length(TS) > 14 (scaled)") 


phylaab <- data_frame()


for (i in GSab) {
  
  
  abbb <- tax_name(query = c(i), get = c("phylum"), db = "ncbi")
  abaa <- tax_name(query = c(i), get = c("kingdom"), db = "ncbi")
}


#phyla <-  tax_name ( query = c(GS1), get = c("phylum"), db = "ncbi")
#kindomsm <-  tax_name(c(GS1), get = c("kingdom"), db = "ncbi")


phylaab <- abbb$phylum
kingdomsm <-  abaa$kingdom
ar1ab <- cbind(ar1ab, phylaab)
ar1ab <- cbind(ar1ab, kingdomsm)
ar1ab <- cbind(ar1ab,ma1ab,latab, longab, GSab, loglikab, glsphi2, phylaab, kingdomsm)
#lm(ar1 ~ lats )
colnames(ar1ab) <- c("ar1","ma1" ,"lat","long","GS","loglike", "phi","phylum","kingdom")

ar1fin <- rbind(ar1s,ar1ab)

## chordates vs arthropods 
ggplot(ar1ab, aes(x = ar1, fill = kingdom))+ geom_histogram()+ ggtitle("AB ar1 by kingdom")
ggplot(ar1ab, aes(x = ar1, fill = phylum))+ geom_histogram()+ ggtitle("AB ar1 by phylum")

ggplot(ar1ab, aes(x = phylum, y = ar1)) + geom_jitter( position=position_jitter(0.2))+
  geom_boxplot(alpha = 0.5)+ ggtitle("AB ar1 by phylum jitter box")  

ggplot(ar1ab, aes(x = kingdom, y = ar1)) + geom_jitter( position=position_jitter(0.2))+
  geom_boxplot(alpha = 0.5)+ ggtitle("AB ar1 by kingdom jitter box")  


API <- "https://apiv3.iucnredlist.org"
IUCN_status <- iucn_summary(c("Lynx lynx"), key = API)
unlist(IUCN_status)
iucn_status(IUCN_status)
iucn_id("Lynx lynx", key = API)



a <-subset(ar1BM,phylum == "Chordata")


