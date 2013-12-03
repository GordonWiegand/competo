#Haushalte
rm(list=ls())
#########################################################################################################

#Intro
#########################################################################################################
#Das Workingdirectory ist anzupassen. Sonstige Parameter auch. Ausgabe des des winzorisierten Mittelwertes einschliesslich Konfidenzintervall ins working directory.
#########################################################################################################

#Parameter definieren
#########################################################################################################
setwd("/Volumes/hd2/131021_briefmarken4/")  
tri <- .1 #Trimmbereich 10%
tri2 <- .15 #Trimmbereich für Bootstrapping. Aus "Kosmetikgründen" etwas höher.
runden <- 10000
plotZens <- 300 #Abschneiden bei den Densityplots aus Lesbarkeit. Maximalwert. (alles grösser gleich Max.)
#########################################################################################################

#Daten vorbereiten
#########################################################################################################
library(foreign)
d0 <- as.data.frame(read.spss("bmhh.sav"))
d0[d0<0] <- NA  #NAs kodiert als -99 -99.99 -90 als NA

#Raking. Randsummen vom BFS
d0$f <- logical(nrow(d0)) #Aim: sum F4a,F4b, Problem: NA, 1st step flag NA's
d0$f[is.na(d0$Frage_4a)] <- T
d0$f <- !d0$f
d0$Frage_4a[is.na(d0$Frage_4a)] <- 0
d0$Frage_4b[is.na(d0$Frage_4b)] <- 0
d0$Frage_4 <- d0$Frage_4a + d0$Frage_4b
d0$Frage_4[d0$Frage_4>5] <- 5
d0$f[d0$Frage_4==0] <- F #Size household, all zeros flagged by d0$f
d0$Sprache <- as.numeric(d0$Sprache)
d0$w <- numeric(nrow(d0))
d0$Frage_4[d0$Frage_4==0] <- 1
listHH <- c(P1=0.359793285, P2=0.3164922642, P3=0.1293573859, 
            P4=0.1316042884 , P5=0.0627527765) #source: bfs 2010
listRE <- c(DE=0.641,FR=0.204,IT=0.065) * 1/.91 #source: bfs 2010 
for (i in 1:length(listHH)) {
  for (j in 1:length(listRE))
        d0$w[d0$Frage_4==i & d0$Sprache==j] <- 
          (listRE[j] * listHH[i] * nrow(d0))/
           sum(d0$Frage_4==i & d0$Sprache==j)
  }
dh <- as.data.frame(cbind(d0$Frage_2,d0$w)) #Als Arbeitsdatensatz nur noch Gewichte und Briefmarken
colnames(dh) <- c("bm","w")
#########################################################################################################

#Define function
#########################################################################################################
#Compute Winsorized Mean
win<-function(dw,tr){
        nd <- nrow(dw)
	dw<-dw[order(dw$bm),]
	l<-round(tr*nd)
	dw$bm[1:l]<-dw$bm[l]
	dw$bm[nd:(nd-l)]<-dw$bm[(nd-l)]
	weighted.mean(dw$bm,dw$w)
}

#Confidence Intervalls
winMeanVar <- function(ds,r,tr){ #ds Datensatz, r Runden, tr trimbereich
    out <- numeric(r)
    for (i in 1:r){
        dwin <- ds[sample(nrow(ds),nrow(ds),replace=T),]
        out[i] <- win(dwin,tr)
    }
    print(sqrt(var(out))*1.96/mean(out))
}
#########################################################################################################

#Output
#########################################################################################################
print("Win Mean")
win(dh,tri)
print("Konfidenzintervall")
winMeanVar(dh,runden,tri2)
#zusätzliche Informationen
print("Top 100 Einheiten")
sort(dh$bm,decreasing=T)[1:100] #Top 100 Briefmarkenwerte
print("Anzahl Einheiten ohne Briefmarken")
table(dh$bm==0) #Anzahl Einheiten ohne Briefmarkenvorrat
print("Anzahl Einheiten")
nrow(dh) #Anzahl Einheiten
print("Anzahl Einheiten mit gültigen BM Werten")
table(!is.na(dh$bm)) #Anzahl Einheiten mit gültigen BM Werten 
print("Arithm. Mittel")
weighted.mean(dh$bm,dh$w,na.rm=T) #Arith. Mittel 
print("Anteil Top 25")
sum(sort(dh$bm,decreasing=T)[1:round(.1*nrow(dh),0)])/sum(dh$bm,na.rm=T) #Anteil an BM der Top 10%
#########################################################################################################

#Plots
#########################################################################################################
library(ggplot2)
#Haushalte
dPlot <- dh[!is.na(dh$bm),] #NAs raus
dPlot$bm[dPlot$bm>plotZens] <- plotZens #bei plotZens abschneiden wg Lesbarkeit
jpeg("h_dens.jpg",quality=100)
    ggplot(dPlot, aes(x=bm)) + 
        geom_histogram(aes(y=..density..),
                       binwidth=20,                 # Bins bei 20 Franken
                       colour="black", fill="white") +
        xlab("Briefmarken in CHF") +
        ylab("Dichte") +
        theme(axis.text.y=element_blank()) +
        geom_density(alpha=.2, fill="yellow") #alpha ist "Durchsichtigkeit"
dev.off()
#########################################################################################################

