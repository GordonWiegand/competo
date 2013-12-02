rm(list=ls())
setwd("/Volumes/hd2/131021_briefmarken4/")  
library(foreign)
d0 <- as.data.frame(read.spss("bmuv.sav"))
d0[d0<0] <- NA
#Impute NAs by estimeated values
d0$f2a[d0$f2b==1] <- mean(d0$f2a[(d0$f2a>0 & d0$f2a<51)],na.rm=T)
d0$f2a[d0$f2b==2] <- mean(d0$f2a[(d0$f2a>50 & d0$f2a<101)],na.rm=T)
d0$f2a[d0$f2b==3] <- mean(d0$f2a[(d0$f2a>100 & d0$f2a<301)],na.rm=T)
d0$f2a[d0$f2b==4] <- mean(d0$f2a[(d0$f2a>300 & d0$f2a<501)],na.rm=T)
d0$f2a[d0$f2b==5] <- mean(d0$f2a[(d0$f2a>500 & d0$f2a<1001)],na.rm=T)
d0$f2a[d0$f2b==6] <- mean(d0$f2a[(d0$f2a>1000 & d0$f2a<2001)],na.rm=T)
d0$f2a[d0$f2b==7] <- mean(d0$f2a[d0$f2a>2000],na.rm=T)
#working data sef
du <- d0[as.character(d0$q0)=="Unternehmen",c("f2a","gew")] #companies
du[1] <- as.numeric(as.matrix(du[1]))
du[2] <- as.numeric(as.matrix(du[2]))
dv <- d0[as.character(d0$q0)=="Vereine",c("f2a","gew")] #associations
dv[1] <- as.numeric(as.matrix(dv[1]))
dv[2] <- as.numeric(as.matrix(dv[2]))
names(du)<-c("bm","w")
names(dv)<-c("bm","w")

#Define function: Compute Winsorized Mean
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
        d <- ds[sample(nrow(ds),nrow(ds),replace=T),]
        out[i] <- win(d,tr)
    }
    print((sqrt(var(out))*1.96)/win(du,tr))
}

print("Unternehmen")
win(du,.1)
winMeanVar(du,10000,.15)

print("Vereine")
win(dv,.1)
winMeanVar(dv,10000,.15)

#Plots
#############################################################

library(ggplot2)


#Unternehmen
dPlot <- du[!is.na(du$bm),] #NAs raus
dPlot$bm[dPlot$bm>300] <- 300 #bei 300 abschneiden wg Lesbarkeit
jpeg("u_dens.jpg",quality=100)
    ggplot(dPlot, aes(x=bm)) + 
        geom_histogram(aes(y=..density..),
                       binwidth=20,                 # Bins bei 20 Franken
                       colour="black", fill="white") +
        xlab("Briefmarken in CHF") +
        ylab("Dichte") +
        theme(axis.text.y=element_blank()) +
        geom_density(alpha=.2, fill="yellow") #alpha ist "Durchsichtigkeit"
dev.off()

#Vereine
dPlot <- dv[!is.na(dv$bm),] #NAs raus
dPlot$bm[dPlot$bm>300] <- 300 #bei 300 abschneiden wg Lesbarkeit
jpeg("v_dens.jpg",quality=100)
    ggplot(dPlot, aes(x=bm)) + 
        geom_histogram(aes(y=..density..),
                       binwidth=20,                 # Bins bei 20 Franken
                       colour="black", fill="white") +
        xlab("Briefmarken in CHF") +
        ylab("Dichte") +
        theme(axis.text.y=element_blank()) +
        geom_density(alpha=.2, fill="yellow") #alpha ist "Durchsichtigkeit"
dev.off()
