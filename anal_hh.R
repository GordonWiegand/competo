rm(list=ls())
library(foreign)
d0 <- as.data.frame(read.spss("/Volumes/hd2/130100_competo2/Competo_2013_mitSprachversion.sav"))
d0[d0<0] <- NA
#cleaning up
raus <- c(24,55,161,351,362,373:378,390,491,528,831,982)
for (i in 1:length(raus)) d0 <- d0[d0$FB_Nr != raus[i],]
####################################
##Raking                          ##
####################################
#Aim: sum F4a,F4b, Problem: NA, 1st step flag NA's
d0$f <- logical(nrow(d0))
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

####################################
#Define function: Compute Winsorized Mean
####################################
win<-function(dw,tr){
        nd <- nrow(dw)
	dw<-dw[order(dw$bm),]
	l<-round(tr*nd)
	dw$bm[1:l]<-dw$bm[l]
	dw$bm[nd:(nd-l)]<-dw$bm[(nd-l)]
	weighted.mean(dw$bm,dw$w)
}
dh <- as.data.frame(cbind(d0$Frage_2,d0$w))
colnames(dh) <- c("bm","w")
#That's the line
win(dh,.1)

####################################
#get the Plots: all
library(ggplot2)
pdf(file="/Volumes/hd2/130100_competo2/density_h_alle.pdf")
ggplot(dh, aes(x=bm)) + geom_density() + xlab("Briefmarken in CHF")
dev.off()
#get the Plots:loup 
dh500 <- dh[dh$bm<500,]
pdf(file="/Volumes/hd2/130100_competo2/density_h_500.pdf")
ggplot(dh500, aes(x=bm)) + geom_density() + xlab("Briefmarken in CHF")
dev.off()

###########################################
#Bootstrap Confidence Intervalls
###########################################
#Companies
d <- dh
outci <- double(10000)
nrd <- nrow(d)
#Heyho, simulate!
for (i in 1:10000){
	ds<-d[sample(nrd,size=nrd,replace=T),]
	names(ds) <- c("bm","w")
	outci[i]<-win(ds,.15)
}
#Produce Output
outi <- double(4)
names(outi) <- c("mean","msig","psig","p" )
outi[1] <- mean(outci)
outi[2] <- mean(outci)-1.96*sqrt(var(outci))
outi[3] <- mean(outci)+1.96*sqrt(var(outci))
outi[4] <- ((outi[1]-outi[2])*100)/outi[1]
outi
