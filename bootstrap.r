
library(foreign)
d0 <- as.data.frame(read.csv("/Volumes/hd2/130100_competo2/competo2013.csv"))
d0[d0<0] <- NA
#Impute NAs by estimeated values
d0$f2a[d0$f2b==1] <- mean(d0$f2a[(d0$f2a>0 & d0$f2a<51)],na.rm=T)
d0$f2a[d0$f2b==2] <- mean(d0$f2a[(d0$f2a>50 & d0$f2a<101)],na.rm=T)
d0$f2a[d0$f2b==3] <- mean(d0$f2a[(d0$f2a>100 & d0$f2a<301)],na.rm=T)
d0$f2a[d0$f2b==4] <- mean(d0$f2a[(d0$f2a>300 & d0$f2a<501)],na.rm=T)
d0$f2a[d0$f2b==5] <- mean(d0$f2a[(d0$f2a>500 & d0$f2a<1001)],na.rm=T)
d0$f2a[d0$f2b==6] <- mean(d0$f2a[(d0$f2a>1000 & d0$f2a<2001)],na.rm=T)
d0$f2a[d0$f2b==7] <- mean(d0$f2a[d0$f2a>2000],na.rm=T)
#working data set
du <- d0[d0$q0==1,c("f2a","gew")] #companies
dv <- d0[d0$q0==2,c("f2a","gew")] #associations
names(du)<-c("bm","w")
names(dv)<-c("bm","w")

#Define function: Compute Winsorized Mean
win<-function(dw,tr=.1){
        nd <- nrow(dw)
	dw<-dw[order(dw$bm),]
	l<-round(tr*nd)
	dw$bm[1:l]<-dw$bm[l]
	dw$bm[nd:(nd-l)]<-dw$bm[(nd-l)]
	weighted.mean(dw$bm,dw$w)
}
#Define function: Compute Truncated Mean
trun<-function(dw,tr){
        nd <- nrow(dw)
	dw<-dw[order(dw$bm),]
	l<-round(tr*nd)
	tes<-dw[l:(nd-l),]
	weighted.mean(tes$bm,tes$w)
}
#That's the line
win(du,.1)
win(dv,.15)
#get the Plots: all
library(ggplot2)
pdf(file="/Volumes/hd2/130100_competo2/density_u_alle.pdf")
ggplot(du, aes(x=bm)) + geom_density() + xlab("Briefmarken in CHF")
dev.off()
#get the Plots:loup 
du500 <- du[du$bm<500,]
pdf(file="/Volumes/hd2/130100_competo2/density_u_500.pdf")
ggplot(du500, aes(x=bm)) + geom_density() + xlab("Briefmarken in CHF")
dev.off()
#get the Plots: all
pdf(file="/Volumes/hd2/130100_competo2/density_v_alle.pdf")
ggplot(dv, aes(x=bm)) + geom_density() + xlab("Briefmarken in CHF")
dev.off()
#get the Plots:loup 
dv500 <- dv[dv$bm<500,]
pdf(file="/Volumes/hd2/130100_competo2/density_v_500.pdf")
ggplot(dv500, aes(x=bm)) + geom_density() + xlab("Briefmarken in CHF")
dev.off()

###########################################
#Bootstrap Confidence Intervalls
###########################################
#Companies
d <- du
out <- as.data.frame(matrix(ncol=5,nrow=1000))
colnames(out) <- c("mean","msig","psig","p","ss" )
for (j in 1:1000){
outci <- double(10000)
nrd <- nrow(d)
ss <- j*20
#Heyho, simulate!
for (i in 1:10000){
	ds<-d[sample(nrd,size=ss,replace=T),]
	names(ds) <- c("bm","w")
	outci[i]<-win(ds,.1)
}
#Produce Output
out[j,1] <- mean(outci)
out[j,2] <- mean(outci)-1.96*sqrt(var(outci))
out[j,3] <- mean(outci)+1.96*sqrt(var(outci))
out[j,4] <- ((out[j,1]-out[j,2])*100)/out[j,1]
out[j,5] <- ss
}



out
#Associations
d <- dv
outciv <- double(10000)
nrd <- nrow(d)
#Heyho, simulate!
for (i in 1:10000){
	ds<-d[sample(nrd,size=nrd,replace=T),]
	names(ds) <- c("bm","w")
	outciv[i]<-win(ds,.15)
}
#Produce Output
outiv <- double(4)
names(outiv) <- c("mean","msig","psig","p" )
outiv[1] <- mean(outciv)
outiv[2] <- mean(outciv)-1.96*sqrt(var(outciv))
outiv[3] <- mean(outciv)+1.96*sqrt(var(outciv))
outiv[4] <- ((outiv[1]-outiv[2])*100)/outiv[1]
outiv
###########################################
#Bootstrap Different Estimtors
###########################################

#assign data set
d <- du
nrd<-nrow(d)
#Build output frame
out<-matrix(nrow=1000,ncol=16)
colnames(out)<-c("am","median","tm001","tm005","tm01","tm05","tm1","tm2","tm25","wm001",
		"wm005","wm01","wm05","wm1","wm20","wm25")

for (i in 1:1000){
	ds<-d[sample(nrd,replace=T),]
	out[i,1]<-weighted.mean(ds$bm,ds$w)
	out[i,2]<-median(ds$bm)
	out[i,3]<-trun(ds,.001)
	out[i,4]<-trun(ds,.005)
	out[i,5]<-trun(ds,.01)
	out[i,6]<-trun(ds,.05)
	out[i,7]<-trun(ds,.1)
	out[i,8]<-trun(ds,.2)
	out[i,9]<-trun(ds,.25)
	out[i,10]<-win(ds,.001)
	out[i,11]<-win(ds,.005)
	out[i,12]<-win(ds,.01)
	out[i,13]<-win(ds,.05)
	out[i,14]<-win(ds,.1)
	out[i,15]<-win(ds,.2)
	out[i,16]<-win(ds,.25)
}
#Define and save output plot
par(col="black")
pdf(file="/Volumes/hd2/130100_competo2/competo_u.pdf",width=15,height=8)
boxplot(out)
par(col="grey")
for (i in 3:9) abline(h=5*i)
par(col="red")
abline(h=median(out$am))
abline(h=median(out$median))
dev.off()






  rm(list=ls())
library(foreign)
d0 <- as.data.frame(read.csv("/Volumes/hd2/130100_competo2/competo2013.csv"))
d0[d0<0] <- NA
#Impute NAs by estimeated values
d0$f2a[d0$f2b==1] <- mean(d0$f2a[(d0$f2a>0 & d0$f2a<51)],na.rm=T)
d0$f2a[d0$f2b==2] <- mean(d0$f2a[(d0$f2a>50 & d0$f2a<101)],na.rm=T)
d0$f2a[d0$f2b==3] <- mean(d0$f2a[(d0$f2a>100 & d0$f2a<301)],na.rm=T)
d0$f2a[d0$f2b==4] <- mean(d0$f2a[(d0$f2a>300 & d0$f2a<501)],na.rm=T)
d0$f2a[d0$f2b==5] <- mean(d0$f2a[(d0$f2a>500 & d0$f2a<1001)],na.rm=T)
d0$f2a[d0$f2b==6] <- mean(d0$f2a[(d0$f2a>1000 & d0$f2a<2001)],na.rm=T)
d0$f2a[d0$f2b==7] <- mean(d0$f2a[d0$f2a>2000],na.rm=T)
#working data set
du <- d0[d0$q0==1,c("f2a","gew")] #companies
dv <- d0[d0$q0==2,c("f2a","gew")] #associations
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

schritte <- 50
runden <- 10

d <- du
library(multicore)
mclist <- list()
for (k in 1:schritte) mclist <- c(mclist,k)  


runner <- function(j){
        out <- numeric(2)
        outci <- double(runden)
        nrd <- nrow(d)
        ss <- j*20
        #Heyho, simulate!
        for (i in 1:runden){
	        ds<-d[sample(nrd,size=ss,replace=T),]
	        names(ds) <- c("bm","w")
	        outci[i]<-win(ds,.1)
                }
        out[1] <- mean(outci)
        out[2] <- ((out[1]-(mean(outci)-1.96*sqrt(var(outci))))*100)/out[1]
        print(schritte)
        return(out[2])
        
}

res<-numeric()
res <- as.numeric(mclapply(mclist,runner))

write.table(res,"/Volumes/hd2/130100_competo2/out_u.csv")



d <- dv
library(multicore)
mclist <- list()
for (k in 1:1000) mclist <- c(mclist,k)  


runner <- function(j){
        out <- numeric(2)
        outci <- double(5000)
        nrd <- nrow(d)
        ss <- j*20
        #Heyho, simulate!
        for (i in 1:5000){
	        ds<-d[sample(nrd,size=ss,replace=T),]
	        names(ds) <- c("bm","w")
	        outci[i]<-win(ds,.1)
                }
        out[1] <- mean(outci)
        out[2] <- ((out[1]-(mean(outci)-1.96*sqrt(var(outci))))*100)/out[1]
        return(out[2])
        
}

res<-numeric()
res <- as.numeric(mclapply(mclist,runner))

write.table(res,"/Volumes/hd2/130100_competo2/out_v.csv")


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

rr <- colnames(d) 
d <- cbind(d0$Frage_2,d0$w)
colnames(d) <- rr
library(multicore)
mclist <- list()
for (k in 1:1000) mclist <- c(mclist,k)  


runner <- function(j){
        out <- numeric(2)
        outci <- double(5000)
        nrd <- nrow(d)
        ss <- j*20
        #Heyho, simulate!
        for (i in 1:5000){
	        ds<-d[sample(nrd,size=ss,replace=T),]
	        names(ds) <- c("bm","w")
	        outci[i]<-win(ds,.1)
                }
        out[1] <- mean(outci)
        out[2] <- ((out[1]-(mean(outci)-1.96*sqrt(var(outci))))*100)/out[1]
        return(out[2])
        
}

res<-numeric()
res <- as.numeric(mclapply(mclist,runner))

write.table(res,"/Volumes/hd2/130100_competo2/out_h.csv")


