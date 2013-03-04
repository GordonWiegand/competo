
  rm(list=ls())
library(foreign)
d0 <- as.data.frame(read.csv("/Volumes/hd2/130100_competo2/competo2013.csv"))
d0[d0<0] <- NA
#impute nas by estimeated values
d0$f2a[d0$f2b==1] <- mean(d0$f2a[(d0$f2a>0 & d0$f2a<51)],na.rm=T)
d0$f2a[d0$f2b==2] <- mean(d0$f2a[(d0$f2a>50 & d0$f2a<101)],na.rm=T)
d0$f2a[d0$f2b==3] <- mean(d0$f2a[(d0$f2a>100 & d0$f2a<301)],na.rm=T)
d0$f2a[d0$f2b==4] <- mean(d0$f2a[(d0$f2a>300 & d0$f2a<501)],na.rm=T)
d0$f2a[d0$f2b==5] <- mean(d0$f2a[(d0$f2a>500 & d0$f2a<1001)],na.rm=T)
d0$f2a[ d0$f2b==6] <- mean(d0$f2a[(d0$f2a>1000 & d0$f2a<2001)],na.rm=T)
d0$f2a[d0$f2b==7] <- mean(d0$f2a[d0$f2a>2000],na.rm=T)
#working data set
du <- d0[d0$q0==1,c("f2a","gew")] #companies
dv <- d0[d0$q0==2,c("f2a","gew")] #associations
names(du)<-c("bm","w")
names(dv)<-c("bm","w")
#define function: compute winsorized mean
win<-function(dw,tr){
      nd <- nrow(dw)
  dw<-dw[order(dw$bm),]
  l<-round(tr*nd)
  dw$bm[1:l]<-dw$bm[l]
  dw$bm[nd:(nd-l)]<-dw$bm[(nd-l)]
  weighted.mean(dw$bm,dw$w)
 }
block <- 70
schritte <- 70
runden <- 1000
d <- du
library(multicore)
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
for (bl in 1:block){
  ptm <- proc.time()
 mclist <- list()
 l <- read.table("/Volumes/hd2/130100_competo2/out_u.csv")
 for (k in ((bl-1)*schritte+1):(bl*schritte) ) mclist <- c(mclist,k)  
 res<-numeric()
 res <- as.numeric(mclapply(mclist,runner))
 ptr <- proc.time()-ptm
 pr <- paste(bl,"/ 70 --- time ",ptr[3])
 print(pr)
 write.table(c(l$x,res),"/Volumes/hd2/130100_competo2/out_u.csv")
 }





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
d <- as.data.frame(cbind(d0$Frage_2,d0$w))
colnames(d) <- c("bm","w")

for (bl in 1:block){
  ptm <- proc.time()
 mclist <- list()
 l <- read.table("/Volumes/hd2/130100_competo2/out_h.csv")
 for (k in ((bl-1)*schritte+1):(bl*schritte) ) mclist <- c(mclist,k)  
 res<-numeric()
 res <- as.numeric(mclapply(mclist,runner))
 ptr <- proc.time()-ptm
 pr <- paste("HH ",bl,"/70 --- time ",ptr[3],sep="")
 print(pr)
 write.table(c(l$x,res),"/Volumes/hd2/130100_competo2/out_h.csv")
 }



#%%%%%%%%%%%%%%%%%%%%%%%%%%


d <- dv

for (bl in 1:block){
  ptm <- proc.time()
 mclist <- list()
 l <- read.table("/Volumes/hd2/130100_competo2/out_v.csv")
 for (k in ((bl-1)*schritte+1):(bl*schritte) ) mclist <- c(mclist,k)  
 res<-numeric()
 res <- as.numeric(mclapply(mclist,runner))
 ptr <- proc.time()-ptm
 pr <- paste("U ",bl,"/70 --- time ",ptr[3],sep="")
 print(pr)
 write.table(c(l$x,res),"/Volumes/hd2/130100_competo2/out_v.csv")
 }

