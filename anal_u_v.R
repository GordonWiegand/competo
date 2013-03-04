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

block <- 80
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
mclist <- list()
l <- read.table("/Volumes/hd2/130100_competo2/out_h.csv")
for (k in ((bl-1)*schritte+1):(bl*schritte) ) mclist <- c(mclist,k)  
res<-numeric()
res <- as.numeric(mclapply(mclist,runner))

write.table(c(l$x,res),"/Volumes/hd2/130100_competo2/out_h.csv")
}

