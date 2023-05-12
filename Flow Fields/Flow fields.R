set.seed(1211)
library(data.table)
library(scattermore)


#takes a function and draws the vector field.
plotFlow <- function(FUN, col='black'){
  
  dat <- expand.grid(x=seq(-1,1,l=20), y=seq(-1,1,l=20))
  setDT(dat)
  
  dat[ , z := x+1i*y ]  # represent positions as complex numbers
  dat[ , v := FUN(z) ]# create vector field by rotating z by 3pi/5.
  dat[ , v := v/Mod(v) ]
  
  dat[ , znew := z + .1*v  ]
  
  plot(dat$z, pch=19, col=col)
  arrows(Re(dat$z), Im(dat$z), Re(dat$znew), Im(dat$znew), length=0.05, col=col)
}
makeStreams <- function(startpos,m=100,FUN,d=0.1){
  
  N=length(startpos)
  
  pos=matrix(nrow=length(startpos), ncol=m)
  pos2=matrix(nrow=length(startpos), ncol=m)
 
  pos[,1] <- startpos
  pos2[,m] <- startpos
  
  for(i in 2:m){
    v <- FUN(pos[,i-1])
    pos[,i] <- pos[,i-1] + runif(N,0,2)*d * v/Mod(v)
  }
  for(i in (m-1):1){
    v <- FUN(pos2[,i+1])
    pos2[,i] <- pos2[,i+1] - runif(N,0,2)*d * v/Mod(v)
  }
  pos <-cbind(pos,pos2) |> as.vector()
  cbind(Re(pos),Im(pos))
}

flowFunction <- function(z){
  fz = z-zeros[1]
  for(i in  zeros[-1]) fz = fz*(z-i)
  for (i in poles) fz = fz / (z+i)
  
  fz/Mod(fz)
}

poles <- exp(2i*pi*runif(10))*0.8
zeros <- exp(2i*pi*runif(50))*0.4

# l here controls the number of streamlines
startZ <- 0.7*exp(2i * pi * seq(0,1,l=30000))
pos <- makeStreams(startZ,m=500,FUN=flowFunction,d=0.05)

par(mar=c(1,1,1,1), bg="#111111")
scattermoreplot(pos, 
                size=c(3000,3000),
                xlim=c(-1,1), 
                ylim=c(-1,1), 
                col=hsv(1,0,1,.1),
                axes=F)
