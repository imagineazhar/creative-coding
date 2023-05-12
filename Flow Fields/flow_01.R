library(scattermore)
makeStreams <- function(startpos,m=100,FUN,d=0.1){
  N=length(startpos)
  pos=matrix(nrow=N, ncol=m)
  pos2=matrix(nrow=N, ncol=m)
  pos[,1] <- startpos
  pos2[,m] <- startpos
  for(i in 2:m){
    v <- FUN(pos[,i-1])
    pos[,i] <- pos[,i-1] + runif(N,0,2)*d * v/Mod(v)
  }
  for(i in (m-1):1){
    v <- FUN(pos2[,i+1])
    pos2[,i] <- pos2[,i+1] - runif(N,0,2)*d* v/Mod(v)
  }
  pos <-cbind(pos,pos2) |> as.vector()
  cbind(Re(pos),Im(pos))
}

# Our flow function
flowFunction <- function(z) {
  fz = z-zeros[1]
  for(i in zeros[-1]) fz = fz * (z-i)
  for(i in poles) fz = fz / (z-i)
  fz/Mod(fz)
}


# Choose the location of the poles and zeros
poles <- runif(20,-1,1) + 1i*runif(20,-1,1)
zeros <- runif(30,-1,1) + 1i*runif(30,-1,1)


# Make the streams positions
N=1000
startZ <- runif(10000, -1,1) + 1i*runif(10000,-1,1)
pos <- makeStreams(startZ,m=1000,FUN=flowFunction,d=0.002)# Plot
par(mar=c(1,1,1,1), bg='#F7C04A')
scattermoreplot(pos,size=c(5000, 5000), xlim=c(-1,1), ylim=c(-1,1),asp=1,axes=F)
