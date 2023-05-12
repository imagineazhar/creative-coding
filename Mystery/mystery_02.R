library(gifski)

circle <- function(amp, freq, phase) amp*1i^(freq*seq(0,400,l=399)+phase)
limits=c(-1,1)*2.5

# lapply here makes a 'list' of plots, 
# save_gif turns this list into a gif

save_gif(lapply(seq(0,4,l=200)[-1],
                function(j){
                  par(bg="white")
                  z = circle(sin(pi*j/2),1,0) + circle(sin(pi*j/2),-5,j) + circle(cos(pi*j/2),1,j)
                  
                  plot(xlim=limits, ylim=limits,col='grey12',pch=20,
                       z, axes=FALSE, ann=FALSE, asp=1, mar=c(0,0,0,0))
                  
                  lines(z,col='grey12', lwd=2.5)
                  
                }),
         delay=1/30,width = 1080,height=1080, gif_file = "mystery_02.gif")

