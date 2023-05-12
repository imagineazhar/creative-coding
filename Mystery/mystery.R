library(gifski)

circle <- function(amp, freq, phase) amp*1i^(freq*seq(0,400,l=399)+phase)
limits=c(-1,1)*2.5

# lapply here makes a 'list' of plots, 
# save_gif turns this list into a gif

save_gif(lapply(seq(0,4,l=500)[-1],
                function(j){
                  par(bg="#191825")
                  z = circle(1,1,0) + circle(sin(pi*j/2),5,0) + circle(cos(pi*j/2),9,j)
                  
                  hue = (j/4+seq(0,0.5,l=399))%%1
                  
                  plot(xlim=limits, ylim=limits,col=hsv(hue,.8,1),pch=19,
                       z, axes=FALSE, ann=FALSE, asp=1, mar=c(0,0,0,0))
                  
                  lines(z,col=hsv(hue[1],.5,1,0.4), lwd=2)
                  
                }),
         delay=1/30,width = 1080,height=1080, gif_file = "Mystery.gif")

