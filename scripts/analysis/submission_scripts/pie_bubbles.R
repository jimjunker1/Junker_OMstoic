library(gridBase)
library(gridGraphics)
library(plotrix)
#from SO post https://stackoverflow.com/questions/38959093/packed-bubble-pie-charts-in-r
#slight modification from Jim Junker Nov-2018
pie_bubbles<-function(xpos,ypos,radii,sectors, 
                      sector_col=NULL,main="",xlab="",ylab="") { 
  xlim<-c(min(xpos-radii),max(xpos+radii)) 
  ylim<-c(min(ypos-radii),max(ypos+radii)) 
  nbubbles<-length(xpos) 
  if(is.null(sector_col)) { 
    sector_col<-list() 
    for(scol in 1:nbubbles) 
      #sector_col[[scol]]<-rainbow(length(sectors[[scol]])) 
      sector_col[[scol]] <- cbbPalette[length(sectors[[scol]])]
  } 
  #plot(0,xlim=xlim,ylim=ylim,type="n", 
  #     main=main,xlab=xlab,ylab=ylab)
  plot.new();par(mar = c(0.5,0.5,0.5,0.5));plot.window(xlim=xlim,ylim=ylim, 
                                                       main=main,xlab=xlab,ylab=ylab) 
  for(bubble in 1:nbubbles) 
    floating.pie(xpos=xpos[bubble],ypos=ypos[bubble], 
                 x=unlist(unname(sectors[[bubble]])),radius=radii[bubble], 
                 col = cbbPalette)#col=sector_col[[bubble]])# 
}
