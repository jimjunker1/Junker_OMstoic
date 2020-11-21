## Community analysis script ##
j.summary_stoic$rank <- reorder(j.summary_stoic$Stream, -j.summary_stoic$C.mean)
j.stoic.df$rank <- factor(j.stoic.df$Stream, levels(j.summary_stoic$rank))

j.df.totcomm <- j.df %>%
  group_by(Stream, Date, SPP, Sample) %>%
  summarise(C.tot = sum(C.tot), N.tot = sum(N.tot), P.tot = sum(P.tot))

####widen out so spp when not present are "0" for each group#####
j.df.comm_rel.C = j.df.totcomm[,c('Stream','Date','Sample','SPP','C.tot')] %>%
  spread(SPP, C.tot) %>%
  replace(.,is.na(.), 0.000001) %>%
  gather(SPP, C.tot, CalHam:Wood)

j.df.comm_Csum <- j.df.comm_rel.C %>%
  group_by(Stream, Date, Sample) %>%
  mutate(C.sum = sum(C.tot, na.rm =T))

#### Combine the less abundant pools into a single group "other" ####

j.df.comm_Csum$SPP.rank <- as.character(j.df.comm_Csum$SPP, c("FBOM", "Font", "CBOM", "Misc", "Grass","Fila","CalHam","Jung","MyrAlt", "Nos", "PotPer", "Horsetail", "Wood", "CalSta"))


fix = which(j.df.comm_Csum$SPP.rank == "CalHam" | j.df.comm_Csum$SPP.rank == "Misc" | j.df.comm_Csum$SPP.rank == "MyrAlt" | j.df.comm_Csum$SPP.rank == "PotPer" | j.df.comm_Csum$SPP.rank == "Horsetail" | j.df.comm_Csum$SPP.rank == "CalSta" | j.df.comm_Csum$SPP.rank == "Wood" | j.df.comm_Csum$SPP.rank == "Grass")
j.df.comm_Csum[fix, "SPP.rank"] = "Other"
fix = which(j.df.comm_Csum$SPP.rank == "CBOMe" | j.df.comm_Csum$SPP.rank == "FBOMe")
j.df.comm_Csum[fix, "SPP.rank"] = "Biofilm"
unique(j.df.comm_Csum$SPP.rank)
j.df.comm_Csum$SPP.rank = as.factor(j.df.comm_Csum$SPP.rank)
unique(j.df.comm_Csum$SPP.rank)

####must combine sum of new 'other' group ####

#j.df.comm_Csum <- ddply(j.df.comm_Csum[,c(1:4,14,5:13)], .(Stream, Date, Sample, SPP.rank), summarize, OM.tot = sum(OM.tot, na.rm = T), C.tot = sum(C.tot), N.tot = sum(N.tot), P.tot = sum(P.tot))
j.df.comm_Csum <- j.df.comm_Csum %>%
  group_by(Stream, Date, Sample, SPP.rank) %>%
  summarise(C.tot = sum(C.tot))
head(j.df.comm_Csum,15)

#### Now summarize the mean mass of each ####

#j.df.comm_Csum = ddply(j.df.comm_Csum, .(Stream, Date, SPP.rank), summarize, OM.mean = mean(OM.tot, na.rm = T), C.mean = mean(C.tot), N.mean = mean(N.tot), P.mean = mean(P.tot))
j.df.comm_Csum <- j.df.comm_Csum %>%
  group_by(Stream, Date, SPP.rank) %>%
  summarise(C.mean = mean(C.tot,na.rm = T))

#### now create relative abundance of CNP####
#j.df.comm_Csum = ddply(j.df.comm_Csum, .(Stream, Date), mutate, OM.sum = sum(OM.mean, na.rm = T), C.sum = sum(C.mean), N.sum = sum(N.mean), P.sum = sum(P.mean))
j.df.comm_Csum = j.df.comm_Csum %>%
  group_by(Stream, Date) %>%
  mutate(C.sum = sum(C.mean, na.rm = T))

#j.comm_rel = ddply(j.df.comm_Csum, .(Stream, Date, SPP.rank), summarize,OM.rel = (OM.mean/OM.sum)*100,  C.rel = (C.mean/C.sum)*100, N.rel = (N.mean/N.sum)*100, P.rel = (P.mean/P.sum)*100)
j.comm_rel <- j.df.comm_Csum %>%
  group_by(Stream, Date, SPP.rank) %>%
  summarise(C.rel = (C.mean/C.sum)*100) %>%
  left_join(unique(j.df.comm_Csum[c("Stream", "C.sum")]), by = "Stream")

#j.df.comm_relsum = ddply(j.comm_rel, .(Stream, Date), summarize, sum = sum(OM.rel,C.rel,N.rel, P.rel) )
j.df.comm_relsum = j.comm_rel %>%
  group_by(Stream, Date) %>%
  summarise(sum = sum(C.rel))
rm(j.df.comm_relsum)

head(j.comm_rel, 15)

nos.df = j.comm_rel %>%
  filter(SPP.rank == 'Nos')

ggplot(nos.df, aes(x = Stream, y = C.rel)) + geom_point(size =3)
####Now make up the pie charts####
j.comm_rel <- j.comm_rel %>%
  ungroup() %>%
  arrange(-C.sum) %>%
  mutate(Stream = factor(Stream, unique(Stream))) %>%
  mutate(SPP.rank = factor(SPP.rank, levels = c("Font", "Jung", "Fila","Nos","Biofilm","CBOM", "FBOM",
                                                "Other")))

############ Figures for community composition #########

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
  plot.new();plot.window(xlim=xlim,ylim=ylim, 
                         main=main,xlab=xlab,ylab=ylab) 
  for(bubble in 1:nbubbles) 
    floating.pie(xpos=xpos[bubble],ypos=ypos[bubble], 
                 x=unlist(unname(sectors[[bubble]])),radius=radii[bubble], 
                 col = cbbPalette)#col=sector_col[[bubble]])# 
}

ypos = rep(1,11)

radii = (unique(log10(j.comm_rel$C.sum/1000)/log10(max(j.comm_rel$C.sum/1000))))
sectors =j.comm_rel %>%
  spread(SPP.rank, C.rel) %>%
  ungroup() %>%
  dplyr::select(Font:Other) %>%
  apply(.,1, as.list)

unique(levels(sectors[[1]]))
xpos = c(1,3,4.8,6.48,8.15,9.65,11.08,12.45,13.6,14.4,14.9)
                 #"Font",    "Jung",     "Fila",   "Nos",   "Biofilm",  "CBOM",     "FBOM",    "Other"
#cbbPalette <- c("#004d00", "#00b300", "#009900", "#660066", "#cc0099", "#663300", "#cc9900", "#FFFFFF")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#FFFFFF")
#fig1 = function() {
tiff(paste(Sys.Date(),"_Figure2_pie2.tiff",sep = ""), res = 600, width = 11, height = 5, units = "in", compression = 'lzw')
pie_bubbles(xpos, ypos,radii, sectors)
segments(x0 = -0.1, y0 = 1, x1 = -0.1, y1 = 1.5169602, lwd = 2, lty = 1)
segments(x0 = -0.1, y0 = 1.5169602, x1 = -0.2, y1 = 1.5169602, lwd = 1.5, lty = 1)
segments(x0 = -0.1, y0 = 1, x1 = -0.2, y1 = 1, lwd = 1.5, lty = 1)
segments(x0 = -0.1, y0 = 1.1723201, x1 = -0.2, y1 = 1.1723201, lwd = 1.5, lty = 1)
segments(x0 = -0.1, y0 = 1.3446401, x1 = -0.2, y1 = 1.3446401, lwd = 1.5, lty = 1)
text(x = -0.4, y = 1, labels = "0")
text(x = -0.4, y = 1.1723201, labels = "1")
text(x = -0.4, y = 1.3446401, labels = "2")
text(x = -0.4, y = 1.5169602, labels = "3")
points(x = 0.5, y = 1.5, pch = 22, bg = "#000000", cex = 2)
text(x = 0.7, y = 1.5, labels = "FONT", adj = 0)
points(x = 1.7, y = 1.5, pch = 22, bg = "#E69F00", cex = 2)
text(x = 1.9, y = 1.5, labels = "JUNG", adj = 0)
points(x = 2.9, y = 1.5, pch = 22, bg = "#56B4E9", cex = 2)
text(x = 3.1, y = 1.5, labels = "FILA", adj = 0)
points(x = 3.9, y = 1.5, pch = 22, bg = "#009E73", cex = 2)
text(x = 4.1, y = 1.5, labels = "NOS", adj = 0)
points(x = 4.9, y = 1.5, pch = 22, bg = "#F0E442", cex = 2)
text(x = 5.1, y = 1.5, labels = "BIOFILM", adj = 0)
points(x = 6.5, y = 1.5, pch = 22, bg = "#0072B2", cex = 2)
text(x = 6.7, y = 1.5, labels = "CBOM", adj = 0)
points(x = 7.8, y = 1.5, pch = 22, bg = "#D55E00", cex = 2)
text(x = 8.0, y = 1.5, labels = "FBOM", adj = 0)
points(x = 9.1, y = 1.5, pch = 22, bg = "#FFFFFF", cex = 2)
text(x = 9.3, y = 1.5, labels = "OTHER", adj = 0)
text(x = xpos, y = 0.5, labels = format(round(sort(j.summary_stoic$C.mean/1000, decreasing = T),1),nsmall = 1), adj = 0.5)
dev.off()#};fig1


grid.echo()
f1  <- grid.grab()
max(log10(j.comm_rel$C.sum/1000))


########## End community figure code ########

