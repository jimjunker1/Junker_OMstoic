#script for figure 1 (4 panel)####
#figure layout is:
#A -- stoic composition and C mass pies
#B -- ecosystem and component CN
#C -- ecosystem and component CP
#D -- ecosystem and component NP

##first panel manipulation and figure##
## Community analysis script ##
j.summary_stoic = j.summary_stoic %>%
    mutate(Stream = reorder(Stream, -C.mean ))

j.stoic.df = j.stoic.df %>% ungroup() %>%
  mutate(Stream = fct_relevel(Stream, unique(levels(j.summary_stoic$Stream))))

j.df = j.df %>%
  mutate(Stream = fct_relevel(Stream, unique(levels(j.summary_stoic$Stream))))


j.df.totcomm <- j.df %>%
  group_by(Stream, Date, SPP, Sample) %>%
  summarize(C.tot = sum(C.tot), N.tot = sum(N.tot), P.tot = sum(P.tot))

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
  summarize(C.tot = sum(C.tot))
head(j.df.comm_Csum,15)

#### Now summarize the mean mass of each ####
#j.df.comm_Csum = ddply(j.df.comm_Csum, .(Stream, Date, SPP.rank), summarize, OM.mean = mean(OM.tot, na.rm = T), C.mean = mean(C.tot), N.mean = mean(N.tot), P.mean = mean(P.tot))
j.df.comm_Csum <- j.df.comm_Csum %>%
  group_by(Stream, Date, SPP.rank) %>%
  summarize(C.mean = mean(C.tot,na.rm = T))

#### now create relative abundance of CNP####
#j.df.comm_Csum = ddply(j.df.comm_Csum, .(Stream, Date), mutate, OM.sum = sum(OM.mean, na.rm = T), C.sum = sum(C.mean), N.sum = sum(N.mean), P.sum = sum(P.mean))
j.df.comm_Csum = j.df.comm_Csum %>%
  group_by(Stream, Date) %>%
  mutate(C.sum = sum(C.mean, na.rm = T))

#j.comm_rel = ddply(j.df.comm_Csum, .(Stream, Date, SPP.rank), summarize,OM.rel = (OM.mean/OM.sum)*100,  C.rel = (C.mean/C.sum)*100, N.rel = (N.mean/N.sum)*100, P.rel = (P.mean/P.sum)*100)
j.comm_rel <- j.df.comm_Csum %>%
  group_by(Stream, Date, SPP.rank) %>%
  summarize(C.rel = (C.mean/C.sum)*100) %>%
  left_join(unique(j.df.comm_Csum[c("Stream", "C.sum")]), by = "Stream")

#j.df.comm_relsum = ddply(j.comm_rel, .(Stream, Date), summarize, sum = sum(OM.rel,C.rel,N.rel, P.rel) )
j.df.comm_relsum = j.comm_rel %>%
  group_by(Stream, Date) %>%
  summarize(sum = sum(C.rel))
rm(j.df.comm_relsum)

head(j.comm_rel, 15)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####Now make up the pie charts####
j.comm_rel <- j.comm_rel %>%
  ungroup() %>%
  arrange(-C.sum) %>%
  mutate(Stream = factor(Stream, unique(Stream))) %>%
  mutate(SPP.rank = factor(SPP.rank, levels = c("Font", "Jung", "Fila","Nos","Biofilm","CBOM", "FBOM",
                                                "Other")))
###### Code to build figure from grobs. Not easy so waiting ####
library(gridBase)
source("./submission_scripts/pie_bubbles.R")
source("./submission_scripts/figure-1a.R")
source("./submission_scripts/figure-1b-d.R")

tiff(file = "./submission/Figures/figure1_full.tiff", width = 8.5, height = 11, units = "in", res = 600, compression = 'lzw')
layout(mat = matrix(c(1,2)), widths = c(0.5,1), heights = c(0.9,3.5))
par(mar = c(0.5,0.5,0.5,0.5))
fig1()
plot.new()
vps = baseViewports()
pushViewport(vps$figure)
vp2 = plotViewport(c(1,1,1,1))
print(fig1b_d(), vp = vp2)
dev.off()
################
#######  End manuscript Figure 1_Ecosystem-BOM-characteristcs code ######