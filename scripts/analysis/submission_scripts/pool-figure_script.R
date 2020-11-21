## pool figure 
####### Plot of ecosystem pools--Dissolved and biomass ######

Nh2o <- j.summary_stoic$nflux/14.007
Ph2o <- j.summary_stoic$pflux/30.9737

Nbom <- (j.summary_stoic$N.mean)/14.007
Pbom <- (j.summary_stoic$P.mean)/30.9737

stream <- rep(j.summary_stoic$Stream, 2)
h2o <- rep ("H2O", 11)
bom <- rep("BOM", 11)

Npool <- c(Nh2o,Nbom)
Ppool <- c(Ph2o, Pbom)
type <- c(h2o, bom)

Pool.df <- data.frame(stream, type, Npool, Ppool)

bom.pool <- Pool.df[which(Pool.df$type == "BOM"),]
bom.lm <- lm(log(Npool)~log(Ppool), data = bom.pool);summary(bom.lm)
h2o.pool <- Pool.df[which(Pool.df$type == "H2O"),]
h2o.lm <- lm(log(Npool)~log(Ppool), data = h2o.pool);summary(h2o.lm)

NP_0.1 = data.frame(Stoic = rep("NP_0.1", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.1)
NP_0.2 = data.frame(Stoic = rep("NP_0.2", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.2)
NP_0.3 = data.frame(Stoic = rep("NP_0.3", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.3)
NP_0.4 = data.frame(Stoic = rep("NP_0.4", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.4)
NP_0.5 = data.frame(Stoic = rep("NP_0.5", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.5)
NP_0.6 = data.frame(Stoic = rep("NP_0.6", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.6)
NP_0.7 = data.frame(Stoic = rep("NP_0.7", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.7)
NP_0.8 = data.frame(Stoic = rep("NP_0.8", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.8)
NP_0.9 = data.frame(Stoic = rep("NP_0.9", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*0.9)
NP_1 = data.frame(Stoic = rep("NP_1", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1))
NP_2 = data.frame(Stoic = rep("NP_2", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*2)
NP_3 = data.frame(Stoic = rep("NP_3", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*3)
NP_4 = data.frame(Stoic = rep("NP_4", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*4)
NP_5 = data.frame(Stoic = rep("NP_5", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*5)
NP_6 = data.frame(Stoic = rep("NP_6", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*6)
NP_7 = data.frame(Stoic = rep("NP_7", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*7)
NP_8 = data.frame(Stoic = rep("NP_8", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*8)
NP_9 = data.frame(Stoic = rep("NP_9", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*9)
NP_10 = data.frame(Stoic = rep("NP_10", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*10)
NP_20 = data.frame(Stoic = rep("NP_20", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*20)
NP_30 = data.frame(Stoic = rep("NP_30", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*30)
NP_40 = data.frame(Stoic = rep("NP_40", 991), Ppool = seq(1,100, by = .1), Npool = seq(1,100, by = .1)*40)
NP_50 = data.frame(Stoic = rep("NP_50", 991), Ppool = seq(1, 100, by = 0.1), Npool = seq(1,100, by = 0.1)*50)
NP_60 = data.frame(Stoic = rep("NP_60", 991), Ppool = seq(1, 100, by = 0.1), Npool = seq(1,100, by = 0.1)*60)
NP_70 = data.frame(Stoic = rep("NP_70", 991), Ppool = seq(1, 100, by = 0.1), Npool = seq(1,100, by = 0.1)*70)
NP_80 = data.frame(Stoic = rep("NP_80", 991), Ppool = seq(1, 100, by = 0.1), Npool = seq(1,100, by = 0.1)*80)
NP_90 = data.frame(Stoic = rep("NP_90", 991), Ppool = seq(1, 100, by = 0.1), Npool = seq(1,100, by = 0.1)*90)
NP_100 = data.frame(Stoic = rep("NP_100", 991), Ppool = seq(1,100, by = 0.1), Npool = seq(1,100, by = 0.1)*100)
NP = data.frame(rbind(NP_0.1, NP_0.2,NP_0.3,NP_0.4, NP_0.5, NP_0.6, NP_0.7, NP_0.8, NP_0.9, NP_1, NP_2, NP_3, NP_4, NP_5, NP_6, NP_7, NP_8, NP_9, NP_10,
                      NP_20, NP_30, NP_40, NP_50, NP_60, NP_70, NP_80, NP_90, NP_100), stringsAsFactors = T)
NP$Stoic = factor(NP$Stoic)
NP = NP[-which(NP$Npool > 1000 | NP$Npool < 1),]
lines = c("solid", rep("dotted", 3), "dashed", rep("dotted", 4), "solid", "solid", "solid", rep("dotted", 6), "dashed", "dashed",rep("dotted", 8))

p1 = ggplot() +
  geom_line(data = NP, aes(x = Ppool, y = Npool, group = Stoic, linetype = Stoic), alpha = 0.7, size = 0.5, colour = "black") +
  scale_linetype_manual(values = lines) +
  theme(panel.grid = element_blank(), legend.position = "none", axis.text = element_text(size = 20))#;p1

p1 = p1 + geom_line(data = Pool.df, aes(x = Ppool, y = Npool, group = stream), colour = "dark grey", alpha = 0.3, size = 1.2) + 
  geom_point(data = Pool.df, aes(x = Ppool, y = Npool, group = stream, fill = factor(type)), shape = 21, colour = "black", size = 5,  stroke = 1.5) +
  scale_fill_manual(values = c("black", "white")) +
  #scale_shape_manual(values = Pool.df$stream) +
  xlab("Phosphorus (mmol) [flux | pool]") +
  ylab("Nitrogen (mmol) [flux | pool]")#;p1# +
#geom_label(data = filter(Pool.df, type == "BOM"), aes(x = Ppool, y = Npool ,label = stream));p1

p2 = p1 +   scale_y_log10() + scale_x_log10() +
  coord_cartesian(ylim = c(1,900), xlim = c(0.9,110))#;p2

p3 = p2 + geom_text(aes(x = 10, y = 1100, label = "100", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 20, y = 1100, label = "50", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 110, y = 1100, label = "10", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 110, y = 500, label = "5", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 110, y = 100.5, label = "1", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 115, y = 50.5, label = "0.5", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 115, y = 10.5, label = "0.1", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 10, y = 0.9, label = "0.1", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 2, y = 0.9, size = 30, label = "0.5"), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 0.9, y = 0.9, label = "1", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 0.88, y = 5, label = "5", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 0.88, y = 10, label = "10", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 0.88, y = 50, label = "50", size = 30), colour = "grey50", fontface = "bold") +
  geom_text(aes(x = 0.88, y = 100, label = "100", size = 30), colour = "grey50", fontface = "bold") +
  annotate("text",x = 2, y = 850, label = paste("BOM N:P scaling\nexponent =",round(bom.lm$coefficients[2],2)," \u00B1 ",round(summary(bom.lm)$coefficients[4],2),sep=""), size = 5)
#p3  


#png("log_pools.png", res = 300, width = 2000, height = 2000, units = "px")
#p3
#dev.off()

##Ecosystem NP with dissolved NP inset##
j.summary_stoic$include = 'in'
j.summary_stoic[which(j.summary_stoic$Stream == "st14"),"include"] = 'out'
j.summary_stoic_in = j.summary_stoic[which(j.summary_stoic$include == 'in'),]
limits = aes(ymin = NP.mean - NP.se, ymax = NP.mean + NP.se)

NP_NP <- ggplot(j.summary_stoic, aes( x = h2oNP, y = NP.mean)) +
  stat_smooth(data = j.summary_stoic, aes(x = h2oNP, y = NP.mean), method = "lm", size = 1.2, colour = "lightgrey", alpha = 0.5, linetype = "dotted", se = FALSE) +
  #geom_point(data = j.stoic.df1, aes( x = h2oNP, y = NP), size = 3, shape = 21, alpha = 0.6) +
  geom_errorbar(limits, width = 0.05, size = 1) +
  geom_point(aes(fill = include, colour = include), shape = 21, stroke = 1, size = 2) +
  stat_smooth(data = j.summary_stoic_in, aes(h2oNP, y = NP.mean), method = "lm", size = 1.5, colour = "darkgrey", linetype = "solid", se = FALSE) +
  scale_fill_manual(values = c("black","grey")) +
  scale_colour_manual(values = c("black","grey")) +
  #geom_text(label = j.summary_stoic$Stream, size = 2) +
  scale_y_continuous(breaks = c(0,5,10,15,20), limits = c(-4,25)) +
  #scale_x_continuous(limits = c(0,5), breaks = c(0,1,2,3,4,5)) +
  geom_abline(intercept = 0, slope = 1, size = 1.5) +
  coord_cartesian(xlim = c(0, 5), ylim = c(0,20)) +
  xlab("DIN:SRP (molar)") +
  ylab("BOM N:P\n (molar)") +
  annotate("text", x = 0.5, y = 2, parse = TRUE, label = "1:1", fontface = 2) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.y = element_text(size = 12, margin = margin(r = -0.35, l = 0)), axis.title.x = element_text(size = 12, margin = margin(t = -0.35, b = 0)),
        axis.text.y = element_text(size = 12, margin = margin(t = 0, r = -0.5, b = 0, l = 0)), axis.text.x = element_text(size = 12, margin = margin(t = -0.5, r = 0, b = 0, l = 0)),
        panel.spacing = unit(0, 'lines'), plot.margin = unit(c(0,0,0,0), 'in'), legend.position = "none");NP_NP

vp = viewport(width = 0.28, height = 0.3, x = 0.83, y = 0.28)

gm = ggplotGrob(p3)
gs = ggplotGrob(NP_NP)
id = 1

panel = gm$layout[gm$layout$name == 'panel',][id,]

inset = grobTree(gs, vp = viewport(width = 0.35, height = 0.28, x = 0.8, y = 0.15))

gm = gtable_add_grob(gm, inset, l = panel$l, t = panel$t)
#grid.newpage()
#grid.draw(gm)

#print(p3)
#print(NP_NP, vp = vp)
tiff("./output/Figure-4_NP-pools.tiff", res = 600, height = 6, width = 7, units = "in", compression = 'lzw')
grid.arrange(gm)
dev.off()
pattern = grep("NP_[0-9]",names(.GlobalEnv), value = T)
rm(list = pattern)
########## End Figure code ######