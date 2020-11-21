#limits <- aes( ymin = log10(C.mean/1000) - log10(C.se/1000), ymax = log10(C.mean/1000) + log10(C.se/1000))

#raw_C_plot = function(){
 OM.pc1 = ggplotGrob(ggplot(j.summary_stoic, aes( x= PC1, log10(C.mean/1000))) +
 labs(title = "PC1") +
  # stat_smooth(data = j.stoic.df, aes(x = PC1, log10(SumC/1000)), method = "loess", size = 1.5, colour = "dark grey", se = F, alpha = 0.2) +
  geom_point( data = j.stoic.df, aes( x = PC1, log10(SumC/1000)), size = 1.2, shape = 21, alpha = 0.6) +
  geom_point(colour = "white", shape = 21, fill = "black", stroke = 1, size = 3) +
  scale_x_continuous(limits = c(-3,3), name = "PC1") + 
  scale_y_continuous(limits = c(0,3), name = expression("log"[10]*"Organic matter (g C m"^-2*")")) +
  annotate("text", x = -2.95, y = 3, hjust = 0, label = expression("+ CV"[Q]), size = 2.5) +
  annotate("text", x = 2.95, y = 3, hjust = 1, label = expression("- CV"[Q]), size = 2.5) +
  annotate("text", x = -2.95, y = 2.8, hjust = 0, label = expression("+ Power"[max]), size = 2.5) +
  annotate("text", x = 2.95, y = 2.8, hjust = 1, label = expression("- Power"[max]), size = 2.5) +
  annotate("text", x = 2.3, y = 0.1, label = "B", size = 5) +
  theme(plot.margin = unit(c(t = 0.1,r = 0.1,b = 0.1,l = 0.1), "cm"), axis.text = element_text(size = 11), plot.title = element_text(size = 11, hjust = 0, vjust = 1),
        axis.title.x = element_blank(),panel.grid = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()))
grid.draw(OM.pc1)

OM.pc2 = ggplotGrob(ggplot(j.summary_stoic, aes( x= PC2, log10(C.mean/1000))) +
                      ggtitle("PC2")+
  # stat_smooth(data = j.stoic.df, aes(x = PC2, log10(SumC/1000)), method = "loess", size = 1.5, colour = "dark grey", se = F, alpha = 0.2) +
  geom_point( data = j.stoic.df, aes( x = PC2, log10(SumC/1000)), size = 1.2, shape = 21, alpha = 0.6) +
  geom_point(colour = "white", shape = 21, fill = "black", stroke = 1, size = 3) +
  scale_x_continuous(limits = c(-3,3), name = "PC2") + 
  scale_y_continuous(limits = c(0,3), name = expression("log"[10]*"Organic matter (g C m"^-2*")")) +
  annotate("text", x = -2.95, y = 3, hjust = 0, label = expression("- Slope"), size = 2.5) +
  annotate("text", x = 2.95, y = 3, hjust = 1, label = expression("+ Slope"), size = 2.5) +
  annotate("text", x = -2.95, y = 2.85, hjust = 0, label = expression("- Substrate"), size = 2.5) +
  annotate("text", x = 2.95, y = 2.85, hjust = 1, label = expression("+ Substrate"), size = 2.5) +
    annotate('text', x = -2.95, y = 2.65, hjust = 0, label = expression("+ Velocity"), size = 2.5) +
    annotate('text', x = 2.95, y = 2.65, hjust = 1, label = expression("- Velocity"), size = 2.5) +
    annotate('text', x = -2.95, y = 2.45, hjust = 0, label = expression("+ Median"[Q]), size = 2.5) +
    annotate('text', x = 2.95, y = 2.45, hjust = 1, label = expression("- Median"[Q]), size = 2.5) +
    annotate("text", x = 2.3, y = 0.1, label = "C", size = 5) +
    theme(plot.margin = unit(c(t = 0.1,r = 0.1,b = 0.1,l = 0.1), "cm"), axis.text = element_text(size = 11), plot.title = element_text(size = 11, hjust = 0, vjust = 1),
        axis.title.x = element_blank(),panel.grid = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()))
#OM.pc2

#lm details r2 = 0.07 p-value = 0.21
OM.temp <- ggplotGrob(ggplot(j.summary_stoic, aes( x = mean_temp, y = log10(C.mean/1000))) +
                        ggtitle(expression("Temperature ("*degree*C*")"))+
  # stat_smooth(data = j.stoic.df, aes( x = mean_temp, y = log10(SumC/1000)), method = "loess", size = 1.5, colour = "dark grey", se = F, alpha = 0.2) +
  geom_point( data = j.stoic.df, aes( x = mean_temp, y = log10(SumC/1000)), size = 1.2, shape = 21, alpha = 0.6) +
  #geom_errorbar(limits, width = 0.05) +
  geom_point(colour = "white", shape = 21, fill = "black", stroke = 0.8, size = 3) +
  #scale_x_continuous(breaks = c(5,10,15,20,25,30), limits = c(4,30)) +
  #scale_y_continuous(breaks = c(10,100,500,1000), limits = c(-10,1015)) +
  scale_x_continuous(limits = c(0,30),name = expression("Temperature ("~degree*C~")"), expand = c(0,0.4)) +
  scale_y_continuous(limits = c(0,3),name = expression("log"[10]*"Organic matter (g C m"^-2*")"),expand = c(0,0.1)) +
  #annotate("text", x = 31, y = 1000, parse = TRUE, label = "R ^ 2 == 0.11") +
  #annotate("text", x = 30, y = 960, parse = T, label = "italic(p-value) == 0.17") +
    annotate("text", x = 28, y = 0.1, label = "A", size = 5) +
    theme(plot.margin = unit(c(t = 0.1,r = 0.15,b = 0.1,l = 0.1), "cm"),axis.text = element_text(size = 11), plot.title = element_text(size = 11, hjust = 0, vjust = 0),
        axis.title.x = element_blank(),panel.grid = element_blank(), axis.title.y = element_text(size = 12)))
#OM.temp
maxwidth = grid::unit.pmax(OM.pc1$widths[2:5], OM.pc2$widths[2:5], OM.temp$width[2:5])
maxheight = grid::unit.pmax(OM.pc1$heights[2:5], OM.pc2$hieghts[2:5], OM.temp$heights[2:5])
OM.pc1$widths[2:5] = as.list(maxwidth);OM.pc1$heights[2:5] = as.list(maxheight)
OM.pc2$widths[2:5] = as.list(maxwidth);OM.pc2$heights[2:5] = as.list(maxheight)
OM.temp$widths[2:5] = as.list(maxwidth);OM.temp$heights[2:5] = as.list(maxheight)
#grid.draw(gridExtra::gtable_cbind(OM.pc1,OM.pc2,OM.temp))
#}
#av_C_plot = function() {
#now do predicted av plots
PC_temp.lm = lm(log10(C.mean/1000)~PC1+PC2+mean_temp, data = j.summary_stoic);summary(PC_temp.lm)
av_dat <- car::avPlots(PC_temp.lm, marginal.scale = F)
#av_dat = car::avPlots(PC_temp.lm, marginal.scale = F)
colnames(av_dat[[1]])[2] = "Carbon_mass"
colnames(av_dat[[2]])[2] = "Carbon_mass"
colnames(av_dat[[3]])[2] = "Carbon_mass"
# av_dat[[1]][,1] = av_dat[[1]][,1] - mean(j.summary_stoic$PC1)
# av_dat[[2]][,1] = av_dat[[2]][,1] - mean(j.summary_stoic$PC2)
# av_dat[[3]][,1] = av_dat[[3]][,1] - mean(j.summary_stoic$mean_temp)  
# av_dat[[1]][,2] = mean(log10(j.summary_stoic$C.mean/1000)) + av_dat[[1]][,2] 
# av_dat[[2]][,2] = mean(log10(j.summary_stoic$C.mean/1000)) + av_dat[[1]][,2]
# av_dat[[3]][,2] = mean(log10(j.summary_stoic$C.mean/1000)) + av_dat[[1]][,2]

PC1_plot = ggplotGrob(ggplot(av_dat[[1]], aes(x = PC1, y = Carbon_mass)) +
  #geom_smooth(method= "loess", se = F, colour = 'dark grey', linetype = 'dashed', size = 1.5, span = 0.9) +
  geom_smooth(method = 'lm', alpha = 0.4, colour = 'black', size = 1.5) +
  geom_point(colour = "white", shape = 21, fill = "black", stroke = 0.8, size = 3) +
  #scale_y_continuous(name = expression("log"[10]*"Organic matter (g C m"^-2*")"), limits = c(-3,3), breaks = c(-3,-2,-1,0,1,2,3)) +
  scale_y_continuous(name = expression("log"[10]*"Organic matter (g C m"^-2*")"[adj.]), limits = c(-3,3), breaks = c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(ylim = c(-2,2), xlim = c(-3,3)) +
  xlab("Adjusted PC1") +
  annotate('text', x = -2.8,y = 2.0, label = "Effect size = 1.66", hjust = 0, size = 3 )+
  annotate('text', x = -2.8, y = 1.7, label  = "italic(p-value) == 0.05", parse = T, hjust = 0, size = 3) +
  annotate('text', x = -2.8, y = 1.4, label  = expression("r"^2~"= 0.33"), hjust = 0, size = 3) +
  annotate("text", x = 2.3, y = -1.9, label = "E", size = 5) +
    theme(plot.margin = unit(c(t = 0.1,r = 0.1,b = 0.1,l = 0.1), "cm"),panel.grid = element_blank(), axis.title.y = element_blank(),
        axis.title.x = element_text(size = 12), axis.text = element_text(size = 11)))
#PC1_plot

#(max(preds[[2]]$pred) - min(preds[[2]]$pred))/(unname(quantile((j.summary_stoic$C.mean/1000),0.75))-unname(quantile((j.summary_stoic$C.mean/1000),0.25)))
PC2_plot = ggplotGrob(ggplot(av_dat[[2]], aes(x = PC2, y = Carbon_mass)) +
  #geom_smooth(method= "loess", se = F, colour = 'dark grey',linetype = 'dashed', size = 1.5, span = 0.9) +
  geom_smooth(method = 'lm', alpha = 0.4, colour = 'black', size = 1.5) +
  geom_point(colour = "white", shape = 21, fill = "black", stroke = 0.8, size = 3) +
  scale_y_continuous(name = expression("log"[10]*"Organic matter (g C m"^-2*")"), limits = c(-3,3), breaks = c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(ylim = c(-2,2), xlim = c(-3,3)) +
  xlab("Adjusted PC2") +
  annotate('text', x = -2.8, y = 2.0, label = "Effect size = 2.24", hjust = 0, size = 3) +
  annotate('text', x = -2.8, y = 1.7, label  = "italic(p-value) == 0.06", parse = T, hjust = 0, size = 3) +
  annotate('text', x = -2.8, y = 1.4, label  = expression("r"^2~"= 0.29"), hjust = 0, size = 3) +
  annotate("text", x = 2.3, y = -1.9, label = "F", size = 5) +
    theme(plot.margin = unit(c(t = 0.1,r = 0.1,b = 0.1,l = 0.1), "cm"),panel.grid = element_blank(), axis.text.x = element_text(size = 11),
        axis.title.y = element_blank(), axis.text.y = element_blank(),axis.title = element_text(size = 13)))
#PC2_plot

#(max(preds[[3]]$pred) - min(preds[[3]]$pred))/(unname(quantile((j.summary_stoic$C.mean/1000),0.75))-unname(quantile((j.summary_stoic$C.mean/1000),0.25)))
temp_plot = ggplotGrob(ggplot(av_dat[[3]], aes(x = mean_temp, y = Carbon_mass)) +
  #geom_smooth(method= "loess", se = F, colour = 'grey',linetype = 'dashed', size = 1.5, span = 0.9) +
  # geom_smooth(method = 'lm', alpha = 0.4, colour = 'black', linetype = 'dotted', size = 1.2, se = F) +
  geom_point(colour = "white", shape = 21, fill = "black", stroke = 0.8, size = 3) +
  scale_y_continuous(name = expression("Adjusted Organic Matter"), limits = c(-3,3), breaks = c(-3,-2,-1,0,1,2,3)) +
  coord_cartesian(ylim = c(-2,2), xlim = c(-15,20))+
  xlab("Adjusted Temperature")+#xlab(expression(paste("Temperature (",degree,"C)|Others"))) +
  annotate('text',x = -13, y = 2.0, label = "Effect size = 0.69", hjust = 0, size = 3) +
  annotate('text', x = -13, y = 1.7, label  = "italic(p-value) == 0.43", parse = T, hjust = 0, size = 3) +
  annotate('text', x = -13, y = 1.4, label  = expression("r"^2~"= 0.04"), hjust = 0, size = 3) +
  annotate("text", x = 18, y = -1.9, label = "D", size = 5) +
    theme(plot.margin = unit(c(t = 0.1,r = 0.15,b = 0.1,l = 0.1), "cm"),panel.grid = element_blank(), axis.text.x = element_text(size = 11),
          axis.title.y = element_text(size = 12),axis.title.x = element_text(size = 12)))
#temp_plot
#pull some code from 
maxwidth = grid::unit.pmax(PC1_plot$widths[2:5], 
                           PC2_plot$widths[2:5], 
                           temp_plot$widths[2:5])#,
                           #OM.pc1$widths[2:5],
                           #OM.pc2$widths[2:5],
                           #OM.temp$widths[2:5])
maxheight = grid::unit.pmax( PC1_plot$heights[2:5], 
                             PC2_plot$heights[2:5], 
                            temp_plot$heights[2:5])#,
                            #OM.pc1$heights[2:5],
                            #OM.pc2$heights[2:5],
                            #OM.temp$heights[2:5])

 PC1_plot$widths[2:5] = as.list(maxwidth);PC1_plot$heights[2:5] = as.list(maxheight)
 PC2_plot$widths[2:5] = as.list(maxwidth);PC2_plot$heights[2:5] = as.list(maxheight)
 temp_plot$widths[2:5] = as.list(maxwidth);temp_plot$heights[2:5] = as.list(maxheight)
 #OM.pc1$widths[2:5] = as.list(maxwidth);OM.pc1$heights[2:5] = as.list(maxheight)
 #OM.pc2$widths[2:5] = as.list(maxwidth);OM.pc2$heights[2:5] = as.list(maxheight)
 #OM.temp$widths[2:5] = as.list(maxwidth);OM.temp$heights[2:5] = as.list(maxheight)
 
 
#grid.draw(
#  x = gridExtra::gtable_cbind(PC1_plot,PC2_plot,temp_plot)#)



tiff("./output/Figure-2.tiff", res = 600,  height = 5, width = 7, units = "in", compression = "lzw" )
#layout(mat = matrix(c(1,2), byrow = TRUE))
#par(mar = c(0.5,0.5,0.5,0.5))
#raw_C_plot()
#plot.new()
#av_C_plot()
#vps = baseViewports()
#pushViewport(vps$figure)
#vp2 = plotViewport(c(1,0.5,1,0.5))
#print(av_C_plot(), vp = vp2)
lay = rbind(c(1,2,3),
            c(4,5,6))
gs = list(OM.temp, OM.pc1, OM.pc2, temp_plot, PC1_plot, PC2_plot) 
grid.arrange(grobs = gs, layout_matrix = lay )
dev.off()
#}
#summary(lm(log(OM.mean/1000)~mean_kt, data = j.summary_stoic))
#temp.lm = lm(log(OM.mean/1000)~mean_kt, data = j.summary_stoic)
#coef(temp.lm)[2]
##### End Figure S1_Temp-BOM code ####