#RMA regression for the scaling of N & P with C
suppressWarnings(library(lmodel2, verbose = F))
##split green and brown biomass
green.df = GB_summary[which(GB_summary$G_B == 'Green'),]
green.samp = GB_samples[which(GB_samples$G_B == 'Green'),]
brown.df = GB_summary[which(GB_summary$G_B == 'Brown'),]
brown.samp = GB_samples[which(GB_samples$G_B == 'Brown'),]

bom_cn = lmodel2(log10(SumN/1000)~log10(SumC/1000), data = j.stoic.df, 'interval', 'interval', 100)
bom_cp = lmodel2(log10(SumP/1000)~log10(SumC/1000), data = j.stoic.df, 'interval', 'interval', 100)
bom_np = lmodel2(log10(SumP/1000)~log10(SumN/1000), data = j.stoic.df, 'interval', 'interval', 100)
#bom_np
#test for OLS
summary(lm(log10(N.mean/1000)~log10(C.mean/1000), data = j.summary_stoic))
summary(lm(log10(P.mean/1000)~log10(C.mean/1000), data = j.summary_stoic))
#c vs N & P
breaks_x = c(seq(0.1,0.9,0.1),seq(1,10,1), seq(20,100,10), seq(200,500,100))
breaks_y = c(seq(0.01,0.09,0.01),seq(0.1,1,0.1),seq(2,10,1),20,30)
labels_y = c(0.01, rep("",8),0.1, rep("",8),1.0,rep("",8),10,"","");length(labels_y);length(breaks_y)
labels_x = c(0.1, rep("", 8),1,rep("",8),10, rep("",8),100,rep("",4));length(labels_x);length(breaks_x)

F1 = ggplotGrob(ggplot(j.summary_stoic, aes(x = (C.mean/1000), y = (N.mean/1000))) + 
                  ggtitle("Ecosystem")+
  stat_smooth(method = "lm", color = 'black', se = F) +
  stat_smooth(mapping = aes(x = (C.mean/1000), y = (P.mean/1000)),method = 'lm', color = 'black', se = F) +
  geom_point(size = 3, colour = 'black') +
  geom_point(aes(x = (C.mean/1000), y = (P.mean/1000)), size = 3, shape = 21, colour = 'black', fill = "white") + 
  scale_y_log10(limits = c(0.01,30), breaks = breaks_y, labels = labels_y) + 
  scale_x_log10(limits = c(0.3,500), breaks = breaks_x, labels = labels_x)+
  annotate('text', x = 64, y = 30,size = 3.5, parse = T, label = paste("y ==",round(10^bom_cn$regression.results$Intercept[4],2),"*x^",round(bom_cn$regression.results$Slope[4],2),sep = ""), hjust = 0)+
  annotate('text', x = 64, y = 20, size = 3.5, parse = T, label = paste("r^2 ==",round(bom_cn$rsquare,2), sep = ""), hjust = 0) +
  annotate('text', x = 64, y = 0.7,  size = 3.5, parse = T, label = paste("y ==",round(10^bom_cp$regression.results$Intercept[4],2),"*x^",round(bom_cp$regression.results$Slope[4],2),sep = ""), hjust = 0)+
  annotate('text', x = 64, y = 0.48, size = 3.5, parse = T, label =  paste("r^2 ==",round(bom_cp$rsquare,2), sep = ""), hjust = 0) +
  annotate('point', x = 0.4, y = 25, size = 3, colour = "black") +
  annotate('point', x = 0.4, y = 16, size = 3, shape = 21, colour = 'black', fill ='white')+
  annotate('text', x = 0.5, y = 25, size = 4, label = "Nitrogen", hjust = 0) +
  annotate('text', x = 0.5, y = 16, size = 4, label = 'Phosphorus', hjust = 0) +
  annotate('text', x = 400, y = 0.01, size = 5, label = "A")+
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_text(size = 14)));F1

#tiff(paste(Sys.Date(),"_Figure1_nutrient-mass.tiff",sep = ""), res = 600, width = 6, height = 6, unit = "in", compression = "lzw")
#F1
#dev.off()
#scaling of Green biomass
green_cn = lmodel2(log10(N.sum/1000)~log10(C.sum/1000), data = green.samp, 'interval', 'interval', 100)
green_cp = lmodel2(log10(P.sum/1000)~log10(C.sum/1000), data = green.samp, 'interval', 'interval', 100)

#Scaling of brown biomass
brown_cn = lmodel2(log10(N.sum/1000)~log10(C.sum/1000), data = brown.samp, 'interval', 'interval', 100)
brown_cp = lmodel2(log10(P.sum/1000)~log10(C.sum/1000), data = brown.samp, 'interval', 'interval', 100)

F2 = ggplotGrob(ggplot(green.df, aes(x = (C.mean/1000), y = (N.mean/1000))) + 
                  ggtitle('Green')+
  stat_smooth(method = "lm", color = 'black', se = F) +
  stat_smooth(mapping = aes(x = (C.mean/1000), y = (P.mean/1000)),method = 'lm', color = 'black', se = F) +
  # stat_smooth(data = brown.df,method = 'lm', mapping = aes(x = (C.mean/1000), y = (N.mean/1000)), linetype = 'dotted', se = F, colour = 'black') +
  # stat_smooth(data = brown.df, method = 'lm', mapping = aes(x = (C.mean/1000), y = (P.mean/1000)), linetype = 'dotted', se = F, colour = 'black') +
  geom_point(size = 2, colour = 'black') +
  geom_point(aes(x = (C.mean/1000), y = (P.mean/1000)), size = 2, shape = 21, colour = 'black', fill = "white") + 
  # geom_point(data = brown.df, aes(x = (C.mean/1000), y = (N.mean/1000)), shape = 24, colour = 'black', fill = 'black', size = 2) +
  # geom_point(data = brown.df, aes(x = (C.mean/1000), y = (P.mean/1000)), shape = 24, colour = 'black', fill = 'white', size = 2) +
  scale_y_log10(limits = c(0.01,30), breaks = breaks_y, labels = labels_y) + 
  scale_x_log10(limits = c(0.3,500), breaks = breaks_x, labels = labels_x)+
  annotate('text', x = 66, y = 20, size = 3.5, parse = T, label = paste("y == ",round(10^green_cn$regression.results$Intercept[4],2),"*x^",round(green_cn$regression.results$Slope[4],2), sep = ""), hjust = 0)+
  annotate('text', x = 66, y = 13.5, size = 3.5,parse = T, label = paste("r^2 == ",round(green_cn$rsquare,2), sep = ""), hjust = 0) +
  annotate('text', x = 66, y = 0.6, size = 3.5, parse = T, label = paste("y == ",round(10^green_cp$regression.results$Intercept[4],2),"*x^",round(green_cp$regression.results$Slope[4],2), sep = ""), hjust = 0) +
  annotate('text', x = 66, y = 0.35, size = 3.5, parse = T, label = paste("r^2 == ", round(green_cp$rsquare,2),sep = ""), hjust = 0) +
  # annotate('text', x = 0.31, y = 0.7, size = 3.5, parse = T, label = paste("y ==",round(10^brown_cn$regression.results$Intercept[4],2),"*x^",round(brown_cn$regression.results$Slope[4],2),sep = ""), hjust = 0) +
  # annotate('text', x = 0.31, y = 0.45, size = 3.5, parse = T, label = paste("r^2 == ", round(brown_cn$rsquare,2),sep = ""), hjust = 0) +
  # annotate('text', x = 2, y = 0.03, size = 3.5, parse = T, label = paste("y ==",round(10^brown_cp$regression.results$Intercept[4],2),"*x^",round(brown_cp$regression.results$Slope[4],2),sep = ""), hjust = 0) +
  # annotate('text', x = 2, y = 0.02, size = 3.5, parse = T, label = paste("r^2 == ", round(brown_cp$rsquare,2),sep = ""), hjust = 0) +
  # annotate('point', x = 0.4, y = 30, size = 2, colour = "black") +
  # annotate('point', x = 0.4, y = 20, size = 2, shape = 21, colour = 'black', fill ='white')+
  # annotate('point', x = 0.48, y = 30, size = 2, shape = 24, colour = "black", fill = 'black') +
  # annotate('point', x = 0.48, y = 20, size = 2, shape = 24, colour = 'black', fill ='white')+ 
  # annotate('text', x = 0.5, y = 30, size = 3.5, label = "Nitrogen", hjust = 0) +
  # annotate('text', x = 0.5, y = 20, size = 3.5, label = 'Phosphorus', hjust = 0) +
  # annotate('segment', x = 0.3, xend = 0.55, y = 13, yend = 13, colour = 'black', linetype = 'solid', size = 1.1)+
  # annotate('segment', x = 0.3, xend = 0.55, y = 10, yend = 10, colour = 'black', linetype = 'dotted', size = 1.05)+
  # annotate('text', x = 0.55, y = 13, size = 3.5, label = "Green", hjust = 0) +
  # annotate('text', x = 0.55, y = 10, size = 3.5, label = 'Brown', hjust = 0) +
  annotate('text', x = 400, y = 0.01, size = 5, label = "B")+
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_text(size = 14)));F2

F3 = ggplotGrob(ggplot(brown.df, aes(x = (C.mean/1000), y = (N.mean/1000))) + 
                  ggtitle("Brown")+
  stat_smooth(method = "lm", color = 'black', se = F) +
  stat_smooth(mapping = aes(x = (C.mean/1000), y = (P.mean/1000)),method = 'lm', color = 'black', se = F) +
  # stat_smooth(data = brown.df,method = 'lm', mapping = aes(x = (C.mean/1000), y = (N.mean/1000)), linetype = 'dotted', se = F, colour = 'black') +
  # stat_smooth(data = brown.df, method = 'lm', mapping = aes(x = (C.mean/1000), y = (P.mean/1000)), linetype = 'dotted', se = F, colour = 'black') +
  geom_point(size = 2, colour = 'black') +
  geom_point(aes(x = (C.mean/1000), y = (P.mean/1000)), size = 2, shape = 21, colour = 'black', fill = "white") + 
  # geom_point(data = brown.df, aes(x = (C.mean/1000), y = (N.mean/1000)), shape = 24, colour = 'black', fill = 'black', size = 2) +
  # geom_point(data = brown.df, aes(x = (C.mean/1000), y = (P.mean/1000)), shape = 24, colour = 'black', fill = 'white', size = 2) +
  scale_y_log10(limits = c(0.01,30), breaks = breaks_y, labels = labels_y) + 
  scale_x_log10(limits = c(0.3,500), breaks = breaks_x, labels = labels_x)+
  # annotate('text', x = 66, y = 20, size = 3.5, parse = T, label = paste("y == ",round(10^green_cn$regression.results$Intercept[4],2),"*x^",round(green_cn$regression.results$Slope[4],2), sep = ""), hjust = 0)+
  # annotate('text', x = 66, y = 13.5, size = 3.5,parse = T, label = paste("r^2 == ",round(green_cn$rsquare,2), sep = ""), hjust = 0) +
  # annotate('text', x = 66, y = 0.6, size = 3.5, parse = T, label = paste("y == ",round(10^green_cp$regression.results$Intercept[4],2),"*x^",round(green_cp$regression.results$Slope[4],2), sep = ""), hjust = 0) +
  # annotate('text', x = 66, y = 0.35, size = 3.5, parse = T, label = paste("r^2 == ", round(green_cp$rsquare,2),sep = ""), hjust = 0) +
  annotate('text', x = 50, y = 10,  size = 3.5, parse = T, label = paste("y ==",round(10^brown_cn$regression.results$Intercept[4],2),"*x^",round(brown_cn$regression.results$Slope[4],2),sep = ""), hjust = 0) +
  annotate('text', x = 50, y = 6.5, size = 3.5, parse = T, label = paste("r^2 == ", round(brown_cn$rsquare,2),sep = ""), hjust = 0) +
  annotate('text', x = 50, y = 0.6, size = 3.5, parse = T, label = paste("y ==",round(10^brown_cp$regression.results$Intercept[4],2),"*x^",round(brown_cp$regression.results$Slope[4],2),sep = ""), hjust = 0) +
  annotate('text', x = 50, y = 0.35, size = 3.5, parse = T, label = paste("r^2 == ", round(brown_cp$rsquare,2),sep = ""), hjust = 0) +
  # annotate('point', x = 0.4, y = 30, size = 2, colour = "black") +
  # annotate('point', x = 0.4, y = 20, size = 2, shape = 21, colour = 'black', fill ='white')+
  # annotate('point', x = 0.48, y = 30, size = 2, shape = 24, colour = "black", fill = 'black') +
  # annotate('point', x = 0.48, y = 20, size = 2, shape = 24, colour = 'black', fill ='white')+ 
  # annotate('text', x = 0.5, y = 30, size = 3.5, label = "Nitrogen", hjust = 0) +
  # annotate('text', x = 0.5, y = 20, size = 3.5, label = 'Phosphorus', hjust = 0) +
  # annotate('segment', x = 0.3, xend = 0.55, y = 13, yend = 13, colour = 'black', linetype = 'solid', size = 1.1)+
  # annotate('segment', x = 0.3, xend = 0.55, y = 10, yend = 10, colour = 'black', linetype = 'dotted', size = 1.05)+
  # annotate('text', x = 0.55, y = 13, size = 3.5, label = "Green", hjust = 0) +
  # annotate('text', x = 0.55, y = 10, size = 3.5, label = 'Brown', hjust = 0) +
  annotate('text', x = 400, y = 0.01, size = 5, label = "C")+
  theme(panel.grid = element_blank(), axis.title = element_blank(), axis.text = element_text(size = 14)));
# grid.draw(F3)

  
lay = matrix(c(1,2,
               3,NA), 
             ncol = 2, nrow = 2, byrow = TRUE)  
tiff(file = "./submission/Figures/F3_biomass-pools_nutrient-mass.tiff", res = 600, height = 7, width = 7, unit = "in",compression = "lzw")
grid.arrange(grobs = list(F1,F2, F3), layout_matrix = lay, left = textGrob(label = expression('Nutrient mass (g'~m^-2~')'), rot = 90), bottom = textGrob(label = expression('Benthic organic matter (g C'~m^-2~')')))
dev.off()
####