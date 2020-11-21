figS3 = function() {
green.df = GB_summary[which(GB_summary$G_B == 'Green'),]
green.samp = GB_samples[which(GB_samples$G_B == 'Green'),]
brown.df = GB_summary[which(GB_summary$G_B == 'Brown'),]
brown.samp = GB_samples[which(GB_samples$G_B == 'Brown'),]

green.df %>% left_join(j.summary_stoic[,c(1,50:55)]) -> green.df
brown.df %>% left_join(j.summary_stoic[,c(1,50:55)]) -> brown.df
green.samp %>% left_join(j.summary_stoic[,]) -> green.samp
brown.samp %>% left_join(j.summary_stoic[,]) -> brown.samp


OM.pc1 = ggplotGrob(ggplot(green.df, aes( x= PC1, log10(C.mean/1000))) +
                      stat_smooth(data = green.df, aes(x = PC1, log10(C.mean/1000)), method = "loess", span = 0.9, size = 1.2, colour = "dark green", se = F, alpha = 0.2) +
                      geom_point( data = green.samp, aes( x = PC1, log10(C.sum/1000)), size = 1.2, shape = 21, alpha = 0.6, fill = "white", color = "dark green") +
                      stat_smooth(data = brown.df, aes(x = PC1, log10(C.mean/1000)), method = "loess", span = 0.9, size = 1.2, colour = "brown", se = F, alpha = 0.2) +
                      geom_point( data = brown.samp, aes( x = PC1, log10(C.sum/1000)), size = 1.2, shape = 24, alpha = 0.6, fill = "white", color = "brown") +
                      geom_point(colour = "black", shape = 21, fill = "dark green", stroke = 1, size = 2) +
                      geom_point(data = brown.df, aes(x = PC1, y = log10(C.mean/1000)), colour = "black", shape = 24, fill = "brown", stroke = 1, size = 2) +
                      scale_x_continuous(limits = c(-3,3), name = "PC1") + 
                      scale_y_continuous(limits = c(-1.5,3), name = expression("log"[10]*"Organic matter (g C m"^-2*")")) +
                      annotate("text", x = -2.95, y = 3, hjust = 0, label = expression("+ CV"[Q]), size = 2.5) +
                      annotate("text", x = 2.95, y = 3, hjust = 1, label = expression("- CV"[Q]), size = 2.5) +
                      annotate("text", x = -2.95, y = 2.8, hjust = 0, label = expression("+ Power"[max]), size = 2.5) +
                      annotate("text", x = 2.95, y = 2.8, hjust = 1, label = expression("- Power"[max]), size = 2.5) +
                      annotate("text", x = 2.5, y = -1.2, label = "A", size = 8)+
                      theme(plot.margin = unit(c(t = 0.1,r = 0.1,b = 0.1,l = 0.1), "cm"), axis.text = element_text(size = 11),
                            panel.grid = element_blank(), axis.title = element_text(size = 12)))
grid.draw(OM.pc1)

OM.pc2 = ggplotGrob(ggplot(green.df, aes( x= PC2, log10(C.mean/1000))) +
                      stat_smooth(data = green.df, aes(x = PC2, log10(C.mean/1000)), method = "loess", span = 0.9, size = 1.2, colour = "dark green", se = F, alpha = 0.2) +
                      geom_point( data = green.samp, aes( x = PC2, log10(C.sum/1000)), size = 1.2, shape = 21, alpha = 0.6, colour = "dark green", fill = "white") +
                      stat_smooth(data = brown.df, aes(x = PC2, log10(C.mean/1000)), method = "loess", span = 0.9, size = 1.2, colour = "brown", se = F, alpha = 0.2) +
                      geom_point( data = brown.samp, aes( x = PC2, log10(C.sum/1000)), size = 1.2, shape = 24, alpha = 0.6, fill = "white", color = "brown") +
                      geom_point(colour = "black", shape = 21, fill = "dark green", stroke = 1, size = 2) +
                      geom_point(data = brown.df, aes(x = PC2, y = log10(C.mean/1000)), colour = "black", shape = 24, fill = "brown", stroke = 1, size = 2) +
                      scale_x_continuous(limits = c(-3,3), name = "PC2") + 
                      scale_y_continuous(limits = c(-1.5,3), name = expression("log"[10]*"Organic matter (g C m"^-2*")")) +
                      annotate("text", x = -2.95, y = 3, hjust = 0, label = expression("- Slope"), size = 2.5) +
                      annotate("text", x = 2.95, y = 3, hjust = 1, label = expression("+ Slope"), size = 2.5) +
                      annotate("text", x = -2.95, y = 2.8, hjust = 0, label = expression("- Substrate"), size = 2.5) +
                      annotate("text", x = 2.95, y = 2.8, hjust = 1, label = expression("+ Substrate"), size = 2.5) +
                      annotate('text', x = -2.95, y = 2.6, hjust = 0, label = expression("+ Velocity"), size = 2.5) +
                      annotate('text', x = 2.95, y = 2.6, hjust = 1, label = expression("- Velocity"), size = 2.5) +
                      annotate('text', x = -2.95, y = 2.4, hjust = 0, label = expression("+ Median"[Q]), size = 2.5) +
                      annotate('text', x = 2.95, y = 2.4, hjust = 1, label = expression("- Median"[Q]), size = 2.5) +
                      annotate("text", x = 2.5, y = -1.2, label = "B", size = 8)+
                      theme(plot.margin = unit(c(t = 0.1,r = 0.1,b = 0.1,l = -1), "cm"), axis.text = element_text(size = 11), axis.title.x = element_text(size = 12),
                            panel.grid = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()))
grid.draw(OM.pc2)

#lm details r2 = 0.07 p-value = 0.21
OM.temp <- ggplotGrob(ggplot(green.df, aes( x = mean_temp, y = log10(C.mean/1000))) +
                        stat_smooth(data = green.df, aes(x = mean_temp, log10(C.mean/1000)), method = "loess", span = 0.9, size = 1.2, colour = "dark green", se = F, alpha = 0.2) +
                        geom_point( data = green.samp, aes( x = mean_temp, log10(C.sum/1000)), size = 1.2, shape = 21, alpha = 0.6, colour = "dark green", fill = "white") +
                        stat_smooth(data = brown.df, aes(x = mean_temp, log10(C.mean/1000)), method = "loess", span = 0.9, size = 1.2, colour = "brown", se = F, alpha = 0.2) +
                        geom_point( data = brown.samp, aes( x = mean_temp, log10(C.sum/1000)), size = 1.2, shape = 24, alpha = 0.6, fill = "white", color = "brown") +
                        geom_point(colour = "black", shape = 21, fill = "dark green", stroke = 1, size = 2) +
                        geom_point(data = brown.df, aes(x = mean_temp, y = log10(C.mean/1000)), colour = "black", shape = 24, fill = "brown", stroke = 1, size = 2) +
                        scale_x_continuous(limits = c(0,30),name = expression("Temperature ("~degree*C~")"), expand = c(0,0.4)) +
                        scale_y_continuous(limits = c(-1.5,3),name = expression("log"[10]*"Organic matter (g C m"^-2*")"),expand = c(0,0.1)) +

                        annotate("text", x = 28, y = -1.2, label = "C", size = 8)+
                        theme(plot.margin = unit(c(t = 0.1,r = 0.2,b = 0.1,l = -1), "cm"),axis.text = element_text(size = 11),axis.title.x = element_text(size = 12),
                              panel.grid = element_blank(), axis.title.y = element_blank(), axis.text.y = element_blank()))
grid.draw(OM.temp)
maxwidth = grid::unit.pmax(OM.pc1$widths[2:5], OM.pc2$widths[2:5], OM.temp$width[2:5])
maxheight = grid::unit.pmax(OM.pc1$heights[2:5], OM.pc2$hieghts[2:5], OM.temp$heights[2:5])
OM.pc1$widths[2:5] = as.list(maxwidth);OM.pc1$heights[2:5] = as.list(maxheight)
OM.pc2$widths[2:5] = as.list(maxwidth);OM.pc2$heights[2:5] = as.list(maxheight)
OM.temp$widths[2:5] = as.list(maxwidth);OM.temp$heights[2:5] = as.list(maxheight)
tiff("./output/raw-GB-env_plot.tiff", res = 600,  height = 2.75, width = 7, units = "in", compression = "lzw" )
grid.draw(gridExtra::gtable_cbind(OM.pc1,OM.pc2,OM.temp))
dev.off()
}
