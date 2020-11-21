# plot of biomass distribution and total biomass
figS6 = function(){
gb_CN.lm = lm(CN.mean~log10(G.B), data = j.summary_stoic);summary(gb_CN.lm)
fS2a = ggplotGrob(ggplot(j.summary_stoic, aes(x = log10(G.B), y = CN.mean, fill = mean_temp)) +
  stat_smooth(method = "lm", se = FALSE, colour = 'dark grey', size = 1.2)+
  geom_point(size = 3, shape = 21, colour = 'black') +
  scale_fill_gradientn(name = 'Temperature', colors = ocecolors[[10]]) +
  scale_x_continuous(name = expression("log"[10]*"Green Biomass/Brown Biomass"), limits = c(-0.5,1.5)) +
  scale_y_continuous(name = expression("C:N (molar)"), limits = c(0,27)) +
  annotate('text', x = -0.5, y = 27, hjust = 0, label = paste("r^2 == ",round(summary(gb_CN.lm)$adj.r.squared,2), sep = ""),parse = T)+
  annotate('text', x = 1.5, y  = 27, hjust = 1, label = "B") +
  theme(legend.position = "none",axis.title.x = element_blank()))#;fS2b  

gb_CP.lm = lm(CP.mean~log10(G.B), data = j.summary_stoic);summary(gb_CP.lm)
fS2b = ggplotGrob(ggplot(j.summary_stoic, aes(x = log10(G.B), y = CP.mean, fill = mean_temp)) +
  stat_smooth(method = "lm", se = FALSE, colour = 'dark grey', size = 1.2)+
  geom_point(size = 3, shape = 21, colour = 'black') +
  scale_fill_gradientn(name = 'Temperature', colors = ocecolors[[10]]) +
  scale_x_continuous(name = expression("log"[10]*"Green Biomass/Brown Biomass"), limits = c(-0.5,1.5)) +
  scale_y_continuous(name = expression("C:P (molar)"), limits = c(0,250), position = 'right') +
  annotate('text', x = -0.5, y = 250, hjust = 0, label = paste("r^2 == ",round(summary(gb_CP.lm)$adj.r.squared,2), sep = ""),parse = T)+
  annotate('text', x = 1.5, y  = 250, hjust = 1, label = "C") +
  theme(legend.position = c(0.8,0.25), legend.title = element_text(size = 6.5),
        axis.title.x = element_blank()))
temp_legend = cowplot::get_legend(fS2b)
fS2b = ggplotGrob(ggplot(j.summary_stoic, aes(x = log10(G.B), y = CP.mean, fill = mean_temp)) +
                    stat_smooth(method = "lm", se = FALSE, colour = 'dark grey', size = 1.2)+
                    geom_point(size = 3, shape = 21, colour = 'black') +
                    scale_fill_gradientn(name = 'Temperature', colors = ocecolors[[10]]) +
                    scale_x_continuous(name = expression("log"[10]*"Green Biomass/Brown Biomass"), limits = c(-0.5,1.5)) +
                    scale_y_continuous(name = expression("C:P (molar)"), limits = c(0,250), position = 'right') +
                    annotate('text', x = -0.5, y = 250, hjust = 0, label = paste("r^2 == ",round(summary(gb_CP.lm)$adj.r.squared,2), sep = ""),parse = T)+
                    annotate('text', x = 1.5, y  = 250, hjust = 1, label = "C") +
                    theme(legend.position = "none",
                          axis.title.x = element_blank()))
gb_NP.lm = lm(NP.mean~log10(G.B), data = j.summary_stoic);summary(gb_NP.lm)
fS2c = ggplotGrob(ggplot(j.summary_stoic, aes(x = log10(G.B), y = NP.mean, fill = mean_temp)) +
                    stat_smooth(method = "lm", se = FALSE, colour = 'dark grey', size = 1.2, linetype = 'dashed')+
                    geom_point(size = 3, shape = 21, colour = 'black') +
                    scale_fill_gradientn(name = 'Temperature', colors = ocecolors[[10]]) +
                    scale_x_continuous(name = expression("log"[10]*"Green Biomass/Brown Biomass"), limits = c(-0.5,1.5)) +
                    scale_y_continuous(name = expression("N:P (molar)"), limits = c(0,25), position = 'right') +
                    annotate('text', x = -0.5, y = 25, hjust = 0, label = paste("r^2 == ",round(summary(gb_NP.lm)$r.squared,2), sep = ""),parse = T)+
                    annotate('text', x = 1.5, y  = 25, hjust = 1, label = "C") +
                    theme(legend.position = "none",
                          axis.title.x = element_blank()));grid.draw(fS2c)
tiff("./output/bio_distribution-stoic_plot.tiff", height = 5, width = 5, unit = "in", res = 600, compression = "lzw") 
# print(fS2a)
lay = rbind(c(1,1,2,2),
            c(1,1,2,2),
            c(NA,3,4,4),
            c(NA,NA,4,4))
grid.arrange(fS2a, fS2b, temp_legend, fS2c, layout_matrix = lay,
             bottom = textGrob(expression("log"[10]*"Green Biomass/Brown Biomass")))
dev.off()
}