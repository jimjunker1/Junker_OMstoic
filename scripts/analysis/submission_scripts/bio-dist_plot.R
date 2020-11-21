# plot of biomass distribution and total biomass
figS5 = function(){
gb_C.lm = lm(log10(C.mean/1000)~log10(G.B), j.summary_stoic)
fS5 = ggplot(j.summary_stoic, aes(x = log10(G.B), y = log10(C.mean/1000), fill = mean_temp)) + 
  stat_smooth(method = "lm", se = F, colour = "dark grey", size = 1.2)+
  geom_point(size = 3, shape = 21, colour = "black") + 
  scale_fill_gradientn(name = "Temperature", colors = ocecolors[[10]]) +
  scale_x_continuous(name = expression("log"[10]*"Green Biomass/Brown Biomass"), limits = c(-0.5,1.5)) +
  scale_y_continuous(name = expression("log"[10]*"Organic matter (g C m"^-2*")"), limits = c(0,3)) +
  annotate('text', x = -0.5, y = 3, hjust = 0, label = paste("r^2 == ",round(summary(gb_C.lm)$adj.r.squared,2), sep = ""),parse = T)+
  annotate('text', x = 1.5, y  = 3, hjust = 1, label = "A") +
  theme(legend.position = c(0.8,0.25),axis.title.x = element_blank())

tiff("./output/bio_distribution_plot.tiff", height = 5, width = 5, unit = "in", res = 600, compression = "lzw") 
print(fS5)
dev.off()
}