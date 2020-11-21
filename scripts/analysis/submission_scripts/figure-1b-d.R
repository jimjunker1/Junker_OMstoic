#figure 1 b-d
# Ecosystem and compartment distribution ####
##Community stoic and mean stoic
fig1b_d = function() {
  stream_temps = c(10.7,20.5,15.9,4.8,5.7,17.5,27.4,11.0,5.2,4.8,7.4)
#reorder(round(j.summary_stoic$mean_temp,2),-j.summary_stoic$C.mean)
#levels(j.summary_stoic$Stream)
#j.summary_stoic$mean_temp

f2 = ggplotGrob(ggplot(j.stoic.df, aes(x = Stream, y = CN)) +
  geom_violin(data = j.df, aes(x = Stream, y = CN, colour = Stream, fill = Stream), alpha = 0.7) +
  geom_boxplot(fill = "grey", size = 1.2, outlier.shape = NA) + 
  scale_y_continuous(limits = c(0,87)) +
  ylab("C:N (molar)") + 
  annotate('text', x = 11, y = 82, label = "B", size = 10) +
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size = 16), #axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.margin = unit(c(0,0.1,0,0), "in")))
f3 = ggplotGrob(ggplot(j.stoic.df, aes(x = Stream, y = CP)) +
  geom_violin(data = j.df, aes(x = Stream, y = CP, colour = Stream, fill = Stream), alpha = 0.7) +
  geom_boxplot(fill = "grey", size = 1.2, outlier.shape = NA) +
  scale_y_continuous(limits = c(0,1060))+
  annotate('text', x = 11, y = 1030, label = "C", size = 10) +
  ylab("C:P (molar)") + theme(legend.position = "none",axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_text(size = 16), #axis.title.y = element_blank(),
                              panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.margin = unit(c(0,0.1,0,0), "in")))

f4 = ggplotGrob(ggplot(j.stoic.df, aes(x = Stream, y = NP)) +
  geom_violin(data = j.df, aes(x = Stream, y = NP, colour = Stream, fill = Stream), alpha = 0.7) +
  geom_boxplot(fill = "grey", size = 1.2, outlier.shape = NA) +
  ylab("N:P (molar)") + scale_x_discrete(labels = stream_temps) +
  annotate('text', x = 11, y = 46, label = "D", size = 10) +
  theme(legend.position = "none",axis.title.x = element_blank(), axis.text.y = element_text(size = 16), axis.text.x = element_text(size = 20),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), plot.margin = unit(c(0,0.1,0,0), "in")))

maxwidth = grid::unit.pmax(f2$widths[2:5], f3$widths[2:5], f4$width[2:5])
f2$widths[2:5] = as.list(maxwidth)
f3$widths[2:5] = as.list(maxwidth)
f4$widths[2:5] = as.list(maxwidth)
grid.draw(gridExtra::gtable_rbind(f2,f3,f4))
}
