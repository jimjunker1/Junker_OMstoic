##### Compartment level violins ###
fig3 = function() {
j.df_mod = j.df %>%
  dplyr::select(c(Stream,SPP,Sample,C.tot,N.tot,P.tot)) %>%
  mutate(SPP = dplyr::recode(SPP, CBOMe = "Biofilm", FBOMe = "Biofilm")) %>%
  group_by(Stream, SPP, Sample) %>%
  mutate(C.tot = sum(C.tot, na.rm = T), 
         N.tot = sum(N.tot, na.rm = T), 
         P.tot = sum(P.tot, na.rm = T)) %>%
  mutate(CN = (C.tot/N.tot)*(14.007/12.011),
         CP = (C.tot/P.tot)*(30.9737/12.011),
         NP = (N.tot/P.tot)*(30.9737/14.007)) %>%
  ungroup()
         
j.df_mod = j.df_mod %>%
  filter(CN < 55 & NP < 45) %>%
  filter(!SPP %in% c("PotPer", "Wood", "Misc")) %>%
  mutate(SPP = dplyr::recode(SPP, CalHam = "Calli. spp.",
                      CalSta = "Calli. spp.", MyrAlt = "Calli. spp.",
                      Horsetail = "Equis."))

j.df_sum <- j.df_mod %>%
  group_by(SPP) %>%
  summarise(CN = mean(CN, na.rm = T),
            CP = mean(CP, na.rm = T),
            NP = mean(NP, na.rm = T),
            CNsd = sd(CN, na.rm = T),
            CPsd = sd(CP, na.rm = T),
            NPsd = sd(NP, na.rm = T))

CN = j.df_mod$CN
CP = j.df_mod$CP
NP = j.df_mod$CP

stoic_man = Manova(lm(cbind(CN,CP,NP)~SPP, j.df_mod))
summary.aov(stoic_man)
  
jul.sppCN <- ggplot(j.df_mod, aes(x = SPP, y = CN)) +
  geom_violin(aes(x = SPP, y = CN, colour = SPP, fill = SPP), alpha = 0.7) +
  #geom_boxplot(fill = "grey", size = 1.2, outlier.shape = NA) + 
  geom_jitter(shape = 16, position = position_jitter(0.1),alpha = 0.5) +
  coord_cartesian(ylim = c(0,55)) +
  ylab("C:N (molar)") + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14), #axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.margin = unit(c(0,0.1,0,0), "in")); jul.sppCN
f1 = ggplotGrob(jul.sppCN)
jul.sppCP <- ggplot(j.df_mod, aes(x = SPP, y = CP)) +
  geom_violin(aes(x = SPP, y = CP, colour = SPP, fill = SPP), alpha = 0.7) +
  #geom_boxplot(fill = "grey", size = 1.2, outlier.shape = NA) + 
  geom_jitter(shape = 16, position = position_jitter(0.1),alpha = 0.5) +
  #coord_cartesian(ylim = c(0,87)) +
  ylab("C:P (molar)") + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14), #axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.margin = unit(c(0,0.1,0,0), "in")); jul.sppCP
f2 = ggplotGrob(jul.sppCP)
jul.sppNP <- ggplot(j.df_mod, aes(x = SPP, y = NP)) +
  geom_violin(aes(x = SPP, y = NP, colour = SPP, fill = SPP), alpha = 0.7) +
  #geom_boxplot(fill = "grey", size = 1.2, outlier.shape = NA) + 
  geom_jitter(shape = 16, position = position_jitter(0.1), alpha = 0.5) +
  coord_cartesian(ylim = c(0,45)) +
  ylab("N:P (molar)") + 
  theme(legend.position = "none", axis.title.x = element_blank(), #axis.text.x = element_blank(), 
        axis.text.y = element_text(size = 14), #axis.title.y = element_blank(),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(), 
        plot.margin = unit(c(0,0.1,0,0), "in")); jul.sppNP
f3 = ggplotGrob(jul.sppNP)

tiff("./output/comp-stoic_plot.tiff", res = 600,  height = 5, width = 7, units = "in", compression = "lzw" )
grid.draw(gridExtra::gtable_rbind(f1,f2,f3))
dev.off()
}
