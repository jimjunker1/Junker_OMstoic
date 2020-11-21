##################################### NMDS ##################################
source("./scripts/analysis/submission_scripts/NMDS_script.R")
stream_order = j.summary_stoic %>% select(Stream, mean_temp) %>%
  arrange(-mean_temp) %>% mutate(Stream = factor(Stream))
stream_temp_labels = round(stream_order$mean_temp, 1)
names(stream_temp_labels) = factor(stream_order$Stream)

all.vec = rbind(env.vec, spp.vec)
treat <- df.env$Stream

find_hull <- function(df) df[chull(df$NMDS1, df$NMDS2),]
hulls <- ddply(NMDS.scrs, "Stream", find_hull)

cbbPalette <- c( "#FF0000", "#E50019", "#CC0033", "#B2004C", "#990066", "#7F007F", "#650099", "#4C00B2", "#3200CC",
                 "#1900E5", "#0000FF")
#cbbPalette <- c("#67001f", "#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#660066", "#d1e5f0", "#92c5de", "#4393c3", "#2166ac", "#053061")
shapes <- c(21,23,24,21,23,24,21,23,24,21,23)
hulls$Stream = factor(hulls$Stream, levels = c("hver", "st8", "st6", "st1", "st5", "st9", "st11U", "st14", "st11L", "st13", "st17"), labels = stream_temp_labels)#c("Hver", "ST8", "ST6", "ST1", "ST5", "ST9", "ST11U", "ST14", "ST11L", "ST13", "ST17"))
vec.plot <- ggplot(NMDS.scrs) +
  geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, fill = Stream, color = Stream), alpha = 0.3) +	
  geom_point(data = hulls,aes(x = NMDS1, y = NMDS2, fill = Stream, shape = Stream), size = 3, colour = "black") +
  coord_cartesian(xlim = c(-1.4,1.8)) +
  geom_segment(data = env.vec, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2), size = 1, 
               arrow = arrow(length(unit(0.5, "cm")))) +
  ggrepel::geom_label_repel(data = env.vec, aes(x = NMDS1, y = NMDS2, label = Species), size = 4, box.padding= 0.35, segment.color = "grey", segment.size = 1.2, force = 1.2) +
  ggrepel::geom_text_repel(data = spp.vec, aes(x = NMDS1, y = NMDS2, label = Species), size = 4, box.padding = 0.35, fontface = "italic", segment.color = "grey", segment.size = 1, force = 1.2) +
  scale_shape_manual(values = shapes, labels = stream_temp_labels) + 
  scale_fill_manual(values = cbbPalette, labels = stream_temp_labels) + 
  scale_color_manual(values = cbbPalette, labels = stream_temp_labels) +
  annotate("text", x = -0.97, y = 0.95, size = 5, parse = T, label=  paste("stress ==",round(jul.NMDS$stress,2))) +
  theme(legend.position = c(0.9,0.6), 
        legend.background = element_rect(fill = NA),
        legend.title = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank());vec.plot 

tiff("/.output/Jul_comm_NMDS.tiff", res = 600, height = 5, width = 8, units = "in", compression = 'lzw')
print(vec.plot)
dev.off()

### END figure code###