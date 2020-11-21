###### Flow/disturbancePCA with predicted substrate #######
### Plotting PCA ####
library(factoextra)
#grid.table(as.data.frame(
#d = summary(flow.pca$importance[[2]])#))
make_pca_plot = function(){
  #cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
set.seed(101)
  mean_temp = j.summary_stoic$mean_temp
flow_pca_plot = fviz_pca_biplot(flow.pca, repel = T,
                                  col.ind = mean_temp,
                                  gradient.cols = ocecolors[[10]],
                                  col.var = "#696969",
                                  legend.title = "mean_temp") +
    theme(panel.grid = element_blank())#;flow_pca_plot
#d = autoplot(flow.pca, data = j.summary_stoic, colour = 'Stream', size = 5, 
#         loadings = T, loadings.label = T)
tiff(file = "./output/PCA_plot.tiff", res = 600, width = 5, height = 4.5, units = "in", compression = "lzw")
print(flow_pca_plot)
dev.off()
}
