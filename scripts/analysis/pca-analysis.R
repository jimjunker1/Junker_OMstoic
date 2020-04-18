###### Flow/disturbancePCA with predicted substrate #######
flow.var = j.summary_stoic[,c('cvQ','D50substrate_mm','medianQ_L_s','medianvelocity_m_s','slope_perc','maxpower_w')]
row.names(flow.var) = j.summary_stoic$stream
#PCA analysis 
set.seed(101)
flow.pca = prcomp(~CV+sub.pred+median_Q+velocity+Slope+max.power, data = flow.var, center = TRUE, scale = TRUE, na.action = na.omit)

axes = predict(flow.pca, newdata = j.summary_stoic)
j.summary_stoic = cbind(j.summary_stoic, axes)
j.summary_stoic %>% mutate(G.B = Green/Brown) -> j.summary_stoic
axes2 = data.frame(stream = j.summary_stoic$stream, axes)
j.stoic.df = j.stoic.df %>% left_join(axes2, by = "stream")
#############  END PCA CODE ################