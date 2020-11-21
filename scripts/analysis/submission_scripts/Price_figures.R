#price figures
#source("./submission_scripts/Price_analysis.R")
#explore the Price analysis for C, N, and P
#PRICE_C_SPP = priceout_C$price_spp %>%
#  left_join(j.summary_pair)

# ggplot(PRICE_C_SPP, aes( y = CDE_ABUN, x = species)) + 
#   geom_violin(aes( x = species, y = CDE_ABUN, colour = species, fill = species), alpha = 0.7)+
#   geom_jitter(shape = 16, position = position_jitter(0.1),alpha = 0.5)
# ggplot(PRICE_C_SPP, aes( y = CDE_FUNC, x = species)) + 
#   geom_violin(aes( x = species, y = CDE_FUNC, colour = species, fill = species), alpha = 0.7)+
#   geom_jitter(shape = 16, position = position_jitter(0.1),alpha = 0.5)
# #ggplot(PRICE_C_SPP, aes( y = CDE_INTR, x = species)) + 
#   geom_violin(aes( x = species, y = CDE_INTR, colour = species, fill = species), alpha = 0.7)+
#   geom_jitter(shape = 16, position = position_jitter(0.1),alpha = 0.5)
# #ggplot(priceout_C$price_spp, aes( x = CDE_ABUN, y = CDE_FUNC)) + geom_point() + geom_abline(intercept = 0, slope = 1)

##### check that parts equal the functional difference #####
# use SCRE_LG, CDE_ABUN, CDE_FUNC, & CDE_INTR
##### Plot the distribution of Price components #####
#lengthen out j.price.C
#PRICE_C_sum = dplyr::summarise(j.price.C, meanTT = mean(T_T, na.rm = T), meanLG = mean(SCRE_LG, na.rm = T), meanABUN = mean(CDE_ABUN, na.rm = T), meanFUNC = mean(CDE_FUNC, na.rm = T), meanINTR = mean(CDE_INTR, na.rm = T), sdTT = se(T_T), sdLG = se(SCRE_LG), sdABUN = se(CDE_ABUN),
#                        sdFUNC = se(CDE_FUNC), sdINTR = se(CDE_INTR))
j.price.C = readRDS(file = "./object_files/j.price.C.rds")
j.price.delCN = readRDS(file = "./object_files/j.price.delCN.rds")
j.price.delCP = readRDS(file = "./object_files/j.price.delCP.rds")
j.price.delNP = readRDS(file = "./object_files/j.price.delNP.rds")

PRICE_C_sum = dplyr::summarise(j.price.C, meanLG = median(SCRE_LG, na.rm = T), meanABUN = median(CDE_ABUN, na.rm = T), meanFUNC = median(CDE_FUNC, na.rm = T), meanINTR = median(CDE_INTR, na.rm = T), sdLG = mad(SCRE_LG, na.rm = T), sdABUN = mad(CDE_ABUN, na.rm = T),
                        sdFUNC = mad(CDE_FUNC, na.rm = T), sdINTR = mad(CDE_INTR, na.rm = T))
PRICE_C_sumlong = t(PRICE_C_sum);colnames(PRICE_C_sumlong) = "price_term"

price_C_term = c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")
price_C_mean = PRICE_C_sumlong[1:4,]
price_C_sd = PRICE_C_sumlong[5:8,]

term_C_summary = data.frame(price_C_term, price_C_mean, price_C_sd)
term_C_summary$price_term = factor(term_C_summary$price_C_term, levels = c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR"))
term_C_density = gather(j.price.C[,c( "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")], price_term, value, c(SCRE_LG, CDE_ABUN:CDE_INTR), factor_key = T)

limits = aes(ymin = price_C_mean - price_C_sd, ymax = price_C_mean + price_C_sd)
CRAW_PRICE_PLOT =ggplotGrob(ggplot(term_C_summary, aes( x = price_term, y = price_C_mean)) + geom_hline(yintercept = 0) +
  #geom_violin(data = term_C_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
  geom_errorbar(limits, width = 0, size = 1.1) + geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
  #coord_cartesian(ylim = c(-900000,5000000)) +
  ylab(expression(paste(Delta,"BOM C mass (g C m"^-2*")"))) +
  #geom_vline(xintercept = 1.4, linetype = "dashed", colour = "grey", size = 1.2) +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), 
        panel.spacing = unit(c(0.1,0.1,0.1,0.1), "cm"), axis.title.y = element_text(size = 16)))
#grid.draw(CRAW_PRICE_PLOT)

##### Plot the distribution of Price components #####
#lengthen out j.price.C
#PRICE_CN_sum = dplyr::summarise(j.price.delCN, meanTT = mean(T_T, na.rm = T), meanLG = mean(SCRE_LG, na.rm = T), meanABUN = mean(CDE_ABUN, na.rm = T), meanFUNC = mean(CDE_FUNC, na.rm = T), meanINTR = mean(CDE_INTR, na.rm = T), 
#                         sdTT = se(T_T), sdLG = se(SCRE_LG), sdABUN = se(CDE_ABUN),
#                         sdFUNC = se(CDE_FUNC), sdINTR = se(CDE_INTR))
PRICE_CN_sum = dplyr::summarise(j.price.delCN,meanLG = median(SCRE_LG, na.rm = T), meanABUN = median(CDE_ABUN, na.rm = T), meanFUNC = median(CDE_FUNC, na.rm = T), meanINTR = median(CDE_INTR, na.rm = T), sdLG = mad(SCRE_LG, na.rm = T), sdABUN = mad(CDE_ABUN, na.rm = T),
                        sdFUNC = mad(CDE_FUNC, na.rm = T), sdINTR = mad(CDE_INTR, na.rm = T))
PRICE_CN_sumlong = t(PRICE_CN_sum);colnames(PRICE_CN_sumlong) = "price_term"

price_CN_term = c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")
price_CN_mean = PRICE_CN_sumlong[1:4,]
price_CN_sd = PRICE_CN_sumlong[5:8,]

term_CN_summary = data.frame(price_CN_term, price_CN_mean, price_CN_sd)
term_CN_summary$price_term = factor(term_CN_summary$price_CN_term, levels = c( "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR"))
term_CN_density = gather(j.price.delCN[,c( "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")], price_term, value, c(SCRE_LG, CDE_ABUN:CDE_INTR), factor_key = T)

limits = aes(ymin = price_CN_mean - price_CN_sd, ymax = price_CN_mean + price_CN_sd)
CNRAW_PRICE_PLOT =ggplotGrob(ggplot(term_CN_summary, aes( x = price_term, y = price_CN_mean)) + 
  geom_hline(yintercept = 1) +
  #geom_boxplot(data = term_CN_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey")  +
  #geom_violin(data = term_CN_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
  geom_errorbar(limits, width = 0, size = 1.1) + 
  geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
  #coord_cartesian(ylim = c(-100,160)) +
  ylab(expression(paste(Delta," BOM C:N ratio (molar)", sep = ""))) +
  theme(panel.grid = element_blank(), axis.title.x = element_blank(), panel.spacing = unit(c(0.1,0.1,0.1,0.1), "cm"), 
        axis.text.x = element_blank(), axis.title.y = element_text(size = 16)))
# grid.draw(CNRAW_PRICE_PLOT)

##CP price
#PRICE_CP_sum = dplyr::summarise(j.price.delCP, meanTT = mean(T_T, na.rm = T), meanLG = mean(SCRE_LG, na.rm = T), meanABUN = mean(CDE_ABUN, na.rm = T), meanFUNC = mean(CDE_FUNC, na.rm = T), meanINTR = mean(CDE_INTR, na.rm = T), sdTT = se(T_T), sdLG = se(SCRE_LG), sdABUN = se(CDE_ABUN),
#                        sdFUNC = se(CDE_FUNC), sdINTR = se(CDE_INTR))
PRICE_CP_sum = dplyr::summarise(j.price.delCP, meanLG = median(SCRE_LG, na.rm = T), meanABUN = median(CDE_ABUN, na.rm = T), meanFUNC = median(CDE_FUNC, na.rm = T), meanINTR = median(CDE_INTR, na.rm = T), sdLG = mad(SCRE_LG, na.rm = T), sdABUN = mad(CDE_ABUN, na.rm = T),
                        sdFUNC = mad(CDE_FUNC, na.rm = T), sdINTR = mad(CDE_INTR, na.rm = T))
PRICE_CP_sumlong = t(PRICE_CP_sum);colnames(PRICE_CP_sumlong) = "price_term"

price_CP_term = c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")
price_CP_mean = PRICE_CP_sumlong[1:4,]
price_CP_sd = PRICE_CP_sumlong[5:8,]

term_CP_summary = data.frame(price_CP_term, price_CP_mean, price_CP_sd)
term_CP_summary$price_term = factor(term_CP_summary$price_CP_term, levels = c( "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR"))
term_CP_density = gather(j.price.delCP[,c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")], price_term, value, c(SCRE_LG, CDE_ABUN:CDE_INTR), factor_key = T)

limits = aes(ymin = price_CP_mean - price_CP_sd, ymax = price_CP_mean + price_CP_sd)
CPRAW_PRICE_PLOT =ggplotGrob(ggplot(term_CP_summary, aes( x = price_term, y = price_CP_mean)) + 
  geom_hline(yintercept = 1) +
  #geom_violin(data = term_CP_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
  geom_errorbar(limits, width = 0, size = 1.1) + geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
  #coord_cartesian(ylim = c(-500,3100)) +
  ylab(expression(paste(Delta," BOM C:P ratio (molar)"))) +
  theme(panel.grid = element_blank(), panel.spacing = unit(c(0.1,0.1,0.1,0.1), "cm"), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(size = 16)))
# grid.draw(CPRAW_PRICE_PLOT)

##### Plot the distribution of NP Price components #####
#PRICE_NP_sum = summarise(j.price.NP, meanTT = mean(T_T, na.rm = T), meanLG = mean(SCRE_LG, na.rm = T), meanABUN = mean(CDE_ABUN, na.rm = T), meanFUNC = mean(CDE_FUNC, na.rm = T), meanINTR = mean(CDE_INTR, na.rm = T), sdTT = se(T_T), sdLG = se(SCRE_LG), sdABUN = se(CDE_ABUN),
#                         sdFUNC = se(CDE_FUNC), sdINTR = se(CDE_INTR))
PRICE_NP_sum = summarise(j.price.delNP, meanLG = median(SCRE_LG, na.rm = T), meanABUN = median(CDE_ABUN, na.rm = T), meanFUNC = median(CDE_FUNC, na.rm = T), meanINTR = median(CDE_INTR, na.rm = T), sdLG = mad(SCRE_LG, na.rm = T), sdABUN = mad(CDE_ABUN, na.rm = T),
                         sdFUNC = mad(CDE_FUNC, na.rm = T), sdINTR = mad(CDE_INTR, na.rm = T))
PRICE_NP_sumlong = t(PRICE_NP_sum);colnames(PRICE_NP_sumlong) = "price_term"

price_NP_term = c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")
price_NP_mean = PRICE_NP_sumlong[1:4,]
price_NP_sd = PRICE_NP_sumlong[5:8,]

term_NP_summary = data.frame(price_NP_term, price_NP_mean, price_NP_sd)
term_NP_summary$price_term = factor(term_NP_summary$price_NP_term, levels = c("SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR"))
term_NP_density = gather(j.price.delNP[,c( "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")], price_term, value, c(SCRE_LG, CDE_ABUN:CDE_INTR), factor_key = T)

limits = aes(ymin = price_NP_mean - price_NP_sd, ymax = price_NP_mean + price_NP_sd)
NPRAW_PRICE_PLOT = ggplotGrob(ggplot(term_NP_summary, aes( x = price_term, y = price_NP_mean)) + 
  geom_hline(yintercept = 1) +
  #geom_violin(data = term_NP_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
  geom_errorbar(limits, width = 0, size = 1.1) + geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
  #coord_cartesian(ylim = c(-100,100)) +
  ylab(expression(paste(Delta," BOM N:P ratio (molar)"))) +
  scale_x_discrete(labels = c("GAIN/LOSS", "REL-BIO", "STOIC-FLEX", "BIO-STOIC-INTR")) +
  theme(panel.grid = element_blank(), panel.spacing = unit(c(0.1,0.1,0.1,0.1), "cm"), axis.title.x = element_blank(), 
        axis.text.x = element_text( size = 10.5), axis.title.y = element_text(size = 16)))
# grid.draw(NPRAW_PRICE_PLOT)
# NPRAW_PRICE_GROB = ggplotGrob(ggplot(term_NP_summary, aes( x = price_term, y = price_NP_mean)) + 
#                                 geom_hline(yintercept = 1) +
#                                 #geom_violin(data = term_NP_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
#                                 geom_errorbar(limits, width = 0, size = 1.1) + geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
#                                 #coord_cartesian(ylim = c(-100,100)) +
#                                 ylab(expression(paste(Delta," BOM N:P ratio (molar)"))) +
#                                 scale_x_discrete(labels = c("GAIN/LOSS", "REL-BIO", "STOIC-FLEX", "BIO-STOIC-INTR")) +
#                                 theme(panel.grid = element_blank(), panel.spacing = unit(c(0.1,0.1,0.1,0.1), "cm"), axis.title.x = element_blank(), 
#                                       axis.text.x = element_text(size = 11),
#                                       axis.title.y = element_text(size = 16)))
# grid.newpage();grid.draw(NPRAW_PRICE_GROB)
##set widths 
maxwidth = grid::unit.pmax(CRAW_PRICE_PLOT$widths[2:5], 
                           CNRAW_PRICE_PLOT$widths[2:5], 
                           CPRAW_PRICE_PLOT$widths[2:5],
                           NPRAW_PRICE_PLOT$widths[2:5])

maxheight = grid::unit.pmax(CRAW_PRICE_PLOT$heights[2:5], 
                            CNRAW_PRICE_PLOT$heights[2:5], 
                            CPRAW_PRICE_PLOT$heights[2:5],
                            NPRAW_PRICE_PLOT$heights[2:5])

 CRAW_PRICE_PLOT $widths[2:5] = as.list(maxwidth)
 CNRAW_PRICE_PLOT$widths[2:5] = as.list(maxwidth)
 CPRAW_PRICE_PLOT$widths[2:5] = as.list(maxwidth)
 NPRAW_PRICE_PLOT$widths[2:5] = as.list(maxwidth)
 NPRAW_PRICE_GROB$widths[2:5] = as.list(maxwidth)

CRAW_PRICE_PLOT$heights[2:5] = as.list(maxheight)
CNRAW_PRICE_PLOT$heights[2:5] = as.list(maxheight)
CPRAW_PRICE_PLOT$heights[2:5] = as.list(maxheight)
NPRAW_PRICE_PLOT$heights[2:5] = as.list(maxheight)
NPRAW_PRICE_GROB$heights[2:5] = as.list(maxheight)
# NPRAW_PRICE_GROB$heights[6] = unit(0,'cm')
# NPRAW_PRICE_GROB$heights[2:5] = unit.c(NPRAW_PRICE_PLOT$heights + x_axis_grob_1$heights)
# x_axis_grob_1 = gtable_filter(NPRAW_PRICE_GROB, 'axis-b')
# panel_id = NPRAW_PRICE_PLOT$layout[NPRAW_PRICE_PLOT$layout$name == "panel", c("t","l","b","r")]
# NPRAW_PRICE_PLOT = gtable_add_rows(NPRAW_PRICE_PLOT, unit(1,"in"))
# NPRAW_PRICE_PLOT = gtable_add_grob(NPRAW_PRICE_PLOT, x_axis_grob_1,
#                                    t = 13, b = 13, l = panel_id$l,
#                                    name = 'new-x')
# # NPRAW_PRICE_PLOT$layout
# gtable_show_layout(NPRAW_PRICE_PLOT)
grid.draw(NPRAW_PRICE_PLOT)
# x_axis_grob_2 = gtable_filter(NPRAW_PRICE_GROB, 'axis-b')
# y_axis_grob = gtable_filter(NPRAW_PRICE_GROB, 'axis-l')
# y_title_grob = gtable_filter(NPRAW_PRICE_GROB, 'ylab-l')

# x_axis_grob_2$widths = unit.c(NPRAW_PRICE_PLOT$widths - unit.c(y_title_grob$widths + y_axis_grob$widths))
# x_axis_grob_2$layout[,"l"] = NPRAW_PRICE_PLOT$layout[NPRAW_PRICE_PLOT$layout$name=="panel","l"]
# x_axis_grob_2$layout[,"r"] = NPRAW_PRICE_PLOT$layout[NPRAW_PRICE_PLOT$layout$name=="panel","r"]
####
# g = list(CRAW_PRICE_PLOT, CNRAW_PRICE_PLOT, CPRAW_PRICE_PLOT, NPRAW_PRICE_PLOT, x_axis_grob_1)
# gs = arrangeGrob(grobs = g, layout_matrix = rbind(c(1,1,1,1,1,1,1),
#                                                   c(2,2,2,2,2,2,2),
#                                                   c(3,3,3,3,3,3,3),
#                                                   c(4,4,4,4,4,4,4),
#                                                   c(NA,NA,5,5,5,5,5)))
# # gtable_width(gs)
# gs = arrangeGrob(rbind(g[[1]],g[[2]], size = 'first'))
# gs = gtable_add_rows(gs, unit(0.5, 'null'))
# gtable_show_layout(gs)
# grid.newpage();grid.draw(gs)
# g$widths = unit.pmax(CRAW_PRICE_PLOT$widths, 
#                      CNRAW_PRICE_PLOT$widths,
#                      CPRAW_PRICE_PLOT$widths,
#                      NPRAW_PRICE_PLOT$widths)
# g = set_panel_heights(g, list(rep(unit(1,"null"),3), unit(1.5, 'null')))

tiff("Price_components.tiff", res = 600, height = 11, width = 5.5, unit = "in", compression = "lzw")
grid.arrange(CRAW_PRICE_PLOT,CNRAW_PRICE_PLOT, CPRAW_PRICE_PLOT, NPRAW_PRICE_PLOT, ncol = 1)
# grid.arrange(arrangeGrob(grobs = list(rbind(CRAW_PRICE_PLOT, CNRAW_PRICE_PLOT, size = 'first'),
#                          rbind(CPRAW_PRICE_PLOT, NPRAW_PRICE_PLOT, size = 'first'),
#                        x_axis_grob_2), ncol = 1))
# grid.draw(gs) 
# gs = grid.arrange(grobs = gs, ncol = 1)
 # panel_id <- NPRAW_PRICE_PLOT$layout[NPRAW_PRICE_PLOT$layout$name == "panel",c("t","l","b","r")]
 # # gs = arrangeGrob(grobs = g, ncol = 1)
 # gs = gtable_add_rows(gs, unit(1,"in"))
 # gs$layout
 # gs = gtable_add_grob(gs, x_axis_grob_1,
 #                        t = 5, l = panel_id$l, b = 5, r = panel_id$r, name = 'axis', 
 #                      clip = 'off')
 # grid.newpage();grid.draw(gs)
 dev.off()
#####
# ######### Lots of plots below #############3
# 
# ######do some plotting of the price components SRCE_LG#####
# j.price.C[j.price.C == 0] = NA
# hist(j.price.C$SCRE_LG)
# shapiro.test(j.price.C$SCRE_LG)
# ###
# temp_SCRE.lm = lm(SCRE_LG~temp.diff, data = j.price.C);summary(temp_SCRE.lm)
# pc1_SCRE.lm = lm(SCRE_LG~pc1.diff, data = j.price.C);summary(pc1_SCRE.lm)
# pc2_SCRE.lm = lm(SCRE_LG~pc2.diff, data = j.price.C);summary(pc2_SCRE.lm)
# cv_SCRE.lm = lm(SCRE_LG~cv.diff, data = j.price.C);summary(cv_SCRE.lm)
# imb_SCRE.lm = lm(SCRE_LG~imb.diff, data = j.price.C);summary(imb_SCRE.lm)
# bom_SCRE.lm = lm(SCRE_LG~bom.diff, data = j.price.C);summary(bom_SCRE.lm)
# 
# ggplot(j.price.C, aes(x = bom.diff, SCRE_LG)) + geom_point(size = 3)
# ggplot(j.price.C, aes(x = pc1.diff, SCRE_LG)) + geom_point(size = 4)
# ggplot(j.price.C, aes(x = pc2.diff, SCRE_LG)) + geom_point(size = 4)
# cv.price_C = ggplot(j.price.C, aes(x = cv.diff, y = SCRE_LG)) + geom_point(size = 6);cv.price_C
# 
# cor.test(j.price.C$no3.diff, j.price.C$SCRE_LG, method = "spearman", exact = F, na.action = "na.exlude")
# cor.test(j.price.C$imb.diff, j.price.C$SCRE_LG, method = "spearman", exact = F, use = "pairwise.complete.obs")
# ##do some plotting of the price components CDE_ABUN#####
# temp.pricelm = lm(CDE_ABUN~temp.diff,data = j.price.C); summary(temp.pricelm)
# pc1_CDEn.lm = lm(CDE_ABUN~pc1.diff, data = j.price.C);summary(pc1_CDEn.lm)
# pc2_CDEn.lm = lm(CDE_ABUN~pc2.diff, data = j.price.C);summary(pc2_CDEn.lm)
# cv_CDEn.lm = lm(CDE_ABUN~cv.diff, data = j.price.C);summary(cv_CDEn.lm)
# imb_CDEn.lm = lm(CDE_ABUN~imb.diff, data = j.price.C);summary(imb_CDEn.lm)
# bom_CDEn.lm = lm(CDE_ABUN~bom.diff, data = j.price.C);summary(bom_CDEn.lm)
# temp.CDEn_C = ggplot(j.price.C, aes(x = temp.diff, y = CDE_ABUN)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Influence of Relative biomass [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   #annotate('text', x = 25, y = 2.9, parse = T, label = "italic(p) == 0.19") +
#   #annotate('text', x = 23, y = 2.67, parse = T, label = "italic(p-value) == 0.17") +
#   theme(panel.grid = element_blank());temp.CDEn_C
# 
# ggplot(j.price.C, aes(x = cv.diff, y = CDE_ABUN)) + geom_point(size = 6)
# #  stat_smooth(method ="lm");cv.price_C; cor.test(j.price.C$cv.diff, log10(abs(j.price.C$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# #slope.price_C = ggplot(j.price.C, aes(x = slope.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6);slope.price_C; cor.test(j.price.C$slope.diff, log10(abs(j.price.C$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# cor.test(j.price.C$no3.diff, j.price.C$CDE_ABUN, method = "spearman", exact = F,  use = "pairwise.complete.obs")
# no3.CDEn_C = ggplot(j.price.C, aes(x = no3.diff, y =CDE_ABUN)) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   #geom_smooth(method = "lm", colour = "black", se = T, size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Influence of relative biomass [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   #annotate('text', x = 0.022, y = 2.9, parse = T, label = "italic(p) == -0.30") +
#   #annotate('text', x = 0.0204, y = 2.67, parse = T, label = "italic(p-value) == 0.02") +
#   theme(panel.grid= element_blank());no3.CDEn_C
# 
# cor.test(j.price.C$np.diff, j.price.C$CDE_ABUN, method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_FUNC#####
# temp_CDEp.lm = lm(CDE_FUNC~temp.diff, data = j.price.C);summary(temp_CDEp.lm)
# pc1_CDEp.lm = lm(CDE_FUNC~pc1.diff, data = j.price.C);summary(pc1_CDEp.lm)
# pc2_CDEp.lm = lm(CDE_FUNC~pc2.diff, data = j.price.C);summary(pc2_CDEp.lm)
# cv_CDEp.lm = lm(CDE_FUNC~cv.diff, data = j.price.C);summary(cv_CDEp.lm)
# imb_CDEp.lm = lm(CDE_FUNC~imb.diff, data = j.price.C);summary(imb_CDEp.lm)
# bom_CDEp.lm = lm(CDE_FUNC~bom.diff, j.price.C);summary(bom_CDEp.lm)
# cor.test(j.price.C$temp.diff,j.price.C$CDE_FUNC, method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# bom.CDEp_C = ggplot(j.price.C, aes(x = bom.diff, y = CDE_FUNC)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth( colour = "black", se = F, method = "lm",linetype = "dashed", size = 1.1) +
#   xlab(expression("BOM difference (g C m"^2*")")) +
#   ylab("Influence of plasticity [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   #annotate('text', x = 25, y = 4.2, parse = T, label = "italic(p) == 0.19") +
#   #annotate('text', x = 23, y = 4, parse = T, label = "italic(p-value) == 0.16") +
#   theme(panel.grid.major =element_blank());bom.CDEp_C
# 
# ggplot(j.price.C, aes(x = cv.diff, y = CDE_FUNC)) + geom_point(size = 6) + geom_smooth(colour = 'black', se = F)
# cor.test(j.price.C$cv.diff, j.price.C$CDE_FUNC, method = "spearman", exact = F, use = "pairwise.complete.obs")
# #cor(j.price.C$cv.diff, log10(abs(j.price.C$CDE_FUNC)), use = "pairwise.complete.obs")
# #slope.price_C = ggplot(j.price.C, aes(x = slope.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);slope.price_C; cor.test(j.price.C$slope.diff, log10(abs(j.price.C$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.C$no3.diff, j.price.C$CDE_FUNC, method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.C$np.diff, j.price.C$CDE_FUNC, method = "spearman", exact = F, use = "pairwise.complete.obs")
# ggplot(j.price.C, aes(x = np.diff, y = CDE_FUNC)) + geom_point(size = 4)
# ##do some plotting of the price components CDE_INT#####
# temp_CDEi.lm = lm(CDE_INTR~temp.diff, data = j.price.C);summary(temp_CDEi.lm)
# pc1_CDEi.lm = lm(CDE_INTR~pc1.diff, data = j.price.C);summary(pc1_CDEi.lm)
# pc2_CDEi.lm = lm(CDE_INTR~pc2.diff, data = j.price.C);summary(pc2_CDEi.lm)
# cv_CDEi.lm = lm(CDE_INTR~cv.diff, data = j.price.C);summary(cv_CDEi.lm)
# imb_CDEi.lm = lm(CDE_INTR~imb.diff, data = j.price.C);summary(imb_CDEi.lm)
# bom_CDEi.lm = lm(CDE_INTR~bom.diff, data = j.price.C);summary(bom_CDEi.lm)
# cor.test(j.price.C$temp.diff, log10(abs(j.price.C$CDE_INTR/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# bom.CDEi_C = ggplot(j.price.C, aes(x =bom.diff, y = CDE_INTR)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("BOM difference (g C m-2)") +
#   ylab("biomass * plasticity [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   #annotate('text', x = 25, y = 4.2, parse = T, label = "italic(p) == 0.28") +
#   #annotate('text', x = 23, y = 4, parse = T, label = "italic(p-value) == 0.04") +
#   theme(panel.grid = element_blank()); bom.CDEi_C
# 
# 
# #png("PriceC_env.png", res = 300, height = 20, width = 20, units = "in")
# grid.arrange(temp.SCRE_C, no3.SCRE_C, srp.SCRE_C, pc1.SCRE_C, pc2.SCRE_C,
#              temp.CDEn_C, no3.CDEn_C, srp.CDEn_C, pc1.CDEn_C, pc2.CDEn_C,
#              temp.CDEp_C, no3.CDEp_C, srp.CDEp_C, pc1.CDEp_C, pc2.CDEp_C,
#              temp.CDEi_C, no3.CDEi_C, srp.CDEi_C, pc1.CDEi_C, pc2.CDEi_C, ncol = 5, nrow = 4)
# #dev.off()
# 
# ##### Plot the distribution of Price components #####
# PRICE_N_sum = summarise(j.price.N, meanTT = mean(T_T, na.rm = T), meanLG = mean(SCRE_LG, na.rm = T), meanABUN = mean(CDE_ABUN, na.rm = T), meanFUNC = mean(CDE_FUNC, na.rm = T), meanINTR = mean(CDE_INTR, na.rm = T), sdTT = se(T_T, na.rm = T), sdLG = se(SCRE_LG, na.rm = T), sdABUN = se(CDE_ABUN, na.rm = T),
#                         sdFUNC = sd(CDE_FUNC, na.rm = T), sdINTR = sd(CDE_INTR, na.rm = T))
# #PRICE_N_sum = summarise(j.price.N, meanTT = median(T_T, na.rm = T), meanLG = median(SCRE_LG, na.rm = T), meanABUN = median(CDE_ABUN, na.rm = T), meanFUNC = median(CDE_FUNC, na.rm = T), meanINTR = median(CDE_INTR, na.rm = T), sdTT = mad(T_T, na.rm = T), sdLG = mad(SCRE_LG, na.rm = T), sdABUN = mad(CDE_ABUN, na.rm = T),
# #                        sdFUNC = mad(CDE_FUNC, na.rm = T), sdINTR = mad(CDE_INTR, na.rm = T))
# PRICE_N_sumlong = t(PRICE_N_sum);colnames(PRICE_N_sumlong) = "price_term"
# 
# price_N_term = c("T_T", "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")
# price_N_mean = PRICE_N_sumlong[1:5,]
# price_N_sd = PRICE_N_sumlong[6:10,]
# 
# term_N_summary = data.frame(price_N_term, price_N_mean, price_N_sd)
# term_N_summary$price_term = factor(term_N_summary$price_N_term, levels = c("T_T", "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR"))
# term_N_density = gather(j.price.N[,c("T_T","SCRE_LG","CDE_ABUN","CDE_FUNC","CDE_INTR")], price_term, value, c(T_T,SCRE_LG, CDE_ABUN:CDE_INTR), factor_key = T)
# 
# limits = aes(ymin = price_N_mean - price_N_sd, ymax = price_N_mean + price_N_sd)
# NRAW_PRICE_PLOT =ggplot(term_N_summary, aes( x = price_term, y = price_N_mean)) + geom_hline(yintercept = 0) +
#   geom_violin(data = term_N_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
#   geom_errorbar(limits, width = 0, size = 1.1) + geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
#   #coord_cartesian(ylim = c(-3.5,6)) +
#   ylab(expression(paste("Change in ecosystem N mass (g N m "^-2*")"))) +
#   geom_vline(xintercept = 1.4, linetype = "dashed", colour = "grey", size = 1.2) +
#   theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.title.y = element_text(size = 16));NRAW_PRICE_PLOT
# ######do some plotting of the price components SRCE_LG#####
# j.price.N[j.price.N == 0] = NA
# hist(j.price.N$SCRE_LG)
# hist(log10(abs(j.price.N$SCRE_LG)))
# shapiro.test(log10(abs(j.price.N$SCRE_LG)))
# ###
# cor.test(j.price.N$temp.diff, log10(abs(j.price.N$SCRE_LG)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.SCRE_N = ggplot(j.price.N, aes(x = temp.diff, y = SCRE_LG)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(SRE+SCE)) [g N m-2]") +
#   annotate('text', x = 25, y = 1.5, parse = T, label = "italic(p) == 0.30") +
#   annotate('text', x = 23, y = 1.2, parse = T, label = "italic(p-value) == 0.04") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.SCRE_N 
# 
# cv.price_N = ggplot(j.price.N, aes(x = cv.diff, y = SCRE_LG)) + geom_point(size = 6); cv.price_N; cor.test(j.price.N$cv.diff, log10(abs(j.price.N$SCRE_LG)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# slope.price_N = ggplot(j.price.N, aes(x = slope.diff, y = SCRE_LG)) + geom_point(size = 6);slope.price_N; cor.test(j.price.N$slope.diff, log10(abs(j.price.N$SCRE_LG)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.N$no3.diff, log10(abs(j.price.N$SCRE_LG/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.SCRE_N = ggplot(j.price.N, aes(x = no3.diff, y = log10(abs(SCRE_LG/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("DIN difference (Nh4+NO3 mg/L)") +
#   ylab("Log10(abs(SRE+SCE)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 0.5, parse = T, label = "italic(p) == 0.02") +
#   annotate('text', x = 0.0208, y = 0.2, parse = T, label = "italic(p-value) == 0.91") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.SCRE_N 
# 
# 
# cor.test(j.price.N$np.diff, log10(abs(j.price.N$SCRE_LG/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# ##do some plotting of the price components CDE_ABUN#####
# temp.pricelm = lm(CDE_ABUN~temp.diff,data = j.price.N); summary(temp.pricelm)
# cor.test(j.price.N$temp.diff, log10(abs(j.price.N$CDE_ABUN/1000)), method = "spearman", use = "pairwise.complete.obs", na.action = "na.exclude")
# temp.CDEn_N = ggplot(j.price.N, aes(x = temp.diff, y = CDE_ABUN)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEn)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 2.9, parse = T, label = "italic(p) == 0.20") +
#   annotate('text', x = 23, y = 2.75, parse = T, label = "italic(p-value) == 0.14") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEn_N
# 
# #cv.price_N = ggplot(j.price.N, aes(x = cv.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6) + scale_y_continuous(limits = c(6,10.5)) +
# #  stat_smooth(method ="lm");cv.price_C; cor.test(j.price.N$cv.diff, log10(abs(j.price.N$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# #slope.price_C = ggplot(j.price.N, aes(x = slope.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6);slope.price_C; cor.test(j.price.N$slope.diff, log10(abs(j.price.N$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# cor.test(j.price.N$no3.diff, log10(abs(j.price.N$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# no3.CDEn_N = ggplot(j.price.N, aes(x = no3.diff, y = CDE_ABUN)) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEn)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 2.9, parse = T, label = "italic(p) == -0.22") +
#   annotate('text', x = 0.0204, y = 2.67, parse = T, label = "italic(p-value) == 0.11") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEn_N
# 
# cor.test(j.price.N$np.diff, j.price.N$CDE_ABUN, method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEn_N = ggplot(j.price.N, aes(x = np.diff, y = CDE_ABUN)) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEn)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 2.9, parse = T, label = "italic(p) == -0.22") +
#   annotate('text', x = 0.0204, y = 2.67, parse = T, label = "italic(p-value) == 0.11") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEn_N
# 
# ##do some plotting of the price components CDE_FUNC#####
# cor.test(j.price.N$temp.diff, log10(abs(j.price.N$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEp_N = ggplot(j.price.N, aes(x = temp.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEp)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 2.6, parse = T, label = "italic(p) == 0.20") +
#   annotate('text', x = 23, y = 2.46, parse = T, label = "italic(p-value) == 0.15") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEp_N
# #cv.price_N = ggplot(j.price.N, aes(x = cv.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);cv.price_N; cor.test(j.price.N$cv.diff, log10(abs(j.price.N$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #cor(j.price.N$cv.diff, log10(abs(j.price.N$CDE_FUNC)), use = "pairwise.complete.obs")
# #slope.price_N = ggplot(j.price.N, aes(x = slope.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);slope.price_N; cor.test(j.price.N$slope.diff, log10(abs(j.price.N$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.N$no3.diff, log10(abs(j.price.N$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEp_N = ggplot(j.price.N, aes(x = no3.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 2.6, parse = T, label = "italic(p) == -0.21") +
#   annotate('text', x = 0.0208, y = 2.46, parse = T, label = "italic(p-value) == 0.12") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEp_N
# 
# 
# cor.test(j.price.N$np.diff, log10(abs(j.price.N$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_INT#####
# cor.test(j.price.N$temp.diff, log10(abs(j.price.N$CDE_INTR/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEi_N = ggplot(j.price.N, aes(x = temp.diff, y = log10(abs(CDE_INTR/1000)))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 2.6, parse = T, label = "italic(p) == 0.34") +
#   annotate('text', x = 23, y = 2.46, parse = T, label = "italic(p-value) == 0.01") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()); temp.CDEi_N
# #cv.price_N = ggplot(j.price.N, aes(x = cv.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); cv.price_N; cor.test(j.price.N$cv.diff, log10(abs(j.price.N$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #slope.price_N = ggplot(j.price.N, aes(x = slope.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); slope.price_N; cor.test(j.price.N$slope.diff, log10(abs(j.price.N$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.N$imb.diff, j.price.N$CDE_INTR, method = "spearman", exact = F, use = "pairwise.complete.obs")
# imb.CDEi_N = ggplot(j.price.N, aes(x = imb.diff, y = CDE_INTR)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab(expression(paste(Delta,"N:P imbalance",sep=""))) +
#   ylab("biomass * plasticity [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   #annotate('text', x = 0.0045, y = 2.6, parse = T, label = "italic(p) == -0.19") +
#   #annotate('text', x = 0.0033, y = 2.46, parse = T, label = "italic(p-value) == 0.16") +
#   theme(panel.grid = element_blank()); imb.CDEi_N
# 
# cor.test(j.price.N$srp.diff, log10(abs(j.price.N$CDE_INTR/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEi_N = ggplot(j.price.N, aes(x = srp.diff, y = log10(CDE_INTR/1000))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0039, y = 2.6, parse = T, label = "italic(p) == -0.07") +
#   annotate('text', x = 0.0023, y = 2.46, parse = T, label = "italic(p-value) == 0.59") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); srp.CDEi_N
# 
# 
# cor.test(j.price.N$pc1.diff, j.price.N$CDE_INTR, method = "spearman", use = "pairwise.complete.obs")
# pc1.CDEi_N = ggplot(j.price.N, aes(x = pc1.diff, y = CDE_INTR)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab(expression(paste(Delta,"PC1",sep = ""))) +
#   ylab("Influence of plasticity [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   #annotate('text', x = 5.81, y = 2.6, parse = T, label = "italic(p) == 0.04") +
#   #annotate('text', x = 5.4, y = 2.46, parse = T, label = "italic(p-value) == 0.77") +
#   theme(panel.grid = element_blank()); pc1.CDEi_N
# 
# cor.test(j.price.N$pc2.diff,j.price.N$CDE_INTR, method = "spearman", use = "pairwise.complete.obs")
# pc2.CDEi_N = ggplot(j.price.N, aes(x = pc2.diff, y = CDE_INTR)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.49, y = 2.6, parse = T, label = "italic(p) == -0.28") +
#   annotate('text', x = 0.33, y =2.46, parse = T, label = "italic(p-value) == 0.06") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc2.CDEi_N
# 
# png("PriceN_env.png", res = 300, height = 20, width = 20, units = "in")
# grid.arrange(temp.SCRE_N, no3.SCRE_N, srp.SCRE_N, pc1.SCRE_N, pc2.SCRE_N,
#              temp.CDEn_N, no3.CDEn_N, srp.CDEn_N, pc1.CDEn_N, pc2.CDEn_N,
#              temp.CDEp_N, no3.CDEp_N, srp.CDEp_N, pc1.CDEp_N, pc2.CDEp_N,
#              temp.CDEi_N, no3.CDEi_N, srp.CDEi_N, pc1.CDEi_N, pc2.CDEi_N, ncol = 5, nrow = 4)
# dev.off()
# 
# # use SCRE_LG, CDE_ABUN, CDE_FUNC, & CDE_INTR
# ##### Plot the distribution of Price components #####
# #lengthen out j.price.C
# PRICE_P_sum = summarise(j.price.P, meanTT = mean(T_T, na.rm = T), meanLG = mean(SCRE_LG, na.rm = T), meanABUN = mean(CDE_ABUN, na.rm = T), meanFUNC = mean(CDE_FUNC, na.rm = T), meanINTR = mean(CDE_INTR, na.rm = T), sdTT = sd(T_T, na.rm = T), sdLG = sd(SCRE_LG, na.rm = T), sdABUN = sd(CDE_ABUN, na.rm = T),
#                         sdFUNC = sd(CDE_FUNC, na.rm = T), sdINTR = sd(CDE_INTR, na.rm = T))
# #PRICE_P_sum = summarise(j.price.P, meanTT = median(T_T, na.rm = T), meanLG = median(SCRE_LG, na.rm = T), meanABUN = median(CDE_ABUN, na.rm = T), meanFUNC = median(CDE_FUNC, na.rm = T), meanINTR = median(CDE_INTR, na.rm = T), sdTT = mad(T_T, na.rm = T), sdLG = mad(SCRE_LG, na.rm = T), sdABUN = mad(CDE_ABUN, na.rm = T),
# #                        sdFUNC = mad(CDE_FUNC, na.rm = T), sdINTR = mad(CDE_INTR, na.rm = T))
# PRICE_P_sumlong = t(PRICE_P_sum);colnames(PRICE_P_sumlong) = "price_term"
# 
# price_P_term = c("T_T", "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")
# price_P_mean = PRICE_P_sumlong[1:5,]
# price_P_sd = PRICE_P_sumlong[6:10,]
# 
# term_P_summary = data.frame(price_P_term, price_P_mean, price_P_sd)
# term_P_summary$price_term = factor(term_P_summary$price_P_term, levels = c("T_T", "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR"))
# term_P_density = gather(j.price.P[,c("T_T", "SCRE_LG", "CDE_ABUN", "CDE_FUNC", "CDE_INTR")], price_term, value, c(T_T,SCRE_LG, CDE_ABUN:CDE_INTR), factor_key = T)
# 
# limits = aes(ymin = price_P_mean  - price_P_sd, ymax = price_P_mean + price_P_sd)
# PRAW_PRICE_PLOT =ggplot(term_P_summary, aes( x = price_term, y = price_P_mean)) + geom_hline(yintercept = 0) +
#   geom_violin(data = term_P_density, aes(x = price_term, y = value), alpha = 0.4, fill = "grey") +
#   geom_errorbar(limits, width = 0, size = 1.1) + geom_point(size = 3, shape = 23, colour = "black", fill = "white", stroke = 1.2) +
#   #coord_cartesian(ylim = c(-3.5,6)) +
#   ylab(expression(paste("Change in ecosystem P mass (g P m-2)"))) +
#   scale_x_discrete(labels = c("T_T", "SCE+SRE", "CDEn", "CDEp", "CDEi")) +
#   geom_vline(xintercept = 1.4, linetype = "dashed", colour = "grey", size = 1.2) +
#   theme(panel.grid = element_blank(), axis.title.x = element_blank(), axis.text.x = element_text(size = 13), axis.title.y = element_text(size = 16));PRAW_PRICE_PLOT
# #####
# pdf("ELEMENT_RAWPRICE.pdf", width = 5, height = 15)
# grid.arrange(CRAW_PRICE_PLOT, NRAW_PRICE_PLOT, PRAW_PRICE_PLOT, ncol = 1, nrow = 3)
# dev.off()
# 
# png("ELEMENT_RAWPRICE.png", res = 300, width = 7, height = 15, units = "in")
# grid.arrange(CRAW_PRICE_PLOT, NRAW_PRICE_PLOT, PRAW_PRICE_PLOT, ncol = 1, nrow = 3)
# dev.off()
# ######do some plotting of the price components SRCE_LG######
# j.price.P[j.price.P == 0] = NA
# hist(abs(j.price.P$SCRE_LG))
# hist(log10(abs(j.price.P$SCRE_LG)))
# shapiro.test(log10(abs(j.price.P$SCRE_LG)))
# ###
# cor.test(j.price.P$temp.diff, log10(abs(j.price.P$SCRE_LG)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.SCRE_P = ggplot(j.price.P, aes(x = temp.diff, y = log10(abs(SCRE_LG/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(SRE+SCE)) [g P m-2]") +
#   annotate('text', x = 25, y = 0, parse = T, label = "italic(p) == 0.29") +
#   annotate('text', x = 23, y = -0.2, parse = T, label = "italic(p-value) == 0.04") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.SCRE_P 
# 
# #cv.price_P = ggplot(j.price.P, aes(x = cv.diff, y = log10(abs(SCRE_LG)))) + geom_point(size = 6); cv.price_P; cor.test(j.price.P$cv.diff, log10(abs(j.price.P$SCRE_LG)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #slope.price_P = ggplot(j.price.P, aes(x = slope.diff, y = log10(abs(SCRE_LG)))) + geom_point(size = 6);slope.price_P; cor.test(j.price.P$slope.diff, log10(abs(j.price.P$SCRE_LG)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.P$no3.diff, log10(abs(j.price.P$SCRE_LG/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.SCRE_P = ggplot(j.price.P, aes(x = no3.diff, y = log10(abs(SCRE_LG/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("DIN difference (Nh4+NO3 mg/L)") +
#   ylab("Log10(abs(SRE+SCE)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 0, parse = T, label = "italic(p) == -0.02") +
#   annotate('text', x = 0.0208, y = -0.2, parse = T, label = "italic(p-value) == 0.90") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.SCRE_P 
# 
# cor.test(j.price.P$srp.diff, log10(abs(j.price.P$SCRE_LG/1000)), method = "spearman", exact = F, na.action = "na.exlude")
# srp.SCRE_P = ggplot(j.price.P, aes(x = srp.diff, y = log10(abs(SCRE_LG/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(SRE+SCE)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.023, y = 6.33, parse = T, label = "italic(p) == 0.008") +
#   annotate('text', x = 0.0218, y = 6.09, parse = T, label = "italic(p-value) == 0.96") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.SCRE_P
# 
# cor.test(j.price.P$pc1.diff, log10(abs(j.price.P$SCRE_LG/1000)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# pc1.SCRE_P = ggplot(j.price.P, aes(x = pc1.diff, y = log10(abs(SCRE_LG/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black",  size = 1.1) +
#   xlab("PC1 difference") +
#   ylab("Log10(abs(SRE+SCE)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.8, y = 6.33, parse = T, label = "italic(p) == -0.47") +
#   annotate('text', x = 5.32, y = 6.09, parse = T, label = "italic(p-value) == 0.002") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.SCRE_P
# 
# cor.test(j.price.P$pc2.diff, log10(abs(j.price.P$SCRE_LG/1000)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# pc2.SCRE_P = ggplot(j.price.P, aes(x = pc2.diff, y = log10(abs(SCRE_LG/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(SRE+SCE)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.11, y = 0, parse = T, label = "italic(p) == 0.08") +
#   annotate('text', x = 2.89, y = -0.2, parse = T, label = "italic(p-value) == 0.63") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.SCRE_P
# 
# cor.test(j.price.P$np.diff, log10(abs(j.price.P$SCRE_LG/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# ##do some plotting of the price components CDE_ABUN#####
# temp.pricelm = lm(log10(abs(CDE_ABUN))~temp.diff,data = j.price.P); summary(temp.pricelm)
# cor.test(j.price.P$temp.diff, log10(abs(j.price.P$CDE_ABUN/1000)), method = "spearman", use = "pairwise.complete.obs", na.action = "na.exclude")
# temp.CDEn_P = ggplot(j.price.P, aes(x = temp.diff, y = log10(abs(CDE_ABUN/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEn)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 2, parse = T, label = "italic(p) == 0.29") +
#   annotate('text', x = 23, y = 1.85, parse = T, label = "italic(p-value) == 0.15") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEn_P
# 
# #cv.price_P = ggplot(j.price.P, aes(x = cv.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6) + scale_y_continuous(limits = c(6,10.5)) +
# #  stat_smooth(method ="lm");cv.price_C; cor.test(j.price.P$cv.diff, log10(abs(j.price.P$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# #slope.price_C = ggplot(j.price.P, aes(x = slope.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6);slope.price_C; cor.test(j.price.P$slope.diff, log10(abs(j.price.P$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# summary(lm(log10(abs(CDE_ABUN/1000))~no3.diff, data = j.price.P))
# cor.test(j.price.P$no3.diff, log10(abs(j.price.P$CDE_ABUN/1000)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# no3.CDEn_P = ggplot(j.price.P, aes(x = no3.diff, y = log10(abs(CDE_ABUN/1000)))) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEn)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0022, y = 2, parse = T, label = "italic(p) == -0.26") +
#   annotate('text', x = 0.00204, y = 1.85, parse = T, label = "italic(p-value) == 0.01") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEn_P
# 
# cor.test(j.price.P$srp.diff, log10(abs(j.price.P$CDE_ABUN/1000)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# srp.CDEn_P = ggplot(j.price.P, aes(x = srp.diff, y = log10(abs(CDE_ABUN/1000)))) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0045, y = 2, parse = T, label = "italic(p) == 0.02") +
#   annotate('text', x = 0.0029, y = 1.85, parse = T, label = "italic(p-value) == 0.87") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.CDEn_P
# 
# cor.test(j.price.P$pc1.diff, log10(abs(j.price.P$CDE_ABUN/1000)), method = 'spearman', exact = F, use = "pairwise.complete.obs")
# pc1.CDEn_P = ggplot(j.price.P, aes(x = pc1.diff, y = log10(abs(CDE_ABUN/1000)))) + 
#   geom_point(size = 6) +  
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g P m-2]") +
#   #coord_cartesian(ylim = c(6.9,10.5)) +
#   annotate('text', x = 5.8, y = 2, parse = T, label = "italic(p) == -0.007") +
#   annotate('text', x = 5.55, y = 1.85, parse = T, label = "italic(p-value) == 0.96") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.CDEn_P
# 
# cor.test(j.price.P$pc2.diff, log10(abs(j.price.P$CDE_ABUN/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc2.CDEn_P = ggplot(j.price.P, aes(x = pc2.diff, y = log10(abs(CDE_ABUN/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g P m-2]") +
#   #coord_cartesian(ylim = c(6.9,10.5)) +
#   annotate('text', x = 0.53, y = 2, parse = T, label = "italic(p) == -0.19") +
#   annotate('text', x = 0.35, y = 1.85, parse = T, label = "italic(p-value) == 0.21") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.CDEn_P;
# 
# cor.test(j.price.P$np.diff, log10(abs(j.price.P$CDE_ABUN/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_FUNC#####
# cor.test(j.price.P$temp.diff, log10(abs(j.price.P$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEp_P = ggplot(j.price.P, aes(x = temp.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEp)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25.1, y = 2.05, parse = T, label = "italic(p) == 0.20") +
#   annotate('text', x = 23, y = 1.90, parse = T, label = "italic(p-value) == 0.14") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEp_P
# #cv.price_P = ggplot(j.price.P, aes(x = cv.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);cv.price_P; cor.test(j.price.P$cv.diff, log10(abs(j.price.P$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #cor(j.price.P$cv.diff, log10(abs(j.price.P$CDE_FUNC)), use = "pairwise.complete.obs")
# #slope.price_P = ggplot(j.price.P, aes(x = slope.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);slope.price_P; cor.test(j.price.P$slope.diff, log10(abs(j.price.P$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.P$no3.diff, log10(abs(j.price.P$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEp_P = ggplot(j.price.P, aes(x = no3.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0033, y = 2.2, parse = T, label = "italic(p) == -0.17") +
#   annotate('text', x = 0.00208, y = 2.05, parse = T, label = "italic(p-value) == 0.21") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEp_P
# 
# cor.test(j.price.P$srp.diff, log10(abs(j.price.P$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEp_P = ggplot(j.price.P, aes(x = srp.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0224, y = 2, parse = T, label = "italic(p) == 0.1") +
#   annotate('text', x = 0.0208, y = 1.85, parse = T, label = "italic(p-value) == 0.45") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.CDEp_P
# 
# cor.test(j.price.P$pc1.diff, log10(abs(j.price.P$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc1.CDEp_P = ggplot(j.price.P, aes(x = pc1.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.8, y = 2.1, parse = T, label = "italic(p) == -0.009") +
#   annotate('text', x = 5.5, y = 1.95, parse = T, label = "italic(p-value) == 0.96") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.CDEp_P
# 
# cor.test(j.price.P$pc2.diff, log10(abs(j.price.P$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc2.CDEp_P = ggplot(j.price.P, aes(x = pc2.diff, y = log10(abs(CDE_FUNC/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.11, y = 2.1, parse = T, label = "italic(p) == -0.09") +
#   annotate('text', x = 2.94, y = 1.95, parse = T, label = "italic(p-value) == 0.54") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.CDEp_P
# 
# cor.test(j.price.P$np.diff, log10(abs(j.price.P$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_INT#####
# cor.test(j.price.P$temp.diff, log10(abs(j.price.P$CDE_INTR/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEi_P = ggplot(j.price.P, aes(x = temp.diff, y = log10(abs(CDE_INTR/1000)))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEi)) [g P m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25.1, y = 2.1, parse = T, label = "italic(p) == 0.41") +
#   annotate('text', x = 23, y = 1.95, parse = T, label = "italic(p-value) == 0.002") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()); temp.CDEi_P
# #cv.price_P = ggplot(j.price.P, aes(x = cv.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); cv.price_P; cor.test(j.price.P$cv.diff, log10(abs(j.price.P$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #slope.price_P = ggplot(j.price.P, aes(x = slope.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); slope.price_P; cor.test(j.price.P$slope.diff, log10(abs(j.price.P$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.P$no3.diff, log10(abs(j.price.P$CDE_INTR/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEi_P = ggplot(j.price.P, aes(x = no3.diff, y = log10(abs(CDE_INTR/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0045, y = 2.1, parse = T, label = "italic(p) == -0.22") +
#   annotate('text', x = 0.0033, y = 1.95, parse = T, label = "italic(p-value) == 0.10") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); no3.CDEi_P
# 
# cor.test(j.price.P$srp.diff, log10(abs(j.price.P$CDE_INTR/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEi_P = ggplot(j.price.P, aes(x = srp.diff, y = log10(CDE_INTR/1000))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 2.1, parse = T, label = "italic(p) == -0.06") +
#   annotate('text', x = 0.0208, y =1.95, parse = T, label = "italic(p-value) == 0.65") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); srp.CDEi_P
# 
# 
# cor.test(j.price.P$pc1.diff, log10(abs(j.price.P$CDE_INTR/1000)), method = "spearman", use = "pairwise.complete.obs")
# pc1.CDEi_P = ggplot(j.price.P, aes(x = pc1.diff, y = log10(abs(CDE_INTR/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC1 difference") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.81, y = 2.1, parse = T, label = "italic(p) == 0.05") +
#   annotate('text', x = 5.4, y = 1.95, parse = T, label = "italic(p-value) == 0.73") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc1.CDEi_P
# 
# cor.test(j.price.P$pc2.diff, log10(abs(j.price.P$CDE_INTR/1000)), method = "spearman", use = "pairwise.complete.obs")
# pc2.CDEi_P = ggplot(j.price.P, aes(x = pc2.diff, y = log10(abs(CDE_INTR/1000)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(CDEi)) [g N m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.1, y = 2.16, parse = T, label = "italic(p) == -0.28") +
#   annotate('text', x = 2.9, y = 2.01, parse = T, label = "italic(p-value) == 0.07") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc2.CDEi_P
# 
# png("PriceP_env.png", res = 300, height = 20, width = 20, units = "in")
# grid.arrange(temp.SCRE_P, no3.SCRE_P, srp.SCRE_P, pc1.SCRE_P, pc2.SCRE_P,
#              temp.CDEn_P, no3.CDEn_P, srp.CDEn_P, pc1.CDEn_P, pc2.CDEn_P,
#              temp.CDEp_P, no3.CDEp_P, srp.CDEp_P, pc1.CDEp_P, pc2.CDEp_P,
#              temp.CDEi_P, no3.CDEi_P, srp.CDEi_P, pc1.CDEi_P, pc2.CDEi_P, ncol = 5, nrow = 4)
# dev.off()
# ########### End elemental raw Price figures ##############
# # ######do some plotting of the price components SRCE_LG#####
# # 
# hist(j.price.delCN$SCRE_LG)
# hist(j.price.delCN$CDE_ABUN)
# hist(j.price.delCN$CDE_FUNC)
# hist(j.price.delCN$CDE_INTR)
###
# j.price.delCN = j.price.delCN[-c(28,37),]
# 
# temp.SCRE_CN = ggplot(j.price.delCN, aes(x = temp.diff, y = SCRE_LG)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", colour = "black", se = F, size = 1.1) +
#   xlab("Temp difference (C)") +
#   ylab("SRE+SCE") +
#   #coord_cartesian(ylim = c(0,55)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());temp.SCRE_CN 
# 
# cv.SCRE_CN = ggplot(j.price.delCN, aes(x = pc2.diff, y = SCRE_LG)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.75,colour = "black", se = F, size = 1.1) +
#   #coord_cartesian(ylim = c(-55, 55)) +
#   xlab("CVQ difference") +
#   ylab("SRE+SCE C:N (molar)")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());cv.SCRE_CN
# 
# bom.SCRE_CN = ggplot(j.price.delCN, aes(x = bom.diff, y = SCRE_LG)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.4, colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("BOM difference g(C m-2)") +
#   ylab(expression(paste("SRE+SCE [",Delta,"C:N ratio]", sep = ""))) +
#   #coord_cartesian(ylim = c(-50,55)) +
#   #annotate('text', x = 0.022, y = 4, parse = T, label = "italic(p) == -0.14") +
#   #annotate('text', x = 0.0208, y = 2, parse = T, label = "italic(p-value) == 0.33") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());bom.SCRE_CN
# 
# #####
# SCRE.df = data.frame(j.price.delCN$cv.diff, j.price.delCN$no3.diff, j.price.delCN$SCRE_LG)
# colnames(SCRE.df) = c("cv.diff", "no3.diff", "SCRE_LG")
# SCRE.df = SCRE.df[-37,]
# SCRE.mat = as.matrix(SCRE.df$cv.diff, SCRE.df$no3.diff)
# 
# hist(SCRE.df$SCRE_LG)
# SCRE.loess = loess(SCRE_LG~cv.diff+no3.diff, data = SCRE.df, degree = 2, span = 0.40)
# plot(resid(SCRE.loess))
# length(which(resid(SCRE.loess) > 0))
# length(which(resid(SCRE.loess)<0))
# length(which(resid(SCRE.loess) == 0))
# SCRE.ss = sum(scale(SCRE.df$SCRE_LG, scale = F)^2)
# SCRE.resid = sum(resid(SCRE.loess)^2)
# 1-SCRE.resid/SCRE.ss
# 
# SCRE.loess.as = loess.as(SCRE.mat,SCRE.df$SCRE_LG, degree = 1, criterion = 'gcv', user.span = NULL);summary(SCRE.loess.as)
# SCRE.fit = expand.grid(cv.diff = seq(min(SCRE.df$cv.diff, na.rm = T),max(SCRE.df$cv.diff, na.rm = T), 0.01), no3.diff = seq(min(SCRE.df$no3.diff), max(SCRE.df$no3.diff),0.0005))
# 
# SCRE.pred = predict(SCRE.loess, newdata = SCRE.fit)
# SCRE.fit$SCRE_LG = as.numeric(SCRE.pred)
# 
# ##### 3D plot of CV-DIN and SCRE using the plot3D package ####
# scatter3D_fancy(j.price.delCN$cv.diff, j.price.delCN$no3.diff, j.price.delCN$SCRE_LG, pch = 19, cex = 2,
#                 xlab = "CV difference", ylab = "DIN difference", zlab = "Log10(SRE+SCE)", theta =40, phi = 17, ticktype = "detailed",
#                 type = "h", bty = "b2")
# dev.off()
# SCRE.wire = wireframe(SCRE_LG~cv.diff+no3.diff, data = SCRE.fit, colorkey = F, shade = F,  drape = T,
#                       zlab = list("SRE+SCE", rot = 90, cex = 2), xlab = list("CV difference", rot = 30, cex = 2),
#                       ylab = list("NO3 difference", rot = -40, cex = 2),par.settings = theme.novpadding,
#                       zoom = 0.92,scales = list(arrows = F, cex = 1.3, col = "black", font = 3), 
#                       panel = function(...){
#                         panel.wireframe(...)
#                         grid.text(expression(paste(alpha, " = 0.40")), x = unit(0.7, "npc"),y=unit(0.362, "npc"),gp = gpar(fontsize = 18))
#                         grid.text(expression(paste('R'['loess']^2, '= 0.41')),x = unit(0.7, "npc"),y=unit(0.332, "npc"),gp = gpar(fontsize = 18))
#                       });SCRE.wire
# 
# ##do some plotting of the price components CDE_ABUN#####
# j.price.delCN = ddply(j.price.CN, .(ref_site, comparison_site), transform, SCRE_LG = SCRE_LG - CN.mean, CDE_ABUN = CDE_ABUN - CN.mean,
#                       CDE_FUNC = CDE_FUNC - CN.mean, CDE_INTR = CDE_INTR - CN.mean)
# j.price.delCNmean = colMeans(j.price.delCN[,c(31,21:23)])
# j.price.delCNsd = apply(j.price.delCN[,c(31,21:23)], 2, FUN = sd)
# 
# ######
# hist(j.price.delCN$CDE_ABUN)
# 
# ####
# ggplot(j.price.delCN, aes(y = cv.diff, x = no3.diff)) + geom_point(size = 6)
# cor(j.price.delCN$cv.diff, j.price.delCN$no3.diff)
# 
# temp.pricelm = lm(log10(abs(CDE_ABUN))~temp.diff,data = j.price.CN); summary(temp.pricelm)
# cor.test(j.price.delCN$temp.diff, abs(j.price.delCN$CDE_ABUN), method = "spearman", use = "pairwise.complete.obs", na.action = "na.exclude")
# temp.CDEn_CN = ggplot(j.price.delCN_ABUN, aes(x = temp.diff, y = CDE_ABUN)) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("abs(CDEn) [C:N (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 2.8, parse = T, label = "italic(p) == -0.19") +
#   annotate('text', x = 23.1, y = 0.8, parse = T, label = "italic(p-value) == 0.16") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());temp.CDEn_CN
# 
# cv.CDEn_CN = ggplot(j.price.delCN_ABUN, aes(x = cv.diff, y = CDE_ABUN)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.75, colour = "black", se = F, size = 1.1) +
#   #coord_cartesian(ylim = c(0, 75)) +
#   xlab("CVQ difference") +
#   ylab("abs(CDEn) C:N (molar)")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());cv.CDEn_CN; 
# 
# bom.CDEn_CN = ggplot(j.price.delCN, aes(x = bom.diff, y = CDE_ABUN)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.75, colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("BOM difference (g C m-2)") +
#   ylab("CDEn C:N (molar)") +
#   coord_cartesian(ylim = c(-70,75)) +
#   #annotate('text', x = 0.022, y = 4, parse = T, label = "italic(p) == -0.24") +
#   #annotate('text', x = 0.0208, y = 2, parse = T, label = "italic(p-value) == 0.08") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());bom.CDEn_CN
# ######
# 
# CDEn.df = data.frame(j.price.delCN$cv.diff, j.price.delCN$no3.diff, j.price.delCN$CDE_ABUN)
# colnames(CDEn.df) = c("cv.diff", "no3.diff", "CDE_ABUN")
# hist(CDEn.df$CDE_ABUN)
# CDEn.mat = as.matrix(CDEn.df$cv.diff, CDEn.df$no3.diff)
# 
# CDEn.loess = loess(CDE_ABUN~cv.diff*no3.diff, data = CDEn.df, degree = 2, span = 0.75)
# plot(resid(CDEn.loess))
# length(which(resid(CDEn.loess) > 0))
# length(which(resid(CDEn.loess)<0))
# length(which(resid(CDEn.loess) == 0))
# CDEn.ss = sum(scale(CDEn.df$CDE_ABUN, scale = F)^2)
# CDEn.resid = sum(resid(CDEn.loess)^2)
# 1-CDEn.resid/CDEn.ss
# 
# CDEn.fit = expand.grid(cv.diff = seq(min(CDEn.df$cv.diff, na.rm = T),max(CDEn.df$cv.diff, na.rm = T), 0.01), no3.diff = seq(min(CDEn.df$no3.diff), max(CDEn.df$no3.diff),0.0005))
# 
# CDEn.pred = predict(CDEn.loess, newdata = CDEn.fit)
# CDEn.fit$CDE_ABUN = as.numeric(CDEn.pred)
# #####
# CDEn.wire = wireframe(CDE_ABUN~cv.diff+no3.diff, data = CDEn.fit, colorkey = F, drape = T,
#                       zlab = list("CDEn", rot = 90, cex = 2), xlab = list("CV difference", rot = 30, cex = 2),
#                       ylab = list("NO3 difference", rot = -40, cex = 2),par.settings = theme.novpadding,
#                       zoom = 0.92,scales = list(arrows = F, cex = 1.3, col = "black", font = 3),
#                       panel = function(...){
#                         panel.wireframe(...)
#                         grid.text(expression(paste(alpha, " = 0.75")), x = unit(0.36, "npc"),y=unit(0.78, "npc"),gp = gpar(fontsize = 18))
#                         grid.text(expression(paste('R'['loess']^2, '= 0.21')),x = unit(0.36, "npc"),y=unit(0.75, "npc"),gp = gpar(fontsize = 18))
#                       });CDEn.wire
# 
# ##do some plotting of the price components CDE_FUNC#####
# j.price.delCN = ddply(j.price.CN, .(ref_site, comparison_site), transform, SCRE_LG = SCRE_LG - CN.mean, CDE_ABUN = CDE_ABUN - CN.mean,
#                       CDE_FUNC = CDE_FUNC - CN.mean, CDE_INTR = CDE_INTR - CN.mean)
# 
# hist(j.price.delCN$CDE_FUNC)
# j.price.delCN = j.price.delCN[-c(37,54),]
# summary(rq(CDE_FUNC~cv.diff+no3.diff, data = j.price.delCN, 0.95))
# 
# 
# #####
# cor.test(j.price.CN$temp.diff, abs(j.price.CN$CDE_FUNC), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEp_CN = ggplot(j.price.CN[-c(37,54),], aes(x = temp.diff, y = abs(CDE_FUNC))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEp)) [C:N (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());temp.CDEp_CN
# 
# cv.CDEp_CN = ggplot(j.price.delCN, aes(x = cv.diff, y = abs(CDE_FUNC))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", colour = "black", se = F, size = 1.1) +
#   coord_cartesian(ylim = c(0, 100)) +
#   xlab("CVQ difference") +
#   ylab("CDEp (molar)")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());cv.CDEp_CN; 
# 
# #cv.price_CN = ggplot(j.price.CN, aes(x = cv.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);cv.price_CN; cor.test(j.price.CN$cv.diff, log10(abs(j.price.CN$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #cor(j.price.CN$cv.diff, log10(abs(j.price.CN$CDE_FUNC)), use = "pairwise.complete.obs")
# #slope.price_CN = ggplot(j.price.CN, aes(x = slope.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);slope.price_CN; cor.test(j.price.CN$slope.diff, log10(abs(j.price.CN$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.CN$no3.diff, log10(abs(j.price.CN$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# bom.CDEp_CN = ggplot(j.price.delCN, aes(x = bom.diff, y = CDE_FUNC)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.6,colour = "black", se = F, linetype = "solid", size = 1.1) +
#   xlab("BOM difference (g C m-2)") +
#   ylab("Function difference C:N(molar)") +
#   #coord_cartesian(ylim = c(0,100)) +
#   annotate('text', x = 5, y = 100, parse = T, label = paste(median(j.price.delCN$CDE_FUNC,na.rm = T))) +
#   #annotate('text', x = 0.003, y = 0.4, parse = T, label = "italic(p-value) == 0.05") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());bom.CDEp_CN
# ####
# CDEp.df = data.frame(j.price.delCN$cv.diff, j.price.delCN$no3.diff, j.price.delCN$CDE_FUNC)
# colnames(CDEp.df) = c("cv.diff", "no3.diff", "CDE_FUNC")
# hist(CDEp.df$CDE_FUNC)
# CDEp.df = CDEp.df[-c(37,54),]
# 
# CDEp.mat = as.matrix(CDEp.df$cv.diff, CDEp.df$no3.diff)
# CDEp.loess.as = loess.as(CDEp.mat,CDEp.df$CDE_FUNC, degree = 2, criterion = 'gcv', user.span = NULL);summary(CDEn.loess.as)
# 
# CDEp.loess = loess(CDE_FUNC~cv.diff*no3.diff, data = CDEp.df, degree = 2, span = 0.65)
# plot(resid(CDEp.loess))
# length(which(resid(CDEp.loess) > 0))
# length(which(resid(CDEp.loess)<0))
# length(which(resid(CDEp.loess) == 0))
# CDEp.ss = sum(scale(CDEp.df$CDE_FUNC, scale = F)^2)
# CDEp.resid = sum(resid(CDEp.loess)^2)
# 1-CDEp.resid/CDEp.ss
# 
# CDEp.fit = expand.grid(cv.diff = seq(min(CDEp.df$cv.diff, na.rm = T),max(CDEp.df$cv.diff, na.rm = T), 0.01), no3.diff = seq(min(CDEp.df$no3.diff), max(CDEp.df$no3.diff),0.0005))
# 
# CDEp.pred = predict(CDEp.loess, newdata = CDEp.fit)
# CDEp.fit$CDE_FUNC = as.numeric(CDEp.pred)
# #####
# 
# CDEp.wire = wireframe(CDE_FUNC~cv.diff*no3.diff, data = CDEp.fit, colorkey = F, drape = T,
#                       zlab = list("CDEp", rot = 90, cex = 2), xlab = list("CV difference", rot = 30, cex = 2),
#                       ylab = list("NO3 difference", rot = -40, cex = 2),par.settings = theme.novpadding,
#                       zoom = 0.92,scales = list(arrows = F, cex = 1.3, col = "black", font = 3),
#                       panel = function(...){
#                         panel.wireframe(...)
#                         grid.text(expression(paste(alpha, " = 0.65")), x = unit(0.7, "npc"),y=unit(0.39, "npc"),gp = gpar(fontsize = 18))
#                         grid.text(expression(paste('R'['loess']^2, '= 0.40')),x = unit(0.7, "npc"),y=unit(0.36, "npc"),gp = gpar(fontsize = 18))
#                       });CDEp.wire
# 
# ##do some plotting of the price components CDE_INT#####
# j.price.delCN = ddply(j.price.CN, .(ref_site, comparison_site), transform, SCRE_LG = SCRE_LG - CN.mean, CDE_ABUN = CDE_ABUN - CN.mean,
#                       CDE_FUNC = CDE_FUNC - CN.mean, CDE_INTR = CDE_INTR - CN.mean)
# 
# cor.test(j.price.CN$temp.diff, abs(j.price.CN$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEi_CN = ggplot(j.price.CN, aes(x = temp.diff, y = abs(CDE_INTR))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("abs(CDEi) [C:N (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 4, parse = T, label = "italic(p) == -0.18") +
#   annotate('text', x = 23.3, y = 1, parse = T, label = "italic(p-value) == 0.20") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()); temp.CDEi_CN
# 
# cv.CDEi_CN = ggplot(j.price.delCN, aes(x = cv.diff, y = abs(CDE_INTR))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.75, colour = "black", se = F, size = 1.1) +
#   coord_cartesian(ylim = c(0, 120)) +
#   xlab("CVQ difference") +
#   ylab("CDEi (molar)")+
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank());cv.CDEi_CN; 
# 
# #cv.price_CN = ggplot(j.price.CN, aes(x = cv.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); cv.price_CN; cor.test(j.price.CN$cv.diff, log10(abs(j.price.CN$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #slope.price_CN = ggplot(j.price.CN, aes(x = slope.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); slope.price_CN; cor.test(j.price.CN$slope.diff, log10(abs(j.price.CN$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.CN$no3.diff, abs(j.price.CN$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# bom.CDEi_CN = ggplot(j.price.delCN, aes(x = bom.diff, y = CDE_INTR)) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "loess", span = 0.75,colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("BOM difference (g C m-2)") +
#   ylab("CDEi C:N (molar)") +
#   #coord_cartesian(ylim = c(0,100)) +
#   annotate('text', x = 5, y = 100, parse = T, label = paste(median(j.price.delCN$CDE_INTR, na.rm = T))) +
#   #annotate('text', x = 0.0033, y =1, parse = T, label = "italic(p-value) == 0.14") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()); bom.CDEi_CN
# #####
# CDEi.df = data.frame(j.price.delCN$cv.diff, j.price.delCN$no3.diff, j.price.delCN$CDE_INTR)
# colnames(CDEi.df) = c("cv.diff", "no3.diff", "CDE_INTR")
# hist(CDEi.df$CDE_INTR)
# CDEi.df = CDEi.df[-24,]
# CDEi.mat = as.matrix(CDEi.df$cv.diff, CDEi.df$no3.diff)
# CDEi.loess.as = loess.as(CDEi.mat,CDEi.df$CDE_INTR, degree = 2, criterion = 'gcv', user.span = NULL)
# summary(CDEi.loess.as)
# 
# CDEi.loess = loess(CDE_INTR~cv.diff*no3.diff, data = CDEi.df, degree = 2, span = 0.75)
# plot(resid(CDEn.loess))
# length(which(resid(CDEn.loess) > 0))
# length(which(resid(CDEn.loess)<0))
# length(which(resid(CDEn.loess) == 0))
# CDEi.ss = sum(scale(CDEi.df$CDE_INTR, scale = F)^2)
# CDEi.resid = sum(resid(CDEi.loess)^2)
# 1-CDEi.resid/CDEi.ss
# 
# CDEi.fit = expand.grid(cv.diff = seq(min(CDEi.df$cv.diff, na.rm = T),max(CDEi.df$cv.diff, na.rm = T), 0.01), no3.diff = seq(min(CDEi.df$no3.diff), max(CDEi.df$no3.diff),0.0005))
# 
# CDEi.pred = predict(CDEi.loess, newdata = CDEi.fit)
# CDEi.fit$CDE_INTR = as.numeric(CDEi.pred)
# #####
# CDEi.wire = wireframe(CDE_INTR~cv.diff*no3.diff, data = CDEi.fit, colorkey = F, drape = T,
#                       zlab = list("CDEi", rot = 90, cex = 2), xlab = list("CV difference", rot = 30, cex = 2),
#                       ylab = list("NO3 difference", rot = -40, cex = 2), par.settings = theme.novpadding,
#                       zoom = 0.92, scales = list(arrows = F, cex = 1.3, col = "black", font = 3),
#                       panel = function(...){
#                         panel.wireframe(...)
#                         grid.text(expression(paste(alpha, " = 0.75")), x = unit(0.7, "npc"),y=unit(0.62, "npc"),gp = gpar(fontsize = 18))
#                         grid.text(expression(paste('R'['loess']^2, '= 0.33')),x = unit(0.7, "npc"),y=unit(0.59, "npc"),gp = gpar(fontsize = 18))
#                       });CDEi.wire
# 
# ######
# 
# png("PriceCN_CV-DIN.png", res = 600, height = 20, width = 20, units = "in")
# grid.arrange(SCRE.wire, CDEn.wire, CDEp.wire, CDEi.wire, ncol = 2)
# dev.off()
# 
# ######do some plotting of the price components SRCE_LG#####
# j.price.CP[j.price.CP == 0] = NA
# hist(abs(j.price.CP$SCRE_LG))
# hist(log10(abs(j.price.CP$SCRE_LG)))
# shapiro.test(log10(abs(j.price.CP$SCRE_LG)))
# shapiro.test(j.price.CP$SCRE_LG)
# ###
# cor.test(j.price.CP$temp.diff, abs(j.price.CP$SCRE_LG), method = "spearman", exact = F, na.action = 'na.exclude')
# #cor.test(j.price.CP$temp.diff, log10(abs(j.price.CP$SCRE_LG/1000)), method = "pearson", na.action = 'na.exclude')
# 
# temp_SCRE.lm = lm(log10(abs(SCRE_LG/1000))~temp.diff, data = j.price.CP);summary(temp_SCRE.lm)
# #temp_SCRE.lm = lm(log10(abs(SCRE_LG/1000))~temp.diff:poly(temp.diff, 3), data = j.price.CP);summary(temp_SCRE.lm)
# temp.SCRE_CP = ggplot(j.price.CP, aes(x = temp.diff, y = log10(abs(SCRE_LG)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", linetype = "dashed", se = F, size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(SRE+SCE)) [C:P (molar)]") +
#   annotate('text', x = 25, y = 3, parse = T, label = "italic(p) == 0.06") +
#   annotate('text', x = 23, y = 2.9, parse = T, label = "italic(p-value) == 0.69") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.SCRE_CP 
# 
# #cv.price_CP = ggplot(j.price.CP, aes(x = cv.diff, y = log10(abs(SCRE_LG)))) + geom_point(size = 6) +
# #  scale_y_continuous(limits = c(5,10.5));cv.price_CP; cor.test(j.price.CP$cv.diff, log10(abs(j.price.CP$SCRE_LG)), method = "spearman", exact = F, na.action = "na.exlude")
# #slope.price_CP = ggplot(j.price.CP, aes(x = slope.diff, y = log10(abs(SCRE_LG)))) + geom_point(size = 6);slope.price_CP; cor.test(j.price.CP$slope.diff, log10(abs(j.price.CP$SCRE_LG)), method = "spearman", exact = F, na.action = "na.exlude")
# cor.test(j.price.CP$no3.diff, abs(j.price.CP$SCRE_LG), method = "spearman", exact = F, na.action = "na.exlude")
# no3.SCRE_CP = ggplot(j.price.CP, aes(x = no3.diff, y = log10(abs(SCRE_LG)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("DIN difference (Nh4+NO3 mg/L)") +
#   ylab("abs(SRE+SCE) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 3, parse = T, label = "italic(p) == -0.01") +
#   annotate('text', x = 0.0208, y = 2.95, parse = T, label = "italic(p-value) == 0.94") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.SCRE_CP; 
# 
# cor.test(j.price.CP$srp.diff, abs(j.price.CP$SCRE_LG), method = "spearman", exact = F, na.action = "na.exlude")
# srp.SCRE_CP = ggplot(j.price.CP, aes(x = srp.diff, y = log10(abs(SCRE_LG)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(SRE+SCE)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.023, y = 3, parse = T, label = "italic(p) == -0.007") +
#   annotate('text', x = 0.0218, y = 2.95, parse = T, label = "italic(p-value) == 0.96") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.SCRE_CP
# 
# cor.test(j.price.CP$pc1.diff, abs(j.price.CP$SCRE_LG), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# pc1.SCRE_CP = ggplot(j.price.CP, aes(x = pc1.diff, y = log10(abs(SCRE_LG)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC1 difference") +
#   ylab("Log10(abs(SRE+SCE)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.8, y = 3, parse = T, label = "italic(p) == 0.17") +
#   annotate('text', x = 5.39, y = 2.95, parse = T, label = "italic(p-value) == 0.30") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.SCRE_CP
# 
# cor.test(j.price.CP$pc2.diff, abs(j.price.CP$SCRE_LG), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# pc2.SCRE_CP = ggplot(j.price.CP, aes(x = pc2.diff, y = log10(abs(SCRE_LG)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(SRE+SCE)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.11, y = 3, parse = T, label = "italic(p) == 0.07") +
#   annotate('text', x = 2.85, y = 2.95, parse = T, label = "italic(p-value) == 0.67") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.SCRE_CP
# 
# cor.test(j.price.CP$np.diff, abs(j.price.CP$SCRE_LG), method = "spearman", exact = F, use = "pairwise.complete.obs")
# ##do some plotting of the price components CDE_ABUN#####
# temp.pricelm = lm(log10(abs(CDE_ABUN))~temp.diff,data = j.price.CP); summary(temp.pricelm)
# cor.test(j.price.CP$temp.diff, abs(j.price.CP$CDE_ABUN), method = "spearman", use = "pairwise.complete.obs", na.action = "na.exclude")
# temp.CDEn_CP = ggplot(j.price.CP, aes(x = temp.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEn)) [C:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 0.15, parse = T, label = "italic(p) == -0.18") +
#   annotate('text', x = 23.1, y =0, parse = T, label = "italic(p-value) == 0.18") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEn_CP
# 
# #cv.price_CP = ggplot(j.price.CP, aes(x = cv.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6) + scale_y_continuous(limits = c(6,10.5)) +
# #  stat_smooth(method ="lm");cv.price_CP; cor.test(j.price.CP$cv.diff, log10(abs(j.price.CP$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# #slope.price_CP = ggplot(j.price.CP, aes(x = slope.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6);slope.price_CP; cor.test(j.price.CP$slope.diff, log10(abs(j.price.CP$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# cor.test(j.price.CP$no3.diff, abs(j.price.CP$CDE_ABUN), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# no3.CDEn_CP = ggplot(j.price.CP, aes(x = no3.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 0.15, parse = T, label = "italic(p) == 0.02") +
#   annotate('text', x = 0.0208, y = 0, parse = T, label = "italic(p-value) == 0.90") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEn_CP
# 
# cor.test(j.price.CP$srp.diff, abs(j.price.CP$CDE_ABUN), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# srp.CDEn_CP = ggplot(j.price.CP, aes(x = srp.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0231, y = 0.15, parse = T, label = "italic(p) == 0.16") +
#   annotate('text', x = 0.0215, y = 0, parse = T, label = "italic(p-value) == 0.24") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.CDEn_CP
# 
# cor.test(j.price.CP$pc1.diff, abs(j.price.CP$CDE_ABUN), method = 'spearman', exact = F, use = "pairwise.complete.obs")
# pc1.CDEn_CP = ggplot(j.price.CP, aes(x = pc1.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) +  
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(6.9,10.5)) +
#   annotate('text', x = 5.8, y = 0.15, parse = T, label = "italic(p) == -0.05") +
#   annotate('text', x = 5.55, y = 0, parse = T, label = "italic(p-value) == 0.74") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.CDEn_CP
# 
# cor.test(j.price.CP$pc2.diff,abs(j.price.CP$CDE_ABUN), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc2.CDEn_CP = ggplot(j.price.CP, aes(x = pc2.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(6.9,10.5)) +
#   annotate('text', x = 3.11, y = 0.15, parse = T, label = "italic(p) == -0.16") +
#   annotate('text', x = 2.94, y = 0, parse = T, label = "italic(p-value) == 0.31") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.CDEn_CP;
# 
# cor.test(j.price.CP$np.diff, log10(abs(j.price.CP$CDE_ABUN)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_FUNC#####
# cor.test(j.price.CP$temp.diff, abs(j.price.CP$CDE_FUNC), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEp_CP = ggplot(j.price.CP, aes(x = temp.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEp)) [C:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 0.5, parse = T, label = "italic(p) == -0.21") +
#   annotate('text', x = 23.2, y = 0.4, parse = T, label = "italic(p-value) == 0.13") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEp_CP
# 
# #cv.price_CP = ggplot(j.price.CP, aes(x = cv.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);cv.price_CP; cor.test(j.price.CP$cv.diff, log10(abs(j.price.CP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #cor(j.price.CP$cv.diff, log10(abs(j.price.CP$CDE_FUNC)), use = "pairwise.complete.obs")
# #slope.price_CP = ggplot(j.price.CP, aes(x = slope.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);slope.price_CP; cor.test(j.price.CP$slope.diff, log10(abs(j.price.CP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# summary(lm(no3.diff~log10(abs(CDE_FUNC)), data = j.price.CP))
# cor.test(j.price.CP$no3.diff, log10(abs(j.price.CP$CDE_FUNC)), method = "pearson", exact = F, use = "pairwise.complete.obs")
# no3.CDEp_CP = ggplot(j.price.CP, aes(x = no3.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "glm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0043, y = 0.5, parse = T, label = "italic(p) == -0.15") +
#   annotate('text', x = 0.003, y = 0.4, parse = T, label = "italic(p-value) == 0.28") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEp_CP
# 
# cor.test(j.price.CP$srp.diff, log10(abs(j.price.CP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEp_CP = ggplot(j.price.CP, aes(x = srp.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0032, y = 0.5, parse = T, label = "italic(p) == -0.11") +
#   annotate('text', x = 0.0023, y = 0.4, parse = T, label = "italic(p-value) == 0.42") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.CDEp_CP
# 
# cor.test(j.price.CP$pc1.diff, log10(abs(j.price.CP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc1.CDEp_CP = ggplot(j.price.CP, aes(x = pc1.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.8, y = 0.5, parse = T, label = "italic(p) == -0.16") +
#   annotate('text', x = 5.5, y = 0.4, parse = T, label = "italic(p-value) == 0.29") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.CDEp_CP
# 
# summary(lm(pc2.diff~log10(abs(CDE_FUNC)), data = j.price.CP))
# cor.test(j.price.CP$pc2.diff, log10(abs(j.price.CP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc2.CDEp_CP = ggplot(j.price.CP, aes(x = pc2.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.13, y = 0.5, parse = T, label = "italic(p) == -0.47") +
#   annotate('text', x = 2.94, y = 0.4, parse = T, label = "italic(p-value) == 0.05") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.CDEp_CP
# 
# cor.test(j.price.CP$np.diff, log10(abs(j.price.CP$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_INT#####
# cor.test(j.price.CP$temp.diff, abs(j.price.CP$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEi_CP = ggplot(j.price.CP, aes(x = temp.diff, y = log10(abs(CDE_INTR)))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEi)) [C:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 1, parse = T, label = "italic(p) == -0.18") +
#   annotate('text', x = 23.3, y = 0.95, parse = T, label = "italic(p-value) == 0.20") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()); temp.CDEi_CP
# #cv.price_CP = ggplot(j.price.CP, aes(x = cv.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); cv.price_CP; cor.test(j.price.CP$cv.diff, log10(abs(j.price.CP$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #slope.price_CP = ggplot(j.price.CP, aes(x = slope.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); slope.price_CP; cor.test(j.price.CP$slope.diff, log10(abs(j.price.CP$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.CP$no3.diff, abs(j.price.CP$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEi_CP = ggplot(j.price.CP, aes(x = no3.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("abs(CDEi) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0215, y = 2.3 , parse = T, label = "italic(p) == -0.20") +
#   annotate('text', x = 0.020, y = 2.25, parse = T, label = "italic(p-value) == 0.16") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); no3.CDEi_CP
# 
# cor.test(j.price.CP$srp.diff, abs(j.price.CP$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEi_CP = ggplot(j.price.CP, aes(x = srp.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEi)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0231, y = 2.3, parse = T, label = "italic(p) == -0.02") +
#   annotate('text', x = 0.0218, y = 2.25, parse = T, label = "italic(p-value) == 0.89") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); srp.CDEi_CP
# 
# cor.test(j.price.CP$pc1.diff, abs(j.price.CP$CDE_INTR), method = "spearman", use = "pairwise.complete.obs")
# pc1.CDEi_CP = ggplot(j.price.CP, aes(x = pc1.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC1 difference") +
#   ylab("Log10(abs(CDEi)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.81, y = 2.3, parse = T, label = "italic(p) == -0.08") +
#   annotate('text', x = 5.4, y = 2.25, parse = T, label = "italic(p-value) == 0.59") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc1.CDEi_CP
# 
# summary(lm(pc2.diff~log10(abs(CDE_INTR)), data = j.price.CP))
# cor.test(j.price.CP$pc2.diff, abs(j.price.CP$CDE_INTR), method = "pearson", use = "pairwise.complete.obs")
# pc2.CDEi_CP = ggplot(j.price.CP, aes(x = pc2.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(CDEi)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.16, y = 2.3, parse = T, label = "italic(p) == -0.31") +
#   annotate('text', x = 3, y = 2.25, parse = T, label = "italic(p-value) == 0.05") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc2.CDEi_CP
# 
# png("PriceCP_env.png", res = 300, height = 20, width = 20, units = "in")
# grid.arrange(temp.SCRE_CP, no3.SCRE_CP, srp.SCRE_CP, pc1.SCRE_CP, pc2.SCRE_CP,
#              temp.CDEn_CP, no3.CDEn_CP, srp.CDEn_CP, pc1.CDEn_CP, pc2.CDEn_CP,
#              temp.CDEp_CP, no3.CDEp_CP, srp.CDEp_CP, pc1.CDEp_CP, pc2.CDEp_CP,
#              temp.CDEi_CP, no3.CDEi_CP, srp.CDEi_CP, pc1.CDEi_CP, pc2.CDEi_CP, ncol = 5, nrow = 4)
# dev.off()
# #####
# 
# # use SCRE_LG, CDE_ABUN, CDE_FUNC, & CDE_INTR
# ggplot(j.price.delNP[which(j.price.delNP$SCRE_LG < 100),], aes(y = SCRE_LG, x = bom.diff/1000)) + geom_point(size = 6)
# ggplot(j.price.delNP[which(j.price.delNP$CDE_ABUN > -100),], aes(y = CDE_ABUN, x = bom.diff/1000))+ geom_point(size = 6)
# ggplot(j.price.delNP, aes(y = CDE_FUNC, x = bom.diff/1000)) + geom_point(size = 6)
# ggplot(j.price.delNP[which(j.price.delNP$CDE_INTR > -100),], aes(y = CDE_INTR, x = bom.diff/1000)) + geom_point(size =6)
# 
# 
# 
# cor.test(j.price.NP$temp.diff, abs(j.price.NP$SCRE_LG), method = "spearman", exact = F, na.action = 'na.exclude')
# #cor.test(j.price.NP$temp.diff, log10(abs(j.price.NP$SCRE_LG/1000)), method = "pearson", na.action = 'na.exclude')
# 
# temp_SCRE.lm = lm(log10(abs(SCRE_LG/1000))~temp.diff, data = j.price.NP);summary(temp_SCRE.lm)
# #temp_SCRE.lm = lm(log10(abs(SCRE_LG/1000))~temp.diff:poly(temp.diff, 3), data = j.price.NP);summary(temp_SCRE.lm)
# temp.SCRE_NP = ggplot(j.price.NP, aes(x = temp.diff, y = abs(SCRE_LG))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", linetype = "dashed", se = F, size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("abs(SRE+SCE) [N:P (molar)]") +
#   annotate('text', x = 25, y = 25, parse = T, label = "italic(p) == -0.12") +
#   annotate('text', x = 23.2, y = 24, parse = T, label = "italic(p-value) == 0.43") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.SCRE_NP 
# 
# #cv.price_NP = ggplot(j.price.NP, aes(x = cv.diff, y = log10(abs(SCRE_LG)))) + geom_point(size = 6) +
# #  scale_y_continuous(limits = c(5,10.5));cv.price_NP; cor.test(j.price.NP$cv.diff, log10(abs(j.price.NP$SCRE_LG)), method = "spearman", exact = F, na.action = "na.exlude")
# #slope.price_NP = ggplot(j.price.NP, aes(x = slope.diff, y = log10(abs(SCRE_LG)))) + geom_point(size = 6);slope.price_NP; cor.test(j.price.NP$slope.diff, log10(abs(j.price.NP$SCRE_LG)), method = "spearman", exact = F, na.action = "na.exlude")
# cor.test(j.price.NP$no3.diff, abs(j.price.NP$SCRE_LG), method = "spearman", exact = F, na.action = "na.exlude")
# no3.SCRE_NP = ggplot(j.price.NP, aes(x = no3.diff, y = abs(SCRE_LG))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("DIN difference (Nh4+NO3 mg/L)") +
#   ylab("abs(SRE+SCE) [N:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 25, parse = T, label = "italic(p) == 0.16") +
#   annotate('text', x = 0.0205, y = 24, parse = T, label = "italic(p-value) == 0.28") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.SCRE_NP; 
# 
# cor.test(j.price.NP$srp.diff, abs(j.price.NP$SCRE_LG), method = "spearman", exact = F, na.action = "na.exlude")
# srp.SCRE_NP = ggplot(j.price.NP, aes(x = srp.diff, y = abs(SCRE_LG))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("abs(SRE+SCE) [N:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.023, y = 25, parse = T, label = "italic(p) == 0.04") +
#   annotate('text', x = 0.0214, y = 24, parse = T, label = "italic(p-value) == 0.81") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.SCRE_NP
# 
# cor.test(j.price.NP$pc1.diff, abs(j.price.NP$SCRE_LG), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# pc1.SCRE_NP = ggplot(j.price.NP, aes(x = pc1.diff, y = abs(SCRE_LG))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC1 difference") +
#   ylab("Log10(abs(SRE+SCE)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.8, y = 25, parse = T, label = "italic(p) == 0.24") +
#   annotate('text', x = 5.39, y = 24, parse = T, label = "italic(p-value) == 0.13") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.SCRE_NP
# 
# cor.test(j.price.NP$pc2.diff, abs(j.price.NP$SCRE_LG), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# pc2.SCRE_NP = ggplot(j.price.NP, aes(x = pc2.diff, y = abs(SCRE_LG))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(SRE+SCE)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.11, y = 25, parse = T, label = "italic(p) == -0.10") +
#   annotate('text', x = 2.85, y = 24, parse = T, label = "italic(p-value) == 0.55") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.SCRE_NP
# 
# cor.test(j.price.NP$np.diff, abs(j.price.NP$SCRE_LG), method = "spearman", exact = F, use = "pairwise.complete.obs")
# ##do some plotting of the price components CDE_ABUN#####
# temp.pricelm = lm(log10(abs(CDE_ABUN))~temp.diff,data = j.price.NP); summary(temp.pricelm)
# cor.test(j.price.NP$temp.diff, log10(abs(j.price.NP$CDE_ABUN)), method = "spearman", use = "pairwise.complete.obs", na.action = "na.exclude")
# temp.CDEn_NP = ggplot(j.price.NP, aes(x = temp.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEn)) [N:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 3, parse = T, label = "italic(p) == -0.14") +
#   annotate('text', x = 23.1, y = 2.8, parse = T, label = "italic(p-value) == 0.31") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEn_NP
# 
# #cv.price_NP = ggplot(j.price.NP, aes(x = cv.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6) + scale_y_continuous(limits = c(6,10.5)) +
# #  stat_smooth(method ="lm");cv.price_NP; cor.test(j.price.NP$cv.diff, log10(abs(j.price.NP$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# #slope.price_NP = ggplot(j.price.NP, aes(x = slope.diff, y = log10(abs(CDE_ABUN)))) + geom_point(size = 6);slope.price_NP; cor.test(j.price.NP$slope.diff, log10(abs(j.price.NP$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# cor.test(j.price.NP$no3.diff, log10(abs(j.price.NP$CDE_ABUN)), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# no3.CDEn_NP = ggplot(j.price.NP, aes(x = no3.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 3, parse = T, label = "italic(p) == 0.29") +
#   annotate('text', x = 0.0208, y = 2.8, parse = T, label = "italic(p-value) == 0.03") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEn_NP
# 
# cor.test(j.price.NP$srp.diff, abs(j.price.NP$CDE_ABUN), method = "spearman", exact = F,  use = "pairwise.complete.obs")
# srp.CDEn_NP = ggplot(j.price.NP, aes(x = srp.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) + 
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0231, y = 3, parse = T, label = "italic(p) == 0.04") +
#   annotate('text', x = 0.0215, y = 2.8, parse = T, label = "italic(p-value) == 0.77") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.CDEn_NP
# 
# cor.test(j.price.NP$pc1.diff, abs(j.price.NP$CDE_ABUN), method = 'spearman', exact = F, use = "pairwise.complete.obs")
# pc1.CDEn_NP = ggplot(j.price.NP, aes(x = pc1.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) +  
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(6.9,10.5)) +
#   annotate('text', x = 5.8, y = 3, parse = T, label = "italic(p) == -0.13") +
#   annotate('text', x = 5.55, y = 2.8, parse = T, label = "italic(p-value) == 0.42") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.CDEn_NP
# 
# cor.test(j.price.NP$pc2.diff,abs(j.price.NP$CDE_ABUN), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc2.CDEn_NP = ggplot(j.price.NP, aes(x = pc2.diff, y = log10(abs(CDE_ABUN)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed",  size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEn)) [g C m-2]") +
#   #coord_cartesian(ylim = c(6.9,10.5)) +
#   annotate('text', x = 3.11, y = 3, parse = T, label = "italic(p) == -0.1") +
#   annotate('text', x = 2.94, y = 2.8, parse = T, label = "italic(p-value) == 0.53") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.CDEn_NP;
# 
# cor.test(j.price.NP$np.diff, log10(abs(j.price.NP$CDE_ABUN/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_FUNC#####
# cor.test(j.price.NP$temp.diff, log10(abs(j.price.NP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEp_NP = ggplot(j.price.NP, aes(x = temp.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEp)) [N:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 2.1, parse = T, label = "italic(p) == -0.09") +
#   annotate('text', x = 23.2, y = 2, parse = T, label = "italic(p-value) == 0.52") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_blank(), axis.title.x = element_blank());temp.CDEp_NP
# 
# #cv.price_NP = ggplot(j.price.NP, aes(x = cv.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);cv.price_NP; cor.test(j.price.NP$cv.diff, log10(abs(j.price.NP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #cor(j.price.NP$cv.diff, log10(abs(j.price.NP$CDE_FUNC)), use = "pairwise.complete.obs")
# #slope.price_NP = ggplot(j.price.NP, aes(x = slope.diff, y = log10(abs(CDE_FUNC)))) + geom_point(size = 6);slope.price_NP; cor.test(j.price.NP$slope.diff, log10(abs(j.price.NP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.NP$no3.diff, abs(j.price.NP$CDE_FUNC), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEp_NP = ggplot(j.price.NP, aes(x = no3.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0043, y = 2.1, parse = T, label = "italic(p) == -0.12") +
#   annotate('text', x = 0.003, y = 2, parse = T, label = "italic(p-value) == 0.39") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());no3.CDEp_NP
# 
# cor.test(j.price.NP$srp.diff, log10(abs(j.price.NP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEp_NP = ggplot(j.price.NP, aes(x = srp.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.022, y = 2.1, parse = T, label = "italic(p) == -0.27") +
#   annotate('text', x = 0.0209, y = 2, parse = T, label = "italic(p-value) == 0.05") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());srp.CDEp_NP
# 
# cor.test(j.price.NP$pc1.diff, log10(abs(j.price.NP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc1.CDEp_NP = ggplot(j.price.NP, aes(x = pc1.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.8, y = 2.1, parse = T, label = "italic(p) == -0.18") +
#   annotate('text', x = 5.5, y = 2, parse = T, label = "italic(p-value) == 0.23") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc1.CDEp_NP
# 
# cor.test(j.price.NP$pc2.diff, log10(abs(j.price.NP$CDE_FUNC)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# pc2.CDEp_NP = ggplot(j.price.NP, aes(x = pc2.diff, y = log10(abs(CDE_FUNC)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = T, linetype = "solid", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("Log10(abs(CDEp)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.13, y = 2.1, parse = T, label = "italic(p) == -0.32") +
#   annotate('text', x = 2.94, y = 2, parse = T, label = "italic(p-value) == 0.03") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title = element_blank(), axis.text = element_blank());pc2.CDEp_NP
# 
# cor.test(j.price.NP$np.diff, log10(abs(j.price.NP$CDE_FUNC/1000)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# 
# ##do some plotting of the price components CDE_INT#####
# cor.test(j.price.NP$temp.diff, abs(j.price.NP$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# temp.CDEi_NP = ggplot(j.price.NP, aes(x = temp.diff, y = log10(abs(CDE_INTR)))) +
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("Temperature difference (C)") +
#   ylab("Log10(abs(CDEi)) [N:P (molar)]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 25, y = 1.3, parse = T, label = "italic(p) == -0.18") +
#   annotate('text', x = 23.3, y = 1.25, parse = T, label = "italic(p-value) == 0.20") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()); temp.CDEi_NP
# #cv.price_NP = ggplot(j.price.NP, aes(x = cv.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); cv.price_NP; cor.test(j.price.NP$cv.diff, log10(abs(j.price.NP$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# #slope.price_NP = ggplot(j.price.NP, aes(x = slope.diff, y = log10(abs(CDE_INTR)))) + geom_point(size = 6); slope.price_NP; cor.test(j.price.NP$slope.diff, log10(abs(j.price.NP$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# cor.test(j.price.NP$no3.diff, abs(j.price.NP$CDE_INTR), method = "spearman", exact = F, use = "pairwise.complete.obs")
# no3.CDEi_NP = ggplot(j.price.NP, aes(x = no3.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("DIN difference (NH4+NO3 mg/L)") +
#   ylab("abs(CDEi) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0033, y = 1.3 , parse = T, label = "italic(p) == -0.04") +
#   annotate('text', x = 0.0022, y = 1.25, parse = T, label = "italic(p-value) == 0.76") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); no3.CDEi_NP
# 
# cor.test(j.price.NP$srp.diff, log10(abs(j.price.NP$CDE_INTR)), method = "spearman", exact = F, use = "pairwise.complete.obs")
# srp.CDEi_NP = ggplot(j.price.NP, aes(x = srp.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("SRP difference (SRP mg/L)") +
#   ylab("Log10(abs(CDEi)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 0.0231, y = 1.3, parse = T, label = "italic(p) == -0.03") +
#   annotate('text', x = 0.0218, y = 1.25, parse = T, label = "italic(p-value) == 0.84") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.y = element_blank(), axis.title.y = element_blank()); srp.CDEi_NP
# 
# cor.test(j.price.NP$pc1.diff, log10(abs(j.price.NP$CDE_INTR)), method = "spearman", use = "pairwise.complete.obs")
# pc1.CDEi_NP = ggplot(j.price.NP, aes(x = pc1.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC1 difference") +
#   ylab("Log10(abs(CDEi)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 5.81, y = 1.3, parse = T, label = "italic(p) == -0.16") +
#   annotate('text', x = 5.4, y = 1.25, parse = T, label = "italic(p-value) == 0.29") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc1.CDEi_NP
# 
# cor.test(j.price.NP$pc2.diff, abs(j.price.NP$CDE_INTR), method = "spearman", use = "pairwise.complete.obs")
# pc2.CDEi_NP = ggplot(j.price.NP, aes(x = pc2.diff, y = log10(abs(CDE_INTR)))) + 
#   geom_point(size = 6) +
#   scale_y_continuous() +
#   geom_smooth(method = "lm", colour = "black", se = F, linetype = "dashed", size = 1.1) +
#   xlab("PC2 difference") +
#   ylab("Log10(abs(CDEi)) [g C m-2]") +
#   #coord_cartesian(ylim = c(4,10.5)) +
#   annotate('text', x = 3.16, y = 1.3, parse = T, label = "italic(p) == -0.18") +
#   annotate('text', x = 3, y = 1.25, parse = T, label = "italic(p-value) == 0.24") +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.title.y = element_blank(), axis.text.y = element_blank()); pc2.CDEi_NP
# 
# png("PriceNP_env.png", res = 300, height = 20, width = 20, units = "in")
# grid.arrange(temp.SCRE_NP, no3.SCRE_NP, srp.SCRE_NP, pc1.SCRE_NP, pc2.SCRE_NP,
#              temp.CDEn_NP, no3.CDEn_NP, srp.CDEn_NP, pc1.CDEn_NP, pc2.CDEn_NP,
#              temp.CDEp_NP, no3.CDEp_NP, srp.CDEp_NP, pc1.CDEp_NP, pc2.CDEp_NP,
#              temp.CDEi_NP, no3.CDEi_NP, srp.CDEi_NP, pc1.CDEi_NP, pc2.CDEi_NP, ncol = 5, nrow = 4)
# dev.off()
# 
# ###### All price component averages #####
# price_summary_full = Reduce(function(x,y) merge(x,y), list( term_CN_summary, term_CP_summary, term_NP_summary))
# price_summary_full = price_summary_full[,c(1,3,6,9)]
# 
# price_summary_full$mean = rowMeans(price_summary_full[,2:4], na.rm = T)
# price_summary_full$sd = apply(price_summary_full[,2:4], 1, function(x) { sd(x, na.rm = T)})
# #########
