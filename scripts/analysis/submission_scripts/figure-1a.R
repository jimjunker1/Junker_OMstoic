fig1 <- function() {
  par(mar = c(0.1,0.1,0.1,0.1))
                #"Font",    "Jung",     "Fila",   "Nos",   "Biofilm",  "CBOM",     "FBOM",    "Other"
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#FFFFFF")
  xpos = c(1,3,4.8,6.48,8.15,9.65,11.08,12.45,13.6,14.4,14.9)
  ############ Figures for community composition #########
  ypos = rep(1,11)
  
  radii = (unique(log10(j.comm_rel$C.sum/1000)/log10(max(j.comm_rel$C.sum/1000))))
  sectors =j.comm_rel %>%
    spread(SPP.rank, C.rel) %>%
    ungroup() %>%
    dplyr::select(Font:Other) %>%
    apply(.,1, as.list)
  xpos2 = c(1,3,4.8,6.48,8.15,9.65,11.08,12.45,13.6,14.35,15.0)
  debugonce(pie_bubbles);debugonce(floating.pie)
  pie_bubbles(xpos, ypos,radii, sectors)
  #segments(x0 = -0.1, y0 = 1, x1 = -0.1, y1 = 1.5169602, lwd = 2, lty = 1)
  #segments(x0 = -0.1, y0 = 1.5169602, x1 = -0.2, y1 = 1.5169602, lwd = 1.5, lty = 1)
  #segments(x0 = -0.1, y0 = 1, x1 = -0.2, y1 = 1, lwd = 1.5, lty = 1)
  #segments(x0 = -0.1, y0 = 1.1723201, x1 = -0.2, y1 = 1.1723201, lwd = 1.5, lty = 1)
  #segments(x0 = -0.1, y0 = 1.3446401, x1 = -0.2, y1 = 1.3446401, lwd = 1.5, lty = 1)
  #text(x = -0.4, y = 1, labels = "0")
  #text(x = -0.4, y = 1.1723201, labels = "1")
  #text(x = -0.4, y = 1.3446401, labels = "2")
  #text(x = -0.4, y = 1.5169602, labels = "3")
  points(x = 0.5, y = 1.7, pch = 22, bg = cbbPalette[1], cex = 2)
  text(x = 0.75, y = 1.7, labels = "FONT", adj = 0, cex = 1.2)
  points(x = 2.2, y = 1.7, pch = 22, bg = cbbPalette[2], cex = 2)
  text(x = 2.45, y = 1.7, labels = "JUNG", adj = 0, cex = 1.2)
  points(x = 3.9, y = 1.7, pch = 22, bg = cbbPalette[3], cex = 2)
  text(x = 4.15, y = 1.7, labels = "FILA", adj = 0, cex = 1.2)
  points(x = 5.3, y = 1.7, pch = 22, bg = cbbPalette[4], cex = 2)
  text(x = 5.55, y = 1.7, labels = "NOS", adj = 0)
  points(x = 6.6, y = 1.7, pch = 22, bg = cbbPalette[5], cex = 2)
  text(x = 6.85, y = 1.7, labels = "BIOFILM", adj = 0)
  points(x = 8.5, y = 1.7, pch = 22, bg = cbbPalette[6], cex = 2)
  text(x = 8.75, y = 1.7, labels = "CBOM", adj = 0)
  points(x = 10.1, y = 1.7, pch = 22, bg = cbbPalette[7], cex = 2)
  text(x = 10.35, y = 1.7, labels = "FBOM", adj = 0)
  points(x = 11.7, y = 1.7, pch = 22, bg = cbbPalette[8], cex = 2)
  text(x = 11.95, y = 1.7, labels = "OTHER", adj = 0)
  text(x = 14.5, y = 1.7, labels = "A", adj = 0, cex = 2.5)
  text(x = xpos2, y = 0.25, labels = format(round(sort(j.summary_stoic$C.mean/1000, decreasing = T),1),nsmall = 1), adj = 0.5, font = 2) 
}