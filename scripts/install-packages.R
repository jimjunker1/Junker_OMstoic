#install packages and functions
#!diagnostics off
##Run this before anything else
# load_packages = function(){
if(!require("pacman")) install.packages("pacman")
library(pacman)
package.list <- c("data.table", "RCurl","plyr","tidyverse","furrr","vegan", "AICcmodavg",
                  "grid","gridExtra", "tictoc","chron","gtable","ggfortify","MuMIn",
                  "sjPlot","viridis", "broom","broom.mixed", "stargazer")
p_load(char = package.list, install = T)
rm("package.list")
se<<-function(x){
  sd(x,na.rm=T)/sqrt(length(!is.na(x)))
}
ci<<-function(x){
  se(x)*1.96
}
# preddat_fun function from https://aosmith.rbind.io/2018/01/31/added-variable-plots/
preddat_fun = function(data, allvars, var) {
  sums = summarise_at(data, 
                      vars( one_of(allvars), -one_of(var) ), 
                      median) 
  cbind( select_at(data, var), sums)
}
pred_plot = function(data, variable, xlab) {
  ggplot(data, aes_string(x = variable, y = "pred") ) +
    geom_line(size = 1) + geom_point(size = 3)+
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25) +
    theme_bw() +
    labs(x = xlab,
         y = expression("Benthic organic matter (g C m"^2*")")) +
    theme(panel.grid = element_blank())
}

colors <- c("#120B02", "#816B58", "#3E90AD", "#A7D05D", "#CFD05D", "#C44F4F")
blackwhite <- c("#000000", "#999999")

#ggplotRegression <- function (fit) {

#for (i in 1:length(fit)) {
#dev.new()
#ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
#  geom_point() +
#  stat_smooth(method = "lm", color = "red")
##}

theme_mod <<- theme_bw() %+replace% theme(panel.grid = element_blank())
theme_black <<- function() {theme_bw() %+replace% theme(panel.background = element_rect(fill = 'transparent', colour = NA),panel.grid = element_blank(), axis.ticks = element_line(color = 'white'),
                                                        axis.title = element_text(color = 'white'), axis.text = element_text(color = 'white'), plot.background =element_rect(fill = 'transparent', colour = NA),
                                                        panel.border = element_rect(fill = NA, colour = 'white'))}
# }
# load_packages()