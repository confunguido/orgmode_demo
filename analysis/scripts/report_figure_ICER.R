##===========================================================================#
## Author: Guido Espana & Alex Perkins
##===========================================================================#
## user input ---------------
##===========================================================================#
rm(list = ls())
library(RColorBrewer)
library(tidyverse)
library(grDevices)

icer_data = read_csv("../output/report_table_cost_effectiveness_QALY.csv")

jpeg('../figures/report_figure_ICER.jpeg',
     width=5,height=5,units='in',res=300)
plot(icer_data$vaccine_cost, icer_data$BothHigh, type = "l", col = "black",
     main = "Cost-effectiveness", ylab = "ICER", xlab = "Vaccine (USD)",
     ylim = c(0,3*icer_data$gdp[1]))
lines(icer_data$vaccine_cost, icer_data$LowSensitivity, type = "l", col = "red")
lines(icer_data$vaccine_cost, icer_data$LowSpecificity, type = "l", col = "blue")
abline(h = icer_data$gdp[1], col = "black")
abline(h = 3*icer_data$gdp[1], col = "black")
abline(h = 0, col = "black")
legend(x = 0, y = 3* 0.9 * icer_data$gdp[1],
       legend = c("Baseline",
                  "Low Sensitivity",
                  "Low Specificity"),
       col = c("black","red","blue"), lty = c(1,1))
dev.off()
