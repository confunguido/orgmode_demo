##===========================================================================#
## Author: Guido España
## Create a .TeX table with the results from the sensitivity analysis
##===========================================================================#
library(xtable)

table = read.csv('../output/report_table_psa_results.csv')
print(xtable(table,label="table-tornado", caption = "Sensitivity analysis of cost-effectiveness", digits = 2), floating = F, include.rownames = F, file = "../../manuscript/table_ICER_psa.tex")

