##===========================================================================#
## Author: Guido Espana 
##===========================================================================#
## user input ---------------
##===========================================================================#
rm(list = ls())
library(RColorBrewer)
library(tidyverse)
library(randomForest)
library(grDevices)
library(mgcv)

set.seed(123)
##===========================================================================#
## Economic parameters ---------------
##===========================================================================#
## Consumer price index for Puerto Rico
CPI_2019 = 118
CPI_2010 = 110
CPI_2016 = 116

## Econ data are based on Flasche2016 from PLOS MEDICINE
perspective = "PublicPayer"
country = "PuertoRico"
econ = list()
econ$c.vax = 70
econ$c.test = 10
if(perspective == "PublicPayer"){
  if(country == "PuertoRico"){
    econ$c.amb = 239 * CPI_2019 / CPI_2010
    econ$c.hosp = 1615 * CPI_2019 / CPI_2010
    econ$c.death = 0
    econ$gdp = 30833 * CPI_2019 / CPI_2016
  }else{
    print("Country not found")
  }  
}

## From Shim et al., 2016
econ$l.hosp = 14/365
econ$l.amb = 4/365

econ$d.amb = 0.197 * 4/365
econ$d.hosp = 0.545 * 14/365
econ$d.death = 1
econ$d.vax = 0
econ$l.vax = 0

## Other values
p.death = 0.005
discounting.rate = 0.03
discounting.health.rate = 0.03

##==========================================================================#
## Functions ---------------
##==========================================================================#
plot_color_bar = function(myColStrip_in, myColBreaks_in,myColBar_in, myTicks_in, plot_line = TRUE,myBarLims_in, label_in = "",...){
  image(t(myColStrip_in),
        col = myColBar_in, breaks = myColBreaks_in,
        xlim = c(0.0,1.0), axes = F)
  ylims = par("usr")[c(3,4)]
  slope_y = diff(ylims) / diff(myBarLims_in) 
  b_y = ylims[2] - slope_y * myBarLims_in[2]
  myBarTicks = myTicks_in * slope_y + b_y
  myBarTickLabels = myTicks_in
  axis(4,labels = rep("",length(myBarTicks)), at = myBarTicks,cex.axis = 0.8,tck = -0.1)
  axis(4, at =  myBarTicks, labels = myBarTickLabels, cex.axis = 0.8,line = -1.0,lwd = 0)
  if(plot_line){
    abline(h = myBarTicks[2], col = "black", lwd = 2) 
  }
  mtext(label_in, side = 4, outer = F, lwd = 2,...)
}

estimate_costs_public_payer = function(dis_table.in, econ.in,
                                       p.death.in, disc.rate.in){
    n.years = nrow(dis_table.in)
    disc.rate = 1/((1+disc.rate.in)^(0:(n.years - 1)))
  
    ## Disease cases in the dataframe dis_table.in do not include hospitalizations...
    ## NO need to substract
    cost.treatment = sum(
    (
        (dis_table.in$DisAverted) * econ.in$c.amb + 
        (dis_table.in$HospAverted) * econ.in$c.hosp + 
        (dis_table.in$HospAverted) * p.death.in * econ.in$c.death
    ) * disc.rate, na.rm = T)
    return(cost.treatment)    
}

estimate_dalys_public_payer = function(dis_table.in, econ.in,
                                       p.death.in, disc.daly.in){
    n.years = nrow(dis_table.in)
    disc.daly = 1/((1+disc.daly.in)^(0:(n.years - 1)))
    daly.treat = sum(
    ((dis_table.in$DisAverted ) * econ.in$d.amb + 
     (dis_table.in$HospAverted) * econ.in$d.hosp 
    ) * disc.daly, na.rm = T)
  
    daly.death = sum(
    (
        (dis_table.in$DaysLostDeathAverted) / 365) * econ.in$d.death * disc.daly, na.rm = T
    )

    daly.vax = sum(dis_table.in$Vaccinated * econ.in$d.vax * disc.daly, na.rm = T)
    
    net.daly = daly.treat - daly.vax
    return(list(net.daly.factor = net.daly, death.daly.factor = daly.death))
}

estimate_qalys_public_payer = function(dis_table.in, econ.in,
                                       p.death.in, disc.qaly.in){
    n.years = nrow(dis_table.in)
    disc.qaly = 1/((1+disc.qaly.in)^(0:(n.years - 1)))
    
    qaly.treat = sum(
    (
        (dis_table.in$DisAverted ) * econ.in$d.amb * (1 - exp(-disc.qaly.in * econ.in$l.amb)) / econ.in$l.amb +
        (dis_table.in$HospAverted ) * econ.in$d.hosp * (1 - exp(-disc.qaly.in * econ.in$l.hosp)) / econ.in$l.hosp  
        ) * disc.qaly, na.rm = T)
  
    qaly.death = sum(
    (
        ( 1 - exp(-disc.qaly.in*(dis_table.in$DaysLostDeathAverted / 365))) / disc.qaly.in
    ) * econ.in$d.death * disc.qaly, na.rm = T)

    
    qaly.vax = sum(
        dis_table.in$Vaccinated * econ.in$d.vax *
        ((1 - exp(-disc.qaly.in * econ.in$l.vax))/disc.qaly.in) * disc.qaly,
        na.rm = T)
  
    net.qaly = qaly.treat - qaly.vax
    return(list(net.qaly.factor = net.qaly, death.qaly.factor = qaly.death))
}

##===========================================================================#
## Read data and set economic variables ---------------
##===========================================================================#
summary_list = readRDS('../data/summary_routine_vaccination_life_exp_All_sweep_files_test.RDS')

    cea_train_table_test = data.frame(
        Specificity = rep(0,length(summary_list)), 
        Sensitivity = 0, SP9 = 0, net.daly.factor = 0,
        net.treat.cost = 0, net.qaly.factor = 0,
        death.daly.factor = 0, death.qaly.factor = 0,
        Prop.vax = 0, Tested = 0, PE9 = 0)

for(ff in 1:length(summary_list)){
    summary_tmp = summary_list[[ff]] %>% filter(Year <= 50)
    cea_train_table_test$Specificity[ff] = summary_tmp$Specificity[1]
    cea_train_table_test$Sensitivity[ff] = summary_tmp$Sensitivity[1]
    cea_train_table_test$SP9[ff] = summary_tmp$SP9Prevax[1]
    cea_train_table_test$PE9[ff] = summary_tmp$PE9[1]
    cea_train_table_test$Prop.vax[ff] = sum(summary_tmp$Vaccinated) / sum(summary_tmp$Tested)
    cea_train_table_test$Tested[ff] =  sum(summary_tmp$Tested)
    
    costs_intervention = estimate_costs_public_payer(
        summary_tmp, econ.in = econ, p.death.in = p.death, 
        disc.rate.in = discounting.rate)

    dalys_averted_intervention = estimate_dalys_public_payer(
        summary_tmp, econ.in = econ, p.death.in = p.death, 
        disc.daly.in = discounting.health.rate)
    
    qalys_gained_intervention = estimate_qalys_public_payer(
        summary_tmp, econ.in = econ, p.death.in = p.death, 
        disc.qaly.in = discounting.health.rate)
    
    cea_train_table_test$net.treat.cost[ff] =  costs_intervention
    cea_train_table_test$net.daly.factor[ff] =  dalys_averted_intervention$net.daly.factor
    cea_train_table_test$death.daly.factor[ff] = dalys_averted_intervention$death.daly.factor
    cea_train_table_test$net.qaly.factor[ff] =  qalys_gained_intervention$net.qaly.factor
    cea_train_table_test$death.qaly.factor[ff] = qalys_gained_intervention$death.qaly.factor
    
    if(ff%%100 == 0){cat("\r:",ff)}
}
##===========================================================================#
## Model fitting ---------
##===========================================================================#
m = "gam"
if(m == "randomforest"){
  model_cost_treat_test = randomForest(net.treat.cost ~ (
    Specificity + Sensitivity + SP9)^3,
    data = cea_train_table_test,
    ntree = 1000, importance = T)
  
  model_daly_test = randomForest(net.daly.factor ~ (
    Specificity + Sensitivity + SP9)^3,
    data = cea_train_table_test,
    ntree = 1000, importance = T)

  model_qaly_test = randomForest(net.qaly.factor ~ (
    Specificity + Sensitivity + SP9)^3,
    data = cea_train_table_test,
    ntree = 1000, importance = T)

  model_qaly_death_test = randomForest(death.qaly.factor ~ (
    Specificity + Sensitivity + SP9)^3,
    data = cea_train_table_test,
    ntree = 1000, importance = T)
    
  model_death = randomForest(death.daly.factor ~ (
    Specificity + Sensitivity + SP9 )^3,
    data = cea_train_table_test,
    ntree = 1000, importance = T)
  
}else if (m == "gam"){
  model_cost_treat_test = gam(net.treat.cost ~ s(
      Specificity,Sensitivity,SP9, bs = "gp"),
      data = cea_train_table_test,
      family = "gaussian")

  model_daly_test = gam(net.daly.factor ~ s(
    Specificity, Sensitivity, SP9, bs = "gp"),
    data = cea_train_table_test,
    family = "gaussian")
  
  model_death = gam(death.daly.factor ~ s(
    Specificity, Sensitivity, SP9, bs = "gp"), 
    data = cea_train_table_test,
    family = "gaussian")

  model_qaly_test = gam(net.qaly.factor ~ s(
    Specificity, Sensitivity, SP9, bs = "gp"),
    data = cea_train_table_test,
    family = "gaussian")
  
  model_qaly_death_test = gam(death.qaly.factor ~ s(
    Specificity, Sensitivity, SP9, bs = "gp"), 
    data = cea_train_table_test,
    family = "gaussian")
}

N.tested = mean(cea_train_table_test$Tested)

##===========================================================================#
## Calculate ICER for specific scenarios ----------
##===========================================================================#
country_pe9 = 0.5
screening_sp = c(0.95,0.95,0.8)
screening_sen = c(0.95,0.8,0.95)

icer_data_all = data.frame(Specificity = screening_sp,
                       Sensitivity = screening_sen,
                       SP9 = country_pe9)

unit_cost_vaccine = as.vector(seq(from=0,to=300,by = 5))
ICER_QALY = matrix(data = 0, nrow = length(unit_cost_vaccine), ncol = nrow(icer_data_all))
ICER_DALY = ICER_QALY

for(i in 1:length(screening_sp)){
    icer_data = icer_data_all[i,]
    prop_vax = icer_data$SP9 * icer_data$Sensitivity + (1 - icer_data$Specificity) * (1-icer_data$SP9)

    costs_int = econ$c.test * N.tested + unit_cost_vaccine * N.tested * prop_vax
    costs_treat_averted = as.numeric(predict(model_cost_treat_test,icer_data,predict.all = F))
    
    dalys_averted_disease = as.numeric(predict(model_daly_test, icer_data,predict.all = F))
    dalys_averted_death = as.numeric(predict(model_death, icer_data,predict.all = F))
    
    qalys_averted_disease = as.numeric(predict(model_qaly_test, icer_data,predict.all = F))
    qalys_averted_death = as.numeric(predict(model_qaly_death_test, icer_data,predict.all = F))

    (ICER_QALY[,i] = (costs_int - costs_treat_averted) / (qalys_averted_disease + qalys_averted_death))
    (ICER_DALY[,i] = (costs_int - costs_treat_averted) / (dalys_averted_disease + dalys_averted_death))
}

colnames(ICER_QALY)= c("BothHigh", "LowSensitivity", "LowSpecificity")
icer_data = cbind(data.frame(vaccine_cost = unit_cost_vaccine, gdp = econ$gdp), ICER_QALY)
write_csv(x = icer_data, path = "../output/report_table_cost_effectiveness_QALY.csv")

##===========================================================================#
## Save PSA data---------------------
##===========================================================================#
psa_data = data.frame(Specificity = c(0.5,1.0),
                      Sensitivity = c(0.5,1.0),
                      SP9 = c(0.25,0.75),
                      vax_cost = c(10,300),
                      test_cost = c(1,20)) %>%
    gather(key = parameter, value = value) %>%
    group_by(parameter) %>%
    summarize(min = min(value), max = max(value)) %>%
    ungroup() %>%
    mutate(ICER_min = 0, ICER_max = 0, ICER_default = 0)

default_train_data = data.frame(Specificity = 0.95,
                                Sensitivity = 0.8,
                                SP9 = 0.5)

for(i in 1:nrow(psa_data)){
    for(value_col in c("min","max","default")){
        train_data = default_train_data
        cost_data = data.frame(vax_cost = econ$c.vax,
                               test_cost = econ$c.test)
        if(value_col != "default"){            
            if(psa_data$parameter[i] %in% colnames(train_data)){
                train_data[,psa_data$parameter[i]] = psa_data[i,value_col]
            }else if(psa_data$parameter[i] %in% colnames(cost_data)){
                cost_data[,psa_data$parameter[i]] = psa_data[i,value_col]
            }
        }
        
        prop_vax = train_data$SP9 * train_data$Sensitivity + (1 - train_data$Specificity) * (1-train_data$SP9)
        
        costs_int = cost_data$test_cost * N.tested + cost_data$vax_cost * N.tested * prop_vax
        costs_treat_averted = as.numeric(predict(model_cost_treat_test,train_data,predict.all = F))
        
        dalys_averted_disease = as.numeric(predict(model_daly_test, train_data,predict.all = F))
        dalys_averted_death = as.numeric(predict(model_death, train_data,predict.all = F))
        
        qalys_averted_disease = as.numeric(predict(model_qaly_test, train_data,predict.all = F))
        qalys_averted_death = as.numeric(predict(model_qaly_death_test, train_data,predict.all = F))

        icer_col = sprintf("ICER_%s",value_col)
        psa_data[i,icer_col] = (costs_int - costs_treat_averted) / (qalys_averted_disease + qalys_averted_death)
    }        
}

psa_data$GDP = econ$gdp

## Now, just save dataframe as a .csv file to create the figure in python :) 
write_csv(x = psa_data, path = "../output/report_table_psa_results.csv")
