
library(tidyverse)
library(lubridate)

#### A  Model and scenarios extraction ####


#### RCP 4.5 ####

# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566


#### Model 4 ####


# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_4_rcp45_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_4_rcp45_wb) <- colnmss

mod_4_rcp45_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_4_rcp45_aqu) <- colnmsss

mod_4_rcp45_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_4_rcp45_reservoir) <- colnmssss

#mod_4_rcp45_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_4_rcp45_flocha) <- colnmsssss

#mod_4_rcp45_flocha <- mod_4_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 4, escenario = "RCP 4.5")


# Analysed variables --> Daily scale

mod_4_rcp45_wb <- mod_4_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_4_rcp45_basinaqu_wb <- mod_4_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_4_rcp45_reservoir <- mod_4_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_4_rcp45_wb_yr <- mod_4_rcp45_wb %>% left_join(., mod_4_rcp45_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_4_rcp45_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_4_rcp45_wb_yr <- mod_4_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_4_rcp45 <- mod_4_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_4_45 <- average_wb_periods_mod_4_rcp45[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_4_45_average_anomalies <- average_wb_periods_mod_4_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_4_45$precip) / ref_values_mod_4_45$precip,
                                                                        et_anom = (et - ref_values_mod_4_45$et)/ref_values_mod_4_45$et,
                                                                        pet_anom = (pet - ref_values_mod_4_45$pet)/ref_values_mod_4_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_4_45$perc)/ref_values_mod_4_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_4_45$rchrg)/ref_values_mod_4_45$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_4_45$total_flow)/ref_values_mod_4_45$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_4_45$surq)/ref_values_mod_4_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_4_45$latq)/ref_values_mod_4_45$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_4_45$gwflo_cha)/ref_values_mod_4_45$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_4_45$gwflo_res)/ref_values_mod_4_45$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_4_45$total_gwflo)/ref_values_mod_4_45$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_4_45$reser_floin_mm)/ref_values_mod_4_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]
                                                   

# Annual anomalies 

mod_4_45_year_anomalies <-  mod_4_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_4_45$precip) / ref_values_mod_4_45$precip,
                                                         et_anom = (et - ref_values_mod_4_45$et)/ref_values_mod_4_45$et,
                                                         pet_anom = (pet - ref_values_mod_4_45$pet)/ref_values_mod_4_45$pet,
                                                         perc_anom = (perc - ref_values_mod_4_45$perc)/ref_values_mod_4_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_4_45$rchrg)/ref_values_mod_4_45$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_4_45$total_flow)/ref_values_mod_4_45$total_flow,
                                                         surq_anom = (surq - ref_values_mod_4_45$surq)/ref_values_mod_4_45$surq,
                                                         latq_anom = (latq - ref_values_mod_4_45$latq)/ref_values_mod_4_45$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_4_45$gwflo_cha)/ref_values_mod_4_45$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_4_45$gwflo_res)/ref_values_mod_4_45$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_4_45$total_gwflo)/ref_values_mod_4_45$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_4_45$reser_floin_mm)/ref_values_mod_4_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_4_45_average_changes <- average_wb_periods_mod_4_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_4_45$precip),
                                                                      et_chg = (et - ref_values_mod_4_45$et),
                                                                      pet_chg = (pet - ref_values_mod_4_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_4_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_4_45$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_4_45$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_4_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_4_45$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_4_45$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_4_45$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_4_45$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_4_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_4_45_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_4_45_year_changes <- mod_4_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_4_45$precip),
                                                      et_chg = (et - ref_values_mod_4_45$et),
                                                      pet_chg = (pet - ref_values_mod_4_45$pet),
                                                      perc_chg = (perc - ref_values_mod_4_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_4_45$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_4_45$total_flow),
                                                      surq_chg = (surq - ref_values_mod_4_45$surq),
                                                      latq_chg = (latq - ref_values_mod_4_45$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_4_45$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_4_45$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_4_45$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_4_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]




# Periods definition  

#rm(list = ls())  

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566


#### Model 6 ####


# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_6_rcp45_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_6_rcp45_wb) <- colnmss

mod_6_rcp45_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_6_rcp45_aqu) <- colnmsss

mod_6_rcp45_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_6_rcp45_reservoir) <- colnmssss

#mod_6_rcp45_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_6_RACMO22/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_6_rcp45_flocha) <- colnmsssss

#mod_6_rcp45_flocha <- mod_6_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 6, escenario = "RCP 4.5")


# Analysed variables --> Daily scale

mod_6_rcp45_wb <- mod_6_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_6_rcp45_basinaqu_wb <- mod_6_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_6_rcp45_reservoir <- mod_6_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_6_rcp45_wb_yr <- mod_6_rcp45_wb %>% left_join(., mod_6_rcp45_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_6_rcp45_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_6_rcp45_wb_yr <- mod_6_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_6_rcp45 <- mod_6_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_6_45 <- average_wb_periods_mod_6_rcp45[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_6_45_average_anomalies <- average_wb_periods_mod_6_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_6_45$precip) / ref_values_mod_6_45$precip,
                                                                        et_anom = (et - ref_values_mod_6_45$et)/ref_values_mod_6_45$et,
                                                                        pet_anom = (pet - ref_values_mod_6_45$pet)/ref_values_mod_6_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_6_45$perc)/ref_values_mod_6_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_6_45$rchrg)/ref_values_mod_6_45$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_6_45$total_flow)/ref_values_mod_6_45$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_6_45$surq)/ref_values_mod_6_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_6_45$latq)/ref_values_mod_6_45$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_6_45$gwflo_cha)/ref_values_mod_6_45$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_6_45$gwflo_res)/ref_values_mod_6_45$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_6_45$total_gwflo)/ref_values_mod_6_45$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_6_45$reser_floin_mm)/ref_values_mod_6_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_6_45_year_anomalies <-  mod_6_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_6_45$precip) / ref_values_mod_6_45$precip,
                                                         et_anom = (et - ref_values_mod_6_45$et)/ref_values_mod_6_45$et,
                                                         pet_anom = (pet - ref_values_mod_6_45$pet)/ref_values_mod_6_45$pet,
                                                         perc_anom = (perc - ref_values_mod_6_45$perc)/ref_values_mod_6_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_6_45$rchrg)/ref_values_mod_6_45$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_6_45$total_flow)/ref_values_mod_6_45$total_flow,
                                                         surq_anom = (surq - ref_values_mod_6_45$surq)/ref_values_mod_6_45$surq,
                                                         latq_anom = (latq - ref_values_mod_6_45$latq)/ref_values_mod_6_45$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_6_45$gwflo_cha)/ref_values_mod_6_45$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_6_45$gwflo_res)/ref_values_mod_6_45$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_6_45$total_gwflo)/ref_values_mod_6_45$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_6_45$reser_floin_mm)/ref_values_mod_6_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_6_45_average_changes <- average_wb_periods_mod_6_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_6_45$precip),
                                                                      et_chg = (et - ref_values_mod_6_45$et),
                                                                      pet_chg = (pet - ref_values_mod_6_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_6_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_6_45$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_6_45$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_6_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_6_45$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_6_45$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_6_45$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_6_45$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_6_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_6_45_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_6_45_year_changes <- mod_6_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_6_45$precip),
                                                      et_chg = (et - ref_values_mod_6_45$et),
                                                      pet_chg = (pet - ref_values_mod_6_45$pet),
                                                      perc_chg = (perc - ref_values_mod_6_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_6_45$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_6_45$total_flow),
                                                      surq_chg = (surq - ref_values_mod_6_45$surq),
                                                      latq_chg = (latq - ref_values_mod_6_45$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_6_45$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_6_45$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_6_45$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_6_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]


#### Model 7 ####
# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566

# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_7_rcp45_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_7_rcp45_wb) <- colnmss

mod_7_rcp45_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_7_rcp45_aqu) <- colnmsss

mod_7_rcp45_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_7_rcp45_reservoir) <- colnmssss

#mod_7_rcp45_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_7_DMI-HIRHAM/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_7_rcp45_flocha) <- colnmsssss

#mod_7_rcp45_flocha <- mod_7_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 7, escenario = "RCP 4.5")


# Analysed variables --> Daily scale

mod_7_rcp45_wb <- mod_7_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_7_rcp45_basinaqu_wb <- mod_7_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_7_rcp45_reservoir <- mod_7_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_7_rcp45_wb_yr <- mod_7_rcp45_wb %>% left_join(., mod_7_rcp45_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_7_rcp45_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_7_rcp45_wb_yr <- mod_7_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_7_rcp45 <- mod_7_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_7_45 <- average_wb_periods_mod_7_rcp45[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_7_45_average_anomalies <- average_wb_periods_mod_7_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_7_45$precip) / ref_values_mod_7_45$precip,
                                                                        et_anom = (et - ref_values_mod_7_45$et)/ref_values_mod_7_45$et,
                                                                        pet_anom = (pet - ref_values_mod_7_45$pet)/ref_values_mod_7_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_7_45$perc)/ref_values_mod_7_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_7_45$rchrg)/ref_values_mod_7_45$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_7_45$total_flow)/ref_values_mod_7_45$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_7_45$surq)/ref_values_mod_7_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_7_45$latq)/ref_values_mod_7_45$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_7_45$gwflo_cha)/ref_values_mod_7_45$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_7_45$gwflo_res)/ref_values_mod_7_45$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_7_45$total_gwflo)/ref_values_mod_7_45$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_7_45$reser_floin_mm)/ref_values_mod_7_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_7_45_year_anomalies <-  mod_7_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_7_45$precip) / ref_values_mod_7_45$precip,
                                                         et_anom = (et - ref_values_mod_7_45$et)/ref_values_mod_7_45$et,
                                                         pet_anom = (pet - ref_values_mod_7_45$pet)/ref_values_mod_7_45$pet,
                                                         perc_anom = (perc - ref_values_mod_7_45$perc)/ref_values_mod_7_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_7_45$rchrg)/ref_values_mod_7_45$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_7_45$total_flow)/ref_values_mod_7_45$total_flow,
                                                         surq_anom = (surq - ref_values_mod_7_45$surq)/ref_values_mod_7_45$surq,
                                                         latq_anom = (latq - ref_values_mod_7_45$latq)/ref_values_mod_7_45$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_7_45$gwflo_cha)/ref_values_mod_7_45$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_7_45$gwflo_res)/ref_values_mod_7_45$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_7_45$total_gwflo)/ref_values_mod_7_45$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_7_45$reser_floin_mm)/ref_values_mod_7_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_7_45_average_changes <- average_wb_periods_mod_7_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_7_45$precip),
                                                                      et_chg = (et - ref_values_mod_7_45$et),
                                                                      pet_chg = (pet - ref_values_mod_7_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_7_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_7_45$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_7_45$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_7_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_7_45$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_7_45$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_7_45$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_7_45$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_7_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_7_45_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_7_45_year_changes <- mod_7_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_7_45$precip),
                                                      et_chg = (et - ref_values_mod_7_45$et),
                                                      pet_chg = (pet - ref_values_mod_7_45$pet),
                                                      perc_chg = (perc - ref_values_mod_7_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_7_45$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_7_45$total_flow),
                                                      surq_chg = (surq - ref_values_mod_7_45$surq),
                                                      latq_chg = (latq - ref_values_mod_7_45$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_7_45$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_7_45$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_7_45$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_7_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]




#### Model 9 ####

# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566
# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_9_rcp45_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_9_rcp45_wb) <- colnmss

mod_9_rcp45_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_9_rcp45_aqu) <- colnmsss

mod_9_rcp45_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_9_rcp45_reservoir) <- colnmssss

#mod_9_rcp45_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_9_MPI-M-MPI/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_9_rcp45_flocha) <- colnmsssss

#mod_9_rcp45_flocha <- mod_9_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 9, escenario = "RCP 4.5")


# Analysed variables --> Daily scale

mod_9_rcp45_wb <- mod_9_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_9_rcp45_basinaqu_wb <- mod_9_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_9_rcp45_reservoir <- mod_9_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_9_rcp45_wb_yr <- mod_9_rcp45_wb %>% left_join(., mod_9_rcp45_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_9_rcp45_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_9_rcp45_wb_yr <- mod_9_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_9_rcp45 <- mod_9_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_9_45 <- average_wb_periods_mod_9_rcp45[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_9_45_average_anomalies <- average_wb_periods_mod_9_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_9_45$precip) / ref_values_mod_9_45$precip,
                                                                        et_anom = (et - ref_values_mod_9_45$et)/ref_values_mod_9_45$et,
                                                                        pet_anom = (pet - ref_values_mod_9_45$pet)/ref_values_mod_9_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_9_45$perc)/ref_values_mod_9_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_9_45$rchrg)/ref_values_mod_9_45$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_9_45$total_flow)/ref_values_mod_9_45$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_9_45$surq)/ref_values_mod_9_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_9_45$latq)/ref_values_mod_9_45$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_9_45$gwflo_cha)/ref_values_mod_9_45$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_9_45$gwflo_res)/ref_values_mod_9_45$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_9_45$total_gwflo)/ref_values_mod_9_45$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_9_45$reser_floin_mm)/ref_values_mod_9_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_9_45_year_anomalies <-  mod_9_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_9_45$precip) / ref_values_mod_9_45$precip,
                                                         et_anom = (et - ref_values_mod_9_45$et)/ref_values_mod_9_45$et,
                                                         pet_anom = (pet - ref_values_mod_9_45$pet)/ref_values_mod_9_45$pet,
                                                         perc_anom = (perc - ref_values_mod_9_45$perc)/ref_values_mod_9_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_9_45$rchrg)/ref_values_mod_9_45$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_9_45$total_flow)/ref_values_mod_9_45$total_flow,
                                                         surq_anom = (surq - ref_values_mod_9_45$surq)/ref_values_mod_9_45$surq,
                                                         latq_anom = (latq - ref_values_mod_9_45$latq)/ref_values_mod_9_45$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_9_45$gwflo_cha)/ref_values_mod_9_45$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_9_45$gwflo_res)/ref_values_mod_9_45$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_9_45$total_gwflo)/ref_values_mod_9_45$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_9_45$reser_floin_mm)/ref_values_mod_9_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_9_45_average_changes <- average_wb_periods_mod_9_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_9_45$precip),
                                                                      et_chg = (et - ref_values_mod_9_45$et),
                                                                      pet_chg = (pet - ref_values_mod_9_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_9_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_9_45$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_9_45$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_9_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_9_45$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_9_45$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_9_45$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_9_45$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_9_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_9_45_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_9_45_year_changes <- mod_9_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_9_45$precip),
                                                      et_chg = (et - ref_values_mod_9_45$et),
                                                      pet_chg = (pet - ref_values_mod_9_45$pet),
                                                      perc_chg = (perc - ref_values_mod_9_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_9_45$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_9_45$total_flow),
                                                      surq_chg = (surq - ref_values_mod_9_45$surq),
                                                      latq_chg = (latq - ref_values_mod_9_45$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_9_45$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_9_45$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_9_45$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_9_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]






#### Model 14 ####

# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566

# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_14_rcp45_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_14_rcp45_wb) <- colnmss

mod_14_rcp45_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_14_rcp45_aqu) <- colnmsss

mod_14_rcp45_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_14_rcp45_reservoir) <- colnmssss

#mod_14_rcp45_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_14_MOHC-HadGEM2/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_14_rcp45_flocha) <- colnmsssss

#mod_14_rcp45_flocha <- mod_14_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 14, escenario = "RCP 4.5")


# Analysed variables --> Daily scale

mod_14_rcp45_wb <- mod_14_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                               latq_cha, latq_res, 
                                               surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_14_rcp45_basinaqu_wb <- mod_14_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                        seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_14_rcp45_reservoir <- mod_14_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_14_rcp45_wb_yr <- mod_14_rcp45_wb %>% left_join(., mod_14_rcp45_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_14_rcp45_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_14_rcp45_wb_yr <- mod_14_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                        year %in% midterm_period ~ "midterm", 
                                                                        year %in% longterm_period ~ "longterm", 
                                                                        .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_14_rcp45 <- mod_14_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_14_45 <- average_wb_periods_mod_14_rcp45[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_14_45_average_anomalies <- average_wb_periods_mod_14_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_14_45$precip) / ref_values_mod_14_45$precip,
                                                                          et_anom = (et - ref_values_mod_14_45$et)/ref_values_mod_14_45$et,
                                                                          pet_anom = (pet - ref_values_mod_14_45$pet)/ref_values_mod_14_45$pet,
                                                                          perc_anom = (perc - ref_values_mod_14_45$perc)/ref_values_mod_14_45$perc,
                                                                          rchrg_anom = (rchrg - ref_values_mod_14_45$rchrg)/ref_values_mod_14_45$rchrg,
                                                                          total_flow_anom = (total_flow  - ref_values_mod_14_45$total_flow)/ref_values_mod_14_45$total_flow,
                                                                          surq_anom = (surq - ref_values_mod_14_45$surq)/ref_values_mod_14_45$surq,
                                                                          latq_anom = (latq - ref_values_mod_14_45$latq)/ref_values_mod_14_45$latq,
                                                                          gwflocha_anom = (gwflo_cha - ref_values_mod_14_45$gwflo_cha)/ref_values_mod_14_45$gwflo_cha, 
                                                                          gwflores_anom = (gwflo_res - ref_values_mod_14_45$gwflo_res)/ref_values_mod_14_45$gwflo_res,
                                                                          totalgwflo_anom = (total_gwflo - ref_values_mod_14_45$total_gwflo)/ref_values_mod_14_45$total_gwflo,
                                                                          res_floin_anom = (reser_floin_mm - ref_values_mod_14_45$reser_floin_mm)/ref_values_mod_14_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_14_45_year_anomalies <-  mod_14_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_14_45$precip) / ref_values_mod_14_45$precip,
                                                           et_anom = (et - ref_values_mod_14_45$et)/ref_values_mod_14_45$et,
                                                           pet_anom = (pet - ref_values_mod_14_45$pet)/ref_values_mod_14_45$pet,
                                                           perc_anom = (perc - ref_values_mod_14_45$perc)/ref_values_mod_14_45$perc,
                                                           rchrg_anom = (rchrg - ref_values_mod_14_45$rchrg)/ref_values_mod_14_45$rchrg,
                                                           total_flow_anom = (total_flow  - ref_values_mod_14_45$total_flow)/ref_values_mod_14_45$total_flow,
                                                           surq_anom = (surq - ref_values_mod_14_45$surq)/ref_values_mod_14_45$surq,
                                                           latq_anom = (latq - ref_values_mod_14_45$latq)/ref_values_mod_14_45$latq,
                                                           gwflocha_anom = (gwflo_cha - ref_values_mod_14_45$gwflo_cha)/ref_values_mod_14_45$gwflo_cha, 
                                                           gwflores_anom = (gwflo_res - ref_values_mod_14_45$gwflo_res)/ref_values_mod_14_45$gwflo_res,
                                                           totalgwflo_anom = (total_gwflo - ref_values_mod_14_45$total_gwflo)/ref_values_mod_14_45$total_gwflo,
                                                           res_floin_anom = (reser_floin_mm - ref_values_mod_14_45$reser_floin_mm)/ref_values_mod_14_45$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_14_45_average_changes <- average_wb_periods_mod_14_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_14_45$precip),
                                                                        et_chg = (et - ref_values_mod_14_45$et),
                                                                        pet_chg = (pet - ref_values_mod_14_45$pet),
                                                                        perc_chg = (perc - ref_values_mod_14_45$perc),
                                                                        rchrg_chg = (rchrg - ref_values_mod_14_45$rchrg),
                                                                        totalflo_chg = (total_flow - ref_values_mod_14_45$total_flow),
                                                                        surq_chg = (surq - ref_values_mod_14_45$surq),
                                                                        latq_chg = (latq - ref_values_mod_14_45$latq),
                                                                        gwflocha_chg = (gwflo_cha - ref_values_mod_14_45$gwflo_cha),
                                                                        gwflores_chg = (gwflo_res - ref_values_mod_14_45$gwflo_res),
                                                                        totalgwflo_chg = (total_gwflo - ref_values_mod_14_45$total_gwflo),
                                                                        res_floin_chg = (reser_floin_mm - ref_values_mod_14_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_14_45_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_14_45_year_changes <- mod_14_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_14_45$precip),
                                                        et_chg = (et - ref_values_mod_14_45$et),
                                                        pet_chg = (pet - ref_values_mod_14_45$pet),
                                                        perc_chg = (perc - ref_values_mod_14_45$perc),
                                                        rchrg_chg = (rchrg - ref_values_mod_14_45$rchrg),
                                                        totalflo_chg = (total_flow - ref_values_mod_14_45$total_flow),
                                                        surq_chg = (surq - ref_values_mod_14_45$surq),
                                                        latq_chg = (latq - ref_values_mod_14_45$latq),
                                                        gwflocha_chg = (gwflo_cha - ref_values_mod_14_45$gwflo_cha),
                                                        gwflores_chg = (gwflo_res - ref_values_mod_14_45$gwflo_res),
                                                        totalgwflo_chg = (total_gwflo - ref_values_mod_14_45$total_gwflo),
                                                        res_floin_chg = (reser_floin_mm - ref_values_mod_14_45$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]




#### RCP 8.5 ####

# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566


#### Model 4 ####


# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_4_rcp85_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_4_rcp85_wb) <- colnmss

mod_4_rcp85_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_4_rcp85_aqu) <- colnmsss

mod_4_rcp85_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_4_rcp85_reservoir) <- colnmssss

#mod_4_rcp85_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_4_CCLM/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_4_rcp85_flocha) <- colnmsssss

#mod_4_rcp85_flocha <- mod_4_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 4, escenario = "RCP 8.5")


# Analysed variables --> Daily scale

mod_4_rcp85_wb <- mod_4_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_4_rcp85_basinaqu_wb <- mod_4_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_4_rcp85_reservoir <- mod_4_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_4_rcp85_wb_yr <- mod_4_rcp85_wb %>% left_join(., mod_4_rcp85_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_4_rcp85_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_4_rcp85_wb_yr <- mod_4_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_4_rcp85 <- mod_4_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_4_85 <- average_wb_periods_mod_4_rcp85[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_4_85_average_anomalies <- average_wb_periods_mod_4_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_4_85$precip) / ref_values_mod_4_85$precip,
                                                                        et_anom = (et - ref_values_mod_4_85$et)/ref_values_mod_4_85$et,
                                                                        pet_anom = (pet - ref_values_mod_4_85$pet)/ref_values_mod_4_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_4_85$perc)/ref_values_mod_4_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_4_85$rchrg)/ref_values_mod_4_85$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_4_85$total_flow)/ref_values_mod_4_85$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_4_85$surq)/ref_values_mod_4_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_4_85$latq)/ref_values_mod_4_85$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_4_85$gwflo_cha)/ref_values_mod_4_85$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_4_85$gwflo_res)/ref_values_mod_4_85$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_4_85$total_gwflo)/ref_values_mod_4_85$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_4_85$reser_floin_mm)/ref_values_mod_4_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_4_85_year_anomalies <-  mod_4_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_4_85$precip) / ref_values_mod_4_85$precip,
                                                         et_anom = (et - ref_values_mod_4_85$et)/ref_values_mod_4_85$et,
                                                         pet_anom = (pet - ref_values_mod_4_85$pet)/ref_values_mod_4_85$pet,
                                                         perc_anom = (perc - ref_values_mod_4_85$perc)/ref_values_mod_4_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_4_85$rchrg)/ref_values_mod_4_85$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_4_85$total_flow)/ref_values_mod_4_85$total_flow,
                                                         surq_anom = (surq - ref_values_mod_4_85$surq)/ref_values_mod_4_85$surq,
                                                         latq_anom = (latq - ref_values_mod_4_85$latq)/ref_values_mod_4_85$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_4_85$gwflo_cha)/ref_values_mod_4_85$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_4_85$gwflo_res)/ref_values_mod_4_85$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_4_85$total_gwflo)/ref_values_mod_4_85$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_4_85$reser_floin_mm)/ref_values_mod_4_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_4_85_average_changes <- average_wb_periods_mod_4_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_4_85$precip),
                                                                      et_chg = (et - ref_values_mod_4_85$et),
                                                                      pet_chg = (pet - ref_values_mod_4_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_4_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_4_85$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_4_85$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_4_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_4_85$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_4_85$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_4_85$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_4_85$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_4_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_4_85_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_4_85_year_changes <- mod_4_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_4_85$precip),
                                                      et_chg = (et - ref_values_mod_4_85$et),
                                                      pet_chg = (pet - ref_values_mod_4_85$pet),
                                                      perc_chg = (perc - ref_values_mod_4_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_4_85$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_4_85$total_flow),
                                                      surq_chg = (surq - ref_values_mod_4_85$surq),
                                                      latq_chg = (latq - ref_values_mod_4_85$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_4_85$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_4_85$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_4_85$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_4_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]




# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566


#### Model 6 ####


# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_6_rcp85_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_6_rcp85_wb) <- colnmss

mod_6_rcp85_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_6_rcp85_aqu) <- colnmsss

mod_6_rcp85_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_6_rcp85_reservoir) <- colnmssss

#mod_6_rcp85_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_6_RACMO22/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_6_rcp85_flocha) <- colnmsssss

#mod_6_rcp85_flocha <- mod_6_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 6, escenario = "RCP 8.5")


# Analysed variables --> Daily scale

mod_6_rcp85_wb <- mod_6_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_6_rcp85_basinaqu_wb <- mod_6_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_6_rcp85_reservoir <- mod_6_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_6_rcp85_wb_yr <- mod_6_rcp85_wb %>% left_join(., mod_6_rcp85_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_6_rcp85_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_6_rcp85_wb_yr <- mod_6_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_6_rcp85 <- mod_6_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_6_85 <- average_wb_periods_mod_6_rcp85[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_6_85_average_anomalies <- average_wb_periods_mod_6_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_6_85$precip) / ref_values_mod_6_85$precip,
                                                                        et_anom = (et - ref_values_mod_6_85$et)/ref_values_mod_6_85$et,
                                                                        pet_anom = (pet - ref_values_mod_6_85$pet)/ref_values_mod_6_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_6_85$perc)/ref_values_mod_6_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_6_85$rchrg)/ref_values_mod_6_85$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_6_85$total_flow)/ref_values_mod_6_85$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_6_85$surq)/ref_values_mod_6_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_6_85$latq)/ref_values_mod_6_85$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_6_85$gwflo_cha)/ref_values_mod_6_85$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_6_85$gwflo_res)/ref_values_mod_6_85$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_6_85$total_gwflo)/ref_values_mod_6_85$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_6_85$reser_floin_mm)/ref_values_mod_6_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_6_85_year_anomalies <-  mod_6_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_6_85$precip) / ref_values_mod_6_85$precip,
                                                         et_anom = (et - ref_values_mod_6_85$et)/ref_values_mod_6_85$et,
                                                         pet_anom = (pet - ref_values_mod_6_85$pet)/ref_values_mod_6_85$pet,
                                                         perc_anom = (perc - ref_values_mod_6_85$perc)/ref_values_mod_6_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_6_85$rchrg)/ref_values_mod_6_85$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_6_85$total_flow)/ref_values_mod_6_85$total_flow,
                                                         surq_anom = (surq - ref_values_mod_6_85$surq)/ref_values_mod_6_85$surq,
                                                         latq_anom = (latq - ref_values_mod_6_85$latq)/ref_values_mod_6_85$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_6_85$gwflo_cha)/ref_values_mod_6_85$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_6_85$gwflo_res)/ref_values_mod_6_85$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_6_85$total_gwflo)/ref_values_mod_6_85$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_6_85$reser_floin_mm)/ref_values_mod_6_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_6_85_average_changes <- average_wb_periods_mod_6_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_6_85$precip),
                                                                      et_chg = (et - ref_values_mod_6_85$et),
                                                                      pet_chg = (pet - ref_values_mod_6_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_6_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_6_85$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_6_85$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_6_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_6_85$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_6_85$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_6_85$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_6_85$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_6_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_6_85_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_6_85_year_changes <- mod_6_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_6_85$precip),
                                                      et_chg = (et - ref_values_mod_6_85$et),
                                                      pet_chg = (pet - ref_values_mod_6_85$pet),
                                                      perc_chg = (perc - ref_values_mod_6_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_6_85$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_6_85$total_flow),
                                                      surq_chg = (surq - ref_values_mod_6_85$surq),
                                                      latq_chg = (latq - ref_values_mod_6_85$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_6_85$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_6_85$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_6_85$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_6_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]


#### Model 7 ####
# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566

# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_7_rcp85_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_7_rcp85_wb) <- colnmss

mod_7_rcp85_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_7_rcp85_aqu) <- colnmsss

mod_7_rcp85_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_7_rcp85_reservoir) <- colnmssss

#mod_7_rcp85_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_7_DMI-HIRHAM/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_7_rcp85_flocha) <- colnmsssss

#mod_7_rcp85_flocha <- mod_7_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 7, escenario = "RCP 8.5")


# Analysed variables --> Daily scale

mod_7_rcp85_wb <- mod_7_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_7_rcp85_basinaqu_wb <- mod_7_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_7_rcp85_reservoir <- mod_7_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_7_rcp85_wb_yr <- mod_7_rcp85_wb %>% left_join(., mod_7_rcp85_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_7_rcp85_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_7_rcp85_wb_yr <- mod_7_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_7_rcp85 <- mod_7_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_7_85 <- average_wb_periods_mod_7_rcp85[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_7_85_average_anomalies <- average_wb_periods_mod_7_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_7_85$precip) / ref_values_mod_7_85$precip,
                                                                        et_anom = (et - ref_values_mod_7_85$et)/ref_values_mod_7_85$et,
                                                                        pet_anom = (pet - ref_values_mod_7_85$pet)/ref_values_mod_7_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_7_85$perc)/ref_values_mod_7_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_7_85$rchrg)/ref_values_mod_7_85$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_7_85$total_flow)/ref_values_mod_7_85$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_7_85$surq)/ref_values_mod_7_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_7_85$latq)/ref_values_mod_7_85$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_7_85$gwflo_cha)/ref_values_mod_7_85$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_7_85$gwflo_res)/ref_values_mod_7_85$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_7_85$total_gwflo)/ref_values_mod_7_85$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_7_85$reser_floin_mm)/ref_values_mod_7_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_7_85_year_anomalies <-  mod_7_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_7_85$precip) / ref_values_mod_7_85$precip,
                                                         et_anom = (et - ref_values_mod_7_85$et)/ref_values_mod_7_85$et,
                                                         pet_anom = (pet - ref_values_mod_7_85$pet)/ref_values_mod_7_85$pet,
                                                         perc_anom = (perc - ref_values_mod_7_85$perc)/ref_values_mod_7_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_7_85$rchrg)/ref_values_mod_7_85$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_7_85$total_flow)/ref_values_mod_7_85$total_flow,
                                                         surq_anom = (surq - ref_values_mod_7_85$surq)/ref_values_mod_7_85$surq,
                                                         latq_anom = (latq - ref_values_mod_7_85$latq)/ref_values_mod_7_85$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_7_85$gwflo_cha)/ref_values_mod_7_85$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_7_85$gwflo_res)/ref_values_mod_7_85$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_7_85$total_gwflo)/ref_values_mod_7_85$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_7_85$reser_floin_mm)/ref_values_mod_7_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_7_85_average_changes <- average_wb_periods_mod_7_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_7_85$precip),
                                                                      et_chg = (et - ref_values_mod_7_85$et),
                                                                      pet_chg = (pet - ref_values_mod_7_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_7_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_7_85$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_7_85$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_7_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_7_85$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_7_85$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_7_85$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_7_85$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_7_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_7_85_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_7_85_year_changes <- mod_7_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_7_85$precip),
                                                      et_chg = (et - ref_values_mod_7_85$et),
                                                      pet_chg = (pet - ref_values_mod_7_85$pet),
                                                      perc_chg = (perc - ref_values_mod_7_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_7_85$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_7_85$total_flow),
                                                      surq_chg = (surq - ref_values_mod_7_85$surq),
                                                      latq_chg = (latq - ref_values_mod_7_85$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_7_85$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_7_85$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_7_85$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_7_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]




#### Model 9 ####

# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566

# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_9_rcp85_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_9_rcp85_wb) <- colnmss

mod_9_rcp85_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_9_rcp85_aqu) <- colnmsss

mod_9_rcp85_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_9_rcp85_reservoir) <- colnmssss

#mod_9_rcp85_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_9_MPI-M-MPI/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_9_rcp85_flocha) <- colnmsssss

#mod_9_rcp85_flocha <- mod_9_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 9, escenario = "RCP 8.5")


# Analysed variables --> Daily scale

mod_9_rcp85_wb <- mod_9_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, 
                                             surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_9_rcp85_basinaqu_wb <- mod_9_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_9_rcp85_reservoir <- mod_9_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_9_rcp85_wb_yr <- mod_9_rcp85_wb %>% left_join(., mod_9_rcp85_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_9_rcp85_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_9_rcp85_wb_yr <- mod_9_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_9_rcp85 <- mod_9_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_9_85 <- average_wb_periods_mod_9_rcp85[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_9_85_average_anomalies <- average_wb_periods_mod_9_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_9_85$precip) / ref_values_mod_9_85$precip,
                                                                        et_anom = (et - ref_values_mod_9_85$et)/ref_values_mod_9_85$et,
                                                                        pet_anom = (pet - ref_values_mod_9_85$pet)/ref_values_mod_9_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_9_85$perc)/ref_values_mod_9_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_9_85$rchrg)/ref_values_mod_9_85$rchrg,
                                                                        total_flow_anom = (total_flow  - ref_values_mod_9_85$total_flow)/ref_values_mod_9_85$total_flow,
                                                                        surq_anom = (surq - ref_values_mod_9_85$surq)/ref_values_mod_9_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_9_85$latq)/ref_values_mod_9_85$latq,
                                                                        gwflocha_anom = (gwflo_cha - ref_values_mod_9_85$gwflo_cha)/ref_values_mod_9_85$gwflo_cha, 
                                                                        gwflores_anom = (gwflo_res - ref_values_mod_9_85$gwflo_res)/ref_values_mod_9_85$gwflo_res,
                                                                        totalgwflo_anom = (total_gwflo - ref_values_mod_9_85$total_gwflo)/ref_values_mod_9_85$total_gwflo,
                                                                        res_floin_anom = (reser_floin_mm - ref_values_mod_9_85$reser_floin_mm)/ref_values_mod_9_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_9_85_year_anomalies <-  mod_9_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_9_85$precip) / ref_values_mod_9_85$precip,
                                                         et_anom = (et - ref_values_mod_9_85$et)/ref_values_mod_9_85$et,
                                                         pet_anom = (pet - ref_values_mod_9_85$pet)/ref_values_mod_9_85$pet,
                                                         perc_anom = (perc - ref_values_mod_9_85$perc)/ref_values_mod_9_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_9_85$rchrg)/ref_values_mod_9_85$rchrg,
                                                         total_flow_anom = (total_flow  - ref_values_mod_9_85$total_flow)/ref_values_mod_9_85$total_flow,
                                                         surq_anom = (surq - ref_values_mod_9_85$surq)/ref_values_mod_9_85$surq,
                                                         latq_anom = (latq - ref_values_mod_9_85$latq)/ref_values_mod_9_85$latq,
                                                         gwflocha_anom = (gwflo_cha - ref_values_mod_9_85$gwflo_cha)/ref_values_mod_9_85$gwflo_cha, 
                                                         gwflores_anom = (gwflo_res - ref_values_mod_9_85$gwflo_res)/ref_values_mod_9_85$gwflo_res,
                                                         totalgwflo_anom = (total_gwflo - ref_values_mod_9_85$total_gwflo)/ref_values_mod_9_85$total_gwflo,
                                                         res_floin_anom = (reser_floin_mm - ref_values_mod_9_85$reser_floin_mm)/ref_values_mod_9_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_9_85_average_changes <- average_wb_periods_mod_9_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_9_85$precip),
                                                                      et_chg = (et - ref_values_mod_9_85$et),
                                                                      pet_chg = (pet - ref_values_mod_9_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_9_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_9_85$rchrg),
                                                                      totalflo_chg = (total_flow - ref_values_mod_9_85$total_flow),
                                                                      surq_chg = (surq - ref_values_mod_9_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_9_85$latq),
                                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_9_85$gwflo_cha),
                                                                      gwflores_chg = (gwflo_res - ref_values_mod_9_85$gwflo_res),
                                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_9_85$total_gwflo),
                                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_9_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_9_85_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_9_85_year_changes <- mod_9_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_9_85$precip),
                                                      et_chg = (et - ref_values_mod_9_85$et),
                                                      pet_chg = (pet - ref_values_mod_9_85$pet),
                                                      perc_chg = (perc - ref_values_mod_9_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_9_85$rchrg),
                                                      totalflo_chg = (total_flow - ref_values_mod_9_85$total_flow),
                                                      surq_chg = (surq - ref_values_mod_9_85$surq),
                                                      latq_chg = (latq - ref_values_mod_9_85$latq),
                                                      gwflocha_chg = (gwflo_cha - ref_values_mod_9_85$gwflo_cha),
                                                      gwflores_chg = (gwflo_res - ref_values_mod_9_85$gwflo_res),
                                                      totalgwflo_chg = (total_gwflo - ref_values_mod_9_85$total_gwflo),
                                                      res_floin_chg = (reser_floin_mm - ref_values_mod_9_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]






#### Model 14 ####


# Periods definition  

#rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


area_dren <- 367943566
# 1. Data extraction and manipulation


# Analysed files --> basin_wb and aquifer_basin, reservoir and channel
mod_14_rcp85_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/basin_wb_day.txt", skip = 3)
colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_14_rcp85_wb) <- colnmss

mod_14_rcp85_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/basin_aqu_day.txt", skip = 3)
colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_14_rcp85_aqu) <- colnmsss

mod_14_rcp85_reservoir <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/reservoir_day.txt", skip = 3)
colnmssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/reservoir_day.txt", skip = 1, col_names = F) %>% .[1,]
colnames(mod_14_rcp85_reservoir) <- colnmssss

#mod_14_rcp85_flocha <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/channel_sd_day.txt", skip = 3)
#colnmsssss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_85/TxtInOut_14_MOHC-HadGEM2/channel_sd_day.txt", skip = 1, col_names = F) %>% .[1,]
#colnames(mod_14_rcp85_flocha) <- colnmsssss

#mod_14_rcp85_flocha <- mod_14_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 14, escenario = "RCP 8.5")


# Analysed variables --> Daily scale

mod_14_rcp85_wb <- mod_14_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                               latq_cha, latq_res, 
                                               surq_gen, surq_ls) %>% 
  mutate(surq = surq_gen - surq_ls,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_14_rcp85_basinaqu_wb <- mod_14_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                        seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         total_gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor ,  revap, dep_wt, total_gwflo, flo_cha, flo_res) %>% 
  rename(gwflo_cha = flo_cha, gwflo_res = flo_res)


mod_14_rcp85_reservoir <- mod_14_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
  mutate(reser_floin_mm = (flo_in * 1000) / area_dren )


# Analysed variables --> Aggregation to monthly/yearly values

mod_14_rcp85_wb_yr <- mod_14_rcp85_wb %>% left_join(., mod_14_rcp85_basinaqu_wb, "date") %>% 
  mutate(total_flow = surq + latq + total_gwflo) %>% 
  left_join(., mod_14_rcp85_reservoir, "date") %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~ sum(.))


# Introduction of periods

mod_14_rcp85_wb_yr <- mod_14_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                        year %in% midterm_period ~ "midterm", 
                                                                        year %in% longterm_period ~ "longterm", 
                                                                        .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))


# Average water balance for each period

average_wb_periods_mod_14_rcp85 <- mod_14_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", 
                    "rchrg", "total_flow", "surq",  "latq", 
                    "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) 

ref_values_mod_14_85 <- average_wb_periods_mod_14_rcp85[1,]



# 2. Analysis of temporal effects: Anomalies and changes, average and annual basis


# 2.1 Anomalies of each model regarding its baseline (%): 

# Anomalies in average

mod_14_85_average_anomalies <- average_wb_periods_mod_14_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_14_85$precip) / ref_values_mod_14_85$precip,
                                                                          et_anom = (et - ref_values_mod_14_85$et)/ref_values_mod_14_85$et,
                                                                          pet_anom = (pet - ref_values_mod_14_85$pet)/ref_values_mod_14_85$pet,
                                                                          perc_anom = (perc - ref_values_mod_14_85$perc)/ref_values_mod_14_85$perc,
                                                                          rchrg_anom = (rchrg - ref_values_mod_14_85$rchrg)/ref_values_mod_14_85$rchrg,
                                                                          total_flow_anom = (total_flow  - ref_values_mod_14_85$total_flow)/ref_values_mod_14_85$total_flow,
                                                                          surq_anom = (surq - ref_values_mod_14_85$surq)/ref_values_mod_14_85$surq,
                                                                          latq_anom = (latq - ref_values_mod_14_85$latq)/ref_values_mod_14_85$latq,
                                                                          gwflocha_anom = (gwflo_cha - ref_values_mod_14_85$gwflo_cha)/ref_values_mod_14_85$gwflo_cha, 
                                                                          gwflores_anom = (gwflo_res - ref_values_mod_14_85$gwflo_res)/ref_values_mod_14_85$gwflo_res,
                                                                          totalgwflo_anom = (total_gwflo - ref_values_mod_14_85$total_gwflo)/ref_values_mod_14_85$total_gwflo,
                                                                          res_floin_anom = (reser_floin_mm - ref_values_mod_14_85$reser_floin_mm)/ref_values_mod_14_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")%>% .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


# Annual anomalies 

mod_14_85_year_anomalies <-  mod_14_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_14_85$precip) / ref_values_mod_14_85$precip,
                                                           et_anom = (et - ref_values_mod_14_85$et)/ref_values_mod_14_85$et,
                                                           pet_anom = (pet - ref_values_mod_14_85$pet)/ref_values_mod_14_85$pet,
                                                           perc_anom = (perc - ref_values_mod_14_85$perc)/ref_values_mod_14_85$perc,
                                                           rchrg_anom = (rchrg - ref_values_mod_14_85$rchrg)/ref_values_mod_14_85$rchrg,
                                                           total_flow_anom = (total_flow  - ref_values_mod_14_85$total_flow)/ref_values_mod_14_85$total_flow,
                                                           surq_anom = (surq - ref_values_mod_14_85$surq)/ref_values_mod_14_85$surq,
                                                           latq_anom = (latq - ref_values_mod_14_85$latq)/ref_values_mod_14_85$latq,
                                                           gwflocha_anom = (gwflo_cha - ref_values_mod_14_85$gwflo_cha)/ref_values_mod_14_85$gwflo_cha, 
                                                           gwflores_anom = (gwflo_res - ref_values_mod_14_85$gwflo_res)/ref_values_mod_14_85$gwflo_res,
                                                           totalgwflo_anom = (total_gwflo - ref_values_mod_14_85$total_gwflo)/ref_values_mod_14_85$total_gwflo,
                                                           res_floin_anom = (reser_floin_mm - ref_values_mod_14_85$reser_floin_mm)/ref_values_mod_14_85$reser_floin_mm) %>%
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")%>% .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]





# 2.2 Changes of each model regarding its baseline (Absolute):


# Changes in average
mod_14_85_average_changes <- average_wb_periods_mod_14_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_14_85$precip),
                                                                        et_chg = (et - ref_values_mod_14_85$et),
                                                                        pet_chg = (pet - ref_values_mod_14_85$pet),
                                                                        perc_chg = (perc - ref_values_mod_14_85$perc),
                                                                        rchrg_chg = (rchrg - ref_values_mod_14_85$rchrg),
                                                                        totalflo_chg = (total_flow - ref_values_mod_14_85$total_flow),
                                                                        surq_chg = (surq - ref_values_mod_14_85$surq),
                                                                        latq_chg = (latq - ref_values_mod_14_85$latq),
                                                                        gwflocha_chg = (gwflo_cha - ref_values_mod_14_85$gwflo_cha),
                                                                        gwflores_chg = (gwflo_res - ref_values_mod_14_85$gwflo_res),
                                                                        totalgwflo_chg = (total_gwflo - ref_values_mod_14_85$total_gwflo),
                                                                        res_floin_chg = (reser_floin_mm - ref_values_mod_14_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")%>% 
  .[,c(27,26,1,2,14, 3, 15, 4 ,16, 5 ,17, 6, 18, 7, 19, 8, 20 ,9, 21, 10, 22, 11, 23, 12, 24, 13, 25)]


mod_14_85_average_changes %>%  select(ends_with("chg"))


# Annual changes 


mod_14_85_year_changes <- mod_14_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_14_85$precip),
                                                        et_chg = (et - ref_values_mod_14_85$et),
                                                        pet_chg = (pet - ref_values_mod_14_85$pet),
                                                        perc_chg = (perc - ref_values_mod_14_85$perc),
                                                        rchrg_chg = (rchrg - ref_values_mod_14_85$rchrg),
                                                        totalflo_chg = (total_flow - ref_values_mod_14_85$total_flow),
                                                        surq_chg = (surq - ref_values_mod_14_85$surq),
                                                        latq_chg = (latq - ref_values_mod_14_85$latq),
                                                        gwflocha_chg = (gwflo_cha - ref_values_mod_14_85$gwflo_cha),
                                                        gwflores_chg = (gwflo_res - ref_values_mod_14_85$gwflo_res),
                                                        totalgwflo_chg = (total_gwflo - ref_values_mod_14_85$total_gwflo),
                                                        res_floin_chg = (reser_floin_mm - ref_values_mod_14_85$reser_floin_mm)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")%>% 
  .[,c(28, 27,1, 2, 15, 3, 16, 4, 17, 5, 18, 6, 19, 7, 20, 8, 21, 9, 22, 10, 23, 11, 24, 12, 25, 13, 26)]




#### B  Merging results ####



#### Absolute values merge ####

absolute_values_escenarios_mod4_esc45 <- average_wb_periods_mod_4_rcp45 %>% mutate(scenario = "RCP 4.5", model = "mod_4")
absolute_values_escenarios_mod6_esc45 <- average_wb_periods_mod_6_rcp45%>% mutate(scenario = "RCP 4.5", model = "mod_6")
absolute_values_escenarios_mod7_esc45 <- average_wb_periods_mod_7_rcp45%>% mutate(scenario = "RCP 4.5", model = "mod_7")
absolute_values_escenarios_mod9_esc45 <- average_wb_periods_mod_9_rcp45%>% mutate(scenario = "RCP 4.5", model = "mod_9")
absolute_values_escenarios_mod14_esc45 <- average_wb_periods_mod_14_rcp45 %>% mutate(scenario = "RCP 4.5", model = "mod_14")
absolute_values_escenarios_mod4_esc85 <- average_wb_periods_mod_4_rcp85%>% mutate(scenario = "RCP 8.5", model = "mod_4")
absolute_values_escenarios_mod6_esc85 <- average_wb_periods_mod_6_rcp85%>% mutate(scenario = "RCP 8.5", model = "mod_6")
absolute_values_escenarios_mod7_esc85 <- average_wb_periods_mod_7_rcp85%>% mutate(scenario = "RCP 8.5", model = "mod_7")
absolute_values_escenarios_mod9_esc85 <- average_wb_periods_mod_9_rcp85%>% mutate(scenario = "RCP 8.5", model = "mod_9")
absolute_values_escenarios_mod14_esc85 <- average_wb_periods_mod_14_rcp85 %>% mutate(scenario = "RCP 8.5", model = "mod_14")


absolute_values_allmodels <- 
  absolute_values_escenarios_mod4_esc45 %>% 
  rbind(., absolute_values_escenarios_mod6_esc45 ) %>%  
  rbind(., absolute_values_escenarios_mod7_esc45 ) %>%  
  rbind(., absolute_values_escenarios_mod9_esc45 ) %>%  
  rbind(., absolute_values_escenarios_mod14_esc45) %>% 
  rbind(., absolute_values_escenarios_mod4_esc85 ) %>%  
  rbind(., absolute_values_escenarios_mod6_esc85 ) %>%  
  rbind(., absolute_values_escenarios_mod7_esc85 ) %>%  
  rbind(., absolute_values_escenarios_mod9_esc85 ) %>%  
  rbind(., absolute_values_escenarios_mod14_esc85)


# write.table(absolute_average_values_allmodels, "C:/ASG/R_Projects/climat_change_guajaraz/data/absolute_values_allmodels.txt", row.names = F, quote = F)

absolute_values_allmodels%>% group_by(scenario, periodo) %>% 
  summarise_at(c("precip", "et", "pet", "perc", "rchrg", "total_flow", "surq",  "latq", 
                 "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.)) %>% 
  write.table(., "C:/ASG/R_Projects/climat_change_guajaraz/data/average_absolute_values.txt", row.names = F, quote = F)


#### Reference values #### 

ref_values_mod_4_45_n  <- ref_values_mod_4_45  %>% mutate(scenario = "RCP 4.5")
ref_values_mod_6_45_n  <- ref_values_mod_6_45 %>% mutate(scenario = "RCP 4.5")
ref_values_mod_7_45_n  <- ref_values_mod_7_45 %>% mutate(scenario = "RCP 4.5")
ref_values_mod_9_45_n  <- ref_values_mod_9_45 %>% mutate(scenario = "RCP 4.5")
ref_values_mod_14_45_n <- ref_values_mod_14_45 %>% mutate(scenario = "RCP 4.5")
ref_values_mod_4_85_n  <- ref_values_mod_4_85 %>% mutate(scenario = "RCP 8.5")
ref_values_mod_6_85_n  <- ref_values_mod_6_85 %>% mutate(scenario = "RCP 8.5")
ref_values_mod_7_85_n  <- ref_values_mod_7_85 %>% mutate(scenario = "RCP 8.5")
ref_values_mod_9_85_n  <- ref_values_mod_9_85 %>% mutate(scenario = "RCP 8.5")
ref_values_mod_14_85_n <- ref_values_mod_14_85 %>% mutate(scenario = "RCP 8.5")


reference_values <- 
  ref_values_mod_4_45_n %>% 
  rbind(., ref_values_mod_6_45_n) %>%  
  rbind(., ref_values_mod_7_45_n) %>%  
  rbind(., ref_values_mod_9_45_n) %>%  
  rbind(., ref_values_mod_14_45_n) %>% 
  rbind(., ref_values_mod_4_85_n) %>%  
  rbind(., ref_values_mod_6_85_n) %>%  
  rbind(., ref_values_mod_7_85_n) %>%  
  rbind(., ref_values_mod_9_85_n) %>%  
  rbind(., ref_values_mod_14_85_n)


reference_values_escenarios <- reference_values %>% group_by(scenario) %>% 
  summarise_at(c("precip", "et", "pet", "perc", "rchrg", "total_flow", "surq",  "latq", 
                 "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), ~mean(.))



#### Average changes #### 

average_changes <- mod_4_45_average_changes %>% 
  rbind(., mod_6_45_average_changes) %>%  
  rbind(., mod_7_45_average_changes) %>%  
  rbind(., mod_9_45_average_changes) %>%  
  rbind(., mod_14_45_average_changes) %>% 
  rbind(., mod_4_85_average_changes) %>%  
  rbind(., mod_6_85_average_changes) %>%  
  rbind(., mod_7_85_average_changes) %>%  
  rbind(., mod_9_85_average_changes) %>%  
  rbind(., mod_14_85_average_changes)

# average_changes %>% write.table("C:/ASG/R_Projects/climat_change_guajaraz/data/changes_average_allmodels.txt", quote = F, row.names = F)

average_changes %>% #filter(., model != 14) %>% 
  group_by(escenario, periodo) %>% 
  summarise(across(ends_with("chg"), ~mean(.))) %>% 
  group_by(escenario, periodo) %>% 
  summarise(across(ends_with("chg"), ~round(., 3))) %>% 
  filter(., periodo != "other") #%>% write.table("C:/ASG/R_Projects/climat_change_guajaraz/data/average_changes.txt", quote = F, row.names = F)



#### Average anomalies ####


average_anomalies <- mod_4_45_average_anomalies %>% 
  rbind(., mod_6_45_average_anomalies) %>%  
  rbind(., mod_7_45_average_anomalies) %>%  
  rbind(., mod_9_45_average_anomalies) %>%  
  rbind(., mod_14_45_average_anomalies) %>% 
  rbind(., mod_4_85_average_anomalies) %>%  
  rbind(., mod_6_85_average_anomalies) %>%  
  rbind(., mod_7_85_average_anomalies) %>%  
  rbind(., mod_9_85_average_anomalies) %>%  
  rbind(., mod_14_85_average_anomalies)

# write.table(average_anomalies, "C:/ASG/R_Projects/climat_change_guajaraz/data/anomalies_allmodels.txt", quote = F, row.names = F)


average_anomalies %>% #filter(., model != 14) %>% 
  group_by(escenario, periodo) %>% 
  summarise(across(ends_with("_anom"), ~mean(.))) %>% 
  filter(., periodo != "other")# %>% write.table("C:/ASG/R_Projects/climat_change_guajaraz/data/average_anomalies.txt", quote = F, row.names = F)




#### Annual results for plots ####


# Anomalies

year_anomalies <- mod_4_45_year_anomalies %>% 
  rbind(., mod_6_45_year_anomalies) %>%  
  rbind(., mod_7_45_year_anomalies) %>%  
  rbind(., mod_9_45_year_anomalies) %>%  
  rbind(., mod_14_45_year_anomalies) %>% 
  rbind(., mod_4_85_year_anomalies) %>%  
  rbind(., mod_6_85_year_anomalies) %>%  
  rbind(., mod_7_85_year_anomalies) %>%  
  rbind(., mod_9_85_year_anomalies) %>%  
  rbind(., mod_14_85_year_anomalies)

# Changes

year_changes <- mod_4_45_year_changes %>% 
  rbind(., mod_6_45_year_changes) %>%  
  rbind(., mod_7_45_year_changes) %>%  
  rbind(., mod_9_45_year_changes) %>%  
  rbind(., mod_14_45_year_changes) %>% 
  rbind(., mod_4_85_year_changes) %>%  
  rbind(., mod_6_85_year_changes) %>%  
  rbind(., mod_7_85_year_changes) %>%  
  rbind(., mod_9_85_year_changes) %>%  
  rbind(., mod_14_85_year_changes)


#### PLOTS ####




#### YEAR ANOMALIES PLOT ####

min_anomaly <- year_anomalies %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_anom", "et_anom", "pet_anom", "perc_anom",
                    "rchrg_anom", "total_flow_anom", "surq_anom", 
                    "latq_anom", "gwflocha_anom", "gwflores_anom", "totalgwflo_anom", "res_floin_anom"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_anomaly <- year_anomalies %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_anom", "et_anom", "pet_anom", "perc_anom",
                    "rchrg_anom", "total_flow_anom", "surq_anom", 
                    "latq_anom", "gwflocha_anom", "gwflores_anom", "totalgwflo_anom", "res_floin_anom"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_anomaly <- year_anomalies %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_anom", "et_anom", "pet_anom", "perc_anom",
                    "rchrg_anom", "total_flow_anom", "surq_anom", 
                    "latq_anom", "gwflocha_anom", "gwflores_anom", "totalgwflo_anom", "res_floin_anom"), ~ mean(.))%>% 
  mutate(statis = "mean")


wider_anomalies <- min_anomaly %>% rbind(., max_anomaly) %>% rbind(., mean_anomaly) %>% 
  pivot_wider(., names_from = statis, values_from = c(ends_with("anom")))



vars <- colnames(wider_anomalies) %>% tibble() %>% mutate(vars =str_remove(., "_anom_")) %>% 
  mutate(vars = str_remove(vars, "maximum")) %>% 
  mutate(vars = str_remove(vars, "minimum")) %>% 
  mutate(vars = str_remove(vars, "mean"))

vars <- unique(vars$vars) %>% .[-c(1,2)]




for(i in 1:length(vars)){
  
  var <- c("Precipitation", "Evapotranspiration", "Potential evapotranspiration", "Percolation",
           "Recharge", "Total flow", "Surface runoff", "Lateral flow", "Groundwater flow to channel",
           "Groundwater flow to reservoir", "Total groundwater flow", "Reservoir influent")
  
  var_tab <- wider_anomalies %>% select(., c(year, escenario,starts_with(vars[i]))) 
  colnames(var_tab) <- c("year","escenario", "minimum", "maximum", "mean")
  
  
  plot <- var_tab %>% ggplot(., aes(x = year))+
    geom_ribbon(aes(ymin = minimum, ymax = maximum),fill = "mediumseagreen", alpha = 0.5)+
    geom_line(aes(y = mean), color = "springgreen4", linewidth = 0.8)+
    theme_bw()+
    geom_line(aes(y = 0), linetype = 2, size = 1)+
    labs(y = var[i])+
    facet_wrap(facets = "escenario")+
    ggtitle("Anomaly regarding baseline period for each model")+
    theme(text = element_text(size = 15), axis.title.x = element_blank())
  
  
 #ggsave(plot = plot, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Anomalies/anomalies_yr/", i, "_", vars[i], ".tiff", sep = ""),
 #       device = "tiff", dpi = 600, width = 12, height = 10)
  
  
}

# Boxplots anomalies in periods

for(i in 1:length(vars)){
  
  var <- c("Precipitation", "Evapotranspiration", "Potential evapotranspiration", "Percolation",
           "Recharge", "Total flow", "Surface runoff", "Lateral flow", "Groundwater flow to channel",
           "Groundwater flow to reservoir", "Total groundwater flow", "Reservoir influent")
  
  var_tab <- year_anomalies %>% select(., c(year, escenario,model, starts_with(vars[i]) )) %>% 
    select(., year, escenario,model,ends_with("_anom")) %>% 
    mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
    #filter(., periodo != "other") 
    filter(., periodo %in% c("Mid term (2046-2065)", "Long term (2080-2099)")) 
  
  colnames(var_tab) <- c("year","escenario", "model", "var", "periodo")
  
  
  
  boxplots <- var_tab %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var, fill = model ))+
    theme_bw()+
    labs(y = var[i], fill = "Model", x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario", scales = "free")+
    scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average annual anomalies distribution: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Anomalies/boxplot_anomalies/", i, "_boxplot_", vars[i], ".jpg", sep = ""),
         device = "jpg", dpi = 600, width = 14, height = 10)
  
}



# Boxplots resume for all variables


varsnames <- list(  "pcp_anom"       =  "Precipitation",        
                     "et_anom"        ="Evapotranspiration",            
                     "pet_anom"       = "Potential evapotranspiration",           
                     "perc_anom"      =  "Percolation",          
                     "rchrg_anom"     =  "Recharge",         
                     "total_flow_anom"  = "Total flow",    
                     "surq_anom"      =  "Surface runoff",          
                     "latq_anom"      =  "Lateral flow",          
                     "gwflocha_anom"  =  "Groundwater flow to channel",     
                     "gwflores_anom"  = "Groundwater flow to reservoir",     
                     "totalgwflo_anom"= "Total groundwater flow",  
                     "res_floin_anom" = "Reservoir inflow")


variable_labeller <- function(variable,value){
  return(varsnames[value])
}



longer_anom_ind <- year_anomalies %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  select(., year,escenario, model, periodo, ends_with("anom")) %>% 
  pivot_longer(., -c("year","periodo", "escenario", "model" ), names_to = "variable")


longer_anom_ind$variable <- factor(longer_anom_ind$variable, levels = c("pcp_anom","et_anom","pet_anom","perc_anom","rchrg_anom",
                                                                              "total_flow_anom","surq_anom","latq_anom","gwflocha_anom"  ,
                                                                              "gwflores_anom","totalgwflo_anom","res_floin_anom"))

longer_anom_ind$periodo <- factor(longer_anom_ind$periodo, 
                                     levels = c("baseline", "midterm", "longterm", "other"), 
                                     labels = c("Baseline", "Midterm", "Longterm", "other"))






boxplots_absvals_allvars <- longer_anom_ind %>%
  filter(., periodo %in% c("Midterm", "Longterm")) %>% 
  # filter(periodo != "other") %>% 
  ggplot(., aes(x= escenario, y = value))+
  geom_boxplot(aes(fill = periodo))+
  scale_fill_manual( values = c("darkolivegreen1", "darkolivegreen4", "darkolivegreen") )+
  
  facet_wrap(facets = "variable", scales = "free", labeller = variable_labeller)+
  labs(fill = "Period", x = "Climate change scenario", y = "")+
  theme_bw()+theme(text = element_text(size = 15))+
  ggtitle("Relative changes")



ggsave(plot = boxplots_absvals_allvars, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Anomalies/boxplot_anomalies/", "allvars_boxplot", ".jpg", sep = ""),
       device = "jpg", dpi = 600, width = 18, height = 12)






#### YEAR CHANGES PLOT ####


year_changes <- mod_4_45_year_changes %>% 
  rbind(., mod_6_45_year_changes) %>%  
  rbind(., mod_7_45_year_changes) %>%  
  rbind(., mod_9_45_year_changes) %>%  
  rbind(., mod_14_45_year_changes) %>% 
  rbind(., mod_4_85_year_changes) %>%  
  rbind(., mod_6_85_year_changes) %>%  
  rbind(., mod_7_85_year_changes) %>%  
  rbind(., mod_9_85_year_changes) %>%  
  rbind(., mod_14_85_year_changes)



min_chg <- year_changes %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_chg", "et_chg", "pet_chg", "perc_chg",
                    "rchrg_chg", "totalflo_chg", "surq_chg", 
                    "latq_chg", "gwflocha_chg", "gwflores_chg", "totalgwflo_chg", "res_floin_chg"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_chg <- year_changes %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_chg", "et_chg", "pet_chg", "perc_chg",
                    "rchrg_chg", "totalflo_chg", "surq_chg", 
                    "latq_chg", "gwflocha_chg", "gwflores_chg", "totalgwflo_chg", "res_floin_chg"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_chg <- year_changes %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_chg", "et_chg", "pet_chg", "perc_chg",
                    "rchrg_chg", "totalflo_chg", "surq_chg", 
                    "latq_chg", "gwflocha_chg", "gwflores_chg", "totalgwflo_chg", "res_floin_chg"), ~ mean(.))%>% 
  mutate(statis = "mean")


wider_changes <- min_chg %>% rbind(., max_chg) %>% rbind(., mean_chg) %>% 
  pivot_wider(., names_from = statis, values_from = c(ends_with("chg")))



vars <- colnames(wider_changes) %>% tibble() %>% mutate(vars =str_remove(., "_chg_")) %>% 
  mutate(vars = str_remove(vars, "maximum")) %>% 
  mutate(vars = str_remove(vars, "minimum")) %>% 
  mutate(vars = str_remove(vars, "mean"))

vars <- unique(vars$vars) %>% .[-c(1,2)]


for(i in 1:length(vars)){
  
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  
  var_tab <- wider_changes %>% select(., c(year, escenario,starts_with(vars[i]))) 
  colnames(var_tab) <- c("year","escenario", "minimum", "maximum", "mean")
  
  
  plot <- var_tab %>% ggplot(., aes(x = year))+
    geom_ribbon(aes(ymin = minimum, ymax = maximum),fill = "skyblue", alpha = 0.5)+
    geom_line(aes(y = mean), color = "slateblue4", linewidth = 0.8)+
    theme_bw()+
    labs(y = var[i])+
    facet_wrap(facets = "escenario")+
    ggtitle("Absolute change regarding baseline period average (2006-2019)")+
    theme(text = element_text(size = 15), axis.title.x = element_blank())
  
  ggsave(plot = plot, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Changes/absolute_change/", i, "_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 12, height = 10)
  
  
}



# Boxplots changes in periods

for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  var_tab <- year_changes %>% select(., c(year, escenario,model, starts_with(vars[i]) )) %>% 
    select(., year, escenario,model,ends_with("_chg")) %>% 
    mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
    filter(., periodo %in% c("Mid term (2046-2065)", "Long term (2080-2099)")) 
  #  filter(., periodo != "other") 
  
  colnames(var_tab) <- c("year","escenario", "model", "var", "periodo")
  
  
  
  boxplots <- var_tab %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var,))+ # color = model))+
    theme_bw()+
    labs(y = var[i], fill = "Model", x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario", scales = "free")+
    #scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average annual change distribution: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Changes/boxplots_all/absolute_change_boxplot_", i, "_", vars[i], ".jpg", sep = ""),
         device = "jpg", dpi = 600, width = 14, height = 10)
  
}




# Boxplots resume for all variables


varsnames <- list( "pcp_chg"       =  "Precipitation",        
                   "et_chg"        ="Evapotranspiration",            
                   "pet_chg"       = "Potential evapotranspiration",           
                   "perc_chg"      =  "Percolation",          
                   "rchrg_chg"     =  "Recharge",         
                   "totalflo_chg"  = "Total flow",    
                   "surq_chg"      =  "Surface runoff",          
                   "latq_chg"      =  "Lateral flow",          
                   "gwflocha_chg"  =  "Groundwater flow to channel",     
                   "gwflores_chg"  = "Groundwater flow to reservoir",     
                   "totalgwflo_chg"= "Total groundwater flow",  
                   "res_floin_chg" = "Reservoir inflow")



variable_labeller <- function(variable,value){
  return(varsnames[value])
}



longer_changes_ind <- year_changes %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  select(., year,escenario, model, periodo, ends_with("chg")) %>% 
  pivot_longer(., -c("year","periodo", "escenario", "model" ), names_to = "variable")


longer_changes_ind$variable <- factor(longer_changes_ind$variable, levels = c("pcp_chg","et_chg","pet_chg","perc_chg","rchrg_chg",
                                                                            "totalflo_chg","surq_chg","latq_chg","gwflocha_chg"  ,
                                                                            "gwflores_chg","totalgwflo_chg","res_floin_chg"))

longer_changes_ind$periodo <- factor(longer_changes_ind$periodo, 
                                     levels = c("baseline", "midterm", "longterm", "other"), 
                                     labels = c("Baseline", "Midterm", "Longterm", "other"))



boxplots_abschange_allvars <- longer_changes_ind %>% 
  filter(periodo %in% "Midterm", "Longterm") %>% 
  #filter(periodo != "other") %>% 
  ggplot(., aes(x= escenario, y = value))+
  geom_boxplot(aes(fill = periodo))+
  scale_fill_manual( values = c("darkolivegreen1", "darkolivegreen4", "darkolivegreen") )+
  
  facet_wrap(facets = "variable", scales = "free", labeller = variable_labeller)+
  labs(fill = "Period", x = "Climate change scenario", y = "")+
  theme_bw()+theme(text = element_text(size = 15))+
  ggtitle("Absolute change (mm)")



ggsave(plot = boxplots_abschange_allvars, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Changes/boxplots/", "allvars_abschg_boxplot", ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 18, height = 12)






#### YEAR VALUES PLOT ####

mod_4_rcp45_wb_yr_n <- mod_4_rcp45_wb_yr    %>% cbind(escenario = "RCP 4.5", model = "mod_4")
mod_6_rcp45_wb_yr_n <- mod_6_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_6")
mod_7_rcp45_wb_yr_n <- mod_7_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_7")
mod_9_rcp45_wb_yr_n <- mod_9_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_9")
mod_14_rcp45_wb_yr_n <- mod_14_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_14")
mod_4_rcp85_wb_yr_n <- mod_4_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_4")
mod_6_rcp85_wb_yr_n <- mod_6_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_6")
mod_7_rcp85_wb_yr_n <- mod_7_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_7")
mod_9_rcp85_wb_yr_n <- mod_9_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_9")
mod_14_rcp85_wb_yr_n <- mod_14_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_14")


year_values <- mod_4_rcp45_wb_yr_n %>% 
  rbind(., mod_6_rcp45_wb_yr_n) %>%  
  rbind(., mod_7_rcp45_wb_yr_n) %>%  
  rbind(., mod_9_rcp45_wb_yr_n) %>%  
  rbind(., mod_14_rcp45_wb_yr_n) %>% 
  rbind(., mod_4_rcp85_wb_yr_n) %>%  
  rbind(., mod_6_rcp85_wb_yr_n) %>%  
  rbind(., mod_7_rcp85_wb_yr_n) %>%  
  rbind(., mod_9_rcp85_wb_yr_n) %>%  
  rbind(., mod_14_rcp85_wb_yr_n)



min_val <- year_values %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "total_flow", "surq", 
                    "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_val<- year_values %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "total_flow", "surq", 
                    "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_val <- year_values %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "total_flow", "surq", 
                    "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm"), ~ mean(.))%>% 
  mutate(statis = "mean")


wider_values <- min_val %>% rbind(., max_val) %>% rbind(., mean_val) %>% 
  pivot_wider(., names_from = statis, values_from = c("precip", "et", "pet", "perc",
                                                      "rchrg", "total_flow", "surq", 
                                                      "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm" ))



vars <- colnames(wider_values) %>% tibble() %>% #mutate(vars =str_remove(., "_chg_")) %>% 
  mutate(vars = str_remove(., "_maximum")) %>% 
  mutate(vars = str_remove(vars, "_minimum")) %>% 
  mutate(vars = str_remove(vars, "_mean"))

vars <- unique(vars$vars) %>% .[-c(1,2)]


for(i in 1:length(vars)){
  
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  var_tab <- wider_values %>% select(., c(year, escenario,starts_with(vars[i]))) 
  colnames(var_tab) <- c("year","escenario", "minimum", "maximum", "mean")
  
  
  plot <- var_tab %>% ggplot(., aes(x = year))+
    geom_ribbon(aes(ymin = minimum, ymax = maximum),fill = "gray84", alpha = 0.5)+
    geom_line(aes(y = mean), color = "gray21", linewidth = 0.8)+
    theme_bw()+
    labs(y = var[i])+
    facet_wrap(facets = "escenario")+
    ggtitle("Absolute values")+
    theme(text = element_text(size = 15), axis.title.x = element_blank())
  
  ggsave(plot = plot, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Values/absolute_values/", i, "_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 12, height = 10)
  
  
}


# Boxplots values in periods

for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  var_tab <- year_values %>% select(., c(year, escenario,model, vars[i] )) %>% 
    #select(., year, escenario,model,ends_with("_chg")) %>% 
    #mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
    filter(., periodo != "other") 
  
  colnames(var_tab) <- c("year","escenario", "model" ,"var", "periodo")
  
  
  
  boxplots <- var_tab %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var, fill = model ))+
    theme_bw()+
    labs(y = var[i], fill = "Model", x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario", scales = "free")+
    scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average annual values distribution: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Values/boxplot_values/absolute_value_boxplot_", i, "_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 14, height = 10)
  
}



#### YEAR VALUES PLOT --> Individual lines for each model ####

mod_4_rcp45_wb_yr_nm <- mod_4_rcp45_wb_yr    %>% cbind(scenario = "RCP 4.5", model = "mod_4")
mod_6_rcp45_wb_yr_nm <- mod_6_rcp45_wb_yr %>% cbind(scenario = "RCP 4.5", model = "mod_6")
mod_7_rcp45_wb_yr_nm <- mod_7_rcp45_wb_yr %>% cbind(scenario = "RCP 4.5", model = "mod_7")
mod_9_rcp45_wb_yr_nm <- mod_9_rcp45_wb_yr %>% cbind(scenario = "RCP 4.5", model = "mod_9")
mod_14_rcp45_wb_yr_nm <- mod_14_rcp45_wb_yr %>% cbind(scenario = "RCP 4.5", model = "mod_14")
mod_4_rcp85_wb_yr_nm <- mod_4_rcp85_wb_yr %>% cbind(scenario = "RCP 8.5", model = "mod_4")
mod_6_rcp85_wb_yr_nm <- mod_6_rcp85_wb_yr %>% cbind(scenario = "RCP 8.5", model = "mod_6")
mod_7_rcp85_wb_yr_nm <- mod_7_rcp85_wb_yr %>% cbind(scenario = "RCP 8.5", model = "mod_7")
mod_9_rcp85_wb_yr_nm <- mod_9_rcp85_wb_yr %>% cbind(scenario = "RCP 8.5", model = "mod_9")
mod_14_rcp85_wb_yr_nm <- mod_14_rcp85_wb_yr %>% cbind(scenario = "RCP 8.5", model = "mod_14")


year_values_ind <- mod_4_rcp45_wb_yr_nm %>% 
  rbind(., mod_6_rcp45_wb_yr_nm) %>%  
  rbind(., mod_7_rcp45_wb_yr_nm) %>%  
  rbind(., mod_9_rcp45_wb_yr_nm) %>%  
  rbind(., mod_14_rcp45_wb_yr_nm) %>% 
  rbind(., mod_4_rcp85_wb_yr_nm) %>%  
  rbind(., mod_6_rcp85_wb_yr_nm) %>%  
  rbind(., mod_7_rcp85_wb_yr_nm) %>%  
  rbind(., mod_9_rcp85_wb_yr_nm) %>%  
  rbind(., mod_14_rcp85_wb_yr_nm)



min_val <- year_values_ind %>% 
  group_by(year, scenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "total_flow", "surq", 
                    "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_val<- year_values_ind %>% 
  group_by(year, scenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "total_flow", "surq", 
                    "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_val <- year_values_ind %>% 
  group_by(year, scenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "total_flow", "surq", 
                    "latq", "gwflo_cha", "gwflo_res","total_gwflo", "reser_floin_mm"), ~ mean(.))%>% 
  mutate(statis = "mean")


longer_values_ind <- year_values_ind %>% pivot_longer(., -c("year","periodo", "scenario", "model" ), names_to = "variable")



vars <- unique(longer_values_ind$variable)


for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  
  var_tab <- longer_values_ind %>% select(., c(year, scenario, model, variable, value) ) %>% 
    filter(., variable == vars[i])
  
  
  min <- min_val %>% select(year, vars[i], scenario) %>% rename(min = vars[i])
  max <- max_val %>% select(year, vars[i], scenario)%>% rename(max = vars[i])
  mean <- mean_val %>% select(year, vars[i], scenario)%>% rename(mean = vars[i])
  
  
  var_tab_ind <- var_tab %>% left_join(., min, c("scenario", "year")) %>% 
    left_join(., max, c("scenario", "year")) %>% 
    left_join(., mean, c("scenario", "year"))
  
  
  plot <- var_tab_ind %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = year))+
    geom_ribbon(aes(ymin = min, ymax = max),fill = "grey80", alpha = 0.8, color = "black")+
    geom_line(aes(y = value, color = model))+
    geom_line(aes(y = mean), color = "black", linewidth = 1, linetype = 2)+
    scale_linetype_manual(values =  c(2,1))+
    scale_color_manual(values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    theme_bw()+
    facet_wrap(facets = "scenario")+
    labs(y = var[i], color = "Model")+
    # ggtitle("Absolute values")+
    theme(text = element_text(size = 15), axis.title.x = element_blank())
  
  
  p_plotly <- ggplotly(plot)
  
  
 # boxplots <- var_tab_ind %>%  mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
 #   mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
 #                              year %in% midterm_period ~ "midterm", 
 #                              year %in% longterm_period ~ "longterm", 
 #                              .default = "other")) %>% 
 #   mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
 #   filter(., periodo != "other") %>% 
 #   ggplot(., aes(x = periodo))+
 #   geom_boxplot(aes(y = value, fill = model))+
 #   theme_bw()+
 #   labs(y = var[i], fill = "Model", x = "Climate change scenario")+
 #   theme(text = element_text(size = 15))+
 #   facet_wrap(facets = "scenario", scales = "free")+
 #   scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
 #   ggtitle(paste("Average annual value distribution: ", var[i], sep = ""))
  # boxplots <- var_tab_ind %>%  mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
  #   ggplot(., aes(x = scenario))+
  #   geom_boxplot(aes(y = value, fill = model))+
  #   theme_bw()+
  #   labs(y = var[i], fill = "Model", x = "Climate change scenario")+
  #   theme(text = element_text(size = 15))+
  #   scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
  #   ggtitle(paste("Average annual value distribution: ", var[i], sep = ""))
  # 
  
  ggsave(plot = plot, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Values/individual_proj/", i, "_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 12, height = 10)
  
 # ggsave(plot = boxplots, filename = paste("figures/individual_proyections/", i, "_boxplot_", vars[i], ".tiff", sep = ""),
 #        device = "tiff", dpi = 600, width = 12, height = 10)
  
  htmlwidgets::saveWidget(p_plotly, file = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Values/individual_proj/interactive/", i, "_", vars[i], ".html", sep = ""))
  
  
  
}



# Boxplots resume for all variables


varsnames <- list( "precip" =  "Precipitation (mm)",        
                   "et" =  "Evapotranspiration (mm)",            
                   "pet" =  "Potential evapotranspiration (mm)",           
                   "perc" =  "Percolation (mm)",          
                   "rchrg" =  "Recharge (mm)",         
                   "total_flow" =  "Total flow (mm)",    
                   "surq" =  "Surface runoff (mm)",          
                   "latq" =  "Lateral flow (mm)",          
                   "gwflo_cha" =  "Groundwater flow to channel (mm)",     
                    "gwflo_res" = "Groundwater flow to reservoir (mm)",     
                    "total_gwflo"  = "Total groundwater flow (mm)",  
                    "reser_floin_mm" = "Reservoir inflow (mm)")
  


variable_labeller <- function(variable,value){
  return(varsnames[value])
}


longer_values_ind$variable <- factor(longer_values_ind$variable, levels = c("precip","et","pet","perc","rchrg",
                                                                            "total_flow","surq","latq","gwflo_cha"  ,
                                                                            "gwflo_res","total_gwflo","reser_floin_mm"))

longer_values_ind$periodo <- factor(longer_values_ind$periodo, labels = c("Baseline", "Midterm", "Longterm", "other"))



boxplots_absvals_allvars <- longer_values_ind %>% filter(periodo != "other") %>% 
  ggplot(., aes(x= scenario, y = value))+
  geom_boxplot(aes(fill = periodo))+
  scale_fill_manual( values = c("darkolivegreen1", "darkolivegreen4", "darkolivegreen") )+

  facet_wrap(facets = "variable", scales = "free", labeller = variable_labeller)+
  labs(fill = "Period", x = "Climate change scenario", y = "")+
  theme_bw()+theme(text = element_text(size = 15))+
  ggtitle("Absolute values")



ggsave(plot = boxplots_absvals_allvars, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Values/boxplot_values/", "allvars_boxplot", ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 18, height = 12)




#### Differences Regarding average baseline period values (Reeference scenarios) ####


# Anomalies regarding average baseline period values (Reeference scenarios)

colnames(reference_values_escenarios ) <- c( "escenario", "precip_ref", "et_ref", "pet_ref",
                                             "perc_ref", "rchrg_ref",  "total_flow_ref",  "surq_ref",  "latq_ref", 
                                             "gwflocha_ref", "gwflores_ref", "total_gwflo_ref", "reser_floin_ref")


mod_4_rcp45_wb_yr_nn <- mod_4_rcp45_wb_yr    %>% cbind(escenario = "RCP 4.5", model = "mod_4")
mod_6_rcp45_wb_yr_nn <- mod_6_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_6")
mod_7_rcp45_wb_yr_nn <- mod_7_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_7")
mod_9_rcp45_wb_yr_nn <- mod_9_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_9")
mod_14_rcp45_wb_yr_nn <- mod_14_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_14")
mod_4_rcp85_wb_yr_nn <- mod_4_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_4")
mod_6_rcp85_wb_yr_nn <- mod_6_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_6")
mod_7_rcp85_wb_yr_nn <- mod_7_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_7")
mod_9_rcp85_wb_yr_nn <- mod_9_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_9")
mod_14_rcp85_wb_yr_nn <- mod_14_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_14")


year_anomaly_refvalues <- mod_4_rcp45_wb_yr_nn %>% 
  rbind(., mod_6_rcp45_wb_yr_nn) %>%  
  rbind(., mod_7_rcp45_wb_yr_nn) %>%  
  rbind(., mod_9_rcp45_wb_yr_nn) %>%  
  rbind(., mod_14_rcp45_wb_yr_nn) %>% 
  rbind(., mod_4_rcp85_wb_yr_nn) %>%  
  rbind(., mod_6_rcp85_wb_yr_nn) %>%  
  rbind(., mod_7_rcp85_wb_yr_nn) %>%  
  rbind(., mod_9_rcp85_wb_yr_nn) %>%  
  rbind(., mod_14_rcp85_wb_yr_nn) %>% 
  left_join(., reference_values_escenarios, "escenario" ) %>% 
  
  
  mutate(
    pcp_anomaly_ref = (precip - precip_ref) / precip_ref,
    et_anomaly_ref = (et - et_ref) / et_ref,
    pet_anomaly_ref = (pet - pet_ref) / pet_ref,
    perc_anomaly_ref = (perc - perc_ref) / perc_ref,
    rchrg_anomaly_ref = (rchrg - rchrg_ref) / rchrg_ref,
    totalflo_anomaly_ref = (total_flow - total_flow_ref) / total_flow_ref,
    surq_anomaly_ref = (surq - surq_ref) / surq_ref,
    latq_anomaly_ref = (latq - latq_ref) / latq_ref,
    gwflocha_anomaly_ref = (gwflo_cha - gwflocha_ref) / gwflocha_ref,
    gwflores_anomaly_ref = (gwflo_res - gwflores_ref) / gwflores_ref,
    totalgwflo_anomaly_ref = (total_gwflo - total_gwflo_ref) / total_gwflo_ref,
    reser_floin_anomaly_ref = (reser_floin_mm - reser_floin_ref)/reser_floin_ref)


year_anomaly_refvalues <- year_anomaly_refvalues %>% select(., year, model, escenario, ends_with("ly_ref"))

# Boxplots anomalies in periods


vars <- colnames(year_anomaly_refvalues) %>% .[-c(1:3)]

for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  
  var_tab <- year_anomaly_refvalues %>% select(., c(year, escenario,model, starts_with(vars[i]) )) %>% 
    select(., year, escenario,model,ends_with("y_ref")) %>% 
    #mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
    filter(., periodo != "other") 
  
  colnames(var_tab) <- c("year","escenario", "model", "var", "periodo")
  
  
  
  boxplots <- var_tab %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var, fill = model ))+
    theme_bw()+
    labs(y = var[i], fill = "Model", x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario", scales = "free")+
    scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average annual anomalies distribution: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Average_reference_baseline/Anomalies/boxplot_anomalies_refvalues/", 
                                           i, "_boxplot_refanom_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 14, height = 10)
  
}




# Boxplots resume for all variables


varsnames <- list(  "pcp_anomaly_ref" =  "Precipitation (mm)",        
                    "et_anomaly_ref"=  "Evapotranspiration (mm)",            
                    "pet_anomaly_ref" =  "Potential evapotranspiration (mm)",           
                    "perc_anomaly_ref" =  "Percolation (mm)",          
                    "rchrg_anomaly_ref"=  "Recharge (mm)",         
                    "totalflo_anomaly_ref" =  "Total flow (mm)",    
                     "surq_anomaly_ref"  =  "Surface runoff (mm)",          
                     "latq_anomaly_ref" =  "Lateral flow (mm)",          
                     "gwflocha_anomaly_ref" = "Groundwater flow to channel (mm)",     
                     "gwflores_anomaly_ref" = "Groundwater flow to reservoir (mm)",     
                     "totalgwflo_anomaly_ref" = "Total groundwater flow (mm)",  
                    "reser_floin_anomaly_ref"  = "Reservoir inflow (mm)")



variable_labeller <- function(variable,value){
  return(varsnames[value])
}


# Boxplot resume for all the variables

longer_anomref_ind <- year_anomaly_refvalues %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  select(., year,escenario, model, periodo, ends_with("aly_ref")) %>% 
  pivot_longer(., -c("year","periodo", "escenario", "model" ), names_to = "variable")


longer_anomref_ind$variable <- factor(longer_anomref_ind$variable, 
        levels = c( "pcp_anomaly_ref", "et_anomaly_ref", "pet_anomaly_ref", "perc_anomaly_ref", 
                    "rchrg_anomaly_ref",  "totalflo_anomaly_ref" , "surq_anomaly_ref", "latq_anomaly_ref", 
                    "gwflocha_anomaly_ref", "gwflores_anomaly_ref",  "totalgwflo_anomaly_ref", "reser_floin_anomaly_ref"))

longer_anomref_ind$periodo <- factor(longer_anomref_ind$periodo, 
                                  levels = c("baseline", "midterm", "longterm", "other"), 
                                  labels = c("Baseline", "Midterm", "Longterm", "other"))






boxplots_anom_refs_allvars <- longer_anomref_ind %>% filter(periodo != "other") %>% 
  ggplot(., aes(x= escenario, y = value))+
  geom_boxplot(aes(fill = periodo))+
  scale_fill_manual( values = c("darkolivegreen1", "darkolivegreen4", "darkolivegreen") )+
  
  facet_wrap(facets = "variable", scales = "free", labeller = variable_labeller)+
  labs(fill = "Period", x = "Climate change scenario", y = "")+
  theme_bw()+theme(text = element_text(size = 15))+
  ggtitle("Relative changes regarding average baseline period")



ggsave(plot = boxplots_anom_refs_allvars, 
       filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Average_reference_baseline/Anomalies/boxplot_anomalies_refvalues/", 
                        "allvars_anomref_boxplot", ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 18, height = 12)






# Percentage change regarding reference baseline values


abs_var <- average_changes %>% #filter(., model != 9) %>% 
  group_by(escenario, periodo) %>% 
  summarise(across(ends_with("_chg"), ~mean(.))) %>% 
  filter(., periodo != "other")



porc_changes_refbaseline <- abs_var %>% left_join(., reference_values_escenarios, "escenario") %>% 
  #filter(., !periodo == "baseline") %>% 
  mutate(
    porc_chg_pcp =       (pcp_chg *100) / precip_ref,  # (pcp_chg*100 / precip_ref)
    porc_chg_et =        (100* et_chg) / et_ref,
    porc_chg_pet =       (100* pet_chg) / pet_ref,
    porc_chg_perc =      (100* perc_chg) / perc_ref,
    porc_chg_rchrg =     (100* rchrg_chg) / rchrg_ref,
    porc_chg_totalflow = (100* totalflo_chg) / total_flow_ref,
    porc_chg_surq =      (100* surq_chg) / surq_ref,
    porc_chg_latq =      (100* latq_chg) / latq_ref,
    porc_chg_gwflocha =  (100* gwflocha_chg) / gwflocha_ref ,
    porc_chg_gwflores =  (100* gwflores_chg) / gwflores_ref,
    porc_chg_reservin =  (100*res_floin_chg / reser_floin_ref )) %>% 
  select(escenario, periodo, starts_with("porc"))



write.table(porc_changes_refbaseline, file = "C:/ASG/R_Projects/climat_change_guajaraz/data/percentage_chg_regarding_reference_baseline.txt", quote = F, row.names = F)





# Changes regarding average baseline period values (Reeference scenarios)

colnames(reference_values_escenarios ) <- c( "scenario", "precip_ref", "et_ref", "pet_ref",
                                             "perc_ref", "rchrg_ref",  "total_flow_ref",  "surq_ref",  "latq_ref", 
                                             "gwflocha_ref", "gwflores_ref", "total_gwflo_ref", "reser_floin_ref")


mod_4_rcp45_wb_yr_nn <- mod_4_rcp45_wb_yr    %>% cbind(escenario = "RCP 4.5", model = "mod_4")
mod_6_rcp45_wb_yr_nn <- mod_6_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_6")
mod_7_rcp45_wb_yr_nn <- mod_7_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_7")
mod_9_rcp45_wb_yr_nn <- mod_9_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_9")
mod_14_rcp45_wb_yr_nn <- mod_14_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_14")
mod_4_rcp85_wb_yr_nn <- mod_4_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_4")
mod_6_rcp85_wb_yr_nn <- mod_6_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_6")
mod_7_rcp85_wb_yr_nn <- mod_7_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_7")
mod_9_rcp85_wb_yr_nn <- mod_9_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_9")
mod_14_rcp85_wb_yr_nn <- mod_14_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_14")


year_change_refvalues <- mod_4_rcp45_wb_yr_nn %>% 
  rbind(., mod_6_rcp45_wb_yr_nn) %>%  
  rbind(., mod_7_rcp45_wb_yr_nn) %>%  
  rbind(., mod_9_rcp45_wb_yr_nn) %>%  
  rbind(., mod_14_rcp45_wb_yr_nn) %>% 
  rbind(., mod_4_rcp85_wb_yr_nn) %>%  
  rbind(., mod_6_rcp85_wb_yr_nn) %>%  
  rbind(., mod_7_rcp85_wb_yr_nn) %>%  
  rbind(., mod_9_rcp85_wb_yr_nn) %>%  
  rbind(., mod_14_rcp85_wb_yr_nn) %>% 
  left_join(., reference_values_escenarios, "escenario" ) %>% 
  mutate(
    pcp_change_ref = (precip - precip_ref),
    et_change_ref = (et - et_ref),
    pet_change_ref = (pet - pet_ref),
    perc_change_ref = (perc - perc_ref),
    rchrg_change_ref = (rchrg - rchrg_ref),
    totalflo_change_ref = (total_flow - total_flow_ref),
    surq_change_ref = (surq - surq_ref),
    latq_change_ref = (latq - latq_ref),
    gwflocha_change_ref = (gwflo_cha - gwflocha_ref),
    gwflores_change_ref = (gwflo_res - gwflores_ref),
    totalgwflo_change_ref = (total_gwflo - total_gwflo_ref),
    reser_floin_change_ref = (reser_floin_mm - reser_floin_ref))


year_change_refvalues <- year_change_refvalues %>% select(., year, model, escenario, ends_with("ge_ref"))

# Boxplots anomalies in periods


vars <- colnames(year_change_refvalues) %>% .[-c(1:3)]

for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  
  var_tab <- year_change_refvalues %>% select(., c(year, escenario,model, starts_with(vars[i]) )) %>% 
    select(., year, escenario,model,ends_with("ge_ref")) %>% 
    #mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
    filter(., periodo != "other") 
  
  colnames(var_tab) <- c("year","escenario", "model", "var", "periodo")
  
  
  
  boxplots <- var_tab %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var, fill = model ))+
    theme_bw()+
    labs(y = var[i], fill = "Model", x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario", scales = "free")+
    scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average annual change distribution: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Average_reference_baseline/Changes/boxplot_changes_refvalues/", 
                                           i, "_boxplot_refchg_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 14, height = 10)
  
}




# Boxplots resume for all variables


varsnames <- list(  "pcp_change_ref" =  "Precipitation (mm)",        
                    "et_change_ref"=  "Evapotranspiration (mm)",            
                    "pet_change_ref" =  "Potential evapotranspiration (mm)",           
                    "perc_change_ref" =  "Percolation (mm)",          
                    "rchrg_change_ref"=  "Recharge (mm)",         
                    "totalflo_change_ref" =  "Total flow (mm)",    
                    "surq_change_ref"  =  "Surface runoff (mm)",          
                    "latq_change_ref" =  "Lateral flow (mm)",          
                    "gwflocha_change_ref" = "Groundwater flow to channel (mm)",     
                    "gwflores_change_ref" = "Groundwater flow to reservoir (mm)",     
                    "totalgwflo_change_ref" = "Total groundwater flow (mm)",  
                    "reser_floin_change_ref"  = "Reservoir inflow (mm)")



variable_labeller <- function(variable,value){
  return(varsnames[value])
}


# Boxplot resume for all the variables

longer_chgref_ind <- year_change_refvalues %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  select(., year,escenario, model, periodo, ends_with("ge_ref")) %>% 
  pivot_longer(., -c("year","periodo", "escenario", "model" ), names_to = "variable")


longer_chgref_ind$variable <- factor(longer_chgref_ind$variable, 
                                      levels = c( "pcp_change_ref", "et_change_ref", "pet_change_ref", "perc_change_ref", 
                                                  "rchrg_change_ref",  "totalflo_change_ref" , "surq_change_ref", "latq_change_ref", 
                                                  "gwflocha_change_ref", "gwflores_change_ref",  "totalgwflo_change_ref", "reser_floin_change_ref"))

longer_chgref_ind$periodo <- factor(longer_chgref_ind$periodo, 
                                     levels = c("baseline", "midterm", "longterm", "other"), 
                                     labels = c("Baseline", "Midterm", "Longterm", "other"))






boxplots_chg_refs_allvars <- longer_chgref_ind %>% filter(periodo != "other") %>% 
  ggplot(., aes(x= escenario, y = value))+
  geom_boxplot(aes(fill = periodo))+
  scale_fill_manual( values = c("darkolivegreen1", "darkolivegreen4", "darkolivegreen") )+
  
  facet_wrap(facets = "variable", scales = "free", labeller = variable_labeller)+
  labs(fill = "Period", x = "Climate change scenario", y = "")+
  theme_bw()+theme(text = element_text(size = 15))+
  ggtitle("Absolute changes regarding average baseline period")



ggsave(plot = boxplots_chg_refs_allvars, 
       filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Average_reference_baseline/Changes/boxplot_changes_refvalues/", 
                        "allvars_chgref_boxplot", ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 18, height = 12)








##### Cambio del valor medio de los 5 modelos respecto a el baseline medio ######





union_valores <-  mod_4_rcp45_wb_yr_nn %>% 
  rbind(., mod_6_rcp45_wb_yr_nn) %>%  
  rbind(., mod_7_rcp45_wb_yr_nn) %>%  
  rbind(., mod_9_rcp45_wb_yr_nn) %>%  
  rbind(., mod_14_rcp45_wb_yr_nn) %>% 
  rbind(., mod_4_rcp85_wb_yr_nn) %>%  
  rbind(., mod_6_rcp85_wb_yr_nn) %>%  
  rbind(., mod_7_rcp85_wb_yr_nn) %>%  
  rbind(., mod_9_rcp85_wb_yr_nn) %>%  
  rbind(., mod_14_rcp85_wb_yr_nn) 



valores_medios_5modelos <- union_valores %>% group_by(year,escenario ) %>% 
  summarise_at(c("precip", "et", "pet", "perc", "rchrg", "total_flow", "surq", 
                 "latq", "gwflo_cha", "gwflo_res", "total_gwflo", "reser_floin_mm"), mean )


cambio_medio_anual <- valores_medios_5modelos %>% 
 # mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
 #                            year %in% midterm_period ~ "midterm", 
 #                            year %in% longterm_period ~ "longterm", 
 #                            .default = "other")) %>% 
  left_join(., reference_values_escenarios, c("escenario" = "scenario" )) %>% 
  mutate(
    pcp_change_ref = (precip - precip_ref),
    et_change_ref = (et - et_ref),
    pet_change_ref = (pet - pet_ref),
    perc_change_ref = (perc - perc_ref),
    rchrg_change_ref = (rchrg - rchrg_ref),
    totalflo_change_ref = (total_flow - total_flow_ref),
    surq_change_ref = (surq - surq_ref),
    latq_change_ref = (latq - latq_ref),
    gwflocha_change_ref = (gwflo_cha - gwflocha_ref),
    gwflores_change_ref = (gwflo_res - gwflores_ref),
    totalgwflo_change_ref = (total_gwflo - total_gwflo_ref),
    reser_floin_change_ref = (reser_floin_mm - reser_floin_ref))%>% 
  select(year, escenario, ends_with("change_ref"))






anomalia_media_anual <- valores_medios_5modelos %>% 
  # mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
  #                            year %in% midterm_period ~ "midterm", 
  #                            year %in% longterm_period ~ "longterm", 
  #                            .default = "other")) %>% 
  left_join(., reference_values_escenarios, c("escenario" = "scenario" )) %>% 
mutate(
  pcp_anomaly_ref = (precip - precip_ref) / precip_ref,
  et_anomaly_ref = (et - et_ref) / et_ref,
  pet_anomaly_ref = (pet - pet_ref) / pet_ref,
  perc_anomaly_ref = (perc - perc_ref) / perc_ref,
  rchrg_anomaly_ref = (rchrg - rchrg_ref) / rchrg_ref,
  totalflo_anomaly_ref = (total_flow - total_flow_ref) / total_flow_ref,
  surq_anomaly_ref = (surq - surq_ref) / surq_ref,
  latq_anomaly_ref = (latq - latq_ref) / latq_ref,
  gwflocha_anomaly_ref = (gwflo_cha - gwflocha_ref) / gwflocha_ref,
  gwflores_anomaly_ref = (gwflo_res - gwflores_ref) / gwflores_ref,
  totalgwflo_anomaly_ref = (total_gwflo - total_gwflo_ref) / total_gwflo_ref,
  reser_floin_anomaly_ref = (reser_floin_mm - reser_floin_ref)/reser_floin_ref) %>% 
  select(year, escenario, ends_with("anomaly_ref"))




vars <- colnames(anomalia_media_anual)[-c(1,2)]


for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Total flow (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow to channel (mm)",
           "Groundwater flow to reservoir (mm)", "Total groundwater flow (mm)", "Reservoir influent (mm)")
  
  
  var_tab <- anomalia_media_anual %>% select(., c(year, escenario, starts_with(vars[i]) )) %>% 
    select(., year, escenario,ends_with("ly_ref")) %>% 
    #mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm", "other"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)", "other"))) %>% 
    filter(., periodo %in% c("Mid term (2046-2065)", "Long term (2080-2099)"))
  
  colnames(var_tab) <- c("year","escenario", "var", "periodo")
  
  
  
  boxplots <- var_tab %>% #mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var), fill = "grey80" )+
    theme_bw()+
    labs(y = var[i], x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario")+
    #scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average relative change: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Average_reference_baseline/Anomalies/boxplot_anomalies_refvalues/", 
                                           i, "_boxplot_refchg_", vars[i], ".jpg", sep = ""),
         device = "jpg", dpi = 600, width = 14, height = 10)
  
}









cambio_porcentual_medio_anual <- valores_medios_5modelos %>%
  mutate(quickflow =  surq +  latq,
         quickflow_ref = reference_values_escenarios$surq_ref+reference_values_escenarios$latq_ref) %>% 
  # mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
  #                            year %in% midterm_period ~ "midterm", 
  #                            year %in% longterm_period ~ "longterm", 
  #                            .default = "other")) %>% 
  left_join(., reference_values_escenarios, c("escenario" = "scenario" )) %>% 
  mutate(
    pcp_perc_chg_ref = 100*(precip - precip_ref) / precip_ref,
    et_perc_chg_ref = 100*(et - et_ref) / et_ref,
    pet_perc_chg_ref = 100*(pet - pet_ref) / pet_ref,
    perc_perc_chg_ref = 100*(perc - perc_ref) / perc_ref,
    rchrg_perc_chg_ref = 100*(rchrg - rchrg_ref) / rchrg_ref,
    totalflo_perc_chg_ref = 100*(total_flow - total_flow_ref) / total_flow_ref,
    surq_perc_chg_ref = 100*(surq - surq_ref) / surq_ref,
    latq_perc_chg_ref = 100*(latq - latq_ref) / latq_ref,
    gwflocha_perc_chg_ref = 100*(gwflo_cha - gwflocha_ref) / gwflocha_ref,
    gwflores_perc_chg_ref = 100*(gwflo_res - gwflores_ref) / gwflores_ref,
    totalgwflo_perc_chg_ref = 100*(total_gwflo - total_gwflo_ref) / total_gwflo_ref,
    reser_floin_perc_chg_ref = 100*(reser_floin_mm - reser_floin_ref)/reser_floin_ref,
    quick_perc_chg_ref = 100*(quickflow - quickflow_ref)/quickflow_ref ) %>% 
  select(year, escenario, ends_with("perc_chg_ref"))



vars <- colnames(cambio_porcentual_medio_anual)[-c(1,2)]


for(i in 1:length(vars)){
  
  var <- c("Precipitation (%)", "Evapotranspiration (%)", "Potential evapotranspiration (%)", "Percolation (%)",
           "Recharge (%)", "Total flow (%)", "Surface runoff (%)", "Lateral flow (%)", "Groundwater flow to channel (%)",
           "Groundwater flow to reservoir (%)", "Total groundwater flow (%)", "Reservoir inflow (%)", "Quick flow (%)")
  
  
  var_tab <- cambio_porcentual_medio_anual %>% select(., c(year, escenario, starts_with(vars[i]) )) %>% 
    select(., year, escenario,ends_with("chg_ref")) %>% 
    #mutate(model = paste("mod_", model, sep = "")) %>% 
    mutate(periodo= case_when(year %in% baseline_period ~ "baseline", 
                              year %in% midterm_period ~ "midterm", 
                              year %in% longterm_period ~ "longterm", 
                              .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm", "other"), 
                            labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)", "other"))) %>% 
    filter(., periodo != "other")
   # filter(., periodo %in% c("Mid term (2046-2065)", "Long term (2080-2099)"))
  
  colnames(var_tab) <- c("year","escenario", "var", "periodo")
  
  
  
  boxplots <- var_tab %>% #mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = var), fill = "grey80" , )+
    stat_summary(aes(y = var), fun.y= mean, geom="point", shape=17, size=5, color="red", fill="red")+
    theme_bw()+
    labs(y = paste("Average change: ", var[i], sep = ""), x = "Climate change scenario")+
   # labs(y = var[i], x = "Climate change scenario")+
    theme(text = element_text(size = 18))+
    facet_wrap(facets = "escenario")
    #scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    #ggtitle(paste("Average change: ", var[i], sep = ""))
  
  
  ggsave(plot = boxplots, filename = paste("C:/ASG/R_Projects/climat_change_guajaraz/figures/Average_reference_baseline/Perc_chg/boxplot_perchg_refvalues/", 
                                           i, "_boxplot_percchg_", vars[i], ".jpg", sep = ""),
         device = "jpg", dpi = 600, width = 14, height = 10)
  
}



