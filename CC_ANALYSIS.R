
library(tidyverse)
library(lubridate)

#### RCP 4.5 ####


# Periods definition

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


#### Model 4 ####


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
                                                latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_4_rcp45_basinaqu_wb <- mod_4_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                        seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_4_rcp45_reservoir <- mod_4_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_4_rcp45_wb_yr <- mod_4_rcp45_wb %>% left_join(., mod_4_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
                    group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_4_rcp45_wb_yrmon <- mod_4_rcp45_wb %>% left_join(., mod_4_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
                    group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_4_rcp45_reservoir_yr <- mod_4_rcp45_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 4, escenario = "RCP 4.5")
  
mod_4_rcp45_reservoir_yrmon <-  mod_4_rcp45_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 4, escenario = "RCP 4.5")



# Introduction of periods

mod_4_rcp45_wb_yr <- mod_4_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                year %in% midterm_period ~ "midterm", 
                                                year %in% longterm_period ~ "longterm", 
                                                .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_4_rcp45_wb_yrmon <- mod_4_rcp45_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                year %in% midterm_period ~ "midterm", 
                                                year %in% longterm_period ~ "longterm", 
                                                .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_4_rcp45 <- mod_4_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_4_45 <- average_wb_periods_mod_4_rcp45[1,]

average_month_wb_periods_mod_4_rcp45 <- mod_4_rcp45_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_4_45 <- average_month_wb_periods_mod_4_rcp45[average_month_wb_periods_mod_4_rcp45$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_4_45_average_anomalies <- average_wb_periods_mod_4_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_4_45$precip) / ref_values_mod_4_45$precip,
                              et_anom = (et - ref_values_mod_4_45$et)/ref_values_mod_4_45$et,
                              pet_anom = (pet - ref_values_mod_4_45$pet)/ref_values_mod_4_45$pet,
                              perc_anom = (perc - ref_values_mod_4_45$perc)/ref_values_mod_4_45$perc,
                              rchrg_anom = (rchrg - ref_values_mod_4_45$rchrg)/ref_values_mod_4_45$rchrg,
                              wyld_anom = (wyld - ref_values_mod_4_45$wyld)/ref_values_mod_4_45$wyld,
                              surq_anom = (surq - ref_values_mod_4_45$surq)/ref_values_mod_4_45$surq,
                              latq_anom = (latq - ref_values_mod_4_45$latq)/ref_values_mod_4_45$latq,
                              gwflo_anom = (gwflo - ref_values_mod_4_45$gwflo)/ref_values_mod_4_45$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_4_45_average_changes <- average_wb_periods_mod_4_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_4_45$precip),
                              et_chg = (et - ref_values_mod_4_45$et),
                              pet_chg = (pet - ref_values_mod_4_45$pet),
                              perc_chg = (perc - ref_values_mod_4_45$perc),
                              rchrg_chg = (rchrg - ref_values_mod_4_45$rchrg),
                              wyld_chg = (wyld - ref_values_mod_4_45$wyld),
                              surq_chg = (surq - ref_values_mod_4_45$surq),
                              latq_chg = (latq - ref_values_mod_4_45$latq),
                              gwflo_chg = (gwflo - ref_values_mod_4_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_4_45_year_anomalies <-  mod_4_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_4_45$precip) / ref_values_mod_4_45$precip,
               et_anom = (et - ref_values_mod_4_45$et)/ref_values_mod_4_45$et,
               pet_anom = (pet - ref_values_mod_4_45$pet)/ref_values_mod_4_45$pet,
               perc_anom = (perc - ref_values_mod_4_45$perc)/ref_values_mod_4_45$perc,
               rchrg_anom = (rchrg - ref_values_mod_4_45$rchrg)/ref_values_mod_4_45$rchrg,
               wyld_anom = (wyld - ref_values_mod_4_45$wyld)/ref_values_mod_4_45$wyld,
               surq_anom = (surq - ref_values_mod_4_45$surq)/ref_values_mod_4_45$surq,
               latq_anom = (latq - ref_values_mod_4_45$latq)/ref_values_mod_4_45$latq,
               gwflo_anom = (gwflo - ref_values_mod_4_45$gwflo)/ref_values_mod_4_45$gwflo) %>% 
               mutate(model = 4, escenario = "RCP 4.5")


mod_4_45_year_changes <- mod_4_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_4_45$precip),
                                                                      et_chg = (et - ref_values_mod_4_45$et),
                                                                      pet_chg = (pet - ref_values_mod_4_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_4_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_4_45$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_4_45$wyld),
                                                                      surq_chg = (surq - ref_values_mod_4_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_4_45$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_4_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")


mod_4_rcp45_month_anomaly <- mod_4_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_4_45 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")


mod_4_rcp45_month_chg <- mod_4_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_4_45 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")
  


# Periods definition

# rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099

#### Model 6 ####


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
#
#mod_6_rcp45_flocha <- mod_6_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 6, escenario = "RCP 4.5")



# Analysed variables --> Daily scale

mod_6_rcp45_wb <- mod_6_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_6_rcp45_basinaqu_wb <- mod_6_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_6_rcp45_reservoir <- mod_6_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_6_rcp45_wb_yr <- mod_6_rcp45_wb %>% left_join(., mod_6_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_6_rcp45_wb_yrmon <- mod_6_rcp45_wb %>% left_join(., mod_6_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_6_rcp45_reservoir_yr <- mod_6_rcp45_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 6, escenario = "RCP 4.5")

mod_6_rcp45_reservoir_yrmon <-  mod_6_rcp45_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 6, escenario = "RCP 4.5")



# Introduction of periods

mod_6_rcp45_wb_yr <- mod_6_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_6_rcp45_wb_yrmon <- mod_6_rcp45_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_6_rcp45 <- mod_6_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_6_45 <- average_wb_periods_mod_6_rcp45[1,]

average_month_wb_periods_mod_6_rcp45 <- mod_6_rcp45_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_6_45 <- average_month_wb_periods_mod_6_rcp45[average_month_wb_periods_mod_6_rcp45$periodo == "baseline",]




#Anomalies and changes, average and annual basis

mod_6_45_average_anomalies <- average_wb_periods_mod_6_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_6_45$precip) / ref_values_mod_6_45$precip,
                                                                        et_anom = (et - ref_values_mod_6_45$et)/ref_values_mod_6_45$et,
                                                                        pet_anom = (pet - ref_values_mod_6_45$pet)/ref_values_mod_6_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_6_45$perc)/ref_values_mod_6_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_6_45$rchrg)/ref_values_mod_6_45$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_6_45$wyld)/ref_values_mod_6_45$wyld,
                                                                        surq_anom = (surq - ref_values_mod_6_45$surq)/ref_values_mod_6_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_6_45$latq)/ref_values_mod_6_45$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_6_45$gwflo)/ref_values_mod_6_45$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_6_45_average_changes <- average_wb_periods_mod_6_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_6_45$precip),
                                                                      et_chg = (et - ref_values_mod_6_45$et),
                                                                      pet_chg = (pet - ref_values_mod_6_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_6_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_6_45$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_6_45$wyld),
                                                                      surq_chg = (surq - ref_values_mod_6_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_6_45$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_6_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_6_45_year_anomalies <-  mod_6_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_6_45$precip) / ref_values_mod_6_45$precip,
                                                         et_anom = (et - ref_values_mod_6_45$et)/ref_values_mod_6_45$et,
                                                         pet_anom = (pet - ref_values_mod_6_45$pet)/ref_values_mod_6_45$pet,
                                                         perc_anom = (perc - ref_values_mod_6_45$perc)/ref_values_mod_6_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_6_45$rchrg)/ref_values_mod_6_45$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_6_45$wyld)/ref_values_mod_6_45$wyld,
                                                         surq_anom = (surq - ref_values_mod_6_45$surq)/ref_values_mod_6_45$surq,
                                                         latq_anom = (latq - ref_values_mod_6_45$latq)/ref_values_mod_6_45$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_6_45$gwflo)/ref_values_mod_6_45$gwflo) %>% 
  mutate(model = 6, escenario = "RCP 4.5")


mod_6_45_year_changes <- mod_6_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_6_45$precip),
                                                      et_chg = (et - ref_values_mod_6_45$et),
                                                      pet_chg = (pet - ref_values_mod_6_45$pet),
                                                      perc_chg = (perc - ref_values_mod_6_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_6_45$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_6_45$wyld),
                                                      surq_chg = (surq - ref_values_mod_6_45$surq),
                                                      latq_chg = (latq - ref_values_mod_6_45$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_6_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")



mod_6_rcp45_month_anomaly <- mod_6_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_6_45 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")


mod_6_rcp45_month_chg <- mod_6_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_6_45 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 4.5")





# Periods definition
# rm(list = ls())


baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099

#### Model 7 ####


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
#
#mod_7_rcp45_flocha <- mod_7_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 7, escenario = "RCP 4.5")



# Analysed variables --> Daily scale

mod_7_rcp45_wb <- mod_7_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_7_rcp45_basinaqu_wb <- mod_7_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_7_rcp45_reservoir <- mod_7_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_7_rcp45_wb_yr <- mod_7_rcp45_wb %>% left_join(., mod_7_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_7_rcp45_wb_yrmon <- mod_7_rcp45_wb %>% left_join(., mod_7_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_7_rcp45_reservoir_yr <- mod_7_rcp45_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 7, escenario = "RCP 4.5")

mod_7_rcp45_reservoir_yrmon <-  mod_7_rcp45_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 7, escenario = "RCP 4.5")



# Introduction of periods

mod_7_rcp45_wb_yr <- mod_7_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_7_rcp45_wb_yrmon <- mod_7_rcp45_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_7_rcp45 <- mod_7_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_7_45 <- average_wb_periods_mod_7_rcp45[1,]


average_month_wb_periods_mod_7_rcp45 <- mod_7_rcp45_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 


ref_month_values_mod_7_45 <- average_month_wb_periods_mod_7_rcp45[average_month_wb_periods_mod_7_rcp45$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_7_45_average_anomalies <- average_wb_periods_mod_7_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_7_45$precip) / ref_values_mod_7_45$precip,
                                                                        et_anom = (et - ref_values_mod_7_45$et)/ref_values_mod_7_45$et,
                                                                        pet_anom = (pet - ref_values_mod_7_45$pet)/ref_values_mod_7_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_7_45$perc)/ref_values_mod_7_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_7_45$rchrg)/ref_values_mod_7_45$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_7_45$wyld)/ref_values_mod_7_45$wyld,
                                                                        surq_anom = (surq - ref_values_mod_7_45$surq)/ref_values_mod_7_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_7_45$latq)/ref_values_mod_7_45$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_7_45$gwflo)/ref_values_mod_7_45$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_7_45_average_changes <- average_wb_periods_mod_7_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_7_45$precip),
                                                                      et_chg = (et - ref_values_mod_7_45$et),
                                                                      pet_chg = (pet - ref_values_mod_7_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_7_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_7_45$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_7_45$wyld),
                                                                      surq_chg = (surq - ref_values_mod_7_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_7_45$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_7_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_7_45_year_anomalies <-  mod_7_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_7_45$precip) / ref_values_mod_7_45$precip,
                                                         et_anom = (et - ref_values_mod_7_45$et)/ref_values_mod_7_45$et,
                                                         pet_anom = (pet - ref_values_mod_7_45$pet)/ref_values_mod_7_45$pet,
                                                         perc_anom = (perc - ref_values_mod_7_45$perc)/ref_values_mod_7_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_7_45$rchrg)/ref_values_mod_7_45$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_7_45$wyld)/ref_values_mod_7_45$wyld,
                                                         surq_anom = (surq - ref_values_mod_7_45$surq)/ref_values_mod_7_45$surq,
                                                         latq_anom = (latq - ref_values_mod_7_45$latq)/ref_values_mod_7_45$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_7_45$gwflo)/ref_values_mod_7_45$gwflo) %>% 
  mutate(model = 7, escenario = "RCP 4.5")


mod_7_45_year_changes <- mod_7_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_7_45$precip),
                                                      et_chg = (et - ref_values_mod_7_45$et),
                                                      pet_chg = (pet - ref_values_mod_7_45$pet),
                                                      perc_chg = (perc - ref_values_mod_7_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_7_45$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_7_45$wyld),
                                                      surq_chg = (surq - ref_values_mod_7_45$surq),
                                                      latq_chg = (latq - ref_values_mod_7_45$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_7_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")




mod_7_rcp45_month_anomaly <- mod_7_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_7_45 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")


mod_7_rcp45_month_chg <- mod_7_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_7_45 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 4.5")






# Periods definition
# rm(list = ls())


baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099

#### Model 9 ####


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
#
#mod_9_rcp45_flocha <- mod_9_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 9, escenario = "RCP 4.5")



# Analysed variables --> Daily scale

mod_9_rcp45_wb <- mod_9_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_9_rcp45_basinaqu_wb <- mod_9_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_9_rcp45_reservoir <- mod_9_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_9_rcp45_wb_yr <- mod_9_rcp45_wb %>% left_join(., mod_9_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_9_rcp45_wb_yrmon <- mod_9_rcp45_wb %>% left_join(., mod_9_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_9_rcp45_reservoir_yr <- mod_9_rcp45_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 9, escenario = "RCP 4.5")

mod_9_rcp45_reservoir_yrmon <-  mod_9_rcp45_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 9, escenario = "RCP 4.5")



# Introduction of periods

mod_9_rcp45_wb_yr <- mod_9_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_9_rcp45_wb_yrmon <- mod_9_rcp45_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_9_rcp45 <- mod_9_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_9_45 <- average_wb_periods_mod_9_rcp45[1,]


average_month_wb_periods_mod_9_rcp45 <- mod_9_rcp45_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_9_45 <- average_month_wb_periods_mod_9_rcp45[average_month_wb_periods_mod_9_rcp45$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_9_45_average_anomalies <- average_wb_periods_mod_9_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_9_45$precip) / ref_values_mod_9_45$precip,
                                                                        et_anom = (et - ref_values_mod_9_45$et)/ref_values_mod_9_45$et,
                                                                        pet_anom = (pet - ref_values_mod_9_45$pet)/ref_values_mod_9_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_9_45$perc)/ref_values_mod_9_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_9_45$rchrg)/ref_values_mod_9_45$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_9_45$wyld)/ref_values_mod_9_45$wyld,
                                                                        surq_anom = (surq - ref_values_mod_9_45$surq)/ref_values_mod_9_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_9_45$latq)/ref_values_mod_9_45$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_9_45$gwflo)/ref_values_mod_9_45$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_9_45_average_changes <- average_wb_periods_mod_9_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_9_45$precip),
                                                                      et_chg = (et - ref_values_mod_9_45$et),
                                                                      pet_chg = (pet - ref_values_mod_9_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_9_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_9_45$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_9_45$wyld),
                                                                      surq_chg = (surq - ref_values_mod_9_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_9_45$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_9_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_9_45_year_anomalies <-  mod_9_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_9_45$precip) / ref_values_mod_9_45$precip,
                                                         et_anom = (et - ref_values_mod_9_45$et)/ref_values_mod_9_45$et,
                                                         pet_anom = (pet - ref_values_mod_9_45$pet)/ref_values_mod_9_45$pet,
                                                         perc_anom = (perc - ref_values_mod_9_45$perc)/ref_values_mod_9_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_9_45$rchrg)/ref_values_mod_9_45$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_9_45$wyld)/ref_values_mod_9_45$wyld,
                                                         surq_anom = (surq - ref_values_mod_9_45$surq)/ref_values_mod_9_45$surq,
                                                         latq_anom = (latq - ref_values_mod_9_45$latq)/ref_values_mod_9_45$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_9_45$gwflo)/ref_values_mod_9_45$gwflo) %>% 
  mutate(model = 9, escenario = "RCP 4.5")


mod_9_45_year_changes <- mod_9_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_9_45$precip),
                                                      et_chg = (et - ref_values_mod_9_45$et),
                                                      pet_chg = (pet - ref_values_mod_9_45$pet),
                                                      perc_chg = (perc - ref_values_mod_9_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_9_45$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_9_45$wyld),
                                                      surq_chg = (surq - ref_values_mod_9_45$surq),
                                                      latq_chg = (latq - ref_values_mod_9_45$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_9_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")




mod_9_rcp45_month_anomaly <- mod_9_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_9_45 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")


mod_9_rcp45_month_chg <- mod_9_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_9_45 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 4.5")



# Periods definition
# rm(list = ls())


baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


#### Model 14 ####


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
#
#mod_14_rcp45_flocha <- mod_14_rcp45_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 14, escenario = "RCP 4.5")



# Analysed variables --> Daily scale

mod_14_rcp45_wb <- mod_14_rcp45_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_14_rcp45_basinaqu_wb <- mod_14_rcp45_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_14_rcp45_reservoir <- mod_14_rcp45_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_14_rcp45_wb_yr <- mod_14_rcp45_wb %>% left_join(., mod_14_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_14_rcp45_wb_yrmon <- mod_14_rcp45_wb %>% left_join(., mod_14_rcp45_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_14_rcp45_reservoir_yr <- mod_14_rcp45_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 14, escenario = "RCP 4.5")

mod_14_rcp45_reservoir_yrmon <-  mod_14_rcp45_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 14, escenario = "RCP 4.5")



# Introduction of periods

mod_14_rcp45_wb_yr <- mod_14_rcp45_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_14_rcp45_wb_yrmon <- mod_14_rcp45_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_14_rcp45 <- mod_14_rcp45_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_14_45 <- average_wb_periods_mod_14_rcp45[1,]


average_month_wb_periods_mod_14_rcp45 <- mod_14_rcp45_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 


ref_month_values_mod_14_45 <- average_month_wb_periods_mod_14_rcp45[average_month_wb_periods_mod_14_rcp45$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_14_45_average_anomalies <- average_wb_periods_mod_14_rcp45 %>% mutate(pcp_anom = (precip - ref_values_mod_14_45$precip) / ref_values_mod_14_45$precip,
                                                                        et_anom = (et - ref_values_mod_14_45$et)/ref_values_mod_14_45$et,
                                                                        pet_anom = (pet - ref_values_mod_14_45$pet)/ref_values_mod_14_45$pet,
                                                                        perc_anom = (perc - ref_values_mod_14_45$perc)/ref_values_mod_14_45$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_14_45$rchrg)/ref_values_mod_14_45$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_14_45$wyld)/ref_values_mod_14_45$wyld,
                                                                        surq_anom = (surq - ref_values_mod_14_45$surq)/ref_values_mod_14_45$surq,
                                                                        latq_anom = (latq - ref_values_mod_14_45$latq)/ref_values_mod_14_45$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_14_45$gwflo)/ref_values_mod_14_45$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_14_45_average_changes <- average_wb_periods_mod_14_rcp45 %>% mutate(pcp_chg = (precip - ref_values_mod_14_45$precip),
                                                                      et_chg = (et - ref_values_mod_14_45$et),
                                                                      pet_chg = (pet - ref_values_mod_14_45$pet),
                                                                      perc_chg = (perc - ref_values_mod_14_45$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_14_45$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_14_45$wyld),
                                                                      surq_chg = (surq - ref_values_mod_14_45$surq),
                                                                      latq_chg = (latq - ref_values_mod_14_45$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_14_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_14_45_year_anomalies <-  mod_14_rcp45_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_14_45$precip) / ref_values_mod_14_45$precip,
                                                         et_anom = (et - ref_values_mod_14_45$et)/ref_values_mod_14_45$et,
                                                         pet_anom = (pet - ref_values_mod_14_45$pet)/ref_values_mod_14_45$pet,
                                                         perc_anom = (perc - ref_values_mod_14_45$perc)/ref_values_mod_14_45$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_14_45$rchrg)/ref_values_mod_14_45$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_14_45$wyld)/ref_values_mod_14_45$wyld,
                                                         surq_anom = (surq - ref_values_mod_14_45$surq)/ref_values_mod_14_45$surq,
                                                         latq_anom = (latq - ref_values_mod_14_45$latq)/ref_values_mod_14_45$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_14_45$gwflo)/ref_values_mod_14_45$gwflo) %>% 
  mutate(model = 14, escenario = "RCP 4.5")


mod_14_45_year_changes <- mod_14_rcp45_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_14_45$precip),
                                                      et_chg = (et - ref_values_mod_14_45$et),
                                                      pet_chg = (pet - ref_values_mod_14_45$pet),
                                                      perc_chg = (perc - ref_values_mod_14_45$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_14_45$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_14_45$wyld),
                                                      surq_chg = (surq - ref_values_mod_14_45$surq),
                                                      latq_chg = (latq - ref_values_mod_14_45$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_14_45$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")






mod_14_rcp45_month_anomaly <- mod_14_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_14_45 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")


mod_14_rcp45_month_chg <- mod_14_rcp45_wb_yrmon %>% 
  left_join(., ref_month_values_mod_14_45 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 4.5")


#### RCP 8.5 ####


# Periods definition

rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


#### Model 4 ####


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
#
#mod_4_rcp85_flocha <- mod_4_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 4, escenario = "RCP 8.5")



# Analysed variables --> Daily scale

mod_4_rcp85_wb <- mod_4_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_4_rcp85_basinaqu_wb <- mod_4_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_4_rcp85_reservoir <- mod_4_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_4_rcp85_wb_yr <- mod_4_rcp85_wb %>% left_join(., mod_4_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_4_rcp85_wb_yrmon <- mod_4_rcp85_wb %>% left_join(., mod_4_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_4_rcp85_reservoir_yr <- mod_4_rcp85_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 4, escenario = "RCP 8.5")

mod_4_rcp85_reservoir_yrmon <-  mod_4_rcp85_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 4, escenario = "RCP 8.5")



# Introduction of periods

mod_4_rcp85_wb_yr <- mod_4_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_4_rcp85_wb_yrmon <- mod_4_rcp85_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_4_rcp85 <- mod_4_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_4_85 <- average_wb_periods_mod_4_rcp85[1,]


average_month_wb_periods_mod_4_rcp85 <- mod_4_rcp85_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_4_85 <- average_month_wb_periods_mod_4_rcp85[average_month_wb_periods_mod_4_rcp85$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_4_85_average_anomalies <- average_wb_periods_mod_4_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_4_85$precip) / ref_values_mod_4_85$precip,
                                                                        et_anom = (et - ref_values_mod_4_85$et)/ref_values_mod_4_85$et,
                                                                        pet_anom = (pet - ref_values_mod_4_85$pet)/ref_values_mod_4_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_4_85$perc)/ref_values_mod_4_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_4_85$rchrg)/ref_values_mod_4_85$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_4_85$wyld)/ref_values_mod_4_85$wyld,
                                                                        surq_anom = (surq - ref_values_mod_4_85$surq)/ref_values_mod_4_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_4_85$latq)/ref_values_mod_4_85$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_4_85$gwflo)/ref_values_mod_4_85$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_4_85_average_changes <- average_wb_periods_mod_4_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_4_85$precip),
                                                                      et_chg = (et - ref_values_mod_4_85$et),
                                                                      pet_chg = (pet - ref_values_mod_4_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_4_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_4_85$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_4_85$wyld),
                                                                      surq_chg = (surq - ref_values_mod_4_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_4_85$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_4_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_4_85_year_anomalies <-  mod_4_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_4_85$precip) / ref_values_mod_4_85$precip,
                                                         et_anom = (et - ref_values_mod_4_85$et)/ref_values_mod_4_85$et,
                                                         pet_anom = (pet - ref_values_mod_4_85$pet)/ref_values_mod_4_85$pet,
                                                         perc_anom = (perc - ref_values_mod_4_85$perc)/ref_values_mod_4_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_4_85$rchrg)/ref_values_mod_4_85$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_4_85$wyld)/ref_values_mod_4_85$wyld,
                                                         surq_anom = (surq - ref_values_mod_4_85$surq)/ref_values_mod_4_85$surq,
                                                         latq_anom = (latq - ref_values_mod_4_85$latq)/ref_values_mod_4_85$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_4_85$gwflo)/ref_values_mod_4_85$gwflo) %>% 
  mutate(model = 4, escenario = "RCP 8.5")


mod_4_85_year_changes <- mod_4_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_4_85$precip),
                                                      et_chg = (et - ref_values_mod_4_85$et),
                                                      pet_chg = (pet - ref_values_mod_4_85$pet),
                                                      perc_chg = (perc - ref_values_mod_4_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_4_85$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_4_85$wyld),
                                                      surq_chg = (surq - ref_values_mod_4_85$surq),
                                                      latq_chg = (latq - ref_values_mod_4_85$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_4_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")





mod_4_rcp85_month_anomaly <- mod_4_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_4_85 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")


mod_4_rcp85_month_chg <- mod_4_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_4_85 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 8.5")





# Periods definition
# rm(list = ls())


baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099

#### Model 6 ####


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
#
#mod_6_rcp85_flocha <- mod_6_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 6, escenario = "RCP 8.5")
#


# Analysed variables --> Daily scale

mod_6_rcp85_wb <- mod_6_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_6_rcp85_basinaqu_wb <- mod_6_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_6_rcp85_reservoir <- mod_6_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_6_rcp85_wb_yr <- mod_6_rcp85_wb %>% left_join(., mod_6_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_6_rcp85_wb_yrmon <- mod_6_rcp85_wb %>% left_join(., mod_6_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_6_rcp85_reservoir_yr <- mod_6_rcp85_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 6, escenario = "RCP 8.5")

mod_6_rcp85_reservoir_yrmon <-  mod_6_rcp85_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 6, escenario = "RCP 8.5")



# Introduction of periods

mod_6_rcp85_wb_yr <- mod_6_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_6_rcp85_wb_yrmon <- mod_6_rcp85_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_6_rcp85 <- mod_6_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_6_85 <- average_wb_periods_mod_6_rcp85[1,]


average_month_wb_periods_mod_6_rcp85 <- mod_6_rcp85_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_6_85 <- average_month_wb_periods_mod_6_rcp85[average_month_wb_periods_mod_6_rcp85$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_6_85_average_anomalies <- average_wb_periods_mod_6_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_6_85$precip) / ref_values_mod_6_85$precip,
                                                                        et_anom = (et - ref_values_mod_6_85$et)/ref_values_mod_6_85$et,
                                                                        pet_anom = (pet - ref_values_mod_6_85$pet)/ref_values_mod_6_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_6_85$perc)/ref_values_mod_6_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_6_85$rchrg)/ref_values_mod_6_85$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_6_85$wyld)/ref_values_mod_6_85$wyld,
                                                                        surq_anom = (surq - ref_values_mod_6_85$surq)/ref_values_mod_6_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_6_85$latq)/ref_values_mod_6_85$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_6_85$gwflo)/ref_values_mod_6_85$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_6_85_average_changes <- average_wb_periods_mod_6_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_6_85$precip),
                                                                      et_chg = (et - ref_values_mod_6_85$et),
                                                                      pet_chg = (pet - ref_values_mod_6_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_6_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_6_85$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_6_85$wyld),
                                                                      surq_chg = (surq - ref_values_mod_6_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_6_85$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_6_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_6_85_year_anomalies <-  mod_6_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_6_85$precip) / ref_values_mod_6_85$precip,
                                                         et_anom = (et - ref_values_mod_6_85$et)/ref_values_mod_6_85$et,
                                                         pet_anom = (pet - ref_values_mod_6_85$pet)/ref_values_mod_6_85$pet,
                                                         perc_anom = (perc - ref_values_mod_6_85$perc)/ref_values_mod_6_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_6_85$rchrg)/ref_values_mod_6_85$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_6_85$wyld)/ref_values_mod_6_85$wyld,
                                                         surq_anom = (surq - ref_values_mod_6_85$surq)/ref_values_mod_6_85$surq,
                                                         latq_anom = (latq - ref_values_mod_6_85$latq)/ref_values_mod_6_85$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_6_85$gwflo)/ref_values_mod_6_85$gwflo) %>% 
  mutate(model = 6, escenario = "RCP 8.5")


mod_6_85_year_changes <- mod_6_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_6_85$precip),
                                                      et_chg = (et - ref_values_mod_6_85$et),
                                                      pet_chg = (pet - ref_values_mod_6_85$pet),
                                                      perc_chg = (perc - ref_values_mod_6_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_6_85$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_6_85$wyld),
                                                      surq_chg = (surq - ref_values_mod_6_85$surq),
                                                      latq_chg = (latq - ref_values_mod_6_85$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_6_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")





mod_6_rcp85_month_anomaly <- mod_6_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_6_85 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")


mod_6_rcp85_month_chg <- mod_6_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_6_85 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 6, escenario = "RCP 8.5")


# Periods definition
# rm(list = ls())


baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099

#### Model 7 ####


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
#
#mod_7_rcp85_flocha <- mod_7_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 7, escenario = "RCP 8.5")



# Analysed variables --> Daily scale

mod_7_rcp85_wb <- mod_7_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_7_rcp85_basinaqu_wb <- mod_7_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_7_rcp85_reservoir <- mod_7_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_7_rcp85_wb_yr <- mod_7_rcp85_wb %>% left_join(., mod_7_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_7_rcp85_wb_yrmon <- mod_7_rcp85_wb %>% left_join(., mod_7_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_7_rcp85_reservoir_yr <- mod_7_rcp85_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 7, escenario = "RCP 8.5")

mod_7_rcp85_reservoir_yrmon <-  mod_7_rcp85_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 7, escenario = "RCP 8.5")



# Introduction of periods

mod_7_rcp85_wb_yr <- mod_7_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_7_rcp85_wb_yrmon <- mod_7_rcp85_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_7_rcp85 <- mod_7_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_7_85 <- average_wb_periods_mod_7_rcp85[1,]


average_month_wb_periods_mod_7_rcp85 <- mod_7_rcp85_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 


ref_month_values_mod_7_85 <- average_month_wb_periods_mod_7_rcp85[average_month_wb_periods_mod_7_rcp85$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_7_85_average_anomalies <- average_wb_periods_mod_7_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_7_85$precip) / ref_values_mod_7_85$precip,
                                                                        et_anom = (et - ref_values_mod_7_85$et)/ref_values_mod_7_85$et,
                                                                        pet_anom = (pet - ref_values_mod_7_85$pet)/ref_values_mod_7_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_7_85$perc)/ref_values_mod_7_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_7_85$rchrg)/ref_values_mod_7_85$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_7_85$wyld)/ref_values_mod_7_85$wyld,
                                                                        surq_anom = (surq - ref_values_mod_7_85$surq)/ref_values_mod_7_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_7_85$latq)/ref_values_mod_7_85$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_7_85$gwflo)/ref_values_mod_7_85$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_7_85_average_changes <- average_wb_periods_mod_7_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_7_85$precip),
                                                                      et_chg = (et - ref_values_mod_7_85$et),
                                                                      pet_chg = (pet - ref_values_mod_7_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_7_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_7_85$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_7_85$wyld),
                                                                      surq_chg = (surq - ref_values_mod_7_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_7_85$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_7_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_7_85_year_anomalies <-  mod_7_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_7_85$precip) / ref_values_mod_7_85$precip,
                                                         et_anom = (et - ref_values_mod_7_85$et)/ref_values_mod_7_85$et,
                                                         pet_anom = (pet - ref_values_mod_7_85$pet)/ref_values_mod_7_85$pet,
                                                         perc_anom = (perc - ref_values_mod_7_85$perc)/ref_values_mod_7_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_7_85$rchrg)/ref_values_mod_7_85$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_7_85$wyld)/ref_values_mod_7_85$wyld,
                                                         surq_anom = (surq - ref_values_mod_7_85$surq)/ref_values_mod_7_85$surq,
                                                         latq_anom = (latq - ref_values_mod_7_85$latq)/ref_values_mod_7_85$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_7_85$gwflo)/ref_values_mod_7_85$gwflo) %>% 
  mutate(model = 7, escenario = "RCP 8.5")


mod_7_85_year_changes <- mod_7_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_7_85$precip),
                                                      et_chg = (et - ref_values_mod_7_85$et),
                                                      pet_chg = (pet - ref_values_mod_7_85$pet),
                                                      perc_chg = (perc - ref_values_mod_7_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_7_85$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_7_85$wyld),
                                                      surq_chg = (surq - ref_values_mod_7_85$surq),
                                                      latq_chg = (latq - ref_values_mod_7_85$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_7_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")




mod_7_rcp85_month_anomaly <- mod_7_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_7_85 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")


mod_7_rcp85_month_chg <- mod_7_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_7_85 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 7, escenario = "RCP 8.5")



# Periods definition
# rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099

#### Model 9 ####


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
#
#mod_9_rcp85_flocha <- mod_9_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 9, escenario = "RCP 8.5")



# Analysed variables --> Daily scale

mod_9_rcp85_wb <- mod_9_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                             latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_9_rcp85_basinaqu_wb <- mod_9_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                      seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_9_rcp85_reservoir <- mod_9_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_9_rcp85_wb_yr <- mod_9_rcp85_wb %>% left_join(., mod_9_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_9_rcp85_wb_yrmon <- mod_9_rcp85_wb %>% left_join(., mod_9_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_9_rcp85_reservoir_yr <- mod_9_rcp85_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 9, escenario = "RCP 8.5")

mod_9_rcp85_reservoir_yrmon <-  mod_9_rcp85_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 9, escenario = "RCP 8.5")



# Introduction of periods

mod_9_rcp85_wb_yr <- mod_9_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                      year %in% midterm_period ~ "midterm", 
                                                                      year %in% longterm_period ~ "longterm", 
                                                                      .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_9_rcp85_wb_yrmon <- mod_9_rcp85_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_9_rcp85 <- mod_9_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_9_85 <- average_wb_periods_mod_9_rcp85[1,]


average_month_wb_periods_mod_9_rcp85 <- mod_9_rcp85_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_9_85 <- average_month_wb_periods_mod_9_rcp85[average_month_wb_periods_mod_9_rcp85$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_9_85_average_anomalies <- average_wb_periods_mod_9_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_9_85$precip) / ref_values_mod_9_85$precip,
                                                                        et_anom = (et - ref_values_mod_9_85$et)/ref_values_mod_9_85$et,
                                                                        pet_anom = (pet - ref_values_mod_9_85$pet)/ref_values_mod_9_85$pet,
                                                                        perc_anom = (perc - ref_values_mod_9_85$perc)/ref_values_mod_9_85$perc,
                                                                        rchrg_anom = (rchrg - ref_values_mod_9_85$rchrg)/ref_values_mod_9_85$rchrg,
                                                                        wyld_anom = (wyld - ref_values_mod_9_85$wyld)/ref_values_mod_9_85$wyld,
                                                                        surq_anom = (surq - ref_values_mod_9_85$surq)/ref_values_mod_9_85$surq,
                                                                        latq_anom = (latq - ref_values_mod_9_85$latq)/ref_values_mod_9_85$latq,
                                                                        gwflo_anom = (gwflo - ref_values_mod_9_85$gwflo)/ref_values_mod_9_85$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_9_85_average_changes <- average_wb_periods_mod_9_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_9_85$precip),
                                                                      et_chg = (et - ref_values_mod_9_85$et),
                                                                      pet_chg = (pet - ref_values_mod_9_85$pet),
                                                                      perc_chg = (perc - ref_values_mod_9_85$perc),
                                                                      rchrg_chg = (rchrg - ref_values_mod_9_85$rchrg),
                                                                      wyld_chg = (wyld - ref_values_mod_9_85$wyld),
                                                                      surq_chg = (surq - ref_values_mod_9_85$surq),
                                                                      latq_chg = (latq - ref_values_mod_9_85$latq),
                                                                      gwflo_chg = (gwflo - ref_values_mod_9_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_9_85_year_anomalies <-  mod_9_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_9_85$precip) / ref_values_mod_9_85$precip,
                                                         et_anom = (et - ref_values_mod_9_85$et)/ref_values_mod_9_85$et,
                                                         pet_anom = (pet - ref_values_mod_9_85$pet)/ref_values_mod_9_85$pet,
                                                         perc_anom = (perc - ref_values_mod_9_85$perc)/ref_values_mod_9_85$perc,
                                                         rchrg_anom = (rchrg - ref_values_mod_9_85$rchrg)/ref_values_mod_9_85$rchrg,
                                                         wyld_anom = (wyld - ref_values_mod_9_85$wyld)/ref_values_mod_9_85$wyld,
                                                         surq_anom = (surq - ref_values_mod_9_85$surq)/ref_values_mod_9_85$surq,
                                                         latq_anom = (latq - ref_values_mod_9_85$latq)/ref_values_mod_9_85$latq,
                                                         gwflo_anom = (gwflo - ref_values_mod_9_85$gwflo)/ref_values_mod_9_85$gwflo) %>% 
  mutate(model = 9, escenario = "RCP 8.5")


mod_9_85_year_changes <- mod_9_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_9_85$precip),
                                                      et_chg = (et - ref_values_mod_9_85$et),
                                                      pet_chg = (pet - ref_values_mod_9_85$pet),
                                                      perc_chg = (perc - ref_values_mod_9_85$perc),
                                                      rchrg_chg = (rchrg - ref_values_mod_9_85$rchrg),
                                                      wyld_chg = (wyld - ref_values_mod_9_85$wyld),
                                                      surq_chg = (surq - ref_values_mod_9_85$surq),
                                                      latq_chg = (latq - ref_values_mod_9_85$latq),
                                                      gwflo_chg = (gwflo - ref_values_mod_9_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")





mod_9_rcp85_month_anomaly <- mod_9_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_9_85 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")


mod_9_rcp85_month_chg <- mod_9_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_9_85 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 9, escenario = "RCP 8.5")





# Periods definition
# rm(list = ls())

baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099


#### Model 14 ####


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
#
#mod_14_rcp85_flocha <- mod_14_rcp85_flocha %>% select(day, unit, mon, yr, flo_out) %>% mutate(date = dmy(paste(day,  mon, yr))) %>% select(., -c(day, mon, yr)) %>% 
#  filter(unit == 5) %>% mutate(model = 14, escenario = "RCP 8.5")



# Analysed variables --> Daily scale

mod_14_rcp85_wb <- mod_14_rcp85_wb %>%  select(., mon, day, yr, precip, et, pet, perc,
                                               latq_cha, latq_res, surq_gen, surq_cha) %>% 
  mutate(surq = surq_gen+ surq_cha,
         latq = latq_cha + latq_res, 
         date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(date, precip, et, pet, perc,  surq,  latq)


mod_14_rcp85_basinaqu_wb <- mod_14_rcp85_aqu %>% select(., mon, day, yr, dep_wt, stor, rchrg,
                                                        seep, revap, flo_cha, flo_res) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/")), 
         gwflo = flo_cha + flo_res) %>% 
  select(., date, rchrg , stor , gwflo, revap, dep_wt)


mod_14_rcp85_reservoir <- mod_14_rcp85_reservoir %>%  select(day, mon, yr, flo_in, flo_out, flo_stor) %>% 
  mutate(date = dmy(paste(day, mon, yr))) %>% select(., -c(day, mon, yr))



# Analysed variables --> Aggregation to monthly/yearly values

mod_14_rcp85_wb_yr <- mod_14_rcp85_wb %>% left_join(., mod_14_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year = year(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))

mod_14_rcp85_wb_yrmon <- mod_14_rcp85_wb %>% left_join(., mod_14_rcp85_basinaqu_wb, "date") %>% 
  mutate(wyld = surq + latq + gwflo) %>% 
  group_by(year(date), month(date)) %>% 
  summarise_at(., c("precip", "et", "pet",  "perc", "rchrg", "wyld", "surq",  "latq", "gwflo"), ~ sum(.))


mod_14_rcp85_reservoir_yr <- mod_14_rcp85_reservoir %>% group_by(year = year(date)) %>% 
  summarise(floin_yr = sum(flo_in)/1000000) %>% .[,c("year", "floin_yr")] %>% mutate(model = 14, escenario = "RCP 8.5")

mod_14_rcp85_reservoir_yrmon <-  mod_14_rcp85_reservoir %>% group_by(year(date), month(date)) %>% 
  summarise(floin_monyr = sum(flo_in)/1000000) %>% mutate(monyear = ym(paste(`year(date)`, `month(date)`))) %>% 
  .[,c("monyear", "floin_monyr")] %>% mutate(model = 14, escenario = "RCP 8.5")



# Introduction of periods

mod_14_rcp85_wb_yr <- mod_14_rcp85_wb_yr %>% mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                                                                        year %in% midterm_period ~ "midterm", 
                                                                        year %in% longterm_period ~ "longterm", 
                                                                        .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



mod_14_rcp85_wb_yrmon <- mod_14_rcp85_wb_yrmon %>% 
  rename(year = 'year(date)', month = 'month(date)') %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other"))))



# Average water balance for each period

average_wb_periods_mod_14_rcp85 <- mod_14_rcp85_wb_yr %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_values_mod_14_85 <- average_wb_periods_mod_14_rcp85[1,]


average_month_wb_periods_mod_14_rcp85 <- mod_14_rcp85_wb_yrmon %>% group_by(month, periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 

ref_month_values_mod_14_85 <- average_month_wb_periods_mod_14_rcp85[average_month_wb_periods_mod_14_rcp85$periodo == "baseline",]



#Anomalies and changes, average and annual basis

mod_14_85_average_anomalies <- average_wb_periods_mod_14_rcp85 %>% mutate(pcp_anom = (precip - ref_values_mod_14_85$precip) / ref_values_mod_14_85$precip,
                                                                          et_anom = (et - ref_values_mod_14_85$et)/ref_values_mod_14_85$et,
                                                                          pet_anom = (pet - ref_values_mod_14_85$pet)/ref_values_mod_14_85$pet,
                                                                          perc_anom = (perc - ref_values_mod_14_85$perc)/ref_values_mod_14_85$perc,
                                                                          rchrg_anom = (rchrg - ref_values_mod_14_85$rchrg)/ref_values_mod_14_85$rchrg,
                                                                          wyld_anom = (wyld - ref_values_mod_14_85$wyld)/ref_values_mod_14_85$wyld,
                                                                          surq_anom = (surq - ref_values_mod_14_85$surq)/ref_values_mod_14_85$surq,
                                                                          latq_anom = (latq - ref_values_mod_14_85$latq)/ref_values_mod_14_85$latq,
                                                                          gwflo_anom = (gwflo - ref_values_mod_14_85$gwflo)/ref_values_mod_14_85$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 


mod_14_85_average_changes <- average_wb_periods_mod_14_rcp85 %>% mutate(pcp_chg = (precip - ref_values_mod_14_85$precip),
                                                                        et_chg = (et - ref_values_mod_14_85$et),
                                                                        pet_chg = (pet - ref_values_mod_14_85$pet),
                                                                        perc_chg = (perc - ref_values_mod_14_85$perc),
                                                                        rchrg_chg = (rchrg - ref_values_mod_14_85$rchrg),
                                                                        wyld_chg = (wyld - ref_values_mod_14_85$wyld),
                                                                        surq_chg = (surq - ref_values_mod_14_85$surq),
                                                                        latq_chg = (latq - ref_values_mod_14_85$latq),
                                                                        gwflo_chg = (gwflo - ref_values_mod_14_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 



mod_14_85_year_anomalies <-  mod_14_rcp85_wb_yr %>% mutate(pcp_anom = (precip - ref_values_mod_14_85$precip) / ref_values_mod_14_85$precip,
                                                           et_anom = (et - ref_values_mod_14_85$et)/ref_values_mod_14_85$et,
                                                           pet_anom = (pet - ref_values_mod_14_85$pet)/ref_values_mod_14_85$pet,
                                                           perc_anom = (perc - ref_values_mod_14_85$perc)/ref_values_mod_14_85$perc,
                                                           rchrg_anom = (rchrg - ref_values_mod_14_85$rchrg)/ref_values_mod_14_85$rchrg,
                                                           wyld_anom = (wyld - ref_values_mod_14_85$wyld)/ref_values_mod_14_85$wyld,
                                                           surq_anom = (surq - ref_values_mod_14_85$surq)/ref_values_mod_14_85$surq,
                                                           latq_anom = (latq - ref_values_mod_14_85$latq)/ref_values_mod_14_85$latq,
                                                           gwflo_anom = (gwflo - ref_values_mod_14_85$gwflo)/ref_values_mod_14_85$gwflo) %>% 
  mutate(model = 14, escenario = "RCP 8.5")


mod_14_85_year_changes <- mod_14_rcp85_wb_yr %>% mutate(pcp_chg = (precip - ref_values_mod_14_85$precip),
                                                        et_chg = (et - ref_values_mod_14_85$et),
                                                        pet_chg = (pet - ref_values_mod_14_85$pet),
                                                        perc_chg = (perc - ref_values_mod_14_85$perc),
                                                        rchrg_chg = (rchrg - ref_values_mod_14_85$rchrg),
                                                        wyld_chg = (wyld - ref_values_mod_14_85$wyld),
                                                        surq_chg = (surq - ref_values_mod_14_85$surq),
                                                        latq_chg = (latq - ref_values_mod_14_85$latq),
                                                        gwflo_chg = (gwflo - ref_values_mod_14_85$gwflo)) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")





mod_14_rcp85_month_anomaly <- mod_14_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_14_85 , "month" ) %>% 
  mutate(pcp_anom = (precip.x - precip.y )/ precip.x,
         et_anom = (et.x - et.y )/ et.x,
         pet_anom = (pet.x - pet.y )/ pet.x,
         perc_anom = (perc.x - perc.y )/ perc.x,
         rchrg_anom = (rchrg.x - rchrg.y )/ rchrg.x,
         wyld_anom = (wyld.x - wyld.y )/ wyld.x,
         surq_anom = (surq.x - surq.y )/ surq.x,
         latq_anom = (latq.x - latq.y )/ latq.x,
         gwflo_anom = (gwflo.x - gwflo.y )/ gwflo.x) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")


mod_14_rcp85_month_chg <- mod_14_rcp85_wb_yrmon %>% 
  left_join(., ref_month_values_mod_14_85 , "month" ) %>% 
  mutate(pcp_chg = (precip.x - precip.y ),
         et_chg = (et.x - et.y ),
         pet_chg = (pet.x - pet.y ),
         perc_chg = (perc.x - perc.y ),
         rchrg_chg = (rchrg.x - rchrg.y ),
         wyld_chg = (wyld.x - wyld.y ),
         surq_chg = (surq.x - surq.y ),
         latq_chg = (latq.x - latq.y ),
         gwflo_chg = (gwflo.x - gwflo.y )) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 14, escenario = "RCP 8.5")






#### Merging results ####


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



#### Reservoir values ####

area_dren <- 367943566


floin_mod4_45 <- mod_4_rcp45_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod6_45 <- mod_6_rcp45_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod7_45 <- mod_7_rcp45_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod9_45 <- mod_9_rcp45_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))

floin_mod14_45 <- mod_14_rcp45_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod4_85 <- mod_4_rcp85_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod6_85 <- mod_6_rcp85_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod7_85 <- mod_7_rcp85_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))


floin_mod9_85 <- mod_9_rcp85_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))

floin_mod14_85 <- mod_14_rcp85_reservoir_yr %>% mutate(floin_mm = (floin_yr  * 1e9) / area_dren)%>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  #filter(., periodo != "other") %>% 
  mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm", "other")))) %>% 
  group_by(periodo) %>% 
  summarise(floin_res = mean(floin_mm))



res_in_mod4_45 <- floin_mod4_45  %>%  mutate(model = "mod_4", scenario = "RCP 4.5")
res_in_mod6_45 <- floin_mod6_45 %>%  mutate(model = "mod_6", scenario = "RCP 4.5")
res_in_mod7_45 <- floin_mod7_45 %>%  mutate(model = "mod_7", scenario = "RCP 4.5")
res_in_mod9_45 <- floin_mod9_45 %>%  mutate(model = "mod_9", scenario = "RCP 4.5")
res_in_mod14_45 <- floin_mod14_45 %>%  mutate(model = "mod_14", scenario = "RCP 4.5")
res_in_mod4_85 <- floin_mod4_85 %>%  mutate(model = "mod_4", scenario = "RCP 8.5")
res_in_mod6_85 <- floin_mod6_85 %>%  mutate(model = "mod_6", scenario = "RCP 8.5")
res_in_mod7_85 <- floin_mod7_85 %>%  mutate(model = "mod_7", scenario = "RCP 8.5")
res_in_mod9_85 <- floin_mod9_85 %>%  mutate(model = "mod_9", scenario = "RCP 8.5")
res_in_mod14_85 <- floin_mod14_85 %>%  mutate(model = "mod_14", scenario = "RCP 8.5")


abs_floin_values <- 
  res_in_mod4_45 %>% 
  rbind(., res_in_mod6_45 ) %>%  
  rbind(., res_in_mod7_45 ) %>%  
  rbind(., res_in_mod9_45 ) %>%  
  rbind(., res_in_mod14_45) %>% 
  rbind(., res_in_mod4_85 ) %>%  
  rbind(., res_in_mod6_85 ) %>%  
  rbind(., res_in_mod7_85 ) %>%  
  rbind(., res_in_mod9_85 ) %>%  
  rbind(., res_in_mod14_85)



absolute_values_allmodels <- absolute_values_allmodels %>% left_join(., abs_floin_values, c("periodo","model", "scenario")) #%>% 
  #write.table(.,"C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/table_wb_variables_resin.txt" , quote = F, col.names = F)










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
  


baseline_resin_value <- abs_floin_values %>% filter(periodo == "baseline") %>% .[,"floin_res"]

reference_values <- reference_values %>% mutate(reservoir_floin = baseline_resin_value$floin_res)
  
reference_values_escenarios <- reference_values %>% group_by(scenario) %>% 
  summarise_at(c("precip", "et", "pet", "perc", "rchrg", "wyld", "surq",  "latq", "gwflo", "reservoir_floin"), ~mean(.))



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

average_anomalies %>% #filter(., model != 9) %>% 
  group_by(escenario, periodo) %>% 
  summarise(across(ends_with("_anom"), ~mean(.))) %>% 
  filter(., periodo != "other")# %>% write.table("average_anomalies.txt", quote = F, row.names = F)


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



average_changes %>% #filter(., model != 9) %>% 
  group_by(escenario, periodo) %>% 
  summarise(across(ends_with("_chg"), ~mean(.))) %>% 
  filter(., periodo != "other")# %>% write.table("average_changes.txt", quote = F, row.names = F)

average_changes %>% #filter(., model != 9) %>% 
  group_by(escenario, periodo) %>% 
  summarise_at(c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) %>% 
  filter(., periodo != "other")# %>% write.table("average_values.txt", quote = F, row.names = F)


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


month_anomalies <- mod_4_rcp45_month_anomaly %>% 
  rbind(., mod_6_rcp45_month_anomaly) %>%  
  rbind(., mod_7_rcp45_month_anomaly) %>%  
  rbind(., mod_9_rcp45_month_anomaly) %>%  
  rbind(., mod_14_rcp45_month_anomaly) %>% 
  rbind(., mod_4_rcp85_month_anomaly) %>%  
  rbind(., mod_6_rcp85_month_anomaly) %>%  
  rbind(., mod_7_rcp85_month_anomaly) %>%  
  rbind(., mod_9_rcp85_month_anomaly) %>%  
  rbind(., mod_14_rcp85_month_anomaly)


month_changes <- mod_4_rcp45_month_chg %>% 
  rbind(., mod_6_rcp45_month_chg) %>%  
  rbind(., mod_7_rcp45_month_chg) %>%  
  rbind(., mod_9_rcp45_month_chg) %>%  
  rbind(., mod_14_rcp45_month_chg) %>% 
  rbind(., mod_4_rcp85_month_chg) %>%  
  rbind(., mod_6_rcp85_month_chg) %>%  
  rbind(., mod_7_rcp85_month_chg) %>%  
  rbind(., mod_9_rcp85_month_chg) %>%  
  rbind(., mod_14_rcp85_month_chg)




#### YEAR ANOMALIES PLOT ####

min_anomaly <- year_anomalies %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_anom", "et_anom", "pet_anom", "perc_anom",
                    "rchrg_anom", "wyld_anom", "surq_anom", "latq_anom", "gwflo_anom"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_anomaly <- year_anomalies %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_anom", "et_anom", "pet_anom", "perc_anom",
                    "rchrg_anom", "wyld_anom", "surq_anom", "latq_anom", "gwflo_anom"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_anomaly <- year_anomalies %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_anom", "et_anom", "pet_anom", "perc_anom",
                    "rchrg_anom", "wyld_anom", "surq_anom", "latq_anom", "gwflo_anom"), ~ mean(.))%>% 
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
           "Recharge", "Water yield", "Surface runoff", "Lateral flow", "Groundwater flow")
  
var_tab <- wider_anomalies %>% select(., c(year, escenario,starts_with(vars[i]))) 
colnames(var_tab) <- c("year","escenario", "minimum", "maximum", "mean")


plot <- var_tab %>% ggplot(., aes(x = year))+
  geom_ribbon(aes(ymin = minimum, ymax = maximum),fill = "mediumseagreen", alpha = 0.5)+
  geom_line(aes(y = mean), color = "springgreen4", linewidth = 0.8)+
  theme_bw()+
  geom_line(aes(y = 0), linetype = 2, size = 1)+
  labs(y = var[i])+
  facet_wrap(facets = "escenario")+
  ggtitle("Relative change regarding baseline period average (2006-2019)")+
  theme(text = element_text(size = 15), axis.title.x = element_blank())


ggsave(plot = plot, filename = paste("figures/relative_change/", i, "_", vars[i], ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 12, height = 10)


}

  # Boxplots anomalies in periods

for(i in 1:length(vars)){
  
  var <- c("Precipitation", "Evapotranspiration", "Potential evapotranspiration", "Percolation",
           "Recharge", "Water yield", "Surface runoff", "Lateral flow", "Groundwater flow")
  
  var_tab <- year_anomalies %>% select(., c(year, escenario,model, starts_with(vars[i]) )) %>% 
    select(., year, escenario,model,ends_with("_anom")) %>% 
    mutate(model = paste("mod_", model, sep = "")) %>% 
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


ggsave(plot = boxplots, filename = paste("figures/relative_change/boxplot_anomalies/", i, "_boxplot_", vars[i], ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 14, height = 10)

}


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
                    "rchrg_chg", "wyld_chg", "surq_chg", "latq_chg", "gwflo_chg"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_chg <- year_changes %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_chg", "et_chg", "pet_chg", "perc_chg",
                    "rchrg_chg", "wyld_chg", "surq_chg", "latq_chg", "gwflo_chg"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_chg <- year_changes %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("pcp_chg", "et_chg", "pet_chg", "perc_chg",
                    "rchrg_chg", "wyld_chg", "surq_chg", "latq_chg", "gwflo_chg"), ~ mean(.))%>% 
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
           "Recharge (mm)", "Water yield (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow (mm)")
  
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
  
ggsave(plot = plot, filename = paste("figures/absolute_change/", i, "_", vars[i], ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 12, height = 10)


}




  # Boxplots anomalies in periods

for(i in 1:length(vars)){
  
  var <- c("Precipitation", "Evapotranspiration", "Potential evapotranspiration", "Percolation",
           "Recharge", "Water yield", "Surface runoff", "Lateral flow", "Groundwater flow")
  
  var_tab <- year_anomalies %>% select(., c(year, escenario,model, starts_with(vars[i]) )) %>% 
    select(., year, escenario,model,ends_with("_anom")) %>% 
    mutate(model = paste("mod_", model, sep = "")) %>% 
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


ggsave(plot = boxplots, filename = paste("figures/relative_change/boxplot_anomalies/", i, "_boxplot_", vars[i], ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 14, height = 10)

}

















#### YEAR VALUES PLOT ####

mod_4_rcp45_wb_yr_n <- mod_4_rcp45_wb_yr    %>% cbind(escenario = "RCP 4.5")
mod_6_rcp45_wb_yr_n <- mod_6_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5")
mod_7_rcp45_wb_yr_n <- mod_7_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5")
mod_9_rcp45_wb_yr_n <- mod_9_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5")
mod_14_rcp45_wb_yr_n <- mod_14_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5")
mod_4_rcp85_wb_yr_n <- mod_4_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5")
mod_6_rcp85_wb_yr_n <- mod_6_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5")
mod_7_rcp85_wb_yr_n <- mod_7_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5")
mod_9_rcp85_wb_yr_n <- mod_9_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5")
mod_14_rcp85_wb_yr_n <- mod_14_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5")


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
                    "rchrg", "wyld", "surq", "latq", "gwflo"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_val<- year_values %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "wyld", "surq", "latq", "gwflo"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_val <- year_values %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "wyld", "surq", "latq", "gwflo"), ~ mean(.))%>% 
  mutate(statis = "mean")


wider_values <- min_val %>% rbind(., max_val) %>% rbind(., mean_val) %>% 
  pivot_wider(., names_from = statis, values_from = c("precip", "et", "pet",
                                                        "perc", "rchrg",  "wyld",  "surq",  "latq", "gwflo" ))



vars <- colnames(wider_values) %>% tibble() %>% #mutate(vars =str_remove(., "_chg_")) %>% 
  mutate(vars = str_remove(., "_maximum")) %>% 
  mutate(vars = str_remove(vars, "_minimum")) %>% 
  mutate(vars = str_remove(vars, "_mean"))

vars <- unique(vars$vars) %>% .[-c(1,2)]


for(i in 1:length(vars)){

  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Water yield (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow (mm)")
  
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
  
ggsave(plot = plot, filename = paste("figures/absolute_values/", i, "_", vars[i], ".tiff", sep = ""),
       device = "tiff", dpi = 600, width = 12, height = 10)


}


#### YEAR VALUES PLOT --> Individual lines for each model ####

mod_4_rcp45_wb_yr_nm <- mod_4_rcp45_wb_yr    %>% cbind(escenario = "RCP 4.5", model = "mod_4")
mod_6_rcp45_wb_yr_nm <- mod_6_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_6")
mod_7_rcp45_wb_yr_nm <- mod_7_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_7")
mod_9_rcp45_wb_yr_nm <- mod_9_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_9")
mod_14_rcp45_wb_yr_nm <- mod_14_rcp45_wb_yr %>% cbind(escenario = "RCP 4.5", model = "mod_14")
mod_4_rcp85_wb_yr_nm <- mod_4_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_4")
mod_6_rcp85_wb_yr_nm <- mod_6_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_6")
mod_7_rcp85_wb_yr_nm <- mod_7_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_7")
mod_9_rcp85_wb_yr_nm <- mod_9_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_9")
mod_14_rcp85_wb_yr_nm <- mod_14_rcp85_wb_yr %>% cbind(escenario = "RCP 8.5", model = "mod_14")


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
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "wyld", "surq", "latq", "gwflo"), ~ min(.)) %>% 
  mutate(statis = "minimum")

max_val<- year_values_ind %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "wyld", "surq", "latq", "gwflo"), ~ max(.))%>% 
  mutate(statis = "maximum")

mean_val <- year_values_ind %>% 
  group_by(year, escenario) %>% 
  summarise_at(., c("precip", "et", "pet", "perc",
                    "rchrg", "wyld", "surq", "latq", "gwflo"), ~ mean(.))%>% 
  mutate(statis = "mean")


longer_values_ind <- year_values_ind %>% pivot_longer(., -c("year","periodo", "escenario", "model" ), names_to = "variable")
  


vars <- unique(longer_values_ind$variable)


for(i in 1:length(vars)){
  
  var <- c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
           "Recharge (mm)", "Water yield (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow (mm)")

  
   var_tab <- longer_values_ind %>% select(., c(year, escenario, model, variable, value) ) %>% 
    filter(., variable == vars[i])
  

  min <- min_val %>% select(year, vars[i], escenario) %>% rename(min = vars[i])
  max <- max_val %>% select(year, vars[i], escenario)%>% rename(max = vars[i])
  mean <- mean_val %>% select(year, vars[i], escenario)%>% rename(mean = vars[i])
  
  
  var_tab_ind <- var_tab %>% left_join(., min, c("escenario", "year")) %>% 
    left_join(., max, c("escenario", "year")) %>% 
    left_join(., mean, c("escenario", "year"))

  
  plot <- var_tab_ind %>% mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    ggplot(., aes(x = year))+
    geom_ribbon(aes(ymin = min, ymax = max),fill = "grey80", alpha = 0.8, color = "black")+
    geom_line(aes(y = value, color = model))+
    geom_line(aes(y = mean), color = "black", linewidth = 1, linetype = 2)+
    scale_linetype_manual(values =  c(2,1))+
    scale_color_manual(values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    theme_bw()+
    facet_wrap(facets = "escenario")+
    labs(y = var[i], color = "Model")+
   # ggtitle("Absolute values")+
    theme(text = element_text(size = 15), axis.title.x = element_blank())
  
  
  p_plotly <- ggplotly(plot)
  
  
  boxplots <- var_tab_ind %>%  mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
    mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                               year %in% midterm_period ~ "midterm", 
                               year %in% longterm_period ~ "longterm", 
                               .default = "other")) %>% 
    mutate(periodo = factor(periodo, levels = c("baseline", "midterm", "longterm"), labels = c("Baseline (2006-2019)", "Mid term (2046-2065)", "Long term (2080-2099)"))) %>% 
    filter(., periodo != "other") %>% 
    ggplot(., aes(x = periodo))+
    geom_boxplot(aes(y = value, fill = model))+
    theme_bw()+
    labs(y = var[i], fill = "Model", x = "Climate change scenario")+
    theme(text = element_text(size = 15))+
    facet_wrap(facets = "escenario", scales = "free")+
    scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    ggtitle(paste("Average annual value distribution: ", var[i], sep = ""))
 # boxplots <- var_tab_ind %>%  mutate(model = factor(model, levels = c("mod_4","mod_6","mod_7","mod_9","mod_14"))) %>% 
 #   ggplot(., aes(x = escenario))+
 #   geom_boxplot(aes(y = value, fill = model))+
 #   theme_bw()+
 #   labs(y = var[i], fill = "Model", x = "Climate change scenario")+
 #   theme(text = element_text(size = 15))+
 #   scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
 #   ggtitle(paste("Average annual value distribution: ", var[i], sep = ""))
 # 
  
  ggsave(plot = plot, filename = paste("figures/individual_proyections/", i, "_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 12, height = 10)
  
  ggsave(plot = boxplots, filename = paste("figures/individual_proyections/", i, "_boxplot_", vars[i], ".tiff", sep = ""),
         device = "tiff", dpi = 600, width = 12, height = 10)
  
  htmlwidgets::saveWidget(p_plotly, file = paste("figures/individual_proyections/interactive/", i, "_", vars[i], ".html", sep = ""))
  
  
  
}


varsnames <- tibble(vars,
names = c("Precipitation (mm)", "Evapotranspiration (mm)", "Potential evapotranspiration (mm)", "Percolation (mm)",
                "Recharge (mm)", "Water yield (mm)", "Surface runoff (mm)", "Lateral flow (mm)", "Groundwater flow (mm)"))

  longer_values_ind %>% 
    ggplot(., aes(x= escenario, y = value))+
    geom_boxplot(aes(fill = model))+
    scale_fill_manual( values = c("deeppink2", "goldenrod1", "cadetblue1", "brown1", "mediumorchid3") )+
    facet_wrap(facets = "variable", scales = "free", labeller = as_labeller(varsnames))+
    labs(fill = "Model", x = "Climate change scenario")+
    theme_bw()

  
  # Anomalies regarding average baseline period values
  
  colnames(reference_values_escenarios ) <- c( "escenario", "precip_ref", "et_ref", "pet_ref",
                                               "perc_ref", "rchrg_ref",  "wyld_ref",  "surq_ref",  "latq_ref", "gwflo_ref")
  
  
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
      wyld_anomaly_ref = (wyld - wyld_ref) / wyld_ref,
      surq_anomaly_ref = (surq - surq_ref) / surq_ref,
      latq_anomaly_ref = (latq - latq_ref) / latq_ref,
      gwflo_anomaly_ref = (gwflo - gwflo_ref) / gwflo_ref)
  
  
  
  
  # Boxplots anomalies in periods
  
  for(i in 1:length(vars)){
    
    var <- c("Precipitation", "Evapotranspiration", "Potential evapotranspiration", "Percolation",
             "Recharge", "Water yield", "Surface runoff", "Lateral flow", "Groundwater flow")
    
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
    
    
    ggsave(plot = boxplots, filename = paste("figures/relative_change/boxplot_anomalies/anomalies_refvalues/", i, "_boxplot_refanom_", vars[i], ".tiff", sep = ""),
           device = "tiff", dpi = 600, width = 14, height = 10)
    
  }
  
  
  
  # Percentage change regarding reference baseline values
  
  
  abs_var <- average_changes %>% #filter(., model != 9) %>% 
    group_by(escenario, periodo) %>% 
    summarise(across(ends_with("_chg"), ~mean(.))) %>% 
    filter(., periodo != "other")
  
  
  
  lfm <- abs_var %>% left_join(., reference_values_escenarios, "scenario") %>% 
    filter(., !periodo == "baseline") %>% 
    mutate(
      porc_chg_pcp = -100*(1-(precip_ref + pcp_chg) / precip_ref),  # (pcp_chg*100 / precip_ref)
      porc_chg_et = -100*(1-(et_ref + et_chg) / et_ref),
      porc_chg_pet = -100*(1-(pet_ref + pet_chg) / pet_ref),
      porc_chg_perc = -100*(1-(perc_ref + perc_chg) / perc_ref),
      porc_chg_rchrg = -100*(1-(rchrg_ref + rchrg_chg) / rchrg_ref),
      porc_chg_wyld = -100*(1-(wyld_ref + wyld_chg) / wyld_ref),
      porc_chg_surq = -100*(1-(surq_ref + surq_chg) / surq_ref),
      porc_chg_latq = -100*(1-(latq_ref + latq_chg) / latq_ref),
      porc_chg_gwflo = -100*(1-(gwflo_ref + gwflo_chg) / gwflo_ref)) %>% 
    select(escenario, periodo, starts_with("porc"))
  
  
  
  write.table(lfm, file = "percentage_chg_regarding_reference_baseline.txt", quote = F, row.names = F)
  
  
  
  
  
  
