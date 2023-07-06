







basinwb_baseline <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/Baseline/basin_wb_day.txt", skip = 3)

colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/Baseline/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]

colnames(basinwb_baseline) <- colnmss



baseline_wb <- basinwb_baseline %>%  select(., mon, day,   yr, precip, surq_gen,surq_res, latq_cha ,latq_res, et , perc, pet )

baseline_wb_yr <- baseline_wb %>% mutate(date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(., -c(day,mon,  yr)) %>% group_by(year(date)) %>% 
  summarise_at(., c("precip", "surq_gen", "latq" , "et" , "perc", "pet"), ~sum(.))



baseline_aquifer <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/Baseline/basin_aqu_day.txt", skip = 3)

colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/Baseline/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]

colnames(baseline_aquifer) <- colnmsss

basinaqu_wb <- baseline_aquifer %>% select(., mon, day,   yr, flo , dep_wt, stor, rchrg, seep, revap, flo_cha, flo_res   ) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(., -c(day,mon,  yr)) %>% group_by(year(date)) %>%
  summarise_at(., c("flo" , "dep_wt", "stor", "rchrg", "seep", "revap", "flo_cha", "flo_res" ), ~sum(.))







baseline_period <- 2006:2019
midterm_period <- 2046:2065
longterm_period <- 2080:2099



mod_4_rcp45_wb <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_wb_day.txt", skip = 3)

colnmss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_wb_day.txt", skip = 1, col_names = F) %>% .[1,]

colnames(mod_4_rcp45_wb) <- colnmss




  mod_4_rcp45_aqu <- read.table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_aqu_day.txt", skip = 3)

colnmsss <- read_table("C:/ASG/Guajaraz_modelo/Txt_In_Out_CClimatico/RCP_45/TxtInOut_4_CCLM/basin_aqu_day.txt", skip = 1, col_names = F) %>% .[1,]

colnames(mod_4_rcp45_aqu) <- colnmsss



mod4_rcp45_wb_yr <- mod_4_rcp45_wb %>% mutate(date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(., -c(day,mon,  yr)) %>% group_by(year(date)) %>% 
  summarise_at(., c("precip", "surq_gen", "surq_res", "latq_cha", "latq_res" , "et" , "perc", "pet"), ~sum(.)) %>% 
  mutate(surq = surq_gen +surq_res, latq = latq_cha+ latq_res) %>%  select(., -c(surq_gen, surq_res, latq_cha, latq_res)) 



mod4_rcp45_basinaqu_wb_yr <- mod_4_rcp45_aqu %>% select(., mon, day,   yr , dep_wt, stor, rchrg, seep, revap, flo_cha, flo_res   ) %>% 
  mutate(date = dmy(paste(day,mon,  yr, sep = "/"))) %>% 
  select(., -c(day,mon,  yr)) %>% group_by(year(date)) %>%
  summarise_at(., c("rchrg", "seep", "revap", "flo_cha", "flo_res" ), ~sum(.)) %>% 
  mutate(gwflo = flo_cha+flo_res) %>%  select(., -c(flo_cha ,flo_res))




aaa <- mod4_rcp45_wb_yr %>% left_join(., mod4_rcp45_basinaqu_wb_yr, "year(date)") %>% rename("year" = "year(date)") %>% 
  mutate(wyld = surq + latq +gwflo) %>% 
  mutate(periodo = case_when(year %in% baseline_period ~ "baseline", 
                             year %in% midterm_period ~ "midterm", 
                             year %in% longterm_period ~ "longterm", 
                             .default = "other")) %>% 
  filter(., periodo != "other") %>% 
         mutate(periodo = factor(periodo, levels = (c("baseline", "midterm", "longterm"))))

average_wb_periods <- aaa %>% group_by(periodo) %>% 
  summarise_at(., c("precip", "et", "pet", "perc", "rchrg" , "wyld","surq",  "latq", "gwflo"), ~mean(.)) 


ref_values <- average_wb_periods[1,]


(average_wb_periods[2,-1]- ref_values[,-1])/ ref_values[,-1]
(average_wb_periods[3,-1]- ref_values[,-1])/ ref_values[,-1]



average_wb_periods %>% mutate(pcp_anom = (precip - ref_values$precip) / ref_values$precip,
                              et_anom = (et - ref_values$et)/ref_values$et,
                              pet_anom = (pet - ref_values$pet)/ref_values$pet,
                              perc_anom = (perc - ref_values$perc)/ref_values$perc,
                              rchrg_anom = (rchrg - ref_values$rchrg)/ref_values$rchrg,
                              wyld_anom = (wyld - ref_values$wyld)/ref_values$wyld,
                              surq_anom = (surq - ref_values$surq)/ref_values$surq,
                              latq_anom = (latq - ref_values$latq)/ref_values$latq,
                              gwflo_anom = (gwflo - ref_values$gwflo)/ref_values$gwflo) %>% 
  mutate_if(is.numeric, ~ round(., 3)) %>% 
  mutate(model = 4, escenario = "RCP 4.5")%>% 
  .[, c(21, 20,1, 2,11, 3,12,4,13,5,14,6,15,7,16,8,17,9,18,10,19)] 












aaa %>% mutate(pcp_anom = (precip - ref_values$precip) / ref_values$precip,
               et_anom = (et - ref_values$et)/ref_values$et,
               pet_anom = (pet - ref_values$pet)/ref_values$pet,
               perc_anom = (perc - ref_values$perc)/ref_values$perc,
               rchrg_anom = (rchrg - ref_values$rchrg)/ref_values$rchrg,
               wyld_anom = (wyld - ref_values$wyld)/ref_values$wyld,
               surq_anom = (surq - ref_values$surq)/ref_values$surq,
               latq_anom = (latq - ref_values$latq)/ref_values$latq,
               gwflo_anom = (gwflo - ref_values$gwflo)/ref_values$gwflo) %>% 
               mutate(model = 4, escenario = "RCP 4.5")













aaa %>% filter(., periodo != "other") %>%
  mutate(wyld = latq + )
  select(year, precip, )
  pivot_longer(., -c(year))
  ggplot(aes(x = year))+
  geom_boxplot(aes(y = pet, color = periodo))


table(aaa$periodo)






















