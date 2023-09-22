# Proceso de calibracion del Modelo del Guajaraz

# Se ha calibrado para la serie 1982-1985 y validado para la serie 1986-1988

# Se han corrido 4 rondas de 1000 simulacoines variando parámetros. Se han filtrado aquellas simulaciones con un runoff rate < 20%
# y un groundwater rate < 40%

# Se han seleccionado las 10 mejores simulaciones con mejores estadísticas en la calibración y se han valdiado, seleccionando
# posteriormente las dos simulaciones que han obtenido buenas estadísticas en ambas


library(SWATplusR)
library(lhs)
library(purrr)
library(tidyverse)
library(stringr)
library(hydroGOF)
library(lubridate)


rsq_r2 <-  function(x, y ) cor(x, y)^2


demo_path<- ("C:/Lidia/TxtInOut_Guajaraz_newexe/")

limites_parametros_r1 <- tibble('cn2.hru | change = pctchg' = c(-30, 30),   
                             'perco.hru | change = absval' = c(0, 0.5),
                             'esco.hru | change = absval' = c(0, 0.999),
                             'epco.hru | change = absval' = c(0.001, 0.999), 
                             'ovn.hru | change = pctchg' = c(-20, 20),
                             'cn3_swf.hru | change = absval' = c(0, 1),
                             'latq_co.hru | change = absval' = c(0, 1),
                             'lat_ttime.hru | change = absval' = c(1, 180),
                             'bd.sol | change = pctchg' = c(-30, 30),
                             'k.sol | change = pctchg' = c(-100, 500),
                             'awc.sol | change = pctchg' = c(-80, 80),
                             'z.sol | change = pctchg' = c(-50, 50),
                             'surlag.bsn | change = absval' = c(0.05, 24),
                             'chn.rte | change = absval' = c(0, 0.15),
                             'alpha.aqu | change = absval' = c(0.001, 0.05),
                             'flo_min.aqu | change = absval' = c(0, 10),
                             'revap_co.aqu | change = absval' = c(0.02, 0.2), 
                             'revap_min.aqu | change = absval' = c(0, 10), 
                             'sp_yld.aqu | change = absval' = c(0, 0.5))

limites_parametros_r2 <- tibble('cn2.hru | change = pctchg' = c(-30, 15),   
                             'perco.hru | change = absval' = c(0, 0.5),
                             'esco.hru | change = absval' = c(0.25, 0.999),
                             'epco.hru | change = absval' = c(0.25, 0.999), 
                             'ovn.hru | change = pctchg' = c(-20, 20),
                             'cn3_swf.hru | change = absval' = c(0.25, 1),
                             'latq_co.hru | change = absval' = c(0, 0.25),
                             'lat_ttime.hru | change = absval' = c(1, 25),
                             'bd.sol | change = pctchg' = c(-20, 20),
                             'k.sol | change = pctchg' = c(-100, 100),
                             'awc.sol | change = pctchg' = c(-40, 80),
                             'z.sol | change = pctchg' = c(-15, 50),
                             'surlag.bsn | change = absval' = c(0.05, 24),
                             'chn.rte | change = absval' = c(0, 0.15),
                             'alpha.aqu | change = absval' = c(0.002, 0.02),
                             'flo_min.aqu | change = absval' = c(6, 10),
                             'revap_co.aqu | change = absval' = c(0.02, 0.2), 
                             'revap_min.aqu | change = absval' = c(0, 3), 
                             'sp_yld.aqu | change = absval' = c(0, 0.5))


limites_parametros_r3<- tibble('cn2.hru | change = pctchg' = c(-10, 15),   
                             'perco.hru | change = absval' = c(0.15, 0.5),
                             'esco.hru | change = absval' = c(0.25, 0.8),
                             'epco.hru | change = absval' = c(0.25, 0.8), 
                             'ovn.hru | change = pctchg' = c(-20, 20),
                             'cn3_swf.hru | change = absval' = c(0.25, 0.7),
                             'latq_co.hru | change = absval' = c(0.15, 0.25),
                             'lat_ttime.hru | change = absval' = c(1, 10),
                             'bd.sol | change = pctchg' = c(0, 20),
                             'k.sol | change = pctchg' = c(20, 100),
                             'awc.sol | change = pctchg' = c(-40, 10),
                             'z.sol | change = pctchg' = c(-15, 20),
                             'surlag.bsn | change = absval' = c(0.05, 25),
                             'chn.rte | change = absval' = c(0, 0.1),
                             'alpha.aqu | change = absval' = c(0.002, 0.015),
                             'flo_min.aqu | change = absval' = c(6, 10),
                             'revap_co.aqu | change = absval' = c(0.02, 0.2), 
                             'revap_min.aqu | change = absval' = c(0, 3), 
                             'sp_yld.aqu | change = absval' = c(0, 0.5))



n_sample <- 1000  
n_par <- ncol(limites_parametros_r3) 

parametros_prueba <- randomLHS(n = n_sample, k = n_par) %>% 
  as_tibble(., .name_repair = 'minimal') %>% 
  set_names(names(limites_parametros_r3)) %>% 
  map2_df(., limites_parametros_r3, ~ (.x * (.y[2] - .y[1]) + .y[1]))                             
 

ronda_1 <- run_swatplus(project_path = demo_path, 
                          output =list(caudal = define_output(file = 'channel_sd',
                                                              variable = 'flo_out', 
                                                              unit = 5),
                                       entrada_embalse = define_output(file = 'reservoir',
                                                                       variable = 'flo_in',
                                                                       unit = 1),
                                       salida_embalse = define_output(file = 'reservoir',
                                                                      variable = 'flo_out',
                                                                      unit = 1),
                                       almacenamiento_embalse = define_output(file = 'reservoir',
                                                                              variable = 'flo_stor',
                                                                              unit = 1),
                                       precipitacion = define_output(file = 'basin_wb',
                                                                     variable = 'precip',
                                                                     unit = 1), 
                                       evaporacion = define_output(file = 'basin_wb',
                                                                   variable = 'et',
                                                                   unit = 1), 
                                       percolacion = define_output(file = 'basin_wb',
                                                                   variable = 'perc',
                                                                   unit = 1), 
                                       flujo_lateral = define_output(file = 'basin_wb',
                                                                     variable = 'latq_cha',
                                                                     unit = 1), 
                                       flujo_superficial = define_output(file = 'basin_wb',
                                                                         variable = 'surq_cha',
                                                                         unit = 1), 
                                       flo_aqu = define_output(file = 'basin_aqu',
                                                               variable = 'flo_cha',
                                                               unit = 1),             
                                       recarga_aqu = define_output(file = 'basin_aqu',
                                                                   variable = 'rchrg',
                                                                   unit = 1),  
                                       seep = define_output(file = 'basin_aqu',
                                                            variable = 'seep',
                                                            unit = 1),  
                                       evapo_aqu = define_output(file = 'basin_aqu',
                                                                 variable = 'revap',
                                                                 unit = 1)), 
                          parameter = parametros_prueba, 
                          start_date = 19770101, 
                          end_date = 19851231, 
                          years_skip = 5, 
                          n_thread = 60)

write_rds(ronda_1, "C:/ASG/Calibracion_Guajaraz/new_ex/ronda_3_newexe.rds")



#### ROUND 1 ####

#### EXTRAER Y CONVERTIR VARIABLES ####

hardcal_guajaraz_1 <- read_rds("C:/ASG/Calibracion_Guajaraz/new_ex/ronda_1_newexe.rds")

# Parametros usados

pars_r1 <-  hardcal_guajaraz_1$parameter$values


# Cargamos primero los datos de caudal observados


obs_mensual<- read_csv("C:/ASG/Calibracion_Guajaraz/observed_Guajaraz_Toolbox.csv") %>% 
  mutate(date = my(Date)) %>% mutate(flow = as.numeric(Flow)) %>% 
  filter(., year(date) %in% c(1983:1985)) %>%  .[, c(3:4)] %>% tibble(.)    

# PCP 

precip_anual <- hardcal_guajaraz_1$simulation$precipitacion %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

precip_serie <- precip_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Escorrentia

entradaembalse_mediadiaria_anual <- hardcal_guajaraz_1$simulation$entrada_embalse %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), mean))  

entrada_anual_mm <- entradaembalse_mediadiaria_anual[,-1]*1000 *365 / 367943566
entrada_media_anual_mm <- entrada_anual_mm %>% apply(., 2, mean)

# GW
gwflo_anual <- hardcal_guajaraz_1$simulation$flo_aqu %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

gwflo_medio <- gwflo_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Caudal mensual
cal_med_mensual <- function(x){mean(x)/86400}

q_sim <- hardcal_guajaraz_1$simulation$entrada_embalse %>% group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("run_"), cal_med_mensual)) %>% mutate(mon_year = (paste(`month(date)`, `year(date)`))) %>%
  mutate(mon_year = my(mon_year)) %>% 
  .[, c(1003, 3:1002)] %>% filter(., year(mon_year) > 1982)

# Runoff rate

ratio_escorrentia <- entrada_media_anual_mm / precip_serie %>%  tibble(.)

# Gw rate
ratio_gwflo <- gwflo_medio / entrada_media_anual_mm


#### FILTRAR SIMULACIONES ####
# CRITERIOS : GW < 0.4, RUNOFF < 0.2

names <- colnames(ratio_gwflo)
runoff_rates <- ratio_escorrentia  %>% transpose(.) %>% unlist(.) %>% tibble(runoff_rate = ., run = names)
gwrates <- ratio_gwflo  %>% transpose(.) %>% unlist(.) %>% tibble(groundw_rate = ., run = names)

filt_table <- runoff_rates %>% left_join(., gwrates, "run") %>% .[,c(2,1,3)]

sims_filtradas <- filt_table %>% filter(., runoff_rate < 0.2, groundw_rate < 0.4)#, groundw_rate > 0.15)

runs_filtradas <- sims_filtradas %>% mutate(runname = as.numeric(str_remove(run, "run_"))) %>% .[,"runname"]

#### CALCULO DE ESTADISTICOS ####

nse_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x, obs_mensual$flow))

pbias_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, obs_mensual$flow))

r2_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, obs_mensual$flow))

rmse_q <- q_sim %>% select(-mon_year) %>% map_dbl(., ~rmse(.x, obs_mensual$flow))     

#### Resultados estadisticos ####


tibble(run = names(nse_q[runs_filtradas$runname]), nse = nse_q[runs_filtradas$runname], r2 = r2_q[runs_filtradas$runname],  
       pbias = pbias_q[runs_filtradas$runname], rmse = rmse_q[runs_filtradas$runname]) %>% 
  pivot_longer(., -run) %>% 
  
  ggplot(aes(x = name, fill = name, y = value)) + geom_violin()+facet_wrap(facets = "name", scales = "free")


#### DOTTY PLOTS ####

plot_dotty <- function(par, crit, crit_label = 'crit', n_col = 1) {
  dotty_tbl <- par %>% 
    mutate(crit = crit) %>% 
    pivot_longer(., cols = -crit, names_to = 'parameter')
  
  ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = crit)) +geom_smooth(aes(x = value, y = crit)) +
    facet_wrap(. ~ parameter, ncol = 1, scales = "free_x") +
    labs(x = 'Change of parameter value', y = crit_label) +
    ggtitle("Iteration 2")+
    theme_bw()+
    theme(text = element_text(size = 24), plot.title = element_text(hjust = 0.5), axis.title.x = element_blank())
}

# Variables 

# Runoff rate
plot_dotty(par = pars_r1[runs_filtradas$runname,], crit = unlist(ratio_escorrentia[, runs_filtradas$runname]), "Runoff rate")


# Gw flow
plot_dotty(par = pars_r1[runs_filtradas$runname, c(1,11) ], crit = unlist(ratio_gwflo[, runs_filtradas$runname]), "Groundwater rate")


rank_tableGRAN_r1

plot_dotty(par = pars_r1, crit = unlist(ratio_gwflo), "Groundwater rate")

plot_dotty <- function(par, crit, crit2, crit_label = 'crit', crit2_label = 'crit2', n_col = 5) {
  dotty_tbl <- par %>% 
    mutate(crit = crit) %>%
    mutate(crit2 = crit2) %>% 
    pivot_longer(., cols = -c(crit, crit2), names_to = 'parameter')
  
  ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = crit, color = crit2)) +geom_smooth(aes(x = value, y = crit)) +
    facet_wrap(. ~ parameter, ncol = 5, scales = "free_x") +
    theme_bw()+
    scale_color_gradientn(colors = rev(magma(10)))+
    labs(x = 'Change of parameter value', y = crit_label, color = crit2_label)
}




dotty_metrics_color <- tibble(run = names(nse_q),     
       nse = nse_q,           
       r2 = r2_q,
       pbias = pbias_q, 
       rmse = rmse_q,
       rank_nse = rank(nse),  # La peor es la 1
       rank_r2 = rank(r2),    # La peor es la 1
       rank_rmse = rank(-rmse), # La peor es la primera
       rank_pbias = rank(-abs(pbias)), # La peor es la 1
       rank_run = rank_nse+rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
  left_join(., filt_table, "run") #%>% arrange(-rank_run)




  plot_dotty(par = pars_r1, crit = dotty_metrics_color$groundw_rate, crit2 = dotty_metrics_color$rank_run, crit_label = "Groundwater rate",crit2_label = "Rank")

plot_dotty(par = pars_r1[runs_filtradas$runname, ], crit = dotty_metrics_color$groundw_rate[runs_filtradas$runname ],
            crit2 = dotty_metrics_color$rank_run[runs_filtradas$runname], crit_label = "Groundwater rate",crit2_label = "Rank")


# Estadisticos

http://127.0.0.1:28839/graphics/plot_zoom_png?width=2327&height=1252
# Vemos el efecto de cada par?metro sobre el NSE
plot_dotty(par = pars_r1[runs_filtradas$runname, ], crit = nse_q[runs_filtradas$runname], "NSE")+ ylim(c(-10, 1))


# Vemos el efecto de cada par?metro sobre el R?
plot_dotty(par = pars_r1[runs_filtradas$runname, ], crit = r2_q[runs_filtradas$runname], "R²")


# Vemos el efecto de cada par?metro sobre el valor absoluto del PBIAS
plot_dotty(par = pars_r1[runs_filtradas$runname, ], crit = pbias_q[runs_filtradas$runname], "PBIAS")


#Vemos el efecto sobre el RMSE
plot_dotty(par = pars_r1[runs_filtradas$runname, ], crit = rmse_q[runs_filtradas$runname], "RMSE")+ylim(c(0.1,0.4))



plot_dotty(par = pars_r1[runs_filtradas$runname, c(1,11)], crit = rmse_q[runs_filtradas$runname], "RMSE")



#### RANKING SIMULACIONES ####

rank_tableGRAN_r1 <- tibble(run = names(nse_q[runs_filtradas$runname]),     
                         nse = nse_q[runs_filtradas$runname],           
                         r2 = r2_q[runs_filtradas$runname],
                         pbias = pbias_q[runs_filtradas$runname], 
                         rmse = rmse_q[runs_filtradas$runname],
                         rank_nse = rank(nse),  # La peor es la 1
                         rank_r2 = rank(r2),    # La peor es la 1
                         rank_rmse = rank(-rmse), # La peor es la primera
                         rank_pbias = rank(-abs(pbias)), # La peor es la 1
                         rank_run = rank_nse+rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
  left_join(., sims_filtradas, "run") %>%     filter(groundw_rate > 0.15, abs(pbias)<10, nse > 0.5) %>% 
  dplyr::arrange(., desc(rank_run))



 tibble(run = names(nse_q),     
                            nse = nse_q,           
                            r2 = r2_q,
                            pbias = pbias_q, 
                            rmse = rmse_q,
                            rank_nse = rank(nse),  # La peor es la 1
                            rank_r2 = rank(r2),    # La peor es la 1
                            rank_rmse = rank(-rmse), # La peor es la primera
                            rank_pbias = rank(-abs(pbias)), # La peor es la 1
                            rank_run = rank_nse+rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
  left_join(., filt_table, "run") %>%         #  filter(groundw_rate > 0.15, abs(pbias)<10, nse > 0.5) %>% 
  dplyr::arrange(., desc(rank_run)) %>% write.csv(., "C:/ASG/Calibracion_Guajaraz/new_ex/R3_RANK_sinfiltrar.csv", quote = F, row.names = F)


 
 tibble(run = names(nse_q[runs_filtradas$runname]),     
        nse = nse_q[runs_filtradas$runname],           
        r2 = r2_q[runs_filtradas$runname],
        pbias = pbias_q[runs_filtradas$runname], 
        rmse = rmse_q[runs_filtradas$runname],
        rank_nse = rank(nse),  # La peor es la 1
        rank_r2 = rank(r2),    # La peor es la 1
        rank_rmse = rank(-rmse), # La peor es la primera
        rank_pbias = rank(-abs(pbias)), # La peor es la 1
        rank_run = rank_nse+rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
   left_join(., sims_filtradas, "run") %>%         #  filter(groundw_rate > 0.15, abs(pbias)<10, nse > 0.5) %>% 
   dplyr::arrange(., desc(rank_run))%>% write.csv(., "C:/ASG/Calibracion_Guajaraz/new_ex/R3_RANK_filtrado.csv", quote = F, row.names = F)
 
 
 
 
 
 
 


rank_tableGRAN_r1 %>% filter(groundw_rate > 0.25, abs(pbias)<10) %>% view(.)

diez_mejores_r1 <- rank_tableGRAN_r1[c(1:10),]  # SELECCIONAMOS LAS 10 SIMULACIONES CON MEJOR RANKING          


bestsims_r1 <- diez_mejores_r1$run %>% tibble(.) %>%  mutate(sim_num = as.numeric(str_remove(., "run_"))) 
bestpars_r1 <-  pars_r1[c(bestsims_r1$sim_num),]

# Ver rango de simulaciones filtradas

q_sim[,-1] %>%  .[, runs_filtradas$runname] %>% 
  cbind(mon_year = q_sim[,1]) %>% 
  pivot_longer(., -c(mon_year)) %>%
  group_by(mon_year) %>% summarise(qmin = min(value), qmax = max(value), qmean = mean(value), qmedian = median(value)) %>% 
  cbind(observed = obs_mensual$flow) %>% 
  ggplot(., aes(x = mon_year))+ 
  geom_ribbon(aes(ymin = qmin, ymax = qmax), color = "skyblue", fill = "skyblue", alpha = 0.6)+
  geom_line(aes(y = observed), color = "blue", linewidth = 0.8)+
  geom_line(aes(y = qmedian), color = "darkgreen", linetype = 2)+
  ylab("Average monthly streamflow (m³/s)")+theme(axis.title.x = element_blank())


# Ver hidrograma 10 mejores

caudales_plot <- q_sim[,-1] %>%  .[, runs_filtradas$runname] %>% 
  cbind(mon_year = q_sim[,1]) %>%
  pivot_longer(., cols = -mon_year, values_to = "Flow", names_to = "Simulation")

diez_mejores_r1 <- rank_tableGRAN_r1[c(1:10),1]  #He seleccionado 5

caudales_diez_mejores <- caudales_plot %>% filter(., Simulation %in% diez_mejores_r1$run)

plot_mejores_caudales <-  obs_mensual %>% mutate(mon_year = ymd(date)) %>% left_join(., caudales_diez_mejores, "mon_year") %>% 
  ggplot(aes(x = mon_year))+geom_line(aes(y = flow), color = "black", linetype = 2, linewidth =  0.8)+
  geom_line(aes(y = caudales_diez_mejores$Flow, colour = caudales_diez_mejores$Simulation))

ggplotly(plot_mejores_caudales)


#### ROUND 2 ####

#### EXTRAER Y CONVERTIR VARIABLES ####

hardcal_guajaraz_2 <- read_rds("C:/ASG/Calibracion_Guajaraz/new_ex/ronda_2_newexe.rds")

# Parametros usados

pars_r2 <-  hardcal_guajaraz_2$parameter$values


# PCP 

precip_anual <- hardcal_guajaraz_2$simulation$precipitacion %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

precip_serie <- precip_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Escorrentia

entradaembalse_mediadiaria_anual <- hardcal_guajaraz_2$simulation$entrada_embalse %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), mean))  

entrada_anual_mm <- entradaembalse_mediadiaria_anual[,-1]*1000 *365 / 367943566
entrada_media_anual_mm <- entrada_anual_mm %>% apply(., 2, mean)

# GW
gwflo_anual <- hardcal_guajaraz_2$simulation$flo_aqu %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

gwflo_medio <- gwflo_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Caudal mensual
cal_med_mensual <- function(x){mean(x)/86400}

q_sim <- hardcal_guajaraz_2$simulation$entrada_embalse %>% group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("run_"), cal_med_mensual)) %>% mutate(mon_year = (paste(`month(date)`, `year(date)`))) %>%
  mutate(mon_year = my(mon_year)) %>% 
  .[, c(1003, 3:1002)] %>% filter(., year(mon_year) > 1982)

# Runoff rate

ratio_escorrentia <- entrada_media_anual_mm / precip_serie %>%  tibble(.)

# Gw rate
ratio_gwflo <- gwflo_medio / entrada_media_anual_mm


#### FILTRAR SIMULACIONES ####
# CRITERIOS : GW < 0.5, RUNOFF < 0.2

names <- colnames(ratio_gwflo)
runoff_rates <- ratio_escorrentia  %>% transpose(.) %>% unlist(.) %>% tibble(runoff_rate = ., run = names)
gwrates <- ratio_gwflo  %>% transpose(.) %>% unlist(.) %>% tibble(groundw_rate = ., run = names)

filt_table <- runoff_rates %>% left_join(., gwrates, "run") %>% .[,c(2,1,3)]

sims_filtradas <- filt_table %>% filter(., runoff_rate < 0.2, groundw_rate < 0.4)

runs_filtradas <- sims_filtradas %>% mutate(runname = as.numeric(str_remove(run, "run_"))) %>% .[,"runname"]

#### CALCULO DE ESTADISTICOS ####

nse_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x, obs_mensual$flow))

pbias_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, obs_mensual$flow))

r2_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, obs_mensual$flow))

rmse_q <- q_sim %>% select(-mon_year) %>% map_dbl(., ~rmse(.x, obs_mensual$flow))     

#### Resultados estadisticos ####


tibble(run = names(nse_q[runs_filtradas$runname]), nse = nse_q[runs_filtradas$runname], r2 = r2_q[runs_filtradas$runname],  
       pbias = pbias_q[runs_filtradas$runname], rmse = rmse_q[runs_filtradas$runname]) %>% 
  pivot_longer(., -run) %>% 
  
  ggplot(aes(x = name, fill = name, y = value)) + geom_violin()+facet_wrap(facets = "name", scales = "free")


#### DOTTY PLOTS ####

plot_dotty <- function(par, crit, crit_label = 'crit', n_col = 3) {
  dotty_tbl <- par %>% 
    mutate(crit = crit) %>% 
    pivot_longer(., cols = -crit, names_to = 'parameter')
  
  ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = crit)) +geom_smooth(aes(x = value, y = crit)) +
    facet_wrap(. ~ parameter, ncol = 3, scales = "free_x") +
    labs(x = 'Change of parameter value', y = crit_label) +
    theme_bw()
}

# Variables 

# Runoff rate
plot_dotty(par = pars_r2[runs_filtradas$runname, ], crit = unlist(ratio_escorrentia[, runs_filtradas$runname]), "Runoff rate")


# Gw flow
plot_dotty(par = pars_r2[runs_filtradas$runname, c(1, 11) ], crit = unlist(ratio_gwflo[, runs_filtradas$runname]), "Groundwater rate")


# Estadisticos


# Vemos el efecto de cada par?metro sobre el NSE
plot_dotty(par = pars_r2[runs_filtradas$runname, ], crit = nse_q[runs_filtradas$runname], "NSE")+ ylim(c(-2, 1))


# Vemos el efecto de cada par?metro sobre el R?
plot_dotty(par = pars_r2[runs_filtradas$runname, ], crit = r2_q[runs_filtradas$runname], "R²")


# Vemos el efecto de cada par?metro sobre el valor absoluto del PBIAS
plot_dotty(par = pars_r2[runs_filtradas$runname, ], crit = pbias_q[runs_filtradas$runname], "PBIAS")


#Vemos el efecto sobre el RMSE
plot_dotty(par = pars_r2[runs_filtradas$runname, c(1, 11) ], crit = rmse_q[runs_filtradas$runname], "RMSE")+ ylim(c(0.2, 0.4))



#### RANKING SIMULACIONES ####

rank_tableGRAN_r2 <- tibble(run = names(nse_q[runs_filtradas$runname]),     
                         nse = nse_q[runs_filtradas$runname],           
                         r2 = r2_q[runs_filtradas$runname],
                         pbias = pbias_q[runs_filtradas$runname], 
                         rmse = rmse_q[runs_filtradas$runname],
                         rank_nse = rank(nse),  # La peor es la 1
                         rank_r2 = rank(r2),    # La peor es la 1
                         rank_rmse = rank(-rmse), # La peor es la primera
                         rank_pbias = rank(-abs(pbias)), # La peor es la 1
                         rank_run = rank_nse+rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
  left_join(., sims_filtradas, "run") %>% 
  dplyr::arrange(., desc(rank_run)) %>%  filter(groundw_rate > 0.15, abs(pbias)<10, nse > 0.5) 


diez_mejores_r2 <- rank_tableGRAN_r2[c(1:10),]  # SELECCIONAMOS LAS 10 SIMULACIONES CON MEJOR RANKING          

bestsims_r2 <- diez_mejores_r2$run %>% tibble(.) %>%  mutate(sim_num = as.numeric(str_remove(., "run_"))) 
bestpars_r2 <-  pars_r2[c(bestsims_r2$sim_num),]

# Ver rango de simulaciones filtradas

q_sim[,-1] %>%  .[, runs_filtradas$runname] %>% 
  cbind(mon_year = q_sim[,1]) %>% 
  pivot_longer(., -c(mon_year)) %>%
  group_by(mon_year) %>% summarise(qmin = min(value), qmax = max(value), qmean = mean(value), qmedian = median(value)) %>% 
  cbind(observed = obs_mensual$flow) %>% 
  ggplot(., aes(x = mon_year))+ 
  geom_ribbon(aes(ymin = qmin, ymax = qmax), color = "skyblue", fill = "skyblue", alpha = 0.6)+
  geom_line(aes(y = observed), color = "blue", linewidth = 0.8)+
  geom_line(aes(y = qmedian), color = "darkgreen", linetype = 2)+
  ylab("Average monthly streamflow (m³/s)")+theme(axis.title.x = element_blank())


# Ver hidrograma 10 mejores

caudales_plot <- q_sim[,-1] %>%  .[, runs_filtradas$runname] %>% 
  cbind(mon_year = q_sim[,1]) %>%
  pivot_longer(., cols = -mon_year, values_to = "Flow", names_to = "Simulation")

diez_mejores_r2 <- rank_tableGRAN_r2[c(1:10),1]  

caudales_diez_mejores <- caudales_plot %>% filter(., Simulation %in% diez_mejores_r2$run)

plot_mejores_caudales <-  obs_mensual %>% mutate(mon_year = ymd(date)) %>% left_join(., caudales_diez_mejores, "mon_year") %>% 
  ggplot(aes(x = mon_year))+geom_line(aes(y = flow), color = "black", linetype = 2, linewidth =  0.8)+
  geom_line(aes(y = caudales_diez_mejores$Flow, colour = caudales_diez_mejores$Simulation))

ggplotly(plot_mejores_caudales)

pars[12,]



#### ROUND 3 ####

#### EXTRAER Y CONVERTIR VARIABLES ####

hardcal_guajaraz_3 <- read_rds("C:/ASG/Calibracion_Guajaraz/new_ex/ronda_3_newexe.rds")

# Parametros usados

pars_r3 <-  hardcal_guajaraz_3$parameter$values


# PCP 

precip_anual <- hardcal_guajaraz_3$simulation$precipitacion %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

precip_serie <- precip_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Escorrentia

entradaembalse_mediadiaria_anual <- hardcal_guajaraz_3$simulation$entrada_embalse %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), mean))  

entrada_anual_mm <- entradaembalse_mediadiaria_anual[,-1]*1000 *365 / 367943566
entrada_media_anual_mm <- entrada_anual_mm %>% apply(., 2, mean)

# GW
gwflo_anual <- hardcal_guajaraz_3$simulation$flo_aqu %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

gwflo_medio <- gwflo_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Caudal mensual
cal_med_mensual <- function(x){mean(x)/86400}

q_sim <- hardcal_guajaraz_3$simulation$entrada_embalse %>% group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("run_"), cal_med_mensual)) %>% mutate(mon_year = (paste(`month(date)`, `year(date)`))) %>%
  mutate(mon_year = my(mon_year)) %>% 
  .[, c(1003, 3:1002)] %>% filter(., year(mon_year) > 1982)

# Runoff rate

ratio_escorrentia <- entrada_media_anual_mm / precip_serie %>%  tibble(.)

# Gw rate
ratio_gwflo <- gwflo_medio / entrada_media_anual_mm


#### FILTRAR SIMULACIONES ####
# CRITERIOS : GW < 0.5, RUNOFF < 0.2

names <- colnames(ratio_gwflo)
runoff_rates <- ratio_escorrentia  %>% transpose(.) %>% unlist(.) %>% tibble(runoff_rate = ., run = names)
gwrates <- ratio_gwflo  %>% transpose(.) %>% unlist(.) %>% tibble(groundw_rate = ., run = names)

filt_table <- runoff_rates %>% left_join(., gwrates, "run") %>% .[,c(2,1,3)]

sims_filtradas <- filt_table %>% filter(., runoff_rate < 0.2, groundw_rate < 0.4)

runs_filtradas <- sims_filtradas %>% mutate(runname = as.numeric(str_remove(run, "run_"))) %>% .[,"runname"]

#### CALCULO DE ESTADISTICOS ####

nse_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x, obs_mensual$flow))

pbias_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, obs_mensual$flow))

r2_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, obs_mensual$flow))

rmse_q <- q_sim %>% select(-mon_year) %>% map_dbl(., ~rmse(.x, obs_mensual$flow))     

#### Resultados estadisticos ####


tibble(run = names(nse_q[runs_filtradas$runname]), nse = nse_q[runs_filtradas$runname], r2 = r2_q[runs_filtradas$runname],  
       pbias = pbias_q[runs_filtradas$runname], rmse = rmse_q[runs_filtradas$runname]) %>% 
  pivot_longer(., -run) %>% 
  
  ggplot(aes(x = name, fill = name, y = value)) + geom_violin()+facet_wrap(facets = "name", scales = "free")


#### DOTTY PLOTS ####

plot_dotty <- function(par, crit, crit_label = 'crit', n_col = 3) {
  dotty_tbl <- par %>% 
    mutate(crit = crit) %>% 
    pivot_longer(., cols = -crit, names_to = 'parameter')
  
  ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = crit)) +geom_smooth(aes(x = value, y = crit)) +
    facet_wrap(. ~ parameter, ncol = 3, scales = "free_x") +
    labs(x = 'Change of parameter value', y = crit_label) +
    theme_bw()
}

# Variables 

# Runoff rate
plot_dotty(par = pars_r3[runs_filtradas$runname, ], crit = unlist(ratio_escorrentia[, runs_filtradas$runname]), "Runoff rate")


# Gw flow
plot_dotty(par = pars_r3[runs_filtradas$runname, c(1, 11) ], crit = unlist(ratio_gwflo[, runs_filtradas$runname]), "Groundwater rate")


# Estadisticos


# Vemos el efecto de cada par?metro sobre el NSE
plot_dotty(par = pars_r3[runs_filtradas$runname, ], crit = nse_q[runs_filtradas$runname], "NSE")+ ylim(c(-2, 1))


# Vemos el efecto de cada par?metro sobre el R?
plot_dotty(par = pars_r3[runs_filtradas$runname, ], crit = r2_q[runs_filtradas$runname], "R²")


# Vemos el efecto de cada par?metro sobre el valor absoluto del PBIAS
plot_dotty(par = pars_r3[runs_filtradas$runname, ], crit = pbias_q[runs_filtradas$runname], "PBIAS")


#Vemos el efecto sobre el RMSE
plot_dotty(par = pars_r3[runs_filtradas$runname, ], crit = rmse_q[runs_filtradas$runname], "RMSE")



#### RANKING SIMULACIONES ####

rank_tableGRAN_r3 <- tibble(run = names(nse_q[runs_filtradas$runname]),     
                         nse = nse_q[runs_filtradas$runname],           
                         r2 = r2_q[runs_filtradas$runname],
                         pbias = pbias_q[runs_filtradas$runname], 
                         rmse = rmse_q[runs_filtradas$runname],
                         rank_nse = rank(nse),  # La peor es la 1
                         rank_r2 = rank(r2),    # La peor es la 1
                         rank_rmse = rank(-rmse), # La peor es la primera
                         rank_pbias = rank(-abs(pbias)), # La peor es la 1
                         rank_run = rank_nse + rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
  left_join(., sims_filtradas, "run") %>% 
  dplyr::arrange(., desc(rank_run)) %>% filter(., groundw_rate > 0.15, abs(pbias) < 10, nse > 0.6)


diez_mejores_r3 <- rank_tableGRAN_r3[c(1:10),]  # SELECCIONAMOS LAS 10 SIMULACIONES CON MEJOR RANKING          
bestsims_r3 <- diez_mejores_r3$run %>% tibble(.) %>%  mutate(sim_num = as.numeric(str_remove(., "run_"))) 
bestpars_r3 <-  pars_r3[c(bestsims_r3$sim_num),]

# Ver rango de simulaciones filtradas

q_sim[,-1] %>%  .[, runs_filtradas$runname] %>% 
  cbind(mon_year = q_sim[,1]) %>% 
  pivot_longer(., -c(mon_year)) %>%
  group_by(mon_year) %>% summarise(qmin = min(value), qmax = max(value), qmean = mean(value), qmedian = median(value)) %>% 
  cbind(observed = obs_mensual$flow) %>% 
  ggplot(., aes(x = mon_year))+ 
  geom_ribbon(aes(ymin = qmin, ymax = qmax), color = "skyblue", fill = "skyblue", alpha = 0.6)+
  geom_line(aes(y = observed), color = "blue", linewidth = 0.8)+
  geom_line(aes(y = qmedian), color = "darkgreen", linetype = 2)+
  ylab("Average monthly streamflow (m³/s)")+theme(axis.title.x = element_blank())


# Ver hidrograma 10 mejores

caudales_plot <- q_sim[,-1] %>%  .[, runs_filtradas$runname] %>% 
  cbind(mon_year = q_sim[,1]) %>%
  pivot_longer(., cols = -mon_year, values_to = "Flow", names_to = "Simulation")

diez_mejores_r3 <- rank_tableGRAN[c(1:10),1]  #He seleccionado 5

caudales_diez_mejores <- caudales_plot %>% filter(., Simulation %in% diez_mejores_r3$run)

plot_mejores_caudales <-  obs_mensual %>% mutate(mon_year = ymd(date)) %>% left_join(., caudales_diez_mejores, "mon_year") %>% 
  ggplot(aes(x = mon_year))+geom_line(aes(y = flow), color = "black", linetype = 2, linewidth =  0.8)+
  geom_line(aes(y = caudales_diez_mejores$Flow, colour = caudales_diez_mejores$Simulation))


#### VALIDACION DE LAS MEJORES SIMULACIONES DE CADA RONDA


# Ronda 1 

pars_r1_best <- pars_r1[c(696, 966, 254),]

# Ronda 2

pars_r2_best <- pars_r2[c(415, 897, 225, 392),]

# Ronda 3

pars_r3_best <- pars_r3[as.numeric(str_remove(rank_tableGRAN_r3$run, "run_")),]



CALIBRA_TABLE <- rank_tableGRAN_r1 %>% rbind(., rank_tableGRAN_r2) %>% rbind(., rank_tableGRAN_r3) %>% mutate(round = c(1,1,1,rep(2,4), rep(3,76))) %>% 
    select(., -starts_with("rank")) %>% mutate_at(., c( "nse", "r2", "pbias",  "rmse", "runoff_rate", "groundw_rate"), ~round(., 3)) %>% 
  mutate(run_val = c(1:83))

CALIBRA_TABLE_ORD <- CALIBRA_TABLE %>% mutate(rank_nse = rank(nse),  # La peor es la 1
                         rank_r2 = rank(r2),    # La peor es la 1
                         rank_rmse = rank(-rmse), # La peor es la primera
                         rank_pbias = rank(-abs(pbias)), # La peor es la 1
                         rank_run = rank_nse + rank_r2 + rank_pbias+rank_rmse) %>%   # La que de mas alto en rank_run es la mejor
  dplyr::arrange(., desc(rank_run)) %>% rename(trank_run = rank_run)




param_valid <- pars_r1_best %>% rbind(., pars_r2_best) %>% rbind(pars_r3_best)
colnames(param_valid) <- colnames(limites_parametros_r1)




VALIDATION <- run_swatplus(project_path = demo_path, 
                        output =list(caudal = define_output(file = 'channel_sd',
                                                            variable = 'flo_out', 
                                                            unit = 5),
                                     entrada_embalse = define_output(file = 'reservoir',
                                                                     variable = 'flo_in',
                                                                     unit = 1),
                                     salida_embalse = define_output(file = 'reservoir',
                                                                    variable = 'flo_out',
                                                                    unit = 1),
                                     almacenamiento_embalse = define_output(file = 'reservoir',
                                                                            variable = 'flo_stor',
                                                                            unit = 1),
                                     precipitacion = define_output(file = 'basin_wb',
                                                                   variable = 'precip',
                                                                   unit = 1), 
                                     evaporacion = define_output(file = 'basin_wb',
                                                                 variable = 'et',
                                                                 unit = 1), 
                                     percolacion = define_output(file = 'basin_wb',
                                                                 variable = 'perc',
                                                                 unit = 1), 
                                     flujo_lateral = define_output(file = 'basin_wb',
                                                                   variable = 'latq_cha',
                                                                   unit = 1), 
                                     flujo_superficial = define_output(file = 'basin_wb',
                                                                       variable = 'surq_cha',
                                                                       unit = 1), 
                                     flo_aqu = define_output(file = 'basin_aqu',
                                                             variable = 'flo_cha',
                                                             unit = 1),             
                                     recarga_aqu = define_output(file = 'basin_aqu',
                                                                 variable = 'rchrg',
                                                                 unit = 1),  
                                     seep = define_output(file = 'basin_aqu',
                                                          variable = 'seep',
                                                          unit = 1),  
                                     evapo_aqu = define_output(file = 'basin_aqu',
                                                               variable = 'revap',
                                                               unit = 1)), 
                        
                        parameter = param_valid, 
                        start_date = 19770101, 
                        end_date = 19881231, 
                        years_skip = 5, 
                        n_thread = 60)

write_rds(VALIDATION, "C:/ASG/Calibracion_Guajaraz/new_ex/validation_long.rds")


#### RESULTADOS VALIDACION CORTA ####

# Causal observado en periodo validacion

obs_mensual<- read_csv("C:/ASG/Calibracion_Guajaraz/observed_Guajaraz_Toolbox.csv") %>% 
  mutate(date = my(Date)) %>% mutate(flow = as.numeric(Flow)) %>% 
  filter(., year(date) %in% c(1986:1988)) %>%  .[, c(3:4)] %>% tibble(.)  


#### EXTRAER Y CONVERTIR VARIABLES ####
ronda_val_sort <- read_rds("C:/ASG/Calibracion_Guajaraz/new_ex/validation_sort.rds")


# Parametros usados

pars_val_sort <-  ronda_val_sort$parameter$values


# PCP 

precip_anual <- ronda_val_sort$simulation$precipitacion %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

precip_serie <- precip_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Escorrentia

entradaembalse_mediadiaria_anual <- ronda_val_sort$simulation$entrada_embalse %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), mean))  

entrada_anual_mm <- entradaembalse_mediadiaria_anual[,-1]*1000 *365 / 367943566
entrada_media_anual_mm <- entrada_anual_mm %>% apply(., 2, mean)

# GW
gwflo_anual <- ronda_val_sort$simulation$flo_aqu %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(across(starts_with("run_"), sum))

gwflo_medio <- gwflo_anual %>% 
  summarise(across(starts_with("run_"), mean))

# Caudal mensual
cal_med_mensual <- function(x){mean(x)/86400}

q_sim <- ronda_val_sort$simulation$entrada_embalse %>% group_by(year(date), month(date)) %>% 
  summarise(across(starts_with("run_"), cal_med_mensual)) %>% mutate(mon_year = (paste(`month(date)`, `year(date)`))) %>%
  mutate(mon_year = my(mon_year)) %>% 
  .[, c(86, 3:85)]# %>% filter(., year(mon_year)>1982)

# Runoff rate

ratio_escorrentia <- entrada_media_anual_mm / precip_serie %>%  tibble(.)

# Gw rate
ratio_gwflo <- gwflo_medio / entrada_media_anual_mm


#### FILTRAR SIMULACIONES ####
# CRITERIOS : GW < 0.5, RUNOFF < 0.2

names <- colnames(ratio_gwflo)
runoff_rates <- ratio_escorrentia  %>% transpose(.) %>% unlist(.) %>% tibble(runoff_rate = ., run = names)
gwrates <- ratio_gwflo  %>% transpose(.) %>% unlist(.) %>% tibble(groundw_rate = ., run = names)

filt_table <- runoff_rates %>% left_join(., gwrates, "run") %>% .[,c(2,1,3)]

sims_filtradas <- filt_table %>% filter(., runoff_rate < 0.2, groundw_rate < 0.4)

runs_filtradas <- sims_filtradas %>% mutate(runname = as.numeric(str_remove(run, "run_"))) %>% .[,"runname"]

#### CALCULO DE ESTADISTICOS ####

nse_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x, obs_mensual$flow))

pbias_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, obs_mensual$flow))

r2_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, obs_mensual$flow))

rmse_q <- q_sim %>% select(-mon_year) %>% map_dbl(., ~rmse(.x, obs_mensual$flow))     

#### Resultados estadisticos ####


tibble(run = names(nse_q), nse = nse_q, r2 = r2_q,  
       pbias = pbias_q, rmse = rmse_q) %>% 
  pivot_longer(., -run) %>% 
  
  ggplot(aes(x = name, fill = name, y = value)) + geom_violin()+facet_wrap(facets = "name", scales = "free")


#### DOTTY PLOTS ####

plot_dotty <- function(par, crit, crit_label = 'crit', n_col = 3) {
  dotty_tbl <- par %>% 
    mutate(crit = crit) %>% 
    pivot_longer(., cols = -crit, names_to = 'parameter')
  
  ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = crit)) +geom_smooth(aes(x = value, y = crit)) +
    facet_wrap(. ~ parameter, ncol = 3, scales = "free_x") +
    labs(x = 'Change of parameter value', y = crit_label) +
    theme_bw()
}

# Variables 

# Runoff rate
plot_dotty(par = ronda_val_sort[runs_filtradas$runname, ], crit = unlist(ratio_escorrentia[, runs_filtradas$runname]), "Runoff rate")


# Gw flow
plot_dotty(par = ronda_val_sort[runs_filtradas$runname, ], crit = unlist(ratio_gwflo[, runs_filtradas$runname]), "Groundwater rate")


# Estadisticos


# Vemos el efecto de cada par?metro sobre el NSE
plot_dotty(par = ronda_val_sort[runs_filtradas$runname, ], crit = nse_q, "NSE")+ ylim(c(-2, 1))


# Vemos el efecto de cada par?metro sobre el R?
plot_dotty(par = ronda_val_sort[runs_filtradas$runname, ], crit = r2_q, "R²")


# Vemos el efecto de cada par?metro sobre el valor absoluto del PBIAS
plot_dotty(par = ronda_val_sort[runs_filtradas$runname, ], crit = pbias_q, "PBIAS")


#Vemos el efecto sobre el RMSE
plot_dotty(par = ronda_val_sort[runs_filtradas$runname, ], crit = rmse_q, "RMSE")


#### RANKING SIMULACIONES ####

rank_tableGRAN_valsort <- tibble(run_val = names(nse_q),     
                            nse_val = nse_q,           
                            r2_val = r2_q,
                            pbias_val = pbias_q, 
                            rmse_val = rmse_q,
                            rank_nse_val = rank(nse_val),  # La peor es la 1
                            rank_r2_val = rank(r2_val),    # La peor es la 1
                            rank_rmse_val = rank(-rmse_val), # La peor es la primera
                            rank_pbias_val = rank(-abs(pbias_val)), # La peor es la 1
                            trank_run_val = rank_nse_val+rank_r2_val + rank_pbias_val+rank_rmse_val) %>%   # La que de mas alto en rank_run es la mejor
  left_join(., filt_table, c("run_val" = "run") ) %>% rename(runoff_rate_val = runoff_rate, groundw_rate_val = groundw_rate) %>% arrange(., -trank_run_val)



CALIBRA_TABLE_ORD %>% write.csv(., "calibration_table.csv", quote = F, row.names = F)

CALIBRA_TABLE_ORD <- read.csv("C:/ASG/Calibracion_Guajaraz/new_ex/calibration_table.csv")

rank_tableGRAN_valsort  %>% select(., -c(starts_with("rank"))) %>%
  mutate_at(., c("nse_val", "r2_val", "pbias_val" ,"rmse_val",  "runoff_rate_val", "groundw_rate_val"), ~round(., 3)) %>% write.csv(., "C:/ASG/Calibracion_Guajaraz/new_ex/validation_table.csv", quote = F, row.names = F)

VALIDA_TABLE <- rank_tableGRAN_valsort  %>% select(., -c(starts_with("rank"))) %>% mutate(run_val = as.numeric(str_remove(run_val , "run_"))) %>% 
  mutate_at(., c("nse_val", "r2_val", "pbias_val" ,"rmse_val",  "runoff_rate_val", "groundw_rate_val"), ~round(., 3))



CALIBRA_TABLE_ORD %>%    
  left_join(., VALIDA_TABLE, "run_val" ) %>% 
  mutate_at(., c( "nse","r2", "pbias" , "rmse", "groundw_rate", "nse_val", "r2_val", "pbias_val", "rmse_val", "groundw_rate_val"), ~ round(., 3)) %>% 
  select(., -starts_with("runoff")) %>% 
  select(., -starts_with("rank"))%>% view(.)
  
  write.csv(., "C:/ASG/Calibracion_Guajaraz/new_ex/final_table.csv", quote = F, row.names = F)


tab_final <- CALIBRA_TABLE_ORD %>%   arrange(., -trank_run) %>% 
  left_join(., VALIDA_TABLE, "run_val" ) %>% 
  mutate_at(., c( "nse","r2", "pbias" , "rmse", "groundw_rate", "nse_val", "r2_val", "pbias_val", "rmse_val", "groundw_rate_val"), ~ round(., 3)) %>% 
  select(., -starts_with("runoff")) %>% 
  select(., -starts_with("rank"))




tab_final %>% filter(., nse > 0.6, nse_val > 0.6, abs(pbias) < 15, abs(pbias_val) < 15)








rank_tableGRAN_valsort %>% select(., -starts_with("rank")) %>% mutate(., run_val = c(1:83) ) %>% 
  right_join(CALIBRA_TABLE, ., "run_val") %>% select(., -starts_with("rank")) %>% 
  mutate_at(., c("nse_val", "r2_val", "pbias_val", "rmse_val", "runoff_rate_val" ,"groundw_rate_val"), ~round(., 3)) %>% 
  filter(., abs(pbias_val) <10, nse_val > 0.5 )
 mutate(rank_cal_nse = rank(nse), 
        rank_cal_r2 = rank(r2),
        rank_cal_pbia = rank(-abs(pbias)), 
        rank_cal_rmse = rank(-rmse),
        rank_val_nse = rank(nse_val), 
        rank_val_r2 = rank(r2_val),
        rank_val_pbia = rank(-abs(pbias_val)), 
        rank_val_rmse = rank(-rmse_val),
        mult_rank_cal = (rank_cal_nse+rank_cal_r2+rank_cal_pbia+rank_cal_rmse)) %>% 
 arrange(., -mult_rank_abs) %>% 
  select(., -starts_with("rank")) -
  


 # Ver hidrograma 10 mejores
  
  caudales_plot <- q_sim %>%# select(.,run_0843,run_0564 , run_0668, mon_year     ) %>%  #select(., run_30,run_34 , run_47, mon_year     ) %>% 
    #cbind(mon_year = q_sim[,1]) %>%
    pivot_longer(., cols = -mon_year, values_to = "Flow", names_to = "Simulation")
  
  diez_mejores_r3 <- rank_tableGRAN_valsort[c(1:10),] #He seleccionado 5
  
  caudales_diez_mejores <- caudales_plot %>% filter(., Simulation %in% diez_mejores_r3$run_val)
  
  plot_mejores_caudales <-  obs_mensual %>% mutate(mon_year = ymd(date)) %>% left_join(., caudales_diez_mejores, "mon_year") %>% 
    ggplot(aes(x = mon_year))+geom_line(aes(y = flow), color = "black", linetype = 2, linewidth =  0.8)+
    geom_line(aes(y = caudales_diez_mejores$Flow, colour = caudales_diez_mejores$Simulation))
  
  
  
  ggplotly(plot_mejores_caudales)
  

  
  q_sim[,c(1,11,31)] %>% cbind(obs_flow = obs_mensual$flow) %>% ggplot(., aes(x = mon_year))+geom_line(aes(y = obs_flow))+
    geom_line(aes(y = run_10), color = "green")+
    geom_line(aes(y = run_30), color = "blue")
  
  
  pars_val_sort[10,]
  
  

 
 #### RESULTADOS VALIDACION LARGA #####
 
  # Causal observado en periodo validacion
  
  obs_mensual<- read_csv("C:/ASG/Calibracion_Guajaraz/observed_Guajaraz_Toolbox.csv") %>% 
    mutate(date = my(Date)) %>% mutate(flow = as.numeric(Flow)) %>% 
    filter(., year(date) %in% c(1983:1988)) %>%  .[, c(3:4)] %>% tibble(.)  
  
  
  #### EXTRAER Y CONVERTIR VARIABLES ####
  ronda_val_long <- read_rds("C:/ASG/Calibracion_Guajaraz/new_ex/validation_long.rds")
  
  
  # Parametros usados
  
  pars_val_long <-  ronda_val_long$parameter$values
  
  
  # PCP 
  
  precip_anual <- ronda_val_long$simulation$precipitacion %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    summarise(across(starts_with("run_"), sum))
  
  precip_serie <- precip_anual %>% 
    summarise(across(starts_with("run_"), mean))
  
  # Escorrentia
  
  entradaembalse_mediadiaria_anual <- ronda_val_long$simulation$entrada_embalse %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    summarise(across(starts_with("run_"), mean))  
  
  entrada_anual_mm <- entradaembalse_mediadiaria_anual[,-1]*1000 *365 / 367943566
  entrada_media_anual_mm <- entrada_anual_mm %>% apply(., 2, mean)
  
  # GW
  gwflo_anual <- ronda_val_long$simulation$flo_aqu %>% 
    mutate(year = year(date)) %>% 
    group_by(year) %>% 
    summarise(across(starts_with("run_"), sum))
  
  gwflo_medio <- gwflo_anual %>% 
    summarise(across(starts_with("run_"), mean))
  
  # Caudal mensual
  cal_med_mensual <- function(x){mean(x)/86400}
  
  q_sim <- ronda_val_long$simulation$entrada_embalse %>% group_by(year(date), month(date)) %>% 
    summarise(across(starts_with("run_"), cal_med_mensual)) %>% mutate(mon_year = (paste(`month(date)`, `year(date)`))) %>%
    mutate(mon_year = my(mon_year)) %>% 
    .[, c(86, 3:85)] %>% filter(., year(mon_year)>1982)
  
  # Runoff rate
  
  ratio_escorrentia <- entrada_media_anual_mm / precip_serie %>%  tibble(.)
  
  # Gw rate
  ratio_gwflo <- gwflo_medio / entrada_media_anual_mm
  
  
  #### FILTRAR SIMULACIONES ####
  # CRITERIOS : GW < 0.5, RUNOFF < 0.2
  
  names <- colnames(ratio_gwflo)
  runoff_rates <- ratio_escorrentia  %>% transpose(.) %>% unlist(.) %>% tibble(runoff_rate = ., run = names)
  gwrates <- ratio_gwflo  %>% transpose(.) %>% unlist(.) %>% tibble(groundw_rate = ., run = names)
  
  filt_table <- runoff_rates %>% left_join(., gwrates, "run") %>% .[,c(2,1,3)]
  
  sims_filtradas <- filt_table %>% filter(., runoff_rate < 0.2, groundw_rate < 0.4)
  
  runs_filtradas <- sims_filtradas %>% mutate(runname = as.numeric(str_remove(run, "run_"))) %>% .[,"runname"]
  
  #### CALCULO DE ESTADISTICOS ####
  
  nse_q <- q_sim %>% 
    select(-mon_year) %>% 
    map_dbl(., ~NSE(.x, obs_mensual$flow))
  
  pbias_q <- q_sim %>% 
    select(-mon_year) %>% 
    map_dbl(., ~pbias(.x, obs_mensual$flow))
  
  r2_q <- q_sim %>% 
    select(-mon_year) %>% 
    map_dbl(., ~rsq_r2(.x, obs_mensual$flow))
  
  rmse_q <- q_sim %>% select(-mon_year) %>% map_dbl(., ~rmse(.x, obs_mensual$flow))     
  
  #### Resultados estadisticos ####
  
  
  tibble(run = names(nse_q), nse = nse_q, r2 = r2_q,  
         pbias = pbias_q, rmse = rmse_q) %>% 
    pivot_longer(., -run) %>% 
    
    ggplot(aes(x = name, fill = name, y = value)) + geom_violin()+facet_wrap(facets = "name", scales = "free")
  
  
  #### DOTTY PLOTS ####
  
  plot_dotty <- function(par, crit, crit_label = 'crit', n_col = 3) {
    dotty_tbl <- par %>% 
      mutate(crit = crit) %>% 
      pivot_longer(., cols = -crit, names_to = 'parameter')
    
    ggplot(data = dotty_tbl) +
      geom_point(aes(x = value, y = crit)) +geom_smooth(aes(x = value, y = crit)) +
      facet_wrap(. ~ parameter, ncol = 3, scales = "free_x") +
      labs(x = 'Change of parameter value', y = crit_label) +
      theme_bw()
  }
  
  # Variables 
  
  # Runoff rate
  plot_dotty(par = pars_val_long[runs_filtradas$runname, ], crit = unlist(ratio_escorrentia[, runs_filtradas$runname]), "Runoff rate")
  
  
  # Gw flow
  plot_dotty(par = pars_val_long[runs_filtradas$runname, ], crit = unlist(ratio_gwflo[, runs_filtradas$runname]), "Groundwater rate")
  
  
  # Estadisticos
  
  
  # Vemos el efecto de cada par?metro sobre el NSE
  plot_dotty(par = ronda_val_long[runs_filtradas$runname, ], crit = nse_q, "NSE")+ ylim(c(-2, 1))
  
  
  # Vemos el efecto de cada par?metro sobre el R?
  plot_dotty(par = ronda_val_long[runs_filtradas$runname, ], crit = r2_q, "R²")
  
  
  # Vemos el efecto de cada par?metro sobre el valor absoluto del PBIAS
  plot_dotty(par = ronda_val_long[runs_filtradas$runname, ], crit = pbias_q, "PBIAS")
  
  
  #Vemos el efecto sobre el RMSE
  plot_dotty(par = ronda_val_long[runs_filtradas$runname, ], crit = rmse_q, "RMSE")
  
  
  #### RANKING SIMULACIONES ####
  
  rank_tableGRAN_val_long <- tibble(run_val = names(nse_q),     
                                   nse_val = nse_q,           
                                   r2_val = r2_q,
                                   pbias_val = pbias_q, 
                                   rmse_val = rmse_q,
                                   rank_nse_val = rank(nse_val),  # La peor es la 1
                                   rank_r2_val = rank(r2_val),    # La peor es la 1
                                   rank_rmse_val = rank(-rmse_val), # La peor es la primera
                                   rank_pbias_val = rank(-abs(pbias_val)), # La peor es la 1
                                   rank_run_val = rank_nse_val+rank_r2_val + rank_pbias_val+rank_rmse_val) %>%   # La que de mas alto en rank_run es la mejor
    left_join(., filt_table, c("run_val" = "run") ) %>% rename(runoff_rate_val = runoff_rate, groundw_rate_val = groundw_rate) %>% arrange(., -rank_run_val)
  
  
  
  CALIBRA_TABLE
  
  
  
  
  rank_tableGRAN_valsort %>% select(., -starts_with("rank")) %>% mutate(., run_val = c(1:83) ) %>% 
    right_join(CALIBRA_TABLE, ., "run_val") %>% select(., -starts_with("rank")) %>% 
    mutate_at(., c("nse_val", "r2_val", "pbias_val", "rmse_val", "runoff_rate_val" ,"groundw_rate_val"), ~round(., 3)) %>% 
  filter(., abs(pbias_val) <10, nse_val > 0.5 ) %>% 
 mutate(rank_cal_nse = rank(nse), 
        rank_cal_r2 = rank(r2),
        rank_cal_pbia = rank(-abs(pbias)), 
        rank_cal_rmse = rank(-rmse),
        rank_val_nse = rank(nse_val), 
        rank_val_r2 = rank(r2_val),
        rank_val_pbia = rank(-abs(pbias_val)), 
        rank_val_rmse = rank(-rmse_val),
        mult_rank_abs = ((rank_cal_nse + rank_val_nse)/2) + 
          ((rank_cal_r2+rank_val_r2)/2)+
          ((rank_cal_pbia+rank_val_pbia)/2) + 
          ((rank_cal_rmse+rank_val_rmse)/2)) %>% 
    arrange(., -mult_rank_abs) %>%
    select(., -starts_with("rank")) 
  
  
 
 
#### BEST PARAMETERS SET AND SIMULATION####
  
  
bestpars <- pars_val_sort[10,]
colnames(bestpars) <- colnames(limites_parametros_r1)
  
  
 

 

bestsim <- run_swatplus(project_path = demo_path, 
                           output =list(caudal = define_output(file = 'channel_sd',
                                                               variable = 'flo_out', 
                                                               unit = 5),
                                        entrada_embalse = define_output(file = 'reservoir',
                                                                        variable = 'flo_in',
                                                                        unit = 1),
                                        salida_embalse = define_output(file = 'reservoir',
                                                                       variable = 'flo_out',
                                                                       unit = 1),
                                        almacenamiento_embalse = define_output(file = 'reservoir',
                                                                               variable = 'flo_stor',
                                                                               unit = 1),
                                        precipitacion = define_output(file = 'basin_wb',
                                                                      variable = 'precip',
                                                                      unit = 1), 
                                        evaporacion = define_output(file = 'basin_wb',
                                                                    variable = 'et',
                                                                    unit = 1), 
                                        percolacion = define_output(file = 'basin_wb',
                                                                    variable = 'perc',
                                                                    unit = 1), 
                                        flujo_lateral = define_output(file = 'basin_wb',
                                                                      variable = 'latq_cha',
                                                                      unit = 1), 
                                        flujo_superficial = define_output(file = 'basin_wb',
                                                                          variable = 'surq_cha',
                                                                          unit = 1), 
                                        flo_aqu = define_output(file = 'basin_aqu',
                                                                variable = 'flo_cha',
                                                                unit = 1),             
                                        recarga_aqu = define_output(file = 'basin_aqu',
                                                                    variable = 'rchrg',
                                                                    unit = 1),  
                                        seep = define_output(file = 'basin_aqu',
                                                             variable = 'seep',
                                                             unit = 1),  
                                        evapo_aqu = define_output(file = 'basin_aqu',
                                                                  variable = 'revap',
                                                                  unit = 1)), 
                           
                           parameter = bestpars, 
                           start_date = 19770101, 
                           end_date = 19881231, 
                           years_skip = 5, 
                           n_thread = 60)

write_rds(bestsim, "C:/ASG/Calibracion_Guajaraz/new_ex/bestsim.rds")

 
 
# Causal observado en periodo validacion

obs_mensual<- read_csv("C:/ASG/Calibracion_Guajaraz/observed_Guajaraz_Toolbox.csv") %>% 
  mutate(date = my(Date)) %>% mutate(flow = as.numeric(Flow)) %>% 
  filter(., year(date) %in% c(1983:1988)) %>%  .[, c(3:4)] %>% tibble(.)  


#### EXTRAER Y CONVERTIR VARIABLES ####
bestsim <- read_rds("C:/ASG/Calibracion_Guajaraz/new_ex/bestsim.rds")


# Parametros usados

pars_best <-  bestsim$parameter$values


# PCP 

precip_anual <- tibble(date = bestsim$simulation$date,precipitacion = bestsim$simulation$precipitacion) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(pcp_y = sum(precipitacion))

precip_serie <- precip_anual %>% 
  summarise(pcp_aa = mean(pcp_y))

# Escorrentia

entradaembalse_mediadiaria_anual <- tibble(date = bestsim$simulation$date, entrada_embalse = bestsim$simulation$entrada_embalse) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(entradaembalse_mediadiaria_anual = mean(entrada_embalse))

entrada_anual_mm <- entradaembalse_mediadiaria_anual[,-1]*1000 *365 / 367943566
entrada_media_anual_mm <- entrada_anual_mm %>% apply(., 2, mean)

# GW
gwflo_anual <- tibble(date = bestsim$simulation$date, flo_aqu  = bestsim$simulation$flo_aqu) %>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(floaqu_yr = sum(flo_aqu))

gwflo_medio <- gwflo_anual %>% 
  summarise(flo_aqu_aa = mean(floaqu_yr))

# Recarga 

recarg_anual <- tibble(date = bestsim$simulation$date, recarga  = bestsim$simulation$recarga_aqu)%>% 
  mutate(year = year(date)) %>% 
  group_by(year) %>% 
  summarise(floaqu_yr = sum(recarga))

gwflo_medio <- recarg_anual %>% 
  summarise(flo_aqu_aa = mean(floaqu_yr))




# Caudal mensual
cal_med_mensual <- function(x){mean(x)/86400}

q_sim <- tibble(date = bestsim$simulation$date, entrada_embalse = bestsim$simulation$entrada_embalse) %>% group_by(year(date), month(date)) %>% 
  summarise(entrada_emb_men = cal_med_mensual(entrada_embalse)) %>% mutate(mon_year = (paste(`month(date)`, `year(date)`))) %>%
  mutate(mon_year = my(mon_year)) %>% 
  .[, c(4, 3)] %>% filter(., year(mon_year)>1982)

# Runoff rate

ratio_escorrentia <- entrada_media_anual_mm / precip_serie %>%  tibble(.)

# Gw rate
ratio_gwflo <- gwflo_medio / entrada_media_anual_mm




#### CALCULO DE ESTADISTICOS ####

nse_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~NSE(.x, obs_mensual$flow))

pbias_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~pbias(.x, obs_mensual$flow))

r2_q <- q_sim %>% 
  select(-mon_year) %>% 
  map_dbl(., ~rsq_r2(.x, obs_mensual$flow))

rmse_q <- q_sim %>% select(-mon_year) %>% map_dbl(., ~rmse(.x, obs_mensual$flow))     


tab_summ <- tibble(nse_q, pbias_q, r2_q, rmse_q , ratio_escorrentia,ratio_gwflo ) %>% mutate_all(., ~round(., 3))
colnames(tab_summ) <- c("NSE", "PBIAS", "R²", "RMSE", "Runoff rate", "Groundwater contribution")

gt(tab_summ)



streamf_sim_plot <- q_sim %>% left_join(., obs_mensual, c("mon_year" = "date")) %>% 
  ggplot(aes(x = mon_year))+
  geom_line(aes(y = entrada_emb_men, color = "Simulated"), size = 0.7)+
  geom_line(aes(y = flow, color = "Observed"), linetype = 2,  size = 0.7)+
  scale_color_manual(values = c("black", "blue"))+
  theme_bw()+
  
  annotate("rect",xmin = dmy("01/01/1983"), xmax = dmy("31/12/1985"), 
           ymin = -0.05, ymax = 1.2,  color = "darkgreen", linetype = 2, linewidth = 1.2, fill = "transparent")+
  
  annotate("text", x = dmy("01/06/1984"), y = 1.25, label = "Calibration", size = 6, color = "darkgreen")+



  annotate("rect",xmin = dmy("01/01/1986"), xmax = dmy("31/12/1988"), 
           ymin = -0.05, ymax = 1.2,  color = "darkgreen", linetype = 2,linewidth = 1.2, fill = "transparent")+
  
  annotate("text", x = dmy("01/06/1987"), y = 1.25, label = "Validation", size = 6, color = "darkgreen")+
  
  
  
    labs(x = "Date", 
        # y = "Average monthly reservoir inflow (m³/s)", 
         y = "Reservoir inflow", 
         colour = "")+
  theme(text = element_text(size = 18))
  
  
  
  
  ggsave(plot = streamf_sim_plot, device = "tiff", filename =  "C:/ASG/Calibracion_Guajaraz/streamf_serientera_S.tif", dpi = 600, width = 14, height = 8)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

### sergio


pars_val <- sim_best$parameter$values

colnames(pars_val) <-  sim_best$parameter$definition$full_name


caudal_mejor <- run_swatplus(project_path = demo_path, 
                             output =list(caudal = define_output(file = 'channel_sd',
                                                                 variable = 'flo_out', 
                                                                 unit = 28)), 
                             
                             parameter = pars_val[1,], 
                             start_date = 19510101, 
                             end_date = 20191231, 
                             years_skip = 5, 
                             n_thread = 1)

write_rds(ronda_val_14, "C:/ASG/Calibracion_Guajaraz/rondas_nuevas/validacion_14bests.rds")



caudal_mejor$simulation %>% mutate(anio = year(date), mes = month(date)) %>% 
  group_by(anio, mes) %>%  summarise(flow_m3s_mes = mean(caudal)) %>% 
  mutate(days_mon = case_when(mes %in% c(1,3,5,7,8,10,12) ~ 31,
                              mes %in% c(4,6,9,11) ~ 30,
                              mes == 2 ~ 28)) %>%
  mutate(aportacion_hm3 = (flow_m3s_mes*86400*days_mon/1e6)) %>% 
  
  #save file
  .[,c("anio", "mes", "aportacion_hm3")] %>%# write.csv(., "C:/ASG/guajaraz_streamflow_56-19.csv", row.names = F, quote = F)
#monthly contribution plot
mutate(mon_year = ym(paste(anio, mes, sep = "/"))) %>% 
  ggplot(., aes(x = mon_year, y = aportacion_hm3))+geom_col(color = "darkblue", fill = "steelblue")+
  ylab("Monthly contribution (hm³/month)")+
  xlab("Date")

# annual contribution plot 
# group_by(anio) %>% summarise(anual_flow = sum(aportacion_hm3)) %>% 
# ggplot(., aes(x = anio, y = anual_flow))+geom_col(color = "darkblue", fill = "steelblue")+ geom_smooth()+
# ylab("Annual contribution (hm³/year)")+
# xlab("Year")



cmm <- caudal_mejor$simulation %>% mutate(anio = year(date), mes = month(date)) %>% 
  group_by(anio, mes) %>%  summarise(flow_m3s_mes = round(mean(caudal), 4)) %>% 
  mutate(date = paste(anio, mes, sep = "/")) %>% .[,c("date","flow_m3s_mes")]


mean(caudal_mejor$simulation$caudal)*86400*365/1000000



write.csv(cmm,"C:/ASG/caudal_layos_m3s_m.csv", quote =  F, row.names = F)



### NEW EXE VALIDATION --> BEST OF R2 AND R3


r2_bestpar <- pars_r1[c(840,627),]



r2calmetr <- rank_tableGRAN_r1 



parsval <- bestpars_r1 %>% rbind(r2_bestpar)
colnames(parsval) <- colnames(limites_parametros_r1)



ronda_val_R1R2BESTS <- run_swatplus(project_path = demo_path, 
                             output =list(caudal = define_output(file = 'channel_sd',
                                                                 variable = 'flo_out', 
                                                                 unit = 5),
                                          entrada_embalse = define_output(file = 'reservoir',
                                                                          variable = 'flo_in',
                                                                          unit = 1),
                                          salida_embalse = define_output(file = 'reservoir',
                                                                         variable = 'flo_out',
                                                                         unit = 1),
                                          almacenamiento_embalse = define_output(file = 'reservoir',
                                                                                 variable = 'flo_stor',
                                                                                 unit = 1),
                                          precipitacion = define_output(file = 'basin_wb',
                                                                        variable = 'precip',
                                                                        unit = 1), 
                                          evaporacion = define_output(file = 'basin_wb',
                                                                      variable = 'et',
                                                                      unit = 1), 
                                          percolacion = define_output(file = 'basin_wb',
                                                                      variable = 'perc',
                                                                      unit = 1), 
                                          flujo_lateral = define_output(file = 'basin_wb',
                                                                        variable = 'latq_cha',
                                                                        unit = 1), 
                                          flujo_superficial = define_output(file = 'basin_wb',
                                                                            variable = 'surq_cha',
                                                                            unit = 1), 
                                          flo_aqu = define_output(file = 'basin_aqu',
                                                                  variable = 'flo_cha',
                                                                  unit = 1),             
                                          recarga_aqu = define_output(file = 'basin_aqu',
                                                                      variable = 'rchrg',
                                                                      unit = 1),  
                                          seep = define_output(file = 'basin_aqu',
                                                               variable = 'seep',
                                                               unit = 1),  
                                          evapo_aqu = define_output(file = 'basin_aqu',
                                                                    variable = 'revap',
                                                                    unit = 1)), 
                             
                             parameter = parsval, 
                             start_date = 19810101, 
                             end_date = 19881231, 
                             years_skip = 5, 
                             n_thread = Inf)

write_rds(ronda_val_R1R2BESTS, "C:/ASG/Calibracion_Guajaraz/rondas_nuevas/val_r1r2bests.rds")




rank_best_calibration <- rank_best_calibration %>% mutate(runval = 1:34)


rank_tableGRAN_val40 <- rank_tableGRAN_val40 %>% mutate(runval = 1:34)




rank_best_calibration %>% left_join(., rank_tableGRAN_val40, "runval") %>% select(., -starts_with("rank")) %>% 
  mutate_at(., c("nse", "r2", "pbias",  "rmse" , "groundw_rate", "nse_val", "r2_val" ,"pbias_val" ,"rmse_val" , "groundw_rate_val"), ~round(., 2)) %>% 
  mutate_at(., c("runoff_rate", "runoff_rate_val"), ~round(., 3)) %>% 
  write.csv(., "C:/ASG/Calibracion_Guajaraz/new_ex/TABLA_FINAL_METRICS.csv", quote = F, row.names = F)





rank_best_calibration %>% arrange(., -rank_run)

rank_tableGRAN_val40 %>% arrange(., -rank_run_val)

