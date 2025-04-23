# Estandarizacion----

## Funciones auxiliares para todo el proceso ----

estandariza <- function(x, lag = 36, ...){ # Para estandarizar
  (x - frollmean(x, lag, ...))/frollapply(x, lag, sd, ...)
}

estandariza_completo <- function(x, ...){ # Para estandarizar
  (x - mean(x, na.rm = T, ...))/sd(x, na.rm = T, ...)
}

percentil_inverso <- function(x){ # Para sacar percentil
  frank(x, na.last = "keep", ties.method = "average")/(sum(!is.na(x)))
}


## Lista indicadores jerarquica - para cada mapa ----

lista_indicadores = {list(
  apetito =
    list(interno = c("BVL_MINING_VAR_12", "BVL_FINANCIAL_VAR_12", "BVL_INDUSTRIAL_VAR_12", "BVL_SERVICES_VAR_12", "EMBI", "BRECHA_VTA_INM", "CTA_CAPITALES_CP_SNF"),
         externo = c("ORO_PRECIO", "SPY_VAR_12", "VIX")),
  desb_sector_no_fin =
    list(empresas = c("CD_MAY_VAR_12", "CD_MYPE_VAR_12", "BRECHA_CRED_PBI_MAY", "BRECHA_CRED_PBI_MYPE", "BRECHA_DSR_MAY", "BRECHA_DSR_MYPE"),
         hogares = c("CD_CONSUMO_VAR_12", "CD_HIP_VAR_12", "BRECHA_CRED_PBI_CONS", "BRECHA_CRED_PBI_HIP", "BRECHA_DSR_CONS", "BRECHA_DSR_HIP"),
         gobierno = c("BRECHA_DEUDA_PUB", "DEFICIT_FISCAL")),
  vulnerabilidad_sf =
    list(calidad_cartera = c('RI_MAY', 'RI_MYPE', 'RI_CONS', 'RI_HIP', "MOROSIDAD_AJUSTADA", "GASTO_PROV_CARTERA"),
         rentabilidad = c("ROE"),
         solvencia = c("RK"),
         liquidez = c("RL_MN", "RL_ME")),
  situacion_macro = 
    list(pbi = c("PBI_VAR_12"), 
         term_int = c("TERM_INT_VAR_12"),
         monetario = c("TC_VAR_6", "IPC_VAR", "BRECHA_M2_PBI"),
         ind_adelantados = c("IND_VENT_MES_ANT", "IND_EXPEC"))
)}
  
lista_indicadores_eemype= {list(
    desb_sector_no_fin_eemype =
      list(eemype = c("CD_EEMYPE_VAR_12", "BRECHA_CRED_EEMYPE_PBI", "BRECHA_DSR_EEMYPE")),
    vulnerabilidad_eemype =
      list(calidad_cartera_eemype = c("PORC_COB_EEMYPE", "MOROSIDAD_AJUSTADA_EEMYPE", "RI_EEMYPE", "GASTO_PROV_CARTERA_EEMYPE"),
           rentabilidad_eemype = c("ROE_EEMYPE"),
           solvencia_eemype = c("RK_EEMYPE"),
           liquidez_eemype = c("RL_MN_EEMYPE", "RL_ME_EEMYPE")))}
  
lista_indicadores_consumo = {list(
    desb_sector_no_fin_consumo =
      list(consumo = c("CD_CONSUMO_VAR_12", "BRECHA_CRED_CONSUMO_PBI", "BRECHA_DSR_CONSUMO")),
    vulnerabilidad_consumo =
      list(calidad_cartera_consumo = c("PORC_COB_CONSUMO", "MOROSIDAD_AJUSTADA_CONSUMO", "RI_CONSUMO", "GASTO_PROV_CARTERA_CONSUMO"),
           rentabilidad_consumo = c("ROE_CONSUMO"),
           solvencia_consumo = c("RK_CONSUMO"),
           liquidez_consumo = c("RL_MN_CONSUMO", "RL_ME_CONSUMO")))}
    

# Lista indicadores  -  Direccion riesgo  ----


direccion_riesgo = list(
  doble_cola = c('BVL_MINING_VAR_12', 'BVL_FINANCIAL_VAR_12', 'BVL_INDUSTRIAL_VAR_12', 'BVL_SERVICES_VAR_12', 'BRECHA_VTA_INM', 'RENT_FONDO_2', 'BRECHA_LN_CRED_MAY', 'BRECHA_LN_CRED_MYPE', 'CD_MAY_VAR_12', 
                 'CD_MYPE_VAR_12', 'BRECHA_LN_CRED_CONS', 'BRECHA_LN_CRED_HIP', 'CD_CONSUMO_VAR_12', 'CD_HIP_VAR_12', 'BRECHA_DEUDA_PUB', 'APR_VAR_12', 'PBI_VAR_12', 'TC_VAR_6', 'IPC_VAR', 'BRECHA_M2_PBI', 
                 "BRECHA_CRED_PBI_MAY", "BRECHA_CRED_PBI_MYPE", "BRECHA_DSR_MAY", "BRECHA_DSR_MYPE", "BRECHA_CRED_PBI_CONS", "BRECHA_CRED_PBI_HIP", "BRECHA_DSR_CONS", "BRECHA_DSR_HIP",
                 "BRECHA_LN_CRED_EEMYPE", "CD_EEMYPE_VAR_12", "BRECHA_CRED_EEMYPE_PBI", "BRECHA_DSR_EEMYPE",
                 "BRECHA_LN_CRED_CONSUMO", "CD_CONSUMO_VAR_12", "BRECHA_CRED_CONSUMO_PBI", "BRECHA_DSR_CONSUMO",
                 "APR_EEMYPE_VAR_12", "APR_CONSUMO_VAR_12",
                 "SPY_VAR_12"),
  cola_inversa = c('PORC_COB', 'ROE', 'MARG_FIN_VAR_12', 'PE_NIV1_VAR_12', 'RL_MN_VAR_12', 'RL_ME_VAR_12', 'TERM_INT_VAR_12', 'IND_VENT_MES_ANT', 'IND_EXPEC',
                   "PORC_COB_EEMYPE", "PORC_COB_CONSUMO", "ROE_EEMYPE", "ROE_CONSUMO", 
                   'MARG_FIN_EEMYPE_VAR_12', 'PE_NIV1_EEMYPE_VAR_12', 'RL_MN_EEMYPE_VAR_12', 'RL_ME_EEMYPE_VAR_12',
                   'MARG_FIN_CONSUMO_VAR_12', 'PE_NIV1_CONSUMO_VAR_12', 'RL_MN_CONSUMO_VAR_12', 'RL_ME_CONSUMO_VAR_12',
                   'RK', 'RK_EEMYPE', 'RK_CONSUMO',
                   "RL_MN", "RL_ME", "RL_MN_VAR_12", "RL_ME_VAR_12", "RL_MN_CONSUMO", "RL_ME_CONSUMO", "RL_MN_EEMYPE", "RL_ME_EEMYPE"),
  cola_normal = c('EMBI', 'ORO_PRECIO', 'VIX', 'DEFICIT_FISCAL', "GASTO_PROV_CARTERA", "GASTO_PROV_CARTERA_EEMYPE", "GASTO_PROV_CARTERA_CONSUMO", 
                  'PORC_REPRO','RI_MAY', 'RI_MYPE', 'RI_CONS', 'RI_HIP',
                  'PORC_REPRO_EEMYPE', 'RI_EEMYPE', 
                  'PORC_REPRO_CONSUMO', 'RI_CONSUMO',
                  "MOROSIDAD_AJUSTADA", "MOROSIDAD_AJUSTADA_EEMYPE", "MOROSIDAD_AJUSTADA_CONSUMO",
                  "CTA_CAPITALES_CP_SNF")
)

# Lista indicadores granular para cada mapa ----


lista_indicadores_granular <- purrr::simplify(purrr::simplify_all(lista_indicadores))
lista_indicadores_eemype_granular <- purrr::simplify(purrr::simplify_all(lista_indicadores_eemype))
lista_indicadores_consumo_granular <- purrr::simplify(purrr::simplify_all(lista_indicadores_consumo))


# Estandarizacion para cada mapa ----

indicadores_estandarizado <- Indicadores[, .(PERIODO_ID, PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI, PORC_ME, sapply(.SD, estandariza)), .SDcols = lista_indicadores_granular] 
indicadores_estandarizado_eemype <- Indicadores[, .(PERIODO_ID,PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI, PORC_ME_EEMYPE, sapply(.SD, estandariza)), .SDcols = lista_indicadores_eemype_granular] 
indicadores_estandarizado_consumo <- Indicadores[, .(PERIODO_ID,PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI, PORC_ME_CONSUMO, sapply(.SD, estandariza)), .SDcols = lista_indicadores_consumo_granular] 


#indicadores_estandarizado[, ROE := dnorm(ROE, mean = 0.174, sd = 0.06)]
#indicadores_estandarizado[, MARG_FIN_VAR_12 := dnorm(MARG_FIN_VAR_12, mean = 0.10, sd = 0.10)]

# Percentil para cada mapa ----

indicadores_percentil <- indicadores_estandarizado[PERIODO_IND_TRI == "F",.(PERIODO_ID,PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI, PORC_ME, sapply(.SD, percentil_inverso)), .SDcols = lista_indicadores_granular]
indicadores_percentil_eemype <- indicadores_estandarizado_eemype[PERIODO_IND_TRI == "F",.(PERIODO_ID,PERIODO_TRIMESTRE_ID, PORC_ME_EEMYPE, PERIODO_IND_TRI, sapply(.SD, percentil_inverso)), .SDcols = lista_indicadores_eemype_granular]
indicadores_percentil_consumo <- indicadores_estandarizado_consumo[PERIODO_IND_TRI == "F",.(PERIODO_ID,PERIODO_TRIMESTRE_ID, PORC_ME_CONSUMO, PERIODO_IND_TRI, sapply(.SD, percentil_inverso)), .SDcols = lista_indicadores_consumo_granular]


## Correccion direccion de riesgo ----

indicadores_final <- cbind(
  indicadores_percentil[, .(PERIODO_ID,PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI, PORC_ME, 
                            sapply(.SD, function(x) percentil_inverso(ifelse(x < 0.5, 1-x, x)))), 
                        .SDcols = intersect(lista_indicadores_granular, direccion_riesgo$doble_cola)], # Si es menor a 0.5, 1-p y vuelves a percentil
  indicadores_percentil[, lapply(.SD, function(x) (1-x)), 
                        .SDcols = intersect(lista_indicadores_granular, direccion_riesgo$cola_inversa)], # Si es cola inversa, solo 1-p
  indicadores_percentil[, .SD, 
                        .SDcols = intersect(lista_indicadores_granular, direccion_riesgo$cola_normal)]
  )

indicadores_eemype_final <- cbind(
  indicadores_percentil_eemype[, .(PERIODO_ID,PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI, PORC_ME_EEMYPE,
                            sapply(.SD, function(x) percentil_inverso(ifelse(x < 0.5, 1-x, x)))), 
                        .SDcols = intersect(lista_indicadores_eemype_granular, direccion_riesgo$doble_cola)], # Si es menor a 0.5, 1-p y vuelves a percentil
  indicadores_percentil_eemype[, lapply(.SD, function(x) (1-x)), 
                        .SDcols = intersect(lista_indicadores_eemype_granular, direccion_riesgo$cola_inversa)], # Si es cola inversa, solo 1-p
  indicadores_percentil_eemype[, .SD, 
                        .SDcols = intersect(lista_indicadores_eemype_granular, direccion_riesgo$cola_normal)]
)

indicadores_consumo_final <- cbind(
  indicadores_percentil_consumo[, .(PERIODO_ID,PERIODO_TRIMESTRE_ID, PERIODO_IND_TRI,  PORC_ME_CONSUMO,
                            sapply(.SD, function(x) percentil_inverso(ifelse(x < 0.5, 1-x, x)))), 
                        .SDcols = intersect(lista_indicadores_consumo_granular, direccion_riesgo$doble_cola)], # Si es menor a 0.5, 1-p y vuelves a percentil
  indicadores_percentil_consumo[, lapply(.SD, function(x) (1-x)), 
                        .SDcols = intersect(lista_indicadores_consumo_granular, direccion_riesgo$cola_inversa)], # Si es cola inversa, solo 1-p
  indicadores_percentil_consumo[, .SD, 
                        .SDcols = intersect(lista_indicadores_consumo_granular, direccion_riesgo$cola_normal)]
)

## Ajuste variables general (MASTER CHEF) ----

indicadores_final[, ROE := ifelse(Indicadores[PERIODO_IND_TRI == "F", ROE] > umbral_roe, 
                                 ifelse(ROE > limite_umbral, limite_umbral, ROE), 
                                 ROE)]

indicadores_final[, RK := ifelse(Indicadores[PERIODO_IND_TRI == "F", RK] > umbral_rk, 
                                     ifelse(RK > limite_umbral, limite_umbral, RK), 
                                     RK)]


indicadores_final[, RL_MN := ifelse(Indicadores[PERIODO_IND_TRI == "F", RL_MN] > umbral_rlmn, 
                                        ifelse(RL_MN > limite_umbral, limite_umbral, RL_MN), 
                                        RL_MN)]

indicadores_final[, RL_ME := ifelse(Indicadores[PERIODO_IND_TRI == "F", RL_ME] > umbral_rlme, 
                                        ifelse(RL_ME > limite_umbral, limite_umbral, RL_ME), 
                                        RL_ME)]

# Ajuste variables EEMYPE 

indicadores_eemype_final[, ROE_EEMYPE := ifelse(Indicadores[PERIODO_IND_TRI == "F", ROE_EEMYPE] > umbral_roe, 
                                  ifelse(ROE_EEMYPE > limite_umbral, limite_umbral, ROE_EEMYPE), 
                                  ROE_EEMYPE)]

indicadores_eemype_final[, RK_EEMYPE := ifelse(Indicadores[PERIODO_IND_TRI == "F", RK_EEMYPE] > umbral_rk, 
                                  ifelse(RK_EEMYPE > limite_umbral, limite_umbral, RK_EEMYPE), 
                                 RK_EEMYPE)]


indicadores_eemype_final[, RL_MN_EEMYPE := ifelse(Indicadores[PERIODO_IND_TRI == "F", RL_MN_EEMYPE] > umbral_rlmn, 
                                    ifelse(RL_MN_EEMYPE > limite_umbral, limite_umbral, RL_MN_EEMYPE), 
                                    RL_MN_EEMYPE)]

indicadores_eemype_final[, RL_ME_EEMYPE := ifelse(Indicadores[PERIODO_IND_TRI == "F", RL_ME_EEMYPE] > umbral_rlme, 
                                    ifelse(RL_ME_EEMYPE > limite_umbral, limite_umbral, RL_ME_EEMYPE), 
                                    RL_ME_EEMYPE)]

# Ajustes variables Consumo

indicadores_consumo_final[, ROE_CONSUMO := ifelse(Indicadores[PERIODO_IND_TRI == "F", ROE_CONSUMO] > umbral_roe, 
                                  ifelse(ROE_CONSUMO > limite_umbral, limite_umbral, ROE_CONSUMO), 
                                  ROE_CONSUMO)]

indicadores_consumo_final[, RK_CONSUMO := ifelse(Indicadores[PERIODO_IND_TRI == "F", RK_CONSUMO] > umbral_rk, 
                                 ifelse(RK_CONSUMO > limite_umbral, limite_umbral, RK_CONSUMO), 
                                 RK_CONSUMO)]


indicadores_consumo_final[, RL_MN_CONSUMO := ifelse(Indicadores[PERIODO_IND_TRI == "F", RL_MN_CONSUMO] > umbral_rlmn, 
                                    ifelse(RL_MN_CONSUMO > limite_umbral, limite_umbral, RL_MN_CONSUMO), 
                                    RL_MN_CONSUMO)]

indicadores_consumo_final[, RL_ME_CONSUMO := ifelse(Indicadores[PERIODO_IND_TRI == "F", RL_ME_CONSUMO] > umbral_rlme, 
                                    ifelse(RL_ME_CONSUMO > limite_umbral, limite_umbral, RL_ME_CONSUMO), 
                                    RL_ME_CONSUMO)]

# Agrupacion a nivel categoria y subcategoria ----

# Se supone que los mapas por empresas especializadas tienen el mismo inicio que los otros ----

inicio_calculo = list(interno = 20031231, externo = 20021231, empresas =20061231, hogares = 20061231, gobierno = 20041231,
                      apetito = 20031231, desb_sector_no_fin = 20061231, calidad_cartera = 20060331,
                      rentabilidad = 20060331, solvencia = 20010131  , liquidez = 20060331,
                      vulnerabilidad_sf = 20060331,
                      pbi = 19941231, monetario = 19950331, term_int = 19961231, ind_adelantados = 20050331,
                      situacion_macro = 20030630,
                      agregado = 20061231,
                      
                      eemype = 20061231, calidad_cartera_eemype = 20060331, rentabilidad_eemype = 20060331, solvencia_eemype = 20010131  , liquidez_eemype = 20060331,
                      consumo = 20061231, calidad_cartera_consumo = 20060331,  rentabilidad_consumo = 20060331, solvencia_consumo = 20010131  , liquidez_consumo = 20060331,
                     
                      desb_sector_no_fin_eemype = 20061231, vulnerabilidad_eemype = 20060331,
                      desb_sector_no_fin_consumo = 20061231, vulnerabilidad_consumo = 20060331)


# Primero generamos las variables por subcategoria ----

for(cate in names(lista_indicadores)) {
  for(sub_cate in names(lista_indicadores[[cate]])){
    if(sub_cate == 'solvencia'){
      indicadores_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := RK, .(PERIODO_ID)]
      } else if(sub_cate == 'liquidez'){
      indicadores_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := PORC_ME*(RL_ME)+ (1-PORC_ME)*(RL_MN), .(PERIODO_ID)]
      } else if(sub_cate == 'calidad_cartera'){
        indicadores_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := 0.50*(0.25*RI_MAY+ 0.25*RI_MYPE + 0.25*RI_CONS + 0.25*RI_HIP) + ifelse(is.na(MOROSIDAD_AJUSTADA), 0.5 * GASTO_PROV_CARTERA, 0.25 * MOROSIDAD_AJUSTADA + 0.25 * GASTO_PROV_CARTERA), .(PERIODO_ID)]
      } else{
        indicadores_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := rowMeans(.SD, na.rm = T), .SDcols = lista_indicadores[[cate]][[sub_cate]], .(PERIODO_ID)]
        }
    }
}

for(cate in names(lista_indicadores_eemype)) {
  for(sub_cate in names(lista_indicadores_eemype[[cate]])){
    if(sub_cate == 'solvencia_eemype'){
      indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := RK_EEMYPE, .(PERIODO_ID)]
    } else if(sub_cate == 'liquidez_eemype'){
      indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := PORC_ME_EEMYPE*(RL_ME_EEMYPE)+ (1-PORC_ME_EEMYPE)*(RL_MN_EEMYPE), .(PERIODO_ID)]
    } else if(sub_cate == 'calidad_cartera_eemype'){
      indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := 0.50*(RI_EEMYPE) + ifelse(is.na(MOROSIDAD_AJUSTADA_EEMYPE), 0.5 * GASTO_PROV_CARTERA_EEMYPE, 0.25 * MOROSIDAD_AJUSTADA_EEMYPE + 0.25 * GASTO_PROV_CARTERA_EEMYPE), .(PERIODO_ID)]
    } else {
      indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := rowMeans(.SD, na.rm = T), .SDcols = lista_indicadores_eemype[[cate]][[sub_cate]], .(PERIODO_ID)]
  
    }
  }
}

for(cate in names(lista_indicadores_consumo)) {
  for(sub_cate in names(lista_indicadores_consumo[[cate]])){
    if(sub_cate == 'solvencia_consumo'){
      indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := RK_CONSUMO, .(PERIODO_ID)]
    } else if(sub_cate == 'liquidez_consumo'){
      indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := PORC_ME_CONSUMO*(RL_ME_CONSUMO)+ (1-PORC_ME_CONSUMO)*(RL_MN_CONSUMO), .(PERIODO_ID)]
    } else if(sub_cate == 'calidad_cartera_consumo'){
      indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := 0.50*(RI_CONSUMO) + ifelse(is.na(MOROSIDAD_AJUSTADA_CONSUMO), 0.5 * GASTO_PROV_CARTERA_CONSUMO, 0.25 * MOROSIDAD_AJUSTADA_CONSUMO + 0.25 * GASTO_PROV_CARTERA_CONSUMO), .(PERIODO_ID)]
    } else {
      indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[sub_cate]], (sub_cate) := rowMeans(.SD, na.rm = T), .SDcols = lista_indicadores_consumo[[cate]][[sub_cate]], .(PERIODO_ID)]
    }
  }
}

# Luego le volvemos a generar el percentile

indicadores_final[, names(rlang::squash(lista_indicadores)) := lapply(.SD, percentil_inverso), .SDcols = names(rlang::squash(lista_indicadores))]
indicadores_eemype_final[, names(rlang::squash(lista_indicadores_eemype)) := lapply(.SD, percentil_inverso), .SDcols = names(rlang::squash(lista_indicadores_eemype))]
indicadores_consumo_final[, names(rlang::squash(lista_indicadores_consumo)) := lapply(.SD, percentil_inverso), .SDcols = names(rlang::squash(lista_indicadores_consumo))]


indicadores_final[, liquidez := ifelse(liquidez > limite_umbral & RL_MN <= limite_umbral & RL_ME <= limite_umbral, limite_umbral, liquidez)]
indicadores_eemype_final[, liquidez_eemype := ifelse(liquidez_eemype > limite_umbral & RL_MN_EEMYPE <= limite_umbral & RL_ME_EEMYPE <= limite_umbral, limite_umbral, liquidez_eemype)]
indicadores_consumo_final[, liquidez_consumo := ifelse(liquidez_consumo > limite_umbral & RL_MN_CONSUMO <= limite_umbral & RL_ME_CONSUMO <= limite_umbral, limite_umbral, liquidez_consumo)]

indicadores_final[, rentabilidad := ROE]
indicadores_eemype_final[, rentabilidad_eemype := ROE_EEMYPE]
indicadores_consumo_final[, rentabilidad_consumo := ROE_CONSUMO]

indicadores_final[, solvencia := RK]
indicadores_eemype_final[, solvencia_eemype := RK_EEMYPE]
indicadores_consumo_final[, solvencia_consumo := RK_CONSUMO]

#indicadores_final[, rentabilidad := ifelse(rentabilidad > 0.35 & ROE <= 0.35 & MARG_FIN_VAR_12 <= 0.35, 0.35, rentabilidad)]

# Ahora lo mismo para las categorias ----

for(cate in names(lista_indicadores)) {
  if(cate == 'vulnerabilidad_sf'){
    indicadores_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := rowMeans(.SD, na.rm = T), .SDcols = purrr::simplify(lista_indicadores[[cate]]), .(PERIODO_ID)]
    #indicadores_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := calidad_cartera*0.4+rentabilidad*0.4+ifelse(is.na(solvencia), 0.2*liquidez, solvencia*0.1+liquidez*0.1), .(PERIODO_ID)]
  } else {
    indicadores_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := rowMeans(.SD, na.rm = T), .SDcols = purrr::simplify(lista_indicadores[[cate]]), .(PERIODO_ID)]
  }
}

for(cate in names(lista_indicadores_eemype)) {
  if(cate == 'vulnerabilidad_eemype'){
    indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := rowMeans(.SD, na.rm = T), .SDcols = purrr::simplify(lista_indicadores_eemype[[cate]]), .(PERIODO_ID)]
    #indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := calidad_cartera_eemype*0.4+rentabilidad_eemype*0.4+ifelse(is.na(solvencia_eemype), 0.2*liquidez_eemype, solvencia_eemype*0.1+liquidez_eemype*0.1), .(PERIODO_ID)]
  } else {
    indicadores_eemype_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := rowMeans(.SD, na.rm = T), .SDcols = purrr::simplify(lista_indicadores_eemype[[cate]]), .(PERIODO_ID)]
  }
}


for(cate in names(lista_indicadores_consumo)) {
  if(cate == 'vulnerabilidad_consumo'){
    indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := rowMeans(.SD, na.rm = T), .SDcols = purrr::simplify(lista_indicadores_consumo[[cate]]), .(PERIODO_ID)]
    #indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := calidad_cartera_consumo*0.4+rentabilidad_consumo*0.4+ifelse(is.na(solvencia_consumo), 0.2*liquidez_consumo, solvencia_consumo*0.1+liquidez_consumo*0.1), .(PERIODO_ID)]
  } else {
    indicadores_consumo_final[PERIODO_ID >= inicio_calculo[[cate]], (cate) := rowMeans(.SD, na.rm = T), .SDcols = purrr::simplify(lista_indicadores_consumo[[cate]]), .(PERIODO_ID)]
  }
}

# Percentil para las categorias ----

indicadores_final[, names(lista_indicadores) := lapply(.SD, percentil_inverso), .SDcols = names(lista_indicadores)]
indicadores_eemype_final[, names(lista_indicadores_eemype) := lapply(.SD, percentil_inverso), .SDcols = names(lista_indicadores_eemype)]
indicadores_consumo_final[, names(lista_indicadores_consumo) := lapply(.SD, percentil_inverso), .SDcols = names(lista_indicadores_consumo)]


indicadores_eemype_final[, vulnerabilidad_eemype := (calidad_cartera_eemype  + rentabilidad_eemype  + solvencia_eemype + liquidez_eemype )/4]
indicadores_consumo_final[, vulnerabilidad_consumo := (calidad_cartera_consumo + rentabilidad_consumo + solvencia_consumo + liquidez_consumo)/4]

indicadores_final[PERIODO_ID >= inicio_calculo[["agregado"]], agregado := rowMeans(.SD, na.rm = T), .SDcols = c("apetito", "desb_sector_no_fin", "vulnerabilidad_sf"), .(PERIODO_ID)]
indicadores_eemype_final[PERIODO_ID >= inicio_calculo[["agregado"]], agregado_eemype := rowMeans(.SD, na.rm = T), .SDcols = c("desb_sector_no_fin_eemype", "vulnerabilidad_eemype"), .(PERIODO_ID)]
indicadores_consumo_final[PERIODO_ID >= inicio_calculo[["agregado"]], agregado_consumo := rowMeans(.SD, na.rm = T), .SDcols = c("desb_sector_no_fin_consumo", "vulnerabilidad_consumo"), .(PERIODO_ID)]



indicadores_final[, agregado := percentil_inverso(agregado)]
indicadores_eemype_final[, agregado_eemype := percentil_inverso(agregado_eemype)]
indicadores_consumo_final[, agregado_consumo := percentil_inverso(agregado_consumo)]


# Proyeccion de Adrian----


# Indicadores[, .(PERIODO_ID, PERIODO_IND_TRI, sapply(.SD, estandariza)),
#             .SDcols = c("BRECHA_LN_CRED_EEMYPE", "BRECHA_LN_CRED_CONSUMO",
#                 "BRECHA_DSR_EEMYPE", "BRECHA_DSR_CONSUMO",
#                 "BRECHA_CRED_EEMYPE_PBI", "BRECHA_CRED_CONSUMO_PBI",
#                   "CD_EEMYPE_VAR_12", "CD_CONSUMO_VAR_12")] %>%
#   fwrite("file.path(ruta_inv, fin_ventana, "indicadores_estandarizado_especializadas.csv")


indicadores_estandarizado[, c("PERIODO_ID", "PERIODO_IND_TRI", purrr::simplify(lista_indicadores$situacion_macro)), with = F]

fwrite(Indicadores, file.path(ruta_inv, fin_ventana, "indicadores_niveles_v3.csv"))

Indicadores[PERIODO_IND_TRI == "F"]

dir.create(file.path(ruta_inv, fin_ventana, Sys.Date()), showWarnings = F)


fwrite(indicadores_estandarizado, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_estandarizado.csv"))
fwrite(indicadores_percentil, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_percentil.csv"))
fwrite(indicadores_final, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_final.csv"))

fwrite(indicadores_estandarizado_eemype, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_estandarizado_eemype.csv"))
fwrite(indicadores_percentil_eemype, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_percentil_eemype.csv"))
fwrite(indicadores_eemype_final, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_final_eemype.csv"))

fwrite(indicadores_estandarizado_consumo, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_estandarizado_consumo.csv"))
fwrite(indicadores_percentil_consumo, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_percentil_consumo.csv"))
fwrite(indicadores_consumo_final, file.path(ruta_inv, fin_ventana, Sys.Date(), "indicadores_final_consumo.csv"))