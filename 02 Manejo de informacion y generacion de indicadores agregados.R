library(mFilter) # Paquete para el filtro de HP

aplicarHP <- function(serie, lambda = 14400) { # Para sacar trend en base al filtro HP
  mFilter::hpfilter(serie, lambda, "lambda")[["trend"]]
}

laggearVariable <- function(x, lag = 12){ # Para sacar crecimiento
  (x/ifelse(shift(x, lag) == 0, NA, shift(x, lag)))-1
}

# Calculos a nivel Total

SSFF = SBS[!(CODIGO_ENTIDAD_ID %in% c(125, 61, 99)), 
           .(CARTERA_DIRECTA_MAY = sum(CRED_MAYORISTA, na.rm = T),
             CARTERA_DIRECTA_MYPE = sum(CRED_MYPE , na.rm = T),
             CARTERA_DIRECTA_CONS = sum(CRED_CONSUMO , na.rm = T),
             CARTERA_DIRECTA_HIP = sum(CRED_HIPOTECARIO, na.rm = T),
             
             PORC_REPRO = sum(DIRECTA_REPRO, na.rm = T)/sum(SALDO_TOT, na.rm = T),
             PORC_COB = sum(PROVISIONES_PARA_CREDITOS, na.rm = T)/(sum(CARTERA_ATRASADA, na.rm = T)),
             
             MOROSIDAD_AJUSTADA = (sum(CARTERA_ATRASADA, na.rm = T) + sum(TRANSF_ULT_12, na.rm = T) + sum(CASTIGOS_ULT_12, na.rm = T))/(sum(CARTERA_DE_CREDITOS, na.rm = T) +  + sum(TRANSF_ULT_12, na.rm = T) + sum(CASTIGOS_ULT_12, na.rm = T)),
             #MOROSIDAD_AJUSTADA = (sum(CARTERA_ATRASADA, na.rm = T) )/(sum(CARTERA_DE_CREDITOS, na.rm = T)),
             RI = sum(SALDO_DEF_TOT, na.rm = T)/sum(SALDO_TOT, na.rm = T),
             RI_MAY = sum(SALDO_DEF_MAYOR, na.rm = T)/sum(SALDO_MAYOR, na.rm = T),
             RI_MYPE = sum(SALDO_DEF_MYPE, na.rm = T)/sum(SALDO_MYPE, na.rm = T),
             RI_CONS = sum(SALDO_DEF_CONS, na.rm = T)/sum(SALDO_CONS, na.rm = T),
             RI_HIP = sum(SALDO_DEF_HIP, na.rm = T)/sum(SALDO_HIP, na.rm = T),
             
             GASTO_PROV_CARTERA = sum(GASTOS_PROV_ANUAL, na.rm = T)/sum(CARTERA_PROM_ANUAL, na.rm = T),
             ROE = sum(UTL_ANUAL, na.rm = T)/sum(PATRIMONIO_PROM_ANUAL, na.rm = T),
             MARG_FIN = sum(MARGEN_FINANCIERO_BRUTO_ANUAL, na.rm = T),
             RK = sum(PATRIMONIO_EFECTIVO_TOTAL, na.rm = T)/sum(REQ_PE_TOTAL_APR, na.rm =T),
             PE_NIV1 = sum(PATRIMONIO_EFECTIVO_NIVEL_I, na.rm = T),
             APR = sum(REQ_PE_TOTAL_APR, na.rm = T),
             
             RL_MN = sum(MN_AL, na.rm = T)/sum(MN_PCP, na.rm =T),
             RL_ME = sum(ME_AL, na.rm = T)/sum(ME_PCP, na.rm =T),
             
             PORC_ME = (sum(ME_PCP, na.rm =T))/(sum(MN_PCP, na.rm =T)+sum(ME_PCP, na.rm =T)),
             
             RCL_MN = (sum(ACTIVOS_LIQUIDOS_AJUST_MN, na.rm = T) + min(sum(FLUJOS_ENTRANTES_30D_AJUST_MN, na.rm = T), 0.75*sum(FLUJOS_SALIENTES_30D_AJUST_MN, na.rm = T)))*100/sum(FLUJOS_SALIENTES_30D_AJUST_MN, na.rm = T),
             RCL_ME = (sum(ACTIVOS_LIQUIDOS_AJUST_ME, na.rm = T) + min(sum(FLUJOS_ENTRANTES_30D_AJUST_ME, na.rm = T), 0.75*sum(FLUJOS_SALIENTES_30D_AJUST_ME, na.rm = T)))*100/sum(FLUJOS_SALIENTES_30D_AJUST_ME, na.rm = T)),
           .(PERIODO_ID)]

SBS[, ENTIDAD_PEER_GROUP_SAEE_V2 := ifelse(CRED_MYPE/TOT > 0.5, "EEMYPE", ifelse(CRED_CONSUMO/TOT > 0.5, "CONSUMO", NA))]
SBS[, ENTIDAD_SF_PEERGROUP := ifelse(ENTIDAD_SF_PEERGROUP %in% c("Consumo revolvente", "Consumo no revolvente"), "Consumo", ENTIDAD_SF_PEERGROUP)]

Grupos <- SBS[!(CODIGO_ENTIDAD_ID %in% c(125,61,99)), 
              .(CARTERA_DIRECTA = sum(CARTERA_DIRECTA, na.rm = T),
                CARTERA_DIRECTA_MAY = sum(CRED_MAYORISTA , na.rm = T),
                CARTERA_DIRECTA_MYPE = sum(CRED_MYPE , na.rm = T),
                CARTERA_DIRECTA_CONS = sum(CRED_CONSUMO, na.rm = T),
                CARTERA_DIRECTA_HIP = sum(CRED_HIPOTECARIO, na.rm = T),
                PORC_REPRO = sum(DIRECTA_REPRO, na.rm = T)/sum(SALDO_TOT, na.rm = T),
                PORC_COB = sum(PROVISIONES_PARA_CREDITOS, na.rm = T)/(sum(CARTERA_ATRASADA, na.rm = T)),
                MOROSIDAD_AJUSTADA = (sum(CARTERA_ATRASADA, na.rm = T) + sum(TRANSF_ULT_12, na.rm = T) + sum(CASTIGOS_ULT_12, na.rm = T))/(sum(CARTERA_DE_CREDITOS, na.rm = T) +  + sum(TRANSF_ULT_12, na.rm = T) + sum(CASTIGOS_ULT_12, na.rm = T)),
                RI = sum(SALDO_DEF_TOT, na.rm = T)/sum(SALDO_TOT, na.rm = T),
                RI_MAY = sum(SALDO_DEF_MAYOR, na.rm = T)/sum(SALDO_MAYOR, na.rm = T),
                RI_MYPE = sum(SALDO_DEF_MYPE, na.rm = T)/sum(SALDO_MYPE, na.rm = T),
                RI_CONS = sum(SALDO_DEF_CONS, na.rm = T)/sum(SALDO_CONS, na.rm = T),
                RI_HIP = sum(SALDO_DEF_HIP, na.rm = T)/sum(SALDO_HIP, na.rm = T),
                GASTO_PROV_CARTERA = sum(GASTOS_PROV_ANUAL, na.rm = T)/sum(CARTERA_PROM_ANUAL, na.rm = T),
                ROE = sum(UTL_ANUAL, na.rm = T)/sum(PATRIMONIO_PROM_ANUAL, na.rm = T),
                MARG_FIN = sum(MARGEN_FINANCIERO_BRUTO_ANUAL, na.rm = T),
                RK = sum(PATRIMONIO_EFECTIVO_TOTAL, na.rm = T)/sum(REQ_PE_TOTAL_APR, na.rm =T),
                PE_NIV1 = sum(PATRIMONIO_EFECTIVO_NIVEL_I, na.rm = T),
                APR = sum(REQ_PE_TOTAL_APR, na.rm = T),
                RL_MN = sum(MN_AL, na.rm = T)/sum(MN_PCP, na.rm =T),
                RL_ME = sum(ME_AL, na.rm = T)/sum(ME_PCP, na.rm =T),
                
                PORC_ME = (sum(ME_PCP, na.rm =T))/(sum(MN_PCP, na.rm =T)+sum(ME_PCP, na.rm =T)),
                
                RCL_MN = (sum(ACTIVOS_LIQUIDOS_AJUST_MN, na.rm = T) + min(sum(FLUJOS_ENTRANTES_30D_AJUST_MN, na.rm = T), 0.75*sum(FLUJOS_SALIENTES_30D_AJUST_MN, na.rm = T)))*100/sum(FLUJOS_SALIENTES_30D_AJUST_MN, na.rm = T),
                RCL_ME = (sum(ACTIVOS_LIQUIDOS_AJUST_ME, na.rm = T) + min(sum(FLUJOS_ENTRANTES_30D_AJUST_ME, na.rm = T), 0.75*sum(FLUJOS_SALIENTES_30D_AJUST_ME, na.rm = T)))*100/sum(FLUJOS_SALIENTES_30D_AJUST_ME, na.rm = T)),
              .(PERIODO_ID, ENTIDAD_SF_PEERGROUP = ENTIDAD_SF_PEERGROUP)] %>% # Por orden solo de EEMype y Consumo?
  .[toupper(ENTIDAD_SF_PEERGROUP) %in% c("EEMYPE", "CONSUMO")] %>%
  melt(id.vars = c("PERIODO_ID", "ENTIDAD_SF_PEERGROUP"))%>%
  dcast(PERIODO_ID ~ paste0(variable, "_", toupper(stringr::str_replace(ENTIDAD_SF_PEERGROUP, " ", "_"))))


setnames(Dates, "PER", "PERIODO_ID", skip_absent = T)

Indicadores <- Dates %>%
  merge(BCRP_2, by = "PERIODO_ID", all.x = T) %>%
  merge(VIX, all.x = T) %>%
  merge(POP, by.x = "ANIO", by.y = "PERIODO_ID", all.x = T) %>%
  merge(SPY, by = "PERIODO_ID", all.x = T) %>%
  merge(SSFF, by = "PERIODO_ID", all.x = T) %>% 
  merge(Grupos, by = "PERIODO_ID", all.x = T)

setorder(Indicadores, PERIODO_ID)

# Arreglos -----

# Arreglos que se tienen que hacer porque la info de alguna fuente esta incompleta

Indicadores[PERIODO_ID == 20181231, RENT_FONDO_2 := -3.604] # Ajuste porque este dato no esta en la web
Indicadores[PERIODO_ID < 20110731, RI := NA]

Indicadores[PERIODO_ID < 20200331 | PERIODO_ID > 20220101 , PORC_REPRO := NA]
Indicadores[PERIODO_ID < 20200331 | PERIODO_ID > 20220101, PORC_REPRO_CONSUMO := NA]
Indicadores[PERIODO_ID < 20200331 | PERIODO_ID > 20220101 , PORC_REPRO_MYPE := NA]


Indicadores[BVL_ALE, BVL_FINANCIAL := fcoalesce(BVL_FINANCIAL, i.BVL_FINANCIAL), on = "PERIODO_ID"]
Indicadores[BVL_ALE, BVL_MINING  := fcoalesce(BVL_MINING, i.BVL_MINING), on = "PERIODO_ID"]
Indicadores[BVL_ALE, BVL_INDUSTRIAL := fcoalesce(BVL_INDUSTRIAL, i.BVL_INDUSTRIAL), on = "PERIODO_ID"]
Indicadores[BVL_ALE, BVL_SERVICES := fcoalesce(BVL_SERVICES, i.BVL_SERVICES), on = "PERIODO_ID"]

Indicadores[RI_ANTIGUO, RI  := fcoalesce(RI, i.RI/100), on = "PERIODO_ID"]
Indicadores[RI_ANTIGUO, RI_MAY  := fcoalesce(RI_MAY, i.RI_MAY/100), on = "PERIODO_ID"]
Indicadores[RI_ANTIGUO, RI_MYPE  := fcoalesce(RI_MYPE, i.RI_MAY/100), on = "PERIODO_ID"]
Indicadores[RI_ANTIGUO, RI_CONS  := fcoalesce(RI_CONS, i.RI_MAY/100), on = "PERIODO_ID"]
Indicadores[RI_ANTIGUO, RI_HIP  := fcoalesce(RI_HIP, i.RI_MAY/100), on = "PERIODO_ID"]

#Indicadores[rk_antiguo, RK  := fcoalesce(RK, i.RK), on = "PERIODO_ID"]  --- Daniel pasó los datos por entidad por 

 
Indicadores[CREDITOS_ALE, CARTERA_DIRECTA_MAY  := fcoalesce(ifelse(CARTERA_DIRECTA_MAY == 0, NA, CARTERA_DIRECTA_MAY) , i.CARTERA_DIRECTA_MAY), on = "PERIODO_ID"]
Indicadores[CREDITOS_ALE, CARTERA_DIRECTA_MYPE  := fcoalesce(ifelse(CARTERA_DIRECTA_MYPE == 0, NA, CARTERA_DIRECTA_MYPE) , i.CARTERA_DIRECTA_MYPE), on = "PERIODO_ID"]
Indicadores[CREDITOS_ALE, CARTERA_DIRECTA_CONS  := fcoalesce(ifelse(CARTERA_DIRECTA_CONS == 0, NA, CARTERA_DIRECTA_CONS) , i.CARTERA_DIRECTA_CONS), on = "PERIODO_ID"]
Indicadores[CREDITOS_ALE, CARTERA_DIRECTA_HIP  := fcoalesce(ifelse(CARTERA_DIRECTA_HIP == 0, NA, CARTERA_DIRECTA_HIP) , i.CARTERA_DIRECTA_HIP), on = "PERIODO_ID"]

Indicadores[CREDITOS_ALE, CARTERA_DIRECTA_EEMYPE   := fcoalesce(ifelse(CARTERA_DIRECTA_EEMYPE == 0, NA, CARTERA_DIRECTA_EEMYPE), i.CARTERA_DIRECTA_EEMYPE), on = "PERIODO_ID"]
Indicadores[CREDITOS_ALE, CARTERA_DIRECTA_CONSUMO  := fcoalesce(ifelse(CARTERA_DIRECTA_CONSUMO == 0, NA, CARTERA_DIRECTA_CONSUMO), i.CARTERA_DIRECTA_CONSUMO), on = "PERIODO_ID"]


# Aca se generan los indicadores extra que se generan con lags y etc ----

Indicadores[, TERM_INT_VAR_12 := fcoalesce(TERM_INT_1, TERM_INT_0)]
Indicadores[, VTA_INM := fcoalesce(VTA_INM_1, VTA_INM_0)]


Indicadores[, DEFICIT_FISCAL := frollsum(-RES_ECO, 4*3)/frollsum(PBI_NOM, 4*3)]
Indicadores[, BRECHA_DEUDA_PUB := DEUDA_PUB - frollapply(DEUDA_PUB, 36, FUN = function(x) aplicarHP(x)[36])]

Indicadores[, M2_PBI := (M2*3/1e6)*1000/frollsum(PBI_NOM, 12)]
Indicadores[, BRECHA_M2_PBI := M2_PBI - frollapply(M2_PBI, 36, FUN = function(x) aplicarHP(x)[36])]

Indicadores[, BRECHA_VTA_INM := VTA_INM - frollapply(VTA_INM, 36, FUN = function(x) aplicarHP(x)[36])]


Indicadores[, CD_MAY_VAR_12 := laggearVariable(CARTERA_DIRECTA_MAY)]
Indicadores[, CD_MYPE_VAR_12 := laggearVariable(CARTERA_DIRECTA_MYPE) ]
Indicadores[, CD_CONS_VAR_12 := laggearVariable(CARTERA_DIRECTA_CONS)]
Indicadores[, CD_HIP_VAR_12 := laggearVariable(CARTERA_DIRECTA_HIP)]

Indicadores[, BVL_MINING_VAR_12 := laggearVariable(BVL_MINING)]
Indicadores[, BVL_FINANCIAL_VAR_12 := laggearVariable(BVL_FINANCIAL)]
Indicadores[, BVL_INDUSTRIAL_VAR_12 := laggearVariable(BVL_INDUSTRIAL)]
Indicadores[, BVL_SERVICES_VAR_12 := laggearVariable(BVL_SERVICES)]
Indicadores[, CTA_CAPITALES_CP_SNF_VAR_12 := laggearVariable(CTA_CAPITALES_CP_SNF)]


Indicadores[, TC_VAR_6 := laggearVariable(TC, 6)]
Indicadores[, TC_VAR_12 := laggearVariable(TC, 12)]
Indicadores[, SPY_VAR_12 := laggearVariable(SPY, 12)]

Indicadores[, LN_CRED_PC_MAY := log(((CARTERA_DIRECTA_MAY/1e6)/(IPC))*1E3/POP)]
Indicadores[, LN_CRED_PC_MYPE := log(((CARTERA_DIRECTA_MYPE/1e6)/(IPC))*1E3/POP)]
Indicadores[, LN_CRED_PC_CONS := log(((CARTERA_DIRECTA_CONS/1e6)/(IPC))*1E3/POP)]
Indicadores[, LN_CRED_PC_HIP := log(((CARTERA_DIRECTA_HIP/1e6)/(IPC))*1E3/POP)]

Indicadores[, BRECHA_LN_CRED_MAY := LN_CRED_PC_MAY - frollapply(LN_CRED_PC_MAY, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_LN_CRED_MYPE := LN_CRED_PC_MYPE- frollapply(LN_CRED_PC_MYPE, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_LN_CRED_CONS := LN_CRED_PC_CONS- frollapply(LN_CRED_PC_CONS, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_LN_CRED_HIP := LN_CRED_PC_HIP -frollapply(LN_CRED_PC_HIP, 36, FUN = function(x) aplicarHP(x)[36])]

# Para especializadas

Indicadores[, CD_EEMYPE_VAR_12 := laggearVariable(CARTERA_DIRECTA_EEMYPE)]
Indicadores[, CD_CONSUMO_VAR_12 := laggearVariable(CARTERA_DIRECTA_CONSUMO)]

Indicadores[, LN_CRED_PC_EEMYPE := log(((CARTERA_DIRECTA_EEMYPE/1e6)/(IPC))*1E3/POP)]
Indicadores[, LN_CRED_PC_CONSUMO := log(((CARTERA_DIRECTA_CONSUMO/1e6)/(IPC))*1E3/POP)]

Indicadores[, BRECHA_LN_CRED_EEMYPE := LN_CRED_PC_EEMYPE- frollapply(LN_CRED_PC_EEMYPE, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_LN_CRED_CONSUMO := LN_CRED_PC_CONSUMO- frollapply(LN_CRED_PC_CONSUMO, 36, FUN = function(x) aplicarHP(x)[36])]


# Algunas variables gonzalo

Indicadores[, MARG_FIN_VAR_12 :=laggearVariable(MARG_FIN)]
Indicadores[, PE_NIV1_VAR_12 :=  laggearVariable(PE_NIV1) ]
Indicadores[, APR_VAR_12 := laggearVariable(APR)]
Indicadores[, RL_MN_VAR_12 := laggearVariable(RL_MN)]
Indicadores[, RL_ME_VAR_12 := laggearVariable(RL_ME)]

Indicadores[, MARG_FIN_EEMYPE_VAR_12 := laggearVariable(MARG_FIN_EEMYPE)]
Indicadores[, PE_NIV1_EEMYPE_VAR_12 := laggearVariable(PE_NIV1_EEMYPE)]
Indicadores[, APR_EEMYPE_VAR_12 := laggearVariable(APR_EEMYPE) ]
Indicadores[, RL_MN_EEMYPE_VAR_12 := laggearVariable(RL_MN_EEMYPE) ]
Indicadores[, RL_ME_EEMYPE_VAR_12 := laggearVariable(RL_ME_EEMYPE) ]

Indicadores[, MARG_FIN_CONSUMO_VAR_12 := laggearVariable(MARG_FIN_CONSUMO) ]
Indicadores[, PE_NIV1_CONSUMO_VAR_12 := laggearVariable(PE_NIV1_CONSUMO) ]
Indicadores[, APR_CONSUMO_VAR_12 := laggearVariable(APR_CONSUMO) ]
Indicadores[, RL_MN_CONSUMO_VAR_12 := laggearVariable(RL_MN_CONSUMO) ]
Indicadores[, RL_ME_CONSUMO_VAR_12 :=laggearVariable(RL_ME_CONSUMO) ]


# Variables adicionales que faltaban


Indicadores[, DSR_MAY := ((TAMN/100)/(1-(1+(TAMN/100))^(-30)))*(((CARTERA_DIRECTA_MAY/1e6)*1000*3)/frollsum(ING_NAC_DISP, 12))]
Indicadores[, DSR_MYPE := ((TAMN/100)/(1-(1+(TAMN/100))^(-30)))*(((CARTERA_DIRECTA_MYPE/1e6)*1000*3)/frollsum(ING_NAC_DISP, 12))]
Indicadores[, DSR_CONS := ((TAMN/100)/(1-(1+(TAMN/100))^(-30)))*(((CARTERA_DIRECTA_CONS/1e6)*1000*3)/frollsum(ING_NAC_DISP, 12))]
Indicadores[, DSR_HIP := ((TAMN/100)/(1-(1+(TAMN/100))^(-30)))*(((CARTERA_DIRECTA_HIP/1e6)*1000*3)/frollsum(ING_NAC_DISP, 12))]

Indicadores[, DSR_EEMYPE := ((TAMN/100)/(1-(1+(TAMN/100))^(-30)))*(((CARTERA_DIRECTA_EEMYPE/1e6)*1000*3)/frollsum(ING_NAC_DISP, 12))]
Indicadores[, DSR_CONSUMO := ((TAMN/100)/(1-(1+(TAMN/100))^(-30)))*(((CARTERA_DIRECTA_CONSUMO/1e6)*1000*3)/frollsum(ING_NAC_DISP, 12))]


Indicadores[, BRECHA_DSR_MAY := DSR_MAY- frollapply(DSR_MAY, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_DSR_MYPE := DSR_MYPE- frollapply(DSR_MYPE, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_DSR_CONS := DSR_CONS- frollapply(DSR_CONS, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_DSR_HIP := DSR_HIP- frollapply(DSR_HIP, 36, FUN = function(x) aplicarHP(x)[36])]

Indicadores[, BRECHA_DSR_EEMYPE := DSR_EEMYPE- frollapply(DSR_EEMYPE, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_DSR_CONSUMO := DSR_CONSUMO - frollapply(DSR_CONSUMO, 36, FUN = function(x) aplicarHP(x)[36])]


Indicadores[, CRED_PBI_MAY := (CARTERA_DIRECTA_MAY*3/1e6)*1000/frollsum(PBI_NOM, 12)]
Indicadores[, CRED_PBI_MYPE := (CARTERA_DIRECTA_MYPE*3/1e6)*1000/frollsum(PBI_NOM, 12)]
Indicadores[, CRED_PBI_CONS := (CARTERA_DIRECTA_CONS*3/1e6)*1000/frollsum(PBI_NOM, 12)]
Indicadores[, CRED_PBI_HIP := (CARTERA_DIRECTA_HIP*3/1e6)*1000/frollsum(PBI_NOM, 12)]

Indicadores[, CRED_EEMYPE_PBI := (CARTERA_DIRECTA_EEMYPE*3/1e6)*1000/frollsum(PBI_NOM, 12)]
Indicadores[, CRED_CONSUMO_PBI := (CARTERA_DIRECTA_CONSUMO*3/1e6)*1000/frollsum(PBI_NOM, 12)]

Indicadores[, BRECHA_CRED_PBI_MAY := CRED_PBI_MAY - frollapply(CRED_PBI_MAY, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_CRED_PBI_MYPE := CRED_PBI_MYPE - frollapply(CRED_PBI_MYPE, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_CRED_PBI_CONS := CRED_PBI_CONS - frollapply(CRED_PBI_CONS, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_CRED_PBI_HIP := CRED_PBI_HIP - frollapply(CRED_PBI_HIP, 36, FUN = function(x) aplicarHP(x)[36])]

Indicadores[, BRECHA_CRED_EEMYPE_PBI := CRED_EEMYPE_PBI - frollapply(CRED_EEMYPE_PBI, 36, FUN = function(x) aplicarHP(x)[36])]
Indicadores[, BRECHA_CRED_CONSUMO_PBI := CRED_CONSUMO_PBI - frollapply(CRED_CONSUMO_PBI, 36, FUN = function(x) aplicarHP(x)[36])]



