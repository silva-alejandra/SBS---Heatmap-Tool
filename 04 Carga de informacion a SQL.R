library(AO.scripts)

try(borrarTabla("MAPA_MACROPRU_IND_RAW"))
try(borrarTabla("MAPA_MACROPRU_IND"))
try(borrarTabla("MAPA_MACROPRU_SUBCATE"))
try(borrarTabla("MAPA_MACROPRU_CATE"))

try(borrarTabla("MAPA_MACROPRU_IND_2"))

#subirTabla(Indicadores, "MAPA_MACROPRU_IND_RAW", pwd = "ov1ed0402")

#subirTabla(indicadores_final, "MAPA_MACROPRU_IND", pwd = "ov1ed0402")




subirTabla(indicadores_final %>%
             melt(id.vars = c("PERIODO_ID", "PERIODO_TRIMESTRE_ID", "PERIODO_IND_TRI"),
                  variable.name = "INDICADOR", 
                  value.name = "VALOR"), "MAPA_MACROPRU_IND_2", pwd = "ov1ed0402")

try(borrarTabla("MAPA_MACROPRU_IND_EEMYPE"))
subirTabla(indicadores_eemype_final %>%
             melt(id.vars = c("PERIODO_ID", "PERIODO_TRIMESTRE_ID", "PERIODO_IND_TRI"),
                  variable.name = "INDICADOR", 
                  value.name = "VALOR"), "MAPA_MACROPRU_IND_EEMYPE", pwd = "ov1ed0402")

try(borrarTabla("MAPA_MACROPRU_IND_CONSUMO"))
subirTabla(indicadores_consumo_final %>%
             melt(id.vars = c("PERIODO_ID", "PERIODO_TRIMESTRE_ID", "PERIODO_IND_TRI"),
                  variable.name = "INDICADOR", 
                  value.name = "VALOR"), "MAPA_MACROPRU_IND_CONSUMO", pwd = "ov1ed0402")


try(borrarTabla("BCRP_INDICADORES_MAPA"))
subirTabla(BCRP_2, "BCRP_INDICADORES_MAPA", pwd = "ov1ed0402")

enviarSQL("GRANT SELECT ON MAPA_MACROPRU_IND_2 TO PUBLIC")
enviarSQL("GRANT SELECT ON MAPA_MACROPRU_IND_EEMYPE TO asilva")
enviarSQL("GRANT SELECT ON MAPA_MACROPRU_IND_CONSUMO TO PUBLIC")


try(borrarTabla("MAPA_MACROPRU_IND_RAW"))
  subirTabla(Indicadores[PERIODO_ID>=20150131] %>%
               melt(id.vars = c("PERIODO_ID", "PERIODO_TRIMESTRE_ID", "PERIODO_IND_TRI", "ANIO"),
                    variable.name = "INDICADOR", 
                    value.name = "VALOR"), "MAPA_MACROPRU_IND_RAW", pwd = "ov1ed0402")
