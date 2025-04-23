
## To-do
# 1. incorporar variables_bcrp y names_bcrp en una misma lista [ ]
# 2. Incorporar la TAMN --> lol siempre se pudo sacar desde el BCRP XD [x]
# 

# Inputs ----

library(AO.scripts) # Para descargar info de Oracle, entre otras cosas
library(jsonlite) # Para leer archivos .json de APIs 
library(lubridate) # Para trabajar con fechas
library(glue) # Para generar strings

## API Key para hacer queries a la base de datos de FRED 

inicio_ventana = 19900131
fin_ventana = 20230630 # Esto es lo que se cambia para una nueva corrida

pegar_ultimo_dato = TRUE

FRED_API_KEY = "5cd12eb363aaa58c594f797fe694a3de"

ruta_inv = "K:/INV/Analítica avanzada/Plataforma BI/Mapa Macroprudencial/output"

dir.create(file.path(ruta_inv, fin_ventana), showWarnings = FALSE)

## Serie fechas

Dates = data.table(PER = seq.fechaSQL(inicio_ventana, fin_ventana) %>% as.numeric)
Dates[, ANIO := substr(PER, 1, 4) %>% as.numeric]
Dates[, PERIODO_TRIMESTRE_ID := paste0(year(ymd(PER)), "-", as.roman(quarter(ymd(PER))))]
Dates[, PERIODO_IND_TRI := ifelse(month(ymd(PER)) == quarter(ymd(PER))*3, "F", "N")]


# Umbrales ------------ 

# En caso se quiera cambiar los umbrales para las variables que se acotan

umbral_rk = 0.135
umbral_rlmn = 0.2
umbral_rlme = 0.3

umbral_roe = 0.12


limite_umbral = 0.35

# UMBRALES Y LIMITES PARA LAS ESPECIALIZADAS

umbral_rk_eemype = 0.135 # Promedios similares
umbral_rlmn_eemype = 0.2
umbral_rlme_eemype = 0.3

umbral_roe_eemype = 0.12


umbral_rk_consumo = 0.135 # Promedios similares
umbral_rlmn_consumo = 0.2
umbral_rlme_consumo = 0.3

umbral_roe_consumo = 0.12


limite_umbral = 0.35

# Info BCRP ----

# Funciones de BCRP ----


fechas_bcrp <- function(codigo, fecha){
  
  fecha = stringr::str_replace(fecha, "\\.", "-")
  
  ( if(stringr::str_sub(codigo, -1, -1) == "D"){
    lubridate::dmy(fecha)
  } else if(stringr::str_sub(codigo, -1, -1) == "M"){
    lubridate::dmy(paste0("01-", fecha)) %>% lubridate::ceiling_date(unit = "months")-1
  } else if(stringr::str_sub(codigo, -1, -1) == "Q"){
    lubridate::yq(paste0(strsplit(fecha, "-")[[1]][2], ".", strsplit(fecha, "-")[[1]][1])) + months(3) -1
  } else if(stringr::str_sub(codigo, -1, -1) == "A"){
    lubridate::ymd(paste0(fecha,"1231"))
  } ) %>%
    (function(x) paste0(format(x, "%Y"), format(x, 
                                                "%m"), format(x, "%d")))()%>%
    as.numeric()
}

bcrpDescarga <- function(codigo, desde = NULL, hasta = NULL){
  
  # Generas las fechas con formato normal
  from = (if(!is.null(desde)) {ifelse(stringr::str_sub(codigo, -1, -1) == "A", year(ymd(desde)), 
                                      ifelse(stringr::str_sub(codigo, -1, -1) == "Q",
                                             paste0( year(ymd(desde)), "-", quarter(ymd(desde))), desde))} else{ NULL})
  to = (if(!is.null(desde)) {ifelse(stringr::str_sub(codigo, -1, -1) == "A", year(ymd(hasta)), 
                                      ifelse(stringr::str_sub(codigo, -1, -1) == "Q",
                                             paste0( year(ymd(hasta)), "-", quarter(ymd(hasta))), hasta))} else{ NULL})
  
  
  writeLines(paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api", codigo, "json", from, to, "ing", sep = "/"))
  download.file(paste("https://estadisticas.bcrp.gob.pe/estadisticas/series/api", codigo, "json", from, to, "ing", sep = "/"), codigo, quiet = T, cacheOK = F)
  
  
  tabla = fromJSON(readLines(codigo)) %>%
    as.data.frame() %>%
    as.data.table() %>%
    .[,.(CODIGO = codigo,
         GRUPO= config.title,
         SERIE = config.series.name,
         PERIODO_ID = lapply(periods.name, function(x) fechas_bcrp(codigo = codigo, fecha = x)) %>% as.numeric(),
         PERIODICIDAD = stringr::str_sub(codigo, -1, -1),
         VALOR = periods.values %>% as.numeric)]
  
  file.remove(codigo) # Borro el archivo
  
  return(tabla)
}

# Variables BCRP -------

# Info que se baja del BCRP. Para que se descargue tiene que estar en el objeto variables_bcrp_lista y tiene que tener el formato parecido a lo que se tiene abajo que seria:

# NOMBRE_INDICADOR = 'CODIGO_INDICADOR'


variables_bcrp_lista = list(BVL_IG = 'PN01142MM', 
                            BVL_MINING = 'PN01150MM', 
                            BVL_FINANCIAL = 'PN01148MM', 
                            BVL_INDUSTRIAL = 'PN01149MM', 
                            BVL_SERVICES = 'PN01151MM', 
                            EMBI = 'PN01129XM',
                            ORO_PRECIO = 'PN01654XM', VTA_INM_0 = 'PD04744PQ', 
                            #RENT_FONDO_2 = 'PN01178MM', Desaparece
                            TERM_INT_0 = 'PN01712BM', TERM_INT_1 = 'PN38926BM', TC = 'PN01234PM', IPC_VAR = 'PN01273PM', 
                            IND_VENT_MES_ANT = 'PD38041AM', IND_EXPEC = 'PD38045AM', VTA_INM_1 = 'PD37942PQ', DEUDA_PUB = 'PN03432FQ', 
                            PBI_VAR_12 = 'PN02526AQ', IPC = 'PN38705PM', PBI_NOM = 'PN02550AQ', ING_NAC_DISP = 'PN02577AQ', RES_ECO = 'PN03070FQ', 
                            M2 = 'PN00199MM', TAMN = 'PN07807NM', COEF_M2 = 'PN03494MQ',
                            CTA_CAPITALES_CP_SNF = 'PN39303BQ')

codigos_bcrp = purrr::simplify(variables_bcrp_lista)

names_bcrp <- names(purrr::simplify(variables_bcrp_lista)) # Modificacion para agregar TAMN

# To-do: hacer una lista que sea correspondencia codigo = nemonico. [listo]

BCRP <- lapply(codigos_bcrp, bcrpDescarga, inicio_ventana, fin_ventana) %>%
  rbindlist()

BCRP_2 <- BCRP[PERIODICIDAD == 'Q'] %>%
  merge(Dates[PERIODO_IND_TRI=="F"], by.x = "PERIODO_ID", by.y = "PER") %>%
  merge(Dates, by = "PERIODO_TRIMESTRE_ID", allow.cartesian = T) %>%
  .[,.(PERIODO_ID = PER, CODIGO, VALOR)] %>%
  rbind(BCRP[PERIODICIDAD == 'M'][,.(PERIODO_ID, CODIGO, VALOR)]) %>%
  dcast(PERIODO_ID ~ CODIGO, value.var = "VALOR")

 # Arreglo para venta de inmuebles solo dic 2022 por mieeeentras

# to-do: asignarle un nombre de variable a cada codigo para que sea mas facil leer

setnames(BCRP_2, codigos_bcrp, names_bcrp)
setorder(BCRP_2, PERIODO_ID)

names(BCRP_2)[sapply(BCRP_2[PERIODO_ID == fin_ventana], is.na)] # Variables que a la fecha del fin aun no tienen informacion

#BCRP_2[PERIODO_ID > 20220930, VTA_INM_1  := 4692]
#BCRP_2[PERIODO_ID > 20221231, VTA_INM_1  := 4390]
#BCRP_2[PERIODO_ID > 20230331, VTA_INM_1  := 4400]
#BCRP_2[PERIODO_ID > 20220331, VTA_INM_1  := 4390] Si tampoco hay dato de junio, modificar esto


if(pegar_ultimo_dato){
  setnafill(BCRP_2, cols = setdiff(names(BCRP_2)[sapply(BCRP_2[PERIODO_ID == fin_ventana], is.na)], c("VTA_INM_0", "TERM_INT_0", "M2")), type = "locf")
}

names(BCRP_2)[sapply(BCRP_2[PERIODO_ID == fin_ventana], is.na)]
# Ajuste por dato por fuera


## Series WB y FRED 

# De la FRED sale el dato del VIX y del WB sale el dato de la population de Peru

VIX <- jsonlite::fromJSON("https://api.stlouisfed.org/fred/series/observations?series_id=VIXCLS&api_key=5cd12eb363aaa58c594f797fe694a3de&file_type=json&frequency=m&observation_start=1992-01-31&aggregation_method=eop") %>%
  as.data.frame()%>%
  as.data.table() %>%
  .[,.(PERIODO_ID = paste0(format(ymd(observations.date)-1, "%Y"), 
                           format(ymd(observations.date)-1, "%m"), 
                           format(ymd(observations.date)-1, "%d")) %>% as.numeric,
       VIX = as.numeric(observations.value))]

# Del WB sale la poblacion del peru

POP <- jsonlite::fromJSON("http://api.worldbank.org/v2/country/PER/indicator/SP.POP.TOTL.?format=json&date=2000:2030") %>%
  as.data.frame() %>%
  as.data.table() %>%
  .[,.(PERIODO_ID = (as.numeric(date)),
       POP = value)]

# Agregar detalle de poblacion en los ultimos años

poblacion_adicional <- data.table(PERIODO_ID = c(2022, 2023), POP2 = c(33684000, 33684000))

POP <- merge(POP, poblacion_adicional, all = T)


POP[, POP := fcoalesce(as.numeric(POP), POP2)]

POP[, POP2 := NULL]


# De Yahoo Finance

library(yahoofinancer)

SPY <- Ticker$new('^GSPC')$get_history(start = ymd(inicio_ventana), end = ymd(fin_ventana), interval = "1mo")%>%
  as.data.table() %>%
  .[,.(PERIODO_ID = paste0(format(ceiling_date(date, "month")-days(1), "%Y"), 
                           format(ceiling_date(date, "month")-days(1), "%m"), 
                           format(ceiling_date(date, "month")-days(1), "%d")) %>% as.numeric,
       SPY = adj_close)]

# --- Parte SBS ----

#enviarSQL(paste(readLines("Informacion SBS.sql"), collapse = " "))

SBS <- bajarTabla("AOVIEDO.INDICADORES_MAPA")

setkey(SBS, PERIODO_ID, CODIGO_ENTIDAD_ID)

# ---- Parte extraida desde Siscor Excel -----

CREDITOS_ALE <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/Ale - Creditos.csv", integer64 = "numeric")
BVL_ALE <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/Ale - BVL.csv", integer64 = "numeric")
RI_ANTIGUO <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/RI antiguo.csv", integer64 = "numeric")

VARIABLES_ANTIGUAS <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/Data Historica 200101-201307.csv", integer64 = "numeric")

margen_antiguo <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/Margen Financiero Bruto ANTIGUO.csv", integer64 = "numeric")
#prov_antiguas <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/Provision especifica anual.csv", integer64 = "numeric")

rk_antiguo <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/RK pre-201007.csv", integer64 = "numeric")
rk_antiguo[, RK := (PE/APR)/100]

rk_antiguo_2 <- fread("K:/AOviedo/2022/05 Plataforma Analytics INV/Mapa Macroprudencial/Informacion antigua/RK pre 201007  - Daniel.csv", integer64 = "numeric")

SBS[PERIODO_ID <= 20110731, GASTOS_PROV_ANUAL := NA]
SBS[PERIODO_ID <= 20110731, CARTERA_PROM_ANUAL := NA]
SBS[PERIODO_ID <= 20110731, ACTIVOS_PROM_ANUAL  := NA]
SBS[PERIODO_ID <= 20110731, PATRIMONIO_PROM_ANUAL  := NA]
SBS[PERIODO_ID <= 20110731, UTL_ANUAL  := NA]

SBS[PERIODO_ID <= 20170930, MARGEN_FINANCIERO_BRUTO_ANUAL  := NA]


SBS[rk_antiguo_2, PATRIMONIO_EFECTIVO_TOTAL  := fcoalesce(as.numeric(PE), PATRIMONIO_EFECTIVO_TOTAL), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
SBS[rk_antiguo_2, REQ_PE_TOTAL_APR  := fcoalesce(APR, REQ_PE_TOTAL_APR), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]

SBS[VARIABLES_ANTIGUAS, GASTOS_PROV_ANUAL := fcoalesce(GASTOS_PROV_ANUAL, i.GASTO_PROV_ANUAL), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
SBS[VARIABLES_ANTIGUAS, CARTERA_PROM_ANUAL  := fcoalesce(CARTERA_PROM_ANUAL, i.CARTERA_PROMEDIO), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
SBS[VARIABLES_ANTIGUAS, ACTIVOS_PROM_ANUAL  := fcoalesce(ACTIVOS_PROM_ANUAL, i.ACTIVO_PROMEDIO), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
SBS[VARIABLES_ANTIGUAS, PATRIMONIO_PROM_ANUAL  := fcoalesce(PATRIMONIO_PROM_ANUAL, i.PATRIMONIO_PROMEDIO), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
SBS[VARIABLES_ANTIGUAS, UTL_ANUAL := fcoalesce(UTL_ANUAL, i.UTILIDAD_ANUALIZADA), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
#SBS[VARIABLES_ANTIGUAS, CARTERA_DE_CREDITOS  := fcoalesce(CARTERA_DE_CREDITOS, i.SALDO_TOT), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
#SBS[VARIABLES_ANTIGUAS, CARTERA_ATRASADA  := fcoalesce(CARTERA_ATRASADA, i.CARTERA_ATRASADA), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]
SBS[VARIABLES_ANTIGUAS, PROVISIONES_PARA_CREDITOS  := abs(fcoalesce(PROVISIONES_PARA_CREDITOS, i.PROVISIONES_PARA_CREDITOS)), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]

SBS[margen_antiguo, MARGEN_FINANCIERO_BRUTO_ANUAL := fcoalesce(MARGEN_FINANCIERO_BRUTO_ANUAL/1e6, as.numeric(i.MARGEN_FINANCIERO_BRUTO_ANUAL)), on = c("PERIODO_ID", "CODIGO_ENTIDAD_ID")]


SBS[PERIODO_ID > 20170930,  MARGEN_FINANCIERO_BRUTO_ANUAL := MARGEN_FINANCIERO_BRUTO_ANUAL/1e3]
