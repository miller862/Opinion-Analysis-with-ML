library(tidyverse)
library(readxl)
library(labelled)
library(skimr)
library(psych)
library(openxlsx)


survey134<-read_excel("../data/survey134.xlsx")
glimpse(survey110)


nombres_originales134 <- colnames(survey134)

#colnames(survey110)



colnames(survey134)<-c("TIEMPO","HIJOS","N_HIJOS_HOY", "BUSCA_HIJOS","N_BUSCA_HIJOS","SS","NO_HIJOS_PQ","TH_EDAD","NH_EDAD",
                       "UP","MASCOTHIJO","GORRA","EF","EJERCITO","TARIFAS","EMPRESARIOS", "MALVINAS","EF_QUEES","EL_PROBLEMA",
                       "DESIGUALDAD","NO_PIENSAN","EEUU","PALESTINA","ISRAEL","UCRANIA","RUSIA","BOLIVIA","CHINA","INGLATERRA",
                       "GENERO","EDAD","ESTUDIO","PROV","SOCIECON", "PROGRAMA","NOTICIAS","TRABAJA","GRUPO","PROLE","MILEI","ETIQUETA")




survey134 <- set_variable_labels(survey134, .labels = setNames(nombres_originales134, colnames(survey134)))


################# 
#recargorizacion para survey 134
clean134 <- survey134 %>% 
  mutate(
    HIJOS = ifelse(survey134$HIJOS == "SI", 1, 0),
    BUSCA_HIJOS = case_when(
      BUSCA_HIJOS == "NO LO SE" ~ NA_real_,
      BUSCA_HIJOS == "SI" ~ 1,
      BUSCA_HIJOS == "NO" ~ 0
    ),
    N_BUSCA_HIJOS = case_when(
      N_BUSCA_HIJOS == "0" ~ 0,
      N_BUSCA_HIJOS == "1" ~ 1,
      N_BUSCA_HIJOS == "2" ~ 2,
      N_BUSCA_HIJOS == "3" ~ 3,
      N_BUSCA_HIJOS == "4 o mas" ~ 4
    ),
    NH_EDAD = case_when(
      NH_EDAD %in% c("10000", "1") ~ NA_real_,
      NH_EDAD == "+30" ~ 31,
      TRUE ~ as.numeric(NH_EDAD)
    ),
    # Recategorización de NO_HIJOS_PQ
    NO_HIJOS_PQ = case_when(
      NO_HIJOS_PQ %in% c("Ya está con los que tengo los amo", "No quiero tener más hijos","Ya estoy demasiado grande") ~ 
        "NO APLICA: QUIERO TENER HIJOS O YA LOS TENGO",
      NO_HIJOS_PQ %in% c(
        "estoy en otra etapa de mi vida, estudio y no estoy interesada por el momento, pero sí, me gustaría poder terminar mis estudios, trabajar de ello y tener la economía suficiente como para vivir bien y que las aspiraciones crezcan aún con los hijos que tenga", 
        "Soy muy egoísta", 
        "Siento que primero tengo que vivir ciertas experiencias que teniendo un hijo me limitaría. Sumado a que para tener hijos también primero quiero estar estable laboral y económicamente, para darle todas las comodidades a mi alcance, en el caso de decidir tener hijos"
      ) ~ "Que siento que primero necesito vivir muchas cosas yo antes de tener un hijo",
      NO_HIJOS_PQ == "Mi edad y situacion economica" ~ "Que quiero mas dinero antes de pensar en un hijo",
      NO_HIJOS_PQ == "Aún debatimos en la pareja. No Está cerrado el tema" ~ "que no encuentro la pareja indicada",
      NO_HIJOS_PQ %in% c("Que los quiero tener cuando sea un poco más grande", "La edad que tengo") ~ "NO APLICA: QUIERO TENER HIJOS O YA LOS TENGO",
      TRUE ~ as.character(NO_HIJOS_PQ)  # Mantiene los valores originales que no se especificaron
    ),
    NO_HIJOS_PQ = as.factor(NO_HIJOS_PQ),  # Convertir a factor al final
    
    # Variables de acuerdo-desacuerdo
    UP = ifelse(UP == "DE ACUERDO", 1, 0),
    MASCOTHIJO = ifelse(MASCOTHIJO == "DE ACUERDO", 1, 0),
    GORRA = ifelse(GORRA == "DE ACUERDO", 1, 0),
    EF = ifelse(EF == "DE ACUERDO", 1, 0),
    EJERCITO = ifelse(EJERCITO == "DE ACUERDO", 1, 0),
    TARIFAS = ifelse(TARIFAS == "DE ACUERDO", 1, 0), #PAGABAMOS MUY POCO = 1, DESACUERDO 2
    EMPRESARIOS = ifelse(EMPRESARIOS == "DE ACUERDO", 1, 0),
    
    # SIGNIFICANTES
    MALVINAS = ifelse(MALVINAS == 1, 1, 0),#HEROES =1 "VICTIMAS=0
    EF_QUEES = ifelse(EF_QUEES == 1, 1, 0), #CRIPTO 1  #AHORRAR 0  
    EL_PROBLEMA = ifelse(EL_PROBLEMA == "Hay pocos que tienen mucho dinero", 1, 0),
    # DESIGUALDAD =1 #NO PIENSAN 0
    
    NO_PIENSAN = case_when(
      NO_PIENSAN == "Tiene mas educación o dinero que yo" ~ 1,
      NO_PIENSAN == "Tiene menos educacion o dinero que yo" ~ 0,
      TRUE ~ NA_real_
    ),
    DESIGUALDAD = case_when(
      DESIGUALDAD == 1 ~ 1,
      DESIGUALDAD == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # PAISES
    EEUU = case_when(
      EEUU == "Cariño" ~ 2,
      EEUU == "Positiva" ~ 1,
      EEUU == "Indiferencia / desconocimiento" ~ 0,
      EEUU == "Negativa" ~ -1,
      EEUU == "Rechazo" ~ -2
    ),
    PALESTINA = case_when(
      PALESTINA == "Cariño" ~ 2,
      PALESTINA == "Positiva" ~ 1,
      PALESTINA == "Indiferencia / desconocimiento" ~ 0,
      PALESTINA == "Negativa" ~ -1,
      PALESTINA == "Rechazo" ~ -2
    ),
    ISRAEL = case_when(
      ISRAEL == "Cariño" ~ 2,
      ISRAEL == "Positiva" ~ 1,
      ISRAEL == "Indiferencia / desconocimiento" ~ 0,
      ISRAEL == "Negativa" ~ -1,
      ISRAEL == "Rechazo" ~ -2
    ),
    UCRANIA = case_when(
      UCRANIA == "Cariño" ~ 2,
      UCRANIA == "Positiva" ~ 1,
      UCRANIA == "Indiferencia / desconocimiento" ~ 0,
      UCRANIA == "Negativa" ~ -1,
      UCRANIA == "Rechazo" ~ -2
    ),
    RUSIA = case_when(
      RUSIA == "Cariño" ~ 2,
      RUSIA == "Positiva" ~ 1,
      RUSIA == "Indiferencia / desconocimiento" ~ 0,
      RUSIA == "Negativa" ~ -1,
      RUSIA == "Rechazo" ~ -2
    ),
    CHINA = case_when(
      CHINA == "Cariño" ~ 2,
      CHINA == "Positiva" ~ 1,
      CHINA == "Indiferencia / desconocimiento" ~ 0,
      CHINA == "Negativa" ~ -1,
      CHINA == "Rechazo" ~ -2
    ),
    INGLATERRA = case_when(
      INGLATERRA == "Cariño" ~ 2,
      INGLATERRA == "Positiva" ~ 1,
      INGLATERRA == "Indiferencia / desconocimiento" ~ 0,
      INGLATERRA == "Negativa" ~ -1,
      INGLATERRA == "Rechazo" ~ -2
    ),
    BOLIVIA = case_when(
      BOLIVIA == "Cariño" ~ 2,
      BOLIVIA == "Positiva" ~ 1,
      BOLIVIA == "Indiferencia / desconocimiento" ~ 0,
      BOLIVIA == "Negativa" ~ -1,
      BOLIVIA == "Rechazo" ~ -2
    ),
    
    # DEMOGRAFICAS
    GENERO = ifelse(GENERO == "Masculino", 1, 0),
    ESTUDIO = case_when(
      ESTUDIO == "primario COMPLETO" ~ 0,
      ESTUDIO == "Secundario COMPLETO" ~ 1,
      ESTUDIO == "terciario o universitario INCOMPLETO" ~ 2,
      ESTUDIO == "terciario o universitario COMPLETO" ~ 3,
      ESTUDIO == "posgrado o superior" ~ 4
    ),
    PROGRAMA = as.factor(PROGRAMA),
    NOTICIAS = as.factor(NOTICIAS),
    SOCIECON = case_when(
      SOCIECON == "Clase baja" ~ 0,
      SOCIECON == "Clase media baja" ~ 1,
      SOCIECON == "Clase media alta" ~ 2,
      SOCIECON == "Clase alta" ~ 3
    ),
    GRUPO = as.factor(GRUPO),
    PROV = as.factor(PROV),
    # OCUPACIONAL
    TRABAJA = ifelse(TRABAJA == "SI", 1, 0),
    # POLITICA
    ETIQUETA= case_when(
      ETIQUETA %in% c("Apartidario","Idealista") ~ "No sabe/ No contesta",
      TRUE ~ as.character(ETIQUETA)  # Mantiene los valores originales que no se especificaron
    ),
    ETIQUETA = as.factor(ETIQUETA)
  ) %>% 
  select(-TIEMPO,-PROV)

write.xlsx(clean134, file = "134NODUMMY.xlsx")
##########Creando algunas dummies###########################################################################
unique(clean$BUSCA_HIJOS)
# Cargar librería
library(fastDummies)

# Aplicar One-Hot Encoding <AQUI SE ELIMINA UNA MODALIDAD POR VARIABLE PARA REDUCIR COLINEALIDAD>
#dumificada <- dummy_cols(clean, select_columns = c("PROLE","SS","PROGRAMA", "NOTICIAS", "GRUPO", "NO_HIJOS_PQ"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

dumificada134 <- dummy_cols(clean134, select_columns = c("PROLE","SS","PROGRAMA", "NOTICIAS", "GRUPO", "NO_HIJOS_PQ"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)

kmeans134<- dummy_cols(clean134, select_columns = c("PROLE","SS","PROGRAMA", "NOTICIAS", "GRUPO", "NO_HIJOS_PQ"), remove_first_dummy = FALSE, remove_selected_columns = TRUE)

#EVALUACION DE DATOS
summary(clean)
str(clean)

skim(dumificada)


####################################################       ONE HOT ENCODING    #############################

write.xlsx(dumificada134, file = "D134.xlsx")
write.xlsx(kmeans134, file = "kmeans134.xlsx")



