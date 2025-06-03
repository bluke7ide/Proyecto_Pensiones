BD_Cotizantes <- read_excel("data/BD_Cotizantes.xlsx")
colnames(BD_Cotizantes)[4:ncol(BD_Cotizantes)] <- as.character(
  as.Date(as.numeric(colnames(BD_Cotizantes)[4:ncol(BD_Cotizantes)]),
          origin = "1899-12-30"))
BD_Financiero <- list(
  read_excel("data/BD_Financiero.xlsx", sheet = "EEFF", 
             col_names = c("Tipo", "Monto", "Detalle"), skip = 52),
  read_excel("data/BD_Financiero.xlsx", sheet = "Activo-EEFF"),
  read_excel("data/BD_Financiero.xlsx", 
             sheet = "Datos Historicos",
             col_types = c("date","numeric", "numeric", "numeric",
                           "numeric", "skip", "skip", "skip"))
  )
BD_Pensionados <- read_excel("data/BD_Pensionados.xlsx", 
                             sheet = "Fondo A")
SUPEN <- read_excel("data/tavid2000-2150.xls")
SUPEN <- as.data.frame(lapply(SUPEN, as.numeric), stringsAsFactors = FALSE)
IPC <- read_excel("data/ipc_inec.xlsx", skip = 3)

invalidez <- read_excel("data/Tabla_Invalidez.xlsx")
invalidez <- invalidez %>% 
  pivot_longer(c(Hombres, Mujeres), names_to = "sexo", values_to = "qi") %>%
  mutate(sexo = ifelse(sexo == "Hombres", 1, 2))
invalidez <- data.frame(invalidez)