
#########            ENFERMEDAD AUTOINMUNE / SARS-COV-2       #################


#Para instalar y Cargar las librerias que se requieran 
#1
install.packages("magrittr") 
install.packages("dplyr") 
install.packages("meta") 
install.packages("metafor") 
install.packages("readxl") 
install.packages("magrittr") 
install.packages("ggplot2") 
install.packages("gridExtra") 
install.packages("cowplot")
#2
library(magrittr) 
library(dplyr) 
library(meta)
library(metafor)
library(readxl)
library(magrittr) 
library(ggplot2)
library(gridExtra)
library(cowplot)

#Para que lea elarchivo 
Formulario_ext <- read_excel("data/Formulario_ext.xlsx")

#Para explorar la estructura del data frame 
str(Formulario_ext) #enumera las columnas del indicando el tipo de dato y los valores asignados 
names(Formulario_ext) #Enumera las columnas del DF
Formulario_ext$Design #Enumera los valores de cada observación asignados a la columna especificada despues del $

#Para convertir al formato correcto las variables del dataframe: 
#objeto$variable a cambiar <-  Formato al que se desea cambia ej as.numeric(objeto$variable a cambiar)

Formulario_ext$Women <-  as.numeric(Formulario_ext$Women)
Formulario_ext$Women_E  <-  as.numeric(Formulario_ext$Women)
Formulario_ext$Women_C  <-  as.numeric(Formulario_ext$Women_C)
Formulario_ext$Men <-  as.numeric(Formulario_ext$Men)
Formulario_ext$Men_E <-  as.numeric(Formulario_ext$Men_E)
Formulario_ext$Men_C <-  as.numeric(Formulario_ext$Men_C)
Formulario_ext$Outcome_ag <-  as.factor(Formulario_ext$Outcome_ag)
Formulario_ext$Outcome_rep <-  as.factor(Formulario_ext$Outcome_rep)
Formulario_ext$Incidence_E <-  as.numeric(Formulario_ext$Incidence_E)
Formulario_ext$Incidence_C <-  as.numeric(Formulario_ext$Incidence_C)
Formulario_ext$Est_crude <-  as.numeric(Formulario_ext$Est_crude)
Formulario_ext$CI_L <-  as.numeric(Formulario_ext$CI_L)
Formulario_ext$CI_U <-  as.numeric(Formulario_ext$CI_U)

#Para comprobar el cambio (explorar la estructura del data frame)
str(Formulario_ext)
#Para ver los nombres de las columnas y los valores del dataframe y modificar si aplica
names(Formulario_ext)
Formulario_ext$Outcome_rep
#Modificar valores 
Formulario_ext[16,62] <- "Type 1 diabetes mellitus" #Estaba en minuscula.
Formulario_ext$Association_measure # Homogeneizar valores
Formulario_ext$Association_measure <- ifelse(Formulario_ext$Association_measure %in% c("adjusted hazard ratio (aHRs)", "hazard ratio (HRs)"), "IRRs", Formulario_ext$Association_measure)
Formulario_ext$Association_measure <- ifelse(Formulario_ext$Association_measure %in% c("incidence rate ratios (IRRs)"), "IRRs", Formulario_ext$Association_measure)
Formulario_ext$Association_measure <- ifelse(Formulario_ext$Association_measure %in% c("adjusted Odds ratio (ORs)"), "ORs", Formulario_ext$Association_measure)

#Calcular la incidencia acumulada por grupo y añadirla como nueva columna al DT
Formulario_ext <- Formulario_ext %>%
  mutate(IA_E = (Events_E / Included_E) * 100,
         IA_C = (Events_C / Included_C) * 100)
#Crear un dataframe por cada categoria de Outcome_ag (Desenlaces-predefinidos)

#1 Explorar los nombres de los valores únicos de la variable. 
unique(Formulario_ext$Outcome_ag) 

#2 Para saber que outcomes fueron evaluados en más de 1 estudio, se crea una tabla
tabla <- table(Formulario_ext$Outcome_ag)
tabla

#Crear los DF 
#USar: Columna=subset(DF,condición) (Por outcome especifico)

Espondiloartritis=subset(Formulario_ext,Outcome_ag=="Espondiloartritis")
DMT1=subset(Formulario_ext,Outcome_ag=="Diabetes mellitus Tipo 1")
DMT1PP=subset(Formulario_ext,Outcome_ag=="Diabetes mellitus Tipo 1 PP")
EII=subset(Formulario_ext,Outcome_ag=="Enfermedad inflamatoria intestinal")
LES=subset(Formulario_ext,Outcome_ag=="Lupus eritematoso sistémico")
Polimialgia=subset(Formulario_ext,Outcome_ag=="Polimialgia reumática")
Guillain_Barré=subset(Formulario_ext,Outcome_ag=="Síndrome de Guillain Barré")
Behcet=subset(Formulario_ext,Outcome_ag=="Enfermedad de Behcet")
EASNC=subset(Formulario_ext,Outcome_ag=="Enfermedades autoinmunes del sistema nervioso central")
Esclerosis_sistémica=subset(Formulario_ext,Outcome_ag=="Esclerosis sistémica")
Psoriasis=subset(Formulario_ext,Outcome_ag=="Psoriasis")
Sjögrens=subset(Formulario_ext,Outcome_ag=="Sjögren's syndrome")
Vasculitis=subset(Formulario_ext,Outcome_ag=="Vasculitis")
AR=subset(Formulario_ext,Outcome_ag=="Artritis reumatoide")

#Modificar DF diabetes excluyendo el estudio de Zareini pq no se conoce el N
DMT1=subset(DMT1,!(ID_study =="Zareini, 2023"))

#Modificar DF Guillain Barré el estudio de Xu pq no se conoce el # de eventos por grupo
Guillain_Barré=subset(Guillain_Barré,!(ID_study =="Xu, 2022"))

#Modificar DF Guillain Barré segun el periodo de seguimiento del estudi de Mizrahi
SGB2=subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid early phase (30-180 days)"))
SGB1=subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))

#Modificar DF Guillain Barré sumando los eventos de ambos periodos

Guillain_Barré_A=subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))
Guillain_Barré_A[1,65] <- 18
Guillain_Barré_A[1,66] <- 14

#Modificar DF Artritis excluyendo el estudio de Chevinsky pq no se conoce el # de eventos ni el N
AR=subset(AR,!(ID_study =="Chevinsky, 2021"))

#Modificar DF EASNC excluyendo el estudio de Chevinsky pq no se conoce el # de eventos 
EASNC=subset(EASNC,!(ID_study =="Xu, 2022"))

#################              Metanálisis             #########################
################################################################################

#Metanálisis de efectos aleatorios por enfermedad

#1   Behcet="Enfermedad de Behcet"
metabin(Behcet$Events_E, Behcet$Included_E, Behcet$Events_C, Behcet$Included_C, EB=Behcet, sm="RR", method="MH", studlab=paste(Behcet$ID_study), comb.fixed = T,comb.random = T)

#2   Espondiloartritis ="Espondiloartritis"
metabin(Espondiloartritis$Events_E, Espondiloartritis$Included_E, Espondiloartritis$Events_C, Espondiloartritis$Included_C, EB=Espondiloartritis, sm="RR", method="MH", studlab=paste(Espondiloartritis$ID_study), comb.fixed = T,comb.random = T)

#3   DMT1="Diabetes mellitus Tipo 1"   (#Sin n para Zareini, 202)
metabin(DMT1$Events_E, DMT1$Included_E, DMT1$Events_C, DMT1$Included_C, EB=DMT1, sm="RR", method="MH", studlab=paste(DMT1$ID_study), comb.fixed = T,comb.random = T)

#4   DMT1PP="Diabetes mellitus Tipo 1 PP"
metabin(DMT1PP$Events_E, DMT1PP$Included_E, DMT1PP$Events_C, DMT1PP$Included_C, EB=DMT1PP, sm="RR", method="MH", studlab=paste(DMT1PP$ID_study), comb.fixed = T,comb.random = T)

#5   EII="Enfermedad inflamatoria intestinal"
metabin(EII$Events_E, EII$Included_E, EII$Events_C, EII$Included_C, EB=EII, sm="RR", method="MH", studlab=paste(EII$ID_study), comb.fixed = T,comb.random = T)

#6  LES="Lupus eritematoso sistémico"
metabin(LES$Events_E, LES$Included_E, LES$Events_C, LES$Included_C, EB=LES, sm="RR", method="MH", studlab=paste(LES$ID_study), comb.fixed = T,comb.random = T)

#7  Polimialgia_reumática="Polimialgia reumática"
metabin(Polimialgia$Events_E, Polimialgia$Included_E, Polimialgia$Events_C, Polimialgia$Included_C, EB=Polimialgia, sm="RR", method="MH", studlab=paste(Polimialgia$ID_study), comb.fixed = T,comb.random = T)

#8  Guillain_Barré_="Síndrome de Guillain Barré"     
metabin(Guillain_Barré_A$Events_E, Guillain_Barré_A$Included_E, Guillain_Barré_A$Events_C, Guillain_Barré_A$Included_C, EB=Guillain_Barré_A, sm="RR", method="MH", studlab=paste(Guillain_Barré_A$ID_study), comb.fixed = T,comb.random = T)

#9  SGB1="Síndrome de Guillain Barré" incluyendo solo a Mizrahi de 30-180 dias
metabin(SGB1$Events_E, SGB1$Included_E, SGB1$Events_C, SGB1$Included_C, EB=SGB1, sm="RR", method="MH", studlab=paste(SGB1$ID_study), comb.fixed = T,comb.random = T)

#10  SGB2="Síndrome de Guillain Barré" incluyendo solo a Mizrahi de 180-360 dias
metabin(SGB2$Events_E, SGB2$Included_E, SGB2$Events_C, SGB2$Included_C, EB=SGB2, sm="RR", method="MH", studlab=paste(SGB2$ID_study), comb.fixed = T,comb.random = T)

#11  Esclerosis_sistémica="Esclerosis sistémica"
metabin(Esclerosis_sistémica$Events_E, Esclerosis_sistémica$Included_E, Esclerosis_sistémica$Events_C, Esclerosis_sistémica$Included_C, EB=Esclerosis_sistémica, sm="RR", method="MH", studlab=paste(Esclerosis_sistémica$ID_study), comb.fixed = T,comb.random = T)

#12  Psoriasis="Psoriasis"
metabin(Psoriasis$Events_E, Psoriasis$Included_E, Psoriasis$Events_C, Psoriasis$Included_C, EB=Psoriasis, sm="RR", method="MH", studlab=paste(Psoriasis$ID_study), comb.fixed = T,comb.random = T)

#13  Sjögrens="Sjögren's syndrome"
metabin(Sjögrens$Events_E, Sjögrens$Included_E, Sjögrens$Events_C, Sjögrens$Included_C, EB=Sjögrens, sm="RR", method="MH", studlab=paste(Sjögrens$ID_study), comb.fixed = T,comb.random = T)

#14  Vasculitis
metabin(Vasculitis$Events_E, Vasculitis$Included_E, Vasculitis$Events_C, Vasculitis$Included_C, EB=Vasculitis, sm="RR", method="MH", studlab=paste(Vasculitis$ID_study), comb.fixed = T,comb.random = T)

#15  Artritis reumatoide
metabin(AR$Events_E, AR$Included_E, AR$Events_C, AR$Included_C, EB=AR, sm="RR", method="MH", studlab=paste(AR$ID_study), comb.fixed = T,comb.random = T)

#16  EASNC
metabin(EASNC$Events_E, EASNC$Included_E, EASNC$Events_C, EASNC$Included_C, EB=EASNC, sm="RR", method="MH", studlab=paste(EASNC$ID_study), comb.fixed = T,comb.random = T)
 

##Metanálisis incluyendo casos con medicación
#ANÁLISIS NO INCLUIDO EN EL ARTICULO
#1 Crear una copia de los DF que incluyan los casos con medicación del estudio de tesh 
AR_m <- AR
Behcet_m <- Behcet
EII_m <- EII
EASNC_m <- EASNC
Espondiloartritis_m <- Espondiloartritis
LES_m <- LES
Polimialgia_m <- Polimialgia
Psoriasis_m <- Psoriasis
Sjögrens_m <- Sjögrens
Vasculitis_m <- Vasculitis

#2 Cambiar los valores correspondientes a las variables de resultado
#Para saber el numero de fila que se desea modificar en cada DF
names(AR_m)
names(Psoriasis_m)
#etc

#3 Modificar los valores

# Valores que deseas asignar a la fila 2
valores_AR_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)

# Asignar los nuevos valores a la fila 2 en las columnas correspondientes
AR_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_AR_m

valores_Psoriasis_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Psoriasis_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Psoriasis_m

valores_Sjögrens_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Sjögrens_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Sjögrens_m

valores_Polimialgia_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Polimialgia_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Polimialgia_m

valores_Espondiloartritis_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Espondiloartritis_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Espondiloartritis_m

valores_LES_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
LES_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_LES_m

valores_EII_m2 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
EII_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_EII_m2

valores_EII_m3 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
EII_m[3, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_EII_m3

valores_Vasculitis_m2 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Vasculitis_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Vasculitis_m2

valores_Vasculitis_m3 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Vasculitis_m[3, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Vasculitis_m3


#Metanálisis de efectos aleatorios por enfermedad inclyedo casos con medicación del estudio de Tesh 
#ANÁLISIS NO INCLUIDO EN EL ARTICULO

#2   Espondiloartritis ="Espondiloartritis"
metabin(Espondiloartritis_m$Events_E, Espondiloartritis_m$Included_E, Espondiloartritis_m$Events_C, Espondiloartritis_m$Included_C, EB=Espondiloartritis_m, sm="RR", method="MH", studlab=paste(Espondiloartritis_m$ID_study), comb.fixed = T,comb.random = T)

#5   EII="Enfermedad inflamatoria intestinal"
metabin(EII_m$Events_E, EII_m$Included_E, EII_m$Events_C, EII_m$Included_C, EB=EII_m, sm="RR", method="MH", studlab=paste(EII_m$ID_study), comb.fixed = T,comb.random = T)

#1  LES="Lupus eritematoso sistémico"
metabin(LES_m$Events_E, LES_m$Included_E, LES_m$Events_C, LES_m$Included_C, EB=LES_m, sm="RR", method="MH", studlab=paste(LES_m$ID_study), comb.fixed = T,comb.random = T)

#2  Polimialgia_reumática="Polimialgia reumática"
metabin(Polimialgia_m$Events_E, Polimialgia_m$Included_E, Polimialgia_m$Events_C, Polimialgia_m$Included_C, EB=Polimialgia_m, sm="RR", method="MH", studlab=paste(Polimialgia_m$ID_study), comb.fixed = T,comb.random = T)

#3  Psoriasis="Psoriasis"
metabin(Psoriasis_m$Events_E, Psoriasis_m$Included_E, Psoriasis_m$Events_C, Psoriasis_m$Included_C, EB=Psoriasis_m, sm="RR", method="MH", studlab=paste(Psoriasis_m$ID_study), comb.fixed = T,comb.random = T)

#4  Sjögrens="Sjögren's syndrome"
metabin(Sjögrens_m$Events_E, Sjögrens_m$Included_E, Sjögrens_m$Events_C, Sjögrens_m$Included_C, EB=Sjögrens_m, sm="RR", method="MH", studlab=paste(Sjögrens_m$ID_study), comb.fixed = T,comb.random = T)

#5  Vasculitis
metabin(Vasculitis_m$Events_E, Vasculitis_m$Included_E, Vasculitis_m$Events_C, Vasculitis_m$Included_C, EB=Vasculitis_m, sm="RR", method="MH", studlab=paste(Vasculitis_m$ID_study), comb.fixed = T,comb.random = T)

#6  Artritis reumatoide
metabin(AR_m$Events_E, AR_m$Included_E, AR_m$Events_C, AR_m$Included_C, EB=AR_m, sm="RR", method="MH", studlab=paste(AR_m$ID_study), comb.fixed = T,comb.random = T)


########################GRAFICAS ANÄLISIS DESCITPRIVO#################################

#para sacar una dt solo con los datos de resultado 
Data <- Formulario_ext[, c("ID_study","Comparisons", "Outcome_ag","Outcome_rep", "Included_E", "Included_C", "Events_E", "Events_C", "Incidence_E", "Incidence_C", "Association_measure", "Est_crude", "CI_L", "CI_U", "Est_a", "CI_La", "CI_Ua", "IA_E", "IA_C")]
##Concatenar el estudio y el desenlace y añadirlo como nueva columna del DF 
Data$ID_forrest <- paste(Data$Outcome_rep, Data$ID_study, sep = " / ")
##Concatenar los límites de los intervalos de confianza
Data$IC <- paste(Data$CI_La, Data$CI_Ua, sep = " - ")
#Cambiar el ID del estudio de Mizhari para diferenciar los seguimientos
#Se verifica el numero de fila y columnas que se desean cambiar
names(Data)
Data$ID_forrest
#Modificar los nombre del ID de Mizhari para separar los resultados segun el seguimiento: objeto[#Columna,#Fila] <- "nuevo valor"
Data[14,20] <- "Guillain_Barré / Mizrahi, 2023*"
Data[15,20] <- "Guillain_Barré / Mizrahi, 2023*"
#Eliminar el registro de Chevisnsky pq no tiene datos
Data=subset(Data,!(ID_study =="Chevinsky, 2021"))
#Ordenar el dt por orden alfabetico segun la condición/estudio
Data <- Data[order(Data$ID_forrest), ]
#Añadir una columna al dt con el index 
Data$Index <- seq_len(nrow(Data))

#Crear un nuevo data frame por tipo de metrica 
DataHR=subset(Data,Association_measure=="adjusted hazard ratio (aHRs)")
DataIRR=subset(Data,Association_measure=="incidence rate ratios (IRRs)")


#########################Forrest plot resultados descriptivos####################

#Grafica resultados incliuendo ambos tipos de métrica)

plot1 <- ggplot(Data, aes(y = Index, x = Est_a)) +
  geom_point(aes(color = Data$Association_measure), shape = 18, size = 3) +  
  geom_errorbarh(aes(xmin = CI_La, xmax = CI_Ua), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks = 1:46, labels = Data$ID_forrest, trans = "reverse") +
  xlab("Efect (95% CI)") + 
  ylab(" ") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black", hjust = 0),  # Alinear a la izquierda
        axis.text.x.bottom = element_text(size = 10, colour = "black"),  
        axis.title.x = element_text(size = 12, colour = "black", face = "bold")) # Ajuste para hacer la etiqueta del eje x en negrita

plot1

## Create the table-base pallete
table_base <- ggplot(Data, aes(y=ID_forrest)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=10), 
        axis.text.x = element_text(color="white", hjust = -1, size = 30), ## para ayudar con la alineación de las columnas
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## Efecto point estimate table
tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(Est_a, digits = 1))), size = 4) +
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("Efect")

## 95% CI table
tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = IC), size = 4) + 
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("95% CI")

## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(plot1, tab1, tab2, layout_matrix = lay)


#Grafica resultados IRR

plot2 <- ggplot(DataIRR, aes(y = Index, x = Est_a)) +
  geom_point(shape = 18, size = 3) +  
  geom_errorbarh(aes(xmin = CI_La, xmax = CI_Ua), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:25, labels = DataIRR$ID_forrest, trans = "reverse") +
  xlab("Incidence Rate Ratio (95% CI)") + 
  ylab(" ") + 
  labs(y = "Condition / Study") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),  
        axis.title.x = element_text(size = 12, colour = "black", face = "bold")) # Ajuste para hacer la etiqueta del eje x en negrita
plot2

## Create the table-base pallete
table_base2 <- ggplot(DataIRR, aes(y=ID_forrest)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -1, size = 30), ## Used para ayudar con la alineación
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## HR point estimate table
tab3 <- table_base2 + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(Est_a, digits = 1))), size = 4) +
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("IRR")

## 95% CI table
tab4 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = IC), size = 4) + 
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("95% CI")

#Grafica resultados combinados

plot2 <- ggplot(DataIRR, aes(y = Index, x = Est_a)) +
  geom_point(shape = 18, size = 3) +  
  geom_errorbarh(aes(xmin = CI_La, xmax = CI_Ua), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:25, labels = DataIRR$ID_forrest, trans = "reverse") +
  xlab("Incidence Rate Ratio (95% CI)") + 
  ylab(" ") + 
  labs(y = "Condition / Study") +
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 12, colour = "black"),
        axis.text.x.bottom = element_text(size = 12, colour = "black"),  
        axis.title.x = element_text(size = 12, colour = "black", face = "bold")) # Ajuste para hacer la etiqueta del eje x en negrita
plot2

## Create the table-base pallete
table_base2 <- ggplot(DataIRR, aes(y=ID_forrest)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -1, size = 30), ## Used para ayudar con la alineación
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

## HR point estimate table
tab3 <- table_base2 + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(Est_a, digits = 1))), size = 4) +
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("IRR")

## 95% CI table
tab4 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = IC), size = 4) + 
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("95% CI")
## Merge tables with plot
lay2 <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(plot2, tab3, tab4, layout_matrix = lay2)

#Unir los 2 gráficos:

grid.arrange(plot1, plot2, nrow = 1)
plot_grid(plot1, plot2, labels = c("A", "B"))  






















#Para cambiar valores especificos de una columna del dataframe 
#Usar: objeto[#fila,#columna] <- "nuevo valor"

#Para cambiar valores especificos de una columna del dataframe
#1 Verifica el numero de fila y columnas que se desean cambiar
names(Formulario_ext) #Fila
Formulario_ext$Outcome_ag #Columna 
#2 Modificar los valores coon: objeto[#fila,#columna] <- "nuevo valor"
Formulario_ext[40,61] <- "Vasculitis"
Formulario_ext[24,61] <- "Enfermedades autoinmunes del sistema nervioso central"

#Para borrar objeos creados del ambiente de trabajo
rm(EASNC_m) 
rm(Alopecia_areata) 
rm(Anemia_hemolítica) 
rm(Diabetes_mellitus_1PI) 
rm(list = ls())