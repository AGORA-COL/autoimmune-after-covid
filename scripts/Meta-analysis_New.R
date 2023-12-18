
#########            ENFERMEDAD AUTOINMUNE / SARS-COV-2       #################


library(magrittr) 
library(dplyr) 
library(meta)
library(metafor)
library(readxl)
library(magrittr) 
library(ggplot2)
library(gridExtra)
library(cowplot)

#################Crear un df igual a la base en excel ##########################
################################################################################
Formulario_ext <- read_excel("data/Formulario_ext.xlsx")

######################### REVISION DEL df #####################################
################################################################################
#Para explorar la estructura del data frame 
str(Formulario_ext) #enumera las columnas del indicando el tipo de dato y los valores asignados 

#Para convertir al formato correcto las variables del dataframe: 
#objeto$variable a cambiar <-  Formato al que se desea cambia ej as.numeric(objeto$variable a cambiar)

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
Formulario_ext$Outvome_ag

################# Incidencia acumulada por desenlace ##########################
################################################################################
#Calcular la incidencia acumulada por grupo y añadirla como nueva columna al DT
Formulario_ext <- Formulario_ext %>%
  mutate(IA_E = (Events_E / Included_E) * 100,
         IA_C = (Events_C / Included_C) * 100)


##############df por cada Outcome_ag (Desenlaces-predefinidos)################
################################################################################
#1 Explorar los nombres de los valores únicos de la variable. 
unique(Formulario_ext$Outcome_ag) 

#2 Para saber que outcomes fueron evaluados en más de 1 estudio, se crea una tabla
tabla <- table(Formulario_ext$Outcome_ag)
tabla

#Crear los DF 
#USar: Columna=subset(DF,condición) (Por outcome especifico)

Spondyloarthritis=subset(Formulario_ext,Outcome_ag=="Ankylosing spondylitis")
DMT1=subset(Formulario_ext,Outcome_ag=="Type 1 diabetes mellitus")
DMT1PP=subset(Formulario_ext,Outcome_ag=="Type 1 diabetes mellitus PP")
IBD=subset(Formulario_ext,Outcome_ag=="Inflammatory Bowel Disease")
SLE=subset(Formulario_ext,Outcome_ag=="Systemic Lupus Erythematosus")
Polymyalgia=subset(Formulario_ext,Outcome_ag=="Polymyalgia rheumatica")
Guillain_Barré=subset(Formulario_ext,Outcome_ag=="Guillain-Barré syndrome")
Behcet=subset(Formulario_ext,Outcome_ag=="Behcet’s disease")
ADNS=subset(Formulario_ext,Outcome_ag=="Autoimmune Diseases of the Nervous System")
Systemic_scleroderma=subset(Formulario_ext,Outcome_ag=="Systemic scleroderma")
Psoriasis=subset(Formulario_ext,Outcome_ag=="Psoriasis")
Sjögrens=subset(Formulario_ext,Outcome_ag=="Sjögren's syndrome")
Vasculitis=subset(Formulario_ext,Outcome_ag=="Vasculitis")
RA=subset(Formulario_ext,Outcome_ag=="Rheumatoid arthritis")
ATD=subset(Formulario_ext,Outcome_ag=="Autoimmune Thyroid Disease")

#Modificar DF Guillain Barré el estudio de Xu pq no se conoce el # de eventos por grupo
Guillain_Barré=subset(Guillain_Barré,!(ID_study =="Xu, 2022"))

#Modificar DF Guillain Barré segun el periodo de seguimiento del estudi de Mizrahi
SGB2=subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid early phase (30-180 days)"))
SGB1=subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))

#Modificar DF Guillain Barré sumando los eventos de ambos periodos el estudi de Mizrahi
#Se elimina un registro de mizhari para conservar 1 registro por estudio 
Guillain_Barré_A=subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))

# se cambian los datos correspondientes a los eventos en cada grupo sumando los eventos de ambos periodos del estudio de Mizrahi
names(Guillain_Barré_A) 
Guillain_Barré_A[1,62] <- 18
Guillain_Barré_A[1,63] <- 14

#Modificar DF Artritis excluyendo el estudio de Chevinsky pq no se conoce el # de eventos ni el N
RA=subset(RA,!(ID_study =="Chevinsky, 2021"))

#Modificar DF Vasculitis sumando los eventos de  Giant cell arteritis y Granulomatosis with polyangiitis del estudio de Tesch
Vasculitis=subset(Vasculitis,!(Outcome_rep =="Giant Cell Arteritis"))
valores_Vasculitis <- list(Events_E = 94, Events_C = 49)
Vasculitis[2, c("Events_E", "Events_C")] <- valores_Vasculitis

#Modificar DF IBD sumando los eventos de  Crohn’s disease y Ulcerative colitis del estudio de Tesch
IBD=subset(Vasculitis,!(Outcome_rep =="Ulcerative colitis"))
valores_IBD <- list(Events_E = 665, Events_C = 517)
IBD[2, c("Events_E", "Events_C")] <- valores_IBD


#################              Metanálisis             #########################
################################################################################

#Metanálisis de efectos aleatorios por enfermedad

#1   Behcet="Enfermedad de Behcet"
metabin(Behcet$Events_E, Behcet$Included_E, Behcet$Events_C, Behcet$Included_C, EB=Behcet, sm="RR", method="MH", studlab=paste(Behcet$ID_study), comb.fixed = T,comb.random = T)

#2   Spondyloarthritis ="Espondiloartritis"
metabin(Spondyloarthritis$Events_E, Spondyloarthritis$Included_E, Spondyloarthritis$Events_C, Spondyloarthritis$Included_C, EB=Spondyloarthritis, sm="RR", method="MH", studlab=paste(Spondyloarthritis$ID_study), comb.fixed = T,comb.random = T)

#3   DMT1="Diabetes mellitus Tipo 1"   (#Sin n para Zareini, 202)
metabin(DMT1$Events_E, DMT1$Included_E, DMT1$Events_C, DMT1$Included_C, EB=DMT1, sm="RR", method="MH", studlab=paste(DMT1$ID_study), comb.fixed = T,comb.random = T)

#4   DMT1PP="Diabetes mellitus Tipo 1 PP"
metabin(DMT1PP$Events_E, DMT1PP$Included_E, DMT1PP$Events_C, DMT1PP$Included_C, EB=DMT1PP, sm="RR", method="MH", studlab=paste(DMT1PP$ID_study), comb.fixed = T,comb.random = T)

#5   IBD="Enfermedad inflamatoria intestinal"
metabin(IBD$Events_E, IBD$Included_E, IBD$Events_C, IBD$Included_C, EB=IBD, sm="RR", method="MH", studlab=paste(IBD$ID_study), comb.fixed = T,comb.random = T)

#6  SLE="Lupus eritematoso sistémico"
metabin(SLE$Events_E, SLE$Included_E, SLE$Events_C, SLE$Included_C, EB=SLE, sm="RR", method="MH", studlab=paste(SLE$ID_study), comb.fixed = T,comb.random = T)

#7  Polymyalgia_reumática="Polimialgia reumática"
metabin(Polymyalgia$Events_E, Polymyalgia$Included_E, Polymyalgia$Events_C, Polymyalgia$Included_C, EB=Polymyalgia, sm="RR", method="MH", studlab=paste(Polymyalgia$ID_study), comb.fixed = T,comb.random = T)

#8  Guillain_Barré_="Síndrome de Guillain Barré" (incluye todos los registros y estudios)     
metabin(Guillain_Barré_A$Events_E, Guillain_Barré_A$Included_E, Guillain_Barré_A$Events_C, Guillain_Barré_A$Included_C, EB=Guillain_Barré_A, sm="RR", method="MH", studlab=paste(Guillain_Barré_A$ID_study), comb.fixed = T,comb.random = T)

#9  SGB1="Síndrome de Guillain Barré" incluyendo  a Mizrahi de 30-180 dias
metabin(SGB1$Events_E, SGB1$Included_E, SGB1$Events_C, SGB1$Included_C, EB=SGB1, sm="RR", method="MH", studlab=paste(SGB1$ID_study), comb.fixed = T,comb.random = T)

#10  SGB2="Síndrome de Guillain Barré" incluyendo  a Mizrahi de 180-360 dias
metabin(SGB2$Events_E, SGB2$Included_E, SGB2$Events_C, SGB2$Included_C, EB=SGB2, sm="RR", method="MH", studlab=paste(SGB2$ID_study), comb.fixed = T,comb.random = T)

#11  Esclerosis_sistémica="Esclerosis sistémica"
metabin(Systemic_scleroderma$Events_E, Systemic_scleroderma$Included_E, Systemic_scleroderma$Events_C, Systemic_scleroderma$Included_C, EB=Systemic_scleroderma, sm="RR", method="MH", studlab=paste(Systemic_scleroderma$ID_study), comb.fixed = T,comb.random = T)

#12  Psoriasis="Psoriasis"
metabin(Psoriasis$Events_E, Psoriasis$Included_E, Psoriasis$Events_C, Psoriasis$Included_C, EB=Psoriasis, sm="RR", method="MH", studlab=paste(Psoriasis$ID_study), comb.fixed = T,comb.random = T)

#13  Sjögrens="Sjögren's syndrome"
metabin(Sjögrens$Events_E, Sjögrens$Included_E, Sjögrens$Events_C, Sjögrens$Included_C, EB=Sjögrens, sm="RR", method="MH", studlab=paste(Sjögrens$ID_study), comb.fixed = T,comb.random = T)

#14  Vasculitis
metabin(Vasculitis$Events_E, Vasculitis$Included_E, Vasculitis$Events_C, Vasculitis$Included_C, EB=Vasculitis, sm="RR", method="MH", studlab=paste(Vasculitis$ID_study), comb.fixed = T,comb.random = T)

#15  Artritis reumatoide
metabin(RA$Events_E, RA$Included_E, RA$Events_C, RA$Included_C, EB=RA, sm="RR", method="MH", studlab=paste(RA$ID_study), comb.fixed = T,comb.random = T)


########################GRAFICAS ANÄLISIS DESCITPRIVO#################################

#para sacar una dt solo con los datos de resultado 
Data_g <- Formulario_ext[, c("ID_study","Comparisons", "Outcome_ag","Outcome_rep", "Included_E", "Included_C", "Events_E", "Events_C", "Incidence_E", "Incidence_C", "Association_measure", "Est_crude", "CI_L", "CI_U", "Est_a", "CI_La", "CI_Ua", "IA_E", "IA_C")]
##Concatenar el estudio y el desenlace y añadirlo como nueva columna del DF 
Data_g$ID_forrest <- paste(Data_g$Outcome_rep, Data_g$ID_study, sep = " / ")
##Concatenar los límites de los intervalos de confianza
Data_g$IC <- paste(Data_g$CI_La, Data_g$CI_Ua, sep = " - ")
#Cambiar el ID del estudio de Mizhari para diferenciar los seguimientos
#Se verifica el numero de fila y columnas que se desean cambiar
names(Data_g)
Data_g$ID_forrest
#Modificar los nombre del ID de Mizhari para separar los resultados segun el seguimiento: objeto[#Columna,#Fila] <- "nuevo valor"
Data_g[14,20] <- "Guillain_Barré / Mizrahi, 2023*"
Data_g[15,20] <- "Guillain_Barré / Mizrahi, 2023*"
#Eliminar el registro de Chevisnsky pq no tiene datos
Data_g=subset(Data_g,!(ID_study =="Chevinsky, 2021"))
#Ordenar el dt por orden alfabetico segun la condición/estudio
Data_g <- Data_g[order(Data_g$ID_forrest), ]
#Añadir una columna al dt con el index 
Data_g$Index <- seq_len(nrow(Data_g))

#Crear un nuevo data frame por tipo de metrica 
DataHR=subset(Data_g,Association_measure=="Hazard Ratio (HR)")
DataIRR=subset(Data_g,Association_measure=="Incidence Rate Ratios (IRR)")


###################Forrest plot resultados descriptivos########################

#Grafica resultados incliuendo ambos tipos de métrica)

#plot resultados reportados por decenlace y estuio 
Descriptivos <- ggplot(Data_g, aes(y = Index, x = Est_a)) +
  geom_point(aes(color = Data_g$Association_measure), shape = 18, size = 3) +  
  geom_errorbarh(aes(xmin = CI_La, xmax = CI_Ua), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks = 1:46, labels = Data_g$ID_forrest, trans = "reverse") +
  xlab("Efect (95% CI)") + 
  ylab(" ") + 
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size = 10, colour = "black", hjust = 0),  # Alinear a la izquierda
        axis.text.x.bottom = element_text(size = 10, colour = "black"),  
        axis.title.x = element_text(size = 12, colour = "black", face = "bold"))+ # Ajuste para hacer la etiqueta del eje x en negrita
  labs(color = "Association measure")
Descriptivos

#PAra incorporar el efecto y su intervalo en el plot  
## Create the table-base pallete
table_base <- ggplot(Data_g, aes(y=ID_forrest)) +
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
Efecto <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(Est_a, digits = 1))), size = 4) +
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("Efect")

## 95% CI table
CI_conc <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = IC), size = 4) + 
  theme(plot.title = element_text(face = "bold")) +
  ggtitle("95% CI")

## Merge tables with plot
lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(Descriptivos, Efecto, CI_conc, layout_matrix = lay)



# FIN ################################################################################
################################################################################
################################################################################

#ANÁLISIS NO INCLUIDOs EN EL ARTICULO
##Metanálisis incluyendo casos con medicación del estudio de tesch
#1 Crear una copia de los DF que incluyan los casos con medicación del estudio de tesh 
RA_m <- RA
Behcet_m <- Behcet
IBD_m <- IBD
ADNS_m <- ADNS
Spondyloarthritis_m <- Spondyloarthritis
SLE_m <- SLE
Polymyalgia_m <- Polymyalgia
Psoriasis_m <- Psoriasis
Sjögrens_m <- Sjögrens
Vasculitis_m <- Vasculitis

#2 Cambiar los valores correspondientes a las variables de resultado
#Para saber el numero de fila que se desea modificar en cada DF
names(RA_m)
names(Psoriasis_m)
#etc

#3 Modificar los valores

# Valores que deseas asignar a la fila 2
valores_RA_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)

# Asignar los nuevos valores a la fila 2 en las columnas correspondientes
RA_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_RA_m

valores_Psoriasis_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Psoriasis_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Psoriasis_m

valores_Sjögrens_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Sjögrens_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Sjögrens_m

valores_Polymyalgia_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Polymyalgia_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Polymyalgia_m

valores_Spondyloarthritis_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Spondyloarthritis_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Spondyloarthritis_m

valores_SLE_m <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
SLE_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_SLE_m

valores_IBD_m2 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
IBD_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_IBD_m2

valores_IBD_m3 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
IBD_m[3, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_IBD_m3

valores_Vasculitis_m2 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Vasculitis_m[2, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Vasculitis_m2

valores_Vasculitis_m3 <- list(Events_E = 611, Events_C = 421, Incidence_E = 1.26, Incidence_C = 0.87, Est_a = 1.45, CI_La = 1.28, CI_Ua = 1.64)
Vasculitis_m[3, c("Events_E", "Events_C", "Incidence_E", "Incidence_C", "Est_a", "CI_La", "CI_Ua")] <- valores_Vasculitis_m3


#Metanálisis de efectos aleatorios por enfermedad inclyedo casos con medicación del estudio de Tesh 
#ANÁLISIS NO INCLUIDO EN EL ARTICULO

#2   Espondiloartritis ="Espondiloartritis"
metabin(Spondyloarthritis_m$Events_E, Spondyloarthritis_m$Included_E, Spondyloarthritis_m$Events_C, Spondyloarthritis_m$Included_C, EB=Spondyloarthritis_m, sm="RR", method="MH", studlab=paste(Spondyloarthritis_m$ID_study), comb.fixed = T,comb.random = T)

#5   EII="Enfermedad inflamatoria intestinal"
metabin(IBD_m$Events_E, IBD_m$Included_E, IBD_m$Events_C, IBD_m$Included_C, EB=IBD_m, sm="RR", method="MH", studlab=paste(IBD_m$ID_study), comb.fixed = T,comb.random = T)

#1  LES="Lupus eritematoso sistémico"
metabin(SLE_m$Events_E, SLE_m$Included_E, SLE_m$Events_C, SLE_m$Included_C, EB=SLE_m, sm="RR", method="MH", studlab=paste(SLE_m$ID_study), comb.fixed = T,comb.random = T)

#2  Polimialgia_reumática="Polimialgia reumática"
metabin(Polymyalgia_m$Events_E, Polymyalgia_m$Included_E, Polymyalgia_m$Events_C, Polymyalgia_m$Included_C, EB=Polymyalgia_m, sm="RR", method="MH", studlab=paste(Polymyalgia_m$ID_study), comb.fixed = T,comb.random = T)

#3  Psoriasis="Psoriasis"
metabin(Psoriasis_m$Events_E, Psoriasis_m$Included_E, Psoriasis_m$Events_C, Psoriasis_m$Included_C, EB=Psoriasis_m, sm="RR", method="MH", studlab=paste(Psoriasis_m$ID_study), comb.fixed = T,comb.random = T)

#4  Sjögrens="Sjögren's syndrome"
metabin(Sjögrens_m$Events_E, Sjögrens_m$Included_E, Sjögrens_m$Events_C, Sjögrens_m$Included_C, EB=Sjögrens_m, sm="RR", method="MH", studlab=paste(Sjögrens_m$ID_study), comb.fixed = T,comb.random = T)

#5  Vasculitis
metabin(Vasculitis_m$Events_E, Vasculitis_m$Included_E, Vasculitis_m$Events_C, Vasculitis_m$Included_C, EB=Vasculitis_m, sm="RR", method="MH", studlab=paste(Vasculitis_m$ID_study), comb.fixed = T,comb.random = T)

#6  Artritis reumatoide
metabin(RA_m$Events_E, RA_m$Included_E, RA_m$Events_C, RA_m$Included_C, EB=RA_m, sm="RR", method="MH", studlab=paste(RA_m$ID_study), comb.fixed = T,comb.random = T)


rm(DMT1) 

