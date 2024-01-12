
# Risk of new onset of immune-mediated diseases
# after SARS-CoV-2 infection: A Systematic review
# and meta-analysis.

rm(list=ls())
library(magrittr) 
library(dplyr) 
library(meta)
library(metafor)
library(readxl)
library(magrittr) 
library(ggplot2)
library(gridExtra)
library(cowplot)
library(readr)


################################################################################
dat <- read_excel("data/data_extracted_SR_immune_covid.xlsx")

dat$Incidence_E <-  as.numeric(dat$Incidence_E)
dat$Incidence_C <-  as.numeric(dat$Incidence_C)
dat$Est_crude <-  as.numeric(dat$Est_crude)
dat$CI_L <-  as.numeric(dat$CI_L)
dat$CI_U <-  as.numeric(dat$CI_U)
dat$Est_a <-  as.numeric(dat$Est_a)
dat$CI_La <-  as.numeric(dat$CI_La)
dat$CI_Ua <-  as.numeric(dat$CI_Ua)
dat$Events_E <-  as.numeric(dat$Events_E)
dat$Events_C <-  as.numeric(dat$Events_C)
dat$Included_E <-  as.numeric(dat$Included_E)
dat$Included_C <-  as.numeric(dat$Included_C)

##============Cummulative Incidence 
dat <- dat %>%
  mutate(IA_E = round((Events_E / Included_E) * 100000, 2), 
         IA_C = round((Events_C / Included_C) * 100000, 2))

##============================================================================
##==========================DESCRIPTIVE ANALYSIS =============================
##============================================================================

datg <- dat[, c("ID_study","Comparisons", "Outcome_ag","Outcome_rep", "Included_E", "Included_C", "Events_E", "Events_C", "Incidence_E", "Incidence_C", "Association_measure", "Est_crude", "CI_L", "CI_U", "Est_a", "CI_La", "CI_Ua", "IA_E", "IA_C")]
datg <- subset(datg,!(ID_study =="Chevinsky, 2021")) # Removing Chevinsky because XXXX
datg <- datg[order(datg$Outcome_rep), ]
datg$CI_La <- round(datg$CI_La, 2)
datg$CI_Ua <- round(datg$CI_La, 2)
datg$CI <- paste0(datg$CI_La, "-", datg$CI_Ua)
datg$Est_a <- paste(round(datg$Est_a, 2))
datg$IA_E  <- paste0(round (datg$IA_E, 2))
datg$IA_C  <- paste0(round (datg$IA_C, 2))
datg$Central_Estimate <- ifelse (datg$Association_measure == "IRR", paste0(datg$Est_a, "*"), paste0(datg$Est_a, "^"))

# Create Table 2. 
table_2 <- datg %>% select(Outcome_Family = Outcome_ag,
                           Outcome = Outcome_rep, 
                           Study = ID_study,
                           Exposed = Included_E, 
                           Non_Exposed = Included_C, 
                           Events_Exposed = Events_E,
                           Events_Non_Exposed = Events_C, 
                           Cumm_Incidence_Exposed = IA_E,
                           Cumm_Incidence_Non_Exposed = IA_C,
                           Central_Estimate = Central_Estimate,
                           Confidence_Interval = CI)
write_excel_csv2(table_2, "results/Table2.xls")

##============================================================================
##========================== META-ANALYISIS ==================================
##============================================================================
events_more_1study <- dat %>% group_by(Outcome_ag) %>% summarise(n = n()) %>% filter (n> 1)
# Aquí falta una explicación de porqué se selceccionan 13 desenlaces si de arriba se deriva 
# que hay 15 desenlaces con >1 estudio
Spondyloarthritis <- subset(dat,Outcome_ag=="Spondyloarthritis")
DMT1 <- subset(dat,Outcome_ag=="Type 1 diabetes mellitus")
DMT1PP <- subset(dat,Outcome_ag=="Type 1 diabetes mellitus PP") %>% 
  mutate(Outcome_ag = "Type 1 diabetes mellitus (pediatric)")
IBD <- subset(dat,Outcome_ag=="Inflammatory bowel disease")
SLE <- subset(dat,Outcome_ag=="Systemic lupus erythematosus")
Polymyalgia <- subset(dat,Outcome_ag=="Polymyalgia rheumatica")
Guillain_Barré <- subset(dat,Outcome_ag=="Guillain-Barré syndrome")
Behcet <- subset(dat,Outcome_ag=="Behcet’s disease")
Systemic_scleroderma <- subset(dat,Outcome_ag=="Systemic sclerosis")
Psoriasis <- subset(dat,Outcome_ag=="Psoriasis")
Sjögrens <- subset(dat,Outcome_ag=="Sjögren's syndrome")
Vasculitis <- subset(dat,Outcome_ag=="Vasculitis")
RA <- subset(dat,Outcome_ag=="Rheumatoid arthritis")

##================ Excluding and sub-setting data before final step

# Excluding Zareini et al. as number of individuals >30 days is missing
DMT1 <- subset(DMT1,!(ID_study =="Zareini, 2023"))
# Excluding Xu, 2022 # of events is missing 
Guillain_Barré <- subset(Guillain_Barré,!(ID_study =="Xu, 2022"))
# Subsetting GB by follow-up based on Mizrahi et al.
SGB2 <- subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid early phase (30-180 days)"))
SGB1 <- subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))

# Modificar DF Guillain Barré sumando los eventos de ambos periodos el estudi de Mizrahi
# Se elimina un registro de mizhari para conservar 1 registro por estudio 
Guillain_Barré_A <- subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))
# Aquí no entiendo la diferencia entre SGB1 y Guillain_Barré_A ------ ZULMA

# se cambian los datos correspondientes a los eventos en cada grupo sumando los eventos de ambos periodos del estudio de Mizrahi
Guillain_Barré_A[1, "Events_E"] <- 18 
Guillain_Barré_A[1, "Events_C"] <- 14

# Excluding Chevinsky, 2021 as  # of events and N is missing
RA <- subset(RA,!(ID_study =="Chevinsky, 2021"))

# Modificar DF Vasculitis sumando los eventos de  Giant cell arteritis y Granulomatosis with polyangiitis del estudio de Tesch
Vasculitis <- subset(Vasculitis,!(Outcome_rep =="Arteritis temporalis"))
valores_Vasculitis <- list(Events_E = 94, Events_C = 49)
Vasculitis[2, c("Events_E", "Events_C")] <- valores_Vasculitis

# Modificar DF IBD sumando los eventos de  Crohn’s disease y Ulcerative colitis del estudio de Tesch
IBD <- subset(IBD,!(Outcome_rep =="Ulcerative colitis"))
valores_IBD <- list(Events_E = 665, Events_C = 517)
IBD[2, c("Events_E", "Events_C")] <- valores_IBD


##============================================================================
##=================Random Effects Model by Disease ===========================
##============================================================================
library(stringr)
get_meta_by_disease <- function (datd) {
  Meta_disease <- metabin(data = datd,
                          event.e = Events_E, n.e = Included_E, 
                          event.c = Events_C, n.c = Included_C,  
                          studlab= ID_study, sm="RR", 
                          method="MH",  common = FALSE, random = TRUE, 
                          label.e = "Exposed", label.c = "Non-Exposed")
  size_plot <- length(datd$ID_study)
  title_plot <- str_wrap(unique(datd$Outcome_ag), 20)
  print(title_plot)
  png(paste0("results/Meta-",title_plot, ".png"), width = 600*2, height = 110*size_plot, 
      units = "px", res = 100)  
  forest.meta(x = Meta_disease,  
              col.square = "grey60", 
              col.square.lines = "black",
              col.diamond = "steelblue",
              header.line = "both",
              fontsize = 15,
              xlim = c(0.2, 10),
              # label.right = "Risk Factor",
              # label.left =  "Protector Factor",
              smlab = title_plot)
  dev.off()
  
}


get_meta_by_disease(datd = Behcet)
get_meta_by_disease(datd = DMT1)
get_meta_by_disease(datd = DMT1PP)
get_meta_by_disease(datd = IBD)
get_meta_by_disease(datd = Guillain_Barré)
get_meta_by_disease(datd = Polymyalgia)
get_meta_by_disease(datd = Psoriasis)
get_meta_by_disease(datd = RA)
get_meta_by_disease(datd = SLE)
get_meta_by_disease(datd = Sjögrens)
get_meta_by_disease(datd = Spondyloarthritis)
get_meta_by_disease(datd = Systemic_scleroderma)
get_meta_by_disease(datd = Vasculitis)



##============================================================================
##=================Random Effects All Diseases ==============================
##============================================================================
##================ Unifying dataset of selected outcomes for metabin
udat <- rbind(Spondyloarthritis,
              DMT1,
              DMT1PP,
              IBD,
              SLE,
              Polymyalgia,
              Guillain_Barré,
              Behcet,
              Systemic_scleroderma,
              Psoriasis,
              Sjögrens,
              Vasculitis,
              RA) %>% arrange(Outcome_ag)

Meta_all <- metabin(Events_E, Included_E, Events_C, Included_C, data=udat,
                    studlab= ID_study, subgroup = Outcome_ag, sm="RR",
                    method="MH",  common = FALSE, random = TRUE,
                    label.e = "Exposed", label.c = "Non-Exposed",
                    print.subgroup.name = FALSE,
                    label.right = ""
)


png("results/Meta-All-Diseases.png", width = 600*2, height = 1000*2,
    units = "px", res = 100)  

forest.meta(Meta_all,
            col.square = "grey50",
            col.square.lines = "black",
            col.diamond = "steelblue",
            col.subgroup = "black",
            header.line = "both",
            # weight = "random",
            fs.heading = 16,
            fs.smlab = 16,
            fs.random = 14,
            ff.random  = "plain",
            fs.axis = 14,
            cex = 24,
            xlim = c(0.2, 10),
            label.right = "Risk Factor",
            label.left =  "Protector Factor",
            fs.lr = 14,
            print.I2.ci = TRUE,
            # layout = "RevMan5",
            test.subgroup.random = FALSE,
            overall=FALSE,
            overall.hetstat = FALSE
)
dev.off()


