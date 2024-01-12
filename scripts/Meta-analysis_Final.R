
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
datg <- subset(datg,!(ID_study =="Chevinsky, 2021")) # Chevinsky is removed from the analysis as results for rheumatoid arthritis evaluation were not reported
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
# Thirteen outcomes were selected. Evidence was found for two nervous system diseases, but both conditions were addressed in the same study; 
# therefore, they were not statistically combined. The same situation occurred with Autoimmune Thyroid Disease.
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

# Modifying the Guillain-Barré DataFrame by combining events from both periods of the Mizrahi study
# 1. Removing one Mizrahi record to retain one entry per study 
Guillain_Barré_A <- subset(Guillain_Barré,!(Comparisons =="Covid Vs No covid late phase (180-360 days)"))
# 2. Changing the data related to events in each group by aggregating events from both periods of the Mizrahi study
Guillain_Barré_A[1, "Events_E"] <- 18 
Guillain_Barré_A[1, "Events_C"] <- 14

# Aquí no entiendo la diferencia entre SGB1 y Guillain_Barré_A ------ ZULMA
#SGB1 combina los dos estudios que evaluaron guillan barré, incluyendo unicamente el segiuimiento de 30-180 dias del estudio de Mizrahi
#Dado que se decidió sumar los eventos de ambos periodos del estudio de Mizhari, este análisis no aplica y se pude omitir 

# Excluding Chevinsky, 2021 as  # of events and N is missing
RA <- subset(RA,!(ID_study =="Chevinsky, 2021"))

# Modifying the Vasculitis DataFrame by aggregating events of Giant Cell Arteritis and Granulomatosis with Polyangiitis from the Tesch study
Vasculitis <- subset(Vasculitis,!(Outcome_rep =="Arteritis temporalis"))
valores_Vasculitis <- list(Events_E = 94, Events_C = 49)
Vasculitis[2, c("Events_E", "Events_C")] <- valores_Vasculitis

# Modifying the Inflammatory Bowel Disease (IBD) DataFrame by aggregating events of Crohn’s disease and Ulcerative colitis from the Tesch study.
IBD <- subset(IBD,!(Outcome_rep =="Ulcerative colitis"))
valores_IBD <- list(Events_E = 665, Events_C = 517)
IBD[2, c("Events_E", "Events_C")] <- valores_IBD



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

Meta_all <- metabin(event.e = Events_E, n.e = Included_E, 
                    event.c = Events_C, n.c = Included_C, data=udat,
                    studlab= ID_study, subgroup = Outcome_ag, sm="RR",
                    method="MH",  common = FALSE, random = TRUE,
                    label.e = "Exposed", label.c = "Non-Exposed", 
                    print.subgroup.name = FALSE,
                    label.right = ""
)


png("results/Meta-All-Diseases.png", width = 600*8, height = 1000*6,
    units = "px", res = 300)  

forest.meta(Meta_all,
            layout = "meta",
            col.study = "darkred",
            col.inside = "darkred",
            col.square = "#fccde5",
            col.square.lines = "black",
            col.diamond = "steelblue",
            col.subgroup = "black",
            header.line = "both",
            lwd = 3,
            fs.heading = 16,
            fs.smlab = 16,
            fs.random = 14,
            ff.random  = "plain",
            fs.axis = 14,
            cex = 24,
            xlim = c(0.1, 10),
            label.right = "Risk Factor",
            label.left =  "Protector Factor",
            fs.lr = 14,
            print.I2.ci = TRUE,
            test.subgroup.random = FALSE,
            overall=FALSE,
            overall.hetstat = FALSE,
            colgap.left = "1.5 cm",
            colgap.right = "0.5 cm",
            colgap.forest.left = "0.5 cm",
            colgap.forest.right =  "0.5 cm",
            just = "right"
)
dev.off()



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
  if (size_plot == 3) size_plot <- 2.5
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


library(magick)
p1 <- image_read("results/Meta-Behcet’s disease.png")
p2 <- image_read("results/Meta-Guillain-Barré
syndrome.png")
p3<- image_read("results/Meta-Inflammatory bowel
disease.png")
p4 <- image_read("results/Meta-Polymyalgia
rheumatica.png")
p5 <- image_read("results/Meta-Psoriasis.png")
p6 <- image_read("results/Meta-Rheumatoid arthritis.png")
p7 <- image_read("results/Meta-Sjögren's syndrome.png")
p8 <- image_read("results/Meta-Spondyloarthritis.png")
p9 <- image_read("results/Meta-Systemic lupus
erythematosus.png")
p10 <- image_read("results/Meta-Systemic sclerosis.png")
p11 <- image_read("results/Meta-Type 1 diabetes
mellitus (Pediatric).png")
p12 <- image_read("results/Meta-Type 1 diabetes
mellitus.png")
p13 <- image_read("results/Meta-Vasculitis.png")

combined_imageA <- image_append(c(p1, p2, p3, p4, p5, p6, p7), stack = TRUE)
combined_imageB <- image_append(c(p8, p9, p10, p11, p12, p13), stack = TRUE)
image_write(combined_imageA, "results/Meta-A.png", density = 1)
image_write(combined_imageB, "results/Meta-B.png", density = 1)

pA <- image_read("results/Meta-A.png")
pB <- image_read("results/Meta-B.png")
combined_AB <- image_append(c(pA, pB), stack = FALSE)
image_write(combined_AB, "results/Meta-AB.png", density = 1)




