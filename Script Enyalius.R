## SETTING A WORKING DIRECTORY
setwd("C:/Users/yonin/Desktop/Trabajo/PUBLICACIONES/- 51 MICROHABITAT ENYALIUS") ##Troque aqui pela sua pasta para rodar a análise no seu computador

## PACKAGES (se não tem instalados eles, lembre instalar com install.packages("nomedopacote"), depois com library(nomedopacote) você carrega eles na sessão de trabalho aqui).
library(ggplot2) # plots
library(readxl) # read the data set from Excel
library(writexl) # write excel file from data frame or tibble object
library(readr) # to read data sets into R
library(dplyr)  # data wrangling
library(tidyr) # data wrangling
library(nlme) # GLMM
library(lme4) # GLMM
library(survival) # Conditional Logistic Regression
library(TwoStepCLogit) # Two Step Conditional Logistic Regression
library(forcats) # to manipulate categorical variables
library(knitr) # to present data, as in R Markdown, prepare tables etc.

## LOADING THE DATA
######################################################
data <- read_excel("C:/Users/yonin/Desktop/Trabajo/PUBLICACIONES/- 51 MICROHABITAT ENYALIUS/Projeto_Enyalius_RPPN_jan28.xlsx",
                   col_types = c("text", "text", "date", "date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric",
                                 "text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "text", "text"))
head(data)
str(data)

data1 <- data %>%
  mutate(ID = as.factor(Indivíduo),
         date = Data,
         hour = Hora,
         presence = as.factor(Presença),
         Ts = Temperatura_Substrato,
         Ts_side = Temperatura_lado,
         Ta = Temperatura_do_Ambiente,
         Tb = Temperatura_superfície_ventral_do_indivíduo,
         RH = Umidade,
         height = `Altura_(cm)`,
         luminosity = Luminosidade,
         branch_diameter = branch_circumference/pi,
         DBH = CBH/pi,
         wind = Vento,
         microhabitat = as.factor(Microhabitat),
         color_substrate = Cor_do_Substrato,
         color_animal = Cor_do_Indivíduo,
         mass = Peso,
         SVL = CRC,
         sex = as.factor(Sexo),
         age = as.factor(Idade),
         ID = as.factor(Id._Foto)) %>%
  dplyr::select(ID, date, hour, presence, Ts, Ts_side, Ta, Tb, RH, height, luminosity, wind, microhabitat,
                branch_diameter, DBH, color_substrate, color_animal, mass, SVL, sex, age, ID)
str(data1)


## EXPLORING THE THERMAL ECOLOGY OF ENYALIUS
data_summary <- data1 %>%
  group_by(hour, presence) %>%
  mutate(mean_Ts = mean(Ts))

ggplot(data = data_summary, aes(x = hour, y = mean_Ts, linetype = presence)) +
  geom_line(fun = "mean", lwd = 1.5, alpha = 0.5) +
  #stat_summary(geom = "point", fun = "mean") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"))

data_Tb <- data1 %>%
  filter(presence == 1)

ggplot(data = data_Tb, aes(x = hour, y = Tb)) +
  stat_summary(geom = "point", fun = "mean") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"))

# Linear relationship Ta-Tb
model1 <- lm(Tb ~ Ta, data = data_Tb)
summary(model1)
plot(model1)
ggplot(data = data_Tb, aes(x = Ta, y = Tb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"))


ggplot(data = data_Tb, aes(x = Ts, y = Tb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"))

# Characterization microhabitat use and availability
data2 <- data1 %>%
  group_by(presence) %>%
  summarise(mean_Ts = mean(Ts, na.rm = T),
         sd_Ts = sd(Ts, na.rm = T),
         n_Ts = length(Ts),
         mean_Ta = mean(Ta, na.rm = T),
         sd_Ta = sd(Ta, na.rm = T),
         n_Ta = length(Ta),
         mean_RH = mean(RH, na.rm = T),
         sd_RH = sd(RH, na.rm = T),
         n_RH = length(RH),
         mean_height = mean(height, na.rm = T),
         sd_height = sd(height, na.rm = T),
         n_height = length(height),
         mean_luminosity = mean(luminosity, na.rm = T),
         sd_luminosity = sd(luminosity, na.rm = T),
         n_luminosity = length(luminosity),
         mean_branch_diameter = mean(branch_diameter, na.rm = T),
         sd_branch_diameter = sd(branch_diameter, na.rm = T),
         n_branch_diameter = length(branch_diameter),
         mean_DBH = mean(DBH, na.rm = T),
         sd_DBH = sd(DBH, na.rm = T),
         n_DBH = length(DBH),
         mean_wind = mean(wind, na.rm = T),
         sd_wind = sd(wind, na.rm = T),
         n_wind = length(wind))
data2_long <- data2 %>%
  pivot_longer(cols = mean_Ts:n_wind, names_to = "names", values_to = "value") %>%
  separate(names, into = c("statistic", "variable", "variable2")) %>%
  mutate(variable = paste(variable, variable2)) %>%
  dplyr::select(-variable2) %>%
  pivot_wider(names_from = statistic, values_from = value) %>%
  dplyr::select("variable", "presence", "mean", "sd", "n")

microhab_use <- arrange (data2_long, variable, presence)

write_xlsx(microhab_use, "Summary use vs. availability.xlsx")
table_use_availability <- kable(microhab_use, digits = 2)
table_use_availability

# Microhabitat selection
fct_count(data1$microhabitat)
data3 <- data1 %>%
  mutate(microhabitat = fct_recode(microhabitat, bush = "arbusto", bush =  "arbusto_(Piper_sp.)", branch = "bambuzinho",
                                   land = "barranco_(solo)", branch = "bambu_seco", land = "barranco", grass = "capim",
                                   branch = "caule_de_arbusto", liana = "cipó", liana = "cipó_suspenso", leaf = "folha_de_arbusto",
                                   leaf = "Folha_de_Piper_sp.", branch = "caule_de_Piper_sp.", leaf = "folha_de_samambaia_seca", 
                                   leaf = "folha_samambaia", liana = "cipó_seco", liana = "cipó_verde", grass = "folha_de_capim",
                                   leaflitter = "casca_de_árvore_no_chão", branch = "galho_arvore_fina", branch = "galho_caído", 
                                   branch = "galho_com_epíftas", branch = "galho_com_folhas", leaf = "folha_de_samambaia", leaf = "folha_de_trepadeira",
                                   branch = "galho_de_arbusto", branch = "galho_de_árvore", branch = "galho_de_árvore_caída",
                                   branch = "galho_de_árvore_pequena", branch = "galho_de_muda_de_cacaueiro_(Theobroma_cacao)",
                                   branch = "galho_de_Piper_sp.", branch = "galho_na_copa_de_árvore", branch = "galho_Piper_sp.", 
                                   branch = "galho_podre_no_solo", branch = "galho_seco", branch = "galho_seco_caído",
                                   leaf = "haste_de_folha", leaf = "haste_de_folha_de_samambaia", rock = "pedra", branch = "galho",
                                   branch = "pseudocaulde_de_bananeira_jovem", branch = "pseudocaule_de_bananeira", log = "raíz_exposta_em_barranco",
                                   leaflitter = "samambaia_seca", leaflitter =  "serrapilheira", leaflitter = "sobre_folha_de_Embauba_no_solo",
                                   land = "solo", land = "solo_(limpo)", tree_trunk = "tronco", tree_trunk = "tronco_arvore_fina",
                                   tree_trunk = "tronco_com_espinhos", tree_trunk = "tronco_de_arvore", log = "tronco_de_árvore_caída",
                                   tree_trunk = "tronco_de_árvore_pequena", log = "tronco_de_árvore_seca", tree_trunk = "tronco_Piper_sp._",
                                   log = "tronco_podre_caído", grass = "vegetação_rasteira"))
  
levels(data2$microhabitat)
ggplot(data = data3) +
  geom_bar(aes(presence, fill = microhabitat, position = "fill")) +
  labs(x = "Presence", y = "Proportion") +
  theme(panel.background = element_blank(),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.line = element_line(colour = "black"))

#model_Ts <- clogit(presence ~ Ts + wind + luminosity + height + RH + strata(ID), data = data3)
#model_Ts <- clogit(presence ~ RH + strata(ID), data = data3) #clogit not working because of different number at each stratum
model_Ts2 <- glmer(presence ~ scale(Ts) + scale(height) + scale(branch_diameter) + scale(RH) + scale(luminosity) + scale(wind) + (1|ID), family = binomial, data = data3)
summary(model_Ts2)
