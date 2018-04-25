set.seed(1)
rm(list = ls())
library(tidyverse)
library(ggfortify)
library(gridExtra)
library(car)
library(nlme)
# library(forecast)

#### Read in data ----
ev<-read_delim("./Endenergieverbrauch_vs_Energietraeger_in_TJ.csv", delim = ";") 
ev[,-1] <- ev[,-1]/3600 # Transform in TWh/a
names(ev) <- c("JHR","PET","OEL","TRBS","ELK","GAS","KHL", "HLZ","FWR","KVA","ERN")
comment(ev) <- c("Alle Energien in TWh/a", 
                 "Primaerenergietraeger = PET", 
                 "Erdoelbrennstoffe = OEL", 
                 "Treibstoffe = TRBS", 
                 "Elektrizität = ELK", 
                 "Gas = GAS", 
                 "Kohle = KHL", 
                 "Holz.und.Holzkohle = HLZ", 
                 "Fernwärme = FWR", 
                 "Abfaelle = KVA", 
                 "Uebrige_erneuerbare_Energien = ERN")
# ev <- ev %>% as_tibble()
# ev %>% glimpse()

bip <- read_delim("./BIP_Schweiz_lange_Liste_1948_2016.csv", delim = ";")
bip$BIP <- bip$BIP/1000 # Transform in Mrd.CHF p.a.
names(bip) <- c("JHR", "BIP"); comment(bip) <- "BIP in Mrd. CHF p.a."
# bip <- bip %>% as_tibble()
# bip %>% glimpse()

hgt <- read_csv("./energierelevante_faktoren.csv") %>% .[,c(1,2)]
names(hgt) <- c("JHR", "HGT"); comment(hgt) <- c("Anzahl Heizgradtage seit 1980.")

chco2 <- read_delim("./CO2-Statistik-2017-07_D.csv", delim =";") %>% 
  select_if(~sum(!is.na(.)) > 0)
names(chco2) <- c("JHR", "CTRS", "CBRN")
comment(chco2) <- c("CO2 in Mt p.a",
                    "CO2 durch Treibstoffe total = CTRS",
                    "CO2 durch Brennstoffe total = CBRN")

dat <- inner_join(bip, ev, by = "JHR") %>% 
  left_join(hgt, by = "JHR") %>% 
  left_join(chco2, by = "JHR") %>% 
  replace_na(list(HGT = mean(.$HGT, na.rm= TRUE))) %>% 
  mutate(FWK = FWR / (HGT/mean(.$HGT, na.rm= TRUE))) %>% 
  .[,c("JHR", "BIP", "HGT", "PET", "ELK", "TRBS", "OEL", "GAS", "KHL", "HLZ", "KVA", "FWR", "FWK", "ERN", "CBRN", "CTRS")]
comment(dat) <- c("Jahre 1948 bis 2016",
                 "Alle Energien in TWh/a", 
                 "BIP in Mrd. CHF.",
                 "CO2 in Mt p.a",
                 "Primaerenergietraeger = PET", 
                 "Erdoelbrennstoffe = OEL", 
                 "Treibstoffe = TRBS", 
                 "Elektrizität = ELK", 
                 "Gas = GAS", 
                 "Kohle = KHL", 
                 "Holz.und.Holzkohle = HLZ", 
                 "Fernwärme = FWR", 
                 "Abfaelle = KVA", 
                 "Uebrige_erneuerbare_Energien = ERN",
                 "Heizgradtage = HGT",
                 "Fernwärme HGT-korrigiert = FWK",
                 "CO2 durch Treibstoffe total = CTRS",
                 "CO2 durch Brennstoffe total = CBRN") 
rm(list = c("bip", "ev", "hgt", "chco2"))
#### END - Read in Data

#### ----
summarise(dat, FWR = mean(FWR, na.rm = TRUE))
mean(dat$FWR, na.rm = TRUE)

