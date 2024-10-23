setwd("C:/Users/TOURE/Mes documents/REPOSITORIES/IM_raw_data/ETH_R2/")

library(tidyverse)
library(dplyr)
library(sf)
library(flextable)
library(stringr)
library(stringi)
library(lubridate)
library(readxl)
library(ggplot2)

DF <- read_excel("AFRO_LQAS_template_ETH_Tigray_R2.xlsx")
DB <- DF |> 
  select(country, province, district , response , vaccine.type , roundNumber, numbercluster , start_date, end_date, male_sampled, female_sampled, total_sampled, male_vaccinated, female_vaccinated , total_vaccinated, total_missed, status , performance , r_Non_Compliance, r_House_not_visited, r_childabsent, r_Child_was_asleep, r_Child_is_a_visitor, r_Vaccinated_but_not_FM, other_r, percent_care_Giver_Informed_SIA) |>
  arrange(start_date) |> 
  mutate(district = case_when(
    district	=="Abiy Adi" ~	"Abi Adi Town",
    district	=="Adet" ~	"Naeder Adet",
    district	=="Adwa Town" ~	"Adwa Town",
    district	=="Adwa Zuria" ~	"Adwa",
    district	=="Ahiferom" ~	"Aheferom",
    district	=="Axum Town" ~	"Axum Town",
    district	=="Laelay Maichew" ~	"Laelay Maychew",
    district	=="Tahitay Maichew" ~	"Tahtay Mayechew",
    district	=="Tankua Milash" ~	"Tanqua Abergele",
    district	=="Adigrat" ~	"Adigrat Town",
    district	=="Atsbi" ~	"Atsbi Wenberta",
    district	=="Ganta Afeshum" ~	"Ganta Afeshum",
    district	=="Gulomekeda" ~	"Gulo Mekeda",
    district	=="Hawzen" ~	"Hawzen",
    district	=="Kilte Awlaelo" ~	"Kelete Awelallo",
    district	=="Tsaeda Emba" ~	"Saesie Tsaedamba",
    district	=="Wukro" ~	"Wukro Town",
    district	=="Adi Haki" ~	"Adhaki",
    district	=="Ayder" ~	"Ayder",
    district	=="Hawolti" ~	"Hawelti",
    district	=="Quiha" ~	"Kuha",
    district	=="Asgede" ~	"Tsegede (Tigray)",
    district	=="Seyemti Adiyabo" ~	"Laelay Adiabo",
    district	=="Shire Town" ~	"Sheraro Town",
    district	=="Tahitay Koraro" ~	"Tahtay Koraro",
    district	=="Tsimbla" ~	"Asgede Tsimbila",
    district	=="Degua Temben" ~	"Dega Temben",
    district	=="Enderta" ~	"Enderta",
    district	=="Samre" ~	"Saharti Samre",
    district	=="Endamekoni" ~	"Endamehoni",
    district	=="Maichew Town" ~	"Maychew Town",
    district	=="Raya Azebo" ~	"Raya Azebo",
    TRUE~ district))

ETH_dist<-read_sf("shapefiles/eth_admbnda_csa_bofed_20190827_shp/eth_admbnda_adm3_csa_bofed_20190827.shp")

LQAS  <- DB |> 
  select(country, province , district, response, roundNumber,performance) |>
  group_by(country, province, district, performance) |>
  summarise(count = n())
#join the spatial layer of provinces
LQAS_performance <- left_join(ETH_dist, LQAS, 
                              by=c("ADM3_EN" = "district")) |> 
  filter(performance != "NA")

plot1 <- ggplot() + 
  geom_sf(data = LQAS_performance, aes(fill = performance), color = NA) +
  geom_sf(data = ETH_dist, fill = NA, color = "black", size = 1)+
  scale_fill_manual(values = c("High" = "green4", "Very_poor" = "brown4"))+
  theme_void() +
  labs(fill = paste0("range"), 
       # title = "SYNCHONIZED CAMPAIGNE", 
       subtitle = " LQAS performance, last six months ") +
  theme(plot.title = element_text(hjust=0.5), 
        plot.subtitle = element_text(hjust=0.5))

plot1
  
  
  

write_csv(DB,"C:/Users/TOURE/Mes documents/REPOSITORIES/LQAS_raw_data/LQAS_level1/ETH1_LQAS.csv")