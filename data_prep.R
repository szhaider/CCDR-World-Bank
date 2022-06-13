#Utils
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(sf)
library(stringr)
library(glue)
library(janitor)
library(rio)
library(readxl)
library(devPTIpack)
library(stringr)

rm(list=ls())

#Data Dreparation

################################################################################
#Reading in Shapefiles
#District UNOCHA

district_shp <-  read_sf("data/shapefile_district/pakistan_indicators.shp") %>% 
  clean_names() %>% 
  filter(year == 2018) %>% 
  select(year, province, district, geometry) %>% 
  st_as_sf() %>% 
  mutate(district = str_remove(district, " Agency")) %>% 
  arrange(district) %>% 
  st_transform(crs='+proj=longlat +datum=WGS84') %>% #4326
  mutate(polygon = "district")


#Teshil Shape File

tehsil_shp <- read_sf("data/Tehsils_shp_UNOCHA/pak_admbnda_adm3_ocha_pco_gaul_20181218.shp")%>% 
  clean_names() %>% 
  select(province = adm1_en, district = adm2_en, tehsil = adm3_en, geometry) %>% 
  st_as_sf() %>% 
  st_transform(crs='+proj=longlat +datum=WGS84') %>% 
  mutate(polygon = "tehsil") %>% 
  arrange(tehsil)
################################################################################
#Reading Data
#Natural Hazard Data
natural_hazards <- rio::import_list("data/Pakistan_Natural Hazard Exposure and Impact_Districts Tehsils.xlsx")

hazards_district <- natural_hazards[["PAK_District_SUMMARY"]] %>% 
  as_tibble()%>% 
  rename(province = ADM1_NAME, district = ADM2_NAME) %>% 
  select(-ADM0_CODE, -ADM0_NAME, -ADM1_CODE, -ADM2_CODE) %>% 
  rename(dist_population = ADM2_pop) %>% 
  mutate(dist_population = dist_population/1000) %>% 
  pivot_longer(dist_population:`AP_pop_EAI%` ,names_to = "indicator", values_to = "value") %>% 
  mutate(polygon = "District") %>% 
  arrange(district) %>% 
  mutate(value = case_when(
    indicator = str_detect(indicator, "%") ~ value*100,
    TRUE ~ value
  ))

hazards_tehsil <- natural_hazards[["PAK_Tehsil_SUMMARY"]]%>% 
  as_tibble() %>% 
  select(-ADM0_CODE, -ADM1_CODE, -ADM2_CODE, -ADM3_CODE, -ADM0_NAME) %>% 
  rename(province = ADM1_NAME , district = ADM2_NAME, tehsil = ADM3_NAME) %>%
  rename(tehsil_poplation = ADM3_pop) %>% 
  mutate(tehsil_poplation = tehsil_poplation/1000) %>% 
  arrange(tehsil) 

#Matching Tehsil Names with UNOCHA
tehsil_1 <- tehsil_shp$tehsil

hazards_tehsil <- hazards_tehsil %>%
  bind_cols(tehsil1= tehsil_1) %>% 
  select(-tehsil) %>% 
  rename(tehsil = tehsil1)

#All Matched
# is.element(tehsil_shp$tehsil, hazards_tehsil$tehsil)
# # is.element(x$tehsil, hazards_tehsil$tehsil)
# is.element(hazards_tehsil$tehsil, tehsil_shp$tehsil)

hazards_tehsil <- hazards_tehsil %>% 
  pivot_longer(tehsil_poplation:`AP_pop_EAI%` ,names_to = "indicator", values_to = "value") %>% 
  mutate(polygon = "Tehsil") %>% 
  arrange(tehsil)%>% 
  mutate(value = case_when(
    indicator = str_detect(indicator, "%") ~ value*100,
    TRUE ~ value
  ))

hazards <- bind_rows(hazards_district, hazards_tehsil) %>% 
  mutate(domain = "Natural Hazards")

# %>% 
#   mutate(tehsil =
#            case_when(
#              tehsil == 'Ahmadupr E' ~ 'Ahmadupr East',
#              tehsil == 'Ahmedpur S' ~ 'Ahmedpur Sial',
#              tehsil == 'Ambar Utma' ~ 'Ambar Utman Khel',
#              tehsil == 'Bahawalnag' ~ 'Bahawalnagar',
#              tehsil == 'Banda Daud' ~ 'Banda Daud Shah',
#              tehsil == 'Bar Chamar' ~ 'Bar Chamarkand',
#              tehsil == 'Bulri Shah' ~ 'Bulri Shah Karim',
#              tehsil == 'Central Ku' ~ 'Central Kurram',
#              tehsil == 'Central Or' ~ 'Central Orakzai',
#              tehsil == 'Chak Jhumr' ~ 'Chak Jhumra',
#              tehsil == 'Chichawatn' ~ 'Chichawatni',
#              tehsil == 'Choa Saida' ~ 'Choa Saidan Shah',
#              tehsil == 'Darra Adam' ~ 'Darra Adam Khel',
#              tehsil == 'Dasht (Kec' ~ 'Dasht (Kech)',
#              tehsil == 'Dasht (Mas' ~ 'Dasht (Mastung)',
#              tehsil == 'De- Exclud' ~ 'De- Excluded Area Rajanpur',
#              tehsil == 'De-exclude' ~ 'De-excluded Area D.G.Khan',
#              tehsil == 'Dera Ghazi' ~ 'Dera Ghazi Khan',
#              tehsil == 'Dera Ismai' ~ 'Dera Ismail Khan',
#              tehsil == 'Dera Murad' ~ 'Dera Murad Jamali',
#              tehsil == 'Faisalabad' ~ 'Faisalabad City',
#              tehsil == 'Faisalabad' ~ 'Faisalabad Saddar',
#              tehsil == 'Garhi Khai' ~ 'Garhi Khairo',
# 
#              tehsil == 'Garhi Yasi' ~ 'Garhi Yasin',
#              tehsil == 'Ghulam Kha' ~ 'Ghulam Khan',
#              tehsil == 'Hassan Abd' ~ 'Hassan Abdal',
#              tehsil == 'Hassan Khe' ~ 'Hassan Khel',
#              tehsil == 'Hyderabad' ~  'Hyderabad City',
#              tehsil == 'Jalalpur P' ~ 'Jalalpur Pirwala',
#              tehsil == 'Jan Nawaz' ~ 'Jan Nawaz Ali',
#              tehsil == 'Kahror Pac' ~ 'Kahror Pacca',
#              tehsil == 'Kairpur Ta' ~ 'Kairpur Tamewali',
#              tehsil == 'Kakar Khur' ~ 'Kakar Khurasan',
#              tehsil == 'Kallar Kah' ~ 'Kallar Kahar',
#              tehsil == 'Kallar Say' ~ 'Kallar Sayedan',
#              tehsil == 'Kambar Ali' ~ 'Kambar Ali Khan',
#              tehsil == 'Kan Mehtar' ~ 'Kan Mehtarzai',
#              tehsil == 'Karachi Ce' ~ 'Karachi Central',
#              tehsil == 'Karachi Ea' ~ 'Karachi East',
#              tehsil == 'Karachi So' ~ 'Karachi South',
#              tehsil == 'Karachi We' ~ 'Karachi West',
#              tehsil == 'Karor Lal' ~ 'Karor Lal Esan',
#              tehsil == 'Keti Bande' ~ 'Keti Bander',
#              tehsil == 'Khairpur N' ~ 'Khairpur Nathan Shah',
#              tehsil == 'Khanpur (R' ~ 'Khanpur (Rahim Yar Khan)',
#              tehsil == 'Khanpur (S' ~ 'Khanpur (Shikarpur)',
#              tehsil == 'Khwazakhel' ~ 'Khwazakhela',
#              tehsil == 'Killa Abdu' ~ 'Killa Abdullah',
#              tehsil == 'Killa Saif' ~ 'Killa Saifullah',
#              tehsil == 'Kot Ghulam' ~ 'Kot Ghulam Muhammad',
#              tehsil == 'Kotli Satt' ~ 'Kotli Sattian',
#              tehsil == 'Lahore Can' ~ 'Lahore Cantt',
#              tehsil == 'Lahore Cit' ~ 'Lahore City',
#              tehsil == 'Lakki Marw' ~ 'Lakki Marwat',
#              tehsil == 'Landi Kota' ~ 'Landi Kotal',
#              tehsil == 'Lower Kurr' ~ 'Lower Kurram',
#              tehsil == 'Lower Orak' ~ 'Lower Orakzai',
#              tehsil == 'Mandi Baha' ~ 'Mandi Bahauddin',
#              tehsil == 'Matta Khar' ~ 'Matta Khararai',
#              tehsil == 'Matta Sebu' ~ 'Matta Sebujni',
#              tehsil == 'Mian Chann' ~ 'Mian Channu',
#              tehsil == 'Minchinaba' ~ 'Minchinabad',
#              tehsil == 'Mirpur Bat' ~ 'Mirpur Bathoro',
#              tehsil == 'Mirpur Kha' ~ 'Mirpur Khas',
#              tehsil == 'Mirpur Mat' ~ 'Mirpur Mathelo',
#              tehsil == 'Mirpur Sak' ~ 'Mirpur Sakro',
#              tehsil == 'Multan Cit' ~ 'Multan City',
#              tehsil == 'Multan Sad' ~ 'Multan Saddar',
#              tehsil == 'Muslim Bag' ~ 'Muslim Bagh',
#              tehsil == 'Muzaffarga' ~ 'Muzaffargarh',
#              tehsil == 'Nagarparka' ~ 'Nagarparkar',
#              tehsil == 'Nankana Sahib' ~ 'Nankana Sahib',
#              tehsil == 'Naushahro' ~ 'Naushahro Feroze',
#              tehsil == 'Nowshera V' ~ 'Nowshera Virkan',
#              tehsil == 'Pind Dadan' ~ 'Pind Dadan Khan',
#              tehsil == 'Pindi Bhat' ~ 'Pindi Bhattian',
#              tehsil == 'Pir Baba/' ~ 'Pir Baba/ Gadaizi',
#              tehsil == 'Qubo Saeed' ~ 'Qubo Saeed Khan',
#              tehsil == 'Quetta Cit' ~ 'Quetta City',
#              tehsil == 'Rahim Yar' ~ 'Rahim Yar Khan',
#              tehsil == 'Renala Khu' ~ 'Renala Khurd',
#              tehsil == 'Sahiwal (Sahiwal)' ~ 'Sahiwal (Sahiwal)',    #Sahiwal (Sardogha) == NA
#              tehsil == 'Salarzai Tehsil' ~ 'Salarzai T',
#              tehsil == 'Sam Raniza' ~ 'Sam Ranizai',
#              tehsil == 'Sangla Hil' ~ 'Sangla Hill',
#              tehsil == 'Sarai Alam' ~ 'Sarai Alamgir',
#              tehsil == 'Sarai Naur' ~ 'Sarai Naurang',
#              tehsil == 'Shah Bande' ~ 'Shah Bander',
#              tehsil == 'Shaheed Fa' ~ 'Shaheed Fazal Rahu',
#              tehsil == 'Sheikhupur' ~ 'Sheikhupura',
#              tehsil == 'Swat Raniz' ~ 'Swat Ranizai',
#              tehsil == 'Takht E Na' ~ 'Takht E Nasrati',
#              tehsil == 'Tandlianwa' ~ 'Tandlianwala',
#              tehsil == 'Tando Alla' ~ 'Tando Allah Yar',
#              tehsil == 'Tando Ghul' ~ 'Tando Ghulam Hyder',
#              tehsil == 'Tando Muha' ~ 'Tando Muhammad Khan',
#              tehsil == 'Thano Bula' ~ 'Thano Bula Khan',
#              tehsil == 'Toba Tek S' ~ 'Toba Tek Singh',
#              tehsil == 'Upper Kurr' ~ 'Upper Kurram',
#              tehsil == 'Upper Moma' ~ 'Upper Momand',
#              tehsil == 'Upper Orak' ~ 'Upper Orakzai',
#              tehsil == 'Usta Muham' ~ 'Usta Muhammad',
#              tehsil == 'Utman Khel' ~ 'Utman Khel Tehsil',
#              TRUE ~ tehsil
# 
#             )) %>%
#   arrange(tehsil)
           # pivot_longer(ADM3_pop:`AP_pop_EAI%`,names_to = "indicator", values_to = "value") %>% View()
           #  pivot_wider(names_from = tehsil, values_from = value) %>% 
           #  mutate(`Faisalabad Sadar` = NA,
           #         `Sahiwal (Sardogha)` = NA,
           #         `Hyderabad City`  = NA
           #         ) %>% 
           #  pivot_longer(Wazirabad:`Hyderabad City`, names_to = "tehsil", values_to = "value") %>% 
           #  View()
           #  


legend <- natural_hazards[["Legend"]] %>% 
  as_tibble() 

# View(natural_hazards)

#Socio-economic Indicator

#function to multiply by 100
multiply100 <- function(col){
  col * 100
}

development_indicators <- readxl::read_excel("data/pak_sub_ADM2_handover.xlsx", na="NA") %>% 
  rename(district = ADM2_EN) %>% 
  filter(district != "Karachi Central",       #Karachi City is available, we dont have these in UNOCHA 
         district != "Karachi East",
         district != "Karachi Malir",
         district != "Karachi South",
         district != "Karachi West") %>% 
  mutate(`Population (WorldPop 2020)` = `Population (WorldPop 2020)`/100000,
         `Population Density (WorldPop 2020)` = `Population Density (WorldPop 2020)`/100,
         `Health Facility Density Per 100,000 inhabitants(Alhasan 2017)`= `Health Facility Density Per 100,000 inhabitants(Alhasan 2017)` /100,
         `High Health Facility Density (Alhasan 2017)`=`High Health Facility Density (Alhasan 2017)`/100,
         `Low Health Facility Density (Alhasan 2017)`= `Low Health Facility Density (Alhasan 2017)`/100,
         `Road Density`= `Road Density`/100,
         `Road density normalised`= `Road density normalised`/100,
         `Access to improved toilet facilities (PSLM 2014)`= `Access to improved toilet facilities (PSLM 2014)`/100,
         `Access to improved toilet facilities (PSLM 2019)` = `Access to improved toilet facilities (PSLM 2019)`/100,
         `Access to improved toilet facilities (PSLM 2014/2019)`= `Access to improved toilet facilities (PSLM 2014/2019)`/100,
         ) %>% 
  mutate_if(is.numeric, multiply100) %>%
  pivot_longer(`Population (WorldPop 2020)`:`Lack of access to improved toilet facilities (PSLM 2014/2019)`,
               names_to = "indicator", 
               values_to = "value") %>% 
  mutate(polygon = "District")
# View(development_indicators)
#since province is missing in development indicators, bringing in province
prov <- district_shp %>% 
  as_tibble() %>% 
  select(province, district)

development_indicators <- development_indicators %>% 
  left_join(prov, by="district") %>% 
  mutate(domain = "Development Outcomes") %>% 
  arrange(district)


#Making a List frame
# data <- list(hazards = hazards, development=development_indicators)

#Combining all data
data <- hazards %>% 
  bind_rows(development_indicators) %>% 
  mutate(value = 
           case_when(
          domain == "Development Outcomes" ~ round(value, 2),
          domain == "Natural Hazards" ~ round(value, 2)
          )
  )

#For color mapping on Maps (to reverse for selected indicators)
# data %>% 
#   distinct(indicator) %>% 
#   rio::export("legend.xlsx")

#Reading in Color mapping file for data Join
legend_data <- readxl::read_excel("legend_data.xlsx", na= " ") %>% 
  mutate(unit = replace_na(unit, ""))

 

data %>%
  left_join(legend_data, by="indicator") %>% 
  mutate(value = 
           case_when(
             indicator_1 == "Population (WorldPop 2020)" ~ round(value, 0),
             indicator_1 == "Tehsil Population" ~ round(value, 0),
             indicator_1 == "District Population" ~ round(value, 0),
             indicator_1 == "Expected mortality from river floods (population count)" ~ round(value, 0),
             indicator_1 == "Expected mortality from coastal floods (population count)" ~ round(value, 0),
             indicator_1 == "Population exposed to medium or high landslide hazard (population count)" ~ round(value, 0),
             indicator_1 == "Expected exposure to heat stress (population count)" ~ round(value, 0),
             indicator_1 == "Expected increase of mortality from air pollution (population count)" ~ round(value, 0),
             TRUE ~ value
           )) %>% 
  write_rds("CCDR_Dashboard/data/data.RDS")


# View(data)


#Natual Hazards all matched
# is.element(district_shp$district, hazards_district$district)
# is.element(hazards_district$district, district_shp$district)
# 
# #Development indicators all matched
# is.element(district_shp$district, development_indicators$district)
# is.element(development_indicators$district, district_shp$district)

# x<- district_shp %>% as_tibble() %>% distinct(district)
# y <- hazards_district %>% distinct(district)


# x <- tehsil_shp %>% as_tibble() %>% select(tehsil) 
# %>% 
#   filter(tehsil != "Faisalabad Saddar",
#          tehsil != "Hyderabad City",
#          tehsil != "Sahiwal (Sardogha)")
# y <- hazards_tehsil %>% select(tehsil) 

pak_shp <- list(District = district_shp, Tehsil = tehsil_shp)
pak_shp %>% write_rds("CCDR_Dashboard/data/pak_shp.RDS")

################################################################################
###Metadata for PTI
metadata_climate <- import_list("data/pak_metadata_climate.xlsx")

# devPTIpack::validate_metadata(metadata_climate)

metadata_climate %>% 
  write_rds("CCDR_Dashboard/data/pak_metadata_climate.RDS")
################################################################################
#PTI Shapefile preparation

nums1 <- seq(1:9)
nums1 <- paste0(0, nums1)

nums2 <- as.character(seq(10,131)) 

#Concatinating together
nums <- c(nums1, nums2)

pak_shp_district_PTI <- st_read("data/shapefile_district/pakistan_indicators.shp") %>% 
  st_as_sf() %>% 
  clean_names() %>% 
  select(year, province, district, geometry) %>% 
  filter(year==2018) %>% 
  arrange(district) %>% 
  rename(admin1Name = district)  %>% 
  mutate(admin1Pcod = paste0("Pakistan", nums)) %>% 
  mutate(admin0Pcod = "Pakistan") %>% 
  select(admin0Pcod, admin1Pcod, admin1Name, geometry)

#Making a List
pak_shp <- list(admin1_District = pak_shp_district_PTI)

pak_shp <- 
  pak_shp %>% 
  map(~{.x %>% st_transform("+proj=longlat +datum=WGS84")})

## https://stackoverflow.com/questions/68478179/how-to-resolve-spherical-geometry-failures-when-joining-spatial-data
sf_use_s2(FALSE)

pak_shp$admin0_Country <-
  pak_shp$admin1_District %>% 
  group_by(admin0Pcod) %>% 
  summarise() %>% 
  mutate(admin0Name = "Pakistan") %>% 
  st_make_valid()


pti_shps <- NULL
pti_shps$admin0_Country  <- pak_shp$admin0_Country 
pti_shps$admin1_District <- pak_shp$admin1_District


pti_shps %>% write_rds("CCDR_Dashboard/data/pak_geometries.rds", compress = "gz")

pti_shps$admin0_Country %>% st_crs()
pti_shps$admin1_District %>% st_crs()

validate_geometries(pti_shps)
################################################################################

