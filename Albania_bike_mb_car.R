# Analyzing Albania microdata with nVennR

# later: include a limited gapminder clone via plotly with just Albania and Uganda
# also, embedded scenes from Dollar Street: 25% median 75% quintile-income households side-by-side
# https://www.kaggle.com/jhossain/explore-the-gapminder-dataset-with-plotly-express

library(tidyverse)
library(readr)
library(nVennR)
library(magrittr)
options(tibble.print_max = 1000)

normalized_pop <- 1000

# 2012
# Modul_13C1_Household_Durables
# m13c1_q1b: codes. 121 bicycle, 122 motorcycle, 123 car
# m13c1_q1c: integer number of items.



df_paths <- list(
  ALB_2012 = "./data/ALB_2012_from-gov-website_LSMS 2012_eng/Data_LSMS 2012/Modul_13C1_Household_Durables.sav"
)
df_list <- list()

# loop through the years
  # each year, make a tidy data frame of household observations
for (key in names(df_paths)) {
  df_list[[{{key}}]] <- df_paths[[key]] %>%
    read_sav() %>%
    select(-"m13c1_q1a") %>%
    mutate_at(c("psu", "hh"), as.character) %>%
    mutate(hhid = paste(psu, "_", hh, sep=""), hh=NULL, psu=NULL) %>%
    mutate_at(vars(-("hhid")), as.integer) %>%
    mutate(have_one = (m13c1_q1c >= 1), m13c1_q1c=NULL) %>%
    pivot_wider(id_cols = c("hhid"), names_from = m13c1_q1b, values_from = have_one) %>%
    select(c("hhid", `121`, `122`, `123`)) %>%
    rename(bike = `121`, motorbike = `122`, `car` = `123`) %>%
    print()
}

# loop through the ears
  # each year, make a plot with normalized scale
for (key in names(df_list)) {
  
  # Only pull the $def output from plotVenn, so I can rescale plot
  myV <- plotVenn(list(
    bike         = subset(df_list[[key]], bike)$hhid, 
    motorbike    = subset(df_list[[key]], motorbike)$hhid,
    car          = subset(df_list[[key]], car)$hhid, # COMMENT-OUT COMMA TO REMOVE NO_TRANSPORT
    no_transport = subset(df_list[[key]], !car & !motorbike & !bike)$hhid # COMMENT-OUT LINE TO REMOVE NO_TRANSPORT
  ), nCycles = 3000)$def
  
  nSets <- as.numeric(myV[2])
  a <- 3 + nSets
  b <- length(myV)
  n_hh <- length(df_list[[key]]$hhid)
  sNames <- myV[3:(a-1)]
  sSizes <- as.numeric(myV[a:b])
  sSizesNorm <- round(sSizes * 1000/n_hh)
            
  
  myV3 <- createVennObj(nSets=nSets, sNames=sNames, sSizes=sSizesNorm)
  myV3 <- plotVenn(nVennObj = myV3, nCycles = 5000,
                   fontScale = .8,
                   setColors=c("#00F", "#0F0", "#F00", "#000"), # COMMENT-OUT FOURTH COLOR TO REMOVE NO_TRANSPORT
                   outFile=paste("./svg/", {{key}}, ".svg", sep=""))
}



# myV3 <- createVennObj(nSets = 3, sSizes = c(10,0,0,2,0,2,2,5))
# myV3 <- plotVenn(nVennObj = myV3, nCycles = 3000)
# 
# venn_colors <- c("#00F", "#0F0", "#F00")
# showSVG(myV3, opacity=0.3, outFile="here.svg", systemShow=TRUE, setColors=venn_colors)

# inner_join(codes_h14q02, by=c(h14q02 = "Code")) %>%
# inner_join(codes_h14q03, by=c(h14q03 = "Code")) %>%
  

# UGA_2005_which_asset = "./data/UGA_2005_metadata/codes_h14q02.txt"
# UGA_2005_have_or_not = "./data/UGA_2005_metadata/codes_h14q03.txt"
# UGA_2019_which_asset = "./data/UGA_2019_metadata/codes_h14q02.txt"
# UGA_2019_have_or_not = "./data/UGA_2019_metadata/codes_h14q03.txt"



# codes_h14q02 <- read_delim(paste(path_h14, "codes_h14q02.txt", sep=""), delim="") %>%
#   mutate(Category = str_trim(Asset)) %>%
#   mutate(Value = as.integer(Code))
# 
# codes_h14q03 <- read_delim(paste(path_h14, "codes_h14q03.txt", sep=""), delim="") %>%
#   mutate(Category = str_trim(Question)) %>%
#   mutate(Value = as.integer(Code))
# 
# transport <- hh_have_one %>%
#   select("hhid", "Bicycle", "Motor cycle", "Motor vehicle")

#  showSVG(myV, opacity=0.2, systemShow=TRUE, 
#    setColors=c("#00F", "#0F0", "#F00", "#000"),
#    outFile=paste("./svg/", {{key}}, ".svg", sep="")
#)
