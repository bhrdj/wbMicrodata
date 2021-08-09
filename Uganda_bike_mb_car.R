# Analyzing Uganda microdata with nVennR

library(tidyverse)
library(readr)
library(nVennR)
library(magrittr)

options(tibble.print_max = 1000)
normalized_pop <- 1000

df_paths <- list(
  UGA_2005 = "./data/UGA_2005_2009_UNPS_v01_M_Stata8/2009_GSEC14.dta",
  UGA_2010 = "./data/UGA_2010_UNPS_v01_M_Stata8/GSEC14.dta",
  UGA_2011 = "./data/UGA_2011_UNPS_v01_M_Stata/GSEC14.dta",
  UGA_2013 = "./data/UGA_2013_UNPS_v01_M_STATA8/GSEC14A.dta",
  UGA_2015 = "./data/UGA_2015_UNPS_v01_M_STATA8/gsec14.dta",
  UGA_2018 = "./data/UGA_2018_UNPS_v01_M_STATA12/hh/HH/GSEC14.dta",
  UGA_2019 = "./data/UGA_2019_UNPS_v01_M_STATA14/HH/gsec14.dta"
)


# loop through the years
  # each year, make a tidy data frame of household observations
df_list <- list()
for (key in names(df_paths)) {
  df_list[[{{key}}]] <- df_paths[[key]] %>%
    read_dta() %T>%
    {colnames(.) <- tolower(colnames(.))} %T>%
    {colnames(.) <- sub("q0", "q", colnames(.))} %>%
    select(c("hhid", "h14q2", "h14q3")) %>%
    mutate(across(c("h14q2", "h14q3"), as.integer)) %>%
    mutate(have_one = h14q3 == 1 | h14q3 == 4 | h14q3 == 5) %>%
    select(-c(h14q3)) %>%
    pivot_wider(id_cols = hhid, names_from = h14q2, values_from = have_one) %>%
    select(c("hhid", `10`, `11`, `12`)) %>%
    rename(bike = `10`, motorbike = `11`, `car` = `12`) %>%
    mutate(country = (str_split(key, "_"))[[1]][[1]]) %>%
    mutate(year = (str_split(key, "_"))[[1]][[2]])
}

# loop through the ears
  # each year, make a plot with normalized scale
for (key in names(df_list)) {
  
  # Only pull the $def output from plotVenn, so I can rescale plot
  myV <- plotVenn(list(
    bike         = subset(df_list[[key]], bike)$hhid, 
    motorbike    = subset(df_list[[key]], motorbike)$hhid,
    car          = subset(df_list[[key]], car)$hhid #, 
    # no_transport = subset(df_list[[key]], !car & !motorbike & !bike)$hhid
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
                   setColors=c("#00F", "#0F0", "#F00"), #, "#000"),
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
