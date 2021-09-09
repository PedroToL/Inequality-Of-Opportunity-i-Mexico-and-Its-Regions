setwd("D:/Investigación/JDS-Corrections") 

# Libraries & Functions----
library(ggparty)
library(mable)
library(lognorm)
library(haven)
library(mlt)
library(DescTools)
library(dineq)
library(tidyverse)

source("./Code/Auxiliar-Functions.R")

# Data ----
df <- read_dta("./Data/Emovi_2017.dta")

df <- df %>% transmute(
  ID = folio, 
  wealth_origin      = as.numeric(pcl_n_otcm_ori), 
  wealth_destination = as.numeric(pcl_n_otcm_dest),
  region_origin      = recode_factor(as.double(region14),
                                     `1` = "N",
                                     `2` = "NO",
                                     `3` = "CO",
                                     `4` = "C",
                                     `5` = "CDMX",
                                     `6` = "S"),
  parents_educ       = as.double(anesc_padres), 
  area               = recode_factor(rural,
                                     `1` = "R",
                                     `0` = "U"),
  sex                = recode_factor(as.double(sexo),
                                     `2` = "W",
                                     `1` = "M"),
  skin_tone          = recode_factor(as.character(p151), 
                                     "1" = "D",
                                     "2" = "D", 
                                     "3" = "D",
                                     "4" = "D", 
                                     "5" = "D",
                                     "6" = "D",
                                     "7" = "B",
                                     "8" = "LB",
                                     "9" = "W",
                                     "10" = "W",
                                     "11" = "W"),
  ind_leng_parents   = recode_factor(as.double(hli), 
                                     `1` = "Y",
                                     `0` = "N")
) %>% na.omit()



# Estimation of Types ----
trees <- c("2017", "R", "U", "N", "NO", "CO", "C", "CDMX", "S")

for (i in trees){
  MSE <- NULL
  print(i)
  if (i == "2017"){
    df_ <- df 
  }
  else if (i == "R" | i == "U") {
    df_ <- df %>% filter(area == i)
  }
  else {
    df_ <- df %>% filter(region_origin == i)
  }
  
  for (n in 1:100) {
    set.seed(123)
    tree             <- ctree(wealth_destination~.-ID,
                              control = ctree_control(maxdepth = n),
                              data = df_)
    
    df_$predicted    <- predict(tree,
                                data = df_, 
                                type = "response")
    MSE[n] <- MLmetrics::MSE(df_$predicted, df_$wealth_destination)
  }
  plot(MSE, main = i, xlab = "Max Depth", ylab = "MSE")
  print(which(MSE == min(MSE))[1])
}
