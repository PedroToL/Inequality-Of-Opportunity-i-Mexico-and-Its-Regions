setwd("D:/Investigación/JDS-Corrections")
# Libraries & Functions----
library(tidyverse)
library(ggparty)
library(mable)
library(haven)
library(mlt)
library(dineq)
library(statar)

defaultW <- getOption("warn")
options(warn = -1)

source("./Code/Auxiliar-Functions.R")
# Data ----
df <- read_dta("./Data/Emovi_2017.dta")

df <- df %>% transmute(
  ID = 1:nrow(df), 
  wealth_origin      = as.numeric(percentil_or), 
  wealth_destination = as.numeric(percentil_des),
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
  sex                = recode_factor(as.double(sexo_inf),
                                     `2` = "W",
                                     `1` = "M"),
  skin_tone          = recode_factor(as.character(color_p), 
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
)

variables <- colnames(df)

# Trees ----
trees <- c(.8, .5, 2148)
regions <- c("2017", "R", "U", "N", "NO", "CO", "C", "CDMX", "S")
depth <- c(9, 6, 6, 4, 5, 7, 7, 5, 6) 

j = 0
for (i in regions) {
  j = j + 1
  depth_i = depth[j]
  
  print(i)
  
  if (i == "2017"){
    dfi <- df

  }
  else if (i == "R" | i == "U") {
    dfi <- df %>% filter(area == i)
  }
  else {
    dfi <- df %>% filter(region_origin == i)
  }
  
  for (a in trees){
    print(a)
    
    nodos   <- NULL
    nodo_1  <- NULL
    
    Gini_outcome <- NULL
    MLD_outcome  <- NULL
    
    
    G_XWT <- NULL
    G_XBT <- NULL
    M_XWT <- NULL
    M_XBT <- NULL

    G_XWE <- NULL
    G_XBE <- NULL
    M_XWE <- NULL
    M_XBE <- NULL
    
    R2    <- NULL
    
    set.seed(123)
    for (n in 1:100) {
      print(n)
      if (a <= 1) {
        df_ <- dfi %>% sample_n(nrow(dfi)*a, replace = T)
      } else {
        df_ <- dfi %>% sample_n(a, replace = T)
      }
      
      # Estimation of Types
      tree             <- ctree(wealth_destination~.-ID,
                                control = ctree_control(maxdepth = depth_i),
                                data = df_)
      df_$types        <- predict(tree,
                                  data = df_, 
                                  type = "node")
      df_$predicted    <- predict(tree,
                                  data = df_, 
                                  type = "response")
      types            <- sort(unique(df_$types))
      nodos[n]         <- length(types)
      
      df_ <- df_ %>% transmute(
        ID, wealth_origin, region_origin, area, sex, parents_educ,
        outcome_destino_cont = as.numeric(wealth_destination),
        nodes                = factor(types), predicted
      )
      
      Tree <- try({
        TreeSplits <- capture.output(print(tree))
        nodo_1[n] <- strsplit(TreeSplits[8], ' ')[[1]][5]
      }, silent = T)
      
      if (class(Tree) == "try-error") {
        TreeSplits <- capture.output(print(tree@tree))
        nodo_1[n] <- strsplit(TreeSplits[1], ' ')[[1]][2]
      }
      
      tree_text <- strsplit(TreeSplits, ' ')
      
      v_count <- NULL
      for (variable in variables) {
        count <- NULL
        for (line in tree_text) {
          if (variable %in% as.vector(line)) {
            count <- append(count, 1)
          }
        }
        v_count <- append(v_count, sum(count))
      }
      if (n == 1) {
        variables_c <- rbind(variables, v_count)
      }
      else {
        variables_c <- rbind(variables_c, v_count)
      }
      
      # Estimation of Effort
      df_merge = cbind(ID      = NA, 
                       quintil = NA)
      
      for (t in types) {
        x  <- NA
        q  <- NA
        
        name    <- paste0("df", t)
        dft     <<- filter(df_,
                           nodes == t)
        assign(name, filter(df_, nodes == t))
        x       <- dft$outcome_destino_cont
        
        res <- mable(x,
                     M        = c(2, 100),
                     interval = c(0, max(x)),
                     controls = mable.ctrl(sig.level = 1e-8,
                                           maxit     = 2000,
                                           eps       = 1.0e-9)
        )
        b    <- numeric_var("outcome_destino_cont", "quintil destino",
                            support = c(0, max(x)), 
                            bounds  = c(0, max(x)))
        ctmm <- ctm(response = Bernstein_basis(b, 
                                               order = res$m, 
                                               ui    = "increasing"))
        mltm <- mlt(ctmm, 
                    data = dft,
                    coef = res$p)
        q    <- predict(mltm,
                       newdata = dfi, 
                       type    = "quantile",
                       p       = c(.2, .4, .6, .8))
        
        qi <- sacaQuitnil(x, 
                    distribucion = "bernstein")
        quintil_i <- paste0("quintil_b_nodo_", i)
        assign(quintil_i, qi)
        
        df_quintil_i <- data.frame(
          ID = dft$ID,
          quintil = qi)
        df_merge     <- rbind(df_merge, df_quintil_i) 
      }
      
      df_para_merge = as.tibble(df_merge) %>% na.omit()
      df_final      = data.frame(
        cbind(arrange(df_, desc(ID)), arrange(df_para_merge, desc(ID)))
      )
      
      # IOP Effort Approach
      df_final <- df_final %>%
        mutate(
          pop_mean_outcome = 1
        )
      
      b <- df_final %>%
        group_by(quintil) %>%
        summarise(
          qesf_mean_outcome = mean(outcome_destino_cont,
                                   na.rm = T)
        )
      
      new_base <- left_join(df_final, b,
                            by = "quintil")
      new_base <- new_base %>%
        mutate(
          a_b            = pop_mean_outcome/qesf_mean_outcome,
          outcome_expost = outcome_destino_cont*a_b,
          XWE            = outcome_expost,
          XBE            = qesf_mean_outcome,
        )
      
      # Types Approach ----
      bt <- new_base %>%
        group_by(nodes) %>%
        summarise(
          type_mean_outcome = predicted
        )
      
      new_base2 <- merge(new_base, bt,
                         by = "nodes") %>% 
        mutate(
          XBT  = type_mean_outcome,
          a_bt = pop_mean_outcome/type_mean_outcome,
          XWT  = outcome_destino_cont*a_bt
        )
      
      XWT <- new_base2$XWT
      XBT <- new_base2$XBT
      
      XWE <- new_base2$XWE
      XBE <- new_base2$XBE
      
      outcome_continua <- new_base2$outcome_destino_cont
      
      # Gini & MLD & R^2
      Gini_outcome[n] <- Gini(outcome_continua, na.rm = T)
      MLD_outcome[n]  <- mld.wtd(outcome_continua)
      
      G_XWT[n] <- Gini(XWT, na.rm = T)
      M_XWT[n] <- mld.wtd(XWT)
      G_XBT[n] <- Gini(XBT, na.rm = T)
      M_XBT[n] <- mld.wtd(XBT)
      
      G_XWE[n] <- Gini(XWE, na.rm = T)
      M_XWE[n] <- mld.wtd(XWE)
      G_XBE[n] <- Gini(XBE, na.rm = T)
      M_XBE[n] <- mld.wtd(XBE)
      
      R2[n] <- var(new_base2$predicted)/var(new_base2$outcome_destino_cont)
    }
    
    variables_c  <- as.data.frame(variables_c)
    results_Gini <- as.data.frame(cbind(nodos, nodo_1, 
                                        G_XWE, G_XBE,
                                        G_XWT, G_XBT, 
                                        Gini_outcome))
    results_MLD  <- as.data.frame(cbind(nodos, nodo_1,
                                        M_XWE, M_XBE,
                                        M_XWT, M_XBT, 
                                        MLD_outcome))
    results_R2   <- as.data.frame(R2)
    
    write.csv(results_Gini, paste0('./Results/Resultados_Gini_', a, i, '.csv'))
    write.csv(results_MLD,  paste0('./Results/Resultados_MLD_', a, i, '.csv'))
    write.csv(variables_c,  paste0('./Results/Variables_', a, i, '.csv'))
    write.csv(results_R2,  paste0('./Results/R2_', a, i, '.csv'))
  }
}

