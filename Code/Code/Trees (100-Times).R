setwd("D:/Investigación/JDS-Corrections")
# Libraries & Functions----
library(ggparty)
library(mable)
library(lognorm)
library(haven)
library(mlt)
library(tidyverse)
library(DescTools)
library(dineq)

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

defaultW <- getOption("warn")
options(warn = -1)

variables <- c("wealth_origin", "wealth_destination", "region_origin", "parents_educ",
               "rural", "sex")

# Trees ----
trees <- c(1, .8, .5, 0.1260073)
regions <- c("2017", "R", "U", "N", "NO", "CO", "C", "CDMX", "S")
depth <- c(9, 6, 5, 4, 6, 5, 4, 8, 4)
for (i in regions) {
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
    G_XWE_B <- NULL
    G_XBE_B <- NULL
    G_XWE_L <- NULL
    G_XBE_L <- NULL
    G_XWE_K <- NULL
    G_XBE_K <- NULL
    M_XWE_B <- NULL
    M_XBE_B <- NULL
    M_XWE_L <- NULL
    M_XBE_L <- NULL
    M_XWE_K <- NULL
    M_XBE_K <- NULL
    
    G_XWT        <- NULL
    M_XWT        <- NULL
    G_XBT        <- NULL
    M_XBT        <- NULL
    Gini_outcome <- NULL
    MLD_outcome  <- NULL
    
    M_xwe_rural_b   <- NULL
    G_xwe_rural_b   <- NULL
    M_xwe_urbano_b  <- NULL
    G_xwe_urbano_b  <- NULL
    M_xwe_rural_l   <- NULL
    G_xwe_rural_l   <- NULL
    M_xwe_urbano_l  <- NULL
    G_xwe_urbano_l  <- NULL
    M_xwe_rural_k   <- NULL
    G_xwe_rural_k   <- NULL
    M_xwe_urbano_k  <- NULL
    G_xwe_urbano_k  <- NULL
    M_xwe_region1_b <- NULL
    G_xwe_region1_b <- NULL
    M_xwe_region2_b <- NULL
    G_xwe_region2_b <- NULL
    M_xwe_region3_b <- NULL
    G_xwe_region3_b <- NULL
    M_xwe_region4_b <- NULL
    G_xwe_region4_b <- NULL
    M_xwe_region5_b <- NULL
    G_xwe_region5_b <- NULL
    M_xwe_region6_b <- NULL
    G_xwe_region6_b <- NULL
    M_xwe_region1_l <- NULL
    G_xwe_region1_l <- NULL
    M_xwe_region2_l <- NULL
    G_xwe_region2_l <- NULL
    M_xwe_region3_l <- NULL
    G_xwe_region3_l <- NULL
    M_xwe_region4_l <- NULL
    G_xwe_region4_l <- NULL
    M_xwe_region5_l <- NULL
    G_xwe_region5_l <- NULL
    M_xwe_region6_l <- NULL
    G_xwe_region6_l <- NULL
    M_xwe_region1_k <- NULL
    G_xwe_region1_k <- NULL
    M_xwe_region2_k <- NULL
    G_xwe_region2_k <- NULL
    M_xwe_region3_k <- NULL
    G_xwe_region3_k <- NULL
    M_xwe_region4_k <- NULL
    G_xwe_region4_k <- NULL
    M_xwe_region5_k <- NULL
    G_xwe_region5_k <- NULL
    M_xwe_region6_k <- NULL
    G_xwe_region6_k <- NULL
    
    
    M_xbe_rural_b   <- NULL
    G_xbe_rural_b   <- NULL
    M_xbe_urbano_b  <- NULL
    G_xbe_urbano_b  <- NULL
    M_xbe_rural_l   <- NULL
    G_xbe_rural_l   <- NULL
    M_xbe_urbano_l  <- NULL
    G_xbe_urbano_l  <- NULL
    M_xbe_rural_k   <- NULL
    G_xbe_rural_k   <- NULL
    M_xbe_urbano_k  <- NULL
    G_xbe_urbano_k  <- NULL
    M_xbe_region1_b <- NULL
    G_xbe_region1_b <- NULL
    M_xbe_region2_b <- NULL
    G_xbe_region2_b <- NULL
    M_xbe_region3_b <- NULL
    G_xbe_region3_b <- NULL
    M_xbe_region4_b <- NULL
    G_xbe_region4_b <- NULL
    M_xbe_region5_b <- NULL
    G_xbe_region5_b <- NULL
    M_xbe_region6_b <- NULL
    G_xbe_region6_b <- NULL
    M_xbe_region1_l <- NULL
    G_xbe_region1_l <- NULL
    M_xbe_region2_l <- NULL
    G_xbe_region2_l <- NULL
    M_xbe_region3_l <- NULL
    G_xbe_region3_l <- NULL
    M_xbe_region4_l <- NULL
    G_xbe_region4_l <- NULL
    M_xbe_region5_l <- NULL
    G_xbe_region5_l <- NULL
    M_xbe_region6_l <- NULL
    G_xbe_region6_l <- NULL
    M_xbe_region1_k <- NULL
    G_xbe_region1_k <- NULL
    M_xbe_region2_k <- NULL
    G_xbe_region2_k <- NULL
    M_xbe_region3_k <- NULL
    G_xbe_region3_k <- NULL
    M_xbe_region4_k <- NULL
    G_xbe_region4_k <- NULL
    M_xbe_region5_k <- NULL
    G_xbe_region5_k <- NULL
    M_xbe_region6_k <- NULL
    G_xbe_region6_k <- NULL
    
    M_xwt_rural   <- NULL
    G_xwt_rural   <- NULL
    M_xwt_urbano  <- NULL
    G_xwt_urbano  <- NULL
    M_xwt_region1 <- NULL
    G_xwt_region1 <- NULL
    M_xwt_region2 <- NULL
    G_xwt_region2 <- NULL
    M_xwt_region3 <- NULL
    G_xwt_region3 <- NULL
    M_xwt_region4 <- NULL
    G_xwt_region4 <- NULL
    M_xwt_region5 <- NULL
    G_xwt_region5 <- NULL
    M_xwt_region6 <- NULL
    G_xwt_region6 <- NULL
    
    M_xbt_rural   <- NULL
    G_xbt_rural   <- NULL
    M_xbt_urbano  <- NULL
    G_xbt_urbano  <- NULL
    M_xbt_region1 <- NULL
    G_xbt_region1 <- NULL
    M_xbt_region2 <- NULL
    G_xbt_region2 <- NULL
    M_xbt_region3 <- NULL
    G_xbt_region3 <- NULL
    M_xbt_region4 <- NULL
    G_xbt_region4 <- NULL
    M_xbt_region5 <- NULL
    G_xbt_region5 <- NULL
    M_xbt_region6 <- NULL
    G_xbt_region6 <- NULL
    
    M_outcome_rural   <- NULL
    G_outcome_rural   <- NULL
    M_outcome_urbano  <- NULL
    G_outcome_urbano  <- NULL
    M_outcome_region1 <- NULL
    G_outcome_region1 <- NULL
    M_outcome_region2 <- NULL
    G_outcome_region2 <- NULL
    M_outcome_region3 <- NULL
    G_outcome_region3 <- NULL
    M_outcome_region4 <- NULL
    G_outcome_region4 <- NULL
    M_outcome_region5 <- NULL
    G_outcome_region5 <- NULL
    M_outcome_region6 <- NULL
    G_outcome_region6 <- NULL
    
    set.seed(123)
    j = 0
    for (n in 1:100) {
      j = j+1
      print(n)
      df_ <- dfi %>% sample_n(nrow(df)*a, replace = T)
      
      # Estimation of Types
      tree             <- ctree(wealth_destination~.-ID,
                                control = ctree_control(maxdepth = depth[j]),
                                data = df_)
      
      df_$predicted    <- predict(tree,
                                  data = df_, 
                                  type = "node")
      types            <- sort(unique(df_$predicted))
      nodos[n]         <- length(types)
      df_ <- df_ %>% transmute(
        ID, wealth_origin, region_origin, area, sex, parents_educ,
        outcome_destino_cont = as.numeric(wealth_destination),
        nodes                = factor(predicted)
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
      df_merge = cbind(ID        = NA, 
                       quintil_b = NA, 
                       quintil_l = NA, 
                       quintil_k = NA)
      
      for (i in types) {
        x    <- NA
        q_b  <<- NA
        q_l  <<- NA
        q_k  <<- c(NA)
        qi_b <<- c(NA)
        qi_l <<- c(NA)
        qi_k <<- c(NA)
        
        name    <- paste0("df", i)
        dfi     <<- filter(df_,
                           nodes == i)
        assign(name, filter(df_, nodes == i))
        x       <- dfi$outcome_destino_cont
        
        ## Bernstein 
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
                    data = dfi,
                    coef = res$p)
        q_b  <<- predict(mltm,
                         newdata = dfi, 
                         type    = "quantile",
                         p       = c(.2, .4, .6, .8))
        
        sacaQuitnil(x, 
                    distribucion = "bernstein")
        quintil_i_b <- paste0("quintil_b_nodo_", i)
        assign(quintil_i_b, qi_b)
        
        ## Lognormal
        logn <- estimateParmsLognormFromSample(x)
        l    <- plnorm(x,
                       meanlog = logn[1],
                       sdlog   = logn[2])
        q_l  <<- qlnorm(c(.2, .4, .6, .8),
                        meanlog = logn[1], 
                        sdlog   = logn[2])
        
        sacaQuitnil(x,
                    distribucion = "log-normal")
        quintil_i_l <- paste0("quintil_l_nodo_", i)
        assign(quintil_i_l, qi_l)
        
        ## Kernell
        k   <- density(x,
                       kernel = "gaussian")
        q_k <<- quantile(k[[1]],
                         c(.2, .4, .6, .8))
        
        sacaQuitnil(x, 
                    distribucion = "Kernel")
        quintil_i_k <- paste0("quintil_k_nodo_", i)
        assign(quintil_i_k, qi_k)
        
        df_quintil_i <- as.data.frame(cbind(ID        = dfi$ID,
                                            quintil_b = qi_b, 
                                            quintil_l = qi_l,
                                            quintil_k = qi_k))
        df_merge     <- rbind(df_merge, df_quintil_i)
      }
      
      df_para_merge = as.data.frame(df_merge)
      df_final      = full_join(df_, df_merge, 
                                by = 'ID')
      
      # IOP Effort Approach
      df_final <- df_final %>%
        mutate(
          pop_mean_outcome = mean(outcome_destino_cont,
                                  na.rm = T)
        )
      
      ## Bernstein
      b <- df_final %>%
        group_by(quintil_b) %>%
        summarise(
          qesf_mean_outcome = mean(outcome_destino_cont,
                                   na.rm = T)
        )
      
      new_base <- left_join(df_final, b,
                            by = "quintil_b")
      new_base <- new_base %>%
        mutate(
          a_b            = pop_mean_outcome/qesf_mean_outcome,
          outcome_expost = outcome_destino_cont*a_b,
          XWE_bernstein  = outcome_expost,
          XBE_bernstein  = qesf_mean_outcome,
        )
      
      ## Lognormal
      b_l <- df_final %>%
        group_by(quintil_l) %>%
        summarise(
          qesf_mean_outcome_l = mean(outcome_destino_cont, 
                                     na.rm = T)
        )
      
      new_base <- merge(new_base, b_l,
                        by = "quintil_l") %>%
        mutate(
          a_b_log          = pop_mean_outcome/qesf_mean_outcome,
          outcome_expost_l = outcome_destino_cont*a_b_log,
          XWE_Lognormal    = outcome_expost_l,
          XBE_Lognormal    = qesf_mean_outcome_l,
        )
      
      ## Kernel
      b_k <- df_final %>%
        group_by(quintil_k) %>%
        summarise(
          qesf_mean_outcome_k = mean(outcome_destino_cont,
                                     na.rm = T)
        )
      
      new_base <- merge(new_base, b_k,
                        by = "quintil_k") %>%
        mutate(
          a_b_kernel       = pop_mean_outcome/qesf_mean_outcome,
          outcome_expost_k = outcome_destino_cont*a_b_kernel,
          XWE_kernel       = outcome_expost_k,
          XBE_kernel       = qesf_mean_outcome_k,
        )
      
      
      # Types Approach ----
      bt <- new_base %>%
        group_by(nodes) %>%
        summarise(
          type_mean_outcome = mean(outcome_destino_cont,
                                   na.rm = T)
        )
      
      new_base2 <- merge(new_base, bt,
                         by = "nodes") %>% 
        mutate(
          XBT  = type_mean_outcome,
          a_bt = pop_mean_outcome/type_mean_outcome,
          XWT  = outcome_destino_cont*a_bt
        )
      
      XWE_bernstein <- new_base2$XWE_bernstein
      XBE_bernstein <- new_base2$XBE_bernstein
      XWE_Lognormal <- new_base2$XWE_Lognormal
      XBE_Lognormal <- new_base2$XBE_Lognormal
      XWE_Kernel    <- new_base2$XWE_kernel
      XBE_Kernel    <- new_base2$XBE_kernel
      
      XWT <- new_base2$XWT
      XBT <- new_base2$XBT
      
      outcome_continua <- new_base2$outcome_destino_cont
      
      # Gini & MLD
      G_XWE_B[n] <- Gini(XWE_bernstein, na.rm = T)
      M_XWE_B[n] <- mld.wtd(XWE_bernstein)
      G_XBE_B[n] <- Gini(XBE_bernstein, na.rm = T)
      M_XBE_B[n] <- mld.wtd(XBE_bernstein)
      G_XWE_L[n] <- Gini(XWE_Lognormal, na.rm = T)
      M_XWE_L[n] <- mld.wtd(XWE_Lognormal)
      G_XBE_L[n] <- Gini(XBE_Lognormal, na.rm = T)
      M_XBE_L[n] <- mld.wtd(XBE_Lognormal)
      G_XWE_K[n] <- Gini(XWE_Kernel, na.rm = T)
      M_XWE_K[n] <- mld.wtd(XWE_Kernel)
      M_XBE_K[n] <- Gini(XBE_Kernel, na.rm = T)
      M_XBE_K[n] <- mld.wtd(XBE_Kernel)
      
      G_XWT[n] <- Gini(XWT, na.rm = T)
      M_XWT[n] <- mld.wtd(XWT)
      G_XBT[n] <- Gini(XBT, na.rm = T)
      M_XBT[n] <- mld.wtd(XBT)
      
      Gini_outcome[n] <- Gini(outcome_continua, na.rm = T)
      MLD_outcome[n]  <- mld.wtd(outcome_continua)
    }
    
    variables_c  <- as.data.frame(variables_c)
    results_Gini <- as.data.frame(cbind(nodos, nodo_1, 
                                        G_XWE_B, G_XBE_B, G_XWE_L,
                                        G_XBE_L, G_XWE_K, G_XBE_K,
                                        G_XWT, G_XBT, Gini_outcome)
                                  )
    results_MLD  <- as.data.frame(cbind(nodos, nodo_1,
                                        M_XWE_B, M_XBE_B, M_XWE_L,
                                        M_XBE_L, M_XWE_K, M_XBE_K,
                                        M_XWT, M_XBT, MLD_outcome)
                                  )
    write.csv(results_Gini, paste0('./Results/Resultados_Gini_', a, i, '.csv'))
    write.csv(results_MLD,  paste0('./Results/Resultados_MLD_', a, i, '.csv'))
    write.csv(variables_c,  paste0('./Results/Variables_', a, i, '.csv'))
  }
}
