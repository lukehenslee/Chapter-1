#===============================================================================
# Chapter 1: Stock composition 
#
# Date: October 14, 2021
# 
# Creator: Luke Henslee, ADF&G and CFOS, UAF
#
# Purpose: This code addresses objectives of Chapter 1: Coho salmon stock 
# composition in Subdistricts 5 and 6 of Norton Sound 
#===============================================================================
# NOTES: This script uses 'master_mcode.csv' as a data source
#===============================================================================

# Load packages ################################################################
library(tidyverse)
library(data.table)
library(tools)
library(nlme)
library(lme4)
library(mclogit)
library(MASS)
library(VGAM)
library(boot)
library(mgcv)

# Set working directory ########################################################
setwd("C:/Users/lukeh/Desktop/School/Chapter_1/data")
setwd("C:/Users/lhhenslee/Desktop/Luke/School/Thesis/Chapter1/data")

# Import data ##################################################################
# Import master mcode list
# 'master_colClasses.csv' is just a list for 'colClasses' argument of read.csv() 
col <- read.csv("master_colClasses.csv", header = T)

# Import 'master_mcode.csv'
tags <- read.csv("master_mcode.csv", colClasses = paste(col[1,]))

# Data manipulation ####
  ## Final fates ####
  final.fate <- vector()

  for(i in 1:nrow(tags)) {
    if(tags[i,23] %in% c('1a', '1b', '1c')) {
      final.fate[i] <- '1'
    } else {
      if(tags[i,23] %in% c('2a', '2b', '2c')) {
        final.fate[i] <- '2'
      } else {
        if(tags[i,23] %in% c('3a', '3b', '3c')) {
          final.fate[i] <- '3'
        } else {
          final.fate[i] <- '4'
        }
      }
    }
  }

  table(final.fate)
  
  tags$final.fate <- final.fate
  
  ## Subdistrict of capture ####
capture.loc <- vector()

for(i in 1:nrow(tags)) {
  if(is.na(tags[i,18]) == TRUE) {
    capture.loc[i] <- NA
  } else {
    if(tags[i,18] %in% c('6a', '6b')) {
      capture.loc[i] <- '6'
    } else {
        capture.loc[i] <- '5'
    }
  }
}

tags$capture.loc <- as.factor(capture.loc)

  ## Terminal membership response ####

terminal.stock <- vector()

for(i in 1:nrow(tags)) {
  if(is.na(tags[i,28] == T)) {
    terminal.stock[i] <- NA
  } else {
    if(tags[i,28] == tags[i,18]) {
      terminal.stock[i] <- '1'
    } else {
      terminal.stock[i] <- '0'
    }
  }
}

tags$terminal.stock <- as.factor(terminal.stock)

  ## Age ####

  age <- vector()

  for(i in 1:nrow(tags)) {
    if(is.na(tags[i,9] == T)) {
      age[i] <- NA
    } else {
      if(tags[i,9] %in% c(3, 4, 5, 7, 8)) {
        age[i] <- NA
      } else {
        age[i] <- paste(tags[i,9])
      }
    }
  }

tags$age <- as.factor(age)

  ## Wind ####

  wind.dir <- vector()

  for(i in 1:nrow(tags)) {
    if(is.na(tags[i,16] == T)) {
      wind.dir[i] <- NA
    } else {
      if(tags[i,16] == 'N') {
        wind.dir[i] <- 360
      } else {
        if(tags[i,16] == 'NE') {
          wind.dir[i] <- 45
        } else {
          if(tags[i,16] == 'E') {
            wind.dir[i] <- 90
          } else {
            if(tags[i,16] == 'SE') {
              wind.dir[i] <- 135
            } else {
              if(tags[i,16] == 'S') {
                wind.dir[i] <- 180
              } else {
                if(tags[i,16] == 'SW') {
                  wind.dir[i] <- 225
                } else {
                  if(tags[i,16] == 'W') {
                    wind.dir[i] <- 270
                  } else {
                    if(tags[i,16] == 'NW') {
                      wind.dir[i] <- 315
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  hist(wind.dir)

  tags$wind.dir <- wind.dir
  
  # Convert to kph
    # Multiply mph by 1.609344
  
  tags$wind.sp <- round(tags$wind.sp * 1.61, 2)
  
  # U and V winds
  
  tags$u.wind <- round(tags$wind.sp * cos(wind.dir), 2)
  
  tags$v.wind <- round(tags$wind.sp * sin(wind.dir), 2)

# Multinomial model data ####
coho_mod_dat <- tags %>% 
  filter(species == 'coho', is.na(spawn.group) != T)

  ## Reorder our data into a response matrix ####
stock.multinom <- matrix(data = NA, nrow = nrow(coho_mod_dat), ncol = 5)

for(i in 1:nrow(coho_mod_dat)) {
  if(coho_mod_dat[i,28] == 'N' |
     coho_mod_dat[i,28] == '3') {
    stock.multinom[i,] <- c(1,0,0,0,0)
  } else {
    if(coho_mod_dat[i,28] == '4') {
      stock.multinom[i,] <- c(0,1,0,0,0) 
    } else {
      if(coho_mod_dat[i,28] == '5') {
        stock.multinom[i,] <- c(0,0,1,0,0)
      } else {
        if(coho_mod_dat[i,28] == '6') {
          stock.multinom[i,] <- c(0,0,0,1,0)
        } else {
          if(coho_mod_dat[i,28] == 'S') {
            stock.multinom[i,] <- c(0,0,0,0,1)
          }
        }
      }
    }
  }
}

colnames(stock.multinom) <- c('N', '4', '5', '6', 'S')

# Great, now we have a response matrix. Let's combine with our main dataset
coho_mod_dat <- cbind(coho_mod_dat, stock.multinom)

  # Now we need to combine into sampling events
  coho_mult_dat <- coho_mod_dat %>% 
    group_by(year, julian.day, capture.loc, sex) %>% 
    summarize_at(vars('N', '4', '5', '6', 'S'), sum)

  coho_mult_dat$N4 <- coho_mult_dat$N + coho_mult_dat$`4`
  coho_mult_dat$N4S <- coho_mult_dat$N + coho_mult_dat$`4` + coho_mult_dat$S
  
  coho_mult_dat$capture.loc <- as.factor(coho_mult_dat$capture.loc)

# Multinomial modeling ####
  # 'coho' dataframe
  coho <- tags %>% 
    filter(species == 'coho', !is.na(spawn.group))
  
  # Condense the 'S4N' stock
  stock <- vector()
  for(i in 1:nrow(coho)) {
    if(coho[i,28] %in% c('S', '4', 'N')) {
      stock[i] <- 'S4N'
    } else {
      if(coho[i,28] == '6') {
        stock[i] <- '6'
      } else {
        if(coho[i,28] == '5') {
          stock[i] <- '5'
        }
      }
    }
  }
  
  coho$stock <- as.factor(stock)
  
  # Create multinomial model
    # With mclogit
  coho.mblogit <- mblogit(stock ~ year * julian.day + capture.loc + sex,
                          data = coho)
  
  summary(coho.mblogit)
  
    # With vglm
  coho.vglm <- vglm(stock ~ year * julian.day + capture.loc + sex,
                    family = multinomial,
                    data = coho)
  
  summary(coho.vglm)
  
# Predictions ####
  ## Pivot data 
    harv <- harvest_proj %>% 
    pivot_longer(cols = c('M', 'F'), names_to = 'sex', values_to = 'abundance')
  
  harv$sex <- as.factor(harv$sex)
  
  # Predict with mclogit
  pred.mclogit <- predict(coho.mblogit, newdata = harv, type = 'response')
  head(pred.mclogit)
  
  ## Pred with vglm
  pred.vglm <- predict(coho.vglm, newdata = harv, type = 'response')
    
  head(pred.vglm)
  
# Catch partitioning ####
  
  partition <- cbind(harv, pred.vglm)

  partition$'Subdistrict 5' <- partition$abundance * partition$`5`
  partition$'Subdistrict 6' <- partition$abundance * partition$`6`
  partition$'Transitory' <- partition$abundance * partition$S4N  

  # Visualize abundance
  coho.fig <- partition %>% 
    group_by(year, capture.loc) %>% 
    summarize_at(vars(abundance, 'Subdistrict 5', 'Subdistrict 6', 'Transitory'), sum)
  
  coho.fig.long <- coho.fig %>% 
    pivot_longer(cols = c('Subdistrict 5', 'Subdistrict 6', 'Transitory'),
                 names_to = 'Stock')

  ggplot(coho.fig.long, aes(x = capture.loc, y = value, fill = Stock)) +
    geom_col() +
    facet_wrap(~year)
  
  # Visualize proportions
  coho.fig.prop <- coho.fig.long

  coho.fig.prop$value <- coho.fig.prop$value/coho.fig.prop$abundance
  
  ggplot(coho.fig.prop, aes(x = capture.loc, y = value, fill = Stock)) +
    geom_col() +
    facet_wrap(~year)

  # I want proportions for both years combined for the abstract
  abstract <- partition %>% 
    group_by(capture.loc) %>% 
    summarize_at(vars(abundance, 'Subdistrict 5', 'Subdistrict 6', 'Transitory'), sum)

  abstract <- abstract %>% 
    pivot_longer(cols = c('Subdistrict 5', 'Subdistrict 6', 'Transitory'),
                 names_to = 'Stock')

  abstract$value <- abstract$value/abstract$abundance  
  
  # Combined sd
  abstract2 <- colSums(abstract[,2:5])

  (abstract2[2] + abstract2[3])/abstract2[1] 

  
    
# Logistic modeling ####
  
  coho <- tags %>% 
    filter(species == 'coho', !is.na(spawn.group))
  
  coho.compare <- coho[,c(5:6, 9:10, 13:14, 17:18, 30:35)]
  coho.compare.complete <- coho.compare[complete.cases(coho.compare),]

  ## Global model ####
  summary(global <- glm(terminal.stock ~ year * julian.day + capture.loc + lat +
                          u.wind + v.wind + secchi + cloud.cov + 
                          water.temp +
                          age + sex + length,
                        data = coho.compare.complete,
                        family = binomial))
  
  ## Spatiotemporal and demographic ####
  summary(spatemp.dem <- glm(terminal.stock ~ year * julian.day + capture.loc + 
                               lat +
                               age + length + sex,
                             family = binomial,
                             data = coho.compare.complete))
  
  ## Spatiotemporal and abiotic ####
  summary(spatemp.ab <- glm(terminal.stock ~ year * julian.day + capture.loc + 
                              lat +
                              u.wind + v.wind + secchi + cloud.cov +  
                              water.temp ,
                             family = binomial,
                             data = coho.compare.complete))
  
  ## Demographic and abiotic ####
  summary(dem.ab <- glm(terminal.stock ~ age + sex + length +
                              u.wind + v.wind + secchi + cloud.cov +  
                          water.temp,
                            family = binomial,
                            data = coho.compare.complete))
  
  ## Spatiotemporal ####
  summary(spatemp <- glm(terminal.stock ~ year * julian.day + capture.loc + lat,
                 family = binomial,
                 data = coho.compare.complete))
  
  ## Demographic ####
  summary(dem <- glm(terminal.stock ~ age + sex + length,
                         family = binomial,
                         data = coho.compare.complete))
  
  ## Abiotic ####
  summary(ab <- glm(terminal.stock ~ u.wind + v.wind + secchi + cloud.cov + 
                      water.temp,
                     family = binomial,
                     data = coho.compare.complete))
  
  ## Intercept only ####
  summary(ri <- glm(terminal.stock ~ 1,
                    family = binomial,
                    data = coho.compare.complete))
  
  # AIC ###
  glm.aic <- AIC(global, spatemp.dem, spatemp.ab, dem.ab, spatemp, dem, ab, ri)
  glm.aic$delta <- glm.aic$AIC - min(glm.aic$AIC)
  
  ## Final logistic model ####
  
  coho.compare.final <- coho.compare[,c(1:4, 8:10, 12)]
  coho.compare.final <- coho.compare.final[complete.cases(coho.compare.final),]
  
  summary(final.v1 <- glm(terminal.stock ~ year * julian.day + capture.loc + lat +
                 age + length + sex,
               family = binomial,
               data = coho.compare.final))
  
  summary(final.v1.1 <- glm(terminal.stock ~ year * julian.day + capture.loc + lat +
                            length + sex,
                          family = binomial,
                          data = coho.compare.final))
  
  anova(final.v1, final.v1.1, test = "Chisq")
  
  summary(final.v1.2 <- glm(terminal.stock ~ year * julian.day + capture.loc + lat +
                              age + sex,
                            family = binomial,
                            data = coho.compare.final))
  
  anova(final.v1, final.v1.2, test = "Chisq")
  
  coho.compare.noage <- coho.compare[,c(1:2,4,8:10,12)]
  coho.compare.noage <- coho.compare.noage[complete.cases(coho.compare.noage),]
  
  summary(final.v2 <- glm(terminal.stock ~ year * julian.day + capture.loc + 
                            scale(lat, scale = F) +
                        sex,
                       family = binomial,
                       data = coho.compare.noage))
  
  
# Bootstrapping for logistic coefficients ####
  # From Franz, lab 9
  # First, we append fitted values from the model to the original 
  # date frame, as we will need them for bootstrapping:
  pred <- predict(final.v2)
  
  dat <- cbind(coho, pred)
  
  # Writing a bootstrap function to work with 'boot':
  bf <- function(dat, i) {
    # create a bootstrap data set by adding re-sampled residuals 
    # Fit the model to this new bootstrapped data set:
    coef(glm(terminal.stock ~ year * julian.day + capture.loc + 
                      scale(lat, scale = F) + sex,
                    family = binomial,
                    data = dat[i,]))    
  }
  
  ## 5c. Run the bootstrap
  library(boot)
  # The 'boot' function uses the above function ('bf') to do the bootstrapping,
  # i.e. it runs bf with 1999 different sets of resampled residuals from 'resid(fit)':) 
  coho.boot <- boot(coho, bf, R = 5000)
  
  # Confidence intervals for individual predictions 
  # (predicted survival at the first, second, 10th and 40th wind values)
  # The 'boot.ci' function can return four different types of confidence intervals,
  # we extract only the percentile-based interval:
  boot.ci(coho.boot, type = "perc", index = 1)   # Confidence intervals for fitted value at x[1] = 285
  boot.ci(coho.boot, type = "perc", index = 2)   # CI at x[2] = 292.8
  boot.ci(coho.boot, type = "perc", index = 3)
  boot.ci(coho.boot, type = "perc", index = 4)
  boot.ci(coho.boot, type = "perc", index = 5)
  boot.ci(coho.boot, type = "perc", index = 6)
  boot.ci(coho.boot, type = "perc", index = 7)
  
 
# Scratch paper ####
  nrow(tags %>% 
    filter(year == '2021', species == 'coho'))
  
  nrow(tags %>% 
         filter(year == '2021', species == 'chum'))
  
  time.in <- lubridate::hm(tags$time.in)
  time.in <- 60 * lubridate::hour(time.in) + lubridate::minute(time.in)
  
  time.out <- lubridate::hm(tags$time.out)
  time.out <- 60 * lubridate::hour(time.out) + lubridate::minute(time.out)
  
  coho <- tags %>% filter(species == 'coho')
  
  nrow(coho %>% filter(year == '2021', capture.loc == '5', is.na(det.hist), is.na(recap)))

  time <- time.out - time.in  
  mean(time)
  range(time)

  time  
  
  vec <- c(0, 8, 4, 3, 1, 2)
  vec <- c(0, 8, 4, 3, 1, 2, 5) # 2 5 0 8 4 3 1 
  
  shft <- abs(which.max(vec) - length(vec) / 2)
  
  c(
    vec[(shft + 1):length(vec)],
    vec[1:shft]
  )
  
  library(SOfun)
  
  shifter(vec, which.max(vec) - length(vec) / 2)
  