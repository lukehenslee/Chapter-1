harv <- read.csv('harv_asl_20-21.csv')

coho <- tags %>% 
  filter(species == 'coho', is.na(spawn.group) != T)

glm.mult <- mblogit(spawn.group ~ year + julian.day + capture.loc + sex,
                    data = coho)

harv$year <- as.factor(harv$year)
harv$capture.loc <- as.factor(harv$capture.loc)
harv <- pivot_longer(data = harv, cols = c('F', 'M'), names_to = 'sex',
                     values_to = 'total.fish')
catch <- read.csv('F604_project_catch.csv')
catch <- pivot_longer(data = catch, cols = c('M', 'F'), names_to = 'sex')
catch$year <- as.factor(catch$year)
catch$capture.loc <- as.factor(catch$capture.loc)
catch$sex <- as.factor(catch$sex)

predict(glm.mult, newdata= catch, type = 'response')

glm.mult <- multinom(spawn.group ~ year + julian.day + capture.loc + sex,
                 data = coho)
catch$julian.day <- yday(mdy(catch$julian.day))

write.csv(coho, 'coho.csv')
coho <- read.csv('coho.csv')
coho$sex <- as.factor(coho$sex)
coho$capture.loc <- as.factor(coho$capture.loc)
coho$year <- as.factor(coho$year)
coho$spawn.group <- as.factor(coho$spawn.group)

glm.mult <- mblogit(spawn.group ~ year + julian.day + sex + capture.loc,
                    data = coho)


pred.catch$'Terminal' <- pred.catch$value * pred.catch$pred 
pred.catch$'Transitory' <- pred.catch$value * (1 - pred.catch$pred)

pred.catch2 <- pred.catch %>% 
  group_by(capture.loc, year) %>% 
  summarize_at(vars(value, Terminal, Transitory), sum)

pred.catch2$`Subdistrict 5` <- pred.catch2$`Subdistrict 5`/pred.catch2$value
pred.catch2$`Subdistrict 6` <- pred.catch2$`Subdistrict 6`/pred.catch2$value
pred.catch2$Transitory <- pred.catch2$Transitory/pred.catch2$value

pred.catch3 <- pred.catch2 %>% 
  pivot_longer(cols = c(Terminal , Transitory), names_to = 'Stock', values_to = 'fish')

pred.catch3$fish <- 1
