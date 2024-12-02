head(data)

model <- lm(lnearn ~ highqua + age + agesq, data = data)

summary(model)

model2 <- ivreg(lnearn ~ highqua + age + agesq | twihigh + age + agesq, data = data)

summary(model2)

robust_vcov <- vcovHC(model, type = "HC1")
print(robust_vcov)

coeftest(model, vcov = robust_vcov)

data_wide <- data %>%
  pivot_wider(
    names_from = twinno,        
    values_from = -c(family, twinno), 
    names_prefix = ""             
    
  )

data_wide <- data_wide %>%
  mutate(
    dlnearn = lnearn_1 - lnearn_2,      
    dhigh = highqua_1 - highqua_2,       
    dtwihi = twihigh_1 - twihigh_2      
  )

reg_no_constant <- lm(dlnearn ~ dhigh - 1, data = data_wide) 
summary(reg_no_constant)
ivreg_no_constant <- ivreg(dlnearn ~ dhigh - 1 | dtwihi -1 , data = data_wide) 
summary(ivreg_no_constant)

reg_constant <- lm(dlnearn ~ dhigh , data = data_wide) 
summary(reg_constant)
ivreg_constant <- ivreg(dlnearn ~ dhigh  | dtwihi  , data = data_wide) 
summary(ivreg_constant)


coeftest(reg_no_constant, vcov = vcovHC(reg_no_constant, type = "HC1"))

data_wide <- data_wide %>%
  mutate(awage = abs(earning_1 - earning_2))


data_clean <- data_wide %>%
  filter(awage <= 60) 

reg_no_constant_clean <- lm(dlnearn ~ dhigh - 1, data = data_clean) 
summary(reg_no_constant_clean)
ivreg_no_constant_clean <- ivreg(dlnearn ~ dhigh - 1 | dtwihi -1 , data = data_clean) 
summary(ivreg_no_constant_clean)
