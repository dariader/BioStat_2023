df = read.table('./20231007_hw/hypertension.csv', header = T)
table(df$is_male,df$is_hypertonic)

df = read.table('./20231007_hw/food.csv', header = T)
df$meat_only = ifelse(df$meat==1 & df$salad==0 & df$fish==0, 1,0 )
df$salad_only = ifelse(df$salad==1 & df$meat==0 & df$fish==0, 1,0 )

fish_df = df[df$fish==1,]
table(df$is_poisoned,df$meat_only)

summary(glm(is_poisoned ~ ., df, family = 'binomial'))

cor(df)

library("ggfortify")
autoplot(cor(df))

chisq.test(df$is_poisoned, df$meat)



df = read.table('./20231007_hw/driving.csv', header = T)
table(df$DTP, df$experience_years)

library(survival)
library(survminer)
surv_object <- Surv(time = df$observ_end_day-df$start_day, event = df$DTP)
fit1 <- survfit(surv_object ~ experience_years, data = df)
ggsurvplot(fit1, data = df)
table(df$experience_years,df$DTP)

unexp_time = df[df$experience_years == 0,]
sum(unexp_time$DTP)/sum(unexp_time$observ_end_day-unexp_time$start_day)

exp_time = df[df$experience_years == 1,]
sum(exp_time$DTP)/sum(exp_time$observ_end_day-exp_time$start_day)

