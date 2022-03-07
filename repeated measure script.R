library(rstatix)# need this for anova
library(haven) #needed to inported data
library(ggplot2)
library(ggpubr) # need this for using %>%
library(tidyverse)# need this for anova
library(graphics)
library(dplyr)
library(gt)
library(emmeans)

library(tidyverse)


library(haven)
df1<-read_sav("memory.sav")

summary(df1)

#label the factors including the participant number pn
df1$recall_type <- factor(df1$recall_type,
                   levels=c("I","D"),
                   labels=c("Immediate","delay"))
df1$position <- factor(df1$position, 
                        levels=c(1,6,12), 
                        labels=c("1","6","12"))
df1$pn <- factor(df1$pn)
df1



df2 <- filter(df1, position != 12)



result.anova <-anova_test(data=df2, dv=score,  
                        wid=pn,
                        within = c(position, recall_type),
                        detailed = TRUE)
#you can add type=3 here 
#though don't need to as it is the default for within-subject designs
result.anova

