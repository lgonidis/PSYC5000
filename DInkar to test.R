library(haven) ### used to load SPSS .sav files, read_sav()
library(tidyverse) ### for general purpose functions
library(rstatix) ### used for our ANOVA, anova_test()
library(ggpubr) ### used for graphs such as gghistogram()


df <- read_sav("stress.sav")
summary(df)

df <- read_csv("stress.csv")


### remove missing values before converting wide to long
## this is because we want to keep participants having both
## stress1 and stress2 scores

df <- na.omit(df)
summary(df)


df1<- select(df, !c(fno,gender, role, cond))

df1$id <- seq.int(nrow(df1))


summary(df1)

count(df1)

df1<-na.omit(df1)

### wide to long
df_long <- df1 %>% 
  pivot_longer(c(stress1, stress2), 
               names_to = c("Time"),
               values_to = "Stress")
### Time as factor
df_long$Time <-factor(df_long$Time)

### id as factor
df_long$id <- factor(df_long$id)

### have an initial look at descriptive stats
summary(df_long)




result.anova <- anova_test(data = df_long, 
                           dv = Stress, 
                           wid = id,
                           within = Time,
                           detailed = TRUE)
result.anova











