library(tidyverse) ### for general purpose functions
library(rstatix) ### used for our ANOVA, anova_test()
library(ggpubr) ### used for graphs such as gghistogram()




df <- read_csv("repeated2by4.csv")


### remove missing values before converting wide to long
## this is because we want to keep participants having both
## stress1 and stress2 scores

## acquiring initial descriptives
summary(df)

## marginal means
df %>% 
  group_by()




### wide to long
df_long <- df %>% 
  pivot_longer(!sno, 
               names_to = c("illumination", "colour"),
               names_pattern = "(.*?)_(.*)",
               values_to = "Performance")

### sno as factor
df_long$sno <-factor(df_long$sno)

### illumination as factor
df_long$illumination <- factor(df_long$illumination)

### colour as factor
df_long$colour <- factor(df_long$colour)

### have an initial look at descriptive stats
summary(df_long)



## cell means for colour by illumination

df_long %>% 
  group_by(colour, illumination) %>% 
  get_summary_stats(Performance)


### marginal mean for green screen
### grouping by colour
df_long %>% 
  group_by(colour) %>% 
  get_summary_stats(Performance)


result.anova <- anova_test(data = df_long, 
                           dv = Performance, 
                           wid = sno,
                           within = c(colour, illumination),
                           detailed = TRUE)
result.anova


df_long %>% 
  group_by(colour) %>%
  anova_test(dv = Performance, 
             wid = sno,
             within = illumination,
             detailed = TRUE) %>% 
  get_anova_table()

  
