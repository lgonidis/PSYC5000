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


#means and marginal means
library(rstatix) #(ggpubr) 
df1 %>% group_by(recall_type, position) %>% get_summary_stats(score, type="mean_sd")
df1 %>% group_by(recall_type) %>% get_summary_stats(score, type="mean_sd")
df1 %>% group_by(position) %>% get_summary_stats(score, type="mean_sd")

df1 %>% get_summary_stats(score, type="mean_sd") #grand mean

#interaction plot

library(rstatix)
#interaction plot
#first create a summary table with means and standard errors
df2<-df1 %>% group_by(recall_type, position) %>% 
  get_summary_stats(score, type="mean_se")
df2

library(ggplot2)
#lineplot with standard error bars
ggplot(df2, aes(x=position, y=mean, group=recall_type))+
  geom_line(aes(linetype=recall_type, color=recall_type),lwd=2)+
  geom_point()+
  labs(y="Mean correct (+- 1SE)", x="POSITION")+
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2)+
  theme_grey(base_size = 20)

#Alternative ways to do interaction plot
with(df1, interaction.plot(position, recall_type, score))

interaction.plot(df1$position, df1$recall_type, df1$score,
                 fun = mean,
                 type = "l",
                 ylab = "score",
                 xlab = "Position",
                 lty = 1,
                 ylim = range(0,5),
                 col = c("red", "blue"),
                 trace.label = "recall_type")
anova(model)['Residuals', 'Mean Sq']

library(rstatix)
df1 %>%
    anova_test(dv = score, wid = pn, 
             within = c(position,recall_type), 
             detailed = TRUE) 
#%>%
  #get_anova_table()
 #get_anova_table(., correction = "HF")

result.anova <-anova_test(data=df1, dv=score,  
                        wid=pn,
                        within = c(position, recall_type),
                        detailed = TRUE)
#you can add type=3 here 
#though don't need to as it is the default for within-subject designs
result.anova

#the above result.anova shows the full list
#the below result.anova[1] only shows the first list etc.

result.anova[1] #ANOVA table
result.anova[2] #Mauchly's Test for Sphericity
result.anova[3] #Sphericity Corrections`

get_anova_table(result.anova)
#to get full anova table with correction
get_anova_table(result.anova, correction = "HF")

result.aov  #this does the ANOVA and shows correction for sphericity                       

get_anova_table(result.anova)  #produces a nicer anova table, assumes sphericity

get_anova_table(result.anova, correction = "HF") #this show anova table with sphericity correction

#alternative notation this produces sphericity test output automatically
result.anova<- df1 %>% anova_test(score ~ position*recall_type 
                                + Error(pn/(position*recall_type)))
result.anova


#simple main effect

#version 1

#simple main effect of recall_type within each levels of position
df1 %>%
  group_by(position) %>%
  anova_test(dv = score, wid = pn, 
             within = recall_type, 
             detailed = TRUE) %>%
  get_anova_table()

get_anova_table()
#version 2
result.aov<- df1 %>% group_by(position) %>% 
  anova_test(score ~ recall_type + Error(pn/(recall_type)))%>%
  get_anova_table()
result.aov


#version1
#simple main effect of position within each level of recall_type
 df1 %>%
  group_by(recall_type) %>%
  anova_test(dv = score, wid = pn, 
             within = position, 
             detailed = FALSE) %>%
  get_anova_table(., correction = "HF")

 df1 %>%
   group_by(recall_type) %>%
   anova_test(dv = score, wid = pn, 
              within = position, 
              detailed = TRUE) #%>%
   get_anova_table()  %>%
   adjust_pvalue(method = "bonferroni")
 #p.adjust.method = "bonferroni"
 
   df1 %>%
   group_by(recall_type) %>%
      pairwise_t_test(score ~ position, 
                      paired = TRUE,
                      p.adjust.method = "bonferroni")
     
 #follow on t-test with bonferroni correct
 emmeans(model, pairwise~position, adjust = "bonferroni")
 
#version 2
result.aov<- df1 %>% group_by(recall_type) #%>% 
  #anova_test(score ~ position + Error(pn/(position)))%>%
  lm(score ~ position + Error(pn/(position)))%>%
  get_anova_table()
result.aov

lm(score ~ position + Error(pn/(position), data=df1))
   
result.aov<- df1 %>% group_by(recall_type) %>% 
  #anova_test(score ~ position + Error(pn/(position)))%>%
  aov(score ~ position + Error(pn/(position)))%>%
  get_anova_table()
result.aov
#follow on t-test with bonferroni correction
emmeans(result.aov, pairwise~position|recall_type, adjust = "bonferroni")
emmeans(model, pairwise~recall_type|position, adjust = "bonferroni")

emmeans(model, pairwise~|position) #here default is tukey test

