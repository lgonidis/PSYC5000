# http://www.sthda.com/english/wiki/one-way-anova-test-in-r#summary
#First download the file from Moodle. 
#Then import anorexia.sav file or using the import dataset function opposite. 
# or use data_new<-read_spss("Anorexia_trial.sav")

data_new<-Anorexia_trial


# in R terminology, the column “treatment” is called factor 
# and the different categories (1,2,3) ordered alphabetically
data_new$treatment <- factor(data_new$treatment,
                             levels=c(1,2,3),
                             labels=c("Control","Cognitive Behaviour","Family"))
data_new
summary(data_new)


#load rstatix to use descriptive stat using function get_summary_stats
library(rstatix)
data_new %>% get_summary_stats(type="common")
#or you can use data_new <- get_summary_stats(data_new, type="common")

#for summary stats for each treatment
data_new %>% group_by(treatment) %>% get_summary_stats (type="common")

library(ggplot2)
library(haven)
library(ggpubr)
ggboxplot(data_new, x = "treatment", y="weight_gain",
          color = "treatment",
          palette = "null", 
          ylim = c(-20,30),
          order =c("Cognitive Behaviour","Control","Family"))

############ ggline needs more work to get line on the plot
ggline(data_new,"treatment", "weight_gain", merge=TRUE)

,
       #add=c("mean_se", "jitter"),
          color = "treatment",
          palette = "null", 
          ylim = c(-20,30),
          order =c("Cognitive Behaviour","Control","Family"))


## ANOVA TEST

res.aov <- aov(weight_gain ~ treatment, data=data_new)
res.aov
summary(res.aov)

#or we can use the anova_test() in rstatix
res.aov <- data_new %>% anova_test(weight_gain~treatment, detailed = TRUE)
res.aov
summary(res.aov)

# Compute t-test with bonferroni correction
res.t <- t_test(weight_gain~treatment, 
                data = data_new, 
                var.equal = TRUE, 
                p.adjust.method = "bonferroni")

res.t

## gt used to create nice looking tables
install.packages("gt")
library(gt)
gt(res.t)

TukeyHSD(res.aov)
tukey_hsd(res.aov)
TukeyHSD(aov(weight_gain ~ treatment, data=data_new))
tukey_hsd(aov(weight_gain ~ treatment, data=data_new))
#to improve output of tukey_hsd  use gs() first place the output in res_tky
res_tky <- tukey_hsd(aov(weight_gain ~ treatment, data=data_new))
res_tky
library(gt)
gt(res_tky)


### PLANNED COMPARISION AND CONTRASTS

# here we are working with aov() and its output res.aov

res.aov <- aov(weight_gain ~ treatment, data=data_new)

# this line does not work with fit.contrast():
res.aov <- data_new %>% anova_test(weight_gain~treatment, detailed = FALSE)
get_anova_table(res.aov)
summary(res.aov)

# to use fit.contrast() first call gmodels library
install.packages("gmodels")
library(gmodels)

fit.contrast(res.aov, "treatment", c( -1, .5, .5), df=TRUE)

fit.contrast(res.aov, "treatment", c( 0,1,-1), df=TRUE)


fit.contrast(res.aov, "treatment", 
             rbind(" C vs CBT&FT" = c(-1, .5, .5)," CBT vs FT" = c(0, -1, 1)),
             df=TRUE)

fit.contrast(res.aov, "treatment", 
             rbind (c(-1, .5, .5),c(0, -1, 1)),
             df=TRUE)



######### below have tried other things but not needed at the moment
## model2 <- aov(weight_gain ~ treatment, data=data_new)
##model3 <- emmeans_test(data_new, weight_gain ~ treatment,
p.adjust.method = "bonferroni",
detailed = TRUE)

##summary(model2)

contr.sum(3)

summary(model3)
summary.lm(model2)
summary(aov(weight_gain ~ treatment, data=data_new,contrast2<-c(0, -1, 1)))

K <- cbind("C vs (CBT&FT)" = c(-1, 0.5, 0.5),
           "CBT vs FT" = c(0, -1, 1))

K <- cbind("C vs (CBT&FT)" = c(-1, 0.5, 0.5),
            "CBT vs FT" = c(0, -1, 1))
contrasts(data_new$treatment) <- K
K
model1 <- aov(weight_gain ~ treatment, data=data_new)
summary(model1)
summary.lm(model1)

res.aov <- aov(weight_gain ~ treatment, data=data_new)
summary(res.aov)
summary.aov(res.aov)
contrasts(data_new$treatment) <- K
model1 <- aov(weight_gain ~ treatment, data=data_new)
summary(model1)
summary(model1, split=list(treatment=list("C vs All_T"=1, "CBT vs ft " = 2)))

summary(model1, split=treatment=list("x"=1,"y"=2)))

levels(data_new$treatment)

