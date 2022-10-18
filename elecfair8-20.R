#############################################################################################
####clear everything.
rm(list=ls())

#### package check
pkg <- c("car", "psych", "ggplot2", "ggridges", "estimatr", "summarytools", 
         "tidyverse", "sandwich", "interactions", "patchwork", "flextable",  
         "dotwhisker", "hrbrthemes", "huxtable", "haven", "lmtest")
new.pkg <- pkg[!(pkg %in% installed.packages()[,"Package"])]
if(length(new.pkg)) install.packages(new.pkg)

####packages.
library(car)
library(psych)
library(ggplot2)
library(ggridges)
library(estimatr)
library(summarytools)
library(tidyverse)
library(sandwich)
library(interactions)
library(patchwork)
library(dotwhisker)
library(hrbrthemes)
library(huxtable)
library(haven)
library(lmtest)

## run to use hrbrthemes, follow instructions to install --
## sometimes need a restart of R after this to get fonts:
import_roboto_condensed()

#############################################################################################
###### functions 

# 0-1 recoding function
std01<-function(x){
  min.x<-min(x, na.rm=T)
  max.x<-max(x-min.x, na.rm=T)
  return((x-min.x)/max.x)
}
rsumm<-function(x){
  descr(x, stats='common', transpose=T)
}
tab<-function(x){
  freq(as_factor(x))
}

#############################################################################################
##### original data prep

#####load data from Stata.
raw1<-read_dta("rawT1.dta")
raw2<-read_dta("rawT2.dta")  

#####merge dataframes:
## .t1 = time 1 value of variable, .t2 = value of variable
raw <- merge(raw1, raw2, by="PID", all=T, suffixes = c(".t1",".t2"))
Data <- raw

###### recoding and renaming

# subset for Wave 2 respondents only (for descriptives)
Dataw2 <- subset(Data, is.na(EndDate.t2)==F)

# demographics
# age: Data$age

# gender
Data$gend<-Data$gender_dummy

# income
Data$rinct1<-ifelse(Data$income<12, (Data$income-1)/10, NA)

# education
Data$educ<-Recode(Data$education, "1:3=0; 4=1; 5:6=2; 7:8=3; 9:14=4")/4

# white
Data$whitet1<-ifelse(Data$ethnicity==1 & Data$hispanic==2, 1, 0) 

# ideology, T2
Data$ridt2<-(Data$Ideo1.t2-1)/6

# pid, T2
Data$pid3t2 <- NA
Data$pid3t2 <- replace(Data$pid3t2, Data$Q145.t2==1, 0)
Data$pid3t2 <- replace(Data$pid3t2, Data$Q145.t2==2, 2)
Data$pid3t2 <- replace(Data$pid3t2, Data$Q145.t2==3, 1)
Data$pid3t2 <- replace(Data$pid3t2, Data$Q148.t2==1, 0)
Data$pid3t2 <- replace(Data$pid3t2, Data$Q148.t2==2, 2)
Data$pid3t2 <- factor(Data$pid3t2, labels = c("Democrat/Lean Democrat", "Independent", 
                                              "Republican/Lean Republican"))
Data$rpid3t2<-(relevel(as.factor(Data$pid3t2), ref = "Independent"))

Data$idem<-ifelse(Data$pid3t2=="Democrat/Lean Democrat", 1 , 0)
Data$irep<-ifelse(Data$pid3t2=="Republican/Lean Republican", 1 , 0)
Data$iind<-ifelse(Data$pid3t2=="Independent", 1 , 0)

# perceived election fraud, T2
Data$rf1t2<-Recode(Data$Q123_1.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf2t2<-Recode(Data$Q123_2.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf3t2<-Recode(Data$Q123_3.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf4t2<-Recode(Data$Q123_4.t2, "45=5; 46=4; 47=3; 48=2; 49=1; 50=0")/5
Data$rf5t2<-Recode(Data$Q123_5.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf6t2<-Recode(Data$Q123_6.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf7t2<-Recode(Data$Q123_7.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf8t2<-Recode(Data$Q123_8.t2, "45=5; 46=4; 47=3; 48=2; 49=1; 50=0")/5
Data$rf9t2<-Recode(Data$Q123_9.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf10t2<-Recode(Data$Q123_10.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
Data$rf11t2<-Recode(Data$Q123_11.t2, "45=0; 46=1; 47=2; 48=3; 49=4; 50=5")/5
psych::alpha(with(Data, cbind(rf1t2, rf2t2, rf3t2, rf4t2, rf5t2, rf6t2,
                              rf7t2, rf8t2, rf9t2, rf10t2, rf11t2)))
Data$reft2<-rowMeans(with(Data, cbind(rf1t2, rf2t2, rf3t2, rf4t2, rf5t2, rf6t2,
                                      rf7t2, rf8t2, rf9t2, rf10t2, rf11t2)))

# perceived election unfairness, T2 
Data$ref1t2<-(5-Data$Q132.t2)/4
Data$ref2t2<-(Data$Q133.t2-1)/4
psych::alpha(with(Data, cbind(ref1t2, ref2t2)))
Data$racct22<-rowMeans(with(Data, cbind(ref1t2, ref2t2)))

# perceived count inaccuracy, T2 
Data$ref5t2<-(Data$Q136.t2-1)/4
Data$ref6t2<-Recode(Data$Q137.t2, "23=4; 24=3; 25=2; 26=1; 28=0")/4
psych::alpha(with(Data, cbind(ref5t2, ref6t2)))
Data$rfairt2<-rowMeans(with(Data, cbind(ref5t2, ref6t2)))

# perceived election illegitimacy (composite)
corr.test(with(Data, cbind(reft2, racct22, rfairt2)))
psych::alpha(with(Data, cbind(reft2, racct22, rfairt2)))
Data$rallf2<-rowMeans(with(Data, cbind(reft2, racct22, rfairt2)))

# collective narcissism, T2
Data$rct1<-(Data$Q257_1.t2-1)/6
Data$rct2<-(Data$Q257_2.t2-1)/6
Data$rct3<-(Data$Q257_3.t2-1)/6
Data$rct4<-(Data$Q257_4.t2-1)/6
Data$rct5<-(Data$Q257_5.t2-1)/6
psych::alpha(with(Data, cbind(rct1, rct2, rct3, rct4, rct5)))
Data$rcnt2<-rowMeans(with(Data, cbind(rct1, rct2, rct3, rct4, rct5)))

# ingroup satisfaction / patriotrism, T2
Data$rpa1t2<-(4-Data$Q254.t2)/3
Data$rpa2t2<-(Recode(Data$Q255.t2, "1=0; 2=1; 3=2; 4=3; 8=4"))/4
Data$rpa3t2<-(Recode(Data$Q256.t2, "1=4; 2=3; 3=2; 4=1; 8=0"))/4
psych::alpha(with(Data, cbind(rpa1t2, rpa2t2, rpa3t2)))
Data$rpatt2<-rowMeans(with(Data, cbind(rpa1t2, rpa2t2, rpa3t2)))

#####save dataframe as Rdata
save(Data, file="elec2020.Rdata")

#####load Rdata if coded and saved previously
load("elec2020.RData")

#############################################################################################
###### relations among study variables

### correlations
f <- corr.test(with(Data, cbind(rpatt2, rcnt2, ridt2, reft2, rfairt2, racct22, 
                           rallf2)))

ct<-huxtable(f$r, number_format = 2)
quick_docx(ct, file='ct.docx') 

### CN party contrasts
summary(lm_robust(rcnt2 ~ pid3t2, data=Data, se_type = "HC3"))
summary(lm_robust(rcnt2 ~ rpid3t2, data=Data, se_type = "HC3"))

### IS party contrasts
summary(lm_robust(rpatt2 ~ pid3t2, data=Data, se_type = "HC3"))
summary(lm_robust(rpatt2 ~ rpid3t2, data=Data, se_type = "HC3"))

#############################################################################################
###### ridgeline plot/joyplot of CN and IS by partisanship, color=pid3t2

Data2<-na.omit(subset(Data, select=c(rcnt2, rpatt2, pid3t2)))
f1a <- ggplot(Data2, aes(x = rcnt2, y = pid3t2, fill = pid3t2, color=pid3t2)) + 
  geom_density_ridges(alpha = .40, 
                      from = 0, to = 1, scale = 1.4,
                      color = "white", 
                      size = .75, 
                      show.legend=FALSE, na.rm=F, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) + 
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.75))) +
  theme_ipsum_rc() + 
  theme(aspect.ratio=.50, plot.title = element_text(face="bold", hjust = 0.5),
        panel.grid.minor = element_blank(), 
        #panel.grid.major.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 12, hjust = 1), 
        axis.title.y = element_blank(), 
        panel.border = element_rect(color="black", fill=NA)) +
  scale_fill_cyclical(values = c("#49b7fc", "#17d898", "#ff7b00")) +
  scale_color_cyclical(values = c("#49b7fc", "#17d898", "#ff7b00")) +
  labs(title="Collective Narcissism, by Party",  
       x = "Collective Narcissism (0-1)",
       y = "Party") 

f1b <- ggplot(Data2, aes(x = rpatt2, y = pid3t2, fill = pid3t2, color=pid3t2)) + 
  geom_density_ridges(alpha = .40, 
                      from = 0, to = 1, scale = 1.4,
                      color = "white", 
                      size = .75, 
                      show.legend=FALSE, na.rm=F, quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) + 
  scale_x_continuous(breaks=seq(0,1,0.2)) +
  scale_y_discrete(expand = expansion(mult = c(0.1, 0.75))) +
  theme_ipsum_rc() + 
  theme(aspect.ratio=.50, plot.title = element_text(face="bold", hjust = 0.5),
        panel.grid.minor = element_blank(), 
        #panel.grid.major.y = element_blank(),
        #panel.grid.major.x = element_blank(),
        axis.title.x = element_text(size = 12, hjust = 1), 
        axis.title.y = element_blank(), 
        panel.border = element_rect(color="black", fill=NA)) +
  scale_fill_cyclical(values = c("#49b7fc", "#17d898", "#ff7b00")) +
  scale_color_cyclical(values = c("#49b7fc", "#17d898", "#ff7b00")) +
  labs(title="Ingroup Satisfaction, by Party",  
       x = "Ingroup Satisfaction (0-1)",
       y = "Party") 

fig3 <- (f1a / f1b ) & theme(plot.margin = margin(15, 15, 15, 15))
ggsave(file="effig20-1.png", fig3, width = 10, height = 10, type="cairo")

## "#49b7fc" (blue), "#17d898" (green), "#ff7b00" (red)

#############################################################################################
#### main effects of CN and CN x PID interactions

#############################################################################################
### perceived election fraud
# main effects  
mod1c<-lm_robust(reft2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2, data=Data, se_type = "HC3")
summary(mod1c)

# partisan differences
mod1e<-lm(reft2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
                   rcnt2*pid3t2, data=Data)
coeftest(mod1e, vcovHC(mod1e, type = "HC3"))
sim_slopes(mod1e, pred = rcnt2, modx = pid3t2, robust=T, confint=T)

mod1e2<-lm(reft2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+iind+irep+
            rcnt2*iind+rcnt2*irep, data=Data)
coeftest(mod1e2, vcovHC(mod1e2, type = "HC3"))
sim_slopes(mod1e2, pred = iind, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))
sim_slopes(mod1e2, pred = irep, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))

f2 <- interact_plot(mod1e, pred = rcnt2, modx = pid3t2, interval=T, 
                      int.RRth = 0.95, legend.main = "Partisanship:",
                      colors=c("#49b7fc", "#17d898", "#ff7b00"), robust=T) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),     
        legend.position="none") +
  #geom_rug(data=Data, aes(x=rcnt2, y=reft2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Election Fraud",
       x = "Collective Narcissism (0-1)",
       y = "Perceived Election Fraud (0-1)")

## "#49b7fc" (blue), "#17d898" (green), "#ff7b00" (red)


#############################################################################################
### perceived election unfairness
# main effects  
mod3c<-lm_robust(rfairt2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2, data=Data, se_type = "HC3")
summary(mod3c)

# partisan differences
mod3e<-lm(rfairt2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*pid3t2, data=Data)
coeftest(mod3e, vcovHC(mod3e, type = "HC3"))
sim_slopes(mod3e, pred = rcnt2, modx = pid3t2, robust=T, confint=T)

mod3e2<-lm(rfairt2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+iind+irep+
             rcnt2*iind+rcnt2*irep, data=Data)
coeftest(mod3e2, vcovHC(mod3e2, type = "HC3"))
sim_slopes(mod3e2, pred = iind, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))
sim_slopes(mod3e2, pred = irep, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))

f3 <- interact_plot(mod3e, pred = rcnt2, modx = pid3t2, interval=T, 
                    int.RRth = 0.95, legend.main = "Partisanship:",
                    colors=c("#49b7fc", "#17d898", "#ff7b00"), robust=T) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),        
        legend.position="none") +
  #geom_rug(data=Data, aes(x=rcnt2, y=rfairt2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Election Unfairness",
       x = "Collective Narcissism (0-1)",
       y = "Perceived Election Unfairness (0-1)")

#############################################################################################
### perceived count inaccuracy
# main effects  
mod4c<-lm_robust(racct22 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2, data=Data, se_type = "HC3")
summary(mod4c)

# partisan differences
mod4e<-lm(racct22 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*pid3t2, data=Data)
coeftest(mod4e, vcovHC(mod4e, type = "HC3"))
sim_slopes(mod4e, pred = rcnt2, modx=pid3t2, robust=T, confint=T)

mod4e2<-lm(racct22 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+iind+irep+
             rcnt2*iind+rcnt2*irep, data=Data)
coeftest(mod4e2, vcovHC(mod4e2, type = "HC3"))
sim_slopes(mod4e2, pred = iind, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))
sim_slopes(mod4e2, pred = irep, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))

f4 <- interact_plot(mod4e, pred = rcnt2, modx = pid3t2, interval=T, 
                    int.RRth = 0.95, legend.main = "Partisanship:",
                    colors=c("#49b7fc", "#17d898", "#ff7b00"), robust=T) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),        
        legend.position="none") +
  #geom_rug(data=Data, aes(x=rcnt2, y=racct22), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Inaccuracy of Vote Count",
       x = "Collective Narcissism (0-1)",
       y = "Perceived Inaccuracy of Vote Count (0-1)")

#############################################################################################
### composite election illegitimacy
# main effects  
mod5c<-lm_robust(rallf2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2, data=Data, se_type = "HC3")
summary(mod5c)

# partisan differences
mod5e<-lm(rallf2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*pid3t2, data=Data)
coeftest(mod5e, vcovHC(mod5e, type = "HC3"))
sim_slopes(mod5e, pred = rcnt2, modx=pid3t2, robust=T, confint=T)

mod5e2<-lm(rallf2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+iind+irep+
             rcnt2*iind+rcnt2*irep, data=Data)
coeftest(mod5e2, vcovHC(mod5e2, type = "HC3"))
sim_slopes(mod5e2, pred = iind, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))
sim_slopes(mod5e2, pred = irep, modx = rcnt2, robust=T, confint=T, modx.values=c(0,1))

f5 <- interact_plot(mod5e, pred = rcnt2, modx = pid3t2, interval=T, 
                    int.RRth = 0.95, legend.main = "Partisanship:",
                    colors=c("#49b7fc", "#17d898", "#ff7b00"), robust=T) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA)) +
  #geom_rug(data=Data, aes(x=rcnt2, y=rallf2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Election Illegitimacy (Composite)",
       x = "Collective Narcissism (0-1)",
       y = "Composite Election Illegitimacy (0-1)")

#############################################################################################
######## tables

### main effects
tS1<-huxreg(mod1c, mod3c, mod4c, mod5c,
           statistics = c("N" = "nobs", 
                          "R squared" = "r.squared"),
           number_format = 2)
quick_docx(tS1, file='tS1.docx')

### interactions

i1<-lm_robust(reft2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*pid3t2, data=Data, se_type = "HC3")
i3<-lm_robust(rfairt2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
         rcnt2*pid3t2, data=Data, se_type = "HC3")
i4<-lm_robust(racct22 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
        rcnt2*pid3t2, data=Data, se_type = "HC3")
i5<-lm_robust(rallf2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
         rcnt2*pid3t2, data=Data, se_type = "HC3")

tS2<-huxreg(i1, i3, i4, i5,
           statistics = c("N" = "nobs", 
                          "R squared" = "r.squared"),
           number_format = 2)
quick_docx(tS2, file='tS2.docx')

#############################################################################################
######## figures

### figure 2
sm1 <- tidy(mod1c) %>% filter(term == "rpatt2" | term == "rcnt2" | 
                               term == "ridt2" | term == "pid3t2Independent" |
                                term == "pid3t2Republican/Lean Republican") %>% mutate(model = "Perceived Election Fraud") 
sm2 <- tidy(mod3c) %>% filter(term == "rpatt2" | term == "rcnt2" | 
                                term == "ridt2" | term == "pid3t2Independent" |
                                term == "pid3t2Republican/Lean Republican")  %>% mutate(model = "Perceived Election Unfairness")
sm3 <- tidy(mod4c) %>% filter(term == "rpatt2" | term == "rcnt2" | 
                                term == "ridt2" | term == "pid3t2Independent" |
                                term == "pid3t2Republican/Lean Republican")  %>% mutate(model = "Perceived Count Inaccuracy")
sm4 <- tidy(mod5c) %>% filter(term == "rpatt2" | term == "rcnt2" | 
                                term == "ridt2" | term == "pid3t2Independent" |
                                term == "pid3t2Republican/Lean Republican")  %>% mutate(model = "Perceived Election Illegitimacy (Composite)")
smods <- rbind(sm1, sm2, sm3, sm4)

fig2 <- dwplot(smods, dot_args = list(size = 1.5, shape=21, stroke = 1, color = "red", fill="white"),
              whisker_args = list(size = .5, color = "red")) %>%
  relabel_predictors(c(rpatt2 = "Ingroup\nSatisfaction",
                       rcnt2 = "Collective\nNarcissism",
                       ridt2 = "Ideology\n(High = Consv)",                       
                       "pid3t2Independent" = "Independent",
                       "pid3t2Republican/Lean Republican" = "Republican")) +
  theme_ipsum_rc() + xlab("Coefficient Estimate (OLS)") +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  scale_x_continuous(breaks=seq(-0.4,0.8,0.2), limits = c(-0.4, 0.8)) + 
  #labs(caption="Source: 2020 National Survey") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        legend.position = "none", aspect.ratio=1,
        panel.border = element_rect(color="black", fill=NA),
        strip.text = element_text(hjust=0.5), axis.title.x = element_text(size = 12)) +
  facet_wrap(~model, scales = "free_x", ncol = 2) 
ggsave(file="effig20-2.png", fig2, width = 10, height = 10, type="cairo")

### figure 3
fig3 <- (f2 | f3 ) / (f4 | f5) + plot_layout(guides = 'collect') & 
  theme(plot.margin = margin(15, 15, 15, 15), 
        legend.position="bottom",         
        legend.box.background = element_rect(color = "black"),
        legend.text=element_text(size=12), legend.title = element_text(face = "bold")) 
ggsave(file="effig20-3.png", fig3, width = 10, height = 11, type="cairo")

#############################################################################################
###### CN x ideology
#############################################################################################

### perceived election fraud
mod1f<-lm(reft2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*ridt2, data=Data)
coeftest(mod1f, vcovHC(mod1f, type = "HC3"))

sim_slopes(mod1f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
           robust=T, confint=T)

f2i <- interact_plot(mod1f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
                     interval=T, int.RRth = 0.95, legend.main = "Ideology:",
                     colors="CUD Bright", robust=T, 
                     modx.labels = c("Liberal (-1 SD)", "Conservative (+1 SD)")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),        
        legend.position="none") +
  #geom_rug(data=Data, aes(x=rcnt2, y=reft2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Election Fraud",
       x = "Collective Narcissism (0-1)",
       y = "Perceived Election Fraud (0-1)")

### perceived election unfairness
mod3f<-lm(rfairt2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*ridt2, data=Data)
coeftest(mod3f, vcovHC(mod3f, type = "HC3"))

sim_slopes(mod3f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
           robust=T, confint=T)

f3i <- interact_plot(mod3f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
                     interval=T, int.RRth = 0.95, legend.main = "Ideology:",
                     colors="CUD Bright", robust=T, 
                     modx.labels = c("Liberal (-1 SD)", "Conservative (+1 SD)")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),        
        legend.position="none") +
  #geom_rug(data=Data, aes(x=rcnt2, y=reft2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Election Unfairness",
       x = "Collective Narcissism (0-1)",
       y = "Perceived Election Unfairness (0-1)")

### perceived count inaccuracy
mod4f<-lm(racct22 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*ridt2, data=Data)
coeftest(mod4f, vcovHC(mod4f, type = "HC3"))

sim_slopes(mod4f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
           robust=T, confint=T)

f4i <- interact_plot(mod4f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
                     interval=T, int.RRth = 0.95, legend.main = "Ideology:",
                     colors="CUD Bright", robust=T, 
                     modx.labels = c("Liberal (-1 SD)", "Conservative (+1 SD)")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),        
        legend.position="none") +
  #geom_rug(data=Data, aes(x=rcnt2, y=reft2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Inaccuracy of Vote Count",
       x = "Collective Narcissism (0-1)",
       y = "Perceived Inaccuracy of Vote Count (0-1)")

### composite election illegitimacy
mod5f<-lm(rallf2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
            rcnt2*ridt2, data=Data)
coeftest(mod5f, vcovHC(mod5f, type = "HC3"))

sim_slopes(mod5f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
           robust=T, confint=T)

f5i <- interact_plot(mod5f, pred = rcnt2, modx = ridt2, modx.values=c(0.1757509, 0.7800957),
                     interval=T, int.RRth = 0.95, legend.main = "Ideology:",
                     colors="CUD Bright", robust=T, 
                     modx.labels = c("Liberal (-1 SD)", "Conservative (+1 SD)")) +
  scale_y_continuous(breaks=seq(0,1,0.2), limits=c(0,1)) +  
  scale_x_continuous(breaks=seq(0,1,0.2)) + 
  theme_ipsum_rc() + 
  theme(aspect.ratio=1, plot.title = element_text(size = 13, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12, hjust = 0.5), 
        axis.title.y = element_text(size = 12, hjust = 0.5), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA)) +
  #geom_rug(data=Data, aes(x=rcnt2, y=rallf2), sides="b", alpha=0.25, color='grey50', 
  #         position = "jitter", inherit.aes = F, length = unit(0.015, "npc")) + 
  labs(title = "Perceived Election Illegitimacy (Composite)",
       x = "Collective Narcissism (0-1)",
       y = "Composite Election Illegitimacy (0-1)")

### figure 4
fig4 <- (f2i | f3i) / (f4i | f5i ) + plot_layout(guides = 'collect') & 
  theme(plot.margin = margin(15, 15, 15, 15), 
        legend.position="bottom",         
        legend.box.background = element_rect(color = "black"),
        legend.text=element_text(size=12), legend.title = element_text(face = "bold")) 
ggsave(file="effig20-4.png", fig4, width = 10, height = 11, type="cairo")

### table S3
mod1f<-lm_robust(reft2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
                   rcnt2*ridt2, data=Data, se_type = "HC3")
mod3f<-lm_robust(rfairt2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
                   rcnt2*ridt2, data=Data, se_type = "HC3")
mod4f<-lm_robust(racct22 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
                   rcnt2*ridt2, data=Data, se_type = "HC3")
mod5f<-lm_robust(rallf2 ~ rpatt2+rcnt2+age+gend+whitet1+rinct1+educ+ridt2+pid3t2+
                   rcnt2*ridt2, data=Data, se_type = "HC3")

tS3<-huxreg(mod1f, mod3f, mod4f, mod5f,
            statistics = c("N" = "nobs", 
                           "R squared" = "r.squared"),
            number_format = 2)
quick_docx(tS3, file='tS3.docx')

