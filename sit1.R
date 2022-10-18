#############################################################################################
##### START HERE:

#####load Rdata into dataframe of same name:
load("elecfair.RData")

####packages.
library(car)
library(psych)
library(ggplot2)
library(estimatr)
library(summarytools)
library(tidyverse)
library(sandwich)
library(jtools)
library(interactions)
library(patchwork)
library(dotwhisker)
library(hrbrthemes)
library(dplyr)
library(broom)
library(lemon)
library(effects)
library(margins)
library(mediation)
library(lmtest)
library(ordinal)
library(ggeffects)

## run to use hrbrthemes, follow instructions to install --
## sometimes need a restart of R after this to get fonts:
import_roboto_condensed()

options(scipen = 0, show.signif.stars=TRUE)

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
###### recoding and renaming

# demographics
# age 
Data$age<-Data$age.r

# gender
Data$gend<-Data$gender_dummy.r

# income
Data$rinct1<-ifelse(Data$income.r<12, (Data$income.r-1)/10, NA)

# education
Data$college<-ifelse(Data$education.r<7, 0, 1)
Data$educ<-Recode(Data$education.r, "1:3=0; 4=1; 5:6=2; 7:8=3; 9:14=4")/4

# white
Data$whitet1<-ifelse(Data$ethnicity.r==1 & Data$hispanic.r==2, 1, 0) 

# ideology, T1 and T2
Data$ridt1<-(Data$Ideo1.t1-1)/6
Data$ridt2<-(Data$Ideo1.t2-1)/6

# ideologiCal extremity
Data$ridext1<-std01(abs(Data$ridt1-.5))
Data$ridext2<-std01(abs(Data$ridt2-.5))

# presidential therms
Data$ttt2<-as.numeric(Recode(Data$Q88_5.t2, "1=0; 2=.1; 16=.2; 17=.3; 18=.4; 19=.5; 
                  20=.6; 21=.7; 22=.8; 23=.9; 53=1", as.numeric = F))
Data$btt2<-as.numeric(Recode(Data$Q88_6.t2, "1=0; 2=.1; 16=.2; 17=.3; 18=.4; 19=.5; 
                  20=.6; 21=.7; 22=.8; 23=.9; 53=1", as.numeric = F))
Data$pdift2<-std01(Data$ttt2-Data$btt2)

# pid, T1 and T2
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

# system identity threat, T1 and T2
Data$rs1t1<-(Data$SI1_1.t1-1)/6
Data$rs2t1<-(Data$SI1_2.t1-1)/6
Data$rs3t1<-(7-Data$SI1_3.t1)/6
Data$rs4t1<-(Data$SI1_4.t1-1)/6
Data$rs5t1<-(Data$SI1_5.t1-1)/6
psych::alpha(with(Data, cbind(rs1t1, rs2t1, rs3t1, rs4t1, rs5t1)))
Data$rsit1<-rowMeans(with(Data, cbind(rs1t1, rs2t1, rs3t1, rs4t1, rs5t1)))
Data$rs1t2<-(Data$SI1_1.t2-1)/6
Data$rs2t2<-(Data$SI1_2.t2-1)/6
Data$rs3t2<-(7-Data$SI1_3.t2)/6
Data$rs4t2<-(Data$SI1_4.t2-1)/6
Data$rs5t2<-(Data$SI1_5.t2-1)/6
psych::alpha(with(Data, cbind(rs1t2, rs2t2, rs3t2, rs4t2, rs5t2)))
Data$rsit2<-rowMeans(with(Data, cbind(rs1t2, rs2t2, rs3t2, rs4t2, rs5t2)))

# knowledge
Data$kn1<-ifelse(Data$PK1==5, 1, 0)
Data$kn2<-ifelse(Data$PK2==2, 1, 0)
Data$kn3<-ifelse(Data$PK3==2, 1, 0)
Data$kn4<-ifelse(Data$PK4==4, 1, 0)
Data$kn5<-ifelse(Data$PK5==2, 1, 0)
Data$kn6<-ifelse(Data$PK6==4, 1, 0)
Data$kn7<-ifelse(Data$PK7==1, 1, 0)
Data$kn8<-ifelse(Data$PK8==1, 1, 0)
psych::alpha(with(Data, cbind(kn1, kn2, kn3, kn4, kn5, kn6, kn7, kn8)))
Data$rknt1<-rowMeans(with(Data, cbind(kn1, kn2, kn3, kn4, kn5, kn6, kn7, kn8)))

# perceived election fraud, T1 and T2
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

# perceived election accuracy and unfairness, T1 and T2 
Data$ref1t2<-(5-Data$Q132.t2)/4
Data$ref2t2<-(Data$Q133.t2-1)/4
Data$ref5t2<-(Data$Q136.t2-1)/4
Data$ref6t2<-Recode(Data$Q137.t2, "23=4; 24=3; 25=2; 26=1; 28=0")/4
corr.test(with(Data, cbind(ref1t2, ref2t2, ref5t2, ref6t2)))
psych::alpha(with(Data, cbind(ref1t2, ref2t2, ref5t2, ref6t2 )))
psych::alpha(with(Data, cbind(ref1t2, ref2t2)))
psych::alpha(with(Data, cbind(ref5t2, ref6t2)))

Data$racct22<-rowMeans(with(Data, cbind(ref1t2, ref2t2)))
Data$rfairt2<-rowMeans(with(Data, cbind(ref5t2, ref6t2)))
Data$racft22<-rowMeans(with(Data, cbind(ref1t2, ref2t2, ref5t2, ref6t2)))

# perceived election illegitimacy (composite)
corr.test(with(Data, cbind(reft2, racct22, rfairt2)))
psych::alpha(with(Data, cbind(reft2, racct22, rfairt2)))
Data$rallf2<-rowMeans(with(Data, cbind(reft2, racct22, rfairt2)))

# support for democratic norms, T1 and T2
# 'f' version includes one Bartels item w/ content overlap with SI threat
# dropped this item for main analysis to avoid tautology
Data$rpr1<-6-Data$Q110.t1
Data$rpr2<-6-Data$Q110.t2
psych::alpha(with(Data, cbind(rpr1, Q111.t1, Q112.t1, Q113.t1, Q114.t1)))
psych::alpha(with(Data, cbind(rpr2, Q111.t2, Q112.t2, Q113.t2, Q114.t2)))
Data$rrdt1<-std01(rowMeans(with(Data, cbind(rpr1, Q112.t1, Q113.t1, Q114.t1))))
Data$rrdt2<-std01(rowMeans(with(Data, cbind(rpr2, Q112.t2, Q113.t2, Q114.t2))))
Data$rrdt1f<-std01(rowMeans(with(Data, cbind(rpr1, Q111.t1, Q112.t1, Q113.t1, Q114.t1))))
Data$rrdt2f<-std01(rowMeans(with(Data, cbind(rpr2, Q111.t2, Q112.t2, Q113.t2, Q114.t2))))

# rejection of democratic norms, T1 and T2
# 'f' version includes one Bartels item w/ content overlap with SI threat
# dropped this item for main analysis to avoid tautology
Data$rrrdt1<-1-Data$rrdt1
Data$rrrdt2<-1-Data$rrdt2
Data$rrrdt1f<-1-Data$rrdt1f
Data$rrrdt2f<-1-Data$rrdt2f

# support for military coup, T1 and T2
psych::alpha(with(Data, cbind(Q115_1.t1, Q115_2.t1, Q115_3.t1)))
psych::alpha(with(Data, cbind(Q115_1.t2, Q115_2.t2, Q115_3.t2)))
Data$rcoupt1<-std01(rowMeans(with(Data, cbind(Q115_1.t1, Q115_2.t1, Q115_3.t1))))
Data$rcoupt2<-std01(rowMeans(with(Data, cbind(Q115_1.t2, Q115_2.t2, Q115_3.t2))))

# support for insurrection, T1 and T2
psych::alpha(with(Data, cbind(Q116_1.t1, Q116_2.t1, Q116_3.t1)))
psych::alpha(with(Data, cbind(Q116_1.t2, Q116_2.t2, Q116_3.t2)))
Data$rinst1<-std01(rowMeans(with(Data, cbind(Q116_1.t1, Q116_2.t1, Q116_3.t1))))
Data$rinst2<-std01(rowMeans(with(Data, cbind(Q116_1.t2, Q116_2.t2, Q116_3.t2))))

# anti-democratic attitudes (composite)
corr.test(with(Data, cbind(rrrdt2, rcoupt2, rinst2)))
psych::alpha(with(Data, cbind(rrrdt2, rcoupt2, rinst2)))
Data$radt2<-rowMeans(with(Data, cbind(reft2, racct22, rfairt2)))

# conspiratorial ideation
Data$rci1t1<-(Data$CP1_1.t1-8)/4
Data$rci2t1<-(Data$CP1_2.t1-8)/4
Data$rci3t1<-(Data$CP1_3.t1-8)/4
Data$rci4t1<-(Data$CP1_4.t1-8)/4
psych::alpha(with(Data, cbind(rci1t1, rci2t1, rci3t1, rci4t1)))
Data$rcidt1<-rowMeans(with(Data, cbind(rci1t1, rci2t1, rci3t1, rci4t1)))
Data$rci1t2<-(Data$CP1_1.t2-8)/4
Data$rci2t2<-(Data$CP1_2.t2-8)/4
Data$rci3t2<-(Data$CP1_3.t2-8)/4
Data$rci4t2<-(Data$CP1_4.t2-8)/4
psych::alpha(with(Data, cbind(rci1t2, rci2t2, rci3t2, rci4t2)))
Data$rcidt2<-rowMeans(with(Data, cbind(rci1t2, rci2t2, rci3t2, rci4t2)))

# collective narcissism, T2
Data$rct1<-(Data$Q257_1.t2-1)/6
Data$rct2<-(Data$Q257_2.t2-1)/6
Data$rct3<-(Data$Q257_3.t2-1)/6
Data$rct4<-(Data$Q257_4.t2-1)/6
Data$rct5<-(Data$Q257_5.t2-1)/6
psych::alpha(with(Data, cbind(rct1, rct2, rct3, rct4, rct5)))
Data$rcnt2<-rowMeans(with(Data, cbind(rct1, rct2, rct3, rct4, rct5)))

# patriotism, T1 and T2
Data$rpa1t1<-(4-Data$Q254.t1)/3
Data$rpa2t1<-(Recode(Data$Q255.t1, "1=0; 2=1; 3=2; 4=3; 8=4"))/4
Data$rpa3t1<-(Recode(Data$Q256.t1, "1=4; 2=3; 3=2; 4=1; 8=0"))/4
psych::alpha(with(Data, cbind(rpa1t1, rpa2t1, rpa3t1)))
Data$rpatt1<-rowMeans(with(Data, cbind(rpa1t1, rpa2t1, rpa3t1)))
Data$rpa1t2<-(4-Data$Q254.t2)/3
Data$rpa2t2<-(Recode(Data$Q255.t2, "1=0; 2=1; 3=2; 4=3; 8=4"))/4
Data$rpa3t2<-(Recode(Data$Q256.t2, "1=4; 2=3; 3=2; 4=1; 8=0"))/4
psych::alpha(with(Data, cbind(rpa1t2, rpa2t2, rpa3t2)))
Data$rpatt2<-rowMeans(with(Data, cbind(rpa1t2, rpa2t2, rpa3t2)))

# national ID, T1 and T2
Data$rnatt1<-Data$National_ID_T1
Data$rnatt2<-Data$National_ID_T2

# white ID, T1 & T2
Data$rwhidt1 <- Data$White_ID_T1
Data$rwhidt2 <- Data$White_ID_T2

# dissatisfaction with democracy, 0-1:
Data$rdsat2<-(Data$Q249.t2-1)/3

# dissatisfaction with democracy, 0-3:
Data$dsat2<-factor((Data$Q249.t2-1), labels = c("Very Satisfied", "Fairly Satisfied", 
                                                "Not Very Satisfied", "Not at all Satisfied"))
  
# covid behavior scale
Data$rcobt2<-Data$Covid_BehaviorT2
Data$cb1t2<-ifelse(Data$Q109_1.t2==1, 1, 0) 
Data$cb2t2<-ifelse(Data$Q109_2.t2==1, 1, 0)
Data$cb3t2<-ifelse(Data$Q109_3.t2==1, 1, 0) 
Data$cb4t2<-ifelse(Data$Q109_4.t2==1, 1, 0) 
Data$cb5t2<-ifelse(Data$Q109_5.t2==1, 1, 0) 
Data$cb6t2<-ifelse(Data$Q109_6.t2==1, 1, 0) 
Data$cb7t2<-ifelse(Data$Q109_7.t2==1, 1, 0) 
Data$cb8t2<-ifelse(Data$Q109_8.t2==1, 1, 0)
Data$rcbt2<-rowMeans(with(Data, cbind(cb1t2, cb2t2, cb3t2, cb4t2, cb5t2, cb6t2, cb7t2, cb8t2)))
psych::alpha(with(Data, cbind(cb1t2, cb2t2, cb3t2, cb4t2, cb5t2, cb6t2, cb7t2, cb8t2)))

#####save dataframe as Rdata
save(Data, file="elecfair.Rdata")

#############################################################################################
######### system identity threat models
#############################################################################################

###### election fairness
###### SI threat positively predicts all, net of controls, as predicted

### perceived election fraud 
sm1a<-lm_robust(reft2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm1a)
sm1b<-lm_robust(reft2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm1b)

### perceived election unfairness 
sm2a<-lm_robust(rfairt2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm2a)
sm2b<-lm_robust(rfairt2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm2b)

### perceived count inaccuracy 
sm3a<-lm_robust(racct22 ~ rsit2, data=Data, se_type = "HC3")
summary(sm3a)
sm3b<-lm_robust(racct22 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm3b)

### composite election illegitimacy 
sm4a<-lm_robust(rallf2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm4a)
sm4b<-lm_robust(rallf2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm4b)

### figure combining all four DVs:
sm1 <- tidy(sm1b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican" |
                               term == "rsit2") %>% mutate(model = "Perceived Election Fraud") 
sm2 <- tidy(sm2b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican" |
                               term == "rsit2") %>% mutate(model = "Perceived Election Unfairness")
sm3 <- tidy(sm3b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican" |
                               term == "rsit2") %>% mutate(model = "Perceived Count Inaccuracy")
sm4 <- tidy(sm4b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican"|
                               term == "rsit2") %>% mutate(model = "Perceived Election Illegitimacy (Composite)")
smods <- rbind(sm1, sm2, sm3, sm4)

st1 <- dwplot(smods, dot_args = list(size = 1.5, shape=21, stroke = 1, color = "red", fill="white"),
                whisker_args = list(size = .5, color = "red")) %>%
  relabel_predictors(c(rsit2 = "System\nIdentity Threat", 
                       ridt2 = "Ideology\n(High = Consv)",                       
                       "rpid3t2Democrat/Lean Democrat" = "Democrat",
                       "rpid3t2Republican/Lean Republican" = "Republican")) +
  theme_ipsum_rc() + xlab("Coefficient Estimate (OLS)") +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  scale_x_continuous(breaks=seq(-0.4,0.8,0.2), limits = c(-0.4, 0.8)) + 
  labs(title="Discontent with the 2020 Election", 
       caption="Source: 2020 National Survey") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        legend.position = "none", aspect.ratio=1,
        panel.border = element_rect(color="black", fill=NA),
        strip.text = element_text(hjust=0.5)) +
  facet_wrap(~model, scales = "free_x", ncol = 2) 
ggsave(file="sitfig1.png", st1, width = 12, height = 10, type="cairo")

#############################################################################################
###### antidemocratic attitudes
###### SI threat positively predicts all, net of controls, as predicted

### rejection of democratic norms
sm5a<-lm_robust(rrrdt2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm5a)
sm5b<-lm_robust(rrrdt2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm5b)

### support for coup 
sm6a<-lm_robust(rcoupt2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm6a)
sm6b<-lm_robust(rcoupt2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm6b)

### support for insurrection 
sm7a<-lm_robust(rinst2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm7a)
sm7b<-lm_robust(rinst2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm7b)

### composite anti-democratic attitudes
sm8a<-lm_robust(radt2 ~ rsit2, data=Data, se_type = "HC3")
summary(sm8a)
sm8b<-lm_robust(radt2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm8b)

sm9b<-lm_robust(rdsat2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, se_type = "HC3")
summary(sm9b)


### figure combining all DVs:
sm1 <- tidy(sm5b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican" |
                               term == "rsit2") %>% mutate(model = "Rejection of Democratic Norms") 
sm2 <- tidy(sm6b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican"|
                               term == "rsit2") %>% mutate(model = "Support for Coup")
sm3 <- tidy(sm7b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican"|
                               term == "rsit2") %>% mutate(model = "Support for Insurrection")
sm4 <- tidy(sm8b) %>% filter(term == "ridt2" | term == "rpid3t2Democrat/Lean Democrat" |
                               term == "rpid3t2Republican/Lean Republican"|
                               term == "rsit2") %>% mutate(model = "Anti-Democratic Attitudes (Composite)")
smods <- rbind(sm1, sm2, sm3, sm4)

st2 <- dwplot(smods, dot_args = list(size = 1.5, shape=21, stroke = 1, color = "red", fill="white"),
              whisker_args = list(size = .5, color = "red")) %>%
  relabel_predictors(c(rsit2 = "System\nIdentity Threat", 
                       ridt2 = "Ideology\n(High = Consv)",                       
                       "rpid3t2Democrat/Lean Democrat" = "Democrat",
                       "rpid3t2Republican/Lean Republican" = "Republican")) +
  theme_ipsum_rc() + xlab("Coefficient Estimate (OLS)") +
  geom_vline(xintercept = 0, colour = "black", linetype = 2) +
  scale_x_continuous(breaks=seq(-0.4,0.8,0.2), limits = c(-0.4, 0.8)) + 
  labs(title="Anti-Democratic Attitudes", 
       caption="Source: 2020 National Survey") +
  theme(plot.title = element_text(size=14, hjust=0.5),
        legend.position = "none", aspect.ratio=1,
        panel.border = element_rect(color="black", fill=NA),
        strip.text = element_text(hjust=0.5)) +
  facet_wrap(~model, scales = "free_x", ncol = 2) 
ggsave(file="sitfig2.png", st2, width = 12, height = 10, type="cairo")

#############################################################################################
###### general dissatisfaction with democracy
###### SI threat positively predicts this, net of controls, as predicted

sm9a<-clm(dsat2 ~ rsit2, data=Data, link="logit")
summary(sm9a)
sm9b<-clm(dsat2 ~ rsit2+age+gend+whitet1+rinct1+educ+ridt2+rpid3t2, data=Data, link="logit")
summary(sm9b)

# get predictions, reorder levels, and relabel levels
mydf <- ggeffect(sm9b, terms = "rsit2 [all]")
mydf$response.level <- factor(mydf$response.level, levels = c("Very.Satisfied", "Fairly.Satisfied", 
                                                             "Not.Very.Satisfied", "Not.at.all.Satisfied"))
mydf$response.level <- factor(mydf$response.level, labels = c("Very Satisfied", "Fairly Satisfied", 
                                                              "Not Very Satisfied", "Not at all Satisfied"))

# plot
st3 <- ggplot(mydf, aes(x, predicted)) +
  geom_line(aes(color=factor(response.level), 
                linetype=factor(response.level)), size=1) +  
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill=factor(response.level)), alpha = .2, linetype = 0) +  
  scale_x_continuous(breaks=seq(0,1,0.2), limits = c(0, 1)) + 
  scale_y_continuous(breaks=seq(0,1,0.2), limits = c(0, 1)) +
  theme_ipsum_rc() +

  guides(fill = guide_legend("Satisfaction Level"), 
  linetype = guide_legend("Satisfaction Level"),
  color = guide_legend("Satisfaction Level"))  + 
  theme(aspect.ratio=1, plot.title = element_text(size = 14, face="bold", hjust = .5), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12), 
        panel.grid.major =  element_line(colour = "grey90", size = 0.2), 
        panel.grid.minor =  element_line(colour = "grey98", size = 0.5),
        panel.border = element_rect(color="black", fill=NA),
        legend.key.width = unit(1,"cm")) +
  geom_rug(inherit.aes = T, aes(x=x, y=0), sides="b", alpha=0.5, color='grey50', 
           position = "jitter",  length = unit(0.015, "npc")) +  
  labs(title = "General Dissatisfaction with Democracy", 
       x = "Social Identity Threat (0-1)",
       y = "Pr(Response)") 
ggsave(file="sitfig3.png", st3, width = 10, height = 8, type="cairo")

#############################################################################################
##### original data prep

####clear everything.
rm(list=ls())

#####set directory.
# setwd("F:/electoral fairness/data/")

#####load data from Stata.
library(haven)
comb<-read_dta("Elect Legit Combined Nat Rep.dta")
raw1<-read_dta("rawT1.dta")
raw2<-read_dta("rawT2.dta")  

#####merge dataframes:
## .t1 = time 1 value of variable, .t2 = value of variable
raw <- merge(raw1, raw2, by="PID", all=T, suffixes = c(".t1",".t2"))
## .r = value from 'raw', .c = value from 'comb'
elec <- merge(raw, comb, by="PID", all=T, suffixes = c(".r",".c"))
Data <- elec
