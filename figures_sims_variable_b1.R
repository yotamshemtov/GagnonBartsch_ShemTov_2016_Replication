
rm(list=ls())

# Libraries
library(dplyr)
library(tidyr)
library(ggplot2)

setwd("~/Dropbox/Class.test2/Rcode/analyses/sims/multiple_b1_no_forest/")

load("results_clean.rda")

results01 = results05 = results

results01 = (results01<=0.01)*1
results05 = (results05<=0.05)*1

results01 = apply(results01,c(1,2),mean)
results05 = apply(results05,c(1,2),mean)

beta1 = (0:15)/20

#######################################
# Figure 5% significant level
#######################################

dp = as.data.frame(results05[,c("crossmatch","energy","cpt.log2.rate")])
dp = rename(dp,logistic2 = cpt.log2.rate)
dp$beta1 = beta1
df=gather(dp,value="power",key="test.type", -beta1)
df$test.type = as.factor(df$test.type)
levels(df$test.type) = c(" Cross-Match  "," Energy  "," CPT (Logistic2)  ")

fig05 <- ggplot(df,aes(y=power,x=beta1,col=test.type, shape=test.type))+
  geom_point(size=4)+geom_line(size=1.2)+
  labs(
    title="Significance level = 0.05",
    y = "Power",
    x = expression(rho))+
  #theme_economist()+
  theme_bw()+ 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5))+
  theme(axis.text.y = element_text(colour="grey20",size=16))+
  theme(axis.title.y = element_text(colour="grey20",size=17))+
  theme(axis.title.x = element_text(colour="grey20",size=22))+
  theme(panel.border = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8))+
  theme(title = element_text(size=15))+
  scale_colour_grey()+ 
  guides(col=guide_legend(title=""),shape=guide_legend(title=""))+ # adding legend title
  theme(legend.position="bottom", 
        legend.text = element_text(size=15)) # legend positvjust=0.5ion




setwd("~/Dropbox/Class.test2/figures/")
ggsave(fig05, file = "power05.pdf" )



#######################################
# Figure 1% significant level
#######################################

dp = as.data.frame(results01[,c("crossmatch","energy","cpt.log2.rate")])
dp = rename(dp,logistic2 = cpt.log2.rate)
dp$beta1 = beta1
df=gather(dp,value="power",key="test.type", -beta1)
df$test.type = as.factor(df$test.type)
levels(df$test.type) = c(" Cross-Match  "," Energy  "," CPT (Logistic2)  ")

fig01 <- ggplot(df,aes(y=power,x=beta1,col=test.type, shape=test.type))+
  geom_point(size=4)+geom_line(size=1.2)+
  labs(
    title="Significance level = 0.01",
    y = "power",
    x = expression(rho))+
  #theme_economist()+
  theme_bw()+ 
  theme(axis.text.x = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=.5))+
  theme(axis.text.y = element_text(colour="grey20",size=16))+
  theme(axis.title.y = element_text(colour="grey20",size=17))+
  theme(axis.title.x = element_text(colour="grey20",size=22))+
  theme(panel.border = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8))+
  theme(title = element_text(size=15))+
  scale_colour_grey()+ 
  guides(col=guide_legend(title=""),shape=guide_legend(title=""))+ # adding legend title
  theme(legend.position="bottom", 
        legend.text = element_text(size=15)) # legend positvjust=0.5ion

ggsave(fig05, file = "power01.pdf" )





