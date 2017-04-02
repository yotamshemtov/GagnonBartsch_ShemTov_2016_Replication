rm(list=ls())

# Libraries
#library(reshape)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Define local directory:
user=""
if (user==""){
  dir.dataReal <- "~/Dropbox/Class.test2/user/user/analyses/real_data"
  dir.dataSim <- "~/Dropbox/Class.test2/Rcode/analyses/sims"
  dir.figures <- ""
}

####################
# Figures fixed_b1
####################

# parameters:
alpha = seq(0,1,length=100)  # flase positive rate

# Loading the data:
load(paste(dir.dataSim,"/fixed_b1/sim3.rda",sep=""))


###########################################################################################################################

##################      Comparing in-sample and out-sample classification accuracy measures          ######################

# P-value data
data.pv <- data.frame(
  logistic.interactions = sapply(as.list(c(1:length(simanalysis))),
                                 function(x){return(simanalysis[[x]]$logistic2$rate$pval)}),
  logistic2.mixed = sapply(as.list(c(1:length(simanalysis))),
                           function(x){return(simanalysis[[x]]$logistic2_5fold$rate$pval)})
)

# data for comparing different classifiers with a fixed beta:
data.roc <- data.frame(false.positive = seq(0,0.2,length=100),
                       logistic2 = sapply(alpha,function(x){mean(data.pv$logistic.interactions<x)}),
                       logistic2.mixed = sapply(alpha,function(x){mean(data.pv$logistic2.mixed<x)})
)
dp <- gather(data.roc, value="true.positive",key="classifier", -false.positive)
levels(dp$classifier) = c("Logistic2 (in-sample)", "Logistic2 (out-sample)")

p <- ggplot(dp,aes(x=false.positive,y=true.positive,col=classifier, shape = classifier))+
  geom_point(size=3)+
  geom_line(size=1)+
  xlim(0,0.2)+
  labs(
    title=expression(paste(rho," = 0.5")),
    y = "True positive rate",
    x = expression(paste("False positive rate (",alpha,")")))+
  theme_bw()+ 
  theme(axis.text.x = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=.5))+
  theme(axis.text.y = element_text(colour="grey20",size=14))+
  theme(axis.title.y = element_text(colour="grey20",size=15))+
  theme(axis.title.x = element_text(colour="grey20",size=25))+
  theme(panel.border = element_blank())+
  theme(axis.line.x = element_line(color="black", size = 0.8),
        axis.line.y = element_line(color="black", size = 0.8))+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(title = element_text(size=15))+
  scale_colour_grey( start = 0, end = 0.5 )+ 
  guides(col=guide_legend(title=""),shape=guide_legend(title=""))+ # adding legend title
  theme(legend.position="bottom", 
        legend.text = element_text(size=11)) # legend positvjust=0.5ion


setwd("~/Dropbox/Class.test2/figures/")
ggsave(p, file = "roc_in_vs_out_sample_fig_rho50.pdf")































