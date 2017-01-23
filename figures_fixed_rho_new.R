
rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape)
library(ggplot2)

# load simulation data (results)
load("~/Dropbox/Class.test2/Rcode/analyses/sims/multiple_b1_no_forest/results200.rda") # data with using Random Forest as classifier

########################################################
# ROC figures for simulation results - n = 200
########################################################

# parameters:
alpha = seq(0,1,length=100)  # flase positive rate

methods = c("crossmatch", "energy", "cpt.forest.rate", "cpt.log2.rate")

f.data = function(results, rho.level ){
  
  beta = (0:15)/20
  index.rho.level = which(beta==rho.level)
  
  data.roc <- data.frame(false.positive = seq(0,1,length=100),
                         forest = sapply(alpha,function(x){mean( results[ index.rho.level,"cpt.forest.rate",] < x )} ),
                         logistic2 = sapply(alpha,function(x){mean( results[ index.rho.level,"cpt.log2.rate",] < x )} ),
                         logistic = sapply(alpha,function(x){mean( results[ index.rho.level,"cpt.log.rate",] < x )} ),
                         energy = sapply(alpha,function(x){mean( results[ index.rho.level,"energy",] < x )} ),
                         crossmatch = sapply(alpha,function(x){mean( results[ index.rho.level,"crossmatch",] < x )} )
  )
  
  dp <- reshape::melt(data.roc,id.vars="false.positive",variable_name="Test")
  dp = dplyr::rename(dp, true.positive=value)
  levels(dp$Test) <- c("CPT (Forest) ","CPT (Logistic2)","CPT (Logistic)","Energy","Cross-Match")
  
  return(dp)
  
}

###########################
### Figure rho = 0.25
###########################

dp = f.data(results, rho.level = 0.25)

### Figure 1 - rho = 0.25 ############
p <- ggplot(dp,aes(x=false.positive,y=true.positive,col=Test,shape=Test))+
  geom_point(size=2)+
  geom_line(size=1)+
  labs(
    title=expression(paste(rho," = 0.25")),
    y = "True positive rate",
    x = expression(paste("False positive rate (",alpha,")")))+
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
        legend.text = element_text(size=11)) # legend positvjust=0.5ion

setwd("~/Dropbox/Class.test2/figures/")
pdf("rocfig1_25.pdf")
print(p)
dev.off()



###########################
### Figure rho = 0.5
###########################

dp = f.data(results, rho.level = 0.5)

### Figure 1 - rho = 0.5 ############
p <- ggplot(dp,aes(x=false.positive,y=true.positive,col=Test,shape=Test))+
  geom_point(size=2)+
  geom_line(size=1)+
  labs(
    title=expression(paste(rho," = 0.5")),
    y = "True positive rate",
    x = expression(paste("False positive rate (",alpha,")")))+
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
        legend.text = element_text(size=11)) # legend positvjust=0.5ion

setwd("~/Dropbox/Class.test2/figures/")
pdf("rocfig1_50.pdf")
print(p)
dev.off()













