########################################################
# Layell (2009) data application
# Date: March 19, 2017
#
########################################################

rm(list=ls())

# Libraries
library(ggplot2)
library(cpt.paper)
library(cpt)
library(xtable)

########################################
# Load data and data descriptives
########################################

data("cpt.violence")

cov = c("lpop2000","poverty","tariq","lelev","iso",
        "lnn","garrison","reb")
# checks:
stopifnot( sum( ! cpt.violence$rawdata[, cov] == cpt.violence$Z) == 0 )

# These covariates have been chosen to illustrate a scenario in which the CPT can detect imbalance in observed characteristics 
# that is not observed by either looking on the marginal distributions or by conducting a standard F-test.

### Descriptives

#### Balance in each covariate

f.stat = function(x1,x0){
  #sd.bias = summary(x1-x0)[4]/sd(c(x1,x0),na.rm=T)
  ks = ks.test(x1,x0)$p.value
  ttest = t.test(x1,x0)$p.value
  wilcox = wilcox.test(x1,x0)$p.value
  return(c(mean(x1,na.rm=T),mean(x0,na.rm=T),ttest,wilcox,ks))
}

tab = t(mapply(f.stat,as.list(data.frame(cpt.violence$Z[cpt.violence$T=="1",])),as.list(data.frame(cpt.violence$Z[cpt.violence$T=="0",]))))
tab = round(tab,dig=3)
colnames(tab) = c("Ave. Treat","Ave. control","T-test","Wilcoxon","KS")
rownames(tab) = c("log-Population","Poverty","Tariqa","log-Elevation","Isolation","log distance to Neighbor","Garrison","Rebel")
xtable(tab,caption="Balance table", dig=3)

# F-test

dp = as.data.frame(cpt.violence$Z)
dp$T = cpt.violence$T=="1"

ols1=lm(T~(.),data=dp)
summary(ols1)

########################################
# Estimation
########################################

violence.analysis = cpt(
  Z = cpt.violence$Z,
  T = cpt.violence$T,
  class.methods = "forest",
  metric = "rate",
  perm.N = 1000
)

violence.analysis2 = cpt(
  Z = cpt.violence$Z,
  T = cpt.violence$T,
  class.methods = "logistic2",
  metric = "rate",
  perm.N = 1000
)

########################################
# Figures Lyall (violance example)
########################################

range = c(
  min(violence.analysis$nulldist, violence.analysis$teststat, 
      violence.analysis2$nulldist, violence.analysis2$teststat),
  max(violence.analysis$nulldist, violence.analysis$teststat, 
      violence.analysis2$nulldist, violence.analysis2$teststat)
          )

### Random Forest

teststat.obs <- violence.analysis$teststat 
teststat.null <- violence.analysis$nulldist

fig<-ggplot(data.frame(statistic.null=teststat.null),aes(x=statistic.null))+
  xlim(range)+
  ylim(0,250)+
  geom_histogram(
    fill="grey",
    breaks=seq(range[1],range[2],length=40)
  )+
  labs(
    title="Classifier: Random forest \n",
    x="\n Test statistic under the null",
    y="\n Frequency"
  )+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(), 
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)
  )+
  geom_vline(xintercept=teststat.obs,lty=1,col="black",size=1)+
  annotate("text", x = .7, y = 250, label = "Observed test statistic",col="black", size = 5)
ggsave(fig, file = "lyall-forest.pdf", width = 5, height=4)

### Logistic with interactions

teststat.obs <- violence.analysis2$teststat 
teststat.null <- violence.analysis2$nulldist

fig<-ggplot(data.frame(statistic.null=teststat.null),aes(x=statistic.null))+
  xlim(range)+
  ylim(0,250)+
  geom_histogram(
    fill="grey",
    breaks=seq(range[1],range[2],length=40)
  )+
  labs(
    title="Classifier: Logistic regression with interactions \n",
    x="\n Test statistic under the null",
    y="\n Frequency"
  )+
  theme_bw()+
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank() )+
  theme(panel.border = element_blank(), 
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 12)
  )+
  geom_vline(xintercept=teststat.obs,lty=1,col="black",size=1)+
  annotate("text", x = .7, y = 250, label = "Observed test statistic",col="black", size=5)

ggsave(fig, file = "lyall-log2.pdf", width = 5, height=4)












