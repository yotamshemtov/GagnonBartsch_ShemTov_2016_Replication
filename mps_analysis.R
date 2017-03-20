
rm(list=ls())
set.seed(12345)

library(cpt)
library(cpt.paper)

library(reshape)
library(ggplot2)
library(ggthemes)
library(rdd)
library(xtable)

data(list="cpt.MPs")
labels = cbind(attributes(cpt.MPs$rawdata)$var.labels,attributes(cpt.MPs$rawdata)$names)

Z0 = cpt.MPs$Z
tr0  = cpt.MPs$T
perm.N=500

setwd("~/Dropbox/Class.test2/figures/")

#### Balance in each covariate

f.stat = function(x1,x0){
  #sd.bias = summary(x1-x0)[4]/sd(c(x1,x0),na.rm=T)
  ks = ks.test(x1,x0)$p.value
  ttest = t.test(x1,x0)$p.value
  wilcox = wilcox.test(x1,x0)$p.value
  return(c(mean(x1,na.rm=T),mean(x0,na.rm=T),ttest,wilcox,ks))
}

keep = abs(cpt.MPs$wobl_margin)<0.0655
table(keep)
Z = Z0[keep,]
tr = tr0[keep]

tab = t(mapply(f.stat,as.list(data.frame(Z[tr==1,])),as.list(data.frame(Z[tr==0,]))))
tab = round(tab,dig=3)
colnames(tab) = c("Ave. Treat","Ave. control","T-test","Wilcoxon","KS")
print(tab)

rownames(tab) = c("Female",
                  "Year of birth",
                  "Year of death",
                  "Effective number of candidates",
                  "# voters first winning/best losing race",
                  "Turnout first winning/best losing race", 
                  "Previous attempts",
                  "Local politician",
                  "Civil servant",
                  "Business",
                  "White collar",
                  "Journalist")
                  
xtable(tab,caption="Balance table")

# balance plot:
balance.plot0 =1
if (balance.plot0==1){
  
  # Function to create figure with both summary statistics and plots of p-values for different variables across different groups
  # Author: Rocio Titiunik
  # Date: September 5th, 2008
  # Version: 1.0
  
  # Version to distribute publicly
  
  # NOTE: This function is *very far* from being a real function. As you'll see, there are many parameters set inside the function
  #       that should be arguments to the function instead. I'm currently working on a version that is fully flexible and does not
  #       set any parameters inside the function. But for now, this is it.
  
  # About the arguments of the function:
  
  # 'results':     a matrix whose rows are different variables; whose first two columns contain the means for treated and control;
  #                and whose remaining columns have the pvalues to be plotted for every variable
  
  # 'title':       title of the overall graph
  
  # at1, at2,at3:  scalars which indicates where to locate the three differents groups (mean treatment, mean controls, graph area) in the figure area
  
  # xlim1 :         the left limit of the x-axis; right limit is always set to 1
  
  # textsize:      scalar indicating the size of text in the figure
  
  # legend:        logical indicating whether the legend should be included
  
  # legendx:       scalar indicating the x-coordinate of the legend's location
  
  # legendy:       scalar indicating the y-coordinate of the legend's location
  
  # parcex:        scalar setting cex parameter
  
  
  
  plot.pval <- function(results, title=NULL, legend,legendx=0.7,legendy=3, textsize=0.9, parcex=0.8, at1=-0.35, at2=-0.15, at3=-0.9,xlim1=-0.85) {
    
    
    # set values of different parameters
    #xlim = c(xlim1,1); pchset = c(21,24,22,23); pchcolset = c("blue","yellow","red","darkgreen")
    xlim = c(xlim1,1); pchset = c(21,24,22,23); pchcolset = c("black","black","black","")
    
    # set margins and letter size
    par(cex=parcex, mai = c(0.5, 0.35, 1.1, 0.35))
    
    # set number of rows 
    ny = nrow(results)
    
    # create the empty figure
    if(!is.null(title))  plot(x=NULL,axes=F, xlim=xlim, ylim=c(1,ny),xlab="",ylab="", main=title)
    if(is.null(title))   plot(x=NULL,axes=F, xlim=xlim, ylim=c(1,ny),xlab="",ylab="")
    
    # add the 0, 0.05 and 0.1 vertical lines
    abline(v=c(0,0.05,0.1),lty=c(1,4,4), lwd=c(1,2,2))
    axis(side=1,at=c(0,0.05,0.1,1),tick=TRUE, las=2, cex.axis=0.7)
    
    # add labels on top of the three areas of the graph
    axis(side=3,at=at1,labels="Mean\nTreated",tick=FALSE, padj=0.5,cex.axis=textsize)
    axis(side=3,at=at2,labels="Mean\nControl",tick=FALSE, padj=0.5,cex.axis=textsize)
    axis(side=3,at=0.5,labels="P-values",tick=FALSE, padj=0.5,cex.axis=textsize)
    
    # Fill the figure with the information which is inside the 'results' matrix
    # First, add the p-values as points
    for(i in 4:ncol(results)) points(results[,i],ny:1, pch = pchset[i-4+1], col = pchcolset[i-4+1], bg = pchcolset[i-4+1])
    
    # Second, add each variable name and the means for treated and control
    for(i in 1:ny) {
      text(at3,ny-i+1,results[i,1],adj = 0,cex=textsize) # variable name
      text(at1,ny-i+1,results[i,2], cex=textsize)        # treatment mean
      text(at2,ny-i+1,results[i,3], cex=textsize)        # control mean
    }
    
    # Add dotted horizontal lines every two variables to make it prettier
    for(i in seq(2,by=2,length.out=floor((ny-1)/2))) abline(h = i+0.5, lty = 3)
    
    # Add legend
    if(legend) legend(x=legendx, y=legendy, c("T-test","Wilcoxon","KS"), pch=pchset, pt.bg = pchcolset, cex=0.8)
  }
  
}

tab = cbind(rownames(tab),tab)


pdf("fig_MPs_balance.pdf",height=4,width=5)
plot.pval(tab,legend=TRUE,parcex=0.4)
dev.off()

# joint F-test:
summary(lm(I(tr=="1")~(.),data=as.data.frame(Z)))
summary(lm(I(tr=="1")~(.),data=as.data.frame(Z[,-c(27:28)])))

### Checking balance using multivariate tests:
analysis <- cpt.analysis(Z=Z[,! colnames(Z) %in% c("tory","labour","xxoc_mainer")],
             T=tr,
             perm.N=perm.N, leaveout.N=leaveout.N,
             do_5fold = FALSE)

cat("Energy test P-value: ", analysis$energy$p.value,"\n")
cat("Crossmatch test P-value: ", analysis$crossmatch$approxpval,"\n")

########################################
# CPT figures:
########################################

### Random Forest

teststat.obs <- analysis$forest$rate$teststat 
teststat.null <- analysis$forest$rate$nulldist

range = c(min(teststat.obs,teststat.null),max(teststat.obs,teststat.null))
fig<-ggplot(data.frame(statistic.null=teststat.null),aes(x=statistic.null))+
  xlim(range)+
  geom_histogram(
    fill="grey",
    breaks=seq(range[1],range[2],length=20)
  )+
  theme_bw()+
  theme(panel.border = element_blank(),axis.line = element_line(colour = "black"))+
  labs(
    #title="Classifier: Random forest",
    title="",
    x="\n Test statistic under the null",
    y="\n Frequency"
  )

# add the observed test statistic:
pdf("MPs_dist_null.pdf")
fig + geom_vline(xintercept=teststat.obs,lty=1,col="black",size=1)+
  annotate("text", x = .82, y = 75, label = "Observed test statistic",col="black", size = 4)+
  annotate("text", x = .82, y = 70, label = paste("P-value: ",analysis$forest$rate$pval),col="black", size = 4)
dev.off()

### logistic regression with all two way interactions:

#checking the model works:
summary(glm(tr~(.)^2,data=as.data.frame(Z),family=binomial(link="logit")))


########################################
# Density imbalance
########################################

d.labour <-cpt.MPs$rawdata[cpt.MPs$rawdata$labour==1,] 
d.nonlabour <-cpt.MPs$rawdata[cpt.MPs$rawdata$labour==0,]

DCdensity(cpt.MPs$rawdata$wobl_margin, plot=FALSE)
DCdensity(d.labour$wobl_margin, plot=FALSE)
DCdensity(d.nonlabour$wobl_margin, plot=FALSE)

pdf("hist_all.pdf")
ggplot(cpt.MPs$rawdata, aes(x=wobl_margin))+
  geom_histogram(fill="gray")+
  theme_bw()+
  theme(panel.border = element_blank(),axis.line = element_line(colour = "black"))+
  labs(
    title="All parties",
    x="\n Winning margin",
    y="\n Frequency"
  )+
  ylim(c(0,50))+
  geom_vline(xintercept = 0, col="black",lwd=1)+
  annotate("text", x = -0.33, y = 50, 
           label = paste("McCrary test P-value:",round(DCdensity(cpt.MPs$rawdata$wobl_margin, plot=FALSE), dig=3)),
           col="black")
dev.off()

pdf("hist_labour.pdf")
ggplot(d.labour,aes(x=wobl_margin))+
  geom_histogram(fill="gray")+
  theme_bw()+
  theme(panel.border = element_blank(),axis.line = element_line(colour = "black"))+
  labs(
    title="Labour party candidates",
    x="\n Winning margin",
    y="\n Frequency"
  )+
  ylim(c(0,35))+
  geom_vline(xintercept = 0, col="black",lwd=1)+
  annotate("text", x = -0.25, y = 35, 
           label = paste("McCrary test P-value:",round(DCdensity(d.labour$wobl_margin, plot=FALSE), dig=3))
           ,col="black")
dev.off()

pdf("hist_nonlabour.pdf")
ggplot(d.nonlabour,aes(x=wobl_margin))+
  geom_histogram(fill="gray")+
  theme_bw()+
  theme(panel.border = element_blank(),axis.line = element_line(colour = "black"))+
  labs(
    title="Non-Labour party candidates",
    x="\n Winning margin",
    y="\n Frequency"
  )+
  ylim(c(0,35))+
  geom_vline(xintercept = 0, col="black",lwd=1)+
  annotate("text", x = -0.3, y = 35, 
           label = paste("McCrary test P-value:",round(DCdensity(d.nonlabour$wobl_margin, plot=FALSE), dig=4))
           ,col="black")
dev.off()

##########################################################
# RDD plot figure: CPT vs energy and crossmatch
##########################################################

setwd("~/Dropbox/Class.test2/Rcode/analyses/real_data/MPs/")
load("MPs.rda")
setwd("~/Dropbox/Class.test2/figures/")

### Function for extracting the results:

getresults = function(analysis)
{
  results = matrix(NA,length(analysis$analyses),5)
  colnames(results) = c("window", "observations", "CrossMatch", "Energy", "CPT")
  for (i in 1:length(analysis$analyses))
  {
    results[i,1] = analysis$windowsizes[i]
    results[i,2] = analysis$observations[i]
    
    if (is.na(analysis$analyses[[i]]$crossmatch)){
      results[i,3] = NA
    }else{
      results[i,3] = analysis$analyses[[i]]$crossmatch$approxpval 
    }
    results[i,4] = analysis$analyses[[i]]$energy$p.value
    results[i,5] = analysis$analyses[[i]]$forest$rate$pval
  }
  return(results)
}

### MPs for sale: ###

dp = as.data.frame(getresults(MPs.analysis))
df=melt(dp[,-1],id="observations",variable_name="test.type")

p.cpt <- ggplot(df,aes(x=observations,y=value,col=test.type, shape=test.type))+
  geom_point()+geom_line()+
  labs(
    #title="Eggers and Hainmueller (2009) \n",
    title = "",
    y = "P-value \n" ,
    x = "Number of Obs. in window \n")+
  theme_bw()+ 
  theme(panel.border = element_blank(),axis.line = element_line(colour = "black"))+
  geom_hline(yintercept = 0.05, lty=2,col="black")+
  scale_colour_grey()+
  guides(col=guide_legend(title="Test type: "),shape=guide_legend(title="Test type: "))+ # adding legend title
  theme(legend.position="bottom") # legend position

pdf("MPs_rdd_window.pdf",width=8,height=7)
p.cpt+geom_vline(xintercept = 164, col="grey",lwd=1,lty=2)+
  annotate("text", x = 205, y = 1, 
           label = paste("EH choosen window")
           ,col="black")
dev.off()  


















