
rm(list=ls())

library(cpt)
library(cpt.paper)

data(list="cpt.MPs")

# Parameters:
perm.N = 500
leaveout.N = 50

### MPs
MPs.analysis = list()
MPs.analysis$windowsizes = seq(0.01,0.1, length.out=30)
MPs.analysis$observations = rep(0, length(MPs.analysis$windowsizes))
MPs.analysis$analyses = list()
for (j in 1:length(MPs.analysis$windowsizes))
{
  keep = abs(cpt.MPs$wobl_margin) <= MPs.analysis$windowsizes[j]
  MPs.analysis$observations[j] = sum(keep)
  MPs.analysis$analyses[[j]] = 		cpt.analysis(cpt.MPs$Z[keep,! colnames(cpt.MPs$Z) %in% c("tory","labour")],
							     cpt.MPs$T[keep],
							     perm.N=perm.N, leaveout.N=leaveout.N,
							     do_5fold = FALSE, do_lda = FALSE)
}
save(MPs.analysis, file="MPs.rda")















