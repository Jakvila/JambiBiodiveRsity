
library(rDEA)

# used inputs: Size, Labour and Inputs (Agrochemical use)

 rts.test(xMat, yVec, W=NULL, model = "input", H0= "constant", bw= "cv", B= 100, alpha = 0.05)

 rts.test(xMat, yVec, W=NULL, model = "output", H0= "constant", bw= "cv", B= 100, alpha = 0.05)

# result: Both with input and output orientation the H0 of returns to scale being constant cannot be rejected


# test for non-increasing rts

rts.test(xMat, yVec, W=NULL, model = "input", H0= "non-increasing", bw= "cv", B= 100, alpha = 0.05)

rts.test(xMat, yVec, W=NULL, model = "output", H0= "non-increasing", bw= "cv", B= 100, alpha = 0.05)

# result for input orientation: H0 REJECTED --> returns to scale are INCREASING
# result for output orientation: H0 NOT REJECTED --> rts are NON-INCREASING

# inputs that are used for the whole analysis: see if some outcomes change

 rts.test(inputMat, outputVec, W=NULL, model = "input", H0= "constant", bw= "cv", B= 100, alpha = 0.05)

 rts.test(inputMat, outputVec, W=NULL, model = "output", H0= "constant", bw= "cv", B= 100, alpha = 0.05)

# test for non-increasing

rts.test(inputMat, outputVec, W=NULL, model = "input", H0= "non-increasing", bw= "cv", B= 100, alpha = 0.05)

rts.test(inputMat, outputVec, W=NULL, model = "output", H0= "non-increasing", bw= "cv", B= 100, alpha = 0.05)

# for input orientation: for any test statistic, we have INCREASING returns to scale. For output orientation
# we only have increasing returns to scale according to pvalue48, i.e. for ratio of means/medians


rts_test_table <- data.frame(rbind(c("Test for non-increasing returns to scale",
                                     "0.81", "-0.035"),
                                   c(NA, "not rejected", "not rejected"),
                                   c("Test for constant returns to scale",
                                     "0.6447", "-0.0744"),
                                   c(NA, "not rejected", "rejected")))
names (rts_test_table) <- c(NA, "W_hat 4.5 test statistic", "w_hat 4.8 test statistic")
kable(rts_test_table, row.names = T, "latex")


summary(rtstestinp)
summary(rtstestoutp)


# CRS and VRS hypothetic graphs

# CRS 

CRSgraphX <- c(0, 1, 2, 3, 4, 5)
CRSgraphY <- c(0, 0.9, 1.4, 3, 3.4, 4.7)
CRSgraphABC <- c(NA, "A", "B", "C", "D", "E")
CRSgraph <- data.frame(CRSgraphX, CRSgraphY, CRSgraphABC)
plot(CRSgraph$CRSgraphX, CRSgraph$CRSgraphY)
abline(0,1)

library(ggplot2)
ggplot(data = CRSgraph, aes(x=CRSgraphX , y = CRSgraphY)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5)) +
  geom_area(aes(x = CRSgraphX , y = CRSgraphX), fill = "lightblue") +
  geom_point(colour = "blue") +
  geom_abline(intercept = 0.02, size = 0.75, linetype = "dashed") +
  geom_text(aes(label=CRSgraphABC),hjust=0, vjust=1.2) +
  geom_segment(aes(x=0, xend = 5 , y=0, yend = 0), size=1,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=0, yend = 5), size=1,
               arrow = arrow(length = unit(0.3,"cm"))) +
  xlab("Inputs") +
  ylab("Output") +
  ggtitle("CRS")


# VRS 

CRSgraphX <- c(1, 2, 3, 4, 5)
CRSgraphY <- c(0.7, 1.4, 3, 3.4, 4.3)
CRSgraphABC <- c(NA, "A", "B", "C", "D", "E")
VRSgraphX <- c(1, 1, 3, 5)
VRSgraphY <- c(0, 0.7, 3, 4.3)
VRSgraph <- data.frame(VRSgraphX, VRSgraphY)

CRSgraph <- data.frame(CRSgraphX, CRSgraphY, CRSgraphABC)
plot(CRSgraph$CRSgraphX, CRSgraph$CRSgraphY)
abline(0,1)

library(ggplot2)
ggplot(data = CRSgraph, aes(x=CRSgraphX , y = CRSgraphY)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  geom_area(data = VRSgraph,  aes(x = VRSgraphX , y = VRSgraphY), fill = "lightblue") +
  geom_point(colour = "blue") +
  geom_line(data = VRSgraph,  aes(x = VRSgraphX , y = VRSgraphY),
            intercept = 0.02, size = 0.75, linetype = "dashed") +
  geom_text(aes(label=CRSgraphABC),hjust=-0.2, vjust=1.2) +
  geom_segment(aes(x=0, xend = 5 , y=0, yend = 0), size=1,
               arrow = arrow(length = unit(0.3,"cm"))) +
  geom_segment(aes(x=0, xend = 0 , y=0, yend = 5), size=1,
               arrow = arrow(length = unit(0.3,"cm"))) +
  xlab("Inputs") +
  ylab("Output") +
  ggtitle("VRS")



###
# report all the observations in one table

library(tidyr)

PalmDataRestricted %>% spread(hid, wave)

ObsRestricted <- unique(PalmDataRestricted$hid)
ObsRestricted <- as.data.frame(ObsRestricted)
names(ObsRestricted) <- c("Household ID")
length(unique(PalmDataRestricted$hid))
ObsRestricted$`Household ID` <- as.character(ObsRestricted$`Household ID`)


for(i in 1 : 39){
  if(ObsRestricted$`Household ID`[i] %in% PalmDataRestricted$hid[PalmDataRestricted$wave == 2012]){
    ObsRestricted$"2012"[i] <-  1
  } else{
    ObsRestricted$"2012"[i] <-  0
  }
}

for(i in 1 : 39){
    if(ObsRestricted$`Household ID`[i] %in% PalmDataRestricted$hid[PalmDataRestricted$wave == 2015]){
        ObsRestricted$"2015"[i] <-  1
    } else{
        ObsRestricted$"2015"[i] <-  0
    }
}

for(i in 1 : 39){
  if(ObsRestricted$`Household ID`[i] %in% PalmDataRestricted$hid[PalmDataRestricted$wave == 2018]){
    ObsRestricted$"2018"[i] <-  1
  } else{
      ObsRestricted$"2018"[i] <-  0
  }
}

ObsRestricted$`Household ID` <- as.numeric(ObsRestricted$`Household ID`)

ObsRestricted <- ObsRestricted[order(ObsRestricted$`Household ID`),]
row.names(ObsRestricted) <- NULL
require(stargazer)
require(knitr)

kable(ObsRestricted, row.names = T, "latex")


