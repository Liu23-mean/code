library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gradientForest)
library(RColorBrewer)


#Take Japanese sardine as an example
data <- read.csv("H:/matlab/Japanese sardine/GF/data.csv")

#Random Forest (1000 fittings)
LNRjs.r2 <- NA 
RESjs.r2 <- NA 
wi <- NA #Save weighted importance

for (i in 1:1000){
  set.seed(i)
  gf <- gradientForest(data = data,predictor.vars = colnames(data[,c(4:12)]), 
                       response.vars = colnames(data[,c(2:3)]),ntree = 500)
  LNRjs.r2[i] <- gf$result[names(gf$result)=="LNRjs"][1]
  LNRPSjs.r2[i] <- gf$result[names(gf$result)=="LNRPSjs"][1]
  a<- importance(gf)[colnames(data)[c(4:12)]]
  wi <- cbind(wi,a)
  print(i)
}
#Dataset of model fitting degrees
res <- cbind(LNRjs.r2,RESjs.r2)
#Optimal model
total.r2<-apply(res,1,sum,na.rm=T)
i.bestmodel<-which(total.r2==max(na.omit(total.r2))) #76
i.bestmodel

#Calculation takes too long, save the data
res <- as.data.frame(res)
save(res,file="res.Rdata")

wi <- as.data.frame(wi)
wi$wi <- rownames(wi)
save(wi,file = "wi.Rdata")

##############################Plotting weighted importance####################################
load("wi.Rdata")
wi <- as.data.frame(t(wi))
wi <- wi[-1,]
wi <- gather(wi,Predictor,Importance)
wi$Importance <- as.numeric(wi$Importance)


windowsFonts(
  A=windowsFont("Times New Roman"),
  B=windowsFont("Calibri")
)

va<- c("WSST",'KCdis','WSSH',"KEFlat",'OOFlat','KOTlat','SSSH2','SSST2','KEssh')
va <- rev(va)

theme_set(theme_bw())

f2 <- ggplot(wi)+
  geom_boxplot(aes(x=Predictor,y=Importance,fill=Predictor))+
  #geom_violin(aes(x=Predictor,y=Importance,fill=Predictor))+
  #scale_fill_brewer(palette = "Set3")+
  scale_x_discrete(limits=va,expand=c(0.1,0.1))+
  scale_y_continuous("Importance",expand=c(0,0),minor_breaks = NULL,limits = c(0, 0.0799))+
  coord_flip()+
  theme(axis.text.x = element_text(size=16,face="bold",family = "A"),
        axis.text.y = element_text(size=16,face="bold.italic",family = "A"),
        axis.title.x = element_text(size=20,face="bold",family = "A"),
        axis.title.y =element_text(size=20,face="bold",family = "A"),
        plot.title = element_text(size=16,face="bold",family="A"),
        axis.ticks = element_blank(),
        legend.position = "none",
        #legend.position = c(0.75,0.3),
        legend.key.height = unit(0.5,"cm"),
        legend.title = element_text(size=16,face="bold",family = "A"),
        legend.text = element_text(size=16,face="bold.italic",family = "A"),
        legend.background = element_rect(linetype = "solid",colour = "black")
  )
f2

tiff("weighted importance sardine.tiff",res=400,width = 5,height = 4.5,units="in")
print(f2)
dev.off()
