library(ggplot2)
library(ggpubr)

setwd(dir='H:/matlab/Pacific Saury/LMNLM_KEFNS')
data1 <- read.table("LM_KEFN.csv",header = TRUE,sep=",")
data1["type"] = data1[6]

#Pacific saury
windowsFonts(
  A=windowsFont("Times New Roman"),
  B=windowsFont("Times New Roman")
)

p <- ggboxplot(data1,x="type",y="LNR",
               color = 'type',
               add = 'jitter')+
  scale_y_continuous(name = "LNR") +
  theme(axis.text.x = element_text(size=13,face="bold",family = "A"),
        axis.text.y = element_text(size=13,face="bold",family = "A"),
        axis.title.x = element_text(size=17,face="bold",family = "A"),
        axis.title.y = element_text(size=17,face="bold",family = "A"),
        legend.position = "none")
#legend.title = element_text(size=12,face="bold",family = "A"),
#legend.text = element_text(size=12,face="bold",family = "A"))
p

#Define the comparison of combined lists
my_comparisons <- list(c("LM-KEFS","NLM-KEFS"),
                       c("NLM-KEFS","LM-KEFN"),
                       c("LM-KEFN","NLM-KEFN"),
                       c("LM-KEFS","LM-KEFN"),
                       c("NLM-KEFS","NLM-KEFN"),
                       c("LM-KEFS","NLM-KEFN"))

#Comparison of mean values with box plots added
f <- p+ stat_compare_means(comparisons= my_comparisons) +
  stat_compare_means(label.y = 3.3)
f

tiff("squid_type.tiff",res=300,width = 5,height = 4,units="in")
print(f)
dev.off()

#Japanese sardine
data2 <- data1

p1 <- ggboxplot(data2,x="type",y="LNR",
                color = 'type',
                add = 'jitter')+
  scale_y_continuous(name = "Sardine LNR",position = "right") +
  theme(axis.text.x = element_text(size=13,face="bold",family = "A"),
        axis.text.y = element_text(size=13,face="bold",family = "A"),
        axis.title.x = element_text(size=17,face="bold",family = "A"),
        axis.title.y = element_text(size=17,face="bold",family = "A"),
        legend.position = "none")
#legend.title = element_text(size=12,face="bold",family = "A"),
#legend.text = element_text(size=12,face="bold",family = "A"))

p1


#Define the comparison of combined lists
my_comparisons <- list(c("LM-KEFS","NLM-KEFS"),
                       c("NLM-KEFS","LM-KEFN"),
                       c("LM-KEFN","NLM-KEFN"),
                       c("LM-KEFS","LM-KEFN"),
                       c("NLM-KEFS","NLM-KEFN"),
                       c("LM-KEFS","NLM-KEFN"))

#Comparison of mean values with box plots added
f1 <- p1+ stat_compare_means(comparisons= my_comparisons) +
  stat_compare_means(label.y = 12)

tiff("sardine_type2.tiff",res=300,width = 5,height = 4,units="in")
print(f1)
dev.off()
