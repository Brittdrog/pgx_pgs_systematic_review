require(dplyr)
library(reshape)
library(ggplot2)
library(viridis)
#https://waldyrious.net/viridis-palette-generator/

#load data
 
Significant_data <- read.csv("/Users/janicelyle/Desktop/Sample_size_ancestry_disaese_sig.csv")


#melt to reformat data
#Significant_data_format <- melt(Significant_data, id.vars = "PMID")

Significant_data_disease<-Significant_data [ , c("Disease_area","Significant_result") ]
Significant_data_disease<- melt(Significant_data_disease, id.vars = "Disease_area")
Significant_data_disease <- Significant_data_disease %>% group_by(value, Disease_area) %>% summarise(n = n())



## Disease Plot ####
Significant_data_disease_plot<- ggplot(Significant_data_disease, aes(y= reorder(Disease_area,n), x=n,fill= value ))+
  geom_bar(position="stack", stat="identity") + 
  ggtitle("") +
  ylab("") + xlab("Number of studies") +
  guides(fill=guide_legend(title="Significant"))+
  scale_fill_manual(values = c("#fde725","#440154"), 
                    name = "", labels = c("Yes", "No")) +
  theme_bw() + 
  theme(legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(2, 'cm')) +
  theme(legend.title=element_text(size=17, face="bold"), 
        legend.text=element_text(size=15)) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))

ggsave("/Users/janicelyle/Desktop/Significant_diseases.png", width = 12, height = 6)


