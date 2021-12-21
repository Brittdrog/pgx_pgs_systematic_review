require(dplyr)
library(reshape)
library(ggplot2)
library(viridis)
#https://waldyrious.net/viridis-palette-generator/


#load data
rs_data <- read.csv("/Users/janicelyle/Desktop/Final_mental_cardio_SR.csv")

#melt to reformat data
rs_data_format <- melt(rs_data, id.vars = "PMID")

#add category column
rs_data_format$Category <- ifelse(rs_data_format$variable == "Study_type" |
                                    rs_data_format$variable == "Risk_model_purpose", 
                                  "01_Background", 
                                  ifelse(rs_data_format$variable == "Study_design_recruitment" |
                                           rs_data_format$variable == "Participant_demographics" |
                                           rs_data_format$variable == "Ancestry" |
                                           rs_data_format$variable == "Genetic_data" |
                                           rs_data_format$variable == "Non_genetic_variables" |
                                           rs_data_format$variable == "Outcome_interest" |
                                           rs_data_format$variable == "Missing_data" , 
                                         "02_Study population and data", 
                                         ifelse(rs_data_format$variable == "PRS_construction" |
                                                  rs_data_format$variable == "Risk_model_type" |
                                                  rs_data_format$variable == "Integrated_risk_models" , 
                                                "03_Risk model development application", 
                                                ifelse(rs_data_format$variable == "PRS_distribution" |
                                                         rs_data_format$variable == "Risk_model_predictive_ability" |
                                                         rs_data_format$variable == "Risk_model_discrimination" |
                                                         rs_data_format$variable == "Risk_model_calibration" |
                                                         rs_data_format$variable == "Subgroup_analysis", 
                                                       "04_Risk model evaluation", 
                                                       ifelse(rs_data_format$variable == "Risk_model_interpretation" |
                                                                rs_data_format$variable == "Limitations" |
                                                                rs_data_format$variable == "Generalisability" |
                                                                rs_data_format$variable == "Risk_model_intended_uses", 
                                                              "05_Limitations clinical implications", "06_Data transparency availability")))))

#count number of yes, no and somewhat in each category
broad_rs_data_counts <- rs_data_format %>% group_by(value, Category) %>% summarise(n = n())

rs_Labels<- c("Data transparency and availability","Limitations clinical implications", 
              "Risk model evaluation", "Risk model development application", "Study population and data",
              "Background")


#Plot
broad_rs_data_plot <- ggplot(na.omit(broad_rs_data_counts), aes(fill=value, y=Category, x=n)) + 
  geom_bar(position="fill", stat="identity") +
  ggtitle("") +
  ylab("") + xlab("Proportion of studies") +
  scale_y_discrete(labels= rs_Labels) +
  scale_fill_manual(values = c("#fde725", "#21918c","#440154"), 
                    name = "", labels = c("Yes", "Somewhat", "No")) +
  theme_bw() + 
  theme(legend.key.height= unit(1.5, 'cm'),
        legend.key.width= unit(2, 'cm')) +
  theme(legend.title=element_text(size=17, face="bold"), 
        legend.text=element_text(size=15)) +
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle=45, hjust=1))

ggsave("/Users/janicelyle/Desktop/reporting_standards.png", width = 12, height = 6)

