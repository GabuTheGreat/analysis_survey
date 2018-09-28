#load libraries
library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)

#Read data into R
cleaned_data <- read.csv("~/work/cleaned_capacity.csv", header = TRUE, strip.white=TRUE)

####
#Unique departments
#count Unique departments
by_department = group_by(cleaned_data, department)
unique_dpt = summarise(by_department, count = n())

#create a bar plot
p<- ggplot(unique_dpt, aes(department, count, fill = department))+
      geom_bar(stat = "identity")+
      geom_text(aes(label = count), fontface = "bold")+
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y=element_text(colour = "#7F7F7F", size = 12, face = "bold"),
        axis.title.y = element_text(colour = "#FE6F88", size = 12, face = "bold"),
        legend.title = element_text(colour = "#FE6F88", size = 12, face = "bold"),
        legend.text = element_text(colour = "#7F7F7F", size = 9, face = "bold")
      )

ggplotly(p) %>% config(displayModeBar = F)

####
#count Unique unit
by_unit = group_by(cleaned_data, unit)
unique_unit = summarise(by_unit, count = n())

#create a bar plot
ggplot(unique_unit, aes(unit, count, fill = unit))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = count), fontface = "bold")+
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y=element_text(colour = "#7F7F7F", size = 12, face = "bold"),
    axis.title.y = element_text(colour = "#FE6F88", size = 12, face = "bold"),
    legend.title = element_text(colour = "#FE6F88", size = 12, face = "bold"),
    legend.text = element_text(colour = "#7F7F7F", size = 9, face = "bold")
  )


#####
#Analysis of unique positions reported back in the survey.
cleaned_data$position <- tolower(cleaned_data$position)

#Returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)
cleaned_data$position <-trim.trailing(cleaned_data$position)

#Capitilize the first word only
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
cleaned_data$position <-firstup(cleaned_data$position)

by_position = group_by(cleaned_data, position)
unique_position = summarise(by_position, count = n())
View(unique_position)

###
#How long have you been in your current position
#Develop a box plot

time_current <- cleaned_data$time_current
time_industry <- cleaned_data$time_industry

bxplt_data_1 <- data.frame(group = "current", value =  time_current)
bxplt_data_2 <- data.frame(group = "industry", value =  time_industry)
bxplt_data <- rbind(bxplt_data_1,bxplt_data_2)

#develop a box plot for distribution of experience
ggplot(data = bxplt_data, aes(group, value, fill = group))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)


#Analysis of unique qualifications  reported back in the survey.
cleaned_data$level_education <- tolower(cleaned_data$level_education)
cleaned_data$level_education <-trim.trailing(cleaned_data$level_education)
cleaned_data$level_education <-firstup(cleaned_data$level_education)
by_edlevel = group_by(cleaned_data, level_education)
unique_edlevel = summarise(by_edlevel, count = n())
View(unique_edlevel)

#Training Required Per Unit.
#Analysis on training required per unit
training_data <- cleaned_data[,c('training', 'unit', 'department')]
grouped_training = group_by(training_data, unit)
#unit per training
unit_training <- summarise(grouped_training, Yes = sum(training == 'Yes'), No = sum(training == 'No'))
unit_renamed <- c("C&P","DL","FMT","IS", "PDA","PDM")
unit_training$unit <- unit_renamed

#reshaped training
shp_training <- melt(unit_training)
shp_training <- filter(shp_training, value > 0)

#plotting unit against 
#create a bar plot
ggplot(shp_training, aes(unit, value, fill = variable))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = value), fontface = "bold", position = position_dodge(width = 1), vjust = 0.5)+
  theme(
    axis.text.x = element_text(colour = "#7F7F7F", size = 12, face = "bold"),
    axis.title.x = element_text(colour = "#FE6F88", size = 12, face = "bold"),
    axis.text.y=element_text(colour = "#7F7F7F", size = 12, face = "bold"),
    axis.title.y = element_text(colour = "#FE6F88", size = 12, face = "bold"),
    legend.title = element_text(colour = "#FE6F88", size = 12, face = "bold"),
    legend.text = element_text(colour = "#7F7F7F", size = 9, face = "bold")
  )


## Training
#Analysis of people who have done training
training_done <- group_by(cleaned_data, training)
training_done <- summarise(training_done, count = n()) %>% mutate(Percantage = paste(round((count/sum(count)*100),0),"%", sep = ""))
View(training_done)

#Tabling training done per departement
#Remember to choose training per unit.
cleaned_data$training_done <- tolower(cleaned_data$training_done)
cleaned_data$training_done <- firstup(cleaned_data$training_done)
table_trainingby <- group_by(cleaned_data, training_done)
table_training <- table_trainingby %>% summarise(count = n()) %>% filter(training_done != "")
View(table_training)


##This question
#Analysis knowledge in research research.
#Remember to choose training per unit. choose per training
knowledge_by <- group_by(cleaned_data, kwld_rsch)
table_kwld <- summarise(knowledge_by, count = n()) %>% mutate(Percantage = paste(round((count/sum(count)*100),0),"%", sep = ""))
View(table_kwld) 

#Analysis of people in need of trainig
nd_trby <- group_by(cleaned_data, need_training)
table_ndtr <- summarise(nd_trby, count = n()) %>% mutate(Percantage = paste(round((count/sum(count)*100),0),"%", sep = ""))
View(table_ndtr)

#Analysis on Research question
by_unit = group_by(cleaned_data, unit)
avarage_research = summarise(by_unit,
                    R_var1 = round(mean(R_var1, na.rm = TRUE), 0),
                    R_var2 = round(mean(R_var2, na.rm = TRUE), 0),
                    R_var3 = round(mean(R_var3, na.rm = TRUE), 0),
                    R_var4 = round(mean(R_var4, na.rm = TRUE), 0),
                    R_var5 = round(mean(R_var5, na.rm = TRUE), 0),
                    R_var6 = round(mean(R_var6, na.rm = TRUE), 0),
                    R_var7 = round(mean(R_var7, na.rm = TRUE), 0),
                    R_var8 = round(mean(R_var8, na.rm = TRUE), 0),
                    R_var9 = round(mean(R_var9, na.rm = TRUE), 0),
                    R_var10 = round(mean(R_var10, na.rm = TRUE), 0),
                    R_var11 = round(mean(R_var11, na.rm = TRUE), 0),
                    R_var12 = round(mean(R_var12, na.rm = TRUE), 0),
                    R_var13 = round(mean(R_var13, na.rm = TRUE), 0),
                    R_var14 = round(mean(R_var14, na.rm = TRUE), 0),
                    R_var15 = round(mean(R_var15, na.rm = TRUE), 0),
                    Count = n())


avarage_research <- avarage_research %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(R_var1,R_var2,R_var3,R_var4,R_var5,R_var6,R_var7,R_var8,R_var9,R_var10,R_var11,R_var12,R_var13,R_var14,R_var15)),0))

attach(avarage_research)
avarage_research = avarage_research %>%
  add_row(
    unit = "Mean",
    R_var1 = round(mean(R_var1), 0),
    R_var2 = round(mean(R_var2), 0),
    R_var3 = round(mean(R_var3), 0),
    R_var4 = round(mean(R_var4), 0),
    R_var5 = round(mean(R_var5), 0),
    R_var6 = round(mean(R_var6), 0),
    R_var7 = round(mean(R_var7), 0),
    R_var8 = round(mean(R_var8), 0),
    R_var9 = round(mean(R_var9), 0),
    R_var10 = round(mean(R_var10), 0),
    R_var11 = round(mean(R_var11), 0),
    R_var12 = round(mean(R_var12), 0),
    R_var13 = round(mean(R_var13), 0),
    R_var14 = round(mean(R_var14), 0),
    R_var15 = round(mean(R_var15), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )

View(avarage_research)

## Analysis of Monitoring and Evaluation  Question.
avarage_mne = summarise(by_unit,
                        M_var1 = round(mean(M_var1, na.rm = TRUE), 0),
                        M_var2 = round(mean(M_var2, na.rm = TRUE), 0),
                        M_var3 = round(mean(M_var3, na.rm = TRUE), 0),
                        M_var4 = round(mean(M_var4, na.rm = TRUE), 0),
                        M_var5 = round(mean(M_var5, na.rm = TRUE), 0),
                        M_var6 = round(mean(M_var6, na.rm = TRUE), 0),
                        M_var7 = round(mean(M_var7, na.rm = TRUE), 0),
                        M_var8 = round(mean(M_var8, na.rm = TRUE), 0),
                        M_var9 = round(mean(M_var9, na.rm = TRUE), 0),
                        M_var10 = round(mean(M_var10, na.rm = TRUE), 0),
                        M_var11 = round(mean(M_var11, na.rm = TRUE), 0),
                        Count = n())

avarage_mne <- avarage_mne %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(M_var1,M_var2,M_var3,M_var4,M_var5,M_var6,M_var7,M_var8,M_var9,M_var10,M_var11)),0))

attach(avarage_mne)
avarage_mne = avarage_mne %>%
  add_row(
    unit = "Mean",
    M_var1 = round(mean(M_var1), 0),
    M_var2 = round(mean(M_var2), 0),
    M_var3 = round(mean(M_var3), 0),
    M_var4 = round(mean(M_var4), 0),
    M_var5 = round(mean(M_var5), 0),
    M_var6 = round(mean(M_var6), 0),
    M_var7 = round(mean(M_var7), 0),
    M_var8 = round(mean(M_var8), 0),
    M_var9 = round(mean(M_var9), 0),
    M_var10 = round(mean(M_var10), 0),
    M_var11 = round(mean(M_var11), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )

View(avarage_mne)

## Analysis on learning question.
avarage_learning = summarise(by_unit,
                             L_var1 = round(mean(L_var1, na.rm = TRUE), 0),
                             L_var2 = round(mean(L_var2, na.rm = TRUE), 0),
                             L_var3 = round(mean(L_var3, na.rm = TRUE), 0),
                             L_var4 = round(mean(L_var4, na.rm = TRUE), 0),
                             L_var5 = round(mean(L_var5, na.rm = TRUE), 0),
                             L_var6 = round(mean(L_var6, na.rm = TRUE), 0),
                             L_var7 = round(mean(L_var7, na.rm = TRUE), 0),
                             L_var8 = round(mean(L_var8, na.rm = TRUE), 0),
                             L_var9 = round(mean(L_var9, na.rm = TRUE), 0),
                             L_var10 = round(mean(L_var10, na.rm = TRUE), 0),
                             L_var11 = round(mean(L_var11, na.rm = TRUE), 0),
                             L_var12 = round(mean(L_var12, na.rm = TRUE), 0),
                             L_var13 = round(mean(L_var13, na.rm = TRUE), 0),
                             L_var14 = round(mean(L_var14, na.rm = TRUE), 0),
                             Count = n())

avarage_learning <- avarage_learning %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(L_var1,L_var2,L_var3,L_var4,L_var5,L_var6,L_var7,L_var8,L_var9,L_var10,L_var11,L_var12,L_var13,L_var14)),0))

attach(avarage_learning)
avarage_learning = avarage_learning %>%
  add_row(
    unit = "Mean",
    L_var1 = round(mean(L_var1), 0),
    L_var2 = round(mean(L_var2), 0),
    L_var3 = round(mean(L_var3), 0),
    L_var4 = round(mean(L_var4), 0),
    L_var5 = round(mean(L_var5), 0),
    L_var6 = round(mean(L_var6), 0),
    L_var7 = round(mean(L_var7), 0),
    L_var8 = round(mean(L_var8), 0),
    L_var9 = round(mean(L_var9), 0),
    L_var10 = round(mean(L_var10), 0),
    L_var11 = round(mean(L_var11), 0),
    L_var12 = round(mean(L_var12), 0),
    L_var13 = round(mean(L_var13), 0),
    L_var14 = round(mean(L_var14), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )
View(avarage_learning)

#Analysis on data collection questions
avarage_dcollection = summarise(by_unit,
                                D_var1 = round(mean(D_var1, na.rm = TRUE), 0),
                                D_var2 = round(mean(D_var2, na.rm = TRUE), 0),
                                D_var3 = round(mean(D_var3, na.rm = TRUE), 0),
                                D_var4 = round(mean(D_var4, na.rm = TRUE), 0),
                                D_var5 = round(mean(D_var5, na.rm = TRUE), 0),
                                D_var6 = round(mean(D_var6, na.rm = TRUE), 0),
                                D_var7 = round(mean(D_var7, na.rm = TRUE), 0),
                                D_var8 = round(mean(D_var8, na.rm = TRUE), 0),
                                D_var9 = round(mean(D_var9, na.rm = TRUE), 0),
                                D_var10 = round(mean(D_var10, na.rm = TRUE), 0),
                                Count = n())

avarage_dcollection <- avarage_dcollection %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(D_var1,D_var2,D_var3,D_var4,D_var5,D_var6,D_var7,D_var8,D_var9,D_var10)),0))

attach(avarage_dcollection)
avarage_dcollection = avarage_dcollection %>%
  add_row(
    unit = "Mean",
    D_var1 = round(mean(D_var1), 0),
    D_var2 = round(mean(D_var2), 0),
    D_var3 = round(mean(D_var3), 0),
    D_var4 = round(mean(D_var4), 0),
    D_var5 = round(mean(D_var5), 0),
    D_var6 = round(mean(D_var6), 0),
    D_var7 = round(mean(D_var7), 0),
    D_var8 = round(mean(D_var8), 0),
    D_var9 = round(mean(D_var9), 0),
    D_var10 = round(mean(D_var10), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )

View(avarage_dcollection)

#E: Qualitative and Quantitative Data management, analysis, and visualization packages 
avarage_qqAnalysis = summarise(by_unit,
                               Qq_var1 = round(mean(Qq_var1, na.rm = TRUE), 0),
                               Qq_var2 = round(mean(Qq_var2, na.rm = TRUE), 0),
                               Qq_var3 = round(mean(Qq_var3, na.rm = TRUE), 0),
                               Qq_var4 = round(mean(Qq_var4, na.rm = TRUE), 0),
                               Qq_var5 = round(mean(Qq_var5, na.rm = TRUE), 0),
                               Qq_var6 = round(mean(Qq_var6, na.rm = TRUE), 0),
                               Qq_var7 = round(mean(Qq_var7, na.rm = TRUE), 0),
                               Qq_var8 = round(mean(Qq_var8, na.rm = TRUE), 0),
                               Qq_var9 = round(mean(Qq_var9, na.rm = TRUE), 0),
                               Qq_var10 = round(mean(Qq_var10, na.rm = TRUE), 0),
                               Qq_var11 = round(mean(Qq_var11, na.rm = TRUE), 0),
                               Qq_var12 = round(mean(Qq_var12, na.rm = TRUE), 0),
                               Qq_var13 = round(mean(Qq_var13, na.rm = TRUE), 0),
                               Qq_var14 = round(mean(Qq_var14, na.rm = TRUE), 0),
                               Qq_var15 = round(mean(Qq_var15, na.rm = TRUE), 0),
                               Qq_var16 = round(mean(Qq_var16, na.rm = TRUE), 0),
                               Qq_var17 = round(mean(Qq_var17, na.rm = TRUE), 0),
                               Qq_var18 = round(mean(Qq_var18, na.rm = TRUE), 0),
                               Count = n())


avarage_qqAnalysis <- avarage_qqAnalysis %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(Qq_var1,Qq_var2,Qq_var3,Qq_var4,Qq_var5,Qq_var6,Qq_var7,Qq_var8,Qq_var9,Qq_var10,Qq_var11,Qq_var12,Qq_var13,Qq_var14,Qq_var15,Qq_var16,Qq_var17,Qq_var18)),0))

attach(avarage_qqAnalysis)
avarage_qqAnalysis = avarage_qqAnalysis %>%
  add_row(
    unit = "Mean",
    Qq_var1 = round(mean(Qq_var1), 0),
    Qq_var2 = round(mean(Qq_var2), 0),
    Qq_var3 = round(mean(Qq_var3), 0),
    Qq_var4 = round(mean(Qq_var4), 0),
    Qq_var5 = round(mean(Qq_var5), 0),
    Qq_var6 = round(mean(Qq_var6), 0),
    Qq_var7 = round(mean(Qq_var7), 0),
    Qq_var8 = round(mean(Qq_var8), 0),
    Qq_var9 = round(mean(Qq_var9), 0),
    Qq_var10 = round(mean(Qq_var10), 0),
    Qq_var11 = round(mean(Qq_var11), 0),
    Qq_var12 = round(mean(Qq_var12), 0),
    Qq_var13 = round(mean(Qq_var13), 0),
    Qq_var14 = round(mean(Qq_var14), 0),
    Qq_var15 = round(mean(Qq_var15), 0),
    Qq_var16 = round(mean(Qq_var16), 0),
    Qq_var17 = round(mean(Qq_var17), 0),
    Qq_var18 = round(mean(Qq_var18), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )

View(avarage_qqAnalysis)

#F: Information Systems (IS)
avarage_IFSystems = summarise(by_unit,
                              IF_var1 = round(mean(IF_var1, na.rm = TRUE), 0),
                              IF_var2 = round(mean(IF_var2, na.rm = TRUE), 0),
                              IF_var3 = round(mean(IF_var3, na.rm = TRUE), 0),
                              IF_var4 = round(mean(IF_var4, na.rm = TRUE), 0),
                              IF_var5 = round(mean(IF_var5, na.rm = TRUE), 0),
                              IF_var6 = round(mean(IF_var6, na.rm = TRUE), 0),
                              IF_var7 = round(mean(IF_var7, na.rm = TRUE), 0),
                              IF_var8 = round(mean(IF_var8, na.rm = TRUE), 0),
                              IF_var9 = round(mean(IF_var9, na.rm = TRUE), 0),
                              IF_var10 = round(mean(IF_var10, na.rm = TRUE), 0),
                              IF_var11 = round(mean(IF_var11, na.rm = TRUE), 0),
                              IF_var12 = round(mean(IF_var12, na.rm = TRUE), 0),
                              IF_var13 = round(mean(IF_var13, na.rm = TRUE), 0),
                              Count = n())

avarage_IFSystems <- avarage_IFSystems %>% 
  rowwise() %>% 
  mutate(Mean=round(mean(c(IF_var1,IF_var2,IF_var3,IF_var4,IF_var5,IF_var6,IF_var7,IF_var8,IF_var9,IF_var10,IF_var11,IF_var12,IF_var13)),0))

attach(avarage_IFSystems)
avarage_IFSystems = avarage_IFSystems %>%
  add_row(
    unit = "Mean",
    IF_var1 = round(mean(IF_var1), 0),
    IF_var2 = round(mean(IF_var2), 0),
    IF_var3 = round(mean(IF_var3), 0),
    IF_var4 = round(mean(IF_var4), 0),
    IF_var5 = round(mean(IF_var5), 0),
    IF_var6 = round(mean(IF_var6), 0),
    IF_var7 = round(mean(IF_var7), 0),
    IF_var8 = round(mean(IF_var8), 0),
    IF_var9 = round(mean(IF_var9), 0),
    IF_var10 = round(mean(IF_var10), 0),
    IF_var11 = round(mean(IF_var11), 0),
    IF_var12 = round(mean(IF_var12), 0),
    IF_var13 = round(mean(IF_var13), 0),
    Count = round(mean(Count), 0),
    Mean = round(mean(Mean), 0)
  )
View(avarage_IFSystems)


