#load libraries
library(ggplot2)
library(dplyr)
library(reshape2)

#Read data into R
data_1 <- read.csv("~/work/capacity.csv", header = TRUE, strip.white=TRUE)

#get all row with yes.

cleaned_data <- filter(data_1, Do.you.wish.to.participate.in.this.assessment. == "Yes")
View(cleaned_data)

#rename several coloumns
cleaned_data = rename(cleaned_data,position = X3..What.is.your.current.position)
cleaned_data = rename(cleaned_data,conset = Do.you.wish.to.participate.in.this.assessment.)
cleaned_data = rename(cleaned_data,department = X1..Which.Department.do.you.belong.to.)
cleaned_data = rename(cleaned_data,unit = X2..Which.Unit.do.you.belong.to.)
cleaned_data = select(cleaned_data, -If.you.belong.to.another.department...Kindly.specify.here.)
cleaned_data = select(cleaned_data, -If.you.belong.to.another.Unit...Kindly.specify.here.)

#experience variables
cleaned_data = rename(cleaned_data,time_current = X4..How.long.have.you.been.in.your.current.position..In.months.)
cleaned_data = rename(cleaned_data,time_industry = X5..Experience.in.your.current.area.of.work..In.months.)
#level of education
cleaned_data = rename(cleaned_data,level_education = X6..Highest.level.of.education..Specify.level.of.training.and.actual.area.of.training.)
#training data
cleaned_data = rename(cleaned_data,training = X7..Have.you.done.any.training.in.your.current.area.of.work.)
#rename training done
cleaned_data = rename(cleaned_data,training_done = X8..If.yes..please.specify)

#renaming need for training on research
cleaned_data = rename(cleaned_data,kwld_rsch = X9.What.is.your.overall.assessment.in.relation.to.your.knowledge.and.skills.in.Research..Monitoring.Evaluation.and.Learning.in.relation.to.what.you.do.)
cleaned_data = rename(cleaned_data,need_training = X10..Do.you.require.any.training.in.Research.Monitoring.Evaluation.and.Learning...)


#count Unique departments
by_department = group_by(cleaned_data, department)
unique_dpt = summarise(by_department, count = n())

#create a bar plot
ggplot(unique_dpt, aes(department, count, fill = department))+
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

#Analysis of unique positions reported back in the survey.
 position <- tolower( position)

# returns string w/o trailing whitespace
trim.trailing <- function (x) sub("\\s+$", "", x)

 position <-trim.trailing( position)

by_position = group_by(cleaned_data, position)
unique_position = summarise(by_position, count = n())

#table actual positions reported

#How long have you been in your current position
#Develop a box plot
bxplt_data_1 <- data.frame(group = "current", value =  time_current)
bxplt_data_2 <- data.frame(group = "industry", value =  time_industry)
bxplt_data <- rbind(bxplt_data_1,bxplt_data_2)

#develop a box plot for distribution of experience
ggplot(data = bxplt_data, aes(group, value, fill = group))+
  geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4)


#Analysis of unique qualifications  reported back in the survey.
 level_education <- tolower( level_education)
 level_education <-trim.trailing( level_education)
by_edlevel = group_by(cleaned_data, level_education)
unique_edlevel = summarise(by_edlevel, count = n())

#Analysis on training required per unit
training_data <- cleaned_data[,c('training', 'unit', 'department')]
grouped_training = group_by(training_data, unit)
#unit per training
unit_training <- summarise(grouped_training, Yes = sum(training == 'Yes'), No = sum(training == 'No'))

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

#Tabling training done per departement 
 training_done <- tolower( training_done)

table_trainingby <- group_by(cleaned_data, training_done)
table_training <- summarise(table_trainingby, count = n())

#Analysis knowledge in research research.
knowledge_by <- group_by(cleaned_data, kwld_rsch)
table_kwld <- summarise(knowledge_by, count = n()) %>% mutate(Percantage = paste(round((count/sum(count)*100),1),"%", sep = ""))

#Analysis of people in need of trainig
nd_trby <- group_by(cleaned_data, need_training)
table_ndtr <- summarise(nd_trby, count = n()) %>% mutate(Percantage = paste(round((count/sum(count)*100),0),"%", sep = ""))


#Analysis of research part ****

#Variable defination
# R_var1 Knowledge and skills in Research Ethics and Responsible Conduct of Research
# R_var2 Developing study budgets and work plans
# R_var3 Writing Evaluation/Study Terms of Reference (TOR)
# R_var4 Developing Research and Evaluations study designs (both Experiment and non-experiment study designs)
# R_var5 Writing Operational Research study Proposals
# R_var6 Developing Operational Research study protocols
# R_var7 Knowledge of ethical approval for studies
# R_var8 Developing Study data collection tools
# R_var9 Knowledge of data collection methods and techniques
# R_var10 Knowledge of Sample size estimation
# R_var11 Knowledge of qualitative Sampling techniques
# R_var12 Knowledge of quantitative Sampling techniques
# R_var13 Conducting Evaluations
# R_var14 Writing Research and Evaluation Reports
# R_var15 Study results disseminations
# R_var16 Using study findings to improve programs / Operationalisation of the study results to inform program decisions


# #Renaming of varibales
research_names = c('R_var1','R_var2','R_var3','R_var4','R_var5','R_var6','R_var7','R_var8','R_var9','R_var10','R_var11','R_var12','R_var13','R_var14','R_var15','R_var16')
#Renaming research questions
cleaned_data = cleaned_data %>% rename_at(vars(starts_with('A..Research..')), ~ research_names)

#Renaming monitoring questions
monitoring_names = c('M_var1','M_var2','M_var3','M_var4','M_var5','M_var6','M_var7','M_var8','M_var9','M_var10','M_var11')
#change monitoring names
cleaned_data = cleaned_data %>% rename_at(vars(starts_with('B..Monitoring.and.Evaluation.')), ~ monitoring_names)

#Rename learning questions
learning_names = c('L_var1','L_var2','L_var3','L_var4','L_var5','L_var6','L_var7','L_var8','L_var9','L_var10','L_var11','L_var12','L_var13','L_var14')
cleaned_data = cleaned_data %>% rename_at(vars(starts_with('C..Learning..')), ~ learning_names)

#Renaming Data Collection questions
collection_names = c('D_var1','D_var2','D_var3','D_var4','D_var5','D_var6','D_var7','D_var8','D_var9','D_var10')
cleaned_data = cleaned_data %>% rename_at(vars(starts_with('D..Data.Collection.')), ~ collection_names)

#Renaming Data_mgt questions
data_mgt_names = c('Qq_var1','Qq_var2','Qq_var3','Qq_var4','Qq_var5','Qq_var6','Qq_var7','Qq_var8','Qq_var9','Qq_var10','Qq_var11','Qq_var12','Qq_var13','Qq_var14','Qq_var15','Qq_var16','Qq_var17','Qq_var18')
cleaned_data = cleaned_data %>% rename_at(vars(starts_with('E..Qualitative')), ~ data_mgt_names)


#Analysis on data
avarage