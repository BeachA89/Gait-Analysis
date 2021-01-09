library(readxl)
library(reshape2)
library(tibble)
library(stringr)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)

#lapply excel files

filePaths <- list.files("data", full.names = TRUE, pattern = "\\.xlsx") #~$ is a temporary file if the document is open
dataname <-  str_remove_all(filePaths, ".xlsx")
dataname <-  str_remove_all(dataname, "data/")
labels <-  t(data.frame(strsplit(dataname, "_")))
labels <-  as.data.frame(labels, stringsAsFactors = FALSE)
rownames(labels) <- NULL

CombinedTS = list()
CombinedTS_all = list()

#### Process TS data ####

for  (i in 1:length(filePaths)){
  labelsTS <-  labels[i,]
  ColumnsTS = c("Name","Distance", "Date","Type", "Trial", "Kinematic", 0:100)
  
  SheetsTS = c("Left1Kin","Left2Kin","Left3Kin", "Right1Kin", "Right2Kin", "Right3Kin", "AvgLeft", "AvgRight")
  TrialsTS = c("Left_1","Left_2","Left_3", "Right_1", "Right_2", "Right_3", "Left_Avg", "Right_Avg")
  for (j in 1:length(SheetsTS)){
    a = SheetsTS[[j]]
    b = TrialsTS[[j]]
    
    Trial <- read_excel(filePaths[[i]], sheet = a, col_types = "numeric")
    Trial <- column_to_rownames(Trial, "Percentage of Stance Phase")
    Trial <-  data.frame(t(Trial))
    Trial <-  rownames_to_column(Trial)
    #Trialname <- b
    Trial <- data.frame(labelsTS, b, Trial)
    colnames(Trial) <-  ColumnsTS
    CombinedTS[[j]] <-  data.frame(Trial)
    
  }
  CombinedTS_all[[i]] = rbindlist(CombinedTS, fill=TRUE)
}

CombinedTS_all <-  rbindlist(CombinedTS_all, fill=TRUE)
colnames(CombinedTS_all) <-  ColumnsTS
CombinedTS_all$Name = as.character(CombinedTS_all$Name)
CombinedTS_all$Date = as.character(CombinedTS_all$Date)
CombinedTS_all$Type = as.character(CombinedTS_all$Type)
CombinedTS_all$Trial = as.character(CombinedTS_all$Trial)

#### Process ST data ####
column_Names_ST <-  c("Name","Date","Type", "Trial","Cadence (steps/min):", "Walking Speed (m/s):", "Stride Time (s):", "Foot Off (%):", "Stride Length (m):","Step Width (m):", "Swing Phase (%):", 
                      "Stance Phase (%):", "Double Support (s):","Stride Length (%):", 	"GRF count (s):", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio", "COMHeight Cont", "COMHeight min", "COMHeight TO", 
  "COMHeight range", "COMHeight ratio", "Hip to Toe Contact", "Propulsive PC", "Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Pelvis XRom", "Pelvis YRom", 
  "Pelvis ZRom", "Pelvis Xvelmax", "Pelvis Yvelmax", "Pelvis Zvelmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Thorax XRom", "Thorax YRom", 
  "Thorax ZRom", "Thorax Xvelmax", "Thorax Yvelmax", "Thorax Zvelmax", "Hip Xmax", "Hip Ymax", "Hip Zmax", "Hip XRom", "Hip YRom", "Hip ZRom", 
  "Hip Xvelmax", "Hip Yvelmax", "Hip Zvelmax", "Knee Xmax", "Knee Ymax", "Knee Zmax", "Knee XRom", "Knee YRom", "Knee ZRom", "Knee Xvelmax", 
  "Knee Yvelmax", "Knee Zvelmax", "Ankle Xmax", "Ankle XRom", "Ankle Xvelmax", "Foot Prog Xmax", "Foot Prog XRom", "Foot Prog Xvelmax")


CombinedST <-  list()
CombinedST_all <-  list()
SheetsST = c("Left1ST","Left2ST","Left3ST", "Right1ST", "Right2ST", "Right3ST")
TrialsST = c("Left_1","Left_2","Left_3", "Right_1", "Right_2", "Right_3")

for  (i in 1:length(filePaths)){


  labelsST <-  labels[i,]

  for (j in 1:length(SheetsST)){
    a = SheetsST[[j]]
    b = TrialsST[[j]]
    b = as.character(b)
    
    TrialST <-read_excel(filePaths[[i]], sheet = a)
    STdata <- c(as.numeric(TrialST[3,4]), as.numeric(TrialST[4,4]), as.numeric(TrialST[5,4]),as.numeric(TrialST[8,4]),as.numeric(TrialST[9,4]), as.numeric(TrialST[10,4]),
                         as.numeric(TrialST[15,4]), 100-(as.numeric(TrialST[15,4])), as.numeric(TrialST[16,4]), as.numeric(TrialST[18,4]), as.numeric(TrialST[17,4])/1000, 
                as.numeric(TrialST[2,7]), as.numeric(TrialST[3,7]), as.numeric(TrialST[4,7]),
                         as.numeric(TrialST[5,7]), as.numeric(TrialST[6,7]), as.numeric(TrialST[2,8]), as.numeric(TrialST[3,8]), as.numeric(TrialST[4,8]), 
                         as.numeric(TrialST[5,8]), as.numeric(TrialST[6,8]), as.numeric(TrialST[2,9]), as.numeric(TrialST[2,10]), as.numeric(TrialST[2,12]),
                         as.numeric(TrialST[3,12]), as.numeric(TrialST[4,12]), as.numeric(TrialST[5,12]), as.numeric(TrialST[6,12]), as.numeric(TrialST[7,12]),
                         as.numeric(TrialST[8,12]), as.numeric(TrialST[9,12]), as.numeric(TrialST[10,12]), as.numeric(TrialST[2,13]), as.numeric(TrialST[3,13]),
                         as.numeric(TrialST[4,13]), as.numeric(TrialST[5,13]), as.numeric(TrialST[6,13]), as.numeric(TrialST[7,13]), as.numeric(TrialST[8,13]), 
                         as.numeric(TrialST[9,13]), as.numeric(TrialST[10,13]), as.numeric(TrialST[2,14]), as.numeric(TrialST[3,14]), as.numeric(TrialST[4,14]), 
                         as.numeric(TrialST[5,14]), as.numeric(TrialST[6,14]), as.numeric(TrialST[7,14]), as.numeric(TrialST[8,14]), as.numeric(TrialST[9,14]), 
                         as.numeric(TrialST[10,14]), as.numeric(TrialST[2,15]), as.numeric(TrialST[3,15]), as.numeric(TrialST[4,15]), as.numeric(TrialST[5,15]), 
                         as.numeric(TrialST[6,15]), as.numeric(TrialST[7,15]), as.numeric(TrialST[8,15]), as.numeric(TrialST[9,15]), as.numeric(TrialST[10,15]), 
                         as.numeric(TrialST[2,16]), as.numeric(TrialST[5,16]), as.numeric(TrialST[8,16]), as.numeric(TrialST[2,17]), as.numeric(TrialST[5,17]), 
                         as.numeric(TrialST[8,17]))
    
    STdata <-  data.frame(t(STdata))
    #STdata <-  rownames_to_column(STdata)
    
    #Trialname <- b
    STdata <- data.frame(labelsST, b, STdata)
    STdata$b <- as.character(STdata$b)
    CombinedST[[j]] <-  data.frame(STdata)
    
  }
  CombinedST_all[[i]] <- rbindlist(CombinedST, fill=TRUE)
  
}
CombinedST_all <-  rbindlist(CombinedST_all, fill=TRUE)

colnames(CombinedST_all) <-  column_Names_ST

#CombinedST_all$Name = as.character(CombinedST_all$Name)
#CombinedST_all$Date = as.character(CombinedST_all$Date)
#CombinedST_all$Type = as.character(CombinedST_all$Type)
#CombinedST_all$Trial = as.character(CombinedST_all$Trial)


#### GGPLOTS ####

Line_Colours = c("green4", "green4","green4","green4", "red", "red", "red", "red")
Line_Types = c("dashed", "dashed", "dashed", "solid", "dashed", "dashed", "dashed", "solid")
Line_Size = c(0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 1)

# Filter by Athlete Type Date and melt
FilterTS_Athlete_Type_Date = CombinedTS_all %>% filter(Name == "Jenny Blundell") %>% filter(Type == "Stance") %>% filter(Date == "20180511")%>% reshape2::melt(id = c("Name", "Date", "Type", "Trial", "Kinematic")) %>% mutate(variable = as.numeric(as.character(variable)))

Variable_list = c("COM velocity", "COM height / BH", "Z GRF", "X GRF", "ankle dorsiflexion", "knee flexion", "hip flexion", 
                  "pelvis tilt", "pelvis obliquity", "pelvis rotation",
                  "thorax tilt", "thorax obliquity", "thorax rotation",
                  "ankle dorsiflexion vel", "knee flexion vel", "hip flexion vel",
                  "ankle plantarflexor moment", "knee extensor moment", "hip extensor moment",
                  "total ankle power", "total knee power", "total hip power")

Plot_names = c("COM_velocity", "COM_height/BH", "Z_GRF", "X_GRF", "ankle_dorsiflexion", "knee_flexion", "hip_flexion", 
                  "pelvis_tilt", "pelvis_obliquity", "pelvis_rotation",
                  "thorax_tilt", "thorax_obliquity", "thorax_rotation",
                  "ankle_dorsiflexion_vel", "knee_flexion_vel", "hip_flexion_vel",
                  "ankle_plantarflexor_moment", "knee_extensor_moment", "hip_extensor_moment",
                  "total_ankle_power", "total_knee_power", "total_hip_power")


#ggplot loop
for (i in 1:length(Variable_list)){
  a = Variable_list[[i]]
ggplot1 <-  FilterTS_Athlete_Type_Date %>% filter(Kinematic == a) %>% ggplot(aes(variable, value, colour = Trial, linetype = Trial, size =Trial)) + geom_line()  + scale_linetype_manual(values = Line_Types) + scale_color_manual(values=Line_Colours) + scale_size_manual(values = Line_Size) +
  ggtitle(a) + theme(plot.title = element_text(hjust = 0.5))
  assign(paste0("ggplot_",Plot_names[[i]]),ggplot1)
}

#### TableOutput ####

FilterST_Athlete_Type_Date = CombinedST_all %>% filter(Name == "Jenny Blundell") %>% filter(Type == "Stance") %>% filter(Date == "20180511")#%>% reshape2::melt(id = c("Name", "Date", "Type", "Trial"))

#ST table

STtable <-  FilterST_Athlete_Type_Date %>% select("Trial","Stance Phase (%):",  "Swing Phase (%):","Stride Length (m):", "Stride Length (%):", "Step Width (m):", "Double Support (s):",  "GRF count (s):", "Hip to Toe Contact", "Cadence (steps/min):", "Propulsive PC") 
                                                                        
                                                  
COMVeltable <-  FilterST_Athlete_Type_Date %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
COMHeighttable <-  FilterST_Athlete_Type_Date %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
                                                  
JointAngletable <- FilterST_Athlete_Type_Date %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
PelvicThoraxAngletable <- FilterST_Athlete_Type_Date %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")

JointVeltable <- FilterST_Athlete_Type_Date %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")


                                                  
# Spare ST variables "Pelvis Xvelmax", "Pelvis Yvelmax", "Pelvis Zvelmax",   
#"Thorax Xvelmax", "Thorax Yvelmax", "Thorax Zvelmax", , "Hip Ymax", "Hip Zmax", , "Hip YRom", "Hip ZRom", 
#"Hip Yvelmax", "Hip Zvelmax", , "Knee Ymax", "Knee Zmax", , "Knee YRom", "Knee ZRom",  
#"Knee Yvelmax", "Knee Zvelmax", , , , "Foot Prog Xmax", "Foot Prog XRom", "Foot Prog Xvelmax")
# "Walking Speed (m/s):", "Stride Time (s):", "Foot Off (%):",



datatable(STtable,rownames = NULL, options = list(dom='t'))
datatable(COMVeltable,rownames = NULL, options = list(dom='t'))
datatable(COMHeighttable,rownames = NULL, options = list(dom='t'))
datatable(JointAngletable,rownames = NULL, options = list(dom='t'))
datatable(PelvicThoraxAngletable,rownames = NULL, options = list(dom='t'))
datatable(JointVeltable,rownames = NULL, options = list(dom='t'))



for (i in 1:length(Variable_list)){
  a = Variable_list[[i]]
  ggplot1 <-  Filter_Athlete_Type_Date %>% filter(Kinematic == a) %>% ggplot(aes(variable, value, colour = Trial, linetype = Trial, size =Trial)) + geom_line()  + scale_linetype_manual(values = Line_Types) + scale_color_manual(values=Line_Colours) + scale_size_manual(values = Line_Size) +
    ggtitle(a) + theme(plot.title = element_text(hjust = 0.5))
  assign(paste0("ggplot_",Plot_names[[i]]),ggplot1)
}

