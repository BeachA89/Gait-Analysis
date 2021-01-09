
server <- function(input, output) {
  observeEvent(input$goButton, {
    ############### IMPORTDATA ###############
    #filePaths <<- isolate(list.files(input$Files$datapath, full.names = TRUE, pattern = "\\.xlsx")) #~$ is a temporary file if the document is open
    #### Create Labels ####
    dataname <- input$Files[['name']]
    dataname <-  str_remove_all(dataname, ".xlsx")
    #dataname <<-  str_remove_all(dataname, "data/")
    labels <-  t(data.frame(strsplit(dataname, "_")))
    labels <-  as.data.frame(labels, stringsAsFactors = FALSE)
    rownames(labels) <- NULL
    
    CombinedTS = list()
    CombinedTS_all = list()
    
    #### Process TS data ####
    
    for  (i in 1:length(input$Files$datapath)){
      labelsTS <-  labels[i,]
      ColumnsTS = c("Name","Distance", "Date","Type", "Trial", "Kinematic", 0:100)
      
      SheetsTS = c("Left1Kin","Left2Kin","Left3Kin", "Right1Kin", "Right2Kin", "Right3Kin", "AvgLeft", "AvgRight")
      TrialsTS = c("Left_1","Left_2","Left_3", "Right_1", "Right_2", "Right_3", "Left_Avg", "Right_Avg")
      for (j in 1:length(SheetsTS)){
        a = SheetsTS[[j]]
        b = TrialsTS[[j]]
        
        Trial <- read_excel(input$Files$datapath[[i]], sheet = a, col_types = "numeric")
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
    column_Names_ST <-  c("Name","Distance","Date","Type", "Trial","Cadence (steps/min)", "Walking Speed (m/s)", "Stride Time (s)", "Foot Off (%)", "Stride Length (m)","Step Width (m)", "Swing Phase (%)", 
                          "Stance Phase (%)", "Double Support (s)","Stride Length (%)", "Contact Time (s)", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio", "COMHeight Cont", "COMHeight min", "COMHeight TO", 
                          "COMHeight range", "COMHeight ratio", "Hip to Toe Contact (mm)", "Propulsion (% cycle)", "Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Pelvis XRom", "Pelvis YRom", 
                          "Pelvis ZRom", "Pelvis Xvelmax", "Pelvis Yvelmax", "Pelvis Zvelmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Thorax XRom", "Thorax YRom", 
                          "Thorax ZRom", "Thorax Xvelmax", "Thorax Yvelmax", "Thorax Zvelmax", "Hip Xmax", "Hip Ymax", "Hip Zmax", "Hip XRom", "Hip YRom", "Hip ZRom", 
                          "Hip Xvelmax", "Hip Yvelmax", "Hip Zvelmax", "Knee Xmax", "Knee Ymax", "Knee Zmax", "Knee XRom", "Knee YRom", "Knee ZRom", "Knee Xvelmax", 
                          "Knee Yvelmax", "Knee Zvelmax", "Ankle Xmax", "Ankle XRom", "Ankle Xvelmax", "Foot Prog Xmax", "Foot Prog XRom", "Foot Prog Xvelmax")
    
    
    CombinedST <-  list()
    CombinedST_all <-  list()
    SheetsST = c("Left1ST","Left2ST","Left3ST", "Right1ST", "Right2ST", "Right3ST")
    TrialsST = c("Left_1","Left_2","Left_3", "Right_1", "Right_2", "Right_3")
    
    for  (i in 1:length(input$Files$datapath)){
      
      
      labelsST <-  labels[i,]
      
      for (j in 1:length(SheetsST)){
        a = SheetsST[[j]]
        b = TrialsST[[j]]
        b = as.character(b)
        
        TrialST <-read_excel(input$Files$datapath[[i]], sheet = a)
        STdata <- c(round(as.numeric(TrialST[3,4]),2), round(as.numeric(TrialST[4,4]),2), round(as.numeric(TrialST[5,4]),2),round(as.numeric(TrialST[8,4]),2),round(as.numeric(TrialST[9,4]),2), 
                    round(as.numeric(TrialST[10,4]),2), round(as.numeric(TrialST[15,4]),2), round(100-(as.numeric(TrialST[15,4])),2), round(as.numeric(TrialST[16,4]),2), round(as.numeric(TrialST[18,4]),2), 
                    round((as.numeric(TrialST[17,4])/1000),2), round(as.numeric(TrialST[2,7]),2), round(as.numeric(TrialST[3,7]),2), round(as.numeric(TrialST[4,7]),2),
                    round(as.numeric(TrialST[5,7]),2), round(as.numeric(TrialST[6,7]),2), round(as.numeric(TrialST[2,8]),2), round(as.numeric(TrialST[3,8]),2), round(as.numeric(TrialST[4,8]),2), 
                    round(as.numeric(TrialST[5,8]),2), round(as.numeric(TrialST[6,8]),2), round(as.numeric(TrialST[2,9]),2), round(as.numeric(TrialST[2,10]),2), round(as.numeric(TrialST[2,12]),2),
                    round(as.numeric(TrialST[3,12]),2), round(as.numeric(TrialST[4,12]),2), round(as.numeric(TrialST[5,12]),2), round(as.numeric(TrialST[6,12]),2), round(as.numeric(TrialST[7,12]),2),
                    round(as.numeric(TrialST[8,12]),2), round(as.numeric(TrialST[9,12]),2), round(as.numeric(TrialST[10,12]),2), round(as.numeric(TrialST[2,13]),2), round(as.numeric(TrialST[3,13]),2),
                    round(as.numeric(TrialST[4,13]),2), round(as.numeric(TrialST[5,13]),2), round(as.numeric(TrialST[6,13]),2), round(as.numeric(TrialST[7,13]),2), round(as.numeric(TrialST[8,13]),2), 
                    round(as.numeric(TrialST[9,13]),2), round(as.numeric(TrialST[10,13]),2), round(as.numeric(TrialST[2,14]),2), round(as.numeric(TrialST[3,14]),2), round(as.numeric(TrialST[4,14]),2), 
                    round(as.numeric(TrialST[5,14]),2), round(as.numeric(TrialST[6,14]),2), round(as.numeric(TrialST[7,14]),2), round(as.numeric(TrialST[8,14]),2), round(as.numeric(TrialST[9,14]),2), 
                    round(as.numeric(TrialST[10,14]),2), round(as.numeric(TrialST[2,15]),2), round(as.numeric(TrialST[3,15]),2), round(as.numeric(TrialST[4,15]),2), round(as.numeric(TrialST[5,15]),2), 
                    round(as.numeric(TrialST[6,15]),2), round(as.numeric(TrialST[7,15]),2), round(as.numeric(TrialST[8,15]),2), round(as.numeric(TrialST[9,15]),2), round(as.numeric(TrialST[10,15]),2), 
                    round(as.numeric(TrialST[2,16]),2), round(as.numeric(TrialST[5,16]),2), round(as.numeric(TrialST[8,16]),2), round(as.numeric(TrialST[2,17]),2), round(as.numeric(TrialST[5,17]),2), 
                    round(as.numeric(TrialST[8,17]),2))
        
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
    
    CombinedST_all$Name = as.character(CombinedST_all$Name)
    CombinedST_all$Distance = as.character(CombinedST_all$Distance)
    CombinedST_all$Date = as.character(CombinedST_all$Date)
    CombinedST_all$Type = as.character(CombinedST_all$Type)
    CombinedST_all$Trial = as.character(CombinedST_all$Trial)
    
    
    
    
    
    
    ############# FILTER DATA #############################
    
    ##### Session 1 Filters ####
    
    tab_Filtered_S1_TS <-  reactive({
      
      CombinedTS_all %>%
        filter(Name == input$Name) %>%
        filter(Distance == input$Distance) %>%
        filter(Date == input$Date) %>%
        filter(Type == input$Type) %>%
        reshape2::melt(id = c("Name", "Distance", "Date", "Type", "Trial", "Kinematic")) %>%
        mutate(variable = as.numeric(as.character(variable)))
    })
    
    output$select_Name <-  renderUI({
      
      selectizeInput('Name', 'Select Name', choices = c("select" = "", unique(CombinedTS_all$Name)))
    })
    
    output$select_Distance <-  renderUI({
      inputName = as.character(input$Name)
      choice_Distance <- reactive({
        CombinedTS_all %>%
          filter(Name == inputName) %>%
          pull(Distance) %>%
          as.character()
        
        
      })
      
      
      selectizeInput('Distance', 'Select Distance', choices = c("select" = "", choice_Distance()))
    })
    
    
    output$select_Date <-  renderUI({
      inputName = as.character(input$Name)
      inputDistance = as.character(input$Distance)
      choice_Date <- reactive({
        CombinedTS_all %>%
          filter(Name == inputName) %>%
          filter(Distance == inputDistance) %>%
          pull(Date) %>%
          as.character()
        
        
      })
      
      
      selectizeInput('Date', 'Select Date', choices = c("select" = "", choice_Date()))
      
    })
    
    
    
    
    
    tab_Filtered_S1_ST <-  reactive({
      
      CombinedST_all %>%
        filter(Name == input$Name) %>%
        filter(Distance == input$Distance) %>%
        filter(Date == input$Date) %>%
        filter(Type == input$Type) %>%
        mutate(Session = paste("Session 1", Date, Trial))
    })
    
    
    tab_Filtered_S1_ST_LeftAvg <-  reactive({
      CombinedST_all %>%
        filter(Name == input$Name) %>%
        filter(Distance == input$Distance) %>%
        filter(Date == input$Date) %>%
        filter(Type == input$Type) %>%
        mutate(Session = paste("Session 1", Date, Trial))%>%
        filter(Trial %in% c("Left_1", "Left_2", "Left_3")) %>%
        reshape2::melt(id = c("Name", "Distance", "Type","Date", "Trial", "Session")) %>%
        group_by(variable) %>%
        dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                         Average = mean(value), 
                         LowerLimit = CI(value, ci=0.95)[3]) %>%
        mutate(Average = round(Average, digits = 2)) %>%
        mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
        mutate(LowerLimit = round(LowerLimit, digits = 2)) %>%
        mutate(variable = as.character(variable)) 
    })
    
    
    
    
    tab_Filtered_S1_ST_RightAvg <-  reactive({
      CombinedST_all %>%
        filter(Name == input$Name) %>%
        filter(Distance == input$Distance) %>%
        filter(Date == input$Date) %>%
        filter(Type == input$Type) %>%
        mutate(Session = paste("Session 1", Date, Trial))%>%
        filter(Trial %in% c("Right_1", "Right_2", "Right_3")) %>%
        reshape2::melt(id = c("Name", "Distance", "Type","Date", "Trial", "Session")) %>%
        group_by(variable) %>%
        dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                         Average = mean(value), 
                         LowerLimit = CI(value, ci=0.95)[3]) %>%
        mutate(Average = round(Average, digits = 2)) %>%
        mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
        mutate(LowerLimit = round(LowerLimit, digits = 2)) %>%
        mutate(variable = as.character(variable)) #%>%
      #filter(variable == "Propulsion (% cycle)") %>%
      #print(CombinedST_all)
    })
    
    ################## Session 2 filters #######################
    
    tab_Filtered_S2_TS <-  reactive({
      
      CombinedTS_all %>%
        filter(Name == input$Name2) %>%
        filter(Distance == input$Distance2) %>%
        filter(Date == input$Date2) %>%
        filter(Type == input$Type) %>%
        reshape2::melt(id = c("Name", "Distance", "Date", "Type", "Trial", "Kinematic")) %>%
        mutate(variable = as.numeric(as.character(variable)))
    })
    
    output$select_Name2 <-  renderUI({
      if (input$Report_Type == "Two Sessions"){
        
        selectizeInput('Name2', 'Select Name 2', choices = c("select" = "", unique(CombinedTS_all$Name)))
      }
    })
  
  output$select_Distance2 <-  renderUI({
    inputName2 = as.character(input$Name2)
    choice_Distance2 <- reactive({
      CombinedTS_all %>%
        filter(Name == inputName2) %>%
        pull(Distance) %>%
        as.character()
      
      
    })
    
    
    selectizeInput('Distance2', 'Select Distance 2', choices = c("select" = "", choice_Distance2()))
  })
  
  
  output$select_Date2 <-  renderUI({
    inputName2 = as.character(input$Name2)
    inputDistance2 = as.character(input$Distance2)
    choice_Date2 <- reactive({
      CombinedTS_all %>%
        filter(Name == inputName2) %>%
        filter(Distance == inputDistance2) %>%
        pull(Date) %>%
        as.character()
      
      
    })
    
    
    selectizeInput('Date2', 'Select Date 2', choices = c("select" = "", choice_Date2()))
    
  })
  
  
  
  tab_Filtered_S2_ST <-  reactive({
    
    CombinedST_all %>%
      filter(Name == input$Name2) %>%
      filter(Distance == input$Distance2) %>%
      filter(Date == input$Date2) %>%
      filter(Type == input$Type)%>%
      mutate(Session = paste("Session 2", Date, Trial))
  })
  
  tab_Filtered_S2_ST_LeftAvg <-  reactive({
    CombinedST_all %>%
      filter(Name == input$Name2) %>%
      filter(Distance == input$Distance2) %>%
      filter(Date == input$Date2) %>%
      filter(Type == input$Type) %>%
      mutate(Session = paste("Session 2", Date, Trial))%>%
      filter(Trial %in% c("Left_1", "Left_2", "Left_3")) %>%
      reshape2::melt(id = c("Name", "Distance", "Type","Date", "Trial", "Session")) %>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value), 
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2)) %>%
      mutate(variable = as.character(variable))
  })
  
  
  
  
  tab_Filtered_S2_ST_RightAvg <-  reactive({
    CombinedST_all %>%
      filter(Name == input$Name2) %>%
      filter(Distance == input$Distance2) %>%
      filter(Date == input$Date2) %>%
      filter(Type == input$Type) %>%
      mutate(Session = paste("Session 2", Date, Trial))%>%
      filter(Trial %in% c("Right_1", "Right_2", "Right_3")) %>%
      reshape2::melt(id = c("Name", "Distance", "Type","Date", "Trial", "Session")) %>%
      group_by(variable) %>%
      dplyr::summarise(UpperLimit = CI(value, ci=0.95)[1],
                       Average = mean(value), 
                       LowerLimit = CI(value, ci=0.95)[3]) %>%
      mutate(Average = round(Average, digits = 2)) %>%
      mutate(UpperLimit = round(UpperLimit, digits = 2))%>%
      mutate(LowerLimit = round(LowerLimit, digits = 2)) %>%
      mutate(variable = as.character(variable))
  })
  
  
  
  Variable_list = c("COM velocity", "COM height / BH", "Z GRF", "X GRF", "ankle dorsiflexion", "knee flexion", "hip flexion", 
                    "pelvis tilt", "pelvis obliquity", "pelvis rotation",
                    "thorax tilt", "thorax obliquity", "thorax rotation",
                    "ankle dorsiflexion vel", "knee flexion vel", "hip flexion vel",
                    "ankle plantarflexor moment", "knee extensor moment", "hip extensor moment",
                    "total ankle power", "total knee power", "total hip power")
  
  Plot_names = c("COM_velocity", "COM_height", "Z_GRF", "X_GRF", "ankle_dorsiflexion", "knee_flexion", "hip_flexion", 
                 "pelvis_tilt", "pelvis_obliquity", "pelvis_rotation",
                 "thorax_tilt", "thorax_obliquity", "thorax_rotation",
                 "ankle_dorsiflexion_vel", "knee_flexion_vel", "hip_flexion_vel",
                 "ankle_plantarflexor_moment", "knee_extensor_moment", "hip_extensor_moment",
                 "total_ankle_power", "total_knee_power", "total_hip_power")
  
  #FilterTS_Athlete_Type_Date = CombinedTS_all %>% filter(Name == "Jenny Blundell") %>% filter(Type == "Stance") %>% filter(Date == "20180511")#%>% reshape2::melt(id = c("Name", "Date", "Type", "Trial"))
  
  
  ############ GGPLOTS #############################
  #### Definitions ####
  Line_Colours = c("black", "green4", "green4","green4","green4", "red", "red", "red", "red")
  Line_Types = c("solid", "dashed", "dashed", "dashed", "solid", "dashed", "dashed", "dashed", "solid")
  Line_Size = c(1, 0.1, 0.1, 0.1, 1, 0.1, 0.1, 0.1, 1)
  
  # For 2 sessions #
  Line_Colours2 = c("black","green4", "red", "green4", "red")
  Line_Types2 = c("solid","solid", "solid", "dashed", "dashed")
  Line_Size2 = c(1, 1,1, 1, 1)
  
  # ggplot loop #
  xlabel <- "% stance phase"
  
  # Propulsive % definition #
  PropPCLeft_S1 <- reactive({
    PropPCLeft_S1 <-  tab_Filtered_S1_ST_LeftAvg() %>% select(variable, Average)
    PropPCLeft_S1 <- PropPCLeft_S1 %>% filter(variable == "Propulsion (% cycle)")
    return(PropPCLeft_S1)
  })
  PropPCRight_S1 <- reactive({
    PropPCRight_S1 <-  tab_Filtered_S1_ST_RightAvg() %>% select(variable, Average)
    PropPCRight_S1 <- PropPCRight_S1 %>% filter(variable == "Propulsion (% cycle)")
    return(PropPCRight_S1)
  })
  
  PropPCLeft_S2 <- reactive({
    PropPCLeft_S2 <-  tab_Filtered_S2_ST_LeftAvg() %>% select(variable, Average)
    PropPCLeft_S2 <- PropPCLeft_S2 %>% filter(variable == "Propulsion (% cycle)")
    return(PropPCLeft_S2)
  })
  PropPCRight_S2 <- reactive({
    PropPCRight_S2 <-  tab_Filtered_S2_ST_RightAvg() %>% select(variable, Average)
    PropPCRight_S2 <- PropPCRight_S2 %>% filter(variable == "Propulsion (% cycle)")
    return(PropPCRight_S2)
  })
  
  #### Define Plot and Benchmark data ####
  plotdata2 <-  reactive({
    plotdata_S1 <- tab_Filtered_S1_TS() %>% filter(Trial %in% c("Left_Avg", "Right_Avg"))%>%  mutate(Session = paste("Session 1", Date, Trial))
    plotdata_S2 <- tab_Filtered_S2_TS() %>% filter(Trial %in% c("Left_Avg", "Right_Avg"))%>%  mutate(Session = paste("Session 2", Date, Trial))
    plotdata2 <-  bind_rows(plotdata_S1, plotdata_S2)
    
    return(plotdata2)
    
    
  })
  Benchmark_filtered <-  reactive({
    Benchmark_filtered <-  Benchmark %>% filter(Distance == input$Distance)
    return(Benchmark_filtered)
  })
  
  #### GGPLOT FUNCTION ####
  ggplot_base <- function(kinematic1,yheight1) {
    renderPlot({
      if (input$Report_Type == "Single Session"){
        PropPCLeft_S1 <- data.frame(PropPCLeft_S1())
        PropPCLeft_S1 <-  PropPCLeft_S1[1,2]
        PropPCRight_S1 <- data.frame(PropPCRight_S1())
        PropPCRight_S1 <-  PropPCRight_S1[1,2]
        plotdata = tab_Filtered_S1_TS() %>% filter(Kinematic ==kinematic1)
        plotdata_Benchmark =  Benchmark_filtered() %>% filter(Kinematic ==kinematic1)
        ggplot() + geom_line(data = plotdata, aes(variable, value, colour = Trial, linetype = Trial, size =Trial)) + 
          geom_line(data = plotdata_Benchmark, aes(variable,value, colour=Name, linetype = Name, size =Name)) +
          scale_linetype_manual(values = Line_Types) + scale_color_manual(values=Line_Colours) + scale_size_manual(values = Line_Size) +
          ggtitle(kinematic1)+ xlab(xlabel) + theme(plot.title = element_text(hjust = 0.5))+ 
          geom_vline(xintercept=PropPCLeft_S1, colour = 'green4') + geom_vline(xintercept=PropPCRight_S1, colour = 'red')+
          annotate(geom = "text",x=PropPCLeft_S1, y = yheight1,label="Propulsion",hjust=1,vjust=1, size=3, colour = 'green4')
        
        
      } else if (input$Report_Type == "Two Sessions"){
        PropPCLeft_S1 <- data.frame(PropPCLeft_S1())
        PropPCLeft_S1 <-  PropPCLeft_S1[1,2]
        PropPCRight_S1 <- data.frame(PropPCRight_S1())
        PropPCRight_S1 <-  PropPCRight_S1[1,2]
        PropPCLeft_S2 <- data.frame(PropPCLeft_S2())
        PropPCLeft_S2 <-  PropPCLeft_S2[1,2]
        PropPCRight_S2 <- data.frame(PropPCRight_S2())
        PropPCRight_S2 <-  PropPCRight_S2[1,2]
        
        
        plotdata = plotdata2() %>% filter(Kinematic ==kinematic1)
        plotdata_Benchmark =  Benchmark_filtered() %>% filter(Kinematic ==kinematic1)
        ggplot(data = plotdata, aes(variable, value, colour = Session, linetype = Session, size =Session)) + geom_line()+ 
          geom_line(data = plotdata_Benchmark, aes(variable,value, colour=Name, linetype = Name, size =Name)) +
          scale_linetype_manual(values = Line_Types2) + scale_color_manual(values=Line_Colours2) + scale_size_manual(values = Line_Size2) +
          ggtitle(kinematic1)+ xlab(xlabel) + theme(plot.title = element_text(hjust = 0.5))+ 
          geom_vline(xintercept=PropPCLeft_S1, colour = 'green4') + geom_vline(xintercept=PropPCRight_S1, colour = 'red')+
          geom_vline(xintercept=PropPCLeft_S2, colour = 'green4', linetype='dashed') + geom_vline(xintercept=PropPCRight_S2, colour = 'red', linetype='dashed')+
          annotate(geom = "text",x=PropPCLeft_S1, yheight1,label="Propulsion",hjust=1, vjust=1, size=3, colour = 'green4')
        
        
        
      }
    })
  }
  
  
  #### GGPLOTS ####
  output$ggplotCOM_velocity <- ggplot_base("COM velocity",Inf)
  
  output$ggplotCOM_height <- ggplot_base("COM height / BH",Inf)
  
  output$ggplotZ_GRF <- ggplot_base("Z GRF",Inf)
  
  output$ggplotX_GRF <- ggplot_base("X GRF",Inf)
  
  output$ggplotankle_dorsiflexion <- ggplot_base("ankle dorsiflexion",Inf)
  
  output$ggplotknee_flexion <- ggplot_base("knee flexion",Inf)
  
  output$ggplothip_flexion <- ggplot_base("hip flexion",Inf)
  
  output$ggplotpelvis_tilt <- ggplot_base("pelvis tilt",Inf)
  
  output$ggplotpelvis_obliquity <- ggplot_base("pelvis obliquity",Inf)
  
  output$ggplotpelvis_rotation <- ggplot_base("pelvis rotation",Inf)
  
  output$ggplotthorax_tilt <- ggplot_base("thorax tilt",Inf)
  
  output$ggplotthorax_obliquity <- ggplot_base("thorax obliquity",Inf)
  
  output$ggplotthorax_rotation <- ggplot_base("thorax rotation",Inf)
  
  output$ggplotankle_dorsiflexion_vel <- ggplot_base("ankle dorsiflexion vel",Inf)
  
  output$ggplotknee_flexion_vel <- ggplot_base("knee flexion vel",Inf)
  
  output$ggplothip_flexion_vel <- ggplot_base("hip flexion vel",Inf)
  
  
  
  
  ################# TableOutput ###################
  
  
  
  #### Table Average Labels ####
  
  column_Names_ST_Avg <-  c("Trial","Cadence (steps/min)", "Walking Speed (m/s)", "Stride Time (s)", "Foot Off (%)", "Stride Length (m)","Step Width (m)", "Swing Phase (%)", 
                            "Stance Phase (%)", "Double Support (s)","Stride Length (%)", 	"Contact Time (s)", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio", "COMHeight Cont", "COMHeight min", "COMHeight TO", 
                            "COMHeight range", "COMHeight ratio", "Hip to Toe Contact (mm)", "Propulsion (% cycle)", "Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Pelvis XRom", "Pelvis YRom", 
                            "Pelvis ZRom", "Pelvis Xvelmax", "Pelvis Yvelmax", "Pelvis Zvelmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Thorax XRom", "Thorax YRom", 
                            "Thorax ZRom", "Thorax Xvelmax", "Thorax Yvelmax", "Thorax Zvelmax", "Hip Xmax", "Hip Ymax", "Hip Zmax", "Hip XRom", "Hip YRom", "Hip ZRom", 
                            "Hip Xvelmax", "Hip Yvelmax", "Hip Zvelmax", "Knee Xmax", "Knee Ymax", "Knee Zmax", "Knee XRom", "Knee YRom", "Knee ZRom", "Knee Xvelmax", 
                            "Knee Yvelmax", "Knee Zvelmax", "Ankle Xmax", "Ankle XRom", "Ankle Xvelmax", "Foot Prog Xmax", "Foot Prog XRom", "Foot Prog Xvelmax")
  
  Label_S1_LeftAvg <- paste("Session 1", input$Date, "Left Average")
  Label_S1_RightAvg <- paste("Session 1", input$Date, "Right Average")
  Label_S2_LeftAvg <- paste("Session 2", input$Date2, "Left Average")
  Label_S2_RightAvg <- paste("Session 2", input$Date2, "Right Average")
  
  #### Define Avg tabledata ####
  tabledata_AvgLeft_S1 <-  reactive({
    #STLeftAvg_S1 <-  tab_Filtered_S1(tab_Filtered_S1_ST_LeftAvg(),'variable')      
    tabledata_AvgLeft_S1 <-  tab_Filtered_S1_ST_LeftAvg() %>% select(variable, Average)
    tabledata_AvgLeft_S1 <-  column_to_rownames(tabledata_AvgLeft_S1,'variable')
    tabledata_AvgLeft_S1 <-  data.frame(t(tabledata_AvgLeft_S1))
    tabledata_AvgLeft_S1 <-  rownames_to_column(tabledata_AvgLeft_S1)
    colnames(tabledata_AvgLeft_S1) <-  column_Names_ST_Avg
    tabledata_AvgLeft_S1[1,1] <- Label_S1_LeftAvg
    tabledata_AvgLeft_S1 <- bind_rows(tabledata_AvgLeft_S1)
    return(tabledata_AvgLeft_S1)
    
    
  })
  
  tabledata_AvgRight_S1 <-  reactive({
    #STLeftAvg_S1 <-  tab_Filtered_S1(tab_Filtered_S1_ST_LeftAvg(),'variable')      
    tabledata_AvgRight_S1 <-  tab_Filtered_S1_ST_RightAvg() %>% select(variable, Average)
    tabledata_AvgRight_S1 <-  column_to_rownames(tabledata_AvgRight_S1,'variable')
    tabledata_AvgRight_S1 <-  data.frame(t(tabledata_AvgRight_S1))
    tabledata_AvgRight_S1 <-  rownames_to_column(tabledata_AvgRight_S1)
    colnames(tabledata_AvgRight_S1) <-  column_Names_ST_Avg
    tabledata_AvgRight_S1[1,1] <- Label_S1_RightAvg
    tabledata_AvgRight_S1 <- bind_rows(tabledata_AvgRight_S1)
    
    return(tabledata_AvgRight_S1)
    
    
  })
  
  tabledata_AvgLeft_S2 <-  reactive({
    #STLeftAvg_S1 <-  tab_Filtered_S1(tab_Filtered_S1_ST_LeftAvg(),'variable')      
    tabledata_AvgLeft_S2 <-  tab_Filtered_S2_ST_LeftAvg() %>% select(variable, Average)
    tabledata_AvgLeft_S2 <-  column_to_rownames(tabledata_AvgLeft_S2,'variable')
    tabledata_AvgLeft_S2 <-  data.frame(t(tabledata_AvgLeft_S2))
    tabledata_AvgLeft_S2 <-  rownames_to_column(tabledata_AvgLeft_S2)
    colnames(tabledata_AvgLeft_S2) <-  column_Names_ST_Avg
    tabledata_AvgLeft_S2[1,1] <- Label_S2_LeftAvg
    tabledata_AvgLeft_S2 <- bind_rows(tabledata_AvgLeft_S2)
    return(tabledata_AvgLeft_S2)
    
    
  })
  
  tabledata_AvgRight_S2 <-  reactive({
    #STLeftAvg_S1 <-  tab_Filtered_S1(tab_Filtered_S1_ST_LeftAvg(),'variable')      
    tabledata_AvgRight_S2 <-  tab_Filtered_S2_ST_RightAvg() %>% select(variable, Average)
    tabledata_AvgRight_S2 <-  column_to_rownames(tabledata_AvgRight_S2,'variable')
    tabledata_AvgRight_S2 <-  data.frame(t(tabledata_AvgRight_S2))
    tabledata_AvgRight_S2 <-  rownames_to_column(tabledata_AvgRight_S2)
    colnames(tabledata_AvgRight_S2) <-  column_Names_ST_Avg
    tabledata_AvgRight_S2[1,1] <- Label_S2_RightAvg
    tabledata_AvgRight_S2 <- bind_rows(tabledata_AvgRight_S2)
    
    return(tabledata_AvgRight_S2)
    
    
  })
  
  
  #### Datatables ####
  output$STtable <-  DT::renderDataTable({
    
    if (input$Report_Type == "Single Session"){
      ST <-  tab_Filtered_S1_ST() %>% select("Trial", "Stance Phase (%)",  "Swing Phase (%)", "Stride Length (m)", "Stride Length (%)", 
                                             "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                             "Propulsion (% cycle)") 
      ST[1,1] = "Left 1"
      ST[2,1] = "Left 2"
      ST[3,1] = "Left 3"
      ST[4,1] = "Right 1"
      ST[5,1] = "Right 2"
      ST[6,1] = "Right 3"
      
      STLeftAvg <-  tabledata_AvgLeft_S1() %>% select("Trial","Stance Phase (%)",  "Swing Phase (%)","Stride Length (m)", "Stride Length (%)", 
                                                      "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                                      "Propulsion (% cycle)") 
      STRightAvg <-  tabledata_AvgRight_S1() %>% select("Trial","Stance Phase (%)",  "Swing Phase (%)","Stride Length (m)", "Stride Length (%)", 
                                                        "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                                        "Propulsion (% cycle)") 
      
      STtable <-  bind_rows(ST, STLeftAvg, STRightAvg)
      datatable(STtable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                  'console.log(rowId)','if(rowId == 6) {',
                                                                                  'row.style.backgroundColor = "green";','}','if(rowId == 7) {',
                                                                                  'row.style.backgroundColor = "red";','}','}')))
    }else if (input$Report_Type == "Two Sessions"){
      
      STLeftAvg_S1 <-  tabledata_AvgLeft_S1() %>% select("Trial","Stance Phase (%)",  "Swing Phase (%)","Stride Length (m)", "Stride Length (%)", 
                                                         "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                                         "Propulsion (% cycle)") 
      STRightAvg_S1 <-  tabledata_AvgRight_S1() %>% select("Trial","Stance Phase (%)",  "Swing Phase (%)","Stride Length (m)", "Stride Length (%)", 
                                                           "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                                           "Propulsion (% cycle)") 
      STLeftAvg_S2 <-  tabledata_AvgLeft_S2() %>% select("Trial","Stance Phase (%)",  "Swing Phase (%)","Stride Length (m)", "Stride Length (%)", 
                                                         "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                                         "Propulsion (% cycle)") 
      STRightAvg_S2 <-  tabledata_AvgRight_S2() %>% select("Trial","Stance Phase (%)",  "Swing Phase (%)","Stride Length (m)", "Stride Length (%)", 
                                                           "Step Width (m)", "Double Support (s)",  "Contact Time (s)", "Hip to Toe Contact (mm)", "Cadence (steps/min)", 
                                                           "Propulsion (% cycle)") 
      
      STtable <-  bind_rows(STLeftAvg_S1, STLeftAvg_S2, STRightAvg_S1, STRightAvg_S2)
      datatable(STtable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                  'console.log(rowId)','if(rowId == 0) {',
                                                                                  'row.style.backgroundColor = "green";','}','if(rowId == 2) {',
                                                                                  'row.style.backgroundColor = "red";','}','}')))
      
      
    }
    
  })
  
  output$COMVeltable <-  DT::renderDataTable({
    if (input$Report_Type == "Single Session"){
      
      COMVel <-  tab_Filtered_S1_ST() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      
      COMVel[1,1] = "Left 1"
      COMVel[2,1] = "Left 2"
      COMVel[3,1] = "Left 3"
      COMVel[4,1] = "Right 1"
      COMVel[5,1] = "Right 2"
      COMVel[6,1] = "Right 3"
      
      
      COMVelLeftAvg <-  tabledata_AvgLeft_S1() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      COMVelRightAvg <-  tabledata_AvgRight_S1() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      
      
      COMVeltable <-  bind_rows(COMVel, COMVelLeftAvg, COMVelRightAvg)
      datatable(COMVeltable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                      'console.log(rowId)','if(rowId == 6) {',
                                                                                      'row.style.backgroundColor = "green";','}','if(rowId == 7) {',
                                                                                      'row.style.backgroundColor = "red";','}','}')))
    }else if (input$Report_Type == "Two Sessions"){
      COMVelLeftAvg_S1 <-  tabledata_AvgLeft_S1() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      COMVelRightAvg_S1 <-  tabledata_AvgRight_S1() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      COMVelLeftAvg_S2 <-  tabledata_AvgLeft_S2() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      COMVelRightAvg_S2 <-  tabledata_AvgRight_S2() %>% select("Trial", "COMVel Cont","COMVel min", "COMVel TO", "COMVel range", "COMVel ratio")
      
      COMVeltable <-  bind_rows(COMVelLeftAvg_S1, COMVelRightAvg_S1, COMVelLeftAvg_S2, COMVelRightAvg_S2)
      datatable(COMVeltable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                      'console.log(rowId)','if(rowId == 0) {',
                                                                                      'row.style.backgroundColor = "green";','}','if(rowId == 2) {',
                                                                                      'row.style.backgroundColor = "red";','}','}')))
    }
  })
  
  
  output$COMHeighttable <-  DT::renderDataTable({
    if (input$Report_Type == "Single Session"){
      
      COMHeight <-  tab_Filtered_S1_ST() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      
      COMHeightLeftAvg <-  tabledata_AvgLeft_S1() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      COMHeightRightAvg <-  tabledata_AvgRight_S1() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      
      
      COMHeighttable <-  bind_rows(COMHeight, COMHeightLeftAvg, COMHeightRightAvg)
      datatable(COMHeighttable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                         'console.log(rowId)','if(rowId == 6) {',
                                                                                         'row.style.backgroundColor = "green";','}','if(rowId == 7) {',
                                                                                         'row.style.backgroundColor = "red";','}','}')))
    }else if (input$Report_Type == "Two Sessions"){
      
      COMHeightLeftAvg_S1 <-  tabledata_AvgLeft_S1() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      COMHeightRightAvg_S1 <-  tabledata_AvgRight_S1() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      COMHeightLeftAvg_S2 <-  tabledata_AvgLeft_S2() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      COMHeightRightAvg_S2 <-  tabledata_AvgRight_S2() %>% select("Trial", "COMHeight Cont", "COMHeight min", "COMHeight TO", "COMHeight range", "COMHeight ratio")
      
      COMHeighttable <-  bind_rows(COMHeightLeftAvg_S1, COMHeightRightAvg_S1, COMHeightLeftAvg_S2, COMHeightRightAvg_S2)
      datatable(COMHeighttable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                         'console.log(rowId)','if(rowId == 0) {',
                                                                                         'row.style.backgroundColor = "green";','}','if(rowId == 2) {',
                                                                                         'row.style.backgroundColor = "red";','}','}')))
      
    }
  })
  
  
  output$JointAngletable <-  DT::renderDataTable({
    if (input$Report_Type == "Single Session"){
      
      JointAngle <- tab_Filtered_S1_ST() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      JointAngle[1,1] = "Left 1"
      JointAngle[2,1] = "Left 2"
      JointAngle[3,1] = "Left 3"
      JointAngle[4,1] = "Right 1"
      JointAngle[5,1] = "Right 2"
      JointAngle[6,1] = "Right 3"
      JointAngleLeftAvg <-  tabledata_AvgLeft_S1() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      JointAngleRightAvg <-  tabledata_AvgRight_S1() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      
      
      JointAngletable <-  bind_rows(JointAngle, JointAngleLeftAvg, JointAngleRightAvg)
      datatable(JointAngletable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                          'console.log(rowId)','if(rowId == 6) {',
                                                                                          'row.style.backgroundColor = "green";','}','if(rowId == 7) {',
                                                                                          'row.style.backgroundColor = "red";','}','}')))
    }else if (input$Report_Type == "Two Sessions"){
      JointAngleLeftAvg_S1 <-  tabledata_AvgLeft_S1() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      JointAngleRightAvg_S1 <-  tabledata_AvgRight_S1() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      JointAngleLeftAvg_S2 <-  tabledata_AvgLeft_S2() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      JointAngleRightAvg_S2 <-  tabledata_AvgRight_S2() %>% select("Trial", "Ankle Xmax",  "Knee Xmax", "Hip Xmax","Ankle XRom", "Knee XRom", "Hip XRom")
      JointAngletable <-  bind_rows(JointAngleLeftAvg_S1, JointAngleRightAvg_S1, JointAngleLeftAvg_S2, JointAngleRightAvg_S2)
      datatable(JointAngletable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                          'console.log(rowId)','if(rowId == 0) {',
                                                                                          'row.style.backgroundColor = "green";','}','if(rowId == 2) {',
                                                                                          'row.style.backgroundColor = "red";','}','}')))
      
    }
  }) 
  
  
  output$PelvicThoraxAngletable <-  DT::renderDataTable({
    if (input$Report_Type == "Single Session"){
      
      PelvicThoraxAngle <- tab_Filtered_S1_ST() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                           "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      PelvicThoraxAngle[1,1] = "Left 1"
      PelvicThoraxAngle[2,1] = "Left 2"
      PelvicThoraxAngle[3,1] = "Left 3"
      PelvicThoraxAngle[4,1] = "Right 1"
      PelvicThoraxAngle[5,1] = "Right 2"
      PelvicThoraxAngle[6,1] = "Right 3"
      PelvicThoraxAngleLeftAvg <-  tabledata_AvgLeft_S1() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                     "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      PelvicThoraxAngleRightAvg <-  tabledata_AvgRight_S1() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                       "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      
      
      PelvicThoraxAngletable <-  bind_rows(PelvicThoraxAngle, PelvicThoraxAngleLeftAvg, PelvicThoraxAngleRightAvg)
      datatable(PelvicThoraxAngletable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                                 'console.log(rowId)','if(rowId == 6) {',
                                                                                                 'row.style.backgroundColor = "green";','}','if(rowId == 7) {',
                                                                                                 'row.style.backgroundColor = "red";','}','}')))
    }else if (input$Report_Type == "Two Sessions"){
      PelvicThoraxAngleLeftAvg_S1 <-  tabledata_AvgLeft_S1() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                        "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      PelvicThoraxAngleRightAvg_S1 <-  tabledata_AvgRight_S1() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                          "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      PelvicThoraxAngleLeftAvg_S2 <-  tabledata_AvgLeft_S2() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                        "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      PelvicThoraxAngleRightAvg_S2 <-  tabledata_AvgRight_S2() %>% select("Trial","Pelvis Xmax", "Pelvis Ymax", "Pelvis Zmax", "Thorax Xmax", "Thorax Ymax", "Thorax Zmax", "Pelvis XRom", "Pelvis YRom", 
                                                                          "Pelvis ZRom","Thorax XRom", "Thorax YRom", "Thorax ZRom")
      PelvicThoraxAngletable <-  bind_rows(PelvicThoraxAngleLeftAvg_S1, PelvicThoraxAngleRightAvg_S1, PelvicThoraxAngleLeftAvg_S2, PelvicThoraxAngleRightAvg_S2)
      datatable(PelvicThoraxAngletable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                                 'console.log(rowId)','if(rowId == 0) {',
                                                                                                 'row.style.backgroundColor = "green";','}','if(rowId == 2) {',
                                                                                                 'row.style.backgroundColor = "red";','}','}')))
      
    } 
  })    
  
  
  output$JointVeltable <-  DT::renderDataTable({
    if (input$Report_Type == "Single Session"){
      
      JointVel <- tab_Filtered_S1_ST() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      JointVel[1,1] = "Left 1"
      JointVel[2,1] = "Left 2"
      JointVel[3,1] = "Left 3"
      JointVel[4,1] = "Right 1"
      JointVel[5,1] = "Right 2"
      JointVel[6,1] = "Right 3"
      JointVelLeftAvg <-  tabledata_AvgLeft_S1() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      JointVelRightAvg <-  tabledata_AvgRight_S1() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      
      
      JointVeltable <-  bind_rows(JointVel, JointVelLeftAvg, JointVelRightAvg)
      datatable(JointVeltable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                        'console.log(rowId)','if(rowId == 6) {',
                                                                                        'row.style.backgroundColor = "green";','}','if(rowId == 7) {',
                                                                                        'row.style.backgroundColor = "red";','}','}')))
    }else if (input$Report_Type == "Two Sessions"){
      JointVelLeftAvg_S1 <-  tabledata_AvgLeft_S1() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      JointVelRightAvg_S1 <-  tabledata_AvgRight_S1() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      JointVelLeftAvg_S2 <-  tabledata_AvgLeft_S2() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      JointVelRightAvg_S2 <-  tabledata_AvgRight_S2() %>% select("Trial", "Ankle Xvelmax", "Knee Xvelmax", "Hip Xvelmax")
      JointVeltable <-  bind_rows(JointVelLeftAvg_S1, JointVelRightAvg_S1, JointVelLeftAvg_S2, JointVelRightAvg_S2)
      datatable(JointVeltable,rownames = NULL, options = list(dom='t', rowCallback = JS('function(row, data, index, rowId) {',
                                                                                        'console.log(rowId)','if(rowId == 0) {',
                                                                                        'row.style.backgroundColor = "green";','}','if(rowId == 2) {',
                                                                                        'row.style.backgroundColor = "red";','}','}')))
    } 
  })
  
  
  
  
  
  
  
  
  # Spare ST variables "Pelvis Xvelmax", "Pelvis Yvelmax", "Pelvis Zvelmax",   
  #"Thorax Xvelmax", "Thorax Yvelmax", "Thorax Zvelmax", , "Hip Ymax", "Hip Zmax", , "Hip YRom", "Hip ZRom", 
  #"Hip Yvelmax", "Hip Zvelmax", , "Knee Ymax", "Knee Zmax", , "Knee YRom", "Knee ZRom",  
  #"Knee Yvelmax", "Knee Zvelmax", , , , "Foot Prog Xmax", "Foot Prog XRom", "Foot Prog Xvelmax")
  # "Walking Speed (m/s):", "Stride Time (s):", "Foot Off (%):",
  
  
  
  })
}

