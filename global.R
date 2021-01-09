library(readxl)
library(reshape2)
library(tibble)
library(stringr)
library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(shinydashboard)
library(Rmisc)

Benchmark <- fread('Benchmark.csv')

ggplot_base <- function(kinematic1) {
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
        ggtitle("COM velocity")+ xlab(xlabel) + theme(plot.title = element_text(hjust = 0.5))+ 
        geom_vline(xintercept=PropPCLeft_S1, colour = 'green4') + geom_vline(xintercept=PropPCRight_S1, colour = 'red')+
        geom_text(aes(x=PropPCLeft_S1, y = 6),label="Left Propulsion",hjust=1, size=3, colour = 'green4') +
        geom_text(aes(x=PropPCRight_S1, y = 6.1),label="Right Propulsion",hjust=1, size=3, colour = 'red')
      
      
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
        ggtitle("COM velocity")+ xlab(xlabel) + theme(plot.title = element_text(hjust = 0.5))+ 
        geom_vline(xintercept=PropPCLeft_S1, colour = 'green4') + geom_vline(xintercept=PropPCRight_S1, colour = 'red')+
        geom_vline(xintercept=PropPCLeft_S2, colour = 'green4', linetype="dotted") + geom_vline(xintercept=PropPCRight_S2, colour = 'red', linetype="dotted")+
        geom_text(aes(x=PropPCLeft_S1, y = 6),label="Left Propulsion S1",hjust=1, size=3, colour = 'green4') +
        geom_text(aes(x=PropPCRight_S1, y = 6.1),label="Right Propulsion S1",hjust=1, size=3, colour = 'red') +
        geom_text(aes(x=PropPCLeft_S2, y = 6.2),label="Left Propulsion S2",hjust=1, size=3, colour = 'green4') +
        geom_text(aes(x=PropPCRight_S2, y = 6.3),label="Right Propulsion S2",hjust=1, size=3, colour = 'red')
      
      
    }
  })
}
