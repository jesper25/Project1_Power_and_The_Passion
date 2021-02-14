#libraries to include 
library(plyr)
library(dplyr)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(DT)
library(data.table)
library(usmap)

#read in data
prod <- read.csv("annual_generation_state.csv",header = TRUE, sep = ",")
prod1 <- filter(prod,TYPE.OF.PRODUCER == "Total Electric Power Industry")


#remove states with no names 
prod1 <- prod1[-c(5829,5830,5831),]

#make column name GENERATION.Megawatthours. into numeric values
prod1$GENERATION..Megawatthours. <- as.numeric(gsub(",", "",prod1$GENERATION..Megawatthours.))

#make all the strings in column STATE to uppercase
prod1$STATE <- toupper(prod1$STATE)

#remove rows that have ENERGY.SOURCE as "Other", "Other Gases" , "Other Biomass", and "Pumped Storage"
prod2 <- subset(prod1, ENERGY.SOURCE!="Other" & ENERGY.SOURCE!="Other Gases" & ENERGY.SOURCE!="Other Biomass" & ENERGY.SOURCE!= "Pumped Storage")

#remove negative values in GENERATION..Megawatthours
prod2 <- subset(prod2, prod2$GENERATION..Megawatthours. >= 0)

droplevels(prod2)

#make these columns into categorical values 
prod2$'ENERGY.SOURCE' <- as.factor(prod2$ENERGY.SOURCE)
prod2$'TYPE.OF.PRODUCER' <- as.factor(prod2$TYPE.OF.PRODUCER)
prod2$'STATE' <- as.factor(prod2$STATE)

#
levels(prod2$ENERGY.SOURCE)[levels(prod2$ENERGY.SOURCE) == "Hydroelectric Conventional"] <- "Hydro"
levels(prod2$ENERGY.SOURCE)[levels(prod2$ENERGY.SOURCE) == "Wood and Wood Derived Fuels"] <- "Wood"
levels(prod2$ENERGY.SOURCE)[levels(prod2$ENERGY.SOURCE) == "Solar Thermal and Photovoltaic"] <- "Solar"

rownames(prod2) <- 1:nrow(prod2)
source <- factor(c('Coal','Geothermal','Hydro','Natrual Gas','Nuclear','Petroleum','Solar','Wind','Wood'))
#str(prod2)
values = prod2
values = ddply(values, .(YEAR), transform, PERCENT =GENERATION..Megawatthours./sum(GENERATION..Megawatthours.)*100)

total <- values[prod2$ENERGY.SOURCE == "Total",]
total
notTotal <- values[prod2$ENERGY.SOURCE != "Total",]
notTotal
options(scipen= 999)
colors <- c("Coal" = "grey45","Geothermal" = "cornflowerblue","Hydro" = "steelblue4","Natural Gas" = "royalblue4","Nuclear" = "plum4","Petroleum" = "indianred3","Solar" = "gold2","Wind" = "lightseagreen","Wood" = "rosybrown4")



ui <- fluidPage(title="Project 1",
  tabsetPanel(
    tabPanel(title="stackBarEach",
             plotOutput("stackbar1")
             
             ),#end first tab
    tabPanel(title="stackBarTotal",
             plotOutput("stackbar2")
             
             ),#end second tab
    tabPanel(title="LineChartTotal",
             plotOutput("linechart1")
             
             ),#end third tab    
    tabPanel(title="LineChartPercent",
             plotOutput("linechart2")
             
             ),#end forth tab    
    tabPanel(title="Table",
             DT::dataTableOutput("table1")
             
             ),#end last tab
    tabPanel(title="Split Charts",
             sidebarLayout(
               sidebarPanel (
                 uiOutput("stateInput")
               ),
               mainPanel (
                splitLayout(
                  plotOutput("plot1"),
                  plotOutput("plot2")
              )
              )
             )
             
    ),#end last tab    
    tabPanel(title="Heat Map Total",
             plotOutput("heatMap1")
             ),
    
    tabPanel(title="Heat Map Percent",
             plotOutput("heatMap2")
             )    
    )#end tabsetpanel
                
)#ui ends

server <- function(input, output){
  output$stackbar1 <- renderPlot({
    stackBar <- ggplot() + geom_bar(aes(y = GENERATION..Megawatthours.,x = YEAR, fill = ENERGY.SOURCE), data = notTotal, stat = "identity")
    
    stackBar <- stackBar + labs(x="Years", y="MegaWatts Used", fill = "Type of Energy Source")+
      ggtitle("Total Amount of each Energy Source used from Years 1990-2019")+
      scale_x_continuous(breaks=seq(1990,2019,4))+ theme_minimal()+
      scale_fill_manual("Type of Energy Source", values =colors )
    
    stackBar <- stackBar + 
      theme(text = element_text(size=10))+
      theme(axis.title.x = element_text(size=13))+
      theme(axis.title.y = element_text(size=13))+
      theme(panel.background = element_rect(fill= "grey75",colour="black",size=0.5))    
    
    print(stackBar)
  })
  output$stackbar2 <- renderPlot({
    stackBar2 <- ggplot() + geom_bar(aes(y = PERCENT,x = YEAR, fill = ENERGY.SOURCE ), data = notTotal, stat = "identity")
    
    stackBar2 <- stackBar2 + labs(x="Years", y="Percent of each Energy Source", fill = "Type of Energy Source")+
      ggtitle("Total Amount of each Energy Source used from Years 1990-2019")+
      scale_x_continuous(breaks=seq(1990,2019,4))+
      scale_fill_manual("Type of Energy Source", values =colors ) 
      
      
      stackBar2 <- stackBar2 + 
        theme(text = element_text(size=10))+
        theme(axis.title.x = element_text(size=13))+
        theme(axis.title.y = element_text(size=13))+
        theme(panel.background = element_rect(fill= "grey75",colour="black",size=0.5))      
      
    print(stackBar2)
  })
  output$linechart1 <- renderPlot({
    lineChart <- ggplot(notTotal, aes(x=YEAR, y=GENERATION..Megawatthours., color=ENERGY.SOURCE))
    lineChart <- lineChart + stat_summary(fun.y="mean", geom="line", size=1.1, 
                                          aes(linetype = ENERGY.SOURCE))
    lineChart <- lineChart + labs(x="Years", y="Percent of each Energy Source", fill = "Type of Energy Source")+
      ggtitle("Total Amount of each Energy Source used from Years 1990-2019")+
      scale_x_continuous(breaks=seq(1990,2019,4))+
      scale_fill_manual("Type of Energy Source", values =colors ) 
    
    lineChart <- lineChart + 
      theme(text = element_text(size=10))+
      theme(axis.title.x = element_text(size=13))+
      theme(axis.title.y = element_text(size=13))+
      theme(panel.background = element_rect(fill= "grey90",colour="black",size=0.5))
    print(lineChart)
    
  })
  output$linechart2 <- renderPlot({
    lineChart2 <- ggplot(notTotal, aes(x=YEAR, y=PERCENT, color=ENERGY.SOURCE))
    lineChart2 <- lineChart2 + stat_summary(fun.y="mean", geom="line", size=1.1, 
                                            aes(linetype = ENERGY.SOURCE))
    lineChart2 <- lineChart2 + labs(x="Years", y="Percent of each Energy Source", fill = "Type of Energy Source")+
      ggtitle("Total Amount of each Energy Source used from Years 1990-2019")+
      scale_x_continuous(breaks=seq(1990,2019,4))+
      scale_fill_manual("Type of Energy Source", values =colors ) 
    
    lineChart2 <- lineChart2 + 
      theme(text = element_text(size=10))+
      theme(axis.title.x = element_text(size=13))+
      theme(axis.title.y = element_text(size=13))+
      theme(panel.background = element_rect(fill= "grey90",colour="black",size=0.5))
    print(lineChart2)   
    
    
  })
  output$table1<- DT::renderDataTable(
    
    DT::datatable({ 
      
      prod2Table <- data.table(prod2[,c("GENERATION..Megawatthours.","PERCENT")])
    }, 
    options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
    ), rownames = FALSE 
    )
  )
  
  output$plot1 <- renderPlot({
    stackBar <- ggplot() + geom_bar(aes(y = GENERATION..Megawatthours.,x = YEAR, fill = ENERGY.SOURCE), data = notTotal, stat = "identity")
    
    stackBar <- stackBar + labs(x="Years", y="MegaWatts Used", fill = "Type of Energy Source")+
      ggtitle("Total Amount of each Energy Source used from Years 1990-2019")+
      scale_x_continuous(breaks=seq(1990,2019,4))+ theme_minimal()+
      scale_fill_manual("Type of Energy Source", values =colors )
    
    stackBar <- stackBar + 
      theme(text = element_text(size=10))+
      theme(axis.title.x = element_text(size=13))+
      theme(axis.title.y = element_text(size=13))+
      theme(panel.background = element_rect(fill= "grey75",colour="black",size=0.5))    
    
    print(stackBar)
  })  
  
  output$plot2 <- renderPlot({
    stackBar <- ggplot() + geom_bar(aes(y = GENERATION..Megawatthours.,x = YEAR, fill = ENERGY.SOURCE), data = notTotal, stat = "identity")
    
    stackBar <- stackBar + labs(x="Years", y="MegaWatts Used", fill = "Type of Energy Source")+
      ggtitle("Total Amount of each Energy Source used from Years 1990-2019")+
      scale_x_continuous(breaks=seq(1990,2019,4))+ theme_minimal()+
      scale_fill_manual("Type of Energy Source", values =colors )
    
    stackBar <- stackBar + 
      theme(text = element_text(size=10))+
      theme(axis.title.x = element_text(size=13))+
      theme(axis.title.y = element_text(size=13))+
      theme(panel.background = element_rect(fill= "grey75",colour="black",size=0.5))    
    
    print(stackBar)
  })  
  
  output$stateInput <- renderUI({
    selectInput(
      inputId = "state",
      label = "Select a State:",
      choices = levels(notTotal$STATES)
    )
  })
  
  output$heatMap <- renderPlot({
    
    colnames(notTotal)[2] <- "state"
    
    plot_usmap(data = notTotal, values = "GENERATION..Megawatthours.",labels=TRUE, color = "black",exclude="DC") + 
      scale_fill_continuous(low = "lightblue", high = "blue", name = "The Amount of Energy Source", label = scales::comma) + 
      theme(legend.position = "right")
    
    
    
    plotOutput("heatMap")
  })
 
  
  output$heatMap2 <- renderPlot({
    
    colnames(notTotal)[2] <- "state"
    
    plot_usmap(data = notTotal, values = "PERCENT",labels=TRUE, color = "black",exclude="DC") + 
      scale_fill_continuous(low = "lightblue", high = "blue", name = "The Amount of Energy Source", label = scales::comma) + 
      theme(legend.position = "right")
    
    
    
    plotOutput("heatMap")
  }) 
}#end server


shinyApp(ui = ui, server = server)
