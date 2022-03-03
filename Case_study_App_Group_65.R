library(shiny)
library(shinyWidgets)
library(shinythemes)
library(plotly)
library(dplyr)
library(tidyverse)
library(rapportools)

#read dataset
load("Final_Data_Group_65.RData")

# Define UI for application
ui <- navbarPage(title=div(
  "Group_65",theme=shinytheme("cerulean")),
  
  tabPanel("Registration history",
           fluidPage(
             tags$h1("Registrations in community"),
             br(),
             searchInput(
               inputId = "community",label="Enter the name of the community",
               placeholder="community name",
               btnSearch = icon("search"),
               btnReset = icon("remove"),
               width = "450px"
             ),
             br(),
             radioButtons(
               inputId = "Cumulative", label = NULL,
               choices = c("Non-cumulative","Cumulative"),
               inline = TRUE
             ),
             br(),
             mainPanel(
               plotOutput("graph",height = 500))
           )),
  
  tabPanel("Bar plot of Failures",
           fluidPage(
             titlePanel(title = h4("Failures per month by Vehicle Type", align="center")),
             sidebarPanel(
               dateInput(inputId = "date", 
                         label = "Select date",
                         format= "yyyy-mm",
                         min="2010-03-01",
                         max="2016-07-01",
                         value="2010-03-01",
                         startview = "year"
               )),
             mainPanel(
               plotOutput("bar"),
               plotOutput("bartotal"))
           )),
  
  tabPanel("Vehicle affected",
           fluidPage(
             tags$h1("Is your vehicle affected?"),
             br(),
             searchInput(
               inputId = "search",label="Enter your vehicle-ID",
               placeholder="vehicle-ID",
               btnSearch = icon("search"),
               btnReset = icon("remove"),
               width = "450px"
             ),
             br(),
             verbatimTextOutput(outputId = "res")
           ))
)

server<-function(input,output){
    
  #Registration history
    output$graph <- renderPlot({
      if (!is.empty(input$community)) {
        if (input$Cumulative == "Non-cumulative") {
          df_registration <- t02_affected_vehicles %>% filter(toupper(Gemeinden) == toupper(input$community))
          df_registration1 <- setNames(data.frame(table(df_registration$Zulassung)),c("Date","Count"))
          ggplot(df_registration1, aes(x = as.Date(Date), y = Count)) +
            geom_line() +
            xlab("Date") +
            ylab("Number of registrations of vehicles")
        } else {
          df_registration <- t02_affected_vehicles %>% filter(toupper(Gemeinden) == toupper(input$community))
          df_registration1 <- setNames(data.frame(table(df_registration$Zulassung)),c("Date","Count"))
          ggplot(df_registration1, aes(x = as.Date(Date), y = cumsum(Count))) +
            geom_line() +
            xlab("Date") +
            ylab("Number of registered vehicles")
        }
      }
    })
    
  #barplot
    output$bar <- renderPlot({
      
      #prepare data for app
      # omit rows with na values
      df <- na.omit(t02_affected_vehicles)
      #convert date format without days
      df$datum <- format(as.Date(df$Fehlerhaft_Datum), "%Y-%m")
      
      data1 <- format(input$date, "%Y-%m")
      #df$datum <- format(as.Date(df$Fehlerhaft_Datum), "%Y-%m")
      
      #seperate type 11 and 12
      df$ID_Vehicle <- substr(df$ID_Vehicle,0,2)
      #rename attribute ID_vehicle in Vehicletype
      df <- rename(df, c("ID_Vehicle" = "Vehicletype"))
      df$Vehicletype <- as.factor(df$Vehicletype)
      df <- filter(df, datum %in% data1)
      #df$Vehicletype <- as.factor(df$Vehicletype)
      #alterative version:
      #df_bar <- df %>%
      #na.omit() %>%
      #substr(Fehlerhaft_Datum,0,7) %>%
      #substr(ID_Vehicle,0,2) %>%
      #rename(Vehicletype=ID_Vehicle)
      
      #create bar plot
      ggplot(df,aes(x=Vehicletype, fill=Vehicletype)) +
        geom_bar(position = "dodge") +
        scale_x_discrete(drop=F) +
        scale_fill_manual(limits = c("11", "12"), 
                          values=c("red", "blue"))
      
    })
    
  #bartotal
    output$bartotal <- renderPlot({
      
      # omit rows with na values
      df <- na.omit(t02_affected_vehicles)
      #convert date format without days
      df$datum <- format(as.Date(df$Fehlerhaft_Datum), "%Y-%m")
      #seperate type 11 and 12
      df$ID_Vehicle <- substr(df$ID_Vehicle,0,2)
      #rename attribute ID_vehicle in Vehicletype
      df <- rename(df, c("ID_Vehicle" = "Vehicletype"))
      
      #create bar plot
      ggplot(df,aes(x=datum, color=Vehicletype))+
        geom_bar() +
        theme(axis.text.x = element_text(angle = -90,  hjust=0, vjust=0.5))+ #adjust position of x lables
        scale_x_discrete(name ="Date",
                         breaks=c("2011-01","2012-01","2013-01","2014-01","2015-01","2016-01"),
                         labels=c("2011","2012","2013","2014","2015","2016"))
    })
    
  #Vehicle affected
    output$res <- renderText({
      if (is.empty(input$search)) {
        res <- ""
      } else if(input$search %in% t02_affected_vehicles$ID_Vehicle) {
        res <- "Vehicle is affected."
      } else {
        res <-"Vehicle is not affected or vehicle ID does not exist."
      }
      res
    })
  
}

# Run the application 
shinyApp(ui=ui, server=server)