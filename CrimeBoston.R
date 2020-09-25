
#Libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggvis)
library(dplyr)
library(tidyr)
library(reshape2)
library("RColorBrewer")

# Read data set
crimedf <- read.csv(file.choose())
# Remove shooting column
crimedf1 <- select(crimedf,-c(SHOOTING))
# Remove NA values
crime = na.omit(crimedf1)

grouped_data <- aggregate(list(COUNT=crime$INCIDENT_NUMBER), by=list(DISTRICT=crime$District.Name, YEAR=crime$YEAR), FUN=length);
final <- spread(grouped_data, DISTRICT, COUNT)
finaldf <- final[-1]
row.names(finaldf) <- final$YEAR
group_by_year <- spread(grouped_data, YEAR, COUNT)
group_by_year_1 <- group_by_year[-c(1),]
melt_group_by_year <- melt(group_by_year_1, id.vars="DISTRICT")


# Create a variable count 
crime$Count <- 1
# Replace month colomn by month name
crime$MONTH <- ifelse(crime$MONTH == "1", "JANUARY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "2", "FEBRUARY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "3", "MARCH",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "4", "APRIL",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "5", "MAY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "6", "JUNY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "7", "JULY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "8", "AUGUST",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "9", "SEPTEMBER",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "10", "OCTOBER",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "11", "NOVEMBER",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "12", "DECEMBER",crime$MONTH)

# Define UI
ui= dashboardPage ( 
  
  dashboardHeader( title="Boston Crime Data"
  ),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(tabName = "main", "Crime over District", icon = icon("dashboard")),
      menuItem(tabName = "extra1", "Crime over Time", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "main",
        fluidRow(  
          column (5,
                  box(width = 12,
                      selectInput("DISTRICT", "DISTRICT:", 
                                  choices=colnames(finaldf)),
                      title = "Number of Crime Incidents by District",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      plotOutput("crimeIncidentsPlot", height = "300px")
                  )
          ), 
          column (7,
                  box(width = 12,
                      title = "Summary by District",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      plotOutput("crimeIncidentsGroupByYearLineGraph", height = "380px")
                      )
                  )
          )          
      ),
      
      tabItem(
        tabName = "extra1",
        fluidRow(
          column (6,
                  
                  box(width = 15,
                      title = "Summary by Day",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot1")
                      )
                  )
          ),
          column (6,
                  box(width = 15,
                      title = "Summary by Month",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot2")
                      )
                  ),
          ),
          column (6,
                  box(width = 15,
                      title = "Summary by Hour",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot3")
                      )
                  )
          )
          
        )
      )
    )
  )
)

# Define server logic for the Shiny app
server=shinyServer(function(input, output, session ) {
  
  
  output$crimeIncidentsPlot <- renderPlot({
    
    barplot(finaldf[,input$DISTRICT],
            main=paste("District:", input$DISTRICT),
            ylab="Number of crimes",
            xlab="Year",
            names.arg = c( 2016:2019),
            col=brewer.pal(n = 4, name = "Accent"),
            border = "black"
    )
  })
  
  output$crimeIncidentsGroupByYearLineGraph <- renderPlot({
  
    
    ggplot(melt_group_by_year , aes(x=DISTRICT, y=value,group=variable,
                                    colour=variable)) +
      geom_line() +
      theme(axis.text.y = element_text(size= 6),axis.text.x = element_text(size = 10, angle = 90))
  })
  
  
  output$plot2<- renderPlot({
    ggplot(crime, aes(x=MONTH),border = "black")+geom_bar(stat="Count", width=0.8, fill="steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(x = "Day", y = "Number of crimes") + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
      theme(axis.text.y = element_text(size= 6),axis.text.x = element_text(size = 10, angle = 90, hjust = 1))
  })
  
  output$plot1<- renderPlot({
    ggplot(crime, aes(x=DAY_OF_WEEK))+geom_bar(stat="Count", width=0.8, fill=brewer.pal(n = 7, name = "PRGn"))+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(x = "Day", y = "Number of crimes") + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
  })
  
  output$plot3<- renderPlot({
    ggplot(crime, aes(x=crime$HOUR))+geom_bar(stat="Count", width=0.8, fill="steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  labs(x = "Hours", y = "Number of crimes")
  })
})



# Run the app
shinyApp(ui=ui, server=server)