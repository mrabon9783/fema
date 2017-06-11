#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(sqldf)
library(maps)

options(scipen = 999)
rstatename<- function(x) {
  result<-""
  result<-state.name[state.abb==x]
  tolower(trimws(result))
}

rstatecountyname<- function(x,y) {
  result<-""
  
  result<-paste(trimws(x),",",tolower(trimws(y)),collapse= "")
  tolower(result)
}

cleanCountyName<-function(x){
  
  result<- ""
  rt<-regmatches(x,regexpr("^.*\\(", x))
  if (length(nchar(rt))>0){
    result<-substr(rt,1,nchar(rt)-1)
  }
  return(trimws(result))
}

ofdf<-read.csv('https://www.fema.gov/api/open/v1/DisasterDeclarationsSummaries.csv', header = TRUE, sep = ',', stringsAsFactors = FALSE)

dddf<-sqldf('select max(declarationDate) as DeclarationDate, state as State, declaredCountyArea as County from ofdf where disasterNumber between 4000 and 5000 group by state, declaredCountyArea')
dddf$curdate<-Sys.Date()

dddf$DeclarationDate<-as.Date(as.character(substr(dddf$DeclarationDate,1,10), format = "%Y/%m/%d"))

dddf$DaysSinceDeclaration<-dddf$curdate-dddf$DeclarationDate

dddf$DaysSinceDeclaration<-as.integer(dddf$DaysSinceDeclaration)

dddf$DaysSinceDeclaration[is.na(dddf$DaysSinceDeclaration)]<-c(-1)

dddf<-sqldf("select * from dddf where DaysSinceDeclaration between 0 and 365")
#t <- regexpr("^.*\\(", dddf$County)
dddf$County<- sapply(dddf$County,function(x){cleanCountyName(x)})
dddf$State<- sapply(dddf$State,function(x){rstatename(x)})

data(county.fips)

colors = c("#990000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84", "#fdd49e", "#fff7ec")
dddf$colorBuckets <- as.numeric(cut(dddf$DaysSinceDeclaration, c(0, 15, 30, 45, 60, 90, 180,365)))
leg.txt <- c("< 15", "15-30", "30-45", "45-60", "60-90", "90-180","180")

cnty.fips <- county.fips$fips[match(map("county", plot=FALSE)$names,
                                    
                                    county.fips$polyname)]

#colorsmatched <- dddf$colorBuckets [match(cnty.fips, dddf$FIPSCounty)]

cntydf<- map_data("county")
cntydf<-mutate(cntydf,poly = paste(region,subregion,sep=","))

#cntydf$poly<-paste(cntydf$subregion,cntydf$region,collapse=',')
cntydf$fips<-county.fips$fips[match(cntydf$poly,
                                    
                                    county.fips$polyname)]



namedf<-dddf[,2:3]
#apply(namedf,1,function(x){rstatecountyname(x[1],x[2])})
dddf$fmatchtxt<-apply(namedf,1,function(x){rstatecountyname(x[1],x[2])})
dddf$fmatchtxt<-gsub(" ","", dddf$fmatchtxt)

dddf$FIPSCounty<-county.fips$fips[match(dddf$fmatchtxt,
                                        
                                        county.fips$polyname)]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Recent FEMA Major Presidential Disaster Declarations"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("bins",
                     "Past Number of Days:",
                     min = 1,
                     max = 365,
                     value = 180)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("femaPlot"),
         tableOutput("femaTable")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$femaPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      #hist(x, breaks = bins, col = 'darkgray', border = 'white')
      mapdata<-sqldf(paste0("select * from dddf where DaysSinceDeclaration <= ",input$bins, collapse = ''))
      cntydf$clr<-mapdata$colorBuckets[match(cntydf$fips,
                                          
                                             mapdata$FIPSCounty)]
      
      
      p1<- ggplot()
      p1<- p1 + geom_polygon(data = map_data("state"), aes(x=long, y=lat, group = group), colour = "white", fill = 'grey')
      p1<- p1 + geom_polygon(data = cntydf, aes(x=long, y=lat, fill = factor(clr),group = group), alpha = .85)
      p1<- p1 + scale_fill_manual(values = c("#990000", "#d7301f", "#ef6548", "#fc8d59", "#fdbb84", "#fdd49e", "#fff7ec"),name = "Days Since Presidential \nMajor Disaster Declaration",labels = c("<15 Days Past","15-30 Days Past","30-45 Days Past","45-60 Days Past","60-90 Days Past","90-180 Days Past","180+ Days Past") )
      p1
   })
  
   output$femaTable <- renderTable({

      mapdata<-sqldf(paste0("select * from dddf where DaysSinceDeclaration <= ",input$bins, collapse = ''))
      mapdata
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

