
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggvis)
library("RColorBrewer")
library(DT)
library(dplyr)
library(tidyr)
library(reshape2)
library(formattable)

crime<-read.csv('/Users/bharathpunati/Downloads/crime1.csv')

a<-subset(dummy,count>4000)
a %>% ggplot(aes(x=reorder(OFFENSE_CODE_GROUP,count), y=count,fil=YEAR))+
  geom_bar(stat='identity', width= 0.85, fill='steelblue')+
  labs(title="Total number of each crime",x="type of Crime", y="Count of the crime")+
  coord_flip()


```


##Plot 2: Horizontal Stacked Bar Chart of Report by Group Type
```{r}

df_descriptions1 <- dplyr::count(crime,OFFENSE_DESCRIPTION)
df_descriptions1 <- df_descriptions1[order(df_descriptions1$n, decreasing = TRUE),]

top_descriptions2 <- df_descriptions1$OFFENSE_DESCRIPTION[1:10]

df_newplot1 <- crime %>% 
  filter(OFFENSE_DESCRIPTION %in% top_descriptions2) %>%
  select(OCCURRED_ON_DATE, OFFENSE_DESCRIPTION, YEAR) %>%
  group_by(OFFENSE_DESCRIPTION, YEAR) %>%
  dplyr::summarise(n = length(OFFENSE_DESCRIPTION), .groups = 'keep') %>%
  data.frame()

agg_tot <- df_newplot1 %>%
  select(OFFENSE_DESCRIPTION, n) %>%
  group_by(OFFENSE_DESCRIPTION) %>%
  dplyr::summarise(tot = sum(n), .groups = 'keep') %>%
  data.frame()


ggplot(df_newplot1, aes(x = reorder(OFFENSE_DESCRIPTION, n, sum), y = n, fill = YEAR)) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  coord_flip() +
  labs(title="Crime Report Count by Type of Crime", x="Offense Type", y= "Report Count", fill="Year") +
  theme_light() +
  theme(plot.title = element_text(hjust=0.5),axis.text.x =element_text(vjust=1, size=10, colour= "black"),axis.text.y =element_text(vjust=1, size=10, colour= "black")) +
  scale_color_brewer(palette = "Accent") +
  geom_text(data=agg_tot, aes(x=OFFENSE_DESCRIPTION, y=tot, label=scales::comma(tot), fill=NULL), hjust =-0.2, size=0.4) +
  scale_y_continuous(labels = comma,
                     breaks = seq(0, 40000, by = 5000),
                     limits = c(0, 40000))


```
##plot 3 : Crime Report Count by Hour of the Day

```{r}


hours_df_project1 <- crime %>%
  select(HOUR) %>%
  group_by(HOUR) %>%
  dplyr::summarise(n=length(HOUR), .groups='keep') %>%
  data.frame()

hrs <- hours_df_project1 %>%
  filter(n == min(n) | n == max(n)) %>%
  data.frame()

x_axis_labels <-  min(hours_df_project1$HOUR):max(hours_df_project1$HOUR)



ggplot(hours_df_project1, aes(x=HOUR, y=n)) +
  geom_line(color='black', size=1) +
  geom_point(shape=21, size=4, color='red', fill='white') +
  labs(x="Hour", y="Incident Count", 
       title="Crime Incident Report Count by Hour") +
  scale_y_continuous(labels=comma) +
  theme_light() +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_x_continuous(labels=x_axis_labels, breaks=x_axis_labels, minor_breaks = NULL) +
  geom_point(data = hrs, aes(x=HOUR, y=n), shape=21, size=4, fill='red', color='red')
```

#plot 4 
```{r}

ucr_parts1 <- count(crime, UCR_PART)
ucr_parts1 <- ucr_parts1[order(),]

df_ucr <- crime %>%
  select(UCR_PART, YEAR) %>%
  dplyr::mutate(myParts = ifelse(UCR_PART=="Part Three", "Part Three",
                                 ifelse(UCR_PART=="Part Two", "Part Two",
                                        ifelse(UCR_PART=="Part One", "Part One","Other")))) %>%
  group_by(YEAR, UCR_PART) %>%
  dplyr::summarise(n=length(UCR_PART), .groups='keep') %>%
  group_by(YEAR) %>%
  dplyr::mutate(percent_of_total = round(100*n/sum(n),1)) %>%
  ungroup() %>%
  data.frame()

df_ucr$UCR_PART <- factor(df_ucr$UCR_PART, levels=c("Part One", "Part Two", "Part Three"))

ggplot(data = df_ucr, aes(x="", y=n, fill=UCR_PART)) +
  geom_bar(stat = "identity", position = "fill") +
  coord_polar(theta="y", start=0) +
  labs(fill= "Parts", x= NULL, y= NULL, title = "Crime Reports by Year by UCR Part",
       caption="Slices under 5% are not labeled") +
  theme_light() +
  theme(plot.title = element_text(hjust=0.5),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank()) +
  facet_wrap(~YEAR, ncol = 3, nrow = 2) +
  scale_fill_brewer(palette = "Reds") +
  geom_text(aes(x=1.7, label = ifelse(percent_of_total > 5,paste0(percent_of_total,"%"),"")),
            size=3,
            position = position_fill(vjust=0.5))
```



```




# Import dataset
crimedf <- read.csv("/Users/bharathpunati/Desktop/crime.csv")
#Cleaning unwanted data 
fel1 = select(crimedf,-c(SHOOTING))
#Dropping Null values
fel = na.omit(fel1)

data_group <- aggregate(list(COUNT=fel$REPORT_NUMBER), by=list(DISTRICT=fel$DISTRICT, YEAR=fel$YEAR), FUN=length);
final <- spread(data_group, DISTRICT, COUNT)

final_df <- final[-1]
row.names(final_df) <- final$YEAR

year_group <- spread(data_group, YEAR, COUNT)
year_melt_group <- melt(year_group, id.vars="DISTRICT")


fel$Count <- 1
#Column of month to name of the month
fel$MONTH <- ifelse(fel$MONTH == "1", "JANUARY",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "2", "FEBRUARY",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "3", "MARCH",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "4", "APRIL",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "5", "MAY",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "6", "JUNY",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "7", "JULY",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "8", "AUGUST",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "9", "SEPTEMBER",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "10", "OCTOBER",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "11", "NOVEMBER",fel$MONTH)
fel$MONTH <- ifelse(fel$MONTH == "12", "DECEMBER",fel$MONTH)


#UI Define
ui= dashboardPage ( 
  
  dashboardHeader( title="San Francisco Crime Analysis"
  ),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(tabName = "main", "District wise analysis", icon = icon("chart-bar")),
      menuItem(tabName = "extra3", "Overall Analysis", icon = icon("chart-bar")),
      menuItem(tabName = "extra5", "Data", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "main",
        fluidRow(  
          column (5,
                  box (width=12,
                       color = "green", 
                       selectInput("DISTRICT", "DISTRICT:", 
                                   choices=colnames(final_df)),
                       hr(),
                       helpText("Kaggle San Francisco Crime Incidents Data (2015-2018)."),
                  ),
                  box(width = 12,
                      title = "District wise crime",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("crimeIncidentsPlot")
                      )
                  ),
          ), 
          column (7,
                  box(width = 12,
                      title = "District wise analysis",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("crimeIncidentsGroupByYearLineGraph")
                      )
                  ),
          ),          
          
          
        ),
      ),
      
      
      tabItem(
        tabName = "extra3",
        fluidRow(
          column (6,
                  box(width = 15,
                      title = "Yearly analysis",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot3")
                      )
                  ),
                  box(width = 15,
                      title = "Day analysis",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot5")
                      )
                  ),
          ),
          column (6,
                  box(width = 15,
                      title = "Monthly analysis",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot4")
                      )
                  ),
                  box(width = 15,
                      title = "Hourly analysis",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot6")
                      )
                  ),
          )
          
        ),
      ),
      
      tabItem(
        tabName = "extra5",
        fluidRow(
          box(title = "Data Table", color = "green", ribbon = TRUE, width = 12,status = "success", height = "575",solidHeader = T, dataTableOutput("crimetable"))
          
          
        ),
      )
    ),
  ),
)



# Server define
server=shinyServer(function(input, output, session ) {
  
  
  
  output$crimeIncidentsPlot <- renderPlot({
    
    barplot(final_df[,input$DISTRICT],
            main=paste("District:", input$DISTRICT),
            ylab="Number of crimes",
            xlab="Year",
            names.arg = c( 2015:2018),
            col=brewer.pal(n = 4, name = "PRGn"),
            border = "black"
    )
    
  })
  
  output$crimeIncidentsGroupByYearLineGraph <- renderPlot({
    
    
    ggplot(year_melt_group , aes(x=DISTRICT, y=value,group=variable,
                                 colour=variable)) +
      geom_line()
    
  })
  
  output$plot1<- renderPlot({
    
    dd_aggr1 <- aggregate(Count ~ NAME_OFFENSE + HOUR, data = fel, FUN = sum)
    
    ggplot(data = dd_aggr1, aes(x = HOUR, y = NAME_OFFENSE)) + geom_tile(aes(fill = Count), color = "white") +
      scale_fill_gradient(low = "white", high = "steelblue") +
      theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
      theme(legend.title = element_text(size = 10),
            legend.text = element_text(size = 6),
            axis.text.y = element_text(size= 6),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
    
  })
  
  output$plot2<- renderPlot({
    dd_aggr2 <- aggregate(Count~ NAME_OFFENSE + YEAR, data = fel, FUN = sum)
    
    ggplot(data=dd_aggr2, aes(x=YEAR, y=Count, group = NAME_OFFENSE , colour = NAME_OFFENSE)) +
      geom_line() + geom_point() + theme_minimal() + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())+ theme(legend.title=element_blank(),legend.position = "right",legend.text = element_text( size = 5),)
  })
  output$plot3<- renderPlot({
    
    dd_aggr3 <- aggregate(Count ~ YEAR, data = fel, FUN = sum)
    
    ggplot(dd_aggr3, aes(x=YEAR, y= Count)) + geom_line(colour = "steelblue") + geom_point(colour = "steelblue") + theme_minimal() + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())  +  labs(x = "Year", y = "Number of crimes")
  })
  output$plot4<- renderPlot({
    
    ggplot(fel, aes(x=MONTH),border = "black")+geom_bar(stat="Count", width=0.8, fill="steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(x = "Day", y = "Number of crimes") + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
      theme(axis.text.y = element_text(size= 6),axis.text.x = element_text(size = 10, angle = 90, hjust = 1))
  })
  
  output$plot5<- renderPlot({
    
    ggplot(fel, aes(x=DAY_WEEK))+geom_bar(stat="Count", width=0.8, fill=brewer.pal(n = 7, name = "PRGn"))+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(x = "Day", y = "Number of crimes") + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
  })
  output$plot6<- renderPlot({
    
    ggplot(fel, aes(x=fel$HOUR))+geom_bar(stat="Count", width=0.8, fill="steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  labs(x = "Hours", y = "Number of crimes")
  })
  
  output$map<- renderPlot({
    
    crime1 = fel[!fel$Lat == 	-1.00000, ]
    
    agg <- aggregate(Count~ Lat + Long, data = crime1, FUN = sum)
    agg_count <- subset(agg,Count=500,)
    qmplot(Long, Lat, data = agg_count, colour = I('red'), size = I(0.5), darken = .3)
  })
  
  output$crimetable <- renderDT(datatable(fel, options = list(searching = TRUE, pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T)))
})

#APP
shinyApp(ui=ui, server=server)
