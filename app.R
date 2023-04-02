#IMPORTING THE DATA

population = read.csv("population.csv", header=T)
asylum_application = read.csv("asylum_application.csv", header=T)
asylum_decisions = read.csv("asylum_decisions.csv", header=T)

as.data.frame(population)
as.data.frame(asylum_application)
as.data.frame(asylum_decisions)

head(population)
head(asylum_application)
head(asylum_decisions)

#install.packages("networkD3")
#install.packages("rnaturalearth")
#install.packages("rgdal")
#install.packages("MazamaSpatialUtils")
#install.packages("geosphere")
#install.packages("leaflet.extras")
#install.packages("dbscan")
#install.packages("sf")
#install.packages("leaflet")
#install.packages("shinydashboard")
#install.packages("plotly")
#install.packages("shinythemes")
#install.packages("bslib")
#install.packages("shinyWidgets")

library(networkD3)
library(rnaturalearth)
library(rnaturalearth)
library(rgdal)
library(MazamaSpatialUtils)
library(sp)
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(scales)                                                                                           
library(geosphere)
library(leaflet.extras)
library(dbscan)
library(dplyr)
library(geosphere)
library(magrittr)
library(sf)
library(magrittr)
library(leaflet)
library(RColorBrewer)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(bslib)
library(shinyWidgets)

#DATA CLEANING 
#CHECKING AND TREATING MISSING VALUES
#POPULATION
population[population == ""|population == " "] <- NA    
sapply(population, function(x) sum(is.na(x)))

#UNKNOWN COUNTRIES HAVE UNK ABBREVIATION 
population$Country_origin_ISO[population$Country_origin=="Unknown "] = "UNK"
population$Country_asylum_ISO[population$Country_asylum=="Unknown "] = "UNK"

#asylum_application
asylum_application[asylum_application == ""|asylum_application == " "] <- NA    
sapply(asylum_application, function(x) sum(is.na(x)))

#UNKNOWN COUNTRIES HAVE UNK ABBREVIATION 
asylum_application$Country_origin_ISO[asylum_application$Country_origin=="Unknown "] = "UNK"

#Stage_procedure AND Application_type, DON'T HAVE ANY MISSING VALUE, BUT THEY HAVE ABBREVIATIONS (NA)

#asylum_decisions
asylum_decisions[asylum_decisions == ""|asylum_decisions == " "] <- NA    

sapply(asylum_decisions, function(x) sum(is.na(x)))

#UNKNOWN COUNTRIES HAVE UNK ABBREVIATION 
asylum_decisions$Country_origin_ISO[asylum_decisions$Country_origin=="Unknown "] = "UNK" 

#Stage_procedure DON'T HAVE ANY MISSING VALUE, BUT THEY HAVE ABBREVIATIONS (NA)


#DATA EXPLORATION 
par(mfrow=c(2,2))
#refugees over time
Total_Refugees= population %>% 
  group_by(Year) %>%
  summarise(Total_Refugees=sum(Refugees_UNHCR_mandate))

Total_Refugees

ggplot(Total_Refugees, aes(Year,Total_Refugees,col=Year))+
  geom_line(size=3)+
  theme_classic()+
  ggtitle("Total UNHCR Refugees by Year") +
  xlab("year") + ylab("Refugees")+
  scale_x_continuous(breaks=seq(1950,2021,10))

#asylum requests over time 

Total_asyl_req= asylum_application %>% 
  group_by(Year) %>%
  summarise(Total_asylum_requests=sum(applied))

Total_asyl_req

ggplot(Total_asyl_req, aes(Year,Total_asylum_requests,col=Year))+
  geom_line(size=3)+
  theme_classic()+
  ggtitle("Total Asylum Requests by Year") +
  xlab("year") + ylab("Requests")+
  scale_x_continuous(breaks=seq(2000,2021,2))

#asylum decisions over time 

Total_asyl_dec= asylum_decisions %>% 
  group_by(Year) %>%
  summarise(Total_asylum_decisions=sum(Total_decisions))

Total_asyl_dec

ggplot(Total_asyl_dec, aes(Year,Total_asylum_decisions,col=Year))+
  geom_line(size=3)+
  theme_classic()+
  ggtitle("Total Asylum Decisions by Year") +
  xlab("year") + ylab("Decisions")+
  scale_x_continuous(breaks=seq(2000,2021,2))

#asylum decisions: denied vs. Recognized
Denied_recognized= asylum_decisions %>% 
  group_by(Year) %>%
  summarise(Recognized=sum(Recognized_decisions),Denied=sum(Rejected_decisions))

Denied_recognized

ggplot(Denied_recognized, aes(Year))+
  geom_line(aes(y= Recognized), color="darkgreen")+
  geom_line(aes(y= Denied), color = "darkred")+
  theme_classic()+
  ggtitle("Total Rejected and Recognized by Year") +
  xlab("year") + ylab("Decisions")+
  scale_x_continuous(breaks=seq(2000,2021,2))

# Distinct origin/ asylumn countries
#POPULATION
Unique_Country_O_POP = tibble(OC=unique(population$Country_origin))
nrow(Unique_Country_O_POP)

Unique_Country_As_POP = tibble(AC=unique(population$Country_asylum))
nrow(Unique_Country_As_POP)

#REQUESTS
Unique_Country_O_APP = tibble(OC=unique(asylum_application$Country_origin))
nrow(Unique_Country_O_APP)

Unique_Country_As_APP = tibble(AC=unique(asylum_application$Country_asylum))
nrow(Unique_Country_As_APP)

#DECISIONS
Unique_Country_O_DEC = tibble(OC=unique(asylum_decisions$Country_origin))
nrow(Unique_Country_O_DEC)

Unique_Country_As_DEC = tibble(AC=unique(asylum_decisions$Country_asylum))
nrow(Unique_Country_As_DEC)

#########################################################################
#NUMBER OF REFUGEES ORIGIN COUNTRIES BY YEAR: MAIN CONFLICTS

# FIRST, WE HAVE TO NARROW THE NUMBER OF ORIGIN COUNTRIES
# SELECTING TOTAL SUM OF REFUGEES, FILTERING BY >= 1 MILLION REFUGEES
# EXCLUDING UNKNOWN 
#47 COUNTRIES LEFT

Total_Refugees_Ocountry= population %>% 
  group_by(Country_origin) %>%
  summarise(Total_Refugees=sum(Refugees_UNHCR_mandate))%>%
  filter(Country_origin != "Unknown "& Total_Refugees>=1000000)%>%
  arrange(desc(Total_Refugees))

# USING THE LIST ABOVE TO FILTER MY DATASET 
# ONLY ORIGIN COUNTRIES WITH TOTAL REFUGEES NUMBER >= 1 MILLION

filtered_data = population%>%
  group_by(Country_origin,Year) %>%
  summarise(Total_Refugees=sum(Refugees_UNHCR_mandate))%>%
  filter(Country_origin %in% Total_Refugees_Ocountry$Country_origin)%>%
  arrange(desc(Total_Refugees))


filtered_data

# HEAT MAP
ggplot(filtered_data,aes(x=Year,y=Country_origin, fill=Total_Refugees))+
  geom_tile()+
  theme_classic()+
  scale_fill_gradientn(colours = c("white", "yellow","orange", "red"), na.value="white", labels = comma)+
  scale_x_continuous(breaks=seq(1950,2021,5))+
  ggtitle("Number of Refugees by Origin Countries", subtitle = "Only Origin Countries with total number of refugees equal or greater than 1 million") +
  xlab("Year") + ylab("Origin Country")

#########################################################################
#NUMBER OF ASYLUM REQUESTS BY ORIGIN COUNTRIES BY YEAR: MAIN CONFLICTS

# FIRST, WE HAVE TO NARROW THE NUMBER OF ORIGIN COUNTRIES
# SELECTING TOTAL SUM OF ASYLUM APPLICATIONS, FILTERING BY >= 100 THOUSAND
# EXCLUDING UNKNOWN 
#49 COUNTRIES LEFT

Total_Applications_Ocountry= asylum_application %>% 
  group_by(Country_origin) %>%
  summarise(Total_applications=sum(applied))%>%
  filter(Country_origin != "Unknown "& Total_applications>=100000)%>%
  arrange(desc(Total_applications))

# USING THE LIST ABOVE TO FILTER MY DATASET 
# ONLY ORIGIN COUNTRIES WITH TOTAL REFUGEES NUMBER >= 100 thousand

filtered_data = asylum_application %>%
  group_by(Country_origin,Year) %>%
  summarise(Total_applications=sum(applied))%>%
  filter(Country_origin %in% Total_Applications_Ocountry$Country_origin)%>%
  arrange(desc(Total_applications))


filtered_data

# HEAT MAP
ggplot(filtered_data,aes(x=Year,y=Country_origin, fill=Total_applications))+
  geom_tile()+
  theme_classic()+
  scale_fill_gradientn(colours = c("white", "yellow","orange", "red"), na.value="white", labels = comma)+
  scale_x_continuous(breaks=seq(2000,2021,2))+
  ggtitle("Number of Asylum Applications by Origin Countries", subtitle = "Only Origin Countries with total number of applications equal or greater than 100 thousand") +
  xlab("Year") + ylab("Origin Country")

#######################################################################################
#NUMBER OF ASYLUM REQUESTS BY ORIGIN COUNTRIES BY YEAR: MAIN CONFLICTS

# FIRST, WE HAVE TO NARROW THE NUMBER OF ORIGIN COUNTRIES
# SELECTING TOTAL SUM OF ASYLUM DECISIONS, FILTERING BY >= 100 thousand
# EXCLUDING UNKNOWN 
#47 COUNTRIES LEFT

Total_Decisions_Ocountry= asylum_decisions %>% 
  group_by(Country_origin) %>%
  summarise(Total_decisions=sum(Total_decisions))%>%
  filter(Country_origin != "Unknown "& Total_decisions>=100000)%>%
  arrange(desc(Total_decisions))

# USING THE LIST ABOVE TO FILTER MY DATASET 
# ONLY ORIGIN COUNTRIES WITH TOTAL REFUGEES NUMBER >= 100 thousand

filtered_data = asylum_decisions %>%
  group_by(Country_origin,Year) %>%
  summarise(Total_decisions=sum(Total_decisions))%>%
  filter(Country_origin %in% Total_Decisions_Ocountry$Country_origin)%>%
  arrange(desc(Total_decisions))


filtered_data

# HEAT MAP
ggplot(filtered_data,aes(x=Year,y=Country_origin, fill=Total_decisions))+
  geom_tile()+
  theme_classic()+
  scale_fill_gradientn(colours = c("white", "yellow","orange", "red"), na.value="white", labels = comma)+
  scale_x_continuous(breaks=seq(2000,2021,1))+
  ggtitle("Number of Asylum Decisions by Origin Countries", subtitle = "Only Origin Countries with total number of decisions equal or greater than 100 thousand") +
  xlab("Year") + ylab("Origin Country")

############################################################################ 
#FEATURE ENGINEERING
#Removing Unknown of the list of unique origin countries (population)
Unique_Country_O_POP=Unique_Country_O_POP%>%
  filter(OC != "Unknown ")%>%
  arrange(OC)
nrow(Unique_Country_O_POP)
#Removing Unknown of the list of unique asylum countries (population)
Unique_Country_As_POP = Unique_Country_As_POP%>%
  filter(AC != "Unknown ")%>%
  arrange(AC)
nrow(Unique_Country_As_POP)

#FLOWMAP 
#WHERE THE PLACES ARE LOCATED IN A GEOGRPHICAL SPACE
# DOWNLOADING LAT AND LONG DATASET 

countries_info = read.csv("countries_codes_and_coordinates.csv", header=T)
countries_info = data.frame(countries_info[,3],countries_info[,5:6])

#MATCHING WITH THE POPULATION DATASET: origin country 

population_coord2= population %>%
  left_join(countries_info, by = c('Country_origin_ISO' = 'countries_info...3.'))

#MATCHING WITH THE POPULATION DATASET: asylum country 
population_coord2= population_coord2 %>%
  left_join(countries_info, by = c('Country_asylum_ISO' = 'countries_info...3.'))

# Renaming columns 
population_coord2= population_coord2%>%
  rename(Latitude_Origin_Country=Latitude_average.x,
         Longitude_Origin_Country=Longitude_average.x,
         Latitude_Asylum_Country=Latitude_average.y,
         Longitude_Asylum_Country=Longitude_average.y)

tail(population_coord2)

#TESTING IF THE LAT AND LONG WERE ASSIGNED CORRECTLY 
test=population_coord2 %>%
  filter(is.na(Latitude_Asylum_Country))

unique(test$Country_origin)

#TRANSFORMING IN NUMERIC
population_coord2$Latitude_Origin_Country = as.numeric(as.character(population_coord2$Latitude_Origin_Country))
population_coord2$Longitude_Origin_Country = as.numeric(as.character(population_coord2$Longitude_Origin_Country))
population_coord2$Latitude_Asylum_Country = as.numeric(as.character(population_coord2$Latitude_Asylum_Country))
population_coord2$Longitude_Asylum_Country = as.numeric(as.character(population_coord2$Longitude_Asylum_Country))


#################################

#CREATING A DATASET WITH COORDINATES WITHOUT UNKNOWN 
population_coord3 =population_coord2%>%
  filter(Country_origin_ISO != "UNK" & Country_origin_ISO != "XXA" & Country_asylum_ISO != "UNK")





# SHINY APP
library(shiny)
library(shinydashboard)
# Define UI 

ui <- fluidPage(theme = shinytheme("united"),
                
                # Application title
                titlePanel(title=div("Refugees flow under UNHCR mandate", 
                                     img(src="logo.png", height=130, width=200,
                                         style="position:absolute;right:15px;z-index:1000000;"))),
                # Sidebar 
                sidebarLayout(
                  sidebarPanel(
                    sliderInput("year_filter", "Year", min = 1951, max = 2021, value = c(1960,2021)),
                    uiOutput("O_Country_filter"),
                    uiOutput("AS_Country_filter"),
                    br(),
                    br(),
                    tableOutput("IDPtable"),
                    br(),
                    br(),
                    plotlyOutput("refugees_bycountry"),
                    br(),
                    br(),
                    actionButton(inputId='source', label="Go to Data Source: UNHCR", icon = icon("th"),
                                 onclick ="window.open('https://www.unhcr.org/refugee-statistics/download/?url=3HMho5')",
                                 class = "btn-primary")
                    
                    
                  ),
                  # main panel 
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                tabPanel("Refugees Flow",
                                         h3("Refugees Map Flow"),
                                         leafletOutput("flowmap", width = "100%", height = "400px"),
                                         fluidRow(
                                           splitLayout(cellWidths = c("50%","50%"),
                                                       h3("Total Number of Refugees leaving",br(),"Origin Country"),
                                                       h3("Total Number of Refugees arriving", br(),"at Asylum Country")),
                                           splitLayout(cellWidths = c("50%","50%"),
                                                       leafletOutput("originmap"),
                                                       leafletOutput("asylummap")))),
                                tabPanel("Asylum Applications and Decisions",
                                         uiOutput("n_applications"),
                                         uiOutput("n_decisions"),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         br(),
                                         h3("Decisions related to Asylum Applications"),
                                         plotlyOutput("decision_plot", width = "100%", height = "600px")
                                ))
                    
                    
                    
                  )))

#SERVER
server <- function(input, output) {output$flowmap <- renderLeaflet({
  filtered <- population_coord3 %>%
    filter(Year %in% input$year_filter[1]:input$year_filter[2],
           Country_origin %in% input$O_Country_filter,
           Country_asylum %in% input$AS_Country_filter)%>%
    group_by(Country_origin, Country_asylum)%>%
    summarise(Refugees_UNHCR_mandate=sum(Refugees_UNHCR_mandate),
              Latitude_O_Country =unique(Latitude_Origin_Country), 
              Longitude_O_Country=unique(Longitude_Origin_Country),
              Latitude_AS_Country =unique(Latitude_Asylum_Country), 
              Longitude_AS_Country=unique(Longitude_Asylum_Country))%>%
    select(Country_origin, Country_asylum, Latitude_O_Country, Longitude_O_Country, Latitude_AS_Country, Longitude_AS_Country, Refugees_UNHCR_mandate)
  
  
  flows <- gcIntermediate(filtered[,4:3], filtered[,6:5], sp = TRUE, addStartEnd = TRUE)
  
  refugees <- filtered$Refugees_UNHCR_mandate
  
  
  hover <- paste0(filtered$Country_origin, " to ", 
                  filtered$Country_asylum, ': ', 
                  as.character(refugees))
  
  pal = colorFactor(brewer.pal(4, 'Set2'), filtered$Country_origin)
  
  leaflet() %>% 
    addProviderTiles("OpenStreetMap")%>%
    addPolylines(data= flows, label = hover, group = filtered$Country_origin, color = pal(filtered$Country_origin),weight = (refugees/100000))
})


# ORIGIN COUNTRY MAP 
output$originmap <- renderLeaflet({
  filtered <- population_coord2 %>%  #USING THE ORIGINAL DATASET BECAUSE WE NEED ALL KNOWN ORIGIN COUNTRY DATA REGARDLESS IF THE ASYLUM COUNTRY IS UNKNOWN 
    filter(Year %in% (input$year_filter[1]:input$year_filter[2]),
           Country_origin %in% input$O_Country_filter,
           Country_origin_ISO != "UNK")%>%
    group_by(Country_origin)%>%
    summarise(Refugees_UNHCR_mandate = sum(Refugees_UNHCR_mandate),Latitude_O_Country =unique(Latitude_Origin_Country), Longitude_O_Country=unique(Longitude_Origin_Country))
  
  
  
  refugees <- filtered$Refugees_UNHCR_mandate
  
  hover <- paste0(filtered$Country_origin, " : ", 
                  as.character(refugees))
  
  pal = colorFactor(brewer.pal(4, 'Set2'), filtered$Country_origin)
  
  
  leaflet() %>%
    addProviderTiles("OpenStreetMap")%>%
    addCircleMarkers(filtered$Longitude_O_Country,filtered$Latitude_O_Country, weight = (refugees/100000), label = hover, group = filtered$Country_origin, color = pal(filtered$Country_origin))
})


# ASYLUM COUNTRY MAP 
output$asylummap <- renderLeaflet({
  filtered <- population_coord2 %>%  #USING THE ORIGINAL DATASET BECAUSE WE NEED ALL KNOWN ASYLUM COUNTRY DATA REGARDLESS IF THE ORIGIN COUNTRY IS UNKNOWN 
    filter(Year %in% (input$year_filter[1]:input$year_filter[2]),
           Country_asylum %in% input$AS_Country_filter,
           Country_asylum_ISO != "UNK")%>%
    group_by(Country_asylum)%>%
    summarise(Refugees_UNHCR_mandate = sum(Refugees_UNHCR_mandate),Latitude_AS_Country =unique(Latitude_Asylum_Country), Longitude_AS_Country=unique(Longitude_Asylum_Country))
  
  
  refugees <- filtered$Refugees_UNHCR_mandate
  
  hover <- paste0(filtered$Country_asylum, " : ", 
                  as.character(refugees))
  
  pal = colorFactor(brewer.pal(4, 'Set2'), filtered$Country_asylum)
  
  leaflet() %>%
    addProviderTiles("OpenStreetMap")%>%
    addCircleMarkers(filtered$Longitude_AS_Country,filtered$Latitude_AS_Country, weight = (refugees/100000), label = hover, group = filtered$Country_asylum, color = pal(filtered$Country_asylum))
  
  
})


# ORIGIN COUNTRY FILTER 
output$O_Country_filter <- renderUI({
  Country_origin = population_coord3 %>%
    filter(Year %in% req(input$year_filter[1]:input$year_filter[2]))%>%
    select(Country_origin)%>%
    unique()%>%
    arrange()
  
  selectizeInput("O_Country_filter", "Origin Country", choices = Country_origin$Country_origin, multiple = TRUE, selected = Country_origin$Country_origin[1])
  
})   

# ASYLUM COUNTRY FILTER 
output$AS_Country_filter <- renderUI({
  Asylum_Country = population_coord3 %>%
    filter(Year %in% req(input$year_filter[1]:input$year_filter[2]), 
           Country_origin %in% input$O_Country_filter)%>%
    select(Country_asylum)%>%
    unique()%>%
    arrange()
  
  
  selectizeInput("AS_Country_filter", "Asylum Country",choices=Asylum_Country$Country_asylum, multiple = TRUE, selected = Asylum_Country$Country_asylum[1])
})

# INTERNALLY DISPLACED TABLE 
output$IDPtable <- renderTable({
  IDPtable = population_coord3 %>%
    filter(Year %in% input$year_filter[1]:input$year_filter[2], 
           Country_origin %in% input$O_Country_filter) %>%
    group_by(Country_origin)%>%
    summarise(Internally_Displaced_Persons= sum(IDPs_concern_UNHCR), Asylum_seekers = sum(Asylum_seekers))%>%
    select(Country_origin, Internally_Displaced_Persons,Asylum_seekers)%>%
    arrange(desc(Internally_Displaced_Persons))
})
# NUMBER OF ASYLUM APPLICATIONS 
output$n_applications <- renderUI({
  application = asylum_application %>%
    filter(Year %in% input$year_filter[1]:input$year_filter[2], 
           Country_origin %in% input$O_Country_filter,
           Country_asylum %in% input$AS_Country_filter) %>%
    group_by(Country_origin)%>%
    summarise(Applications= sum(applied))%>%
    select(Country_origin, Applications)
  
  
  lapply(seq_along(application$Applications), function(i) { 
    valueBox(formatC(application$Applications[i],format="d", big.mark=','),
             paste('Asylum applications from', application$Country_origin[i]),
             color = "blue")
    
    
  })      
  
})

# NUMBER OF ASYLUM DECISIONS 
output$n_decisions <- renderUI({
  decision = asylum_decisions %>%
    filter(Year %in% input$year_filter[1]:input$year_filter[2], 
           Country_origin %in% input$O_Country_filter,
           Country_asylum %in% input$AS_Country_filter) %>%
    group_by(Country_origin)%>%
    summarise(N_decisions= sum(Total_decisions))%>%
    select(Country_origin, N_decisions)
  
  
  lapply(seq_along(decision$N_decisions), function(i) { 
    valueBox(formatC(decision$N_decisions[i],format="d", big.mark=',', width = 4),
             paste('Asylum decisions for', decision$Country_origin[i]),
             color = "aqua")
    
  })      
  
})

# DECISIONS: PLOT

output$decision_plot <- renderPlotly({
  decisions = asylum_decisions %>%
    filter(Year %in% input$year_filter[1]:input$year_filter[2], 
           Country_origin %in% input$O_Country_filter,
           Country_asylum %in% input$AS_Country_filter)%>%
    group_by(Country_origin, Country_asylum, Year)%>%
    summarise(Year = unique(Year), 
              Recognized = sum(Recognized_decisions),
              Rejected = sum(Rejected_decisions),
              Closed = sum(Otherwise_closed), 
              Complementary_protection = sum(Complementary_protection), 
              Country_asylum = unique(Country_asylum))%>%
    select(Year, Country_origin,Country_asylum, Recognized,Rejected,Closed, Complementary_protection)
  
  
  recognizeplot = plot_ly(decisions, x = ~Year, y = ~Recognized, type = 'scatter', mode = 'lines',color= ~Country_origin,  linetype = ~Country_asylum, legendgroup = ~Country_origin,
                          hoverinfo = "text",
                          hovertext = paste(decisions$Year,",", "from<b>", decisions$Country_origin, "</b>to<b>", decisions$Country_asylum, ":", decisions$Recognized,
                                            "</b><br>Closed Applications :", decisions$Closed, "<br>Complementary Protection", decisions$Complementary_protection)) 
  
  
  rejectedplot = plot_ly(decisions, x = ~Year, y = ~Rejected, type = 'scatter', mode = 'lines',color= ~Country_origin,  linetype = ~Country_asylum, legendgroup = ~Country_origin, showlegend = FALSE,
                         hoverinfo = "text",
                         hovertext = paste(decisions$Year,",", "from<b>", decisions$Country_origin, "</b>to<b>", decisions$Country_asylum, ":", decisions$Rejected, 
                                           "</b><br>Closed Applications :", decisions$Closed, "<br>Complementary Protection", decisions$Complementary_protection)) 
  
  
  
  x <- list(
    title = "Year")
  y <- list(
    title = "Recognized")
  
  #font 
  t <- list(
    size = 12,
    color = 'black')
  
  subplot(recognizeplot, rejectedplot, nrows =2, shareX=TRUE, shareY=TRUE, titleX=TRUE, titleY=TRUE)%>%
    layout(font=t, xaxis = x, yaxis = y)%>% 
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent')
  
})

# REFUGEES BY COUNTRY 

output$refugees_bycountry <- renderPlotly({
  filtered <- population_coord3 %>%
    filter(Country_origin %in% input$O_Country_filter) %>%
    group_by(Country_origin, Year)%>%
    summarise(Total_Refugees = sum(Refugees_UNHCR_mandate))
  
  plot_ly(filtered, x = ~Year, y = ~Total_Refugees, type = 'scatter', mode = 'lines',color= ~Country_origin)%>%
    layout(title = '<b>Number of refugees under UNHCR mandate</b>',
           xaxis = list(title = 'Year'),
           yaxis = list (title = 'Refugees'))%>% 
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent')
  
  
  
  
})



}
shinyApp(ui = ui, server)

