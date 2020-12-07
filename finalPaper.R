
#https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv
library(readxl)
library(shiny)
library(Lahman)
library(tidyverse)
library(plotly)
library(albersusa)

#Step 1
gettingPopulation <- read_excel("../final project/gettingPopulation.xlsx", skip = 3)
ddd <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

gettingPopulation <- gettingPopulation %>%
  select("...1", "...2", "43647")

gettingPopulation <- rename(gettingPopulation, "Rank" = "...1", "CountryState" = 
                              "...2", "Population" = "43647")

covid19 <- ddd %>%
  select(date, county, state, cases)

gettingPopulation <- gettingPopulation %>%
  separate(CountryState, into = c("Country", "State"), sep=", ")

gettingPopulation <- gettingPopulation %>%
  separate(Country, into = c("County", "Junk"), sep = " ")

gettingPopulation <- gettingPopulation %>%
  select(County, State, Population)

gettingPopulation <- na.omit(gettingPopulation)

combined <- gettingPopulation %>%
  left_join(covid19, by = c("County" = "county", "State" = "state"))

combined <- combined %>%
  mutate(infection_rates_percentage = (cases / Population) * 100)

#Step 2
us_county <- counties_sf("laea")

my_map_theme <-function(){
  theme(panel.background=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank())
}
ggplot(us_county) +
  geom_sf(size = 0.1) +
  my_map_theme()

covidLastDay <- combined %>% filter(date==as.Date("2020-11-17"))

mapdd <- us_county %>%
  left_join(covidLastDay, by=c("name" = "County"))

ggplot(mapdd) +
  geom_sf(aes(fill = infection_rates_percentage),size=0.1) +
  scale_fill_continuous("COVID-19's \nInfection rate (%)", low="blue", high="red") +
  labs(title = "Infection rates in\nfrom USA's counties\nat 2020-11-17") +
  my_map_theme()

v <- ggplot(mapdd) + 
  geom_sf(aes(fill = infection_rates_percentage),size=0.1) +
  scale_fill_continuous("COVID-19's \nInfection rate (%)", low="blue", high="red") +
  labs(title = "Infection rates in\nfrom USA's counties\nat 2020-11-17") +
  my_map_theme()

ggplotly(v) %>%
  style(hoveron = "fill")


#Step 3
covidAnalyze <- function(mydate, varget){
  varset <- enquo(varget)
  selected <- combined %>%
    select(County, State, date, var = !!varset)

  covidLastDay <- selected %>% 
    filter(date==as.Date(mydate))

  mapdd <- us_county %>%
    left_join(covidLastDay, by=c("name" = "County"))
  
  v <- ggplot(mapdd) +
    geom_sf(aes(fill = var),size=0.1) +
    scale_fill_continuous("Showing number of cases or rate of infections in %",low="blue", high="red") +
    my_map_theme()
  
  ggplotly(v) %>%
    style(hoveron = "fill")
}


#Step 4
ui <- fluidPage(
  
  titlePanel("Analyze about USA's counties during COVID-19"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("varset",
                  "Major elements in the COVID-19's map:",
                  choices = list("Infection Rate" = "infection_rates_percentage",
                                 "Number of COVID-19's cases" = "cases"),
                  selected = "cases"),
      sliderInput("mydate",
                 "Select a date to display:",
                 min = as.Date("2020-03-05","%Y-%m-%d"),
                 max = as.Date("2020-11-25","%Y-%m-%d"),
                 value = as.Date("2020-05-20"),
                 timeFormat="%m-%d")
      ),
    
    mainPanel(
      plotlyOutput("map")
    )
                  
    )
)

server <- function(input, output){
  output$map <- renderPlotly({
    covidAnalyze(input$mydate, input$varset)
  })
}

shinyApp(ui = ui, server = server)

