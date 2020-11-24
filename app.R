library(shiny)
library(readxl)
library(tidyr)
library(ggplot2)
library(plotly)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(DT)
rate <- read_excel("dataset.xlsx", sheet ="Sheet5")
location <- read_excel("dataset.xlsx", sheet = "Sheet2")
age <- read_excel("dataset.xlsx", sheet = "Sheet8")
causes <- read_excel("dataset.xlsx", sheet = "Sheet10")

coords <- list(
  c(-16.881477,145.747955),
  c(-27.373809,153.122553),
  c(-24.891305,152.322024),
  c(-26.417984,146.257248),
  c(-23.443747,144.247124),
  c(-20.724871,139.487414),
  c(-23.372583,150.475559),
  c(-26.543734, 148.779203),
  c(-19.252827, 146.773093),
  c(-31.999156,141.467884),
  c(-31.999055, 141.468188),
  c(-33.919397, 150.991899),
  c(-32.215385, 148.565881),
  c(-37.727308, 144.894087),
  c(-33.932933, 151.186901),
  c(-36.740081, 142.215371),
  c(-37.818360, 145.001859),
  c(-37.765997, 145.029465),
  c(-37.578112, 143.849999),
  c(-37.736754, 144.853925),
  c(-38.017576, 145.183921),
  c(-34.230761, 142.084554),
  c(-38.215013, 146.418498),
  c(-38.106491, 147.080954),
  c(-38.105940, 147.079483),
  c(-36.376596, 145.403877),
  c(-36.363984, 146.292666),
  c(-36.138228, 146.876298),
  c(-38.601553, 145.591696),
  c(-34.949470, 138.521337),
  c(-12.410809, 130.885839),
  c(-23.699011, 133.879628),
  c(-30.442832, 137.167756),
  c(-27.300923, 133.621628),
  c(-29.650284, 138.063812),
  c(-32.509345, 137.718847),
  c(-32.093230, 115.879569),
  c(-17.947415, 122.233632),
  c(-30.785029, 121.459630),
  c(-26.524524, 118.541970),
  c(-20.378059, 118.631921)
)

coords <- as.data.frame(do.call("rbind", coords))
colnames(coords) <- c("latitude","longitude")
location <- cbind(location,coords)
rate_1 <- filter(rate, rate$State != "Australia")
age_1 <- filter(age, age$State != "Australia")

ui <- fluidPage(
  
  # Application title
  titlePanel("Suicide in Australia: The Highlighted Facts and The Possible Solution"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("What Did We Experience?"),
      p("Suicide has put more pressure on Australia given the fact that the suicide rates were seen on the rise recently. It is learned that only in 2017, there were more than 3,000 suicide deaths, mostly of 15 - 44-year-old Australian. 
        Likewise, the rural and remote areas did not deviate from the national norm, however, it was even more problematic. Having limited access to mental health services, they experienced higher suicide rates than the Australia's average."),
      sliderInput("Year",
                  "Click Play:",
                  min = 2008,
                  max = 2017,
                  value = 2008, sep = "", animate = animationOptions(interval = 1300, loop = FALSE)),
      pickerInput("State_selected", "Select State(s):",
                  choices = as.character(unique(rate_1$State)),
                  multiple = TRUE,
                  options = list(maxItems = 8, placeholder = 'Select at least one State')
                  ),
      helpText(HTML('<p style="font-size: 9pt">Select States to learn about their suicide rates and number of suicide deaths compared with the national figure.
               Click Play button to see how they changed over 9 years.</p>')),
      radioButtons("select_agegroup", "Select an Age Group:",
                   c("15-24 years" = "15-24 years",
                     "25-34 years" = "25-34 years",
                     "35-44 years" = "35-44 years"),
                   selected = "15-24 years"
      ),
      helpText(HTML('<p style="font-size: 9pt">Select an Age Group to see its top causes of deaths in 2017.</p>')),
      h3("What Can We Do About It?"),
      p("Given that the remote and rural areas have suffered a serious shortage of mental services and emergency treatment, The Royal Flying Doctor Service (RFDS) has made attempts to make healthcare clinics available to people in the areas where they are provided with general practice services. The RFDS is expanding their service accordingly with the increasing demand. 
        Therefore, it is held high hope that poor mental health issues may be addressed, eventually, reducing the number of suicide in Australia."),
      selectInput("statebase_selected", "Get Connected With Local RDFS:",
                  choices = as.character(unique(location$State)),
                  selected = "QLD"),
    helpText(HTML('<p style="font-size: 9pt">Select the state to see all available RDFS bases there.</p>'))),
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Suicide Age Standardised Rate",plotlyOutput("rateplot")),
                  tabPanel("Number of Suicide Deaths", plotlyOutput("ageplot")),
                  tabPanel("Top Causes of Deaths Per Age Group", plotlyOutput("agespecificplot"))),            
      tabsetPanel(type = "tabs",
                  tabPanel("RFDS Service Map",leafletOutput("map", width = "100%", height = "400px")),
                  tabPanel("Details of RFDS Bases",DT::dataTableOutput("locationbase")))
      
      
    )
  )
)



server <- function(input, output) {
  output$rateplot <- renderPlotly({
    if (length(input$State_selected) < 1) {
      ggideal_point <- ggplot(rate) +
        geom_line(data = filter(rate, rate$State =="Australia"),aes(x = Year, y = Rate, colour = State), linetype="dotted") +
        geom_point(data = filter(rate, rate$State =="Australia"),aes(x = Year, y = Rate, colour = State), linetype="dotted",  size = 1) +
        scale_colour_hue(l = 70, c = 150) + labs(title = "Suicide Age Standardised Rate of Australia ") +
        theme(legend.direction = "horizontal",legend.position = "bottom") + ylim(c(0,25)) + scale_x_continuous(breaks = seq(2008,2017,1)) +
        geom_vline(xintercept = input$Year, linetype="dotted", color = "black", size=0.5)
    } else {
      bystate<-  dplyr::filter(rate_1, State %in%  input$State_selected)
      # Graph title
      if (length(input$State_selected) > 2) {
        states_names_comma <- paste(input$State_selected[-length(input$State_selected)], collapse = ',
                               ')
        states_names <- paste0(states_names_comma, ", and ",
                               input$State_selected[length(input$State_selected)])
      } else {
        states_names <- paste(input$State_selected, collapse = ' and ')
      }
      
      graph_title <- paste("Suicide Age Standardised Rate of Australia and ", states_names, sep="")
      
      ggideal_point <- ggplot(rate) +
        geom_line(data = filter(rate, rate$State =="Australia"),aes(x = Year, y = Rate, colour = State), linetype="dotted") +
        geom_point(data = filter(rate, rate$State =="Australia"),aes(x = Year, y = Rate, colour = State), linetype="dotted",  size = 1) +
        geom_line(data = bystate, aes(x = Year, y = Rate, color = State)) +
        geom_point(data = bystate,aes(x = Year, y = Rate, colour = State)) +
        labs(x = "Year", y = "Age Standardised", title = graph_title) +
        scale_colour_hue(l = 70, c = 150) + 
        theme(legend.direction = "horizontal", legend.position = "bottom") + ylim(c(0,25)) + scale_x_continuous(breaks = seq(2008,2017,1)) +
        geom_vline(xintercept = input$Year, linetype="dotted", color = "black", size=0.5) 
      gg <- plotly_build(ggideal_point)
      
    }
  })
  
    output$ageplot <- renderPlotly({
      if (length(input$State_selected) < 1) {
        ggideal_area <- ggplot(age) +
          geom_area(data = filter(age, age$State =="Australia"),aes(x = Year, y = Count, fill = State, group = State)) +
          scale_colour_hue() + labs(title ="Total Number of Suicide Deaths in Australia") +
          theme(legend.direction = "horizontal",legend.position = "bottom") + scale_x_continuous(breaks = seq(2008,2017,1)) +
          geom_vline(xintercept = input$Year, linetype="dotted", color = "black", size=0.5)
      } 
      else {
        bystate_age <-  dplyr::filter(age_1, State %in%  input$State_selected)
        # Graph title
        if (length(input$State_selected) > 2) {
          states_names_comma_2 <- paste(input$State_selected[-length(input$State_selected)], collapse = ',
                                      ')
          states_names_2 <- paste0(states_names_comma_2, ", and ",
                                 input$State_selected[length(input$State_selected)])
        } else {
          states_names_2 <- paste(input$State_selected, collapse = ' and ')
        }
        
        graph_title_2 <- paste("Number of Suicide Deaths In ", states_names_2, " Out of Total Australia", sep="")
        
        ggideal_area <- ggplot(age) +
          geom_area(data = filter(age, age$State  =="Australia"),aes(x = Year, y = Count, group = State, fill = State)) +
          geom_area(data = bystate_age, aes(x = Year, y = Count, group = State, fill = State), position = "stack") +
          scale_colour_hue() + labs(x = "Year", y = "Count", title = graph_title_2) +
          theme(legend.direction = "horizontal",legend.position = "bottom") + scale_x_continuous(breaks = seq(2008,2017,1)) +
          geom_vline(xintercept = input$Year, linetype="dotted", color = "black", size=0.5) 
        
        gg <- plotly_build(ggideal_area)
        
      }
    })
    
  
    output$agespecificplot <- renderPlotly({
    
    byagegroup <- causes[causes$`Age Group` ==input$select_agegroup,]
    byagegroup <- byagegroup %>% arrange(desc(Number))
    #Create title
    state_title <- paste(input$select_agegroup)
    graph2_title <- paste("Top Underlying Cause of Death of ", state_title, " \n In 2017", sep="")
    
    
    p1 <- ggplot(data = byagegroup, aes(y= `Causes`, x = `Number`))
    p1 + geom_point(data=filter(byagegroup, Causes == "Intentional self-harm"), colour = "red", size = 4) +  geom_segment(data=byagegroup, aes(x = 0, y = `Causes`, xend =`Number` ,yend=`Causes`),linetype = 2, colour = "red") +
      geom_point(data=filter(byagegroup, Causes != "Intentional self-harm"), colour = "blue")  + 
      labs(y= " ", x = "Number of Death", title = graph2_title) + theme(axis.text.y=element_text(size = 7)) +
      geom_segment(data = filter(byagegroup, Causes != "Intentional self-harm"), aes(x = 0, y = `Causes`, xend =`Number` ,yend=`Causes`),linetype = 2, colour = "blue") 
    
    
  })
  
  
  output$map <- renderLeaflet({ 
    mapplot<- leaflet(data = location) %>% addTiles() %>% setView(lng = 134, lat = -28, zoom = 3.5) %>%
      addMarkers(~longitude, ~latitude, label = ~as.character(location$Name))
  })
  
  
  output$locationbase <-  DT::renderDataTable({
    bylocation <- subset(location[,-c(1, 7:9)],location$State == input$statebase_selected)
  },
  option= list(lengthMenu = c(5, 30, 50),pageLength = 5, searching=TRUE)
  
  )
}
# Run the application 
shinyApp(ui = ui, server = server)