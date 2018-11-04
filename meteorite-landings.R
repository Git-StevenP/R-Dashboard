library(tidyverse)
library(dplyr)
library(ggplot2)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(htmlwidgets)
library(shiny)
library(shinydashboard)

header <- dashboardHeader(title = "Meteorites dashboard")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      "Dashboard",
      tabName = "dashboard",
      icon = icon("dashboard"),
      selected = TRUE
    ),
    menuItem(
      text = "Dashboard displays",
      icon = icon("th"),
      menuItem(
        "Maps", 
        icon = icon("bar-chart-o"),
        menuSubItem("All impacts map", tabName = "validMap", selected = TRUE),
        menuSubItem("Heatmap", tabName = "heatmap"),
        menuSubItem("Clustered map", tabName = "clusteredMap"),
        menuSubItem("Found and seen falling map", tabName = "foundSeenFallingMap"),
        menuSubItem("Class map", tabName = "classMap")
      ),
      menuItem(
        "Left-Graph", 
        icon = icon("bar-chart-o"),
        menuSubItem("Discoveries over time", tabName = "discoveriesHist", selected = TRUE),
        menuSubItem("Meteorites mass distribution", tabName = "massHist"),
        menuSubItem("Mean mass per type", tabName = "meanMassPerFall"),
        menuSubItem("Meteorites classes", tabName = "classesHist"),
        menuSubItem("Mass per year per class", tabName = "meanMassPerYearClass")
      ),
      menuItem(
        "Right-Graph", 
        icon = icon("bar-chart-o"),
        menuSubItem("Found over time", tabName = "foundHist", selected = TRUE),
        menuSubItem("Seen falling over time", tabName = "seenFallingHist"),
        menuSubItem("Mass per type", tabName = "massViolin"),
        menuSubItem("Mean mass per class", tabName = "meanMassPerClass")
      )
    ),
    menuItem("Analysis", icon = icon("bar-chart-o"), tabName = "analysis"),
    sliderInput(inputId = "date",
                label = "Date",
                min = 0,
                max = 2020,
                value = 1900
    )
  )
)
body <- dashboardBody(
  tabItems(
      tabItem(tabName = "dashboard",
            tabItems(
              tabItem(tabName = "validMap",
                fluidRow(
                  class = "text-center",
                  box(width = 12,
                      "Map of all valid impacts by five decades"
                  )
                ),
                fluidRow(
                  column(width = 10, offset="1",
                      leafletOutput("map4_dashboard", height = 270)
                  )
                )
              ),
              tabItem(
                tabName = "heatmap",
                fluidRow(
                  class = "text-center",
                  box(
                    width = 12,
                    "Heatmap of all valid impacts over time"
                  )
                ),
                fluidRow(
                  column(width = 10, offset="1",
                    leafletOutput("map2_dashboard", height = 270)
                  )
                )
              ),
              tabItem(
                tabName = "clusteredMap",
                fluidRow(
                  class = "text-center",
                  box(width = 12,
                      "Clustered map of all valid impacts over time"
                  )
                ),
                fluidRow(
                  column(width = 10, offset="1",
                    leafletOutput("map1_dashboard", height = 270)
                  )
                )
              ),
              tabItem(
                tabName = "foundSeenFallingMap",
                fluidRow(
                  class = "text-center",
                  box(width = 12,
                      "Map of all meteorites impacts over time depending on the type of their discovery"
                  )
                ),
                fluidRow(
                  column(width = 10, offset="1",
                    leafletOutput("map_dashboard", height = 270)
                  )
                )
              ),
              tabItem(
                tabName = "classMap",
                fluidRow(
                  class = "text-center",
                  box(width = 12,
                      "Map of all meteorites impacts and their mass over time depending on their classes (mass represented by radius)"
                  )
                ),
                fluidRow(
                  column(width = 10, offset="1",
                    leafletOutput("map3_dashboard", height = 270)
                  )
                )
              )
            ),
            fluidRow(
              tabItems(
                tabItem(
                  tabName = "discoveriesHist",
                    box(width = 6, plotOutput("discoveries_hist_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "massHist",
                  box(width = 6, plotOutput("mass_distribution_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "meanMassPerFall",
                  box(width = 6, plotOutput("mean_mass_col_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "classesHist",
                  box(width = 6, plotOutput("freq_class_distribution_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "meanMassPerYearClass",
                  box(width = 6, plotOutput("facetting_plot_dashboard", height = 250))
                )
              ),
              tabItems(
                tabItem(
                  tabName = "foundHist",
                    box(width = 6, plotOutput("found_meteorites_hist_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "seenFallingHist",
                    box(width = 6, plotOutput("fell_meteorites_hist_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "massViolin",
                  box(width = 6, plotOutput("violin_plot_dashboard", height = 250))
                ),
                tabItem(
                  tabName = "meanMassPerClass",
                  box(width = 6, plotOutput("mean_mass_per_class_plot_dashboard", height = 250))
                )
              )
            )
      ),
      tabItem(tabName = "analysis",
            fluidRow(
              class = "text-center",
              box(status = "primary", solidHeader = TRUE, width = 12, height = 40, title = "Meteorite landings analysis")
            ),
            fluidRow(
              class = "text-center",
              box(status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  title = "Geolocalisation over time analysis",
                  "In this section, we will be analysing the geolocalisation of the meteorites over time according to their discovery type"
              )
            ),
            fluidRow(
              valueBoxOutput("box1", width = 6),
              valueBoxOutput("box2", width = 6)
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  "Map of all valid impacts by five decades"
              )
            ),
            fluidRow(
              leafletOutput("map4_analysis")
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "As we can see on this map, the geolocalisation of the meteorite impacts concurs with where people live. Indeed, there are numbers of impacts in the continents whereas there are few of them elsewhere. But let's precise this hypothesis with a heatmap !"
              )
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  "Heatmap of all valid impacts over time"
              )
            ),
            fluidRow(
              leafletOutput("map2_analysis")
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "Looking at the heatmap above, we can confirme ours assumptions and even precise them. In fact, the distribution of reports concerning meteorites impacts worldwide is related to the density of the population. Therefore, it is logical to see heat areas on countries such as the US. But it is also possible to deduce something concerning Antartica. The heat located on this area could be deduced from the amount of laboratories present in that place and reporting every new meteorites landing."
              )
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  "Clustered map of all valid impacts over time"
              )
            ),
            fluidRow(
              leafletOutput("map1_analysis")
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "Now let's compare the discoveries of meteorites according to two types : the meteorites seen falling, and the meteorites found after their fall"
              )
            ),
            fluidRow(
              box(width = 12, plotOutput("discoveries_hist_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "As we can see on this histogram, the meteorites discoveries keep increasing over time. We can assume that this is due to the population growth, but also the improvement in technology. Let's split into two types : meteorites seen falling and meteorites found."
              )
            ),
            fluidRow(
              box(width = 6, plotOutput("found_meteorites_hist_analysis", height = 250)),
              box(width = 6, plotOutput("fell_meteorites_hist_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "The two last histogram reveal something really interesting... While meteorites found per year keep increasing the same way as discoveries in general, the number of meteorites seen falling stay fixed over the years. But why ?"
              )
            ),
            fluidRow(
              valueBoxOutput("box3", width = 6),
              valueBoxOutput("box4", width = 6)
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  "Map of all meteorites impacts over time depending on the type of their discovery"
              )
            ),
            fluidRow(
              leafletOutput("map_analysis")
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  title="Conclusion",
                  status = "success",
                  "Using this last map, we discovered something curious ! Indeed, depending on the type of discovery of the meteorites, their location is pretty different. While seen falling meteorites seem to correlate with densely populated areas, found meteorites are often located in desertic areas ! This could be explained because of the greater ease to find a meteorite in the desert rather than in moutains for example !"
              )
            ),
            fluidRow(
              class = "text-center",
              box(status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  title = "Meteorites mass analysis",
                  "In this section, we will be analysing the mass of the meteorites depending on their type of discovery"
              )
            ),
            fluidRow(
              box(width = 12, plotOutput("mass_distribution_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "According to the histogram above, the mass of the meteorites is well distributed from 0Kg to 1000Kg and has a mean mass of about 10g. But let's split this data according to thei type of discovery !"
              )
            ),
            fluidRow(
              box(width = 5, plotOutput("mean_mass_col_analysis", height = 250)),
              box(width = 7, plotOutput("violin_plot_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  title="Conclusion",
                  status = "success",
                  "Using those two last plots, we can easily conclude that there's a difference in mass between the meteorites found and the ones seen falling. Indeed, seen falling meteorites have a greater mass (and mean mass) than found meteorites."
              )
            ),
            fluidRow(
              class = "text-center",
              box(status = "primary",
                  width = 12,
                  solidHeader = TRUE,
                  title = "Meteorites class analysis",
                  "In this section, we will be analysing the class of the meteorites and their mass"
              )
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "Counting all the different classes of meteorites in the data, we've been able to bring out six main classes : H6, H5, H4, L6, L5 and LL5"
              )
            ),
            fluidRow(
              box(width = 12, plotOutput("freq_class_distribution_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "According to the histogram above, the most common meteorite class is the L6 and the least (among the main classes) is the LL5. Let's represent them on a map with their respective mass !"
              )
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  "Map of all meteorites impacts and their mass over time depending on their classes (mass represented by radius)"
              )
            ),
            fluidRow(
              leafletOutput("map3_analysis")
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "We can see on this map that the distribution of the meteorites worldwide is the same for each type of class (logic). But what matters is that we can foresee a difference between meteorites classes mean masses. Let's deepen that assumption !"
              )
            ),
            fluidRow(
              box(width = 12, plotOutput("facetting_plot_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  "Thanks to this facetting, we discover that the distribution of the different classes of meteorites is equivalent except for the LL5 which has a distribution which starts from 1970. This may have been a new class starting on this year. But what matters the most is that we can assume that the LL5 class has a lower mean mass compared to the others and the L6 class the highest. Let's confirm our thoughts."
              )
            ),
            fluidRow(
              box(width = 12, plotOutput("mean_mass_per_class_plot_analysis", height = 250))
            ),
            fluidRow(
              class = "text-center",
              box(width = 12,
                  solidHeader = TRUE,
                  title="Conclusion",
                  status = "success",
                  "With the help of this last plot, we can conclude on several things... Indeed, we firstly discovered that the distribution of the localisations of the meteorites wasn't correlated with their classes. Secondly, we learnt that the most common class of meteorite is the L6 which is 'An ordinary chondrite from the L group that is petrologic type 6'. Lastly, we discovered that in addition to be the most represented, the L6 is also the class with the highest mean mass whereas LL5 stays in last position for the two criterias (distribution and mean mass)"
              )
            )
        )
    )
)

# Create the UI using the header, sidebar, and body
ui <- dashboardPage(header, sidebar, body)

meteorite_data <- read.csv("Datasets/meteorite-landings.csv")

meteorite_data <- drop_na(meteorite_data) %>%
  filter(year <= 2020)
colnames(meteorite_data)[colnames(meteorite_data)=="reclong"] <- "lng"
colnames(meteorite_data)[colnames(meteorite_data)=="reclat"] <- "lat"
meteorite_data <- meteorite_data %>% 
  mutate(html = paste0("<b>", name," (", ifelse(fall == "Found", "Found", "Seen falling")," in ", year, ")" , "</b>", "<br/>", mass, "g"))

by_mass <- na.omit(meteorite_data) %>%
  group_by(fall) %>%
  summarize(meanMass = mean(mass))

most_found_meteor_year <- meteorite_data %>%
  filter(fall == "Found") %>%
  group_by(year) %>%
  count(fall) %>%
  ungroup() %>%
  filter(n == max(n, na.rm = TRUE))

most_fell_meteor_year <- meteorite_data %>%
  filter(fall == "Fell") %>%
  group_by(year) %>%
  count(fall) %>%
  ungroup() %>%
  filter(n == max(n, na.rm = TRUE))

most_massive_meteor <- meteorite_data %>%
  filter(mass == max(mass, na.rm = TRUE)) %>%
  select(name, mass, year)
  
oldest_meteor_discovery <- meteorite_data %>%
  filter(year == min(year, na.rm = TRUE)) %>%
  select(name, mass, year)

class_freq_table <- meteorite_data %>%
  count(recclass) %>%
  arrange(desc(n)) %>%
  head()
class_freq_table$recclass <- reorder(class_freq_table$recclass, -class_freq_table$n)

main_class_table <- meteorite_data %>%
  filter(recclass == class_freq_table$recclass) %>%
  group_by(recclass, year) %>%
  summarize(meanMassPerClass = mean(mass, na.rm = TRUE))

first_main_class_meteorite_data_since_1950 <- meteorite_data %>%
  filter(recclass == "L6", year <= 2010 & year >=1950)
second_main_class_meteorite_data_since_1950 <- meteorite_data %>%
  filter(recclass == "H5", year <= 2010 & year >=1950)
third_main_class_meteorite_data_since_1950 <- meteorite_data %>%
  filter(recclass == "H6", year <= 2010 & year >=1950)
fourth_main_class_meteorite_data_since_1950 <- meteorite_data %>%
  filter(recclass == "H4", year <= 2010 & year >=1950)
fifth_main_class_meteorite_data_since_1950 <- meteorite_data %>%
  filter(recclass == "L5", year <= 2010 & year >=1950)
sixth_main_class_meteorite_data_since_1950 <- meteorite_data %>%
  filter(recclass == "LL5", year <= 2010 & year >=1950)

meteorite_data_since_1800 <- meteorite_data %>%
  filter(year >= 1800 & year <= 2000)

meteorite_data_from_1800_to_1850 <- meteorite_data %>%
  filter(year >= 1800 & year < 1850)
meteorite_data_from_1850_to_1900 <- meteorite_data %>%
  filter(year >= 1850 & year < 1900)
meteorite_data_from_1900_to_1950 <- meteorite_data %>%
  filter(year >= 1900 & year < 1950)
meteorite_data_from_1950_to_2000 <- meteorite_data %>%
  filter(year >= 1950 & year <= 2000)

found_meteorites <- meteorite_data %>%
  filter(fall == "Found")

fell_meteorites <- meteorite_data %>%
  filter(fall == "Fell")

labs <- as.list(meteorite_data$html)
pal <- colorFactor(palette = c("green", "blue"), 
                   levels = c("Found meteorites", "Seen falling meteorites"))
classPal <- colorFactor(palette = c("green", "blue", "orange", "yellow", "red", "purple"), 
                   levels = c("L6", "H5", "H6", "H4", "L5", "LL5"))
smoothPal <- colorNumeric(palette = "Blues", 
                        domain = meteorite_data_since_1800$year)

server <- function(input, output) {
  output$discoveries_hist_dashboard <- output$discoveries_hist_analysis <- renderPlot({
    ggplot(filter(meteorite_data, year >= input$date), aes(x = year, fill = ..count..)) + geom_histogram() + scale_y_log10() + ggtitle(paste("Meteorite discoveries distribution since ", input$date)) +
      xlab("Time (year)") + ylab("Quantity of discoveries") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$violin_plot_dashboard <- output$violin_plot_analysis <- renderPlot({
    ggplot(filter(meteorite_data, year >= input$date), aes(x = fall, y = mass, fill = fall)) + geom_violin() + scale_y_log10() + ggtitle("Meteorites mass according to the type of discovery") +
      xlab("Type of discovery") + ylab("Mass (in g)") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$mass_distribution_dashboard <- output$mass_distribution_analysis <- renderPlot({
    ggplot(filter(meteorite_data, year >= input$date), aes(x = mass, fill = ..count..)) + geom_histogram() + scale_x_log10() + ggtitle("Meteorite mass distribution") +
      xlab("Mass (in g)") + ylab("Quantity of meteorites") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$mean_mass_col_dashboard <- output$mean_mass_col_analysis <- renderPlot({
    ggplot(by_mass, aes(x = fall, y = meanMass, fill = fall)) + geom_col() + ggtitle("Mean mass in terms of type of discovery") +
      xlab("Type of discovery") + ylab("Mean mass (in g)") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$freq_class_distribution_dashboard <- output$freq_class_distribution_analysis <- renderPlot({
    ggplot(class_freq_table, aes(x = recclass, y = n, fill = recclass)) + geom_col() + ggtitle("Six main meteorites class frequency") +
      xlab("Class of meteorite") + ylab("Quantity of meteorites") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$facetting_plot_dashboard <- output$facetting_plot_analysis <- renderPlot({
    ggplot(filter(main_class_table, year >= input$date), aes(x = year, y = meanMassPerClass, color = recclass)) + geom_point() + facet_wrap(~recclass) + scale_y_log10() + ggtitle("Meteorites mean mass discoveries since 1950 faceted by their class") +
      xlab("Time (year)") + ylab("Mean mass (in g)") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$mean_mass_per_class_plot_dashboard <- output$mean_mass_per_class_plot_analysis <- renderPlot({
    ggplot(filter(main_class_table, year >= input$date), aes(x = recclass, y = meanMassPerClass, fill = recclass)) + geom_col() + scale_y_log10() + ggtitle("Meteorites mean mass according to their class") +
      xlab("Meteorite class") + ylab("Mean mass (in g)") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$found_meteorites_hist_dashboard <- output$found_meteorites_hist_analysis <- renderPlot({
    ggplot(filter(found_meteorites, year >= input$date), aes(x = year, fill = ..count..)) + geom_histogram(binwidth = 10) + scale_y_log10() + ggtitle(paste("Found meteorites distribution since ", input$date)) +
      xlab("Time (year)") + ylab("Quantity of discoveries") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$fell_meteorites_hist_dashboard <- output$fell_meteorites_hist_analysis <- renderPlot({
    ggplot(filter(fell_meteorites, year >= input$date), aes(x = year, fill = ..count..)) + geom_histogram(binwidth = 10) + scale_y_log10() + ggtitle(paste("Seen falling meteorites distribution since ", input$date)) +
      xlab("Time (year)") + ylab("Quantity of discoveries") + theme(plot.title = element_text(hjust = 0.5))
  })
  output$box1 <- renderValueBox(
    valueBox(
      value = paste(most_massive_meteor["mass"], " g"),
      subtitle = "Most massive meteor named Hoba",
      icon = icon("rocket"),
      color = "blue"
    )
  )
  output$box2 <- renderValueBox(
    valueBox(
      value = paste(oldest_meteor_discovery["year"], " J.-C."),
      subtitle = "Oldest meteor discovery named Havana",
      icon = icon("calendar"),
      color = "blue"
    )
  )
  output$box3 <- renderValueBox(
    valueBox(
      value = paste(most_found_meteor_year["n"]," in " , most_found_meteor_year["year"]),
      subtitle = "Most found meteorites in a year",
      icon = icon("rocket"),
      color = "blue"
    )
  )
  output$box4 <- renderValueBox(
    valueBox(
      value = paste(most_fell_meteor_year["n"]," in " , most_fell_meteor_year["year"]),
      subtitle = "Most seen falling meteorites in a year",
      icon = icon("calendar"),
      color = "blue"
    )
  )
  output$map_dashboard <- output$map_analysis <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Esri.WorldImagery") %>%  
        addCircleMarkers(
          data = filter(found_meteorites, year >= input$date), 
          radius = 3, 
          color = ~pal(ifelse(fall == "Found", "Found meteorites", "Seen falling meteorites")),
          label = lapply(labs, HTML),
          group = "Found meteorites"
        ) %>%
        addCircleMarkers(
          data = filter(fell_meteorites, year >= input$date), 
          radius = 3, 
          color = ~pal(ifelse(fall == "Found", "Found meteorites", "Seen falling meteorites")),
          label = lapply(labs, HTML),
          group = "Seen falling meteorites"
        ) %>%
      addLegend(pal = pal,
                values = c("Found meteorites", "Seen falling meteorites"),
                title = "Type of discovery") %>%
      addLayersControl(position = "bottomleft",
                       overlayGroups = c("Found meteorites", "Seen falling meteorites")) %>%
      setView(lng = 25.71801, 
              lat = 5.803217, 
              zoom = 1) %>%
      addResetMapButton()
  })
  output$map1_dashboard <- output$map1_analysis <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  
      addCircleMarkers(
        data = filter(meteorite_data, year >= input$date), 
        radius = 3, 
        color = ~pal(ifelse(fall == "Found", "Found meteorites", "Seen falling meteorites")),
        label = lapply(labs, HTML),
        group = "Found meteorites",
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(pal = pal,
                values = c("Found meteorites", "Seen falling meteorites"),
                title = "Type of discovery") %>%
      setView(lng = 25.71801, 
              lat = 5.803217, 
              zoom = 1) %>%
      addResetMapButton()
  })
  output$map2_dashboard <- output$map2_analysis <- renderLeaflet({
     leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  
      addWebGLHeatmap(data = filter(meteorite_data, year >= input$date), size = 1000000) %>%
      setView(lng = 25.71801, 
              lat = 5.803217, 
              zoom = 1) %>%
      addResetMapButton()
  })
  output$map3_dashboard <- output$map3_analysis <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  
      addCircleMarkers(
        data = first_main_class_meteorite_data_since_1950, 
        radius = ~log10(mass), 
        color = ~classPal(recclass),
        label = lapply(labs, HTML),
        group = "L6"
      ) %>%
      addCircleMarkers(
        data = second_main_class_meteorite_data_since_1950, 
        radius = ~log10(mass), 
        color = ~classPal(recclass),
        label = lapply(labs, HTML),
        group = "H5"
      ) %>%
      addCircleMarkers(
        data = third_main_class_meteorite_data_since_1950, 
        radius = ~log10(mass), 
        color = ~classPal(recclass),
        label = lapply(labs, HTML),
        group = "H6"
      ) %>%
      addCircleMarkers(
        data = fourth_main_class_meteorite_data_since_1950, 
        radius = ~log10(mass), 
        color = ~classPal(recclass),
        label = lapply(labs, HTML),
        group = "H4"
      ) %>%
      addCircleMarkers(
        data = fifth_main_class_meteorite_data_since_1950, 
        radius = ~log10(mass), 
        color = ~classPal(recclass),
        label = lapply(labs, HTML),
        group = "L5"
      ) %>%
      addCircleMarkers(
        data = sixth_main_class_meteorite_data_since_1950, 
        radius = ~log10(mass), 
        color = ~classPal(recclass),
        label = lapply(labs, HTML),
        group = "LL5"
      ) %>%
      addLegend(pal = classPal,
                values = c("L6", "H5", "H6", "H4", "L5", "LL5"),
                title = "Type of meteorite") %>%
      addLayersControl(position = "bottomleft",
                       overlayGroups = c("L6", "H5", "H6", "H4", "L5", "LL5")) %>%
      setView(lng = 25.71801, 
              lat = 5.803217, 
              zoom = 1) %>%
      addResetMapButton()  %>%
      saveWidget(file = "discoveryClassMap.html")
  })
  output$map4_dashboard <- output$map4_analysis <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  
      addCircleMarkers(
        data = meteorite_data_from_1800_to_1850, 
        radius = 3, 
        color = ~smoothPal(year),
        fillOpacity = 0.5,
        label = lapply(labs, HTML),
        group = "From 1800 to 1850"
      ) %>%
      addCircleMarkers(
        data = meteorite_data_from_1850_to_1900, 
        radius = 3, 
        color = ~smoothPal(year),
        fillOpacity = 0.5,
        label = lapply(labs, HTML),
        group = "From 1850 to 1900"
      ) %>%
      addCircleMarkers(
        data = meteorite_data_from_1900_to_1950, 
        radius = 3, 
        color = ~smoothPal(year),
        fillOpacity = 0.5,
        label = lapply(labs, HTML),
        group = "From 1900 to 1950"
      ) %>%
      addCircleMarkers(
        data = meteorite_data_from_1950_to_2000, 
        radius = 3, 
        color = ~smoothPal(year),
        fillOpacity = 0.5,
        label = lapply(labs, HTML),
        group = "From 1950 to 2000"
      ) %>%
      addLegend(pal = smoothPal,
                values = meteorite_data_since_1800$year,
                title = "Year") %>%
      addLayersControl(position = "bottomleft",
                       overlayGroups = c("From 1800 to 1850", "From 1850 to 1900", "From 1900 to 1950", "From 1950 to 2000")) %>%
      setView(lng = 25.71801, 
              lat = 5.803217, 
              zoom = 1) %>%
      addResetMapButton()
  })
}

shinyApp(ui, server)

