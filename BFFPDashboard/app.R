#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(tidyverse)
library(DT)

#Import data
data <- read_csv("aggregated_data.csv") %>% 
  replace_na(list(Grand_Total = 0, volunteers = 0))

#Set plot theme
theme_set(theme_classic())

#Country category list for selectInput choices
country_list <- c(".", unique(data$Country))
names(country_list) <- c("-", unique(data$Country))

#Brand list for selectInput choices; remove brands with a Grand Total of 0 items
item_brands <- filter(data, Grand_Total != 0)
brand_list <- c(".", unique(item_brands$Parent_company))
names(brand_list) <- c("-", unique(item_brands$Parent_company))

#Define UI

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("BFFP Dashboard", tabName = "BFFP", icon = icon("trash")),
    menuItem("Brand Spotlight", tabName = "Brands", icon = icon("copyright"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "BFFP",
            h3("This tab approximately reproduces BFFP's dashboard, which can be found",
               a("here", href = "https://www.breakfreefromplastic.org/globalbrandauditreport2020/"), "."),
            fluidRow(
              column(width = 4,
                     selectInput("BFFP_country", "Select Country",
                                 choices = country_list,
                                 selected = "-"
                     )
              )
            ),
            fluidRow(
              column(width = 4,
                     box(title = "Total Number of Plastics Recorded", width = 12,
                         tableOutput("BFFP_total"))
              ),
              column(width = 8,
                     tabBox(id = "tabs", title = "Top Polluters per year", width = 12,
                       tabPanel("2020", tableOutput("BFFP_2020Polluters")),
                       tabPanel("2019", tableOutput("BFFP_2019Polluters"))
                     )
                         
                     )
            ),
            fluidRow(
              box(title = "Recorded plastics", width = 12,
                  plotOutput("BFFP_plot")
              )
            ),
    ),
    tabItem(tabName = "Brands",
            h3("This tab presents the BFFP data based on brands and includes the breakdown of plastic types."),
            p("When there is no brand selected, feel free to browse the data using the search function in the table below for inspiration!"),
            fluidRow(
              column(width = 4,
                     selectizeInput("brands", "Browse or type in box to select brand",
                                    choices = brand_list,
                                    selected = "-",
                                    multiple = FALSE
                     )
              ),
              column(offset = 4, width = 3,
                     infoBoxOutput("Brands_countries", width = 12)
              )
            ),
            fluidRow(
              box(title = "Plastics type breakdown", width = 12,
                  plotOutput("Brands_plot"))
            ),
            fluidRow(
              dataTableOutput("Brands_table")
            )
    )
  )
)

# Put them together into a dashboardPage
ui <- dashboardPage(dashboardHeader(title = "Break Free From Plastic TidyTuesday by Sarah Sauve", titleWidth = 500),
                    sidebar,
                    body
)

server <- function(input, output) {
  
  #BFFP Tab Contents ---------------------------------------------------
  #Total national output per year
  total_table_reactfunc <- reactive({
    data <- data %>%
      filter(Country == input$BFFP_country) %>%
      group_by(Year) %>%
      summarise(Total = sum(Grand_Total, na.rm = TRUE))
    
    data$Year <- as.integer(data$Year)
    data$Total <- as.integer(data$Total)
    
    return(data)
  })
  
  output$BFFP_total <- renderTable({
    data <- total_table_reactfunc()
    return(data)
  })
  
  #Plot
  output$BFFP_plot <- renderPlot({
    ggplot(total_table_reactfunc(),
           aes(x = as.factor(Year), y = Total)) +
      geom_bar(stat = "identity", position = "dodge", width = .7) +
      xlab("Year")
  })
  
  #Tables for top 3 polluters per year
  polluters_2019reactfunc <- reactive({
    country <- filter(data, Country == input$BFFP_country)
    
    data <- country %>%
      filter(Year == 2019) %>%
      arrange(desc(Grand_Total)) %>%
      select(Parent_company, Grand_Total) %>%
      filter(Parent_company != "Unbranded") %>%
      head(n = 3) %>%
      mutate("%" = (Grand_Total/sum(country$Grand_Total))*100)
    
    colnames(data) <- c("Polluters", "Recorded Plastics", "%")
    
    return(data)
  })
  
  output$BFFP_2019Polluters <- renderTable({
    data <- polluters_2019reactfunc()
    return(data)
  })
  
  polluters2020_reactfunc <- reactive({
    country <- filter(data, Country == input$BFFP_country)

    data <- country %>%
      filter(Year == 2020) %>%
      arrange(desc(Grand_Total)) %>%
      select(Parent_company, Grand_Total) %>%
      filter(Parent_company != "Unbranded") %>%
      head(n = 3) %>%
      mutate("%" = (Grand_Total/sum(country$Grand_Total))*100)

    colnames(data) <- c("Polluters", "Recorded Plastics", "%")

    return(data)
  })

  output$BFFP_2020Polluters <- renderTable({
    data <- polluters2020_reactfunc()
    return(data)
  })
  
  
  # Brand tab contents ------------------------------------------------------
  
  output$Brands_countries <- renderInfoBox({
    if(input$brands == "."){
      unique_countries <- "0"
    }else{
      unique_countries <- length(unique(brand_selection_reactfunc()$Country))
    }
    
    infoBox(
      input$brands, paste(unique_countries, "countries"),icon = icon("hashtag"),
      color = "red", fill = TRUE
    )
  })
  
  brand_plot_reactfunc <- reactive({
    data <- data %>%
      filter(Parent_company == input$brands) %>% 
      group_by(Parent_company, Year) %>% 
      summarise(Empty = sum(Empty),
                HDPE = sum(HDPE),
                LDPE = sum(LDPE),
                O = sum(O),
                PET = sum(PET),
                PP = sum(PP),
                PS = sum(PS),
                PVT = sum(PVC)
      ) %>% 
      pivot_longer(cols = c(3:10))
    colnames(data) <- c("Company", "Year", "MaterialType", "Count")
    data$Year <- as.factor(data$Year)
    
    return(data)
  })
  
  output$Brands_plot <- renderPlot({
    ggplot(brand_plot_reactfunc(),
           aes(x = MaterialType, y = Count, fill = Year)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      scale_fill_manual(values = c("blue", "orange"))
  })
  
  brand_selection_reactfunc <- reactive({
    if(input$brands == "."){
      return(data)
    }else{
      data <- data %>%
        filter(Parent_company == input$brands) %>% 
        select(-num_events, -volunteers)
      
      return(data)
    }
  })
  
  output$Brands_table <- DT::renderDataTable({
    brand_selection_reactfunc()
  },
  rownames = FALSE)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
