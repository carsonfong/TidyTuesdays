Introduction
------------

I found out about [Break Free From
Plastic](https://www.breakfreefromplastic.org/)’s Brand Audits through
my involvement with the local [Social Justice Cooperative of
Newfoundland and Labrador](https://www.sjcnl.ca/)’s Zero Waste Action
Team, which I have been volunteering with for little over a year. One of
my colleagues and friends proposed an audit in St. John’s, partially to
contribute to the global audit and as part of a bigger project to
understand the sources of plastic in our city. We completed our audit in
October 2020 and are the first submission to BFFP from Newfoundland! You
can find our data presented in
[this](https://sarahsauve.shinyapps.io/BrandAuditDashboard/) Shiny
dashboard.

Then when I found out that the 2019 and 2020 global audit data was
available online, my first thought was that it would make an excellent
Tidy Tuesday dataset! Very nerdy, I know, but there you are. It’s an
interesting dataset, with lots of room to play around and so many
options for visualization, plus plastic pollution is an important topic
to talk about and raise awareness of! You can read BFFP’s Brand Audit
Reports for
[2018](https://www.breakfreefromplastic.org/globalbrandauditreport2018/),
[2019](https://www.breakfreefromplastic.org/globalbrandauditreport2019/)
and
[2020](https://www.breakfreefromplastic.org/globalbrandauditreport2020/)
to get an idea of what they’ve done with the data.

The data
--------

The data is available through Google Drive; you can find the 2019 data
[here](https://drive.google.com/drive/folders/1O75ekNUQPbAAZ8KE5kb2EdbKgxIhz7HP)
and the 2020 data
[here](https://drive.google.com/drive/folders/1mdIsoaj5vW368YWw7-vD2hDFANqaS_Lh).
As far as I can tell, the 2018 data was not made public. There are more
folders in the 2019 folder than 2020; I downloaded the Countries folder
from 2019 to match the 2020 data format.

#### The plan

Just so you know what you’re getting into and how this blog is going to
read, I’ll start by laying out my plan. In Part 1, I’ll manipulate the
data so that all the CSV files are aggregated into one large data set
with total counts by year, country, brands and type of plastic. Then,
I’ll walk through replicating (Part 2) and extending (Part 3) BFFP’s
[dashboard](https://www.breakfreefromplastic.org/globalbrandauditreport2020/)
using Shiny. If you want to have a peek at the end product, you can find
the dashboard
[here](https://sarahsauve.shinyapps.io/TidyTuesdayBrandAuditDashboard/).

Part 1 - Data manipulation
--------------------------

### Step 1 - Data import

Since there are a lot of files to import, we’re going to create a
function that will not only import all of the files but collate them
into a single data set. I’m going to import each year separately first,
then merge the two years at the end.

I’ve adapted a function that a friend of mine, [David
Baker](https://davidjohnbaker.rbind.io/), made for me years ago when I
had lots of files to import - I use it all the time now so thanks Dave!
We’ll walk through each part one at a time before putting it all
together and using it.

So, the first step is to gather all the file names found in a particular
folder using `list.files` and then start importing the raw data in those
files. I’ll start with the first file in 2019 for now. Later, in the
function, we’ll run a loop that will go through all the files in the
list.

    fns <- list.files("./2019/", pattern=".csv")

    pathname <- paste0("./2019/", fns[1])

    rawdata <- read_csv(pathname)

### Step 2 - Isolate country name

Next, we want to extract the country name in the file title to add to
the data as a new column. This will only apply to the 2019 data in the
end, the 2020 data already has this information in the CSV files.

    filetitle <- basename(fns[1]) %>% 
      str_sub(1, -5) #keep all characters except last 4

    rawdata$Country <- filetitle

### Step 3 - Ensure all files are formatted in the same way

Ideally, every country would have submitted data for all types of
plastic and all country files would have the same number of columns, but
unfortunately that’s not the case for the 2019 data (it is for the 2020
data). Compare for example Argentina and Australia:

    head(read_csv("./2019/Argentina.csv"))

    ## Parsed with column specification:
    ## cols(
    ##   ParentCoFinal = col_character(),
    ##   EMPTY = col_double(),
    ##   HDPE = col_double(),
    ##   LDPE = col_double(),
    ##   O = col_double(),
    ##   PET = col_double(),
    ##   PP = col_double(),
    ##   PS = col_double(),
    ##   PVC = col_double(),
    ##   `Grand Total` = col_double(),
    ##   `number of events` = col_double(),
    ##   `number of volunteers` = col_double()
    ## )

    ## # A tibble: 6 x 12
    ##   ParentCoFinal EMPTY  HDPE  LDPE     O   PET    PP    PS   PVC `Grand Total`
    ##   <chr>         <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>         <dbl>
    ## 1 Grand Total       0   215    55   607  1376   281   116    18          2668
    ## 2 Unbranded         0   155    50   532   848   122   114    17          1838
    ## 3 The Coca-Col~     0     0     0     0   222    35     0     0           257
    ## 4 Secco             0     0     0     0    39     4     0     0            43
    ## 5 Doble Cola        0     0     0     0    38     0     0     0            38
    ## 6 Pritty            0     0     0     0    22     7     0     0            29
    ## # ... with 2 more variables: `number of events` <dbl>, `number of
    ## #   volunteers` <dbl>

    head(read_csv("./2019/Australia.csv"))

    ## Parsed with column specification:
    ## cols(
    ##   ParentCoFinal = col_character(),
    ##   PET = col_double(),
    ##   `Grand Total` = col_double(),
    ##   `number of events` = col_double(),
    ##   `number of volunteers` = col_double()
    ## )

    ## # A tibble: 2 x 5
    ##   ParentCoFinal      PET `Grand Total` `number of events` `number of volunteers`
    ##   <chr>            <dbl>         <dbl>              <dbl>                  <dbl>
    ## 1 Woolworths Group     3             3                  1                      5
    ## 2 Grand Total          3             3                  1                      5

So, we need to create a function to add columns to those files missing
them.

    test.columns <- function(file){
      column.names <- c("EMPTY", "HDPE", "LDPE", "O", "PET", "PP", "PS", "PVC")
      
      for(j in 1:8){ #for each type of plastic in column.names
        if(!column.names[j] %in% colnames(file)){ #if the column name is not a part of the file's column names
          file$new <- 0                           #create a new column
          colnames(file)[ncol(file)] <- column.names[j]  #name the column
        }
      }
      
      #rearrange column order to match order of 2020 files
      
      data <- select(file, "Country", "ParentCoFinal", "EMPTY", "HDPE", "LDPE", "O", "PET", "PP", "PS", "PVC", "Grand Total", "number of events", "number of volunteers")
      
      data
      
    }

### Step 4 - Combine into function

Now, we combine all the steps above into a single function, looping
through each file and collating them into one big data frame.

    import2019 <- function(fns=list.files("./2019/", pattern=".csv")){
      
      fulldata <- NULL

      for(i in seq(along=fns)){   #go through each file one at a time
        
        pathname <- paste0("./2019/", fns[i])

        rawdata <- read_csv(pathname)
        
        #Country information
        filetitle <- basename(fns[i]) %>% 
          str_sub(1, -5) #keep all characters except last 4
        
        rawdata$Country <- filetitle
        
        #Add columns as necessary and rearrange all columns
        rawdata <- test.columns(rawdata)
        
        #Remove first row, which is the total for each plastic type category; this can easily be calculated later if needed
        rawdata <- filter(rawdata, ParentCoFinal != "Grand Total")
        
        #Add to full data set
        
        fulldata <- rbind(fulldata, rawdata)
        
      }
      
      fulldata

    }

    data_2019 <- import2019()

Since the 2020 data skips many of these steps, we’ll write a separate
function for it:

    import2020 <- function(fns=list.files("./2020/", pattern=".csv")){
      
      fulldata <- NULL

      for(i in seq(along=fns)){   #go through each file one at a time
        
        pathname <- paste0("./2020/", fns[i])

        rawdata <- read_csv(pathname)
        
        #Remove rows where Parent_company is NULL or null; these are the plastic material type totals and are not always the first row
        rawdata <- filter(rawdata, Parent_company != "NULL", Parent_company != "null")
        
        #Add to full data set
        fulldata <- rbind(fulldata, rawdata)
        
      }
      
      fulldata

    }

    data_2020 <- import2020()

### Step 5 - Merge the two

Now all that’s left to do is to add a column for year, ensure the column
names match, and then use a simple `rbind` to create one single dataset
to explore if that’s what you’d like to do, or to use in our Shiny
dashboard we’re going to build next.

    #2019
    data_2019$Year <- 2019
    data_2019 <- select(data_2019, Year, everything())

    #2020
    data_2020$Year <- 2020
    data_2020 <- select(data_2020, Year, everything())

    #rename 2019 columns
    colnames(data_2019) <- colnames(data_2020)

    data <- rbind(data_2019, data_2020)

    #Write to file
    write_csv(data, "aggregated_data.csv")

Now you have your data all in one place! You can of course stop here and
work on any kind of further manipulation or visualization you’d like!
I’ve been working on my Shiny skills so I went with making a Shiny
dashboard. I’ve found that for large datasets like these, it’s helpful
to have an interactive way to explore the data, especially to explore
all the countries and brands. Next up, making a Shiny app!

Part 2 - Reproduce BFFP dashboard with Shiny
--------------------------------------------

To create a Shiny app in RStudio, go to File -&gt; New File -&gt; Shiny
Web App. Name your project and choose a directory in which it will live.
You can also choose whether you would like to make one file (app.R) or
two files (ui.R/server.R). I prefer the single file option but that’s
just out of habit and that fact that I’ve not been making particularly
complicated apps. For a primer on Shiny, check out their
[website](https://shiny.rstudio.com/) - I’ve been using it a lot while
creating this, as well as the [Shiny dashboards
page](http://rstudio.github.io/shinydashboard/?_ga=2.211946640.540917545.1610916899-2027945117.1603571057).

### Step 1 - Load the context

The first thing to do is to set up all the packages, data, files,
variables, etc. you’re going to need throughout your app. I’ve imported
the data we just cleaned in Part 1 and created a vector that contains
all the countries in the dataset to use in our drop-down menu. I also
set a plot theme that I like. In this case, there isn’t much data
manipulation needed, but maybe for other apps you have multiple datasets
or you have a particular subset you know you’re going to use a lot.
Those should be created here for the sake of efficiency: the code in
this section is only ran once.

    library(shinydashboard)
    library(tidyverse)

    #Import data
    data <- read_csv("aggregated_data.csv") %>% 
      replace_na(list(Grand_Total = 0, volunteers = 0)) #replaced any NAs present in the dataset with 0

    #Set plot theme
    theme_set(theme_classic())

    #Country category list for selectInput choices; add a blank option at the front of the list
    country_list <- c(".", unique(data$Country))
    names(country_list) <- c("-", unique(data$Country))

### Step 2 - Define the UI: sidebar

Then, we define our UI, which will contain a header, a sidebar and a
body. Since the header will be simple, I’ll leave it for later when we
put the three parts together. So, I’ll start with the sidebar. This will
be pretty simple for now since we only have one tab to start with. You
ned a title that can be seen on the app, a `tabName` that you’ll use to
refer to this tab in your code later, and you can add an icon. Since
it’s a trash audit, I went for a garbage can.

    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem("BFFP Dashboard", tabName = "BFFP", icon = icon("trash"))
      )
    )

### Step 3 - Define the UI: body

For our tab contents, we need four items: (1) a drop-down menu to select
the country, (2) a table showing the total number of plastics recorded
for each year, (3) a box with two tabs showing the top 3 polluters for
each year and (4) a plot illustrating the total number of plastics
recorded each year.

These are placed within a `tabItem`, which references the `tabName` we
gave our tab in the sidebar in Step 2. I then put in a short sentence
describing the tab in header level 3 font.

The first of our four items takes input from the user so we’re going to
use one of Shiny’s widgets, `selectInput`. It takes a tag that you’ll
use to refer to it in the `server` portion of the code later, a title
that the user can see, the list of options (in this case our list of
countries we made earlier) and a default selection.

The other three modify the data to produce an output. We’ll use
`tableOutput` and `plotOutput` to produce tables and a plot,
respectively. Both these functions take a tag that you’ll use to refer
to it later.

I’ve organized these into rows and columns using the `fluidRow` and
`column` functions, which help you build your layout. As you can guess
from their names, `fluidRow` creates a new row, and `column` creates a
new column within that row. I’ve also placed our first table and the
plot in a box so that they can have titles, just like the BFFP
dashboard. Finally, I’ve used `tabBox` with two `tabPanel`s to create
the table showcasing the top polluters per year.

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
                         tabBox(title = "Top Polluters per year", width = 12,
                                tabPanel("2020", tableOutput("BFFP_2019Polluters")),
                                tabPanel("2019", tableOutput("BFFP_2020Polluters"))
                         )
                         
                  )
                ),
                fluidRow(
                  box(title = "Recorded plastics", width = 12,
                      plotOutput("BFFP_plot")
                  )
                )
        )
      )
    )

### Step 4 - Build the UI

We can now put together our sidebar and body, and add a header to give
the app a title, to make our UI.

    ui <- dashboardPage(dashboardHeader(title = "Break Free From Plastic TidyTuesday by Sarah Sauve", titleWidth = 500),
                        sidebar,
                        body
    )

### Step 5 - Fill the server

Next, we fill the server function of the app, which will manipulate the
data and create outputs based on user input. This function is
essentially made up of a combination of `reactive` and `render`
functions.

To create our first table, we’re going to create a `reactive` function
to use the user input from `selectInput` in the UI to filter our dataset
by Country. We’re then going to group the data by year before
calculating the total number of plastics found in that country each
year. Then, we’ll use `renderTable` to output a table to the location we
want using the tag from the matching `tableOutput` in the UI, in this
case “BFFP\_total”.

    total_table_reactfunc <- reactive({
        data <- data %>%
          filter(Country == input$BFFP_country) %>%
          group_by(Year) %>%
          summarise(Total = sum(Grand_Total, na.rm = TRUE))
        
        data$Year <- as.integer(data$Year) #to ensure proper data type for plot
        data$Total <- as.integer(data$Total)
        
        return(data)
      })
      
      output$BFFP_total <- renderTable({
        data <- total_table_reactfunc()
        return(data)
      })

The next two tables are a little bit more involved. This is mostly
because I couldn’t figure out how to make one single function for both
years. So, I have the same pair of functions twice, once for 2019 and
once for 2020. The first of the pair is the `reactive` function, which
filters the data by country, then year. Then we arrange the dataset from
largest polluter to smallest based on total plastic pollution using
`arrange` and `desc`. I’ve decided to take out “Unbranded” or it would
likely be the top polluter everywhere. We take the top 3 using `head`
and then transform the numbers into a percentage of total plastic found
in that country using `mutate`. Finally, we assign the column names from
the BFFP dashboard.

The output is once again a simple `renderTable` with the matching tag.
And repeat for 2020.

     polluters2019_reactfunc <- reactive({
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
        data <- polluters2019_reactfunc()
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

Next, we’ve got to make our plot. This is made easier by the fact that
we’ve already processed the data to get the numbers we need to make our
first table. So, we simply insert the appropriate `reactive` function we
wrote earlier into `ggplot`! I chose a bar plot here instead of a line
since we only have two values.

      output$BFFP_plot <- renderPlot({
        ggplot(total_table_reactfunc(),
               aes(x = as.factor(Year), y = Total)) +
          geom_bar(stat = "identity", position = "dodge", width = .7) +
          xlab("Year")
      })

Finally, we put all of those chunks together into a server function,
like this:

    server <- function(input, output) {
      
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
      polluters2019_reactfunc <- reactive({
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
        data <- polluters2019_reactfunc()
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
    }

You can now run, or even
[publish](https://www.r-bloggers.com/2020/05/how-to-publish-a-shiny-app-example-with-shinyapps-io/)
your app!

Part 3 - Extend BFFP dashboard with Shiny
-----------------------------------------

So far the BFFP dashboard has been useful to practice my Shiny skills,
but it doesn’t provide quite enough detailed data for my taste. So, here
we’re going to add a new tab where we can investigate the data by brand
and plastic material type.

### Step 1 - Load the context

We’re going to need a couple more things in our global area for this new
section. The first is the `DT` package for really nicely formatted,
searchable tables, and the second is a list of brands to create a brand
drop-down menu like we did for countries in the BFFP dashboard. There
are many more brands than there are countries, so we’re going to start
making that more manageable by removing all brands with a `Grand_Total`
of plastics of 0.

    library(DT)

    #Brand list for selectInput choices; remove brands with a Grand Total of 0 items
    item_brands <- filter(data, Grand_Total != 0)
    brand_list <- c(".", unique(item_brands$Parent_company))
    names(brand_list) <- c("-", unique(item_brands$Parent_company))

### Step 2 - Define the UI: sidebar

Then, we move to our UI. We’re making a new tab, so we need to add a
menu item to our sidebar. The two-tab sidebar looks like this:

    sidebar <- dashboardSidebar(
      sidebarMenu(
        menuItem("BFFP Dashboard", tabName = "BFFP", icon = icon("trash")),
        menuItem("Brand Spotlight", tabName = "Brands", icon = icon("copyright"))
      )
    )

### Step 3 - Define the UI: body

We’re going to add contents to our new tab by adding to the body portion
of our app. This new contents goes under a new `tabItem`, which goes
within the `tabItems` function that we used to make the body (the new
tab is placed in context in the next code chunk, with the context
commented out).

On this tab, I’ve decided to have (1) a drop-down selection list of
brands, (2) an info-box that shows how many countries that brand was
found in, (3) a plot that shows the number of pieces of plastic found
for each type of plastic materials and (4) a table showing all the data
for that brand for browsing/more detail.

Since there are 9,000+ brands in this brand audit, I wanted the option
to search the list. Otherwise, scrolling would take much too long! It
turns out that Shiny has such a function; it’s called `selectizeInput`
and it takes the same arguments as `selectInput`.

The reactive info box is generated by the `infoBoxOutput` function, the
plot by `plotOutput` and the data table by `dataTableOutput`, with is
the `DT` version of a data table.

    #body <- dashboardBody(
    #  tabItems(
    #    tabItem(tabName = "BFFP", "All the previous tab contents"),
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
    #  )
    #)

### Step 4 - Add to the server

We’re going to add to the server function to provide contents for the
new objects we put in the UI. Each of these code chunks go in the server
function that we made earlier.

First, the info box, using `renderInfoBox` and assigning it to the
appropriate output. I start with an if/else statement so that when no
brand is selected, there isn’t an error or an NA in the info box. When a
brand is selected (the else portion), then we can count the number of
unique countries where that brand was found. We then insert that number
into an `infoBox` with the selected brand as the title and the number of
countries as subtitle, with a hashtag icon and coloured red.

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

Second, the plot. This plot requires some data manipulation to get to
the information and format we need to feed to `ggplot`. So, we’ll write
a `reactive` function to filter the data by brand, then sum the total
number of pieces of waste for each plastic type for each year using a
combination of `group_by` and `summarise`. Finally, we’ll need to pivot
the data so that the type of plastic becomes a variable. I name the
columns and ensure `Year` is a factor for ease of plotting. The output
of this reactive function is then used in `renderPlot` to create a bar
graph assigned to the corresponding output.

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

Last but not least, our data table. The data manipulation here is fairly
simple, I’ve just filtered by brand and removed the `num_events` and
`volunteers` columns. The `reactive` function also starts with an
if/else statement here so that when there’s no brand selected, the full
data set is displayed. This allows users to search the entire data set,
which could be useful to find inspiration for what kind of information
they’d like to see plotted. Instead of using Shiny’s `renderTable` here,
we’re going to use `renderDataTable` from the `DT` package. It makes
much nicer, scrollable and searchable tables, which is very useful for
very large tables like this one.

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

When it’s all put together, it looks like
[this](https://sarahsauve.shinyapps.io/TidyTuesdayBrandAuditDashboard/)!
You can find the code and data on GitHub
[here](https://github.com/sarahsauve/TidyTuesdays/tree/master/BFFPDashboard).
