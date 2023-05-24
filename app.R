# Libraries

library(readxl)
library(purrr)
library(tidyverse)
library(shiny)
library(rsconnect)
library(shinythemes)
library(vroom)
library(shinyWidgets)
library(broom)
library(plotly)
library(shinythemes)

# Data read in 

df_list <- map(set_names(excel_sheets("NationalTotalAndSubcategory.xlsx")), # map returns a list containing each sheet of the excel 
               read_excel, path = "NationalTotalAndSubcategory.xlsx")

list2env(df_list, envir = .GlobalEnv) # convert from list to environment

# Background data wranglin' 

NatbySub<-`National by Subcategory` # rename df
NatbySub$Date<-as.Date(NatbySub$Date) # adjust variable to date
NatbySub$month <- floor_date(NatbySub$Date, "month") # monthly identifier

choices_month <- format(seq.Date(from = as.Date("2019-10-01"), by = "month", length.out = 39), "%Y-%m-%d")
tail(choices_month)



## Sum the variables by the categories
cat<-NatbySub %>%
  filter(Category != "All foods") %>%
  group_by(month, Category) %>%
  summarise(Dollars = sum(Dollars),
            `Unit sales` = sum(`Unit sales`),
            `Volume sales` = sum(`Volume sales`),
            `Dollars/EQ` = (Dollars/`Volume sales`)) %>%
  pivot_longer(cols = c("Dollars", 'Unit sales', 'Volume sales', 'Dollars/EQ'), names_to = "variable", values_to = "amt")

## Sum the variables by the subcategories

subcat<-NatbySub %>%
  filter(Category != 'All foods') %>%
  select(c("month", "Date", "Category", "Subcategory", "Dollars", 'Unit sales', 'Volume sales')) %>%
  group_by(month, Category, Subcategory) %>%
  summarise(Dollars = sum(Dollars),
            `Unit sales` = sum(`Unit sales`),
            `Volume sales` = sum(`Volume sales`),
            `Dollars/EQ` = (Dollars/`Volume sales`)) %>%
  pivot_longer(cols = c("Dollars", 'Unit sales', 'Volume sales', 'Dollars/EQ'), names_to = "variable", values_to = "amt")


# UI 
ui <- fluidPage(theme = shinytheme("superhero"),
  # Application title
  titlePanel("Monthly Retail Food Sales"),
  hr(),
  fluidRow(column(12, 
    h3("This app utilizes a dope dataset from the USDA regarding total dollars spent and units sold of consumer food products separated out into 11 main categories and further subdivided into subcategories. For more information see here: https://www.ers.usda.gov/data-products/weekly-retail-food-sales/. 
    
There was so much available data from the USDA and this only scratches the surface of what is available from them to look at.
       
The data is a time series starting 10/01/2019 agglomerated from weekly into monthly data until 12/01/2022. The variables reported for each category are as follows:"), offset = 0.2)),
  fluidRow(column(12, h4(" - Dollars: Total value of sales"), offset = 1)),
  fluidRow(column(12, h4(" - Unit sales: Total units sold, any size"), offset = 1)),
  fluidRow(column(12, h4(" - Volume sales: Total volume sold in equivalent units"), offset = 1)),
  fluidRow(column(12, h4(" - Average dollars/equivalent unit: Dollars/Volume sales. I.e. Dollars per                single equivalent unit"), offset = 1)),
  
  hr(),
  fluidRow(column(12, h2("Selecting date range and variable to view"), offset = 0.2)),
  fluidRow(column(6, offset = 0.1, sliderTextInput(
    inputId = "test", label = "Date Range", width = "100%",
    choices = choices_month, 
    selected = choices_month[c(1, 39)])),
    column(3, selectInput(inputId = "select1", label = "Select variable to view", 
                        choices = unique(cat$variable))),
    column(3, selectInput("y1", "Select Between Total and Proportion", c("Total", "Proportion")) 
  )),
  
  hr(),
  ## all categories 
  sidebarLayout( 
    sidebarPanel(
      checkboxGroupInput(
        inputId = "catpic",
        label = "Categories",
        choices = unique(cat$Category),
        selected = "Alcohol"
       # multiple = TRUE
       )
      ),
    # selectInput(inputId = "select1", label = "select data", 
            #      choices = unique(cat$variable)), 
     # selectInput("y1", "Y axis", c("Total", "Proportion")) 
   # ),
    mainPanel(
      tabsetPanel(
        tabPanel("Category",plotOutput("catplot")),
        tabPanel("Slope", plotOutput("slopeplot")),
        tabPanel ("Table", dataTableOutput("catinfo")))
    )),
  hr(),
 ## select a category to view subcategory information
 fluidRow(column(12, offset = 0.2, h3("Take a closer look at what makes up an individual category by selecting one of the categories you selected above."))),
 sidebarLayout( 
   sidebarPanel( 
     selectInput(inputId = "selectcat", label = "Select A Category", 
                 choices = unique(cat$Category))),
 #    selectInput(inputId = "subvar", label = "variable select", choices = unique(subcat$variable
 #)), 
   #  selectInput("y2", "Y axis", c("Total", "Proportion")) 
  # ),
   mainPanel(
     tabsetPanel(
       tabPanel("Subcategory",plotOutput("subplot")),
       tabPanel("SubSlope", plotOutput("subslopeplot")),
       tabPanel ("Table", dataTableOutput("subinfo")))
   ))
)



# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  observeEvent(input$catpic, {
    updateSelectInput(inputId = "selectcat", label = "Select a Category",  choices = c(input$catpic))
  })  
  
## create reactive to update data with the selections made in the ui

  selected <- reactive(cat  %>% filter(variable == input$select1) %>% filter( Category %in% input$catpic) %>% filter(month >= as.Date(input$test[1]) & month <= as.Date(input$test[2])))
  selectedsub <- reactive(subcat %>% filter(Category == input$selectcat & variable == input$select1) %>% filter(month >= as.Date(input$test[1]) & month <= as.Date(input$test[2])))

  
# Categories
  
  ## create proportion of total
  data_set <- reactive({
    selected() %>%
      group_by(month) %>%
      mutate(Pctd = amt/sum(amt))
  })

  ## plots
  output$catplot <- renderPlot({
    
    if (input$y1 == "Total") {
     data_set() %>%
        ggplot(aes( month , amt, color = Category)) +
        geom_line() +
        geom_point() +
        theme_bw()

    } else {
      data_set() %>%
        ggplot(aes(month , Pctd, fill = Category)) +
        geom_col(na.rm = TRUE) +
        theme_bw()
    }
  }, res=96)

 ## slope
  output$slopeplot <- renderPlot({
    data_set() %>%
      ggplot(aes(month, amt, color = Category, fill = Category)) +
      geom_smooth(method = "lm") +
      theme_bw()
  }, res=96)
  
  ## table
  
# lm<- reactive({
   # selected() %>%
    #  group_by(Category) %>%
    # mutate(lmi = lm(amt ~ month)) })
 
# coef_table <- as.data.frame(summary(lm)$coefficients)
  
 output$catinfo <- renderDataTable(selected(),options = list(pageLength = 15))


  # Selected Category and view subcategory
  ## set up proportion
  
sub_set <- reactive({
  selectedsub() %>%
    group_by(month, Category) %>%
    mutate(Pctd = amt/sum(amt))
})
 ## plot it
output$subplot <- renderPlot({
  
  if (input$y1 == "Total") {
    sub_set() %>%
      ggplot(aes(month, amt, color = Subcategory)) +
      geom_line() +
      geom_point()+
      theme_bw()
  } else {
    sub_set() %>%
      ggplot(aes(month, Pctd, fill = Subcategory)) +
      geom_col(na.rm = TRUE) +
      theme_bw()
  }
}, res=96)

## slope of change in subcategories

output$subslopeplot <- renderPlot({
  sub_set() %>%
    ggplot(aes(month, amt, color= Subcategory, fill = Subcategory)) +
    geom_smooth(method = "lm") +
    theme_bw()
}, res=96)

output$subinfo <- renderDataTable(sub_set(), options = list(pageLength = 15))


}

# Run the application 
shinyApp(ui = ui, server = server)
