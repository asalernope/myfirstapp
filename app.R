# Libraries

library(readxl)
library(purrr)
library(tidyverse)
library(shiny)
library(rsconnect)
library(shinythemes)
library(vroom)
library(shinyWidgets)

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
ui <- fluidPage(
  # Application title
  titlePanel("I am hungry"),
  fluidRow(
    column(12, 
        "Dollars: Total value of sales;
          Unit sales: Total units sold, any size;
          Volume sales: Total volume sold in equivalent units;
          Average dollars/equivalent unit: Dollars/Volume sales. I.e. Dollars per                single equivalent unit")),
  
  ## all categories 
  
  sidebarLayout( 
    sidebarPanel(
      sliderTextInput(
        inputId = "test", label = "Month", width = "100%",
        choices = choices_month, 
        selected = choices_month[c(1, 36)]),
      selectInput(
        inputId = "catpic",
        label = "Categories",
        choices = unique(cat$Category),
        selected = "Alcohol",
        multiple = TRUE),
     selectInput(inputId = "select1", label = "select data", 
                  choices = unique(cat$variable)), 
      selectInput("y1", "Y axis", c("Total", "Proportion")) 
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Category",plotOutput("catplot")),
        tabPanel("Slope", plotOutput("slopeplot")))
    )
  ),
 
 ## select a category to view subcategory information
 
 fluidRow(
   column(12, "select a category to view subcategory information")
 ),
 sidebarLayout( 
   sidebarPanel( 
     selectInput(inputId = "selectcat", label = "select category", 
                 choices = unique(cat$Category)),
     selectInput(inputId = "subvar", label = "variable select", choices = unique(subcat$variable)), 
     selectInput("y2", "Y axis", c("Total", "Proportion")) 
   ),
   mainPanel(
     tabsetPanel(
       tabPanel("Subcategory",plotOutput("subplot")),
       tabPanel("SubSlope", plotOutput("subslopeplot")))
   )))



# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
## create reactive to update data with the selections made in the ui
  
    ## input$numeric_var is a character vector, so we cast it to a list of symbols
  # var_list <- observeEvent(input$catpic)
  
  
  
  selected <- reactive(cat  %>% filter(variable == input$select1) %>% filter( Category %in% input$catpic) %>% filter(month >= as.Date(input$test[1]) & month <= as.Date(input$test[2])))
  selectedsub <- reactive(subcat %>% filter(Category == input$selectcat & variable == input$subvar)
                          %>% filter(month >= as.Date(input$test[1]) & month <= as.Date(input$test[2])))

  
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
        ggplot(aes( month , amt, fill = Category)) +
        geom_col() +
    
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

  # Selected Category and view subcategory
  ## set up proportion
  
sub_set <- reactive({
  selectedsub() %>%
    group_by(month, Category) %>%
    mutate(Pctd = amt/sum(amt))
})
 ## plot it
output$subplot <- renderPlot({
  
  if (input$y2 == "Total") {
    sub_set() %>%
      ggplot(aes(month, amt, fill = Subcategory)) +
      geom_col() +
      
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
}

# Run the application 
shinyApp(ui = ui, server = server)
