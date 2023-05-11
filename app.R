library(readxl)
library(purrr)
library(tidyverse)
library(shiny)
setwd("~/Desktop/app")


df_list <- map(set_names(excel_sheets("NationalTotalAndSubcategory.xlsx")), # map returns a list containing each sheet of the excel 
               read_excel, path = "NationalTotalAndSubcategory.xlsx")

list2env(df_list, envir = .GlobalEnv) # convert from list to environment

var_names<- setNames(`National by Subcategory`$Category, `National by Subcategory`$Subcategory)


# UI 
ui <- fluidPage(
  
  # Application title
  titlePanel("Food Spending 2019 to 2023"),
  fluidRow(
    column(8,align="center",
           selectInput("Category", "Subcategory",
                       choices = setNames(`National by Subcategory`$Category, `National by Subcategory`$Subcategory), #have the choices be combined with the set of names
                       width = "100%"
           )
    )
  ),
  
  fluidRow( #create a table for the overall category and then allow for the selection of the subcategory within the category to be shown in the table
    column(6, tableOutput("cate")),
    column(6, tableOutput("bysub"))
  ),
  fluidRow(
    column(12, plotOutput("cat_time"))
  ))
# Define server logic required to draw a histogram

server <- function(input, output, session) {
  selected <- reactive(injuries %>% filter(var_names == input$Category))
  
  
  output$cate<- renderTable(
    selected() 
  )
  output$bysub <- renderTable(
    selected() )
  
  output$age_sex <- renderPlot({
    selected() %>%
      ggplot(aes(Date, Dollars, colour = Subcategory)) +
      geom_line() +
      labs(y = "attempt")
  }, res = 96)
  
}


# Run the application 
shinyApp(ui = ui, server = server)
