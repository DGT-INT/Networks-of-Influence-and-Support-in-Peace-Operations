# Shiny App for Custom Neural Networks // 

library(shiny)
library(visNetwork)
library(tidygraph)
library(dplyr)
library(tidyverse)
library(DT)
library(shinyjs)
library(glue)

# Importing Proccessed Data
{
  master_data <- readRDS("../data/Processed Data/test_data.rds")
}

# Assigning Input Vectors for the filter Selection (i might need to update this to make it interactive)
{
country <- c("Burundi", "Colombia")
dataframe <- c("United Nations Multi-Partner Trust Fund (MPTF) documents",
               "Organization for Economic Co-operation and Development Creditor Reporting System (OECD CRS)")
sender_org_type <- unique(master_data$sender_orgtype)
receiver_org_type <- unique(master_data$receiver_orgtype)
sector <- unique(master_data$sector)
relationships <- c("Number of Contracts", "Cost")
}

# Define UI for application
ui <- { navbarPage("Research on International Policy Implementation Lab",
                 id = "tabs",

    tabPanel("Home"),
    navbarMenu("Data",
                 tabPanel("About the data"),
                 tabPanel("Data Vizualization",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     
                                     h4("Filter Based on Data", style = "text-decoration: underline;"),
                                     checkboxInput("data_filters", "Customize network based on data?"),
                                     conditionalPanel(
                                       condition = "input.data_filters == true",
                                       selectInput("select_dataframe", "What dataframe are you interested in?", choices= dataframe, selected = "mptf" ),
                                       selectInput("select_country", "What country are you interested in?", choices= country),
                                       sliderInput("years", "What year are you interested in?", value = 2016, min = 2005, max = 2021, sep = "")
                                     ),
                                     
                                     h4("Filter Based on Nodes", style = "text-decoration: underline;"),
                                     checkboxInput("node_filters", "Customize network based on nodes?"),
                                     conditionalPanel(
                                       condition = "input.node_filters == true",
                                       selectInput("select_sender_org_type", "What type of sender organizations are you interested in?", choices= sender_org_type, multiple = TRUE, selected = sender_org_type),
                                       selectInput("select_receiver_org_type", "What type of receiver organizations are you interested in?", choices= receiver_org_type, multiple = TRUE, selected = receiver_org_type)
                                     ),
                                     
                                     h4("Filter Based on Relationships", style = "text-decoration: underline;"),
                                     checkboxInput("edge_filters", "Cutomize network based on edges?"),
                                     conditionalPanel(
                                       condition = "input.edge_filters == true",
                                       selectInput("select_relationship", "What type of relationship are you interested in?", choices= relationships),
                                       sliderInput("num_contracts", "Filter relationships based on number of contracts.", value = c(0,20), min = 0, max = 20),
                                       sliderInput("cost_contracts", "Filter relationships based on the cost of the contracts.", value = c(0,12000000), min = 0, max = 120000000),
                                       selectInput("select_sector", "What sectors are you interested in?", choices= sector, multiple = TRUE, selected = sector),
                                     )
                                     ),
                              column(8,
                                     titlePanel("Network Visualization of Peace Operations"),
                                     div(
                                       style = "border: 2px solid #444; border-radius: 10px;",
                                       visNetworkOutput("network_viz", height = "75vh", width = "100%")
                                     ))
                              ),
                            fluidRow(
                              column(12,
                                     downloadButton("download_full", "Download full dataset"),
                                     downloadButton("download_custom", "Download Custom dataset"),
                                     
                                     dataTableOutput("data_table")
                                     )
                            )
                            )
                 ), # closing data vis tab
               
                 tabPanel("Testing",
                          verbatimTextOutput("nodes_dataframe"),
                          verbatimTextOutput("edges_dataframe"))
      )
) }
    
# Define server logic required
server <- function(input, output) {
  
  ## Creating Reactive Data Frame for the display
  {
    display_data <- reactive({
      master_data
    })
  }
  
  ## Creating Nodes Data Frame for the Visual
  {
    nodes <- reactive({
      master_data %>%
        filter(sender_orgtype %in% input$select_sender_org_type,
               Year == input$years,
               data == input$select_dataframe,
               Country == input$select_country) %>%
        mutate(id = sender, group = sender_orgtype) %>%
        select(id, group, sector) %>%
        bind_rows(
          master_data %>%
            filter(receiver_orgtype %in% input$select_receiver_org_type,
                   Year == input$years,
                   data == input$select_dataframe,
                   Country == input$select_country) %>%
            mutate(id = receiver, group = receiver_orgtype) %>%
            select(id, group, sector)
        ) %>%
        distinct(id, .keep_all = TRUE) %>%
        mutate(label = id,
               title = glue("The organization is {id}. <br> They are a {group} type of organization."))
    })
  }
  
  output$nodes_dataframe <- renderPrint(head(nodes()))
  
  ## Creating Edges Data Frame for the Visual (weighted with number of contracts)
  edges_base <- reactive({ 
    master_data %>%
      mutate(from = sender, to = receiver) %>%
      select(from, to, sector, cost) %>%
      group_by(from, to) %>%
      summarise(
        value = n(),              # number of contracts
        value2 = sum(cost),       # total cost
        sector = first(sector),   # keep a sector label (if you need one)
        .groups = "drop"
      ) %>%
      filter(
        value >= input$num_contracts[1],
        value <= input$num_contracts[2],
        value2 >= input$cost_contracts[1],
        value2 <= input$cost_contracts[2],
        sector %in% input$select_sector
      ) %>%
      mutate(
        title = glue(
          "This relationship represents {value} contracts <br>
         from {from} <br>
         to {to} <br>
         within the {sector} sector. <br>
         Total cost: {scales::comma(value2)}"
        )
      ) %>%
      select(-sector)   # drop sector if you don’t need it anymore
  })
  
  ## selecting which relationship to display visually
  edges <- reactive({
    edges_base() %>%
      mutate(
        value = case_when(
          input$select_relationship == "Number of Contracts" ~ value,
          input$select_relationship == "Cost" ~ value2
          )
        )%>%
      select(-value2)
    })
  
  
  output$edges_dataframe <- renderPrint(head(edges()))
  
  
  ## Visual
  output$network_viz <- renderVisNetwork({
    visNetwork(nodes(), edges()) %>%
      visLayout(randomSeed = 123) %>%
      visEdges(arrows = "to",
               shadow = TRUE,
               color = list(color = "#038a81", highlight = "red")) %>%
      visOptions(highlightNearest = TRUE) %>%
      visLegend(position = "right", main = "Organization Type") %>%
      visInteraction(navigationButtons = TRUE)
  })
  
  
  ## Data Frame on display
  output$data_table <- renderDT({
    datatable(display_data(), options = list(pageLength = 10))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
