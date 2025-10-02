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

    tabPanel("Overview",
             h1("Networks of Influence and Support: An Interactive Visualization"),
               br(),
               h4("About"),
               p("This project is an interactive R Shiny application designed to visually explore the networks
                 of influence and support in peace operations. The tool enables users—researchers, policymakers, 
                 or practitioners—to investigate key relationships and actors involved in international 
                 peacebuilding missions."),
             
              h4("Purpose"),
              p("Peace operations are complex systems involving multiple actors—governments, 
                NGOs, donors, military organizations, and local communities. This app aims to:"),
              tags$ul(
                tags$li("Visually map the relationships of influence (e.g., advisory roles, decision-making authority) 
                        and support (e.g., funding, logistics, coordination)."),
                tags$li("Allow users to filter, highlight, and customize the network to explore specific actors 
                        or types of connections."),
                tags$li("Provide insights into power dynamics, information flows, and collaborative structures 
                        within peace operations.")
              ),
               
               h4("Data"),
               p("The Networks of Influence and Support in Peace Operations project provides three 
                 complementary datasets that capture the presence and roles of peacebuilding actors in 
                 conflict-affected countries. The first, the Organization List Dataset, documents all 
                 peacebuilding actors present in a given country and includes key organizational characteristics 
                 such as type, mandate, and origin. The second, the Contractual Agreements Dataset, captures the 
                 involvement of actors in donor-funded projects, recording each organization’s role, the relationships 
                 formed through these projects, and project-level details such as funding, sector, and geographic focus. 
                 This dataset also provides variables that enable the study of both direct and indirect network relationships. 
                 The third, the Coordination Structures Dataset, tracks organizations’ participation in formal coordination 
                 mechanisms, such as donor coordination groups and the United Nations Cluster System, which bring together 
                 humanitarian, peacebuilding, and development actors. These datasets can be analyzed individually or 
                 combined, offering a detailed foundation for exploring how peacebuilding organizations interact, 
                 collaborate, and share resources in conflict-affected contexts."),

               h4("How to Use the App"),
               tags$ol(
                 tags$li("Use the navigation tabs at the top to switch to the data visulization tab."),
                 tags$li("The 'Network Visualization' tab allows you to explore nodes and edges interactively."),
                 tags$li("Use filters (e.g., by year, sector, number of contracts) to focus on specific subsets."),
                 tags$li("Hover or click on nodes to display additional details.")
               ),
             h4("Limitations"),
             p("The current version of this application is based on an incomplete version of the dataset.
               At this stage, the data are limited to the years 2016 and 2017, which means that the patterns 
               and relationships presented here should be interpreted with caution. Future updates will 
               incorporate additional years and more complete information, providing a fuller picture of 
               the networks of influence and support in peace operations."),
               
               h4("Disclaimer"),
               p("This visualization is intended for exploratory and illustrative purposes. 
      The dataset may not capture all relationships and should not be considered comprehensive."),
            
    tags$h4("Contact Us"),
    tags$p(
      "For questions, feedback, or collaboration inquiries related to the ",
      tags$em("Networks of Influence and Support in Peace Operations"),
      " project, please reach out to us at:"
    ),
    tags$p(
      tags$b("Email: "),
      tags$a(href = "mailto:Info@DGT-International.com", "Info@DGT-International.com")
    ),
    tags$p(
      tags$b("Website: "),
      tags$a(href = "https://www.DGT-International.com", "www.DGT-International.com", target = "_blank"),
      tags$br(),
      tags$a(href = "https://www.ripilab.com", "www.ripilab.com", target = "_blank")
    )
),
    
    
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
                                     ),
                                     
                                     downloadButton("download_full", "Download full dataset"),
                                     
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
                                     dataTableOutput("data_table")
                                     )
                            )
                            )
                 ) # closing data vis tab
               
                
      
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
  
  ## Download Data
  output$download_full <- downloadHandler(
    filename = function() {
      paste("full_dataset-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(master_data, file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)