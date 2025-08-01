# Shiny App for Custom Neural Networks // 

library(shiny)
library(visNetwork)
library(tidygraph)
library(dplyr)
library(tidyverse)
library(DT)
library(sigmajs)
library(shinyjs)

# Data Wrangling
## Note: I need to update the loop to automate on a larger scale when i get more data
{
Burundi_CRS <- readRDS("../data/bdi_result_crs.rds")
Colombia_CRS <- readRDS("../data/col_result_crs.rds")

Burundi_MPTF <- readRDS("../data/bdi_result_mptf_nogovsig.rds")
Colombia_MPTF <- readRDS("../data/col_result_mptf_nogovsig.rds")

# (Burundi) Extracting and Binding CRS data by Country
years <- names(Burundi_CRS[["edge_lists"]]) %>% 
  as.numeric() %>% 
  sort()

first_year <- min(years)
last_year  <- max(years)

for (i in first_year:last_year) {
  Burundi_CRS_i <- Burundi_CRS[["edge_lists"]][[as.character(i)]]
  
  Burundi_CRS_i <- Burundi_CRS_i %>%
    mutate(Country = "Burundi", Year = i)
  
  assign(paste0("Burundi_CRS_", i), Burundi_CRS_i)
}

Burundi_CRS_all_years <- bind_rows(
  mget(paste0("Burundi_CRS_", first_year:last_year))
)

rm(list = paste0("Burundi_CRS_", first_year:last_year), Burundi_CRS_i, Burundi_CRS)

# (Burundi) Extracting and Binding MPTF data by Country
years <- names(Burundi_MPTF[["edge_lists"]]) %>% 
  as.numeric() %>% 
  sort()

first_year <- min(years)
last_year  <- max(years)


for (i in first_year:last_year) {
  Burundi_MPTF_i <- Burundi_MPTF[["edge_lists"]][[as.character(i)]]
  
  Burundi_MPTF_i <- Burundi_MPTF_i %>%
    mutate(Country = "Burundi", Year = i)
  
  assign(paste0("Burundi_MPTF_", i), Burundi_MPTF_i)
}

Burundi_MPTF_all_years <- bind_rows(
  mget(paste0("Burundi_MPTF_", first_year:last_year))
)

rm(list = paste0("Burundi_MPTF_", first_year:last_year), Burundi_MPTF_i, Burundi_MPTF)

#-------

# (Colombia) Extracting and Binding CRS data by Country
years <- names(Colombia_CRS[["edge_lists"]]) %>% 
  as.numeric() %>% 
  sort()

first_year <- min(years)
last_year  <- max(years)

for (i in first_year:last_year) {
  Colombia_CRS_i <- Colombia_CRS[["edge_lists"]][[as.character(i)]]
  
  Colombia_CRS_i <- Colombia_CRS_i %>%
    mutate(Country = "Colombia", Year = i)
  
  assign(paste0("Colombia_CRS_", i), Colombia_CRS_i)
}

Colombia_CRS_all_years <- bind_rows(
  mget(paste0("Colombia_CRS_", first_year:last_year))
)

rm(list = paste0("Colombia_CRS_", first_year:last_year), Colombia_CRS_i, Colombia_CRS)

# (Colombia) Extracting and Binding MPTF data by Country
years <- names(Colombia_MPTF[["edge_lists"]]) %>% 
  as.numeric() %>% 
  sort()

first_year <- min(years)
last_year  <- max(years)


for (i in first_year:last_year) {
  Colombia_MPTF_i <- Colombia_MPTF[["edge_lists"]][[as.character(i)]]
  
  Colombia_MPTF_i <- Colombia_MPTF_i %>%
    mutate(Country = "Colombia", Year = i)
  
  assign(paste0("Colombia_MPTF_", i), Colombia_MPTF_i)
}

Colombia_MPTF_all_years <- bind_rows(
  mget(paste0("Colombia_MPTF_", first_year:last_year))
)

rm(list = paste0("Colombia_MPTF_", first_year:last_year), Colombia_MPTF_i, Colombia_MPTF)

}

## Network Specific Feature Engeneering
{
  Burundi_CRS_all_years <- Burundi_CRS_all_years %>%
    mutate(title_node = paste(sender, "is a ", sender_orgtype, "."),
           title_edges = paste("This relationship from", sender, "to", receiver,"is in the ",sector, "sector"))
  
}

# Inputs for the filter Selection
{
country <- c("Burundi", "Colombia")
dataframe <- c("United Nations Multi-Partner Trust Fund (MPTF) documents",
               "Organization for Economic Co-operation and Development Creditor Reporting System (OECD CRS)",
               "International Aid Transparency Initiative (IATI)")
sender_org_type <- unique(Burundi_CRS_all_years$sender_orgtype)
receiver_org_type <- unique(Burundi_CRS_all_years$receiver_orgtype)
sector <- unique(Burundi_CRS_all_years$sector)
relationships <- c("relationship 1", "relationship 2", "relationship 3")
}

# Define UI for application
ui <- { navbarPage("Research on International Policy Implementation Lab",
                 id = "tabs",

    tabPanel("Home"),
    navbarMenu("Data",
                 tabPanel("Data Vizualization",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     selectInput("select_dataframe", "What dataframe are you interested in?", choices= dataframe, selected = "Organization for Economic Co-operation and Development Creditor Reporting System (OECD CRS)" ),
                                     selectInput("select_country", "What country are you interested in?", choices= country),
                                     sliderInput("years", "What time year are you interested in?", value = 2011, min = 2005, max = 2021),
                                     selectInput("select_sender_org_type", "What type of sender organizations are you interested in?", choices= sender_org_type, multiple = TRUE, selected = sender_org_type),
                                     selectInput("select_receiver_org_type", "What type of receiver organizations are you interested in?", choices= receiver_org_type, multiple = TRUE, selected = receiver_org_type),
                                     selectInput("select_sector", "What sectors are you interested in?", choices= sector, multiple = TRUE),
                                     selectInput("select_relationship", "What type of relationship are you interested in?", choices= relationships)
                                     ),
                              column(8,
                                     titlePanel("Network Visualization of Peace Operations"),
                                     div(
                                       style = "border: 2px solid #444; border-radius: 10px;",
                                       sigmajsOutput("network_viz", height = "85vh", width = "100%")
                                     )
                                     
                                     )
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
                          verbatimTextOutput("edges_dataframe")),
                 tabPanel("To Be Determined", textInput("example text", "label3"))
      ),
      navbarMenu("Meet The Team")
) }
    
# Define server logic required
server <- function(input, output) {
  
  ## Creating Reactive Data Frame
  {
    network_viz_data <- reactive({
      Burundi_CRS_all_years %>%
        filter(Year == input$years,
               sender_orgtype %in% input$select_sender_org_type,
               receiver_orgtype %in% input$select_receiver_org_type)
    })
  }
  
  ## Creating Nodes Data Frame
  {
    nodes <- reactive({ network_viz_data() %>%
      mutate(id = sender, group = sender_orgtype, title = title_node) %>%
      select(id, group, title) %>%
      bind_rows(
        network_viz_data() %>%
          mutate(id = receiver, group = receiver_orgtype, title = title_node) %>%
          select(id, group, title)
      ) %>%
      distinct(id, .keep_all = TRUE) %>%
      mutate(label = id, size = 2, color = "#000000")
    })
    
  }
  
  ## Creating Edges Data Frame
  {
    edges <- reactive({
      network_viz_data() %>%
        mutate(id = row_number(),source = sender, target = receiver, type = "arrow",
               size = 100000000000000000000000^1000000 *100000000000000000000000^1000000 * 100000000000000000000000^1000000 *100000000000000000000000^1000000 * 100000000000000000000000^1000000 *100000000000000000000000^1000000 * cost) %>%
        select(id, source, target, type, size)
    
    ### this will be dependent on the user selection (pick one option)
    
    #### option 1
#    edges$size <- Burundi_CRS_all_years$cost
    
    #### option 2
#    n_contract_sum <- network_viz_data() %>%
#      group_by(sender, receiver, title_edges) %>%
#      summarise(value = n_distinct(id), .groups = "drop") %>%
#      mutate(from = sender, to = receiver, title = title_edges) %>%
#      select(from, to, value, title)
    
#    left_join(base_edges, n_contract_sum, by = c("from", "to", "title")) %>%
#      distinct(from, to, value, .keep_all = TRUE)
    })
  }
  
  ## Data Frame on display
  output$data_table <- renderDT({
    datatable(network_viz_data(), options = list(pageLength = 20))
  })
  
  ## Network Visualization
#  output$network_visualization <- renderVisNetwork({
#    visNetwork(nodes(), edges(), main = paste(input$select_country,"'s",input$select_dataframe," data from",input$years[1]," to",input$years[2])) %>%
#      visLayout(randomSeed = 123) %>%
#      visEdges(arrows = "to", shadow = TRUE) %>%
#      visLegend() %>%
#      visOptions(highlightNearest = TRUE)
#  })
  
  ## sigmajs
  output$network_viz <- renderSigmajs({
    sigmajs() %>%
      sg_nodes(nodes(), id = "id" ,label = "id", size = "size", color= "color") %>%
      sg_edges(edges(), id = "id", source= "source", target= "target", type = "type", size = "size") %>%
      sg_layout()%>%
      sg_settings(labelThreshold = 13) %>%
      sg_drag_nodes() %>%
      sg_neighbors()
    }) 
      
    

  
  
  #testing panel
  
  output$nodes_dataframe <- renderPrint(head(nodes()))
  output$edges_dataframe <- renderPrint(head(edges()))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
