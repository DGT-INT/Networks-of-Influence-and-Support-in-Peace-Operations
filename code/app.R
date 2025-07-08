# Shiny App for Custom Neural Networks

library(shiny)
library(visNetwork)
library(tidygraph)
library(dplyr)
library(tidyverse)
library(DT)

# Data Wrangling
# Note: I need to update the loop to automate on a larger scale when i get more data
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







burundi_crs_2005 <- burundi_crs[["edge_lists"]][["2005"]]
burundi_crs_2005 <- burundi_crs_2005 %>%
  mutate(country = "Burundi")

country <- c("Burundi", "Colombia")
dataframe <- c("United Nations Multi-Partner Trust Fund (MPTF) documents",
               "Organization for Economic Co-operation and Development Creditor Reporting System (OECD CRS)",
               "International Aid Transparency Initiative (IATI)")
org_type <- c("orgtype 1", "orgtype 2", "orgtype 3")
sector <- c("sector 1", "sector 2", "sector 3")
relationships <- c("relationship 1", "relationship 2", "relationship 3")



# Data Wrangling for Visualization
{
  
  # Create a node list of all unique orgs from sender and receiver
  nodes <- burundi_crs_2005 %>%
    select(id, name = sender, orgtype = sender_orgtype) %>%
    bind_rows(
      burundi_crs_2005 %>%
        select(id, name = receiver, orgtype = receiver_orgtype)
    ) %>%
    distinct(name, .keep_all = TRUE) %>%
    mutate(label = name, id = name)  # visNetwork needs 'id' and 'label'
  
  # Creating edge data
  edges <- burundi_crs_2005 %>%
    mutate(from = sender, to = receiver) %>%
    select(from, to, cost, n_contracts)

}


# Define UI for application that draws a histogram
ui <- { navbarPage("Research Implementation Policy Institute",
                 id = "tabs",

    tabPanel("Home"),
    navbarMenu("Data",
                 tabPanel("Data Vizualization",
                          fluidPage(
                            fluidRow(
                              column(4,
                                     selectInput("select_dataframe", "What dataframe are you interested in?", choices= dataframe),
                                     selectInput("select_country", "What country are you interested in?", choices= country),
                                     sliderInput("years", "What time period are you interested in?", value= c(2012,2014), min = 2005, max = 2021),
                                     selectInput("select_sender_org_type", "What type of sender organizations are you interested in?", choices= org_type),
                                     selectInput("select_receiver_org_type", "What type of receiver organizations are you interested in?", choices= org_type),
                                     selectInput("select_sector", "What sectors are you interested in?", choices= sector),
                                     selectInput("select_relationship", "What type of relationship are you interested in?", choices= relationships)
                                     ),
                              column(8,
                                     visNetworkOutput("network_visualization")
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
               
                 tabPanel("To Be Determined", textInput("example text", "label2")),
                 tabPanel("Also To Be Determined", textInput("example text", "label3"))
      ),
      navbarMenu("Meet The Team")
) }
    

# Define server logic required to draw a histogram
server <- function(input, output) {

# Data Frame
  output$data_table <- renderDT({
    datatable(burundi_crs_2005, options = list(pageLength = 20))
  })
  
# Network Visualization
  output$network_visualization <- renderVisNetwork({
    visNetwork(nodes, edges) %>%
      visNodes(shape = "dot", scaling = list(min = 10, max = 30)) %>%
      visEdges(smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visGroups(groupname = "Government", color = "lightblue") %>%
      visGroups(groupname = "NGO", color = "lightgreen") %>%
      visGroups(groupname = "Private", color = "orange") %>%
      visLegend() %>%
      visLayout(randomSeed = 123)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
