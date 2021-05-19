library(shiny)
library(tidyverse)
library(here)

campaignDonations2 <- read.csv(here("data", "campaignDonations.csv")) %>%
  mutate(candidate_year = paste(candidate, year)) %>%
  pivot_longer(
    cols = c(under200, between200and500, between500and1000, between1000and2000, morethan2000, total),
    names_to = "type",
    values_to = "amount") %>%
  mutate(donation_type = case_when(type == "under200" ~ "Under $200",
                                   type == "between200and500" ~ "Between $200 and $500",
                                   type == "between500and1000" ~ "Between $500 and $1000",
                                   type == "between1000and2000" ~ "Between $1000 and $2000",
                                   type == "morethan2000" ~ "Over $2000",
                                   type == "total" ~ "Total")) %>%
  select(candidate_year, donation_type, amount)
  
campaignDonations2$donation_type <- factor(campaignDonations2$donation_type, 
                                           levels = c("Under $200", "Between $200 and $500",
                                                    "Between $500 and $1000", 
                                                    "Between $1000 and $2000", 
                                                    "Over $2000", "Total"))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  titlePanel("Individual Campaign Contributions (2000-2020)"),
  
  sidebarPanel(
    
    selectizeInput(inputId = "candidate"
                   , label = "Select candidates to compare:"
                   , choices = campaignDonations2$candidate_year
                   , selected = "Joe Biden 2020"
                   , multiple = TRUE),
    
    checkboxGroupInput(inputId = "donation_level"
                       , label = "Select donation levels:"
                       , choices = unique(campaignDonations2$donation_type)
                       , selected = c("Under $200","Total"))
  ),
  
  mainPanel(
    plotOutput("distPlot")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    campaignDonations2 %>%
      filter(candidate_year %in% input$candidate & donation_type %in% input$donation_level) %>%
      ggplot(aes(fill = candidate_year, x = donation_type, y = amount/1000000)) +
      geom_bar(position = "dodge", stat = "identity") +
      xlab("Donation Level") +
      ylab("Total Contributed (Millions of Dollars)") +
      labs(fill = "Candidate")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)