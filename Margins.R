library(shiny)
library(tidyverse)
library(ggrepel)
library(shinythemes)
library(datasets)
library(mdsr)
library(gapminder)
library(maps)
library(dplyr)
library(ggplot2)

# Grab donation data from all elections

# data wrangling
contributions_2020 <- read_csv("Contributions_2020.csv") %>%
  mutate("Year" = 2020) %>%
  select(Year, State, Democrat = "BIDEN, JOSEPH R JR", Republican = "TRUMP, DONALD J.") %>%
  arrange(State)
contributions_2016 <- read_csv("Contributions_2016.csv") %>%
  mutate("Year" = 2016) %>%
  select(Year, State, Democrat = "CLINTON, HILLARY RODHAM / TIMOTHY MICHAEL KAINE", Republican = "TRUMP, DONALD J. / MICHAEL R. PENCE") %>%
  arrange(State)
contributions_2012 <- read_csv("Contributions_2012.csv") %>%
  mutate("Year" = 2012) %>%
  select(Year, State, Democrat = "OBAMA, BARACK", Republican = "ROMNEY, MITT / RYAN, PAUL D.") %>%
  arrange(State)
contributions_2008 <- read_csv("Contributions_2008.csv") %>%
  mutate("Year" = 2008) %>%
  select(Year, State, Democrat = "OBAMA, BARACK", Republican = "MCCAIN, JOHN S.") %>%
  arrange(State)
contributions_2004 <- read_csv("Contributions_2004.csv") %>%
  mutate("Year" = 2004) %>%
  select(Year, State, Democrat = "KERRY, JOHN F", Republican = "BUSH, GEORGE W") %>%
  arrange(State)
contributions_2000 <- read_csv("Contributions_2000.csv") %>%
  mutate("Year" = 2000) %>%
  select(Year, State, Democrat = "GORE, AL", Republican = "BUSH, GEORGE W") %>%
  arrange(State)
contributions_1996 <- read_csv("Contributions_1996.csv") %>%
  mutate("Year" = 1996) %>%
  select(Year, State, Democrat = "CLINTON, WILLIAM JEFFERSON", Republican = "DOLE, ROBERT J") %>%
  arrange(State)
contributions_1992 <- read_csv("Contributions_1992.csv") %>%
  mutate("Year" = 1992) %>%
  select(Year, State, Democrat = "CLINTON, WILLIAM JEFFERSON", Republican = "BUSH, GEORGE") %>%
  arrange(State)
contributions_1988 <- read_csv("Contributions_1988.csv") %>%
  mutate("Year" = 1988) %>%
  select(Year, State, Democrat = "DUKAKIS, MICHAEL S", Republican = "BUSH, GEORGE") %>%
  arrange(State)
contributions_1984 <- read_csv("Contributions_1984.csv") %>%
  mutate("Year" = 1984) %>%
  select(Year, State, Democrat = "MONDALE, WALTER F", Republican = "REAGAN, RONALD") %>%
  arrange(State)
contributions_1980 <- read_csv("Contributions_1980.csv") %>%
  mutate("Year" = 1980) %>%
  select(Year, State, Democrat = "CARTER, JIMMY", Republican = "REAGAN, RONALD") %>%
  arrange(State)

#Combine each election, omit non states

individual_contributions <- contributions_2020 %>%
  full_join(contributions_2016) %>%
  full_join(contributions_2012) %>%
  full_join(contributions_2008) %>%
  full_join(contributions_2004) %>%
  full_join(contributions_2000) %>%
  full_join(contributions_1996) %>%
  full_join(contributions_1992) %>%
  full_join(contributions_1988) %>%
  full_join(contributions_1984) %>%
  full_join(contributions_1980) %>%
  filter(State != "ZZ") %>%
  filter(State != "AA") %>%
  filter(State != "AE")  %>%
  filter(State != "VI")  %>%
  filter(State != "AP")  %>%
  filter(State != "AS")  %>%
  filter(State != "GU")  %>%
  filter(State != "MP")  %>%
  filter(State != "OT")  %>%
  filter(State != "DC") %>%
  filter(State != "PR")


# omit 1976 and pivot longer so democrat and republican are in the same column
individual_contributions2 <- individual_contributions %>%
  filter(Year != 1976) %>%
  pivot_longer(!State & !Year, names_to = "Party", values_to = "Contribution") %>%
  arrange(State)


state_info <- data.frame(state_full = tolower(state.name) , State = state.abb,
                         Region = state.region)

usa_states <- map_data(map = "state"
                       , region = ".")

#combine contribution data with map to create map of states and contributions
contributions_map <- individual_contributions2 %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("state_full" = "region"))



#Remove dollar signs, allow math to be done to adjust for inflation
#Compute donation share of each party
individual_contributions3 <-individual_contributions2 %>%
  pivot_wider(names_from = Party, values_from = Contribution) %>%
  transform(Democrat = as.numeric(gsub("[$),]", "", Democrat))) %>%
  transform(Republican = as.numeric(gsub("[$),]", "", Republican))) %>%
  mutate(Democrat = case_when(Year == "2020" ~ Democrat,
                              Year == "2016" ~ Democrat*1.0772,
                              Year == "2012" ~ Democrat*1.1257,
                              Year == "2008" ~ Democrat*1.2023,
                              Year == "2004" ~ Democrat*1.3640,
                              Year == "2000" ~ Democrat*1.4965,
                              Year == "1996" ~ Democrat*1.6449,
                              Year == "1992" ~ Democrat*1.8363,
                              Year == "1988" ~ Democrat*2.1663,
                              Year == "1984" ~ Democrat*2.4728,
                              Year == "1980" ~ Democrat*3.0706,)) %>%
  mutate(Republican = case_when(Year == "2020" ~ Republican,
                                Year == "2016" ~ Republican*1.0772,
                                Year == "2012" ~ Republican*1.1257,
                                Year == "2008" ~ Republican*1.2023,
                                Year == "2004" ~ Republican*1.3640,
                                Year == "2000" ~ Republican*1.4965,
                                Year == "1996" ~ Republican*1.6449,
                                Year == "1992" ~ Republican*1.8363,
                                Year == "1988" ~ Republican*2.1663,
                                Year == "1984" ~ Republican*2.4728,
                                Year == "1980" ~ Republican*3.0706,)) %>%
  mutate(R_donshare = Republican / (Democrat+Republican) * 100) %>%
  mutate(D_donshare = Democrat / (Democrat+Republican) * 100) %>%
  mutate(don_margin_pct = R_donshare - D_donshare) %>%
  select(Year, State, don_margin_pct)

# Load 1976-2020 Presidential Election data
presidential_elections <- read_csv("1976-2020-president.csv")
presidential_elections
# Temporary data set containing total votes in each state in each election year.
temp1 <- presidential_elections %>%
  select(state_po, year, totalvotes) %>%
  group_by(state_po, year) %>%
  summarize(totalvotes = sum(totalvotes)/n())

# Temporary data set containing number of votes the D and R presidential candidates received in
# each state during each election.
presidential_elections
temp2 <- presidential_elections %>%
  filter(party_simplified == "DEMOCRAT" | party_simplified == "REPUBLICAN") %>%
  filter(candidatevotes > 1000) %>%
  filter(state_po != "DC") %>%
  pivot_wider(id_cols = c(state_po, year), names_from = party_simplified, 
              values_from = candidatevotes) %>%
  rename(R_votes = REPUBLICAN, D_votes = DEMOCRAT)

#Merge the two temporary data sets, omit 1976
presidential_elections <- inner_join(temp2, temp1) %>%
  mutate(R_voteshare = R_votes / totalvotes * 100) %>%
  mutate(D_voteshare = D_votes / totalvotes * 100) %>%
  mutate(margin = R_votes - D_votes) %>%
  mutate(margin_pct = R_voteshare - D_voteshare) %>%
  filter(year != 1976) %>%
  select(State = state_po, margin_pct, Year = year)

#Generate the map dataset that shows the margin of victory by state
electoral_map <- presidential_elections %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("state_full" = "region"))

#Generate the map dataset that shows the donation margins by state and has election data
margin_donation <- inner_join(presidential_elections, individual_contributions3, by = c("State", "Year"))
margin_donation_map <- margin_donation %>%
  left_join(state_info, by = "State") %>%
  right_join(usa_states, by = c("state_full" = "region"))

###################################################
# define choice values and labels for user inputs #
###################################################

# for selectizeInput choice for State, pull directly from data
year_choice <- unique(electoral_map$Year)
state_choice <-unique(presidential_elections$State)


############
#    ui    #
############
ui <- navbarPage(
  
  theme = shinytheme("cerulean"),
  
  title="Campaign Contributions and Electoral Success",
  
  #For the two maps by electoral margin and donation margin
  
  tabPanel(
    title = "Maps",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "years"
                       , label = "Choose Year:"
                       , choices = year_choice
                       , selected = "2020"
                       , multiple = FALSE)
      ),
      mainPanel(
        plotOutput(outputId = "map1"),
        plotOutput(outputId = "map2")
      )
    )
  ),
  
  #For the scatterplot comparing electoral margin and donation margin by state
  tabPanel(
    title = "Scatter",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "years2"
                       , label = "Choose Year:"
                       , choices = year_choice
                       , selected = "2020"
                       , multiple = FALSE)
      ),
      mainPanel(
        plotOutput(outputId = "scatter")
      )
    )
  ),
  #For the scatterplot showing changes in margins in each state over time
  tabPanel(
    title = "Individual States",
    
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "states"
                       , label = "Choose State:"
                       , choices = state_choice
                       , selected = "AL"
                       , multiple = TRUE)
      ),
      mainPanel(
        plotOutput(outputId = "scatter2"),
        plotOutput(outputId = "scatter3")
      )
    )
  )
)

############
# server   #
############
server <- function(input,output){
  
  # INTERACTIVE MAP
  
  #Generate the primary margin of victory map
  output$map1 <- renderPlot({
    
    electoral_map <- electoral_map %>%
      filter(Year %in% input$years)
    # subset data
    # plot
    ggplot(mapping = aes(x = long, y = lat, group = group), color = "white") +
      scale_fill_distiller(
        palette = "RdBu",
        limits = c(-55,55)) +
      geom_polygon(data = electoral_map, aes(fill = margin_pct)) +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Republican Vote Margin (%)") +
      theme(legend.position="bottom"
            , legend.title = element_text(size = 18)
            , legend.text = element_text(size = 12)
            , strip.text = element_text(size = 18)
            , plot.title = element_text(size=22))
  })
  #Generate the donation margin map
  output$map2 <- renderPlot({
    margin_donation_map <- margin_donation_map %>%
      filter(Year %in% input$years)
    
    ggplot(mapping = aes(x = long, y = lat, group = group), color = "white") +
      scale_fill_distiller(
        palette = "RdBu",
        limits = c(-100,100)) +
      geom_polygon(data = margin_donation_map, aes(fill = don_margin_pct)) +
      theme_void() +
      coord_fixed(ratio = 1.3) +
      labs(title = "Republican Donation Margin (%)") +
      theme(legend.position="bottom"
            , legend.title = element_text("hi", size = 18)
            , legend.text = element_text(size = 12)
            , strip.text = element_text(size = 18)
            , plot.title = element_text(size=22))
  })
  #Generate the donation margin vs. MOV scatterplot
  output$scatter <- renderPlot({
    margin_donation <- margin_donation %>%
      filter(Year %in% input$years2)
    ggplot(data = margin_donation, aes(x = margin_pct, y = don_margin_pct)) +
      xlab("Republican Vote Margin (%)") +
      ylab("Republican Donation Margin (%)") +
      xlim(-55,55) +
      ylim(-100,100) +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      geom_point() +
      geom_label_repel(aes(label = State), show.legend = FALSE) +
      geom_smooth(method = "lm", formula = y~x)
  })
  #Generate the MOV vs. Time for each state plot
  output$scatter2 <- renderPlot({
    presidential_elections <- presidential_elections %>%
      filter(State %in% input$states)
    ggplot(data = presidential_elections, aes(x = Year, y = margin_pct, color = State)) +
      xlab("Year") +
      ylab("Republican Electoral Margin (%)") +
      xlim(1976,2020) +
      ylim(-55,55) +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      geom_point(aes(color = State)) +
      geom_label_repel(aes(label = State, color = State), show.legend = TRUE) +
      geom_smooth(method = "loess", se = FALSE)
  })
  #Generate the donation margin vs. Time for each state plot
  output$scatter3 <- renderPlot({
    margin_donation <- margin_donation %>%
      filter(State %in% input$states)
    ggplot(data = margin_donation, aes(x = Year, y = don_margin_pct, color = State)) +
      xlab("Year") +
      ylab("Republican Donation Margin (%)") +
      xlim(1976,2020) +
      ylim(-100,100) +
      theme(axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14)) +
      geom_point(aes(color = State)) +
      geom_label_repel(aes(label = State, color = State), show.legend = FALSE) +
      geom_smooth(method = "loess", se = FALSE)
  })
}



####################
# call to shinyApp #
####################
shinyApp(ui = ui, server = server)