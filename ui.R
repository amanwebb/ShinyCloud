

library(shiny)
library(tidyverse)
library(XML) # xml_Parse
library(dplyr)
library(tidyr)
library(stringi)
library(rvest) # html_table, html_node
library(ggplot2)
library(RCurl) # getURL
library(shinythemes)


ui <- fluidPage(
  # Theme doesnt look that good... consider making css file
  theme = shinytheme('cerulean'),
  # Title
  titlePanel("Sports App",
             windowTitle = 'Sports App'),
  tabsetPanel(
    ############ TAB ONE ###############
    tabPanel(
      "Scrape Data",
      # First Tab Title
      titlePanel("Scrape Data"),
      sidebarLayout(
        sidebarPanel(
          # select the sport to scrape
          selectInput(
            "Sport",
            label = h4("Select Sport"),
            choices = list("NBA" = 'NBA',
                           "NFL" = 'nfl'),
            selected = 'NBA'
          ),
          # user inputs four digit year that is wanted to scrape
          textInput(
            "Year",
            label = h4("What Season Do You Want?"),
            value = "YYYY"
          ),
          # scrape data button
          actionButton('scrape', 'Scrape the Web'),
          tags$br(),
          # Default is to show only 10 obs. checkbox to show all data
          checkboxInput("All", label = "All Data", value = FALSE),
          
          # Conditional dropdown box if nfl is clicked
          conditionalPanel(
            condition = "input.Sport == 'nfl'",
            selectInput(
              'Position',
              label = h4('Stat Type'),
              choices = list(
                'Interceptions' = 'interceptions',
                'Field Goals' = 'field-goals',
                'Passing' = 'Passing',
                'Rushing' = 'Rushing',
                'Receiving' = 'Receiving',
                'Tackles' = 'tackles'
              ),
              selected = 'Passing'
            )
          )
          
        ),
        # End of sidebar panel
        
        mainPanel(# Output table
          tableOutput('data_table')) # End of main panel
      )
    ),
    ############ TAB TWO ###############
    # Second Tab Title
    tabPanel("Compare Players",
             pageWithSidebar(
               headerPanel('Compare Players'),
               sidebarPanel(
                 # Pick what two players to compare
                 selectInput('Player_One', 'Player One', ''),
                 selectInput('Player_Two', 'Player Two', ''),
                 # Select what plot to view
                 selectInput(
                   "Plots",
                   label = h4("Select Plot"),
                   choices = list(
                     "Histogram" = 'Histogram',
                     "Lollipop Plot" = 'Lollipop',
                     "Spider Plot" = 'Spider'
                   ),
                   selected = 'Lollipop'
                 ),
                 # If histogram is picked and no stat then Show a message that a red line warning will appear if no stat is picked on histogram
                 conditionalPanel(condition = "input.Plots == 'Histogram'",
                                  h4('Error will go away when at least one stat is picked.')),
                 # show the available stats to compare when histogram or lollipop is picked
                 conditionalPanel(condition = "input.Plots == 'Histogram' | input.Plots == 'Lollipop'",
                                  uiOutput("checkbox")),
               ),
               
               # Start main Panel
               mainPanel(
                 # conditional Title of plot
                 conditionalPanel(condition = "input.Plots == 'Histogram'",
                                  titlePanel('Histogram')),
                 conditionalPanel(condition = "input.Plots == 'Lollipop Plot'",
                                  titlePanel('Lollipop')),
                 conditionalPanel(condition = "input.Plots == 'Spider Plot'",
                                  titlePanel('Spider')),
                 
                 # Show the plot
                 plotOutput('comparePlot')
               )
             ))
  ))



