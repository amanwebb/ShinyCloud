

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

server <- function(input, output, session) {
  ############ web scrape function ###############
  scraper <- eventReactive(eventExpr = input$scrape,
                           valueExpr = {
                             # assign selected year to variable
                             year <- input$Year
                             # assign selected sport to variable
                             sport <- input$Sport
                             # assign selected position to variable
                             Position <- input$Position
                             
                             # start the web scrape nba if statement
                             if (input$Sport == 'NBA') {
                               # paste the url
                               url <-
                                 paste0(
                                   'https://www.basketball-reference.com/leagues/',
                                   sport,
                                   '_',
                                   year,
                                   '_per_game.html'
                                 )
                               
                               # scrape the url
                               webpage <- read_html(url)
                               hp_table <-
                                 html_nodes(webpage, "table")
                               nba_table <- html_table(hp_table)
                               # convert to dataframe
                               df <- as.data.frame(nba_table)
                               # Website "ranks" the players in column one so we delete it
                               df <- df[,-1]
                             } # End nba if
                             
                             # Start nfl if statement
                             else if (input$Sport == 'nfl') {
                               # Start passing if statement
                               if (input$Position == 'Passing') {
                                 url <-
                                   paste0(
                                     'https://www.nfl.com/stats/player-stats/category/',
                                     Position,
                                     '/',
                                     year,
                                     '/reg/all/passingyards/desc'
                                   )
                                 
                                 # scrape the url
                                 webpage <- read_html(url)
                                 hp_table <-
                                   html_nodes(webpage, "table")
                                 nfl_table <- html_table(hp_table)
                                 # convert to dataframe
                                 df <- as.data.frame(nfl_table)
                               }
                               
                               # Start rushing if statement
                               else if (input$Position == 'Rushing') {
                                 url <-
                                   paste0(
                                     'https://www.nfl.com/stats/player-stats/category/',
                                     Position,
                                     '/',
                                     year,
                                     '/reg/all/rushingyards/desc'
                                   )
                                 
                                 # scrape the url
                                 webpage <- read_html(url)
                                 hp_table <-
                                   html_nodes(webpage, "table")
                                 nfl_table <- html_table(hp_table)
                                 # convert to dataframe
                                 df <- as.data.frame(nfl_table)
                               }
                               
                               # Start receiving if statement
                               else if (input$Position == 'Receiving') {
                                 url <-
                                   paste0(
                                     'https://www.nfl.com/stats/player-stats/category/',
                                     Position,
                                     '/',
                                     year,
                                     '/reg/all/receivingreceptions/desc'
                                   )
                                 
                                 # scrape the url
                                 webpage <- read_html(url)
                                 hp_table <-
                                   html_nodes(webpage, "table")
                                 nfl_table <- html_table(hp_table)
                                 # convert to dataframe
                                 df <- as.data.frame(nfl_table)
                               }
                               
                               # Start field goals if statement
                               else if (input$Position == 'field-goals') {
                                 url <-
                                   paste0('https://www.pro-football-reference.com/years/',
                                          year,
                                          '/kicking.htm')
                                 
                                 # scrape the url
                                 webpage <- read_html(url)
                                 hp_table <-
                                   html_nodes(webpage, "tbody")
                                 nfl_table <- html_table(hp_table)
                                 # convert to dataframe
                                 df <- as.data.frame(nfl_table)
                                 # create the names of the columns
                                 colnames(df) <-
                                   c(
                                     'Rk',
                                     'Player',
                                     'Team ',
                                     'Age',
                                     'Position',
                                     'G',
                                     'GS',
                                     '0-19 FGA',
                                     '0-19 FGM',
                                     '20-29 FGA',
                                     '20-29 FGM',
                                     '30-39 FGA',
                                     '30-39 FGM',
                                     '40-49 FGA',
                                     '40-49 FGM',
                                     '50+ FGA',
                                     '50+ FGM',
                                     'Total FGA',
                                     'Total FGM',
                                     'Long',
                                     'FG%',
                                     'XPA',
                                     'XPM',
                                     'XP%',
                                     'KO',
                                     'KOYDS',
                                     'TB',
                                     'TB%',
                                     'KOAvg',
                                     'Punts',
                                     'PuntsYds',
                                     'PuntLng',
                                     'PuntBlk',
                                     'Y/P'
                                   )
                                 # Make the numeric columns numeric
                                 df[, c(4, 6:34)] <-
                                   sapply(df[, c(4, 6:34)], as.numeric)
                                 # assign all nan to 0
                                 df[is.na(df)] <- 0
                                 # Delete rank team and position columns
                                 df[-c(1, 3, 5)]
                               }
                               
                               # Start interceptions if statement
                               else if (input$Position == 'interceptions') {
                                 url <-
                                   paste0(
                                     'https://www.nfl.com/stats/player-stats/category/',
                                     Position,
                                     '/',
                                     year,
                                     '/reg/all/defensiveinterceptions/desc'
                                   )
                                 
                                 # scrape the url
                                 webpage <- read_html(url)
                                 hp_table <-
                                   html_nodes(webpage, "table")
                                 nfl_table <- html_table(hp_table)
                                 # convert to dataframe
                                 df <- as.data.frame(nfl_table)
                               }
                               
                               # Start tackles if statement
                               else if (input$Position == 'tackles') {
                                 url <-
                                   paste0(
                                     'https://www.nfl.com/stats/player-stats/category/',
                                     Position,
                                     '/',
                                     year,
                                     '/reg/all/defensivecombinetackles/desc'
                                   )
                                 
                                 # scrape the url
                                 webpage <- read_html(url)
                                 hp_table <-
                                   html_nodes(webpage, "table")
                                 nfl_table <- html_table(hp_table)
                                 # convert to dataframe
                                 df <- as.data.frame(nfl_table)
                               }
                               
                             } # End nfl if statement
                           })
  #### end web scrape function ####
  
  
  #### DataFrame on tab One ####
  output$data_table <- renderTable({
    # When checkbox is NOT checked, do head of data
    if (input$All == FALSE) {
      head(scraper(), 10)
    }
    # Show all data
    else {
      scraper()
    }
  })
  #### end dataframe ####
  
  
  
  #### Dropdown boxes to select players on tab two ####
  observe(
    updateSelectInput(
      session,
      inputId = 'Player_One',
      label = 'Player One',
      choices = scraper()$Player,
      selected = scraper()$Player[1]
    )
  )
  observe(
    updateSelectInput(
      session,
      inputId = 'Player_Two',
      label = 'Player Two',
      choices = scraper()$Player,
      selected = scraper()$Player[2]
    )
  )
  #### End of Dropdown boxes to select players on tab two ####
  
  
  
  
  #### Plot on second tab ####
  output$comparePlot <- renderPlot({
    #### if nba was selected only show select variables in checkboxes ####
    if (input$Sport == 'NBA') {
      if (input$Plots == 'Histogram') {
        output$checkbox <- renderUI({
          choice <-  names(scraper())
          checkboxGroupInput(
            "checkbox",
            "Select Stats",
            choices = choice[-c(1, 2, 4)],
            selected = input$checkbox
          )
        })
        
        df <- scraper() %>% pivot_longer(cols = Pos:PTS) %>%
          mutate(value = as.numeric(value)) %>%
          filter(!is.na(value)) %>%
          filter(Player %in% c(input$Player_One, input$Player_Two))
        
        df <- df[df$name %in% input$checkbox, ]
        
        
        ggplot(df, aes(
          x = Player,
          fill = Player,
          y = value
        )) +
          geom_bar(stat = "identity", position = "dodge") +
          facet_wrap( ~ name, "free_y", shrink = TRUE) +
          theme(axis.text.x = element_blank()) +
          scale_fill_manual(values = c("#9933FF",
                                       "#33FFFF"))
      }
      else if (input$Plots == 'Lollipop') {
        output$checkbox <- renderUI({
          choice <-  names(scraper())
          checkboxGroupInput(
            "checkbox",
            "Select Stats",
            choices = choice[-c(1, 2, 4)],
            selected = input$checkbox
          )
        })
        df <-
          scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
          filter(Player %in% c(input$Player_One, input$Player_Two))
        
        df <- df[df$Stats %in% input$checkbox,]
        
        df$paired <-
          factor(df$Stats, levels = unique(df$Stats))
        df[order(df$Stats), ]
        
        df$Value <- as.numeric(as.character(df$Value))
        
        df %>%
          ggplot(aes(x = Stats, y = Value)) +
          geom_line(aes(group = paired)) +
          geom_point(aes(color = Player), size = 5) +
          theme(legend.position = "top") +
          coord_flip() +
          scale_color_brewer(palette = "Paired")
        
      }
    }
    #### if nfl was selected only show select variables in checkboxes ####
    else if (input$Sport == 'nfl') {
      output$checkbox <- renderUI({
        choice <-  names(scraper())
        checkboxGroupInput(
          "checkbox",
          "Select Stats",
          choices = choice[-c(1)],
          selected = input$checkbox
        )
      })
      if (input$Plots == 'Histogram') {
        # if passing is selected then update checkboxes on second tab
        if (input$Position == 'Passing') {
          df <- scraper() %>% pivot_longer(cols = Pass.Yds:SckY) %>%
            mutate(value = as.numeric(value)) %>%
            filter(!is.na(value)) %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$name %in% input$checkbox,]
          
          ggplot(df, aes(
            x = Player,
            fill = Player,
            y = value
          )) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~ name, "free_y", shrink = TRUE) +
            theme(axis.text.x = element_blank()) +
            scale_fill_manual(values = c("#9933FF",
                                         "#33FFFF"))
        }
        # if Rushing is selected then update checkboxes on second tab
        else if (input$Position == 'Rushing') {
          df <- scraper() %>% pivot_longer(cols = Rush.Yds:Rush.FUM) %>%
            mutate(value = as.numeric(value)) %>%
            filter(!is.na(value)) %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$name %in% input$checkbox,]
          
          ggplot(df, aes(
            x = Player,
            fill = Player,
            y = value
          )) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~ name, "free_y", shrink = TRUE) +
            theme(axis.text.x = element_blank()) +
            scale_fill_manual(values = c("#9933FF",
                                         "#33FFFF"))
        }
        # if Receiving is selected then update checkboxes on second tab
        else if (input$Position == 'Receiving') {
          df <- scraper() %>% pivot_longer(cols = Rec:Tgts) %>%
            mutate(value = as.numeric(value)) %>%
            filter(!is.na(value)) %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$name %in% input$checkbox,]
          
          ggplot(df, aes(
            x = Player,
            fill = Player,
            y = value
          )) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~ name, "free_y", shrink = TRUE) +
            theme(axis.text.x = element_blank()) +
            scale_fill_manual(values = c("#9933FF",
                                         "#33FFFF"))
        }
        # if field goals is selected then update checkboxes on second tab
        else if (input$Position == 'field-goals') {
          df <- scraper() %>% pivot_longer(cols = Age:PuntBlk) %>%
            mutate(value = as.numeric(value)) %>%
            filter(!is.na(value)) %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$name %in% input$checkbox,]
          
          ggplot(df, aes(
            x = Player,
            fill = Player,
            y = value
          )) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~ name, "free_y", shrink = TRUE) +
            theme(axis.text.x = element_blank()) +
            scale_fill_manual(values = c("#9933FF",
                                         "#33FFFF"))
        }
        # if interceptions is selected then update checkboxes on second tab
        else if (input$Position == 'interceptions') {
          df <- scraper() %>% pivot_longer(cols = INT:Lng) %>%
            mutate(value = as.numeric(value)) %>%
            filter(!is.na(value)) %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$name %in% input$checkbox,]
          
          ggplot(df, aes(
            x = Player,
            fill = Player,
            y = value
          )) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap(~ name, "free_y", shrink = TRUE) +
            theme(axis.text.x = element_blank()) +
            scale_fill_manual(values = c("#9933FF",
                                         "#33FFFF"))
        }
        # if interceptions is selected then update checkboxes on second tab
        else if (input$Position == 'tackles') {
          df <- scraper() %>% pivot_longer(cols = Comb:Sck) %>%
            mutate(value = as.numeric(value)) %>%
            filter(!is.na(value)) %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$name %in% input$checkbox, ]
          
          ggplot(df, aes(
            x = Player,
            fill = Player,
            y = value
          )) +
            geom_bar(stat = "identity", position = "dodge") +
            facet_wrap( ~ name, "free_y", shrink = TRUE) +
            theme(axis.text.x = element_blank()) +
            scale_fill_manual(values = c("#9933FF",
                                         "#33FFFF"))
        }
      }
      else if (input$Plots == 'Lollipop') {
        if (input$Position == 'Passing') {
          df <-
            scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$Stats %in% input$checkbox, ]
          
          df$paired <-
            factor(df$Stats, levels = unique(df$Stats))
          df[order(df$Stats),]
          
          df$Value <- as.numeric(as.character(df$Value))
          
          df %>%
            ggplot(aes(x = Stats, y = Value)) +
            geom_line(aes(group = paired)) +
            geom_point(aes(color = Player), size = 4) +
            theme(legend.position = "top") +
            coord_flip()
        }
        else if (input$Position == 'Rushing') {
          df <-
            scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$Stats %in% input$checkbox, ]
          
          df$paired <-
            factor(df$Stats, levels = unique(df$Stats))
          df[order(df$Stats),]
          
          df$Value <- as.numeric(as.character(df$Value))
          
          df %>%
            ggplot(aes(x = Stats, y = Value)) +
            geom_line(aes(group = paired)) +
            geom_point(aes(color = Player), size = 4) +
            theme(legend.position = "top") +
            coord_flip()
        }
        else if (input$Position == 'Receiving') {
          df <-
            scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$Stats %in% input$checkbox, ]
          
          df$paired <-
            factor(df$Stats, levels = unique(df$Stats))
          df[order(df$Stats),]
          
          df$Value <- as.numeric(as.character(df$Value))
          
          df %>%
            ggplot(aes(x = Stats, y = Value)) +
            geom_line(aes(group = paired)) +
            geom_point(aes(color = Player), size = 4) +
            theme(legend.position = "top") +
            coord_flip()
        }
        else if (input$Position == 'field-goals') {
          df <-
            scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$Stats %in% input$checkbox, ]
          
          df$paired <-
            factor(df$Stats, levels = unique(df$Stats))
          df[order(df$Stats),]
          
          df$Value <- as.numeric(as.character(df$Value))
          
          df %>%
            ggplot(aes(x = Stats, y = Value)) +
            geom_line(aes(group = paired)) +
            geom_point(aes(color = Player), size = 4) +
            theme(legend.position = "top") +
            coord_flip()
        }
        else if (input$Position == 'interceptions') {
          df <-
            scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$Stats %in% input$checkbox, ]
          
          df$paired <-
            factor(df$Stats, levels = unique(df$Stats))
          df[order(df$Stats),]
          
          df$Value <- as.numeric(as.character(df$Value))
          
          df %>%
            ggplot(aes(x = Stats, y = Value)) +
            geom_line(aes(group = paired)) +
            geom_point(aes(color = Player), size = 4) +
            theme(legend.position = "top") +
            coord_flip()
        }
        else if (input$Position == 'tackles') {
          df <-
            scraper() %>% pivot_longer(!Player, names_to = 'Stats', values_to = 'Value') %>%
            filter(Player %in% c(input$Player_One, input$Player_Two))
          
          df <- df[df$Stats %in% input$checkbox, ]
          
          df$paired <-
            factor(df$Stats, levels = unique(df$Stats))
          df[order(df$Stats),]
          
          df$Value <- as.numeric(as.character(df$Value))
          
          df %>%
            ggplot(aes(x = Stats, y = Value)) +
            geom_line(aes(group = paired)) +
            geom_point(aes(color = Player), size = 4) +
            theme(legend.position = "top") +
            coord_flip()
        }
      }
    }
    
    
    
  }) # end filtered plot
  
  
  
  
}



shinyApp(ui, server, options = list(height = 1300))



