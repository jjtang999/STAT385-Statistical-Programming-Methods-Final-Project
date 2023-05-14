library(shiny)
library(tidyverse)

soccer = read_csv(file = "data/soccer.csv")

ref_call_order = c(
  "HomeFouls",
  "HomeYellowCards",
  "HomeRedCards",
  "AwayFouls",
  "AwayYellowCards",
  "AwayRedCards"
)

ui = navbarPage(
  title = "Referee Calls",
  tabPanel(
    title = "Input / Visualization",
    titlePanel(title = "Premier League Match Data: 2021 - 2022"),
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "ref",
                    label = "Referee:",
                    choices = sort(unique(soccer$Referee)),
                    ),
        selectInput(inputId = "hteam",
                    label = "Home Team:",
                    choices = sort(unique(soccer$HomeTeam)),
                    ),
        selectInput(inputId = "ateam",
                    label = "Away Team:",
                    choices = sort(unique(soccer$AwayTeam)),
                    ),
        checkboxInput(inputId = "game", 
                      label = "Filter Table to Game",
                      value = FALSE)
      ),
      mainPanel(plotOutput("plot"))
    )
  ),
  tabPanel(title = "Table", 
           dataTableOutput("table")),
  tabPanel(title = "About", 
           includeMarkdown("about.Rmd"))
  )

server = function(input, output) {
    soccer_ref = reactive({
      soccer %>% 
        filter(Referee == input$ref)
    })
    
    observeEvent(
      eventExpr = input$ref,
      handlerExpr =  {
        updateSelectInput(inputId = "hteam",
                          choices = sort(unique(soccer_ref()$HomeTeam)),
                          selected = min(unique(soccer_ref()$HomeTeam)))
        updateSelectInput(inputId = "ateam",
                          choices = sort(unique(soccer_hteam()$AwayTeam)),
                          selected = sort(unique(soccer_hteam()$AwayTeam))[1])
      }
    )
    
    soccer_hteam = reactive({
      soccer_ref() %>% 
        filter(HomeTeam == input$hteam)
    })
    
    observeEvent(
      eventExpr = input$hteam,
      handlerExpr =  {
        updateSelectInput(inputId = "ateam",
                          choices = sort(unique(soccer_hteam()$AwayTeam)),
                          selected = sort(unique(soccer_hteam()$AwayTeam))[1])
      }
    )
    
    output$plot = renderPlot({
      soccer %>% 
        filter(Referee == input$ref) %>% 
        filter(HomeTeam == input$hteam) %>%
        filter(AwayTeam == input$ateam) %>% 
        pivot_longer(HomeFouls:AwayRedCards, names_to = "Referee_Call", values_to = "Count") %>%
        group_by("Referee Call") %>% 
        mutate(Referee_Call = factor(Referee_Call, levels = ref_call_order)) %>%
        ggplot() +
        aes(x = Referee_Call, y = Count, fill = Count) %>% 
        geom_bar(stat = "identity") +
        scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
        scale_y_continuous(n.breaks = 10) +
        theme_bw()
    })
    
    output$table = renderDataTable({
      tab = soccer_ref()
      
      if (input$game) {
        tab = tab %>% 
          filter(HomeTeam == input$hteam) %>% 
          filter(AwayTeam == input$ateam)
      }
      
      tab
    })
}

shinyApp(ui = ui, server = server)
