library(shiny)
library(shinydashboard)
library(dplyr)
library(shinythemes)
library(stringr)

load("combined.Rdata")
subtitle_text <- readRDS("subtitle.RDS")
flag_choices <- readRDS("flags.RDS")
sv <- read.csv("lists-Reefers-2022-11-11_04-40.csv")

## NATO
nato_countries <- c("GER", "USA", "FRA")
flag_list <- function(selection) {
  if(selection == "U.S.") return(c("USA"))
  if(selection == "NATO") return(nato_countries)
  if(selection == "Five Eyes") return(c("USA", "GBR", "NZA", "AUS", "CAN"))
  else return(flag_choices)
}


header <- dashboardHeader(
  title = "Reefer Tracking Portal"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",

    # Page Tabs
    menuItem("Home", icon = icon("home"), tabName = "home"),
    menuItem("Ranking", icon = icon("table"), tabName = "ranking"),
    menuItem("Plot", icon = icon("bar-chart"), tabName = "plot"),
    menuItem("Parameters", icon = icon("cog"), tabName = "parameters"),

    # filter vessels
    sliderInput(
      "year_range",
      "Years",
      min = 2012,
      max = 2022,
      value = c(2012, 2022),
      sep = "",
      round = TRUE,
      ticks = FALSE
    ),
    sliderInput(
      "top_n",
      "Minimum number of meetings by vessel",
      min = 0,
      max = 500,
      value = 100,
      # sep = "",
      round = FALSE,
      animate = TRUE
    ),
    sliderInput(
      "distance",
      "Distance from shore (nm)",
      min = 0,
      max = 1400,
      value = c(200, 250),
      # sep = "",
      round = FALSE,
      animate = TRUE
    )
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      "home",
      h3("Introduction"),
      fluidRow(
        column(
          width = 6,
          align = "left",
          p(subtitle_text)
        )
      )
    ),
    tabItem(
      "ranking",
      h3("Vulnerability to enforcement"),
      radioButtons(
        "enforcer",
        "in jurisdiction of",
        choices = c("U.S.", "Five Eyes", "NATO", "any country"),
        selected = "U.S.",
        inline = TRUE
      ),
      br(),
      fluidRow(
        valueBoxOutput("flag", width = 3),
        valueBoxOutput("eez", width = 3),
        valueBoxOutput("rfmo", width = 3),
        valueBoxOutput("port", width = 3)
      ),
      fluidRow(
        column(
          width = 3,
          align = "center",
          actionButton("show_flag", label = "show list")
        ),
        column(
          width = 3,
          align = "center",
          actionButton("show_eez", label = "show list")
        ),
        column(
          width = 3,
          align = "center",
          actionButton("show_rfmo", label = "show list")
        ),
        column(
          width = 3,
          align = "center",
          actionButton("show_port", label = "show list")
        )
      ),
      hr(),
      textOutput("table_heading"),
      fluidRow(
        column(
          width = 12,
          align = "center",
          DT::dataTableOutput(outputId = "rankingstable")
        )
      )
    ),
    tabItem(
      "plot"
    ),
    tabItem(
      "parameters"
    )
  )
)

ui <- dashboardPage(header, sidebar, body, title = "Illegal Fishing")



server <- function(input, output) {
  generate_rankings2 <- reactive({
    all_meetings <- rbind(encounter, loitering) %>%

    # filter by date when the meeting occured
    mutate(start = as.Date(start)) %>%
    filter(between(
        start,
        as.Date(paste0(input$year_range[1], "-01-01")),
        as.Date(paste0(input$year_range[2], "-12-31"))
    )) %>%

    # filter by the distance from shore where the meeting occured
    filter(between(
        distance_from_shore_m,
        input$distance[1] * 1852,
        input$distance[2] * 1852
    ))

  # filter by the flags of the vessels
  if ("flags" %in% names(input) & !is.null(input$flags)) {
    all_meetings <- all_meetings %>%
      filter(vessel.flag %in% input$flags)
  }


  # generate table columns: name of vessel and flag of vessel
  reefer_info <- all_meetings %>%
    count(vessel.mmsi, vessel.name, vessel.flag, sort = TRUE) %>%
    group_by(vessel.mmsi) %>%
    summarise(Reefer.Name = vessel.name[1], Reefer.Flag = vessel.flag[1])

  # generate table columns: number of tracked and dark meetings,
  # and median distance from shore
  meeting_info <- all_meetings %>%
    mutate(encounter = ifelse(type == "encounter", 1, 0),
      loitering = ifelse(type == "loitering", 1, 0)) %>%
    group_by(vessel.mmsi) %>%
    summarise(
      n_encounter = sum(encounter),
      n_loitering = sum(loitering),
      avg_distance = median(distance_from_shore_m) / 1852,
      authorized = sum(encounter.authorization_status == "authorized",
        na.rm = TRUE)
    ) %>%
    mutate(
      total_meetings = n_encounter + n_loitering,
      tracked = n_encounter / total_meetings,
      authorized = authorized / total_meetings
    )


  table_data <- reefer_info %>%

    # joining data
    left_join(meeting_info, by = "vessel.mmsi") %>%
    left_join(sv, by = c("vessel.mmsi" = "MMSI")) %>%

    # sorting the rows
    arrange(-total_meetings) %>%
    mutate(Reefer.Name = str_to_title(Reefer.Name)) %>%

    # filter by minimum number of meetings
    filter(total_meetings >= input$top_n) %>%
    select(-Status) %>%
    select(-Flag) %>%

    # renaming the rows
    rename(
      "MMSI" = vessel.mmsi,
      "Vessel Name" = Reefer.Name,
      "Flag" = Reefer.Flag,
      "Number of tracked meetings" = n_encounter,
      "Number of dark meetings" = n_loitering,
      "Median distance from shore (nm)" = avg_distance,
      "Meetings" = total_meetings,
      "Status" = Navigation.Status
    )

  table_data
  })


  rv <- reactiveValues(table_shown = "none")

  flag_data <- reactive({
    generate_rankings2() %>%
      select(
        "Flag",
        "Vessel Name",
        "Longitude",
        "Latitude",
        "Meetings",
        "tracked",
        "authorized",
        "Status"
      ) %>%
      filter(Flag %in% flag_list(input$enforcer))
  })

  output$flag <- renderValueBox({
    num <- flag_data() %>%
      nrow()
    if (rv$table_shown == "flag") {
      valueBox(
        subtitle = paste0("flagged to ", input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = paste0("flagged to ", input$enforcer),
        value = num,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  observeEvent(
    eventExpr = input$show_flag,
    handlerExpr = {
      output$rankingstable <- DT::renderDataTable(
        DT::datatable(
          # data = generate_rankings(input, session, encounter, loitering)(),
          data = flag_data(),
          options = list(pageLength = 10),
          rownames = FALSE)
      )
      rv$table_shown <- "flag"
    }
  )

  output$eez <- renderValueBox({
    eez_subtitle <- function(enforcer) {
      if (enforcer == "any country") {
        return(paste0("in any EEZ"))
      } else {
          return(paste0("in ", input$enforcer, " EEZ"))
      }
    }
    if (rv$table_shown == "eez") {
      valueBox(
        subtitle = eez_subtitle(input$enforcer),
        value = 1,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = eez_subtitle(input$enforcer),
        value = 1,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  observeEvent(
    eventExpr = input$show_eez,
    handlerExpr = {
      output$rankingstable <- DT::renderDataTable(
        DT::datatable(
          # data = generate_rankings(input, session, encounter, loitering)(),
          data = generate_rankings2(),
          options = list(pageLength = 10),
          rownames = FALSE)
      )
      rv$table_shown <- "eez"
    }
  )

  output$rfmo <- renderValueBox({
    rfmo_subtitle <- function(enforcer) {
      if (enforcer == "any country") {
        return(paste0("in any RFMO"))
      } else {
        if (enforcer == "U.S.") {
          return(paste0("in RFMOs enforceable by the U.S."))
        } else {
          return(paste0("in RFMOs enforceable by ", input$enforcer))
        }
      }
    }
    if (rv$table_shown == "rfmo") {
      valueBox(
        subtitle = rfmo_subtitle(input$enforcer),
        value = 1,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = rfmo_subtitle(input$enforcer),
        value = 1,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  observeEvent(
    eventExpr = input$show_rfmo,
    handlerExpr = {
      output$rankingstable <- DT::renderDataTable(
        DT::datatable(
          # data = generate_rankings(input, session, encounter, loitering)(),
          data = generate_rankings2(),
          options = list(pageLength = 10),
          rownames = FALSE)
      )
      rv$table_shown <- "rfmo"
    }
  )

  port_data <- reactive({
    generate_rankings2() %>%
      select(
        "Flag",
        "Vessel Name",
        "Longitude",
        "Latitude",
        "Meetings",
        "tracked",
        "authorized",
        "Status"
      ) %>%
      filter(Status == "5-Moored")
  })

  output$port <- renderValueBox({
    port_subtitle <- function(enforcer) {
      if (enforcer == "any country") {
        return(paste0("at any port"))
      } else return(paste0("at ", input$enforcer, " ports"))
    }
    if (rv$table_shown == "port") {
      valueBox(
        subtitle = port_subtitle(input$enforcer),
        value = 1,
        icon = icon("flag"),
        color = "yellow")
    } else {
      valueBox(
        subtitle = port_subtitle(input$enforcer),
        value = 1,
        icon = icon("flag"),
        color = "light-blue")
    }
  })

  observeEvent(
    eventExpr = input$show_port,
    handlerExpr = {
      output$table_heading <- renderText(
        "Vessels currently in port"
      )
      output$rankingstable <- DT::renderDataTable(
        DT::datatable(
          # data = generate_rankings(input, session, encounter, loitering)(),
          data = port_data(),
          options = list(pageLength = 10),
          rownames = FALSE)
      )
      rv$table_shown <- "port"
    }
  )
}

# Run the application ----------------------------------------------
shinyApp(ui = ui, server = server)