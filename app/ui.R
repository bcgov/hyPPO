#tags$head(tags$link(rel = "stylesheet", href = "bcgov.css"))

vbs <- list(
  value_box(
    title = "Date of Last Field Visit",
    value = p(textOutput("fv"), style = "font-size: 70%"),
    showcase = bs_icon("clipboard2-data-fill"),
    theme = "primary",
    max_height = "100px"
  ),
  value_box(
    title = "Last Shift Applied",
    value = p(textOutput("shift"), style = "font-size: 70%"),
    showcase = bs_icon("droplet-fill"),
    theme = "primary",
    max_height = "100px"
  )
)

card2 <- navset_card_tab(
  height = "400px",
  full_screen = TRUE,
  title = "Telemetry Timeseries",
  nav_panel(title = "Discharge", plotlyOutput("discharge_plot")),
  nav_panel(title = "Stage", plotlyOutput("stage_plot"))
)


card3 <- card(
  min_height = 400,
  full_screen = TRUE,
  tableOutput("current_conditions_table")
)

#sstr(bcgov_footer)

ui <- page_navbar(
  header = tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "bcgov.css"
  ),

  title = tags$div(
    class = "bcgov-navbar-title",
    tags$img(
      src = "gov3_bc_logo.png",
      class = "bcgov-logo",
      alt = "Government of British Columbia"
    ),
    tags$div(
      class = "bcgov-title-text",
      tags$div("HyPPO Report", class = "bcgov-title"),
      tags$div(
        "Hydrometric Program Performance Optimizer",
        class = "bcgov-subtitle"
      )
    )
  ),

  nav_spacer(),

  nav_panel(
    "Overview Map",
    layout_column_wrap(
      width = 1 / 2,
      heights_equal = "row",
      card(tableOutput("highlighted_values")),
      card(leafletOutput("map"))
    )
  ),

  nav_panel(
    "Station Data",
    layout_sidebar(
      sidebar = sidebar(
        title = "Station Name",
        open = TRUE,
        selectizeInput(
          inputId = "LocationName",
          label = "Site Name",
          choices = c(
            "Clapperton Creek",
            "Chilako River",
            "Corkscrew Creek",
            "Five Mile",
            "French Creek",
            "Frosst Creek",
            "Gold Creek",
            "Goldstream River",
            "Guichon Creek",
            "Gwillim Creek",
            "Haslam Creek",
            "Illecillewaet River",
            "Little Campbell",
            "Lost Shoe",
            "Salmon River",
            "Rosewall Creek",
            "Windermere Creek"
          )
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        heights_equal = "row",
        !!!vbs,
        card2,
        card3
      )
    )
  ),

  # nav_panel(
  #   "Approval Status - OPTION 1",
  #   layout_column_wrap(
  #     width = 1 / 2, # width = 1 is clearer than 2/1 (same effect)
  #     heights_equal = "all",
  #     card(plotlyOutput("approval_plot")),
  #     card(plotlyOutput("approval_plot_discharge"))
  #   )
  # ),

  nav_panel(
    "Approval Status",
    layout_sidebar(
      sidebar = sidebar(
        title = "Data Type",
        open = TRUE,
        selectizeInput(
          inputId = 'Parameter',
          label = "Parameter",
          choices = c(
            "Discharge",
            "Stage"
          )
        )
      ),
      card(plotlyOutput("timeseries_approval_status"))
    )
  ),

  nav_spacer(),
  nav_spacer(),

  footer = tags$footer(
    class = "bcgov-footer",
    tags$div(
      class = "container-fluid",
      tags$div(
        "© Province of British Columbia",
        style = "font-weight: 200;"
      )
    )
  )
)
