# Copyright 2026 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

server <- function(input, output, session) {
  site_name_rv <- reactive({
    input$LocationName
  })

  timeseries_type <- reactive({
    input$Parameter
  })

  timeseries_data <- reactive({
    ts_approval |>
      filter(Parameter == timeseries_type())
  })

  filtered_data <- reactive({
    current_reading |>
      filter(LocationName == site_name_rv()) |>
      select(c(
        "LocationName",
        "DateTime",
        "NumericValue1",
        "Parameter",
        "Label",
        "Unit",
        "week_difference",
        "diff_from_monthly_ave"
      ))
  })

  fv_data <- reactive({
    most_recent |>
      filter(LocationName == site_name_rv()) |>
      select(c(
        "LocationName",
        "Date",
        "Current_Date",
        "Most_Recent"
      ))
  })

  shift_data <- reactive({
    shift_last_applied |>
      filter(LocationName == site_name_rv()) |>
      select(c(
        "Value",
        "Timestamp"
      ))
  })

  telemetry_data <- reactive({
    ts_telemetry |>
      filter(LocationName == site_name_rv()) |>
      filter(Parameter == "Discharge" | Parameter == "Stage")
  })

  station_click = reactiveVal()

  observeEvent(input$map_marker_click, {
    station_click(input$leafmap_marker_click$LocationName)
    # if(period_rv()=="Yearly"){
    shiny::updateNavbarPage(
      inputId = 'Station Data',
      selected = 'Site Name'
    )
    # }
  })

  output$current_conditions_table <-
    render_gt({
      if (nrow(filtered_data() > 0)) {
        current_status <- filtered_data() |>
          ungroup() |>
          select(c(
            Parameter,
            NumericValue1,
            Unit,
            week_difference,
            diff_from_monthly_ave
          )) |>
          gt() |>
          cols_label(
            Parameter = "Parameter",
            NumericValue1 = "Current Value",
            Unit = "Unit",
            week_difference = "Change from<br>7-days Ago",
            diff_from_monthly_ave = "Change from<br>30-day Average",
            .fn = md
          ) |>
          fmt_number(
            columns = matches("diff"),
            decimal = 2,
            force_sign = TRUE
          ) |>
          fmt_number(columns = matches("Value"), decimal = 2) |>
          cols_width(everything() ~ px(120)) |>
          cols_align(
            align = "left",
            columns = everything()
          ) |>
          tab_header(
            title = md(paste0(
              "**Site Telemetry Readings at ",
              site_name_rv(),
              "**"
            ))
          ) |>
          tab_source_note(
            source_note = md(paste("Data Retrieved on", date_to))
          ) |>
          tab_source_note(
            source_note = md(paste(
              "Change columns = current value - previous value (7 days, monthly average)"
            ))
          )

        current_status
      } else {
        print("Data not found.")
      }
    })

  output$highlighted_values <-
    render_gt({
      #if(nrow(filtered_data()>0)){

      threshold_values <- map_data |>
        ungroup() |>
        filter(exceed_20 == TRUE) |>
        filter(Parameter != "TW") |>
        select(c(
          LocationName,
          Parameter,
          NumericValue1,
          monthly_mean,
          Unit,
          perc_diff
        )) |>
        arrange(desc(perc_diff)) |>
        gt() |>
        cols_label(
          Parameter = "Parameter",
          LocationName = "Location Name",
          NumericValue1 = "Current Value",
          monthly_mean = "30-day Average",
          Unit = "Unit",
          perc_diff = "Percent Difference from<br>30-day Average (%)",
          .fn = md
        ) |>
        fmt_number(columns = matches("diff"), decimal = 2, force_sign = TRUE) |>
        fmt_number(columns = matches("Value"), decimal = 2) |>
        fmt_number(columns = matches("mean"), decimal = 2) |>
        #cols_width(everything() ~ px(120)) |>
        cols_align(
          align = "center",
          columns = everything()
        ) |>
        tab_header(
          title = md(paste0(
            "**Values exceeding 20% change from 30-day averages**"
          ))
        ) |>
        tab_source_note(
          source_note = md(paste("Data Retrieved on", date_to))
        )

      threshold_values
      # } else {
      #   print("Data not found.")
      # }
    })

  output$shift <- renderText({
    if (nrow(shift_data()) > 0) {
      shift_value <- shift_data() |>
        pull(Value) |>
        round(digits = 3)

      date_applied <- shift_data() |>
        pull(Timestamp)

      if (
        !is.null(shift_value) && !is.na(shift_value) && !is.na(date_applied)
      ) {
        paste0("Value: ", shift_value, "\nDate Applied: ", date_applied)
      } else {
        "NA"
      }
    } else {
      "NA"
    }
  })

  output$battery <- renderText({
    battery_value <- filtered_data() |>
      ungroup() |>
      filter(Parameter == "Bvolt") |>
      pull(NumericValue1)

    if (!is.null(battery_value) && !is.na(battery_value)) {
      as.character(battery_value)
    } else {
      "NA"
    }
  })

  output$stage <- renderText({
    stage_value <- filtered_data() |>
      ungroup() |>
      filter(Parameter == "Stage") |>
      pull(NumericValue1)

    if (!is.null(stage_value) && !is.na(stage_value)) {
      as.character(stage_value)
    } else {
      "NA"
    }
  })

  output$discharge <- renderText({
    discharge_value <- filtered_data() |>
      ungroup() |>
      filter(Parameter == "Discharge") |>
      pull(NumericValue1)

    if (!is.null(discharge_value) && !is.na(discharge_value)) {
      as.character(discharge_value)
    } else {
      "NA"
    }
  })

  output$fv <- renderText({
    most_recent <- fv_data() |>
      pull(Date)

    days_since <- fv_data() |>
      pull(Most_Recent)

    if (!is.null(most_recent) && !is.na(most_recent) && !is.na(days_since)) {
      as.character(paste0(most_recent, " (", days_since, " days ago)"))
    } else {
      "NA"
    }
  })

  output$discharge_plot <- renderPlotly({
    if (nrow(telemetry_data() |> filter(Parameter == "Discharge") > 0)) {
      discharge = telemetry_data() |>
        filter(Parameter == "Discharge") |>
        mutate(line_type = "Discharge") |>
        mutate(
          text = paste0(
            "<br>Date: ",
            Timestamp,
            "<br>Parameter: ",
            Parameter,
            "<br>Value: ",
            round(NumericValue1, 2),
            "<br>Location Name: ",
            LocationName
          )
        )

      hline_df = data.frame(
        yintercept = unique(discharge$monthly_mean),
        line_type = as.factor("30-day Average")
      )

      min_date = as_datetime(min(discharge$Date))
      max_date = as_datetime(max(discharge$Date))

      #if(nrow(discharge)>0){

      d <- ggplot(
        discharge,
        aes(
          x = as_datetime(Timestamp),
          y = NumericValue1,
          col = Parameter,
          text = text,
          group = 1
        )
      ) +
        labs(subtitle = paste0("Data from: ", min_date, " to ", max_date)) +
        geom_line(aes(linetype = line_type), na.rm = FALSE) +
        geom_hline(
          data = hline_df,
          aes(yintercept = yintercept, linetype = line_type),
          color = 'black'
        ) +
        scale_x_datetime(limits = c(min_date, max_date)) +
        scale_y_continuous(expand = c(0, 0)) +
        scale_color_manual(
          labels = c("Discharge"),
          values = c("#238A8DFF")
        ) +
        theme_minimal() +
        theme(
          text = element_text(colour = "black", size = 13),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          axis.line = element_line(colour = "grey50"),
          legend.position = "bottom",
          legend.box = "horizontal",
          plot.title = element_text(hjust = 0),
          plot.subtitle = element_text(hjust = 0, face = "plain", size = 11)
        ) +
        #theme(plot.margin = margin(10, 10, 0, 10, "points")) +
        xlab("Date") +
        ylab("Hourly Mean") +
        scale_linetype_manual(
          values = c("Discharge" = "solid", "30-day Average" = "dashed")
        ) +
        labs(linetype = "")
      ggplotly(d, tooltip = "text")
    } else {
      ggplot() +
        geom_text(aes(x = 1, y = 1, label = 'Insufficient Data')) +
        theme_minimal()
    }
  })

  output$stage_plot <-
    renderPlotly({
      if (nrow(telemetry_data() |> filter(Parameter == "Stage") > 0)) {
        stage = telemetry_data() |>
          filter(Parameter == "Stage") |>
          mutate(line_type = "Stage") |>
          mutate(
            text = paste0(
              "<br>Date: ",
              Timestamp,
              "<br>Parameter: ",
              Parameter,
              "<br>Value: ",
              round(NumericValue1, 2),
              "<br>Location Name: ",
              LocationName
            )
          )

        hline_df = data.frame(
          yintercept = unique(stage$monthly_mean),
          line_type = as.factor("30-day Average")
        )

        #if(nrow(stage)>0){

        min_date = as_datetime(min(stage$Date))
        max_date = as_datetime(max(stage$Date))

        s <- ggplot(
          stage,
          aes(
            x = as_datetime(Timestamp),
            y = NumericValue1,
            col = Parameter,
            text = text,
            group = 1
          )
        ) +
          labs(subtitle = paste0("Data from: ", min_date, " to ", max_date)) +
          geom_line(aes(linetype = line_type), na.rm = FALSE) +
          scale_x_datetime(limits = c(min_date, max_date)) +
          geom_hline(
            data = hline_df,
            aes(yintercept = yintercept, linetype = line_type),
            color = 'black'
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          scale_color_manual(
            labels = c("Stage"),
            values = c("#95d840ff")
          ) +
          theme_minimal() +
          theme(
            text = element_text(colour = "black", size = 13),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.line = element_line(colour = "grey50"),
            legend.position = "bottom",
            legend.box = "horizontal",
            plot.title = element_text(hjust = 0),
            plot.subtitle = element_text(hjust = 0, face = "plain", size = 11)
          ) +
          #theme(plot.margin = margin(10, 10, 0, 10, "points")) +
          xlab("Date") +
          ylab("Hourly Mean") +
          scale_linetype_manual(
            values = c("Discharge" = "solid", "30-day Average" = "dashed")
          ) +
          labs(linetype = "")

        ggplotly(s, tooltip = "text")
      } else {
        ggplot() +
          geom_text(aes(x = 1, y = 1, label = 'Insufficient Data')) +
          theme_minimal()
      }
    })

  output$approval_plot <-
    renderPlotly({
      plot_data = ts_approval |>
        filter(Parameter == "Stage") |>
        mutate(Date = as.Date(Date)) |>
        mutate(LocationName = forcats::fct_inorder(LocationName)) |>
        mutate(
          LevelDescription = fct_relevel(
            LevelDescription,
            "Approved",
            "Reviewed",
            "In Review",
            "Working"
          )
        ) |>
        arrange(LocationName, LevelDescription) |>
        mutate(
          text_ts = paste0(
            "Date: ",
            Date,
            "<br>Location Name: ",
            LocationName,
            "<br>Parameter: ",
            Parameter,
            "<br>Approval Status: ",
            LevelDescription
          )
        )

      stage_sites = unique(plot_data$LocationName)

      field_visit_year = field_visit_year |>
        filter(LocationName %in% stage_sites) |>
        mutate(
          text_fv = paste0(
            "<b>FIELD VISIT </b>",
            "<br>Date: ",
            FieldVisitDate,
            "<br>Location Name: ",
            LocationName,
            "<br>Approval Status: ",
            LevelDescription
          )
        )

      review_levels <- c(
        "Working" = "#fe3200",
        "In Review" = "#ffcc02",
        "Reviewed" = "#b4fec0",
        "Approved" = "#31a926"
      )

      fv_review_levels = c(
        "Working" = "#fe0800",
        "In Review" = "#ffb702",
        "Reviewed" = "#bbfeb4",
        "Approved" = "#52a926"
      )

      t <- ggplot(
        plot_data,
        aes(
          x = Date,
          y = fct_rev(LocationName),
          color = LevelDescription,
          group = LocationName
        )
      ) +
        geom_point(
          aes(
            color = LevelDescription,
            group = LocationName,
            size = 7
          ),
          na.rm = TRUE
        ) +
        geom_point(
          data = field_visit_year,
          shape = 21,
          color = "black",
          size = 3,
          aes(
            x = FieldVisitDate,
            y = fct_rev(LocationName),
            fill = LevelDescription,
            text = text_fv
          ),
          show.legend = TRUE
        ) +
        scale_color_manual(
          values = review_levels,
          name = "Approval Status"
        ) +
        scale_fill_manual(
          values = fv_review_levels,
          name = ""
        ) +
        xlab("Date") +
        ylab("") +
        labs(
          title = paste0(
            unique(plot_data$Parameter),
            " Timeseries Approval Status: Updated ",
            max(plot_data$Date)
          )
        ) +
        theme_soe() +
        scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
        theme(
          panel.grid.major = element_line(linewidth = 0.5, colour = "grey85"),
          panel.grid.minor = element_line(linewidth = 0.5, colour = "grey85"),
          axis.line = element_line(linewidth = 0.5, colour = "black"),
          panel.grid.major.x = element_line(linewidth = 0.5, colour = "black"),
          panel.grid.minor.x = element_blank(),
          axis.text.x = element_text(
            vjust = 1,
            hjust = 1,
            size = 10,
            angle = 45
          ),
          axis.title.y = element_text(
            size = 10,
            margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
          ),
          legend.text = element_text(size = 8),
          #legend.title = element_blank(),
          legend.position = "bottom",
          legend.background = element_rect(colour = "white"),
          panel.background = element_rect(fill = "white", colour = NA), # No border around panel
          plot.background = element_rect(fill = "white", colour = NA)
        )

      tplot = ggplotly(t, tooltip = "text") # tooltip = "text
      #
      tplot
    })

  output$approval_plot_discharge <- renderPlotly({
    plot_data = ts_approval |>
      filter(Parameter == "Discharge") |>
      mutate(Date = as.Date(Date)) |>
      arrange(LocationName) |>
      mutate(LocationName = forcats::fct_inorder(LocationName)) |>
      mutate(
        LevelDescription = fct_relevel(
          LevelDescription,
          "Approved",
          "Reviewed",
          "In Review",
          "Working"
        )
      ) |>
      mutate(
        text_ts = paste0(
          "Date: ",
          Date,
          "<br>Location Name: ",
          LocationName,
          "<br>Parameter: ",
          Parameter,
          "<br>Approval Status: ",
          LevelDescription
        )
      )

    discharge_sites = unique(plot_data$LocationName)

    field_visit_year = field_visit_year |>
      filter(LocationName %in% discharge_sites) |>
      mutate(
        text_fv = paste0(
          "<b>FIELD VISIT </b>",
          "<br>Date: ",
          FieldVisitDate,
          "<br>Location Name: ",
          LocationName,
          "<br>Approval Status: ",
          LevelDescription
        )
      )

    review_levels <- c(
      "Working" = "#fe3200",
      "In Review" = "#ffcc02",
      "Reviewed" = "#b4fec0",
      "Approved" = "#31a926"
    )

    fv_review_levels = c(
      "Working" = "#fe0800",
      "In Review" = "#ffb702",
      "Reviewed" = "#bbfeb4",
      "Approved" = "#52a926"
    )

    d <- ggplot(
      plot_data,
      aes(
        x = Date,
        y = fct_rev(LocationName),
        color = LevelDescription,
        group = LocationName
      )
    ) +
      geom_point(
        aes(
          color = LevelDescription,
          group = LocationName,
          size = 7
        ),
        na.rm = TRUE
      ) +
      geom_point(
        data = field_visit_year,
        shape = 21,
        size = 3,
        color = "black",
        aes(
          x = FieldVisitDate,
          y = fct_rev(LocationName),
          fill = LevelDescription,
          text = text_fv
        ),
        show.legend = TRUE
      ) +
      scale_color_manual(
        values = review_levels,
        name = "Approval Status"
      ) +
      scale_fill_manual(
        values = fv_review_levels,
        name = ""
      ) +
      xlab("Date") +
      ylab("") +
      labs(
        title = paste0(
          unique(plot_data$Parameter),
          " Timeseries Approval Status: Updated ",
          max(plot_data$Date)
        )
      ) +
      theme_soe() +
      scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
      theme(
        panel.grid.major = element_line(linewidth = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "grey85"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(vjust = 1, hjust = 1, size = 10, angle = 45),
        axis.title.y = element_text(
          size = 10,
          margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
        ),
        legend.text = element_text(size = 8),
        #legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(colour = "white"),
        panel.background = element_rect(fill = "white", colour = NA), # No border around panel
        plot.background = element_rect(fill = "white", colour = NA)
      )

    dplot = ggplotly(d, tooltip = "text")
    #
    dplot
  })

  output$timeseries_approval_status <- renderPlotly({
    plot_data = timeseries_data() |>
      mutate(
        text_ts = paste0(
          "Date: ",
          Date,
          "<br>Location Name: ",
          LocationName,
          "<br>Parameter: ",
          Parameter,
          "<br>Approval Status: ",
          LevelDescription
        )
      )

    timeseries_sites = unique(plot_data$LocationName)

    field_visit_year = field_visit_year |>
      filter(LocationName %in% timeseries_sites) |>
      mutate(
        text_fv = paste0(
          "<b>FIELD VISIT </b>",
          "<br>Date: ",
          FieldVisitDate,
          "<br>Location Name: ",
          LocationName,
          "<br>Approval Status: ",
          LevelDescription
        )
      )

    review_levels <- c(
      "Working" = "#fe3200",
      "In Review" = "#ffcc02",
      "Reviewed" = "#b4fec0",
      "Approved" = "#31a926"
    )

    fv_review_levels = c(
      "Working" = "#fe0800",
      "In Review" = "#ffb702",
      "Reviewed" = "#bbfeb4",
      "Approved" = "#52a926"
    )

    d <- ggplot(
      plot_data,
      aes(
        x = Date,
        y = fct_rev(LocationName),
        color = LevelDescription,
        group = LocationName
      )
    ) +
      geom_point(
        aes(
          color = LevelDescription,
          group = LocationName,
          size = 7
        ),
        na.rm = TRUE
      ) +
      geom_point(
        data = field_visit_year,
        shape = 21,
        size = 3,
        color = "black",
        aes(
          x = FieldVisitDate,
          y = fct_rev(LocationName),
          fill = LevelDescription,
          text = text_fv
        ),
        show.legend = TRUE
      ) +
      scale_color_manual(
        values = review_levels,
        name = "Approval Status"
      ) +
      scale_fill_manual(
        values = fv_review_levels,
        name = ""
      ) +
      xlab("Date") +
      ylab("") +
      labs(
        title = paste0(
          unique(plot_data$Parameter),
          " Timeseries Approval Status: Updated ",
          max(plot_data$Date)
        )
      ) +
      theme_soe() +
      scale_x_date(date_labels = "%Y-%b", date_breaks = "1 month") +
      theme(
        panel.grid.major = element_line(linewidth = 0.5, colour = "grey85"),
        panel.grid.minor = element_line(linewidth = 0.5, colour = "grey85"),
        axis.line = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.major.x = element_line(linewidth = 0.5, colour = "black"),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(vjust = 1, hjust = 1, size = 10, angle = 45),
        axis.title.y = element_text(
          size = 10,
          margin = margin(t = 0, r = 10, b = 0, l = 0, unit = "pt")
        ),
        legend.text = element_text(size = 8),
        #legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(colour = "white"),
        panel.background = element_rect(fill = "white", colour = NA), # No border around panel
        plot.background = element_rect(fill = "white", colour = NA)
      )

    dplot = ggplotly(d, tooltip = "text")

    dplot
  })

  output$map <- renderLeaflet({
    most_recent <- most_recent |>
      mutate(
        legend = as.factor(legend),
        legend = fct_relevel(legend, "< 25", "25-30", "> 30")
      ) |>
      rename(latitude = Latitude, longitude = Longitude)

    colorpal <- colorFactor(
      palette = c("#4fb06d", "#f07857", "#bf2c34"),
      domain = most_recent$legend
    )

    leaflet(most_recent) |>
      addTiles() |>
      setView(lat = 51.8, lng = -121.8, zoom = 6) |>
      # addPolygons(
      #   data = regions, layerId = ~REGION_NAME
      # ) |>
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        color = "black",
        fillColor = ~ colorpal(legend),
        radius = 4,
        weight = 5,
        fillOpacity = 1,
        label = ~LocationName,
        popup = ~ paste(
          "<strong>",
          LocationName,
          ": ",
          LocationIdentifier,
          "</strong><br>Elevation: ",
          Elevation,
          " m",
          "<br>Contact Name: ",
          UploadedByUser,
          "<br>Days Since Last Visit: ",
          Most_Recent
        ),
        labelOptions = labelOptions(
          noHide = FALSE,
          offset = c(0, -12),
          opacity = 1,
          textsize = "12px",
          textOnly = FALSE
        )
      ) |>
      addLegend(
        position = "topright",
        pal = colorpal,
        values = ~legend,
        title = "Days Since Last Visit"
      )
  })
}
