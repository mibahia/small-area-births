library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)

# ── Load data ───────────────────────────────────────────────────────────────
msoa_asfr <- readRDS(
  "data/processed/smooth_rolling_sum_asfr_msoa_from_2011_to_2021.rds"
)

msoa_tfr <- readRDS("data/processed/msoa_tfr_timeseries.rds")

ward_asfr <- readRDS("data/processed/ward_asfr.rds")

ward_tfr <- readRDS("data/processed/ward_tfr.rds")

# ── Colours ─────────────────────────────────────────────────────────────────
clr_bg <- "#0f1923"
clr_panel <- "#162030"
clr_sidebar <- "#111d29"
clr_border <- "#1e3045"
clr_accent <- "#4ecdc4"
clr_accent2 <- "#f7c59f"
clr_text <- "#e8e0d5"
clr_muted <- "#8299ad"

chart_palette <- c(
  "#4ecdc4",
  "#f7c59f",
  "#ffe66d",
  "#a8dadc",
  "#e76f51",
  "#90be6d",
  "#f4a261",
  "#577590",
  "#43aa8b",
  "#f9c74f",
  "#ff6b6b",
  "#b5838d",
  "#6d6875",
  "#c9cba3",
  "#ffe1a8",
  "#e26d5c"
)

# ── ggplot theme ─────────────────────────────────────────────────────────────
theme_fertility <- function() {
  theme_minimal(base_family = "DM Sans", base_size = 13) +
    theme(
      plot.background = element_rect(fill = clr_panel, colour = NA),
      panel.background = element_rect(fill = clr_panel, colour = NA),
      panel.grid.major = element_line(colour = clr_border, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      axis.text = element_text(colour = clr_muted, size = 11),
      axis.title = element_text(
        colour = clr_muted,
        size = 11,
        margin = margin(t = 6, r = 6)
      ),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(
        colour = clr_text,
        size = 17,
        family = "DM Serif Display",
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        colour = clr_muted,
        size = 11.5,
        margin = margin(b = 14)
      ),
      plot.caption = element_text(colour = clr_muted, size = 9),
      legend.background = element_rect(fill = clr_panel, colour = NA),
      legend.key = element_rect(fill = clr_panel, colour = NA),
      legend.title = element_text(colour = clr_muted, size = 10),
      legend.text = element_text(colour = clr_text, size = 10.5),
      legend.key.size = grid::unit(1.1, "cm"),
      legend.position = "right",
      plot.margin = margin(12, 16, 12, 12)
    )
}

# ── CSS ──────────────────────────────────────────────────────────────────────
css <- paste0(
  "
  body, .tab-content, .tab-pane {
    background-color: ",
  clr_bg,
  " !important;
  }
  .navbar-default {
    background-color: ",
  clr_bg,
  " !important;
    border-bottom: 1px solid ",
  clr_border,
  " !important;
  }
  .navbar-default .navbar-brand {
    color: ",
  clr_text,
  " !important;
    font-family: 'DM Serif Display', serif;
    font-size: 1.3rem;
  }
  .navbar-default .navbar-nav > li > a {
    color: ",
  clr_muted,
  " !important;
    font-size: 0.8rem;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.08em;
  }
  .navbar-default .navbar-nav > .active > a,
  .navbar-default .navbar-nav > .active > a:focus,
  .navbar-default .navbar-nav > .active > a:hover {
    color: ",
  clr_accent,
  " !important;
    background-color: transparent !important;
    border-bottom: 2px solid ",
  clr_accent,
  ";
  }
  .navbar-default .navbar-nav > li > a:hover {
    color: ",
  clr_accent,
  " !important;
  }
  /* Sidebar panels */
  .well {
    background-color: ",
  clr_sidebar,
  " !important;
    border: 1px solid ",
  clr_border,
  " !important;
    border-radius: 6px;
  }
  /* Labels */
  label, .control-label {
    color: ",
  clr_muted,
  " !important;
    font-size: 0.72rem !important;
    text-transform: uppercase !important;
    letter-spacing: 0.09em !important;
    font-weight: 600 !important;
  }
  /* Checkbox item labels (not uppercase) */
  .shiny-options-group label {
    text-transform: none !important;
    letter-spacing: normal !important;
    font-weight: 400 !important;
    font-size: 0.83rem !important;
    color: ",
  clr_text,
  " !important;
  }
  input[type='checkbox'] { accent-color: ",
  clr_accent,
  "; }
  /* Radio pills — ASFR geography toggle */
  #asfr_geo .radio label,
  #tfr_geo  .radio label {
    display: inline-block;
    padding: 0.28rem 0.8rem;
    border-radius: 100px;
    font-size: 0.82rem;
    font-weight: 500;
    cursor: pointer;
    color: ",
  clr_muted,
  " !important;
    border: 1px solid ",
  clr_border,
  ";
    margin-right: 0.3rem;
    text-transform: none !important;
    letter-spacing: normal !important;
    transition: all 0.15s;
  }
  #asfr_geo .radio input[type='radio']:checked + label,
  #tfr_geo  .radio input[type='radio']:checked + label {
    background-color: ",
  clr_accent,
  " !important;
    border-color:     ",
  clr_accent,
  " !important;
    color:            ",
  clr_bg,
  " !important;
    font-weight: 600;
  }
  #asfr_geo .radio input[type='radio'],
  #tfr_geo  .radio input[type='radio'] { display: none; }
  /* Selectize */
  .selectize-control .selectize-input {
    background-color: #1a2d40 !important;
    border: 1px solid ",
  clr_border,
  " !important;
    color: ",
  clr_text,
  " !important;
    border-radius: 5px !important;
    box-shadow: none !important;
    font-size: 0.88rem;
  }
  .selectize-dropdown {
    background-color: #1a2d40 !important;
    border: 1px solid ",
  clr_border,
  " !important;
    color: ",
  clr_text,
  " !important;
  }
  .selectize-dropdown .option:hover,
  .selectize-dropdown .option.selected {
    background-color: ",
  clr_accent,
  "22 !important;
    color: ",
  clr_accent,
  " !important;
  }
  /* Scrollable checkbox list */
  .cb-scroll {
    max-height: 300px;
    overflow-y: auto;
    padding-right: 4px;
  }
  .cb-scroll::-webkit-scrollbar { width: 4px; }
  .cb-scroll::-webkit-scrollbar-thumb {
    background: ",
  clr_border,
  "; border-radius: 4px;
  }
  /* Plot card */
  .plot-card {
    background-color: ",
  clr_panel,
  ";
    border: 1px solid ",
  clr_border,
  ";
    border-radius: 8px;
    padding: 1.25rem 1.5rem;
  }
  /* Sidebar note */
  .sidebar-note {
    font-size: 0.77rem;
    color: ",
  clr_muted,
  ";
    font-style: italic;
    border-left: 2px solid ",
  clr_border,
  ";
    padding-left: 0.55rem;
    margin-bottom: 0.8rem;
    line-height: 1.5;
    text-transform: none !important;
    letter-spacing: normal !important;
    font-weight: 400 !important;
  }
  hr.divider {
    border-color: ",
  clr_border,
  ";
    margin: 0.75rem 0;
  }
  .btn { font-size: 0.78rem !important; }
  .btn-outline-secondary {
    color: ",
  clr_muted,
  " !important;
    border-color: ",
  clr_border,
  " !important;
  }
  .btn-outline-secondary:hover {
    color: ",
  clr_accent,
  " !important;
    border-color: ",
  clr_accent,
  " !important;
    background: transparent !important;
  }
"
)

# ── Helper: geo label ─────────────────────────────────────────────────────
geo_label <- function(label) {
  tags$div(
    style = paste0(
      "font-size:0.72rem; text-transform:uppercase; letter-spacing:0.09em;",
      "font-weight:600; color:",
      clr_muted,
      "; margin-bottom:0.3rem;"
    ),
    label
  )
}

# ═══════════════════════════════════════════════════════════════════════════
#  UI
# ═══════════════════════════════════════════════════════════════════════════
ui <- navbarPage(
  title = "Fertility Rates Explorer",
  theme = bs_theme(
    version = 5,
    bg = clr_bg,
    fg = clr_text,
    primary = clr_accent,
    base_font = font_google("DM Sans"),
    heading_font = font_google("DM Serif Display")
  ),
  header = tags$style(HTML(css)),

  # ══════════════════════════════════════════════════════
  #  TAB 1 — Age-Specific Fertility Rates
  # ══════════════════════════════════════════════════════
  tabPanel(
    "Age-Specific Fertility Rates",
    sidebarLayout(
      sidebarPanel(
        width = 3,

        geo_label("Geography"),
        radioButtons(
          "asfr_geo",
          label = NULL,
          choices = c("MSOA" = "msoa", "Ward" = "ward"),
          selected = "msoa",
          inline = TRUE
        ),
        hr(class = "divider"),

        # MSOA controls
        conditionalPanel(
          "input.asfr_geo === 'msoa'",
          selectInput(
            "asfr_year",
            "Year:",
            choices = sort(unique(msoa_asfr$year))
          ),
          selectInput(
            "asfr_msoa_la",
            "Local Authority 2023:",
            choices = sort(unique(msoa_asfr$gss_name))
          ),
          div(
            class = "d-flex gap-2 mb-2",
            actionButton(
              "asfr_msoa_all",
              "Select all",
              class = "btn btn-sm btn-outline-secondary"
            ),
            actionButton(
              "asfr_msoa_clear",
              "Clear all",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          div(
            class = "cb-scroll",
            checkboxGroupInput("asfr_msoa_cd", "MSOAs:", choices = NULL)
          )
        ),

        # Ward controls
        conditionalPanel(
          "input.asfr_geo === 'ward'",
          selectInput(
            "asfr_ward_la",
            "Local Authority 2022:",
            choices = sort(unique(ward_asfr$gss_name))
          ),
          div(
            class = "d-flex gap-2 mb-2",
            actionButton(
              "asfr_ward_all",
              "Select all",
              class = "btn btn-sm btn-outline-secondary"
            ),
            actionButton(
              "asfr_ward_clear",
              "Clear all",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          div(
            class = "cb-scroll",
            checkboxGroupInput("asfr_ward", "Wards:", choices = NULL)
          )
        )
      ),

      mainPanel(
        width = 9,
        div(class = "plot-card", plotOutput("asfr_plot", height = "580px"))
      )
    )
  ),

  # ══════════════════════════════════════════════════════
  #  TAB 2 — Total Fertility Rates
  # ══════════════════════════════════════════════════════
  tabPanel(
    "Total Fertility Rates",
    sidebarLayout(
      sidebarPanel(
        width = 3,

        geo_label("Geography"),
        radioButtons(
          "tfr_geo",
          label = NULL,
          choices = c("MSOA" = "msoa", "Ward" = "ward"),
          selected = "msoa",
          inline = TRUE
        ),
        hr(class = "divider"),

        # MSOA TFR — time series
        conditionalPanel(
          "input.tfr_geo === 'msoa'",
          div(
            class = "sidebar-note",
            "MSOA TFR is available as a time series. Select MSOAs to compare trends over time."
          ),
          selectInput(
            "tfr_msoa_la",
            "Local Authority 2023:",
            choices = sort(unique(na.omit(msoa_tfr$gss_name)))
          ),
          div(
            class = "d-flex gap-2 mb-2",
            actionButton(
              "tfr_msoa_all",
              "Select all",
              class = "btn btn-sm btn-outline-secondary"
            ),
            actionButton(
              "tfr_msoa_clear",
              "Clear all",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          div(
            class = "cb-scroll",
            checkboxGroupInput("tfr_msoa_cd", "MSOAs:", choices = NULL)
          )
        ),

        # Ward TFR — cross-sectional ranked
        conditionalPanel(
          "input.tfr_geo === 'ward'",
          div(
            class = "sidebar-note",
            "Ward TFR is a single cross-sectional estimate. Wards are ranked by TFR within the selected local authority."
          ),
          selectInput(
            "tfr_ward_la",
            "Local Authority 2022:",
            choices = sort(unique(na.omit(ward_tfr$gss_name)))
          ),
          div(
            class = "d-flex gap-2 mb-2",
            actionButton(
              "tfr_ward_all",
              "Select all",
              class = "btn btn-sm btn-outline-secondary"
            ),
            actionButton(
              "tfr_ward_clear",
              "Clear all",
              class = "btn btn-sm btn-outline-secondary"
            )
          ),
          div(
            class = "cb-scroll",
            checkboxGroupInput("tfr_ward", "Wards:", choices = NULL)
          )
        )
      ),

      mainPanel(
        width = 9,
        div(class = "plot-card", plotOutput("tfr_plot", height = "700px"))
      )
    )
  )
)

# ═══════════════════════════════════════════════════════════════════════════
#  SERVER
# ═══════════════════════════════════════════════════════════════════════════
server <- function(input, output, session) {
  # Stored choice vectors for select-all
  asfr_msoa_choices <- reactiveVal(NULL)
  asfr_ward_choices <- reactiveVal(NULL)
  tfr_msoa_choices <- reactiveVal(NULL)
  tfr_ward_choices <- reactiveVal(NULL)

  # ── ASFR: MSOA checkboxes ──────────────────────────────────────────────
  observeEvent(input$asfr_msoa_la, {
    lookup <- msoa_asfr |>
      filter(gss_name == input$asfr_msoa_la) |>
      distinct(msoa21_code, msoa21_name) |>
      arrange(msoa21_name)
    cv <- setNames(lookup$msoa21_code, lookup$msoa21_name)
    asfr_msoa_choices(cv)
    updateCheckboxGroupInput(
      session,
      "asfr_msoa_cd",
      choices = cv,
      selected = cv[1]
    )
  })
  observeEvent(
    input$asfr_msoa_all,
    updateCheckboxGroupInput(
      session,
      "asfr_msoa_cd",
      selected = asfr_msoa_choices()
    )
  )
  observeEvent(
    input$asfr_msoa_clear,
    updateCheckboxGroupInput(session, "asfr_msoa_cd", selected = character(0))
  )

  # ── ASFR: Ward checkboxes ──────────────────────────────────────────────
  observeEvent(input$asfr_ward_la, {
    wards <- ward_asfr |>
      filter(gss_name == input$asfr_ward_la) |>
      pull(wd22nm) |>
      unique() |>
      sort()
    asfr_ward_choices(wards)
    updateCheckboxGroupInput(
      session,
      "asfr_ward",
      choices = wards,
      selected = wards[1]
    )
  })
  observeEvent(
    input$asfr_ward_all,
    updateCheckboxGroupInput(
      session,
      "asfr_ward",
      selected = asfr_ward_choices()
    )
  )
  observeEvent(
    input$asfr_ward_clear,
    updateCheckboxGroupInput(session, "asfr_ward", selected = character(0))
  )

  # ── TFR: MSOA checkboxes ───────────────────────────────────────────────
  observeEvent(input$tfr_msoa_la, {
    req(input$tfr_msoa_la)
    lookup <- msoa_tfr |>
      filter(!is.na(gss_name), gss_name == input$tfr_msoa_la) |>
      distinct(msoa21_code, msoa21_name) |>
      arrange(msoa21_name)
    cv <- setNames(lookup$msoa21_code, lookup$msoa21_name)
    tfr_msoa_choices(cv)
    updateCheckboxGroupInput(
      session,
      "tfr_msoa_cd",
      choices = cv,
      selected = cv[1]
    )
  })
  observeEvent(
    input$tfr_msoa_all,
    updateCheckboxGroupInput(
      session,
      "tfr_msoa_cd",
      selected = tfr_msoa_choices()
    )
  )
  observeEvent(
    input$tfr_msoa_clear,
    updateCheckboxGroupInput(session, "tfr_msoa_cd", selected = character(0))
  )

  # ── TFR: Ward checkboxes ───────────────────────────────────────────────
  observeEvent(input$tfr_ward_la, {
    req(input$tfr_ward_la)
    wards <- ward_tfr |>
      filter(!is.na(gss_name), gss_name == input$tfr_ward_la) |>
      pull(wd22nm) |>
      unique() |>
      sort()
    tfr_ward_choices(wards)
    # Default: all selected so ranked chart appears immediately
    updateCheckboxGroupInput(
      session,
      "tfr_ward",
      choices = wards,
      selected = wards
    )
  })
  observeEvent(
    input$tfr_ward_all,
    updateCheckboxGroupInput(session, "tfr_ward", selected = tfr_ward_choices())
  )
  observeEvent(
    input$tfr_ward_clear,
    updateCheckboxGroupInput(session, "tfr_ward", selected = character(0))
  )

  # ══════════════════════════════════════════════════════
  #  ASFR PLOT
  # ══════════════════════════════════════════════════════
  output$asfr_plot <- renderPlot(bg = clr_panel, {
    if (input$asfr_geo == "msoa") {
      req(input$asfr_msoa_cd, input$asfr_year)

      dat <- msoa_asfr |>
        filter(msoa21_code %in% input$asfr_msoa_cd, year == input$asfr_year)

      n <- length(unique(dat$msoa21_name))

      ggplot(
        dat,
        aes(
          age,
          fertility_rate,
          colour = msoa21_name,
          linetype = fitting_status
        )
      ) +
        geom_line(linewidth = 1.3, alpha = 0.9) +
        scale_colour_manual(values = rep(chart_palette, length.out = n)) +
        scale_linetype_manual(
          values = c("succeeded" = "solid", "failed" = "dotted"),
          labels = c("succeeded" = "Model fit", "failed" = "No fit")
        ) +
        labs(
          title = paste("Age-specific fertility rates \u2014", input$asfr_year),
          subtitle = input$asfr_msoa_la,
          x = "Age",
          y = "Fertility rate",
          colour = "MSOA",
          linetype = "Fit status"
        ) +
        theme_fertility()
    } else {
      req(input$asfr_ward)

      dat <- ward_asfr |>
        filter(wd22nm %in% input$asfr_ward)

      n <- length(unique(dat$wd22nm))

      ggplot(dat, aes(age, fertility_rate, colour = wd22nm)) +
        geom_line(linewidth = 1.3, alpha = 0.9) +
        scale_colour_manual(values = rep(chart_palette, length.out = n)) +
        labs(
          title = "Age-specific fertility rates",
          subtitle = input$asfr_ward_la,
          x = "Age",
          y = "Fertility rate",
          colour = "Ward"
        ) +
        theme_fertility()
    }
  })

  # ══════════════════════════════════════════════════════
  #  TFR PLOT
  # ══════════════════════════════════════════════════════
  output$tfr_plot <- renderPlot(bg = clr_panel, {
    # ── MSOA: time series ──────────────────────────────
    if (input$tfr_geo == "msoa") {
      req(input$tfr_msoa_cd, input$tfr_msoa_la)

      dat <- msoa_tfr |>
        filter(msoa21_code %in% input$tfr_msoa_cd)

      la_all <- msoa_tfr |>
        filter(gss_name == input$tfr_msoa_la)

      y_limits <- range(la_all$tfr, na.rm = TRUE)

      # padding
      y_pad <- diff(y_limits) * 0.05
      y_limits <- c(y_limits[1] - y_pad, y_limits[2] + y_pad)

      req(nrow(dat) > 0)
      n <- length(unique(dat$msoa21_name))

      ggplot(dat, aes(year, tfr, colour = msoa21_name)) +
        geom_hline(
          yintercept = 2.1,
          colour = clr_muted,
          linetype = "dashed",
          linewidth = 0.6,
          alpha = 0.7
        ) +
        geom_line(linewidth = 1.2, alpha = 0.9) +
        geom_point(size = 2.5, alpha = 0.85) +
        scale_colour_manual(values = rep(chart_palette, length.out = n)) +
        scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
        annotate(
          "text",
          x = min(dat$year, na.rm = TRUE),
          y = 2.1,
          label = "Replacement level (2.1)",
          vjust = -0.5,
          hjust = 0,
          colour = clr_muted,
          size = 3.3,
          family = "DM Sans"
        ) +
        coord_cartesian(ylim = y_limits) +
        labs(
          title = "Total fertility rate",
          subtitle = input$tfr_msoa_la,
          x = "Year",
          y = "Total fertility rate",
          colour = "MSOA"
        ) +
        theme_fertility()

      # ── Ward: ranked lollipop ──────────────────────────
    } else {
      req(input$tfr_ward, input$tfr_ward_la)

      dat <- ward_tfr |>
        filter(wd22nm %in% input$tfr_ward) |>
        mutate(wd22nm = reorder(wd22nm, tfr))

      la_all <- ward_tfr |>
        filter(gss_name == input$tfr_ward_la)

      x_limits <- range(la_all$tfr, na.rm = TRUE)

      # padding
      x_pad <- diff(x_limits) * 0.08
      x_limits <- c(x_limits[1] - x_pad, x_limits[2] + x_pad)

      req(nrow(dat) > 0)
      la_avg <- mean(la_all$tfr, na.rm = TRUE)

      ggplot(dat, aes(x = tfr, y = wd22nm)) +
        geom_vline(
          xintercept = 2.1,
          colour = clr_muted,
          linetype = "dashed",
          linewidth = 0.6,
          alpha = 0.7
        ) +
        geom_vline(
          xintercept = la_avg,
          colour = clr_accent2,
          linetype = "dotted",
          linewidth = 0.7,
          alpha = 0.85
        ) +
        geom_segment(
          aes(x = 0, xend = tfr, yend = wd22nm),
          colour = clr_border,
          linewidth = 0.5
        ) +
        geom_point(aes(colour = tfr), size = 4.5, alpha = 0.95) +
        scale_colour_gradient(
          low = "#577590",
          high = clr_accent,
          name = "TFR",
          guide = guide_colorbar(
            barwidth = 0.6,
            barheight = 9,
            title.position = "top"
          )
        ) +
        annotate(
          "text",
          x = 2.1,
          y = Inf,
          label = "Replacement\nlevel (2.1)",
          vjust = 1.4,
          hjust = -0.08,
          colour = clr_muted,
          size = 3.1,
          family = "DM Sans"
        ) +
        annotate(
          "text",
          x = la_avg,
          y = Inf,
          label = paste0("LA average (", round(la_avg, 2), ")"),
          vjust = 1.4,
          hjust = 1.1,
          colour = clr_accent2,
          size = 3.1,
          family = "DM Sans"
        ) +
        coord_cartesian(xlim = x_limits) +
        labs(
          title = "Total fertility rate \u2014 wards ranked",
          subtitle = input$tfr_ward_la,
          x = "Total fertility rate",
          y = NULL
        ) +
        theme_fertility() +
        theme(
          axis.text.y = element_text(size = 9.5),
          panel.grid.major.y = element_line(
            colour = clr_border,
            linewidth = 0.3
          ),
          panel.grid.major.x = element_line(
            colour = clr_border,
            linewidth = 0.5
          )
        )
    }
  })
}

shinyApp(ui, server)
