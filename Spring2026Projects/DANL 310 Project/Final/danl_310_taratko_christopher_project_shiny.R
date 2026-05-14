# =========================================================
# BeeNestShinyApp.R
# Cavity-Nesting Bee & Wasp Study – 3-Page Shiny Application
# my layout:
# Page 1: Cover / Index
# Page 2: Story-telling Graphs (static, sidebar-driven)
# Page 3: Interactive Plots (use plotly)
#
# Note --> All CSV reads use: read.csv("Data Analysis/{file name}"), change as needed if it doesn't work
# Background: #fdf6e3  |  Outline: grey50  |  lwd: 0.5
# =========================================================

# ── Libraries ─────────────────────────────────────────────
library(shiny)
library(shinydashboard)   
library(tidyverse)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(ggtext)
library(ggridges)
library(janitor)
library(plotly)           

# =========================================================
# GLOBAL DATA READS  (run once at app startup)
# =========================================================
ID_freq              <- read.csv("Data Analysis/Actual andPresumed ID.csv")
ID_freq_comp_raw     <- read.csv("Data Analysis/Actual_presumed_complete.csv")
fill_by_location_2025     <- read.csv("Data Analysis/Proportion_of_Fill_By_Location - Sheet1.csv")
fill_by_location_2025_clean <- read.csv("Data Analysis/Proportion_of_Fill_By_Location - 2025-2026.csv")
ID_freq_2025_clean   <- read.csv("Data Analysis/Bee_Occupants_for_ESA_poster_final.csv")
fill_2024_raw        <- read.csv("Data Analysis/Week_of_First_FIll_for_CSV - Sheet1.csv")
fill_2025_clean_raw  <- read.csv("Data Analysis/Week_of_First_FIll_2025 - 2026_cleaned.csv")

# ── Manual 2023 first-fill dataset ───────────────────────
Week_vec <- rep(1:20, each = 5)
Fill_Type_vec <- rep(c("WhiteDebris","Masticated_Leaves","Light_Mud","Leaves","Grass"), 20)
Num_vec <- c(
  0,0,0,0,0,  0,0,0,0,0,  1,0,4,0,0,  0,2,1,0,0,  1,2,1,0,0,
  7,4,5,0,0,  2,8,4,0,0,  0,1,3,0,3,  0,6,5,0,2,  0,0,6,0,3,
  1,0,6,1,2,  1,0,1,7,0,  0,0,0,14,2, 1,0,0,11,7, 0,0,0,9,1,
  0,0,0,8,2,  0,0,0,4,4,  0,0,0,3,2,  0,0,0,0,1,  0,0,0,0,0
)
fill_2023 <- data.frame(Week = Week_vec, Fill_Type = Fill_Type_vec,
                        Number_of_Tubes_First_Filled = Num_vec) |>
  mutate(Fill_Type = ifelse(Fill_Type == "WhiteDebris", "Debris", Fill_Type))

# ── Cleaned year datasets ─────────────────────────────────
fill_2023_cleaned <- fill_2023 |>
  mutate(Year = 2023,
         Number_of_Tubes_First_Filled = replace_na(Number_of_Tubes_First_Filled, 0))

fill_2024_cleaned <- fill_2024_raw |>
  mutate(Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled,
         Number_of_Tubes_First_Filled = replace_na(Number_of_Tubes_First_Filled, 0),
         Year = 2024) |>
  select(Week, Year, Number_of_Tubes_First_Filled, Fill_Type)

fill_2025_cleaned <- fill_2025_clean_raw |>
  mutate(Number_of_Tubes_First_Filled = Number_of_Tubes_First.Filled,
         Number_of_Tubes_First_Filled = replace_na(Number_of_Tubes_First_Filled, 0),
         Year = 2025) |>
  select(Week, Year, Number_of_Tubes_First_Filled, Fill_Type)

combined_df <- bind_rows(fill_2023_cleaned, fill_2024_cleaned, fill_2025_cleaned)

# Cumulative totals (all fill types combined per week)
fill_2023_cum <- fill_2023_cleaned |> mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2024_cum <- fill_2024_cleaned |> mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
fill_2025_cum  <- fill_2025_cleaned |> mutate(cum_sum = cumsum(Number_of_Tubes_First_Filled))
full_cum       <- bind_rows(fill_2023_cum, fill_2024_cum, fill_2025_cum)

full_cum_clean <- full_cum |>
  mutate(Fill_Type = recode(Fill_Type, "Intact_Leaves" = "Leaves")) |>
  filter(!Fill_Type %in% c("Silk", "Debris"))

# ── Species-level data ────────────────────────────────────
sample_sizes_species <- data.frame(
  Location    = c("Arboretum","BeeBarn","EGarden","Meadow","NoMowZone"),
  Sample_Size = c(2, 95, 7, 14, 49)
)
ID_freq_comp <- ID_freq_comp_raw |>
  mutate(Inhabitant = factor(Inhabitant,
    levels = c("Chrysis cessata","Coelioxys alternatus","Isodontia mexicana",
               "Ancistrocerus","Euodynerus foraminatus","Trypoxylon lactitarse",
               "Megachile pugnata","Megachile rotundata","Megachile relativa",
               "Osmia caerulescens"))) |>
  left_join(sample_sizes_species, by = "Location")

sample_sizes_filltype <- data.frame(
  Location    = c("Arboretum","ISC/Greenhouse","No Mow Zone","EGarden","Island Preserve"),
  Sample_Size = c(140, 40, 20, 20, 160)
)
fill_by_location_2025      <- left_join(fill_by_location_2025,       sample_sizes_filltype, by = "Location")
fill_by_location_2025_clean <- left_join(fill_by_location_2025_clean, sample_sizes_filltype, by = "Location")

fill_by_location_2025_clean <- fill_by_location_2025_clean |>
  mutate(Fill_Type = factor(Fill_Type,
    levels = c("Empty","Debris","Silk","Grass","Light_Mud","Intact_Leaves","Masticated_Leaves")))

ID_freq_2025_clean <- ID_freq_2025_clean |>
  mutate(Inhabitant = factor(Inhabitant,
    levels = c("Parasite","Grass Wasp","Potter Wasp","Square-headed Wasp",
               "Unknown Wasp","Leafcutter- Mp","Leafcutter- Leaf","Mason Bee","Small Cell Bee")))

# =========================================================
# SHARED AESTHETICS
# =========================================================
BG <- "#fdf6e3"

my_colors <- c(
  "Grass"             = "darkgreen",
  "Leaves"            = "#95D840FF",
  "Intact_Leaves"     = "#29AF7FFF",
  "Masticated_Leaves" = "#FDE725FF",
  "Light_Mud"         = "#33638DFF",
  "Debris"            = "grey",
  "Silk"              = "white",
  "Empty"             = "black"
)
my_labels <- c(
  "Grass"             = "Grass",
  "Leaves"            = "Leaves",
  "Intact_Leaves"     = "Intact Leaves",
  "Masticated_Leaves" = "Masticated Leaves",
  "Light_Mud"         = "Light Mud",
  "Debris"            = "Debris",
  "Silk"              = "Spider Silk",
  "Empty"             = "Empty"
)
species_colors <- c(
  "Chrysis cessata"        = "#480154FF",
  "Coelioxys alternatus"   = "#481567FF",
  "Isodontia mexicana"     = "#1F968BFF",
  "Ancistrocerus"          = "#287D8EFF",
  "Euodynerus foraminatus" = "#33638DFF",
  "Trypoxylon lactitarse"  = "#404788FF",
  "Megachile pugnata"      = "#55C667FF",
  "Megachile rotundata"    = "lightgreen",
  "Megachile relativa"     = "#FDE725FF",
  "Osmia caerulescens"     = "#DCE319FF"
)

# my_theme: plots flush with the cream background
my_theme <- function() {
  theme(
    plot.title        = element_text(face = "bold", family = "Times New Roman", size = 16),
    axis.title        = element_text(face = "italic", family = "Times New Roman"),
    axis.text         = element_text(family = "Times New Roman"),
    plot.background   = element_rect(fill = BG, color = NA),
    panel.background  = element_rect(fill = BG, color = "grey50"),   # flush with bg
    panel.grid.minor  = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    axis.line.y       = element_line(color = "grey50", linewidth = 0.5),
    legend.background = element_rect(fill = BG, color = NA),
    legend.title      = element_text(family = "Times New Roman", face = "bold"),
    strip.background  = element_rect(fill = "#f0e6cc", color = "grey50"),
    strip.text        = element_text(color = "black", face = "bold"),
    plot.margin       = margin(12, 12, 12, 12)
  )
}

# =========================================================
# PLOT FACTORY  (named list for Page 2 dropdown)
# =========================================================
make_plots <- function() {

  # -- P1: 2023 First Fill Stacked Bar --------------------
  fill_inhabitant_labels <- tibble(
    Fill_Type = c("Grass","Grass","Grass",
                  "Masticated_Leaves","Masticated_Leaves",
                  "Intact_Leaves","Intact_Leaves",
                  "Light_Mud","Silk","Debris","Empty"),
    Inhabitant = c("Grass Wasp","Square-headed Wasp","Potter Wasp",
                   "Leafcutter- Mp","Leafcutter- Leaf",
                   "Leafcutter- Mp","Leafcutter- Leaf",
                   "Mason Bee","Mason Bee","Parasite", NA_character_)
  )
  fill_labels_collapsed <- fill_inhabitant_labels |>
    filter(!is.na(Inhabitant)) |>
    group_by(Fill_Type) |>
    summarise(Inhabitant = paste(Inhabitant, collapse = "\n"), .groups = "drop")

  label_positions <- fill_2023 |>
    group_by(Week) |>
    arrange(Week, Fill_Type) |>
    mutate(ymax = cumsum(Number_of_Tubes_First_Filled),
           ymin = ymax - Number_of_Tubes_First_Filled,
           ymid = (ymax + ymin) / 2) |>
    ungroup() |>
    left_join(fill_labels_collapsed, by = "Fill_Type") |>
    filter(Number_of_Tubes_First_Filled >= 2, !is.na(Inhabitant))

  p1 <- ggplot(fill_2023, aes(fill = Fill_Type, y = Number_of_Tubes_First_Filled, x = Week)) +
    geom_bar(position = "stack", stat = "identity",
             col = "grey50", linewidth = 0.5) +
    geom_text(data = label_positions,
              aes(x = Week, y = ymid, label = Inhabitant),
              inherit.aes = FALSE, size = 2.6, fontface = "italic",
              color = "black", check_overlap = TRUE) +
    scale_fill_manual(values = my_colors, labels = my_labels) +
    scale_x_continuous(breaks = 1:20) +
    scale_y_continuous(breaks = seq(0, 27, 1)) +
    labs(title = "Figure 1a. Date of First Fill in 2023 by Type",
         x = "Week", y = "Number of Tubes First Filled", fill = "Fill Type") +
    my_theme()

  # -- P2: Proportion of Fill Types by Location (2025 clean) ----
  p2_base <- ggplot(fill_by_location_2025_clean,
                    aes(fill = Fill_Type, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity",
             col = "grey50", linewidth = 0.5) +
    scale_fill_manual(values = my_colors, labels = my_labels) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(title = "Proportion of Fill Types by Location (2025–2026 Clean)",
         x = "Location", y = "Proportion", fill = "Fill Type") +
    my_theme() +
    coord_cartesian(ylim = c(-0.08, 1), clip = "off") +
    theme(plot.margin = margin(5, 5, 50, 5))

  for (i in seq_len(nrow(sample_sizes_filltype))) {
    p2_base <- p2_base +
      annotate("text",
               x    = sample_sizes_filltype$Location[i],
               y    = -0.04,
               label = sample_sizes_filltype$Sample_Size[i],
               vjust = 1.5, size = 3.4)
  }

  # -- P3: Proportion of Inhabitant Types (2025 Bee Occupants) ---
  p3 <- ggplot(ID_freq_2025_clean,
               aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity",
             col = "grey50", linewidth = 0.5) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(title = "Proportion of Inhabitant Types by Location (2025)",
         x = "Location", y = "Proportion", fill = "Inhabitant") +
    my_theme()

  # -- P4: Species-level Composition ------------------------
  p4_base <- ggplot(ID_freq_comp,
                    aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity",
             col = "grey50", linewidth = 0.5) +
    scale_fill_manual(values = species_colors, breaks = names(species_colors)) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(title = "Proportion of Inhabitant Types by Location (Species-level)",
         x = "Location", y = "Proportion", fill = "Inhabitant") +
    my_theme() +
    coord_cartesian(ylim = c(-0.08, 1), clip = "off") +
    theme(plot.margin = margin(5, 5, 50, 5))

  for (i in seq_len(nrow(sample_sizes_species))) {
    p4_base <- p4_base +
      annotate("text",
               x    = sample_sizes_species$Location[i],
               y    = -0.04,
               label = sample_sizes_species$Sample_Size[i],
               vjust = 1.5, size = 3.4)
  }

  # -- P5: Cumulative First Fill Across Years ---------------
  cum_sum_by_year_week <- full_cum_clean |>
    group_by(Year, Week) |>
    summarise(total = sum(Number_of_Tubes_First_Filled, na.rm = TRUE), .groups = "drop") |>
    group_by(Year) |>
    mutate(cum_total = cumsum(total))

  p5 <- ggplot(cum_sum_by_year_week,
               aes(x = Week, y = cum_total, color = factor(Year), group = factor(Year))) +
    geom_line(linewidth = 1.1) +
    geom_point(size = 2.2) +
    scale_color_manual(values = c("2023" = "#404788FF", "2024" = "#1F968BFF",
                                  "2025" = "#FDE725FF"),
                       name = "Year") +
    scale_x_continuous(breaks = 1:20) +
    labs(title = "Cumulative First Fills by Week Across Years",
         x = "Week of Season", y = "Cumulative Tubes First Filled") +
    my_theme()

  # -- P6: Actual & Presumed ID by Location -----------------
  p6 <- ggplot(ID_freq, aes(fill = Inhabitant, y = Proportion_of_Fill, x = Location)) +
    geom_bar(position = "fill", stat = "identity",
             col = "grey50", linewidth = 0.5) +
    scale_fill_manual(
      values = c("navy","lightyellow","orange","lightgrey","brown","darkgreen",
                 "pink","lightblue","black","lightgreen","yellow","red",
                 "purple","blue","darkgreen","limegreen","darkgrey","hotpink")) +
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    labs(title = "Figure 2: Proportion of Inhabitant Types by Location (Actual & Presumed)",
         x = "Location", y = "Proportion", fill = "Inhabitant") +
    my_theme()

  list(
    "Figure 1a – Date of First Fill in 2023 by Type"              = p1,
    "Proportion of Fill Types by Location (2025–2026 Clean)"       = p2_base,
    "Proportion of Inhabitant Types by Location (2025)"            = p3,
    "Proportion of Inhabitant Types – Species-level"               = p4_base,
    "Cumulative First Fills by Week Across Years"                  = p5,
    "Figure 2 – Inhabitant Types by Location (Actual & Presumed)"  = p6
  )
}

plots_list <- make_plots()
plot_names  <- names(plots_list)

# =========================================================
# CSS
# =========================================================
app_css <- paste0("
  body, .content-wrapper, .right-side { background-color: ", BG, " !important; }
  .skin-blue .main-header .logo { background-color: #8B7355; }
  .skin-blue .main-header .navbar { background-color: #8B7355; }
  .sidebar { background-color: #e8dcc8 !important; }
  .sidebar-menu li a { color: #3d2b1f !important; }

  /* Cover page */
  #cover-wrap {
    display: flex; flex-direction: column; align-items: center;
    justify-content: center; min-height: 88vh; text-align: center;
    padding: 30px;
  }
  #cover-title {
    font-family: 'Times New Roman', serif;
    font-size: 2.6em; font-weight: bold;
    color: #3d2b1f; margin-bottom: 20px;
    border-bottom: 2px solid grey50;
  }
  #cover-subtitle {
    font-family: 'Times New Roman', serif;
    font-size: 1.15em; color: #5a4030;
    max-width: 720px; line-height: 1.7;
    background: rgba(139,115,85,0.08);
    border: 1px solid rgba(139,115,85,0.3);
    border-radius: 8px; padding: 18px 28px;
    margin-top: 10px; margin-bottom: 30px;
    text-align: left;
  }
  .cover-btn {
    margin: 0 10px;
    font-family: 'Times New Roman', serif;
    font-size: 1em;
    min-width: 130px;
  }
  /* Page 2 story header */
  .story-header {
    font-family: 'Times New Roman', serif;
    font-size: 1.9em; font-weight: bold;
    color: #3d2b1f; padding: 12px 0 6px 0;
    border-bottom: 1px solid grey50;
    margin-bottom: 14px;
  }
  /* Bottom nav buttons */
  .nav-bar {
    display: flex; justify-content: center; gap: 14px;
    margin-top: 20px; padding-bottom: 16px;
  }
  .nav-btn { min-width: 130px; }
  /* Page 3 */
  .interactive-note {
    font-family: 'Times New Roman', serif; font-size: 0.92em;
    color: #5a4030; font-style: italic; margin-bottom: 10px;
  }
  /* Sidebar */
  .sidebar-select label {
    font-family: 'Times New Roman', serif; font-weight: bold;
    color: #3d2b1f;
  }
  /* General well */
  .well { background-color: #ede5d0; border-color: grey50; }
")

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  tags$head(tags$style(HTML(app_css))),

  # ---- hidden page tracker ----
  tags$div(
    uiOutput("page_ui"),
    style = "min-height: 100vh;"
  )
)

# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {

  # Current page reactive (1 = Cover, 2 = Story, 3 = Interactive)
  page <- reactiveVal(1)

  # ── Navigation observers ──────────────────────────────
  observe({
    lapply(c("go_next_cover","go_next_story","go_next_int"), function(id) {
      observeEvent(input[[id]], { page(min(page() + 1, 3)) }, ignoreInit = TRUE)
    })
    lapply(c("go_prev_story","go_prev_int"), function(id) {
      observeEvent(input[[id]], { page(max(page() - 1, 1)) }, ignoreInit = TRUE)
    })
    lapply(c("exit_cover","exit_story","exit_int"), function(id) {
      observeEvent(input[[id]], { stopApp() }, ignoreInit = TRUE)
    })
  })

  # ── Master UI renderer ────────────────────────────────
  output$page_ui <- renderUI({
    switch(as.character(page()),
      "1" = page1_ui(),
      "2" = page2_ui(),
      "3" = page3_ui()
    )
  })

  # =======================================================
  # PAGE 1 – COVER
  # =======================================================
  page1_ui <- function() {
    div(id = "cover-wrap",
      div(id = "cover-title",
          "Cavity-Nesting Bee & Wasp Community Study",
          tags$br(),
          tags$span(style = "font-size:0.55em; font-weight:normal; color:#7a5c40;",
                    "Monitoring Tube Occupancy Across Campus Habitats, 2023–2026")
      ),

      div(style = "margin: 18px 0;",
          actionButton("exit_cover",   "Exit App",  class = "btn btn-danger  cover-btn"),
          actionButton("go_next_cover","Next Page →", class = "btn btn-success cover-btn")
      ),

      div(id = "cover-subtitle",
          tags$b("Project Background"), tags$br(), tags$br(),
          "This project investigates the nesting behavior, habitat preferences, and seasonal
          phenology of cavity-nesting bees and wasps at SUNY Geneseo and surrounding sites.
          Hollow reed tubes were placed in boxes at five distinct campus locations — the Arboretum, Bee Barn,
          Meadow, No Mow Zone, and E-Garden, as well as in boxes at a nearby nature preserve (Island Preserve). The boxes were checked 
          throughout the spring and summer seasons (2023–2026).", tags$br(), tags$br(),
          "Collected tubes were dissected to identify occupants preliminarily  based on the fill material used
          to close each cell. Fill types such as grass, intact leaves, masticated leaves, and light mud are indicative of the families of the occupant. 
          After these insects emerged, they were identified to the species level using online taxonomic keys.
          Three years of data allow us to compare seasonal timing (week of first fill), fill-type
          composition, and occupant community structure across habitats and years."
      )
    )
  }

  # =======================================================
  # PAGE 2 – STORY-TELLING GRAPHS
  # =======================================================
  page2_ui <- function() {
    fluidPage(
      style = paste0("background-color:", BG, "; min-height: 100vh;"),

      div(class = "story-header", "Story-telling Graphs"),

      sidebarLayout(
        sidebarPanel(
          width = 3,
          style = "background-color: #e8dcc8; border: 1px solid grey50; border-radius:6px;",

          div(class = "sidebar-select",
              selectInput("plot_choice",
                          label   = "Select a Graph:",
                          choices = plot_names,
                          width   = "100%")
          ),
          tags$hr(style = "border-color: grey50;"),
          tags$p(style = "font-family:'Times New Roman'; font-size:0.88em; color:#5a4030;",
                 "Each graph tells part of the story of tube occupancy across habitats and seasons.
                  Use the dropdown above to navigate between plots."),

          # ---- optional axis/color toggle ----
          checkboxInput("show_grid", "Show major grid lines", value = TRUE),
          # radioButtons("bar_outline", "Bar outline color:",
          #              choices  = c("Grey50" = "grey50", "Black" = "black", "None" = NA),
          #              selected = "grey50")
        ),

        mainPanel(
          width = 9,
          plotOutput("story_plot", height = "560px")
        )
      ),

      # Bottom nav
      div(class = "nav-bar",
          actionButton("go_prev_story", "← Prev Page",  class = "btn btn-secondary nav-btn"),
          actionButton("exit_story",    "Exit App",      class = "btn btn-danger    nav-btn"),
          actionButton("go_next_story", "Next Page →",   class = "btn btn-success   nav-btn")
      )
    )
  }

  # Render the selected story plot
  output$story_plot <- renderPlot({
    req(input$plot_choice)
    p <- plots_list[[input$plot_choice]]

    # Apply optional user tweaks
    if (!input$show_grid) {
      p <- p + theme(panel.grid.major.y = element_blank())
    }
    p
  }, bg = BG)

  # =======================================================
  # PAGE 3 – INTERACTIVE PLOTS
  # =======================================================
  page3_ui <- function() {
    fluidPage(
      style = paste0("background-color:", BG, "; min-height: 100vh;"),

      tags$h2("Interactive Plots",
              style = "font-family:'Times New Roman'; font-weight:bold;
                       color:#3d2b1f; border-bottom:1px solid grey50;
                       padding-bottom:6px; margin-bottom:14px;"),

      tabsetPanel(id = "int_tabs", type = "tabs",

        # TAB 1: Cumulative fill trajectory ----------------
        tabPanel("Seasonal Trajectory",
          br(),
          div(class = "interactive-note",
              "Hover over points to see exact counts. Click a year in the legend to show/hide it."),
          fluidRow(
            column(3,
              wellPanel(
                checkboxGroupInput("sel_years", "Show Years:",
                                   choices  = c("2023","2024","2025"),
                                   selected = c("2023","2024","2025")),
                checkboxGroupInput("sel_filltypes_cum",
                                   "Filter Fill Types:",
                                   choices  = c("Grass","Leaves","Light_Mud",
                                                "Masticated_Leaves","Empty"),
                                   selected = c("Grass","Leaves","Light_Mud",
                                                "Masticated_Leaves","Empty"))
              )
            ),
            column(9, plotlyOutput("cum_plotly", height = "500px"))
          )
        ),

        # TAB 2: Weekly fill by fill-type bar (animated) ---
        tabPanel("Weekly Fill Breakdown",
          br(),
          div(class = "interactive-note",
              "Drag the slider to move through the season week by week.
               Hover bars for exact counts."),
          fluidRow(
            column(12,
              sliderInput("week_slider", "Select Week:",
                          min = 1, max = 20, value = 10, step = 1,
                          width = "70%")
            )
          ),
          fluidRow(
            column(12, plotlyOutput("weekly_bar_plotly", height = "420px"))
          )
        ),

        # TAB 3: Habitat bubble / scatter -------------------
        tabPanel("Habitat Comparison",
          br(),
          div(class = "interactive-note",
              "Each bubble represents a location × fill-type combination.
               Bubble size = proportion of fill. Click legend items to isolate fill types."),
          fluidRow(
            column(12, plotlyOutput("habitat_bubble_plotly", height = "480px"))
          )
        ),

        # TAB 4: Sankey — fill type → occupant -------------
        tabPanel("Fill → Occupant Flow",
          br(),
          div(class = "interactive-note",
              "Hover nodes and flows to explore how fill types link to likely occupants."),
          fluidRow(
            column(12, plotlyOutput("sankey_plotly", height = "480px"))
          )
        )
      ),

      div(class = "nav-bar",
          actionButton("go_prev_int", "← Prev Page", class = "btn btn-secondary nav-btn"),
          actionButton("exit_int",    "Exit App",     class = "btn btn-danger    nav-btn")
      )
    )
  }

  # ---- Interactive plot: Cumulative trajectory -----------
  output$cum_plotly <- renderPlotly({
    req(input$sel_years, input$sel_filltypes_cum)

    df <- full_cum_clean |>
      filter(Year %in% as.integer(input$sel_years),
             Fill_Type %in% input$sel_filltypes_cum) |>
      group_by(Year, Week) |>
      summarise(total = sum(Number_of_Tubes_First_Filled, na.rm = TRUE), .groups = "drop") |>
      group_by(Year) |>
      mutate(cum_total = cumsum(total))

    year_pal <- c("2023" = "#404788FF", "2024" = "#1F968BFF", "2025" = "#E6A817")

    p <- plot_ly(df, x = ~Week, y = ~cum_total, color = ~factor(Year),
                 colors = year_pal,
                 type = "scatter", mode = "lines+markers",
                 hovertemplate = "Week %{x}<br>Cumulative: %{y}<extra>%{fullData.name}</extra>",
                 line = list(width = 2.5), marker = list(size = 7)) |>
      layout(
        title  = list(text = "Cumulative First Fills by Week",
                      font = list(family = "Times New Roman", size = 18, color = "#3d2b1f")),
        xaxis  = list(title = "Week of Season", tickmode = "linear", dtick = 1,
                      gridcolor = "rgba(0,0,0,0.08)"),
        yaxis  = list(title = "Cumulative Tubes First Filled",
                      gridcolor = "rgba(0,0,0,0.08)"),
        paper_bgcolor = BG, plot_bgcolor = BG,
        legend = list(title = list(text = "<b>Year</b>"),
                      font = list(family = "Times New Roman"))
      )
    p
  })

  # ---- Interactive plot: Weekly breakdown ----------------
  output$weekly_bar_plotly <- renderPlotly({
    req(input$week_slider)
    
    my_labels <- c(
      "Grass"             = "Grass",
      "Leaves"            = "Leaves",
      "Intact_Leaves"     = "Intact Leaves",
      "Masticated_Leaves" = "Masticated Leaves",
      "Light_Mud"         = "Light Mud",
      "Debris"            = "Debris",
      "Silk"              = "Spider Silk",
      "Empty"             = "Empty"
    )
    

    df <- combined_df |>
      filter(Week == input$week_slider) |>
      group_by(Year, Fill_Type) |>
      summarise(n = sum(Number_of_Tubes_First_Filled, na.rm = TRUE), .groups = "drop") |>
      mutate(
        Year = factor(Year),
        Clean_Fill = my_labels[Fill_Type]
      )

    # fill_pal <- c(
    #   "Grass"             = "darkgreen",
    #   "Leaves"            = "#95D840FF",
    #   "Intact_Leaves"     = "#29AF7FFF",
    #   "Masticated_Leaves" = "#FDE725FF",
    #   "Light_Mud"         = "#33638DFF",
    #   "Debris"            = "grey70",
    #   "Silk"              = "#cccccc",
    #   "Empty"             = "#444444"
    # )
    fill_pal <- c(
      "Grass"             = "darkgreen",
      "Leaves"            = "#95D840FF",
      "Intact Leaves"     = "#29AF7FFF",
      "Masticated Leaves" = "#FDE725FF",
      "Light Mud"         = "#33638DFF",
      "Debris"            = "grey70",
      "Spider Silk"       = "#aaaaaa",
      "Empty"             = "#444444"
    )

    plot_ly(df, x = ~Year, y = ~n, color = ~Clean_Fill, colors = fill_pal,
            type = "bar",
            hovertemplate = "<b>%{fullData.name}</b><br>Year: %{x}<br>Tubes: %{y}<extra></extra>") |>
      layout(
        barmode = "stack",
        title   = list(text = paste("First Fill Counts – Week", input$week_slider),
                       font = list(family = "Times New Roman", size = 17, color = "#3d2b1f")),
        xaxis   = list(title = "Year"),
        yaxis   = list(title = "Number of Tubes First Filled",
                       gridcolor = "rgba(0,0,0,0.08)"),
        paper_bgcolor = BG, plot_bgcolor = BG,
        legend = list(title = list(text = "<b>Fill Type</b>"),
                      font = list(family = "Times New Roman"))
      )
  })

  # ---- Interactive plot: Habitat bubble chart ------------
  output$habitat_bubble_plotly <- renderPlotly({

    my_labels <- c(
      "Grass"             = "Grass",
      "Leaves"            = "Leaves",
      "Intact_Leaves"     = "Intact Leaves",
      "Masticated_Leaves" = "Masticated Leaves",
      "Light_Mud"         = "Light Mud",
      "Debris"            = "Debris",
      "Silk"              = "Spider Silk",
      "Empty"             = "Empty"
    )
    
    df <- fill_by_location_2025_clean |>
      filter(!is.na(Proportion_of_Fill), Proportion_of_Fill > 0) %>% 
      mutate(Clean_Fill = my_labels[Fill_Type])

    # fill_pal <- c(
    #   "Grass"             = "darkgreen",
    #   "Leaves"            = "#95D840FF",
    #   "Intact_Leaves"     = "#29AF7FFF",
    #   "Masticated_Leaves" = "#FDE725FF",
    #   "Light_Mud"         = "#33638DFF",
    #   "Debris"            = "grey70",
    #   "Silk"              = "#aaaaaa",
    #   "Empty"             = "#444444"
    # )
    fill_pal <- c(
      "Grass"             = "darkgreen",
      "Leaves"            = "#95D840FF",
      "Intact Leaves"     = "#29AF7FFF",
      "Masticated Leaves" = "#FDE725FF",
      "Light Mud"         = "#33638DFF",
      "Debris"            = "grey70",
      "Spider Silk"       = "#aaaaaa",
      "Empty"             = "#444444"
    )

    plot_ly(df,
            x    = ~Location,
            y    = ~Fill_Type,
            size = ~Proportion_of_Fill,
            color= ~Clean_Fill,
            colors = fill_pal,
            type = "scatter", mode = "markers",
            marker = list(opacity = 0.78, sizemode = "diameter", sizeref = 1.35),
            hovertemplate = "<b>%{x}</b><br>Fill: %{y}<br>Proportion: %{marker.size:.3f}<extra></extra>") |>
      layout(
        title  = list(text = "Fill Type Proportions by Habitat Location (2025–2026)",
                      font = list(family = "Times New Roman", size = 17, color = "#3d2b1f")),
        xaxis  = list(title = "Location", gridcolor = "rgba(0,0,0,0.08)"),
        yaxis  = list(title = "Fill Type", gridcolor = "rgba(0,0,0,0.08)"),
        paper_bgcolor = BG, plot_bgcolor = BG,
        showlegend = TRUE,
        legend = list(title = list(text = "<b>Fill Type</b>"),
                      font = list(family = "Times New Roman"))
      )
  })

  # ---- Interactive plot: Sankey fill → occupant ----------
  output$sankey_plotly <- renderPlotly({

    # Nodes: fill types on the left, occupants on the right
    fill_nodes <- c("Grass","Masticated Leaves","Intact Leaves",
                    "Light Mud","Debris","Silk","Empty")
    occ_nodes  <- c("Isodontia mexicana (Grass Wasp)",
                    "Megachile spp. (Leafcutter – leaf cell)",
                    "Megachile spp. (Leafcutter – masticated)",
                    "Osmia / Mason Bee",
                    "Ancistrocerus / Potter Wasp (mud)",
                    "Euodynerus (mud)",
                    "Parasitoids / Chrysis",
                    "Spider",
                    "Unknown / Other")

    all_nodes <- c(fill_nodes, occ_nodes)
    idx <- function(n) match(n, all_nodes) - 1   # 0-indexed

    # Source → Target links with approximate values
    src <- c(
      idx("Grass"),            idx("Grass"),
      idx("Masticated Leaves"),idx("Masticated Leaves"),
      idx("Intact Leaves"),    idx("Intact Leaves"),
      idx("Light Mud"),        idx("Light Mud"),
      idx("Debris"),           idx("Silk"),
      idx("Empty")
    )
    tgt <- c(
      idx("Isodontia mexicana (Grass Wasp)"),
      idx("Unknown / Other"),
      idx("Megachile spp. (Leafcutter – masticated)"),
      idx("Osmia / Mason Bee"),
      idx("Megachile spp. (Leafcutter – leaf cell)"),
      idx("Parasitoids / Chrysis"),
      idx("Ancistrocerus / Potter Wasp (mud)"),
      idx("Euodynerus (mud)"),
      idx("Parasitoids / Chrysis"),
      idx("Spider"),
      idx("Unknown / Other")
    )
    val <- c(45, 5, 55, 15, 40, 8, 30, 20, 10, 12, 30)

    node_colors <- c(
      "darkgreen","#FDE725FF","#95D840FF","#33638DFF","grey60","#dddddd","#333333",
      "#1F968BFF","#FDE725FF","#55C667FF","#29AF7FFF","#287D8EFF","#33638DFF",
      "#480154FF","#888888","#aaaaaa"
    )

    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        pad   = 18, thickness = 22,
        line  = list(color = "grey50", width = 0.5),
        label = all_nodes,
        color = node_colors
      ),
      link = list(
        source = src, target = tgt, value = val,
        color  = rep("rgba(160,140,110,0.35)", length(val))
      )
    ) |>
      layout(
        title      = list(text = "Fill Type → Likely Occupant Flow",
                          font = list(family = "Times New Roman", size = 17, color = "#3d2b1f")),
        font       = list(family = "Times New Roman", size = 12),
        paper_bgcolor = BG
      )
  })
}

# =========================================================
# RUN
# =========================================================
shinyApp(ui = ui, server = server)
