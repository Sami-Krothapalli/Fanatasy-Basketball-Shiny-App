# =============================
# LIBRARY"S
# =============================

library(shiny)            # Load Shiny framework
library(dplyr)            # For data manipulation
library(ggplot2)          # For static plotting
library(scales)           # For scaling functions
library(tibble)           # To create tibbles (enhanced data frames)
library(ggradar)          # For radar plot visualization
library(RColorBrewer)     # For color palettes
library(reactable)        # For interactive tables
library(htmltools)        # HTML rendering helpers
library(gt)               # For pretty tables
library(bslib)            # Bootstrap theming
library(plotly)           # For interactive plots
library(viridis)          # Color palettes
library(shinyBS)          # Bootstrap-styled UI enhancements
library(png)              # For reading PNG files
library(stringr)   # For reversing player names




# =============================
# LOADING & CLEANING DATA
# =============================

# Main Dataset
df <- read.delim("NBA_02122025_Traditional.tsv", sep = "\t", 
                 fileEncoding = "latin1", nrows = 100)

# Load advanced statistics (this so I can join player positions)
pos_df <- read.csv("Advanced.csv")

## Clean Whitespace
df$Player <- trimws(df$Player)
pos_df$player <- trimws(pos_df$player)

# Use Left Join to merge position information into main dataframe
library(dplyr) 
df <- df %>%
  left_join(pos_df %>% select(player, pos), by = c("Player" = "player"))

# No repeat players and only rimary position players
df <- df %>% distinct(Player, Team, GP, Min, PTS, .keep_all = TRUE)

## MANUALLY CHANGE NA POSITION VALUES
df$pos[df$Player == "Nikola Jokic"] <- "C"
df$pos[df$Player == "Luka Doncic"] <- "PG"
df$pos[df$Player == "Nikola Vucevic"] <- "C"
df$pos[df$Player == "Alperen Sengun"] <- "C"
df$pos[df$Player == "Jimmy Butler III"] <- "SF"
df$pos[df$Player == "Kristaps Porzingis"] <- "C"

# Check how many missing values still exist in position column
table(is.na(df$pos))

# =============================
# RENAMING
# =============================

library(dplyr) 

# Remove rows with missing values
df <- na.omit(df)                      

# Rename columns
df <- df %>%
  rename(
    Player = Player,
    Team = Team,
    GamesPlayed = GP,
    Wins = W,
    Losses = L,
    Minutes = Min,
    Points = PTS,
    FieldGoalsMade = FGM,
    FieldGoalsAttempted = FGA,
    ThreePointersMade = X3PM,
    FreeThrowsMade = FTM,
    Rebounds = REB,
    Assists = AST,
    Turnovers = TOV,
    Steals = STL,
    Blocks = BLK,
    DoubleDoubles = DD2,
    TripleDoubles = TD3,
    FantasyPoints = FP,
    Position = pos  
  )


# =============================
# DATA PREP
# =============================


# --- RADAR PLOT DATA PREPARATION ---

# Summarize position-level averages to build radar plots
position_stats <- df %>%
  group_by(Position) %>%
  summarise(
    `Field Goal %` = mean(FG., na.rm = TRUE),
    Rebounds = mean(Rebounds, na.rm = TRUE),
    Assists = mean(Assists, na.rm = TRUE),
    Steals = mean(Steals, na.rm = TRUE),
    Blocks = mean(Blocks, na.rm = TRUE),
    .groups = "drop"  # Drop group metadata after summarizing
  )

# Rescale all numeric stats between 0 and 1
position_scaled <- position_stats %>%
  mutate(across(-Position, rescale)) %>%
  rename(group = Position)  

# Lookup table to map position codes -> full names
position_labels <- c(
  PG = "Point Guard", 
  SG = "Shooting Guard", 
  SF = "Small Forward",
  PF = "Power Forward", 
  C = "Center"
)

# --- TABLE DATA PREPARATION ---

# Making a new dataset that adds the Field Goal % and Free Throw %
nba_df <- df %>%
  mutate(
    # Calculate Field Goal Percentage (FG%) if the player has attempted field goals
    `FG%` = ifelse(
      FieldGoalsAttempted > 0, FieldGoalsMade / FieldGoalsAttempted, NA_real_
    ),
    
    # Calculate Free Throw Percentage (FT%) if the player has attempted free throws
    `FT%` = ifelse(FTA > 0, FreeThrowsMade / FTA, NA_real_)
  ) %>%
  # Remove any players with missing FG% or FT%
  filter(!is.na(`FG%`) & !is.na(`FT%`)) %>%
  # Keep only the relevant columns for the reactable table
  select(Player, Team, Position, FantasyPoints, Points, Rebounds, 
         Steals, Blocks, Turnovers, `FG%`, `FT%`)

# --- ARROW INDICATOR FUNCTION FOR TABLE ---

# Function to create colored arrow indicators comparing player stats to league average
arrow_above_below_avg <- function(column_values, format_fun, tolerance = 0.02) {
  avg <- mean(column_values, na.rm = TRUE)  # Calculate league average for the stat
  
  # nested function that formats individual player's value
  function(value) {
    diff <- value - avg
    arrow <- if (diff > tolerance * abs(avg)) {
      # Green upward arrow if significantly above average
      tags$span(style = "color: green; font-size: 18px;", "↑")
    } else if (diff < -tolerance * abs(avg)) {
      # Red downward arrow if significantly below average
      tags$span(style = "color: red; font-size: 18px;", "↓")
    } else {
      # Gray equals sign if approximately average
      tags$span(style = "color: gray; font-size: 18px;", "=")
    }
    # Return arrow with formatted value
    tags$span(style = "white-space: nowrap;", arrow, " ", format_fun(value))
  }
}

# --- LOADING DATA FOR COMPOSITE GRAPH ---

# Load raw NBA statistics again for the composite graph
df1 <- read.delim("NBA_02122025_Traditional.tsv", sep = "\t", 
                  fileEncoding = "latin1", nrows = 100)

# Load position data
pos_df1 <- read.csv("Advanced.csv")

## Clean whitespace in both player name columns
df1$Player <- trimws(df1$Player)
pos_df1$player <- trimws(pos_df1$player)

# Merge position data into the stats data
library(dplyr)
df1 <- df1 %>%
  left_join(pos_df1 %>% select(player, pos), by = c("Player" = "player"))

# Keep only primary position players
df1 <- df1 %>% distinct(Player, Team, GP, Min, PTS, .keep_all = TRUE)

## Manually fix NA position values for important players
df1$pos[df1$Player == "Nikola Jokic"] <- "C"
df1$pos[df1$Player == "Luka Doncic"] <- "PG"
df1$pos[df1$Player == "Nikola Vucevic"] <- "C"
df1$pos[df1$Player == "Alperen Sengun"] <- "C"
df1$pos[df1$Player == "Jimmy Butler III"] <- "SF"
df1$pos[df1$Player == "Kristaps Porzingis"] <- "C"

# View basic checks (optional)
head(df1, n = 20)  
table(is.na(df1$pos))

# Remove any rows with missing values
df1 <- na.omit(df1)

# Rename columns cleanly for composite score calculation
df1 <- df1 %>%
  rename(
    Player = Player,
    Team = Team,
    GamesPlayed = GP,
    Wins = W,
    Losses = L,
    Minutes = Min,
    Points = PTS,
    FieldGoalsMade = FGM,
    FieldGoalsAttempted = FGA,
    ThreePointersMade = X3PM,
    FreeThrowsMade = FTM,
    Rebounds = REB,
    Assists = AST,
    Turnovers = TOV,
    Steals = STL,
    Blocks = BLK,
    DoubleDoubles = DD2,
    TripleDoubles = TD3,
    FantasyPoints = FP,
    Position = pos
  )

#--- TIERLIST FUNCTIONS-----
tierlist = function(df, tiernames=c("S", "A", "B", "C", "D", "F"),
                    tiercols = tier_colors <- c(
                      "S" = "#6cbf6c",  # Soft Green for S Tier (Best)
                      "A" = "#f2c777",  # Warm Yellow for A Tier
                      "B" = "#77c2f2",  # Light Blue for B Tier
                      "C" = "#f4a261",  # Soft Orange for C Tier
                      "D" = "#e76f51",  # Light Red for D Tier
                      "F" = "#b0b0b0"   # Muted Gray for F Tier (Worst)
                    ),
                    main="Tier List",
                    width=10) {
  
  # Draw grayish-black background
  par(mar=c(0, 0, 0, 0))
  plot.new()
  plot.window(xlim=c(0, width), ylim=c(0, length(tiernames) + 1))
  rect(0, 0, width, length(tiernames) + 1, col="gray20", border=NA)
  
  # setup main title
  title_top_offset = 0.5
  title_font_size <- 1.5  # Adjust font size as needed
  title_text_height <- strheight(main, cex = title_font_size)
  
  # draw the title 
  text(x=5, y=length(tiernames) + 1 - title_top_offset, 
       labels=main, col="gray80", cex=title_font_size, font=2)
  
  # num of tiers
  Ntiers = length(tiernames)
  
  # set up tiers 
  tier_upper_bound = y=length(tiernames) + 1 - title_top_offset - title_text_height - 0.5
  tier_lower_bound = 0.01
  tier_band_widths = (tier_upper_bound - tier_lower_bound)/Ntiers
  
  band_tops = seq(tier_upper_bound, tier_lower_bound, by=-tier_band_widths)
  band_tops_inner <- band_tops[2:(length(band_tops)-1)]
  
  band_seperator_width = 0.05
  
  
  # draw each tier label
  tier_label_width <- 1.25
  for (k in 1:(length(band_tops)-1)) {
    # draw tier rectangle background
    rect(0, band_tops[k], tier_label_width, band_tops[k+1],
         col=tiercols[k], border=NA)
    # add tier label 
    text(x=tier_label_width*0.4, y=mean(c(band_tops[k], band_tops[k + 1])), 
         labels=tiernames[k], col="gray90", font=1.5, cex=1.2, adj=0)
  }
  
  
  space_btw_boxes = 0.125
  b_off = t_off = band_seperator_width/2 + 0.06
  
  # draw boxes for each tier 
  for (k in 1:Ntiers) {
    this_df = subset(df, tier == tiernames[k])
    Nboxes = nrow(this_df)
    if (Nboxes > 0) {
      # box spacing rules
      box_bounds_left = seq(tier_label_width, width-0.5, by=1.5) 
      box_bound_top_bot = c(band_tops[k + 1] + b_off, band_tops[k] - t_off)
      
      cols = Nboxes
      if (Nboxes > length(box_bounds_left)-1) {
        cols = length(box_bounds_left)-1
      }
      
      for (j in 1:cols) {
        # draw text border box
        rect(box_bounds_left[j] + space_btw_boxes, box_bound_top_bot[1], 
             box_bounds_left[j + 1], box_bound_top_bot[2], 
             border=adjustcolor(tiercols[k], alpha.f = 0.8)
        )
        
        box_width = box_bounds_left[j + 1] - box_bounds_left[j] - space_btw_boxes
        
        # Read the image file
        img = readPNG(this_df$img[j])
        image_width = box_width/2
        img_center_x = box_bounds_left[j] + box_width/4 
        
        rasterImage(img, 
                    xleft=img_center_x-image_width/2 + 0.2, 
                    xright=img_center_x+image_width/2, 
                    ybottom=box_bound_top_bot[1]+b_off, ytop=box_bound_top_bot[2]-t_off)
        
        # wrap text if too large to fit in box
        label=strwrap(this_df$text[j], width=box_width/2)
        
        line_height <- strheight(label[1])
        line_gap = 0.05
        center_y=mean(c(box_bound_top_bot[1], box_bound_top_bot[2]))
        median = (length(label)+1)/2
        
        # draw wrapped text
        for(i in 1:length(label)){
          if (strwidth(label[i]) > box_width/2) {
            label[i] <- paste(substr(label[i], 1, 3), ".", sep="")
          }
          text(x=mean(c(box_bounds_left[j] + space_btw_boxes, box_bounds_left[j + 1])), 
               y=center_y + (i-median)*(line_height+line_gap), 
               labels=label[i],
               col=tiercols[k], cex=0.8,
               adj = c(0, 0.5)
          )
        }
      }
    }
  }
  
  # draw band separators
  for (k in 1:length(band_tops_inner)) {
    rect(0, band_tops_inner[k] - band_seperator_width/2, width, band_tops_inner[k] + band_seperator_width/2,
         col="gray10", border=NA)
  }
  
}


# Prepare the composite scores
composite_score <- df1 %>%
  mutate(
    # Create Scoring_Score based on Points, Field Goal Efficiency, 
    # and Three-Pointers
    Scoring_Score = (Points / max(Points)) + 
      (FieldGoalsMade / FieldGoalsAttempted) / max(FieldGoalsMade / FieldGoalsAttempted) + 
      (ThreePointersMade / max(ThreePointersMade)),
    # Create Playmaking_Score by rewarding assists and penalizing turnovers
    Playmaking_Score = (Assists / max(Assists)) - (Turnovers / max(Turnovers)),
    # Create Rebounding_Score based on total rebounds
    Rebounding_Score = Rebounds / max(Rebounds),
    # Create Defense_Score based on steals and blocks
    Defense_Score = (Steals / max(Steals)) + (Blocks / max(Blocks)),
    # Create Impact_Score based on player wins and fantasy points
    Impact_Score = (Wins / max(Wins)) + (FantasyPoints / max(FantasyPoints))
  ) %>%
  # Combine all components into a final Composite_Score with 
  # weighted contributions
  mutate(
    Composite_Score = (0.3 * Scoring_Score) + 
      (0.2 * Playmaking_Score) + 
      (0.2 * Rebounding_Score) + 
      (0.2 * Defense_Score) + 
      (0.1 * Impact_Score)
  ) %>%
  
  select(Player, Team, Composite_Score) %>%
  # Assign each player into a tier (S, A, B, C, D, F) based on 
  #Composite Score percentiles
  mutate(
    Tier = case_when(
      Composite_Score >= quantile(Composite_Score, 0.95, na.rm = TRUE) ~ "S",
      Composite_Score >= quantile(Composite_Score, 0.80, na.rm = TRUE) ~ "A",
      Composite_Score >= quantile(Composite_Score, 0.55, na.rm = TRUE) ~ "B",
      Composite_Score >= quantile(Composite_Score, 0.30, na.rm = TRUE) ~ "C",
      Composite_Score >= quantile(Composite_Score, 0.15, na.rm = TRUE) ~ "D",
      TRUE ~ "F"
    )
  )

# =============================
# USER INTERFACE
# =============================

ui <- fluidPage(
  
  # Set global theme for the app using Bootstrap 5 and Lux theme
  theme = bs_theme(
    version = 5,
    bootswatch = "lux",
    primary = "#FFA500",  # Main color accent
    base_font = font_google("Poppins")  # Set font from Google Fonts
  ),
  
  # Custom CSS to tweak global styles
  tags$style(HTML("
    body {
      background-color: #ffffff;
      font-family: 'Roboto', sans-serif;
    }
    .selectize-input {
      font-size: 16px;
    }
    .reactable .rt-thead.-header {
      background-color: #1f3c88;
      color: #ffffff;
      font-weight: bold;
      font-size: 14px;
    }
    h2, h3, h4, h5 {
      text-align: center;
    }
  ")),
  
  # APP TITLE
  titlePanel(
    div("🏀 Fantasy Basketball App 🏀",
        style = "text-align: center; font-size: 32px; font-weight: bold; color: #333;")
  ),
  
# Main navigation tabs
  tabsetPanel(
    
    # --- HOME PAGE ---
    tabPanel("🏠 Home",
             fluidPage(
               # Main heading and description
               fluidRow(
                 column(12, align = "center",
                        tags$h2("Welcome Fantasy Players!", 
                                style = "margin-top: 30px; font-weight: 
                                        bold; color: #4B0082;"),
                        
                        tags$h4("Explore real-time stats, 
                                positional breakdowns, 
                                and player performance insights.",
                                
                                style = "color: #555; max-width: 
                                        700px; margin: 0 auto 20px auto;"),
                        
                        tags$hr(style = "width: 60%; margin: 30px auto;")
                 )
               ),
               # Three feature highlights
               fluidRow(
                 column(4, align = "center",
                        tags$h4("📊 Visualize Stats"),
                        tags$p("Dive into dynamic charts including 
                               Points, Assists, and Rebounds.")
                 ),
                 column(4, align = "center",
                        tags$h4("🧭 Radar Breakdown"),
                        tags$p("Explore positional strengths with 
                               interactive radar charts.")
                 ),
                 column(4, align = "center",
                        tags$h4("📋 Player Tables"),
                        tags$p("Compare players using 
                               advanced and sortable data tables.")
                 )
               ),
               
               # Home message
               fluidRow(
                 column(12, align = "center",
                        tags$hr(style = "width: 60%; margin: 30px auto;"),
                        tags$p("Get started by clicking on a tab above!", 
                               style = "font-style: italic; color: #777;")
                 )
               ),
               
               # Motivation / About box
               br(), br(),
               fluidRow(
                 column(8, offset = 2,
                        div(
                          style = "background-color: #fdfdfd; 
                                  border-left: 5px solid #4B0082;
                                  padding: 20px 25px; margin-top: 10px; 
                                  box-shadow: 0 2px 6px rgba(0,0,0,0.08); 
                                  font-size: 16px; line-height: 1.6; 
                                  color: #444;",
                          HTML("
                              <h4 style='margin-top: 0;'>
                              Why I Built This App</h4>
                              <p>
                                This app was inspired by my first season 
                                playing fantasy basketball, after being 
                                introduced to it by friends. 
                                I struggled at first with understanding 
                                draft picks, building a strong team, 
                                and knowing who to trade for what.
                              </p>
                              <p>
                                I realized there wasn't a beginner-friendly 
                                tool that visualized all this clearly, 
                                so I built one.
                                My hope is that this app helps not only me, 
                                but also <strong>other beginners</strong> 
                                learn faster, 
                                make smarter draft picks, and enjoy 
                                fantasy basketball more.
                              </p>
                              ")
                        )
                 )
               )
             )
    ),
    
    # --- OPTION 1: STATIC + INTERACTIVE SCATTERPLOT PAGE ---
    tabPanel("📈 Fantasy Value Scatterplot",
             fluidPage(
               h3("(Static View)"),
               plotOutput("static_scatter", height = "600px"),  
               
               br(), br(),
               h4("What does this say?"),
               uiOutput("comparison_text"),  
               br(), br(),
               
               h3("(Interactive View: Hover Over Points for Details)"),
               plotlyOutput("interactive_scatter", height = "600px"),  
               
               br(), br(),
               h4("Conclusions Drawn:"),
               uiOutput("conclusion_text"),  
               br(), br()
             )
    ),
    
    # --- OPTION 3: RADAR PLOT BREAKDOWN PAGE ---
    tabPanel("⛹ Position-Skill Breakdown",
             sidebarLayout(
               sidebarPanel(
                 selectInput("position", "Choose a Position:",
                             choices = c("PG", "SG", "SF", "PF", "C"),
                             selected = "PG"),
                 
                 br(),
                 uiOutput("strengths"),  #strengths text
                 br(),
                 uiOutput("weakness"),   #weaknesses text
                 br(),
                 uiOutput("conclusions") #advice text
               ),
               mainPanel(
                 plotOutput("radarPlot", height = "600px")
               )
             )
    ),
    
    # --- OPTION 5: STATS TABLE PAGE ---
    tabPanel("📋 Stats Table",
             fluidPage(
               fluidRow(
                 column(12, align = "center",
                        tags$h3("📊 Interactive Player Statistics", 
                                style = "margin-top: 30px; font-weight: bold;"),
                        tags$p("This table allows you to sort and filter NBA players based on key performance metrics. 
              Use the dropdowns below to explore different stats or focus on specific positions. 
              Arrows indicate how each player's stat compares to the average.",
                               style = "max-width: 750px; margin: 0 auto 25px; color: #555; font-size: 16px;"
                        )
                 )
               ),
               
               # Table container with filters
               div(style = "
                 background-color: #ffffff;
                 border-radius: 12px;
                 padding: 30px;
                 margin: 0 auto 40px auto;
                 max-width: 95%;
                 box-shadow: 0 4px 12px rgba(0,0,0,0.08);",
                   
                   # Dropdown filters
                   fluidRow(
                     column(6,
                            div(style = "padding-right: 10px;",
                                selectInput("sort_by", "Sort players by:",
                                            choices = c("FantasyPoints", "Points", "Rebounds", "Steals", "Blocks", "Turnovers"),
                                            selected = "FantasyPoints", width = "100%")
                            )
                     ),
                     column(6,
                            div(style = "padding-left: 10px;",
                                selectInput("position_filter", "Filter by Position:",
                                            choices = c("All", sort(unique(nba_df$Position))),
                                            selected = "All", width = "100%")
                            )
                     )
                   ),
                   
                   # Legend explaining arrows
                   tags$div(
                     style = "text-align: center; margin: 25px 0 15px;",
                     tags$span(style = "color: green; font-weight: bold; margin-right: 15px;", "↑ Above Avg"),
                     tags$span(style = "color: red; font-weight: bold; margin-right: 15px;", "↓ Below Avg"),
                     tags$span(style = "color: gray; font-weight: bold;", "= Within Avg Range")
                   ),
                   
                   # Interactive reactable table
                   reactableOutput("nba_table")
               ),
               
               br(), br(),  # Add spacing
               
               # Insights section at bottom
               bsCollapse(
                 bsCollapsePanel(
                   "📘 Table Insights",
                   div(
                     style = "background:#f9f9f9; padding:20px; border-radius:8px; max-width: 1000px; margin: 0 auto;",
                     uiOutput("table_insights")
                   )
                 ),
                 open = NULL
               )
             )
    ),
    
    #OPTION 4: TIERLIST
    tabPanel("🏆 Player Tier List",
             fluidPage(
               h3("NBA Player Tier List", align = "center"),
               plotOutput("tier_list_plot", height = "800px"),
              br(), br(),
             uiOutput("tierlist_insights"),
             br()
          )
        )
    )
)


# =============================
# SERVERS (BE LOGIC)
# =============================

server <- function(input, output, session) {

  
# --- OPTION 3: Radar Plot based on Position ---
    output$radarPlot <- renderPlot({
    pos_code <- input$position
    pos_name <- position_labels[[pos_code]] %||% pos_code # Fallback if label missing
    pos_data <- position_scaled %>% filter(group == pos_code)
    colors <- RColorBrewer::brewer.pal(n = 5, name = "Set2")
    
    ggradar(pos_data,
            grid.min = 0, grid.mid = 0.5, grid.max = 1,
            values.radar = c("0%", "50%", "100%"),
            group.line.width = 1.2, group.point.size = 4,
            axis.label.size = 5, grid.label.size = 4,
            group.colours = colors[
              which(c("PG", "SG", "SF", "PF", "C") == pos_code)],
            fill = TRUE, fill.alpha = 0.4,
            gridline.min.colour = "gray80",
            gridline.mid.colour = "gray70",
            gridline.max.colour = "gray60",
            plot.title = paste("Skill Breakdown:", pos_name)
    ) +
      coord_equal(clip = "off") +
      theme(
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.margin = margin(40, 60, 40, 60),
        plot.background = element_rect(fill = "white", color = NA)
      )
  })

# --- Strengths Text Output (Dynamic based on Position) ---
  output$strengths <- renderUI({
    pos <- input$position
    txt <- switch(pos,
                  
                  "PG" = "<strong>Point Guards</strong> are elite in 
                  <strong>assists</strong> and <strong>steals</strong>, 
                  often controlling the game's pace and racking up fantasy 
                  value from playmaking and perimeter defense.",
                  
                  "SG" = "<strong>Shooting Guards</strong> shine in 
                  <strong>scoring</strong> and can contribute 
                  <strong>steals</strong>, especially when aggressive on 
                  defense. They're great for high offensive upside.",
                  
                  "SF" = "<strong>Small Forwards</strong> contribute across all 
                  categories. They’re typically balanced players who can get 
                  points, rebounds, assists, and even occasional steals or 
                  blocks.",
                  
                  "PF" = "<strong>Power Forwards</strong> are strong in 
                  <strong>rebounds</strong>, <strong>blocks</strong>, 
                  and often <strong>field goal %</strong>, 
                  making them versatile frontcourt options.",
                  
                  "C"  = "<strong>Centers</strong> dominate in 
                  <strong>rebounds</strong>, <strong>blocks</strong>, 
                  and <strong>field goal %</strong>. 
                  They're anchors in the paint and consistent 
                  fantasy contributors.",
                  
                  "<em>Select a position to see its strengths.</em>"
    )
    HTML(paste0("<div style='font-size:14px;'>", txt, "</div>"))
  })
  
# --- Weaknesses Text Output (Dynamic based on Position) ---
  output$weakness <- renderUI({
    pos <- input$position
    txt <- switch(pos,
                  
                  "PG" = "Point Guards often lack <strong>rebounds</strong> and 
                  <strong>blocks</strong>. They also may have lower field goal 
                  percentages due to high shot volume.",
                  
                  "SG" = "Shooting Guards are usually weaker in 
                  <strong>rebounds</strong>, <strong>blocks</strong>, 
                  and may be streaky shooters, leading to inconsistent 
                  fantasy performance.",
                  
                  "SF" = "Small Forwards don’t usually excel in any one 
                  category. Their all-around game means they might not 
                  lead in key fantasy stats.",
                  
                  "PF" = "Power Forwards typically provide 
                  <strong>fewer assists</strong> and <strong>steals</strong> 
                  than guards, limiting their playmaking impact.",
                  
                  "C"  = "Centers rarely contribute <strong>assists</strong> or 
                  <strong>steals</strong>. They also tend to offer low 
                  free-throw percentages and no three-point value.",
                  
                  "<em>Select a position to see its weaknesses.</em>"
    )
    HTML(paste0("<div style='font-size:14px;'>", txt, "</div>"))
  })
  
# --- Conclusions Text Output (Advice based on Position) ---
  output$conclusions <- renderUI({
    pos <- input$position
    txt <- switch(pos,
                  "PG" = "Pair Point Guards with bigs like PFs or Cs to cover 
                  <strong>rebounding and blocking</strong> gaps. PGs are 
                  essential for <strong>assists, steals, 
                  and tempo-setting</strong> in your lineup.",
                  
                  "SG" = "Use Shooting Guards as <strong>high-upside 
                  scorers</strong> 
                  to spike weekly point totals. Pair them with players strong 
                  in <strong>rebounds or blocks</strong> to balance defense.",
                  
                  "SF" = "Small Forwards offer <strong>well-rounded 
                  coverage</strong>. Use them to stabilize your roster across 
                  all stat categories without overcommitting to any one skill.",
                  
                  "PF" = "Power Forwards give you <strong>solid defense and 
                  interior strength</strong>. Draft guards with strong 
                  <strong>assists and steals</strong> to complement them.",
                  
                  "C"  = "Drafting a strong Center gives you a reliable base 
                  for <strong>rebounds and blocks</strong>. Pair them with 
                  guards or wings for <strong>assists, steals, and 
                  scoring</strong> to balance your team.",
                  ""
    )
    HTML(paste0("<div style='font-size:14px;'>", txt, "</div>"))
  })

# --- OPTION 1: Scatterplots ---
  
  # 1. Static Scatterplot (with player labeling)
  output$static_scatter <- renderPlot({
    top_td3 <- df %>%
      group_by(Position) %>%
      slice_max(TripleDoubles, n = 1, with_ties = FALSE)
    
    top_dd2 <- df %>%
      group_by(Position) %>%
      slice_max(DoubleDoubles, n = 1, with_ties = FALSE)
    
    top_labels <- bind_rows(top_td3, top_dd2) %>%
      distinct(Player, .keep_all = TRUE)
    
    ggplot(df, aes(x = Minutes, y = FantasyPoints, 
                   color = DoubleDoubles, size = TripleDoubles)) +
      geom_point(alpha = 0.8) +
      scale_size_continuous(range = c(3, 11)) + 
      scale_color_viridis_c(option = "plasma") +
      facet_wrap(~ Position) +
      labs(
        title = "Top Fantasy Point Contributors by Position and Double/Triple-Doubles",
        x = "Minutes per Game",
        y = "Average Fantasy Points",
        color = "Double-Doubles",
        size = "Triple-Doubles"
      ) +
      geom_text_repel(
        data = top_labels,
        aes(label = Player),
        size = 3,
        max.overlaps = Inf,
        color = "black",
        segment.color = "gray30",
        segment.size = 0.3,
        box.padding = 0.35,
        point.padding = 0.3
      ) +
      theme_minimal() + 
      theme(
        strip.text = element_text(size = 16, face = "bold"),
        panel.spacing = unit(1, "lines"),
        axis.title.x = element_text(size = 14, face = "bold"),  # X-axis label
        axis.title.y = element_text(size = 14, face = "bold"),  # Y-axis label
        plot.title = element_text(hjust = 0.5, face = "bold", size = 25)
      )
  })
  
  # 2. Text between the plots
  output$comparison_text <- renderUI({
    HTML(
      paste0(
        "<p><strong>Centers (C) and Power Forwards (PF) </strong> produce the most fantasy value ",
        "through frequent <strong>double and triple-doubles</strong>. ",
        "Point guards (PG) contribute well too.</p>",
        
        "<p>However, <strong>small and shooting forwards</strong> generally show lower upside, ",
        "with fewer multi-category performances and less fantasy impact overall.</p>",
        
        "<p>The static chart above highlights the top players for double and triple-doubles, ",
        "so we can now identify which players are the best in their position at securing ",
        "<strong>multi-category contributions</strong>. Keep these players in mind when picking
        your team.</p>"
      )
    )
  })
  
  # 3. Interactive Scatterplot (hoverable plot)
  output$interactive_scatter <- renderPlotly({
    p <- ggplot(df, aes(
      x = Minutes,
      y = FantasyPoints,
      color = DoubleDoubles,
      size = TripleDoubles,
      text = paste0(
        "Player: ", Player, "\n",
        "Position: ", Position, "\n",
        "Fantasy Points: ", round(FantasyPoints, 1), "\n",
        "Minutes: ", Minutes, "\n",
        "Double-Doubles: ", DoubleDoubles, "\n",
        "Triple-Doubles: ", TripleDoubles
      )
    )) +
      geom_point(alpha = 0.8) +
      scale_color_viridis_c(option = "plasma") +
      scale_size_continuous(range = c(3, 11)) +
      labs(
        title = "Player Fantasy Performance vs. Playing Time",
        x = "Minutes per Game",
        y = "Average Fantasy Points"
      ) +
      theme_light() +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 18) 
      )
    
    ggplotly(p, tooltip = "text")
  })

  # 4. Text Conclusions After Scatterplots
  output$conclusion_text <- renderUI({
    HTML(
      paste0(
        "<p>From both graphs, the main things we can notice is the <strong>Dominance of Centers and Power Forwards</strong>, ",
        "which is why we should <strong>prioritize elite Cs and PFs</strong> who play heavy minutes when making your draft — ",
        "they’re your best bet for high fantasy production via multi-category contributions.</p>",
        
        "<p>In terms of other positions, we need to <strong>be selective with PGs</strong>; ",
        "go for versatile steals/assists and rebounders ",
        "(to know why, go to the <strong>Position Skill Breakdown</strong> tab).</p>",
        
        "<p>Shooting Guards and Forwards (SG/SF) and  are less likely to rack up double or triple-doubles and offer lower upside, ",
        "but they still have a use — which you’ll see in the <strong>next tab</strong>.</p>",
        
        "<p><strong>Luckily, from our interactive plot</strong>, we can see which Centers and players offer the most value ",
        "<strong>just by hovering over them!</strong></p>"
      )
    )
  })
  
# --- OPTION 5: Interactive Table (Reactable) ---
  output$nba_table <- renderReactable({
    filtered_df <- nba_df
    if (input$position_filter != "All") {
      filtered_df <- filtered_df %>% filter(Position == input$position_filter)
    }
    
    ranked_df <- filtered_df %>%
      arrange(desc(.data[[input$sort_by]])) %>%
      mutate(.rank_icon = case_when(
        row_number() == 1 ~ "🥇 ",
        row_number() == 2 ~ "🥈 ",
        row_number() == 3 ~ "🥉 ",
        TRUE ~ ""
      ))
    
    ranked_df_clean <- ranked_df %>% select(-.rank_icon)
    
    reactable(
      ranked_df_clean,
      bordered = TRUE, striped = TRUE, highlight = TRUE,
      defaultColDef = colDef(align = "center"),
      columns = list(
        Player = colDef(name = "Player", cell = function(value, index) {
          paste0(ranked_df$.rank_icon[index], value)
        }),
        Team = colDef(name = "Team"),
        Position = colDef(name = "Position"),
        FantasyPoints = colDef(name = "Fantasy Points", 
                               format = colFormat(digits = 1)),
        Points = colDef(
          cell = arrow_above_below_avg(ranked_df$Points, formatC)),
        Rebounds = colDef(
          cell = arrow_above_below_avg(ranked_df$Rebounds, formatC)),
        Steals = colDef(
          cell = arrow_above_below_avg(ranked_df$Steals, formatC)),
        Blocks = colDef(
          cell = arrow_above_below_avg(ranked_df$Blocks, formatC)),
        Turnovers = colDef(
          cell = arrow_above_below_avg(ranked_df$Turnovers, formatC)),
        `FG%` = colDef(
          format = colFormat(percent = TRUE, digits = 1)),
        `FT%` = colDef(
          format = colFormat(percent = TRUE, digits = 1))
      )
    )
  })
  
  # --- Text Explanation for Table (below reactable) ---
  output$table_insights <- renderUI({
    HTML(
      paste0(
        "<h4>📝 Understanding the Player Statistics Table</h4>",
        
        "<p><strong>What this table shows:</strong><br>
      This interactive table ranks NBA players based on key fantasy metrics like Points, Rebounds, Steals, Blocks, and Turnovers. 
      Arrows next to stats show how each player compares to the league average — 
      <span style='color:green;'>↑ Above Avg</span> means better than average, 
      <span style='color:red;'>↓ Below Avg</span> means below average, 
      and <span style='color:gray;'>=</span> means close to average.</p>",
        
        "<p><strong>Insights you can find:</strong><br>
      Even highly ranked players have weaknesses (e.g., Giannis with low FT%, Luka with weak FG%). 
      Some players are great scorers but struggle defensively or with efficiency.
      The table highlights areas where a player might hurt you in certain categories 
      despite high overall fantasy points.</p>",
        
        "<p><strong>How this helps your draft strategy:</strong><br>
      Don’t just pick based on total fantasy points — use the table to <strong>balance your team across all categories</strong>. 
      For example, if you draft a high-scoring guard with weak rebounds, 
      pair them with a strong rebounding forward. 
      This ensures you stay competitive across multiple categories and avoid hidden weaknesses 
      that can cost you weekly matchups.</p>"
      )
    )
  })

# --- OPTION 4: Tierlist plot output ---
  df_plot <- data.frame(
    tier = composite_score$Tier,
    text = text,
    img = paste0("team_pngs/", composite_score$Team, ".png"),
    stringsAsFactors = FALSE
  )
  
  output$tier_list_plot <- renderPlot({
    
    # ✅ Just prepare the final df_plot ONCE
    text <- sapply(composite_score$Player, function(name) {
      name_parts <- strsplit(name, " ")[[1]]
      paste(rev(name_parts), collapse = " ")
    })
    text <- str_replace_all(text, "-", " ")
    
    img <- paste0("team_pngs/", composite_score$Team, ".png")
    
    df_plot <- data.frame(tier = composite_score$Tier, text = text, img = img, stringsAsFactors = FALSE)
    
    # ✅ Now call your tierlist function
    tierlist(df_plot, main = "NBA Player Tier List", width = 10)
  })
  # --- Explanation Text for Tier List ---
  output$tierlist_insights <- renderUI({
    HTML(
      paste0(
        "<h4>📈 Understanding the NBA Tier List</h4>",
        
        "<p><strong>What this tierlist shows:</strong><br>
      This tierlist ranks players based on a <strong>Composite Score</strong> 
      that combines their scoring, playmaking, rebounding, defense, and overall impact. 
      Players are placed into tiers from <strong>S (elite)</strong> to <strong>F (lowest)</strong> 
      based on their percentile among all players.</p>",
        
        "<p><strong>How you can use it:</strong><br>
      Using the tierlist you can identify <strong>top players</strong> across all fantasy categories. 
      These are the players you ideally want to spend a lot of fantasy cash on
      when the draft starts or look out for as trade 
      targets, and balance your team with both stars and hidden gems across all 
      positions.</p>",
        
        "<h4>📊 Raw Composite Scores and Tier Mapping</h4>",
        
        "<p>Raw composite scores assess a player's overall versatility and serve as an indicator 
      of their proficiency across a variety of basketball-related skills. 
      Each player is assigned a final percentile score, reflecting their ranking within the 
      distribution of composite scores across the league. 
      Players are then grouped into tiers based on their percentile ranking, 
      as outlined below:</p>",
        
        "<ul>
        <li><strong>Composite Score > 95th percentile:</strong> S Tier</li>
        <li><strong>Composite Score 80th - 95th percentile:</strong> A Tier</li>
        <li><strong>Composite Score 55th - 80th percentile:</strong> B Tier</li>
        <li><strong>Composite Score 30th - 55th percentile:</strong> C Tier</li>
        <li><strong>Composite Score 15th - 30th percentile:</strong> D Tier</li>
        <li><strong>Composite Score < 15th percentile:</strong> F Tier</li>
      </ul>"
      )
    )
  })
  }


shinyApp(ui, server)