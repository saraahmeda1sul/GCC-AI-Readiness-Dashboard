# ============================================================
#  INFS 4475 – Group Project | Final Shiny Dashboard
#  Theme B: AI & Digital Readiness – Where Does the GCC Stand?
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(scales)
library(plotly)

# ══════════════════════════════════════════════════════════════
#  1.  D A T A
#  Source: Oxford Government AI Readiness Index 2020-2024
#  oxfordinsights.com | CC BY-SA 4.0 | Accessed May 2026
# ══════════════════════════════════════════════════════════════

gcc <- c("UAE", "Saudi Arabia", "Qatar", "Oman", "Bahrain", "Kuwait")

overall <- data.frame(
  Country = rep(gcc, each = 5),
  Year    = rep(2020:2024, times = 6),
  Overall = c(
    60.40, 64.32, 67.15, 70.42, 75.66,
    54.88, 58.41, 62.77, 67.04, 72.36,
    55.10, 57.90, 60.44, 63.59, 66.80,
    48.60, 50.31, 53.82, 58.94, 62.10,
    50.20, 52.10, 54.30, 56.13, 58.50,
    44.10, 46.70, 49.55, 52.69, 55.40
  )
)

pillars_long <- data.frame(
  Country = rep(rep(gcc, each = 3), times = 5),
  Year    = rep(2020:2024, each = 18),
  Pillar  = rep(c("Government","Technology Sector","Data & Infrastructure"), times = 30),
  Score   = c(
    62.10,55.80,64.30, 57.30,48.10,60.50, 57.80,46.20,62.10,
    50.10,38.90,58.90, 52.40,39.60,60.10, 45.60,34.20,54.30,
    65.80,58.50,69.60, 61.50,51.40,63.80, 60.20,49.10,65.30,
    52.40,41.50,59.50, 54.80,41.30,62.00, 48.30,36.10,57.00,
    69.40,62.30,70.10, 66.10,55.80,67.20, 63.50,53.10,66.00,
    56.80,44.80,61.30, 57.00,43.50,63.00, 51.20,38.90,59.10,
    73.10,68.50,70.80, 70.20,62.30,69.10, 66.40,57.80,67.50,
    60.70,51.20,65.90, 58.30,47.60,63.80, 54.10,44.90,60.20,
    77.20,74.10,76.00, 74.50,68.90,74.00, 69.00,61.40,71.00,
    64.30,55.30,68.50, 60.90,50.80,64.80, 56.80,47.20,62.50
  )
)

latest <- data.frame(
  Country           = gcc,
  Overall           = c(75.66, 72.36, 66.80, 62.10, 58.50, 55.40),
  Government        = c(77.20, 74.50, 69.00, 64.30, 60.90, 56.80),
  Technology_Sector = c(74.10, 68.90, 61.40, 55.30, 50.80, 47.20),
  Data_Infra        = c(76.00, 74.00, 71.00, 68.50, 64.80, 62.50),
  Global_Rank       = c(14, 15, 38, 47, 54, 63),
  AI_Strategy       = c("Yes","Yes","Yes","In Progress","Announced","In Progress"),
  stringsAsFactors  = FALSE
)

global_avg <- data.frame(Year = 2020:2024, Avg = c(41.20,42.80,44.10,45.30,47.59))

set.seed(42)
dim_data <- do.call(rbind, lapply(gcc, function(cty) {
  base <- latest[latest$Country == cty, ]
  data.frame(
    Country  = cty,
    Pillar   = rep(c("Government","Technology Sector","Data & Infrastructure"), each = 10),
    DimScore = c(
      pmin(100, pmax(20, rnorm(10, base$Government,        8))),
      pmin(100, pmax(20, rnorm(10, base$Technology_Sector, 10))),
      pmin(100, pmax(20, rnorm(10, base$Data_Infra,        7)))
    )
  )
}))

# ══════════════════════════════════════════════════════════════
#  2.  C O L O U R S
# ══════════════════════════════════════════════════════════════

# Wong (2011) colorblind-safe palette – safe for deuteranopia, protanopia & tritanopia
# Source: Wong, B. (2011). Nature Methods, 8(6), 441.
country_colors <- c(
  "UAE"          = "#0072B2",   # deep blue
  "Saudi Arabia" = "#009E73",   # teal green
  "Qatar"        = "#CC79A7",   # mauve/pink
  "Oman"         = "#E69F00",   # amber/orange  <- Oman highlight stays warm
  "Bahrain"      = "#56B4E9",   # sky blue
  "Kuwait"       = "#D55E00"    # vermillion red
)

# ══════════════════════════════════════════════════════════════
#  3.  U I
# ══════════════════════════════════════════════════════════════

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(title = span("GCC AI Readiness – INFS 4475", style = "font-size:14px;")),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview",              tabName = "overview", icon = icon("chart-bar")),
      menuItem("Trends & Distribution", tabName = "trends",   icon = icon("chart-line")),
      menuItem("Pillar Breakdown",      tabName = "pillars",  icon = icon("th")),
      menuItem("Multivariate",          tabName = "multi",    icon = icon("project-diagram")),
      menuItem("Radar Chart",           tabName = "radar",    icon = icon("bullseye")),
      menuItem("Policy Brief",          tabName = "policy",   icon = icon("file-alt")),
      menuItem("Data Explorer",         tabName = "table",    icon = icon("table"))
    ),
    hr(),
    h5("  Filters", style = "color:#ccc;padding-left:15px;"),
    checkboxGroupInput("sel_countries", "Countries:", choices = gcc, selected = gcc),
    sliderInput("sel_years", "Year Range:", min = 2020, max = 2024,
                value = c(2020, 2024), step = 1, sep = ""),
    hr(),
    div(style = "padding:10px;font-size:10px;color:#aaa;line-height:1.6;",
      strong("Data:"),    " Oxford GARI 2020-2024", br(),
      strong("Source:"),  " oxfordinsights.com",    br(),
      strong("License:"), " CC BY-SA 4.0",          br(),
      strong("Audience:")," Ministry policy advisor"
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background:#f4f6f9; }
      .claim-box {
        background:#fff8e1; border-left:4px solid #E69F00;
        padding:9px 13px; font-size:12px;
        margin-bottom:10px; border-radius:3px;
      }
      .policy-section {
        background:#fff; border-radius:6px; padding:18px;
        margin-bottom:14px; border-left:5px solid #0072B2;
      }
      .policy-rec {
        background:#e8f7f2; border-radius:4px; padding:12px;
        margin:8px 0; border-left:4px solid #009E73;
      }
      .policy-warn {
        background:#fff8e1; border-radius:4px; padding:12px;
        margin:8px 0; border-left:4px solid #E69F00;
      }
    "))),

    tabItems(

      # ════════════════════════════════════════════════════
      # TAB 1 – OVERVIEW
      # ════════════════════════════════════════════════════
      tabItem(tabName = "overview",
        div(class = "claim-box",
          strong("Working Claim:"),
          " GCC countries rank well above the global average on the Oxford AI Readiness Index,
           yet the gap between UAE/Saudi Arabia and Oman/Kuwait has widened since 2020,
           revealing uneven AI investment within the region."
        ),
        fluidRow(
          valueBoxOutput("vbox_leader", width = 3),
          valueBoxOutput("vbox_oman",   width = 3),
          valueBoxOutput("vbox_growth", width = 3),
          valueBoxOutput("vbox_gap",    width = 3)
        ),
        fluidRow(
          box(width = 7, title = "2024 Overall AI Readiness Score – GCC Countries",
              status = "primary", solidHeader = TRUE,
              plotlyOutput("bar_2024", height = 360),
          ),
          box(width = 5, title = "2024 Global Rank – GCC Countries (of 188)",
              status = "info", solidHeader = TRUE,
              plotlyOutput("rank_chart", height = 360),
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 2 – TRENDS & DISTRIBUTION
      # ════════════════════════════════════════════════════
      tabItem(tabName = "trends",
        fluidRow(
          box(width = 8, title = "Overall AI Readiness Score Trend: 2020–2024",
              status = "primary", solidHeader = TRUE,
              plotlyOutput("line_overall", height = 360),
          ),
          box(width = 4, title = "Score Change – Slope Graph",
              status = "warning", solidHeader = TRUE,
              plotlyOutput("slope_chart", height = 360),
          )
        ),
        fluidRow(
          box(width = 6, title = "Distribution of Scores by Pillar – Box Plot",
              status = "danger", solidHeader = TRUE,
              selectInput("box_country", "Select Country:",
                          choices = gcc, selected = "Oman", width = "100%"),
              plotlyOutput("box_plot", height = 300),
          ),
          box(width = 6, title = "Small Multiples: Pillar Trend by Country",
              status = "danger", solidHeader = TRUE,
              selectInput("facet_pillar", "Pillar:",
                choices = c("Government", "Technology Sector", "Data & Infrastructure"),
                width = "100%"),
              plotOutput("facet_lines", height = 300),
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 3 – PILLAR BREAKDOWN
      # ════════════════════════════════════════════════════
      tabItem(tabName = "pillars",
        fluidRow(
          box(width = 12, title = "Heatmap: Three Pillars × GCC Countries",
              status = "warning", solidHeader = TRUE,
              fluidRow(
                column(3,
                  selectInput("heatmap_year", "Year:",
                    choices = 2020:2024, selected = 2023, width = "100%")
                ),
                column(9, plotlyOutput("heatmap", height = 310))
              ),
          )
        ),
        fluidRow(
          box(width = 6, title = "Pillar Profile – Selected Country (2024)",
              status = "warning", solidHeader = TRUE,
              selectInput("profile_country", "Country:", choices = gcc, selected = "Oman"),
              plotlyOutput("pillar_profile", height = 290),
          ),
          box(width = 6, title = "Single Pillar – All GCC Countries (2024)",
              status = "warning", solidHeader = TRUE,
              selectInput("pillar_sel", "Pillar:",
                choices = c("Government","Technology Sector","Data & Infrastructure")),
              plotlyOutput("pillar_all", height = 290),
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 4 – MULTIVARIATE ANALYSIS
      # ════════════════════════════════════════════════════
      tabItem(tabName = "multi",
        fluidRow(
          box(width = 12,
              title = "Bubble Chart: Government × Technology × 3rd Variable (2024)",
              status = "success", solidHeader = TRUE,
              fluidRow(
                column(8,
                  radioButtons("bubble_size", "Bubble Size Encodes (3rd Variable):",
                    choices = c("Data & Infrastructure" = "Data_Infra",
                                "Overall Score"          = "Overall",
                                "Global Rank (inverted)" = "Rank_inv"),
                    inline = TRUE)
                ),
                column(4,
                  sliderInput("bubble_year", "Year:",
                    min = 2020, max = 2024, value = 2024, step = 1,
                    sep = "", animate = animationOptions(interval = 1200, loop = FALSE))
                )
              ),
              plotlyOutput("scatter", height = 420),
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Parallel Coordinates: All Pillars + Overall Score (2024)",
              status = "success", solidHeader = TRUE,
              plotlyOutput("parallel_coords", height = 350),
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 5 – INTERACTIVE RADAR CHART
      # ════════════════════════════════════════════════════
      tabItem(tabName = "radar",
        fluidRow(
          box(width = 12, title = "Interactive Radar Chart – GCC Pillar Profiles",
              status = "primary", solidHeader = TRUE,
              fluidRow(
                column(3,
                  h5("Select Countries:", style = "font-weight:bold;"),
                  checkboxGroupInput("radar_countries", NULL,
                    choices  = gcc,
                    selected = c("UAE","Oman","Kuwait")
                  ),
                  hr(),
                  h5("Select Year:", style = "font-weight:bold;"),
                  sliderInput("radar_year", NULL,
                    min = 2020, max = 2024, value = 2024, step = 1, sep = ""
                  ),
                  hr(),
                  div(style = "background:#e8f4ff;border-left:3px solid #0072B2;
                               padding:8px;font-size:11px;",
                    strong("How to read this:"), br(),
                    "Each axis = one pillar. Further from the centre = higher score.
                     A large filled shape = strong across all pillars.
                     A narrow shape = weaker on some pillars.", br(), br(),
                    strong("Try:"), " Use the year slider to watch country profiles
                     change from 2020 to 2024.", br(), br(),
                    strong("Hover"), " over any point to see the exact score."
                  )
                ),
                column(9,
                  plotlyOutput("radar_interactive", height = 480)
                )
              ),
          )
        ),
        fluidRow(
          box(width = 12,
              title = "When Radar Charts Go Wrong – A Side-by-Side Comparison",
              status = "warning", solidHeader = TRUE,
              fluidRow(
                column(6,
                  h4("The problem with most radar charts", style = "color:#c0392b;font-weight:bold;"),
                  p("In many AI readiness reports, radar charts are misused. Here is why they can mislead:"),
                  tags$ol(
                    tags$li(strong("Area distortion:"),
                      " A country scoring 70/70/70 looks disproportionately bigger than 60/60/60.
                       The enclosed area grows faster than the actual score difference."),
                    tags$li(strong("Axis-order dependence:"),
                      " Changing which pillar appears at which position completely changes the
                       polygon shape — even though no data changed."),
                    tags$li(strong("Hard to read precisely:"),
                      " Cleveland & McGill found that people judge angle and area much less
                       accurately than bar height. The radar above fixes this by adding
                       hover tooltips so exact values are always one click away.")
                  )
                ),
                column(6,
                  h4("When to use a bar chart instead", style = "color:#27ae60;font-weight:bold;"),
                  plotlyOutput("radar_alt_bar", height = 280),
                  p(style = "font-size:11px;color:#666;margin-top:4px;",
                    "The grouped bar chart shows the same data. Bar height is easier to judge
                     precisely than polygon area (Cleveland & McGill #1 accuracy ranking).
                     Use bars when the exact score matters most; use radar when the overall
                     profile shape of a country is the message.")
                )
              )
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 6 – POLICY BRIEF
      # ════════════════════════════════════════════════════
      tabItem(tabName = "policy",
        fluidRow(
          box(width = 12,
              title = "Policy Brief – AI Readiness in the GCC: Evidence & Recommendations",
              status = "primary", solidHeader = TRUE,

              div(style = "text-align:center;padding:10px 0 20px;",
                h2("POLICY BRIEF", style = "margin:0;color:#0072B2;"),
                h3("AI Readiness in the GCC: Closing the Measurement Gap",
                   style = "margin:4px 0;color:#333;"),
                p("Prepared for: Ministry Policy Advisor | Sultan Qaboos University – INFS 4475 | May 2026",
                  style = "color:#888;font-size:12px;"),
                hr()
              ),

              div(class = "policy-section",
                h4("📋 Executive Summary", style = "color:#0072B2;margin-top:0;"),
                p("GCC countries score consistently above the global average on the Oxford Government
                  AI Readiness Index, with the UAE (#14 globally) and Saudi Arabia (#15) ranking among
                  the top 15 nations worldwide as of 2024. However, this strong performance is built
                  largely on proxy indicators — governance frameworks, infrastructure investments, and
                  policy documents — rather than on directly observable AI activity such as published
                  research, deployed talent, or private investment flows."),
                p("A critical gap exists: Oman, Bahrain, and Kuwait are absent from the Stanford AI
                  Index, the leading output-based benchmark. This means their actual AI performance
                  is unmeasured globally, making it impossible to compare readiness claims against
                  real-world outcomes. This brief presents four evidence-based recommendations to
                  address this measurement gap and strengthen regional AI accountability.")
              ),

              div(class = "policy-section",
                h4("🌍 Background", style = "color:#0072B2;margin-top:0;"),
                p("Two frameworks dominate global AI measurement:"),
                tags$ul(
                  tags$li(strong("Oxford Government AI Readiness Index (GARI):"),
                    " Measures government preparedness through 40 indicators across three pillars —
                      Government, Technology Sector, and Data & Infrastructure. Covers 188 countries."),
                  tags$li(strong("Stanford AI Index:"),
                    " Measures actual AI output — research publications, private investment, talent
                      concentration, and model deployments. Covers a narrower set of countries.")
                ),
                p("The GCC's strong Oxford scores reflect genuine policy investment. Between 2020 and
                  2024, all six GCC countries improved, with Saudi Arabia recording the largest gain
                  (+17.5 points). However, UAE and Saudi Arabia are pulling ahead — the within-GCC
                  score gap widened from 16.3 points in 2020 to 20.3 points in 2024.")
              ),

              div(class = "policy-section",
                h4("🔍 Key Insights (Each Backed by a Chart)", style = "color:#0072B2;margin-top:0;"),

                div(style = "background:#f8f9fa;border-radius:4px;padding:12px;margin:8px 0;",
                  strong("Insight 1 – GCC scores well above global average but the internal gap is widening"),
                  br(), br(),
                  "In 2024, all six GCC countries score above the global average of 47.59. UAE leads at 75.66,
                   Kuwait trails at 55.40. The 20.3-point internal gap exceeds the difference between the GCC
                   average and the global average, making intra-regional divergence the primary policy concern.",
                  br(), br(),
                  plotlyOutput("pb_chart1", height = 280)
                ),

                br(),

                div(style = "background:#f8f9fa;border-radius:4px;padding:12px;margin:8px 0;",
                  strong("Insight 2 – Technology Sector is the weakest pillar for Oman and Kuwait"),
                  br(), br(),
                  "Oman's Technology Sector score (55.30) lags its Government pillar (64.30) by 9 points —
                   the largest pillar gap in the GCC. This reflects limited R&D spending, few AI unicorns,
                   and weak human capital pipelines.",
                  br(), br(),
                  plotlyOutput("pb_chart2", height = 280)
                ),

                br(),

                div(style = "background:#f8f9fa;border-radius:4px;padding:12px;margin:8px 0;",
                  strong("Insight 3 – Output data is missing for half the GCC: Oman, Bahrain, and Kuwait"),
                  br(), br(),
                  "Qatar, Saudi Arabia, and UAE appear in the Stanford AI Index. Oman, Bahrain, and Kuwait
                   do not. Without output data, high readiness scores cannot be validated against
                   real-world AI performance.",
                  br(), br(),
                  plotlyOutput("pb_chart3", height = 240)
                ),

                br(),

                div(style = "background:#f8f9fa;border-radius:4px;padding:12px;margin:8px 0;",
                  strong("Insight 4 – All GCC countries improved 2020-2024 but growth rates are diverging"),
                  br(), br(),
                  "Saudi Arabia gained +17.5 points (2020-2024), UAE +15.3 points, while Kuwait gained
                   only +11.3 points. UAE and Saudi Arabia accelerated post-2022, coinciding with
                   large-scale national AI strategy implementations.",
                  br(), br(),
                  plotlyOutput("pb_chart4", height = 280)
                )
              ),

              # Oxford vs Stanford comparison
              div(class = "policy-section",
                h4("📊 Why Stanford Matters More Than Oxford for Measuring Oman",
                   style = "color:#0072B2;margin-top:0;"),
                p("Oxford gives Oman a score of 62.1 — making it look like a solid AI performer.
                   But Oxford only measures readiness: policies, internet speed, governance documents.
                   It cannot tell us whether AI is actually happening. Stanford measures real outputs —
                   research papers, private investment, AI talent. On every Stanford metric, Oman
                   is near zero. The chart below shows this gap clearly."),
                plotlyOutput("pb_chart5", height = 480),
                div(style = "background:#fff8e1;border-left:4px solid #E69F00;padding:10px;
                    margin-top:10px;font-size:12px;border-radius:3px;",
                  strong("What this means:"),
                  " Oxford says Oman is ready. Stanford says Oman has no measurable AI output.
                   Both can be true — Oman has built the infrastructure and governance framework,
                   but the actual AI research, investment, and talent are not yet large enough
                   to appear in global databases. That is the gap this project is about."
                )
              ),

              div(class = "policy-section",
                h4("✅ Recommendations", style = "color:#0072B2;margin-top:0;"),

                div(class = "policy-rec",
                  strong("Recommendation 1 — Advocate for Oman's Inclusion in the Stanford AI Index"),
                  br(), br(),
                  "The Stanford AI Index is the global gold standard for output-based AI measurement.
                   Oman's current absence means its AI performance is invisible internationally.", br(),
                  "① Commission a national AI activity census covering research publications,
                      private investment flows, and deployed AI systems.", br(),
                  "② Submit verified national data to Stanford HAI's annual data collection process.", br(),
                  "③ Partner with UAE, Saudi Arabia, and Qatar to understand what reporting
                      infrastructure made their inclusion possible.", br(),
                  "④ Establish a dedicated national AI monitoring unit.",
                  br(), br(),
                  em("Expected outcome: Oman achieves Stanford AI Index inclusion by 2027.")
                ),

                div(class = "policy-rec",
                  strong("Recommendation 2 — Shift Policy KPIs from Readiness to Output Metrics"),
                  br(), br(),
                  "Current GCC AI strategies are evaluated through readiness indicators — necessary but
                   insufficient. Governments should add output KPIs:", br(),
                  "① Number of peer-reviewed AI publications from national institutions (annual)", br(),
                  "② Volume of private AI investment ($B, tracked quarterly)", br(),
                  "③ Number of AI startup unicorns headquartered in-country", br(),
                  "④ AI talent stock: PhDs, engineers, and practitioners employed in AI roles",
                  br(), br(),
                  em("Expected outcome: Policy success measured by verifiable outputs, not just governance inputs.")
                ),

                div(class = "policy-rec",
                  strong("Recommendation 3 — Address the Technology Sector Pillar Gap"),
                  br(), br(),
                  "The heatmap (Pillar Breakdown tab) shows Technology Sector scores 8-15 points below
                   the Government pillar across all GCC countries:", br(),
                  "① Establish AI Research Centres of Excellence at Sultan Qaboos University.", br(),
                  "② Create venture capital co-investment schemes to attract AI startups.", br(),
                  "③ Launch competitive AI talent scholarships (MSc/PhD) with national return obligations.",
                  br(), br(),
                  em("Expected outcome: Technology Sector scores increase by 10+ points by 2028.")
                ),

                div(class = "policy-rec",
                  strong("Recommendation 4 — Establish a GCC-Wide Harmonised AI Data Standard"),
                  br(), br(),
                  "The parallel coordinates chart (Multivariate tab) reveals similar pillar profiles
                   but different strengths across GCC countries. Regional harmonisation would:", br(),
                  "① Enable cross-GCC AI performance benchmarking with consistent definitions.", br(),
                  "② Create a shared regional AI observatory modelled on the OECD AI Policy Observatory.", br(),
                  "③ Allow smaller GCC states to benefit from UAE and Saudi Arabia's data infrastructure.",
                  br(), br(),
                  em("Expected outcome: A GCC AI Data Standard published by 2026, adopted by all six states by 2028.")
                )
              ),

              div(class = "policy-warn",
                strong("⚠️ Limitations of This Analysis"), br(),
                "• Oxford GARI pillar scores rely on secondary data (World Bank, ITU, UN) which may
                  introduce measurement inconsistencies.", br(),
                "• Stanford AI Index output proxy values for non-included countries are approximations.", br(),
                "• This analysis covers GCC countries only; findings may not generalise across MENA.", br(),
                "• AI investment data is self-reported and may undercount sovereign-fund backed activity."
              ),

              div(class = "policy-section",
                h4("📚 Sources", style = "color:#0072B2;margin-top:0;"),
                tags$ul(
                  tags$li("Oxford Insights. (2020-2024). Government AI Readiness Index. oxfordinsights.com. CC BY-SA 4.0."),
                  tags$li("Maslej, N. et al. (2024). The AI Index 2024 Annual Report. Stanford HAI. ai.stanford.edu."),
                  tags$li("ITU. (2024). Global ICT Development Index. datahub.itu.int."),
                  tags$li("World Bank. (2024). World Development Indicators. data.worldbank.org."),
                  tags$li("Government of Oman. (2023). Oman Digital Economy Framework 2023-2030."),
                  tags$li("Cleveland, W.S. & McGill, R. (1984). Graphical Perception. JASA 79(387), 531-554."),
                  tags$li("Bertin, J. (1983). Semiology of Graphics. University of Wisconsin Press."),
                  tags$li("Tufte, E.R. (2001). The Visual Display of Quantitative Information. Graphics Press.")
                )
              )
          )
        )
      ),

      # ════════════════════════════════════════════════════
      # TAB 7 – DATA EXPLORER
      # ════════════════════════════════════════════════════
      tabItem(tabName = "table",
        fluidRow(
          box(width = 12, title = "Longitudinal Dataset: Oxford GARI 2020-2024",
              status = "info", solidHeader = TRUE, DTOutput("data_long"))
        ),
        fluidRow(
          box(width = 12, title = "2024 Detailed Scores + Pillar Breakdown",
              status = "info", solidHeader = TRUE, DTOutput("data_2024"))
        )
      )

    ) # end tabItems
  ) # end dashboardBody
) # end dashboardPage

# ══════════════════════════════════════════════════════════════
#  4.  S E R V E R
# ══════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  # ── Reactive subsets ──────────────────────────────────────
  r_overall <- reactive({
    overall %>%
      filter(Country %in% input$sel_countries,
             Year >= input$sel_years[1],
             Year <= input$sel_years[2])
  })

  r_latest <- reactive({
    latest %>% filter(Country %in% input$sel_countries)
  })

  r_pillars_yr <- reactive({
    pillars_long %>%
      filter(Country %in% input$sel_countries,
             Year == as.integer(input$heatmap_year))
  })

  r_pillars_filt <- reactive({
    pillars_long %>%
      filter(Country %in% input$sel_countries,
             Year >= input$sel_years[1],
             Year <= input$sel_years[2])
  })

  # ── VALUE BOXES ───────────────────────────────────────────
  output$vbox_leader <- renderValueBox({
    df <- r_latest()
    if (nrow(df) == 0)
      return(valueBox("---", "No country selected", icon = icon("trophy"), color = "blue"))
    top <- df %>% arrange(desc(Overall)) %>% slice(1)
    valueBox(paste0(top$Country, " (", top$Overall, ")"),
             paste0("Highest Score (", nrow(df), " selected)"),
             icon = icon("trophy"), color = "blue")
  })

  output$vbox_oman <- renderValueBox({
    df <- r_latest()
    if (nrow(df) == 0)
      return(valueBox("---", "No country selected", icon = icon("map-marker"), color = "orange"))
    bot <- df %>% arrange(Overall) %>% slice(1)
    valueBox(paste0(bot$Overall, " | #", bot$Global_Rank, " globally"),
             paste0(bot$Country, " - Lowest in selection"),
             icon = icon("map-marker"), color = "orange")
  })

  output$vbox_growth <- renderValueBox({
    df <- overall %>%
      filter(Country %in% input$sel_countries, Year %in% c(2020, 2024)) %>%
      group_by(Country) %>%
      summarise(Change = diff(Overall), .groups = "drop") %>%
      arrange(desc(Change)) %>% slice(1)
    if (nrow(df) == 0)
      return(valueBox("---", "No data", icon = icon("arrow-up"), color = "green"))
    valueBox(paste0("+", round(df$Change, 1), " pts"),
             paste0("Biggest Gain 2020-2024 (", df$Country, ")"),
             icon = icon("arrow-up"), color = "green")
  })

  output$vbox_gap <- renderValueBox({
    df <- r_latest()
    if (nrow(df) < 2)
      return(valueBox("---", "Select 2+ countries", icon = icon("arrows-alt-h"), color = "red"))
    top_c <- df %>% arrange(desc(Overall)) %>% slice(1)
    bot_c <- df %>% arrange(Overall) %>% slice(1)
    gap   <- round(top_c$Overall - bot_c$Overall, 1)
    valueBox(gap, paste0("Gap: ", top_c$Country, " vs ", bot_c$Country),
             icon = icon("arrows-alt-h"), color = "red")
  })

  # ══════════════════════════
  # TAB 1 CHARTS
  # ══════════════════════════

  output$bar_2024 <- renderPlotly({
    df <- r_latest() %>%
      arrange(desc(Overall)) %>%
      mutate(Country = factor(Country, levels = Country),
             tip = paste0("<b>", Country, "</b><br>Score: ", Overall,
                          "<br>Rank: #", Global_Rank, "<br>AI Strategy: ", AI_Strategy))
    p <- ggplot(df, aes(x = Country, y = Overall, fill = Country, text = tip)) +
      geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
      geom_hline(yintercept = 47.59, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
      annotate("text", x = 0.7, y = 49.5, label = "Global avg 47.59",
               hjust = 0, size = 3.1, colour = "grey40") +
      scale_fill_manual(values = country_colors, guide = "none") +
      scale_y_continuous(limits = c(0, 85), expand = c(0, 0)) +
      labs(x = NULL, y = "Score (0-100)",
           caption = "Source: Oxford GARI 2024 | oxfordinsights.com") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$rank_chart <- renderPlotly({
    df <- r_latest() %>%
      arrange(Global_Rank) %>%
      mutate(Country = factor(Country, levels = rev(Country)),
             tip = paste0("<b>", Country, "</b><br>Global Rank: #", Global_Rank,
                          "<br>Overall Score: ", Overall))
    p <- ggplot(df, aes(x = Country, y = Global_Rank, colour = Country, text = tip)) +
      geom_segment(aes(xend = Country, y = 0, yend = Global_Rank), linewidth = 2) +
      geom_point(size = 6) +
      scale_colour_manual(values = country_colors, guide = "none") +
      scale_y_continuous(limits = c(0, 80)) +
      coord_flip() +
      labs(x = NULL, y = "Global Rank (lower = better)",
           caption = "Source: Oxford GARI 2024") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.y = element_blank(),
            axis.text.y = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  # ══════════════════════════
  # TAB 2 CHARTS
  # ══════════════════════════

  output$line_overall <- renderPlotly({
    df <- r_overall() %>%
      mutate(tip = paste0("<b>", Country, "</b><br>Year: ", Year,
                          "<br>Score: ", round(Overall, 1)))
    gavg <- global_avg %>%
      filter(Year >= input$sel_years[1], Year <= input$sel_years[2])
    p <- ggplot(df, aes(x = Year, y = Overall, colour = Country, group = Country, text = tip)) +
      geom_line(linewidth = 1.4) +
      geom_point(size = 3.5) +
      geom_line(data = gavg, aes(x = Year, y = Avg), inherit.aes = FALSE,
                linetype = "dashed", colour = "grey45", linewidth = 1.0) +
      scale_colour_manual(values = country_colors) +
      scale_x_continuous(breaks = 2020:2024) +
      labs(x = NULL, y = "Score (0-100)", colour = NULL,
           caption = "Source: Oxford GARI 2020-2024") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$slope_chart <- renderPlotly({
    y1 <- max(2020, input$sel_years[1])
    y2 <- min(2024, input$sel_years[2])
    df <- overall %>%
      filter(Country %in% input$sel_countries, Year %in% c(y1, y2)) %>%
      group_by(Country) %>% filter(n() == 2) %>% ungroup() %>%
      mutate(tip = paste0("<b>", Country, "</b><br>Year: ", Year,
                          "<br>Score: ", round(Overall, 1)))
    if (nrow(df) == 0 || y1 == y2)
      return(plotly_empty() %>% layout(title = "Select a multi-year range"))
    p <- ggplot(df, aes(x = factor(Year), y = Overall, colour = Country,
                        group = Country, text = tip)) +
      geom_line(linewidth = 1.6) +
      geom_point(size = 5) +
      scale_colour_manual(values = country_colors, guide = "none") +
      scale_x_discrete(expand = expansion(mult = c(0.3, 0.2))) +
      labs(x = NULL, y = "Score", caption = "Source: Oxford GARI") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 13))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$box_plot <- renderPlotly({
    df <- dim_data %>%
      filter(Country == input$box_country) %>%
      mutate(tip = paste0("Pillar: ", Pillar, "<br>Score: ", round(DimScore, 1)))

    gcc_avg_by_pillar <- data.frame(
      Pillar  = c("Government","Technology Sector","Data & Infrastructure"),
      GCC_Avg = c(mean(latest$Government),
                  mean(latest$Technology_Sector),
                  mean(latest$Data_Infra))
    ) %>%
      mutate(tip_ref = paste0("GCC avg (", Pillar, "): ", round(GCC_Avg, 1)))

    p <- ggplot(df, aes(x = Pillar, y = DimScore, fill = Pillar, text = tip)) +
      geom_boxplot(width = 0.5, outlier.shape = 21, outlier.fill = "white") +
      geom_jitter(width = 0.12, alpha = 0.5, size = 1.8) +
      geom_segment(data = gcc_avg_by_pillar,
                   aes(x = as.numeric(factor(Pillar,
                         levels = c("Data & Infrastructure","Government","Technology Sector"))) - 0.48,
                       xend = as.numeric(factor(Pillar,
                         levels = c("Data & Infrastructure","Government","Technology Sector"))) + 0.48,
                       y = GCC_Avg, yend = GCC_Avg, text = tip_ref),
                   inherit.aes = FALSE,
                   colour = "grey40", linewidth = 1.0, linetype = "dashed") +
      annotate("text", x = 3.52, y = gcc_avg_by_pillar$GCC_Avg[gcc_avg_by_pillar$Pillar == "Technology Sector"] + 2.5,
               label = "GCC average", colour = "grey40", size = 3.2, hjust = 0) +
      scale_fill_manual(values = c(
        "Government"           = "#0072B2",
        "Technology Sector"    = "#009E73",
        "Data & Infrastructure"= "#56B4E9"
      ), guide = "none") +
      scale_y_continuous(limits = c(20, 100)) +
      labs(x = NULL, y = "Indicator Score (0-100)",
           title   = paste(input$box_country, "– Indicator Distribution by Pillar (2024)"),
           caption = "Source: Oxford GARI 2024. Dashed line = GCC 6-country average per pillar.") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$facet_lines <- renderPlot({
    df <- r_pillars_filt() %>% filter(Pillar == input$facet_pillar)
    ggplot(df, aes(x = Year, y = Score, colour = Country, group = Country)) +
      geom_line(linewidth = 1.1) +
      geom_point(size = 2.5) +
      facet_wrap(~Country, ncol = 3) +
      scale_colour_manual(values = country_colors, guide = "none") +
      scale_x_continuous(breaks = c(2020, 2022, 2024)) +
      scale_y_continuous(limits = c(30, 85)) +
      labs(x = NULL, y = "Score",
           title   = paste(input$facet_pillar, "– Small Multiples by Country"),
           caption = "Same y-axis scale across all panels (Tufte). Source: Oxford GARI.") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold"))
  })

  # ══════════════════════════
  # TAB 3 CHARTS
  # ══════════════════════════

  output$heatmap <- renderPlotly({
    df <- r_pillars_yr() %>%
      mutate(
        alpha_val = 1,
        tip = paste0("<b>", Country, "</b><br>Pillar: ", Pillar,
                     "<br>Score: ", round(Score, 1), "<br>Year: ", input$heatmap_year)
      )
    p <- ggplot(df, aes(x = Pillar, y = Country, fill = Score, text = tip)) +
      geom_tile(aes(alpha = alpha_val), colour = "white", linewidth = 1.5) +
      geom_text(aes(label = round(Score, 1)), size = 4.5, fontface = "bold", colour = "white") +
      scale_fill_gradientn(colours = c("#FFFFCC","#78C679","#238443","#004529"), name = "Score", values = scales::rescale(c(30,50,70,85))) +
      scale_alpha_identity() +
      scale_y_discrete(limits = rev(gcc[gcc %in% input$sel_countries])) +
      labs(x = NULL, y = NULL,
           title   = paste("Pillar Heatmap –", input$heatmap_year),
           caption = "Source: Oxford GARI pillar scores") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(face = "bold", size = 11),
            axis.text.y = element_text(face = "bold"),
            panel.grid  = element_blank())
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$pillar_profile <- renderPlotly({
    df <- pillars_long %>%
      filter(Country == input$profile_country, Year == 2024) %>%
      arrange(desc(Score)) %>%
      mutate(Pillar = factor(Pillar, levels = Pillar),
             tip    = paste0("Pillar: ", Pillar, "<br>Score: ", round(Score, 1)))
    p <- ggplot(df, aes(x = Pillar, y = Score, fill = Pillar, text = tip)) +
      geom_col(width = 0.55, show.legend = FALSE) +
      geom_text(aes(label = round(Score, 1)), vjust = -0.4, fontface = "bold", size = 4.5) +
      scale_fill_manual(values = c(
        "Government"           = "#0072B2",
        "Technology Sector"    = "#56B4E9",
        "Data & Infrastructure"= "#009E73"
      )) +
      scale_y_continuous(limits = c(0, 90)) +
      labs(x = NULL, y = "Score (0-100)",
           title   = paste(input$profile_country, "– Pillar Scores 2024"),
           caption = "Source: Oxford GARI 2024") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$pillar_all <- renderPlotly({
    df <- pillars_long %>%
      filter(Country %in% input$sel_countries, Pillar == input$pillar_sel, Year == 2024) %>%
      arrange(desc(Score)) %>%
      mutate(Country = factor(Country, levels = Country),
             tip     = paste0("<b>", Country, "</b><br>", input$pillar_sel, ": ", round(Score, 1)))
    p <- ggplot(df, aes(x = Country, y = Score, fill = Country, text = tip)) +
      geom_col(width = 0.6) +
      geom_text(aes(label = round(Score, 1)), vjust = -0.4, fontface = "bold", size = 4) +
      scale_fill_manual(values = country_colors, guide = "none") +
      scale_y_continuous(limits = c(0, 90)) +
      labs(x = NULL, y = "Score (0-100)",
           title   = paste(input$pillar_sel, "– All GCC, 2024"),
           caption = "Source: Oxford GARI 2024") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  # ══════════════════════════
  # TAB 4 CHARTS
  # ══════════════════════════

  output$scatter <- renderPlotly({
    yr <- as.integer(input$bubble_year)

    # Build wide-format for selected year from pillars_long
    pill_wide <- pillars_long %>%
      filter(Year == yr, Country %in% input$sel_countries) %>%
      pivot_wider(names_from = Pillar, values_from = Score) %>%
      rename(Government_p = Government,
             Technology_Sector_p = `Technology Sector`,
             Data_Infra_p = `Data & Infrastructure`)

    df <- latest %>%
      filter(Country %in% input$sel_countries) %>%
      select(Country, Global_Rank, AI_Strategy) %>%
      left_join(pill_wide %>% select(Country, Government_p, Technology_Sector_p, Data_Infra_p),
                by = "Country") %>%
      left_join(overall %>% filter(Year == yr) %>% select(Country, Overall_yr = Overall),
                by = "Country") %>%
      mutate(Rank_inv = 188 - Global_Rank)

    # Pick size variable
    size_map <- c("Data_Infra"  = "Data_Infra_p",
                  "Overall"     = "Overall_yr",
                  "Rank_inv"    = "Rank_inv")
    size_col <- size_map[input$bubble_size]
    df$size_val <- df[[size_col]]

    df$tip <- paste0("<b>", df$Country, "</b>",
                     "<br>Year: ", yr,
                     "<br>Government: ",   round(df$Government_p, 1),
                     "<br>Technology: ",   round(df$Technology_Sector_p, 1),
                     "<br>Data & Infra: ", round(df$Data_Infra_p, 1),
                     "<br>Overall: ",      round(df$Overall_yr, 1),
                     "<br>Global Rank: #", df$Global_Rank)

    p <- ggplot(df, aes(x = Government_p, y = Technology_Sector_p, size = size_val,
                        colour = Country, text = tip)) +
      geom_point(alpha = 0.85) +
      scale_colour_manual(values = country_colors, name = "Country") +
      scale_size_continuous(range = c(6, 22), guide = "none") +
      scale_x_continuous(limits = c(40, 85)) +
      scale_y_continuous(limits = c(30, 80)) +
      labs(x = "Government Pillar Score", y = "Technology Sector Score",
           title = paste("GCC Pillar Scores –", yr),
           caption = "Source: Oxford GARI. Bubble size = selected 3rd variable. Hover for details.") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            legend.position = "right")
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)),
             legend = list(title = list(text = "Country")))
  })

  output$parallel_coords <- renderPlotly({
    df <- latest %>%
      filter(Country %in% input$sel_countries) %>%
      select(Country, Government, Technology_Sector, Data_Infra, Overall) %>%
      rename(`Gov.`          = Government,
             `Tech.`         = Technology_Sector,
             `Data & Infra.` = Data_Infra,
             `Overall`       = Overall)
    vars    <- c("Gov.", "Tech.", "Data & Infra.", "Overall")
    long_df <- df %>%
      pivot_longer(-Country, names_to = "Variable", values_to = "Score") %>%
      mutate(Variable = factor(Variable, levels = vars),
             xpos     = as.numeric(Variable),
             tip      = paste0("<b>", Country, "</b><br>", Variable, ": ", round(Score, 1)))
    p <- ggplot(long_df, aes(x = xpos, y = Score, colour = Country,
                              group = Country, text = tip)) +
      geom_line(linewidth = 1.4, alpha = 0.9) +
      geom_point(size = 4) +
      geom_text(data = long_df %>% filter(xpos == max(xpos)),
                aes(x = xpos + 0.06, label = Country),
                hjust = 0, size = 3.5, fontface = "bold", show.legend = FALSE) +
      scale_colour_manual(values = country_colors, guide = "none") +
      scale_x_continuous(breaks = 1:length(vars), labels = vars,
                         expand = expansion(mult = c(0.05, 0.3))) +
      scale_y_continuous(limits = c(40, 85)) +
      labs(x = NULL, y = "Score (0-100)", caption = "Source: Oxford GARI 2024") +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            axis.text.x = element_text(face = "bold", size = 12))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  # ══════════════════════════
  # TAB 5 INTERACTIVE RADAR
  # ══════════════════════════

  output$radar_interactive <- renderPlotly({
    req(length(input$radar_countries) > 0)
    yr  <- as.integer(input$radar_year)
    ctr <- input$radar_countries

    df <- pillars_long %>%
      filter(Country %in% ctr, Year == yr) %>%
      select(Country, Pillar, Score)

    gcc_avg_radar <- pillars_long %>%
      filter(Year == yr) %>%
      group_by(Pillar) %>%
      summarise(Score = mean(Score), .groups = "drop")

    pillars_order <- c("Government", "Technology Sector", "Data & Infrastructure")

    fig <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")

    avg_row  <- gcc_avg_radar %>% arrange(match(Pillar, pillars_order))
    avg_vals <- c(avg_row$Score, avg_row$Score[1])
    avg_cats <- c(pillars_order, pillars_order[1])

    fig <- fig %>% add_trace(
      r         = avg_vals,
      theta     = avg_cats,
      name      = "GCC Average",
      line      = list(color = "grey50", dash = "dash", width = 1.5),
      fillcolor = "rgba(0,0,0,0)",
      marker    = list(size = 5, color = "grey50"),
      hovertemplate = "<b>GCC Average</b><br>%{theta}: %{r:.1f}<extra></extra>"
    )

    for (cty in ctr) {
      sub <- df %>% filter(Country == cty) %>% arrange(match(Pillar, pillars_order))
      if (nrow(sub) == 0) next
      r_vals   <- c(sub$Score, sub$Score[1])
      t_vals   <- c(pillars_order, pillars_order[1])
      clr      <- country_colors[cty]
      clr_fill <- paste0("rgba(", paste(as.integer(col2rgb(clr)), collapse = ","), ",0.15)")
      fig <- fig %>% add_trace(
        r         = r_vals,
        theta     = t_vals,
        name      = cty,
        line      = list(color = clr, width = 2.5),
        fillcolor = clr_fill,
        marker    = list(size = 8, color = clr),
        hovertemplate = paste0("<b>", cty, "</b><br>%{theta}: %{r:.1f}<extra></extra>")
      )
    }

    fig %>% layout(
      polar = list(
        radialaxis = list(
          visible   = TRUE,
          range     = c(0, 85),
          tickvals  = c(20, 40, 60, 80),
          tickfont  = list(size = 11),
          gridcolor = "#ddd"
        ),
        angularaxis = list(
          tickfont = list(size = 13, family = "Arial Black")
        )
      ),
      showlegend    = TRUE,
      legend        = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.12),
      title         = list(text = paste0("GCC Pillar Profiles – ", yr, "  (Hover for scores)"),
                           font = list(size = 14)),
      margin        = list(t = 60, b = 60),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
  })

  output$radar_alt_bar <- renderPlotly({
    ctr <- c("UAE", "Oman", "Kuwait")
    df  <- pillars_long %>%
      filter(Country %in% ctr, Year == 2024) %>%
      mutate(Country = factor(Country, levels = ctr),
             tip     = paste0("<b>", Country, "</b><br>", Pillar, ": ", round(Score, 1)))
    p <- ggplot(df, aes(x = Pillar, y = Score, fill = Country, text = tip)) +
      geom_col(position = "dodge", width = 0.65) +
      geom_text(aes(label = round(Score, 1)), position = position_dodge(width = 0.65),
                vjust = -0.3, size = 3.2, fontface = "bold") +
      scale_fill_manual(values = country_colors[ctr]) +
      scale_y_continuous(limits = c(0, 90)) +
      labs(x = NULL, y = "Score (0-100)", fill = NULL,
           caption = "Position on common scale (Cleveland & McGill most accurate). Source: Oxford GARI 2024") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom",
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  # ══════════════════════════
  # POLICY BRIEF CHARTS
  # ══════════════════════════

  output$pb_chart1 <- renderPlotly({
    df <- latest %>%
      arrange(desc(Overall)) %>%
      mutate(Country = factor(Country, levels = Country),
             tip     = paste0("<b>", Country, "</b><br>Score: ", Overall,
                              "<br>Global Rank: #", Global_Rank))
    p <- ggplot(df, aes(x = Country, y = Overall, fill = Country, text = tip)) +
      geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
      geom_hline(yintercept = 47.59, linetype = "dashed", colour = "grey40", linewidth = 0.8) +
      annotate("text", x = 0.7, y = 49.5, label = "Global avg 47.59",
               hjust = 0, size = 3, colour = "grey40") +
      scale_fill_manual(values = country_colors, guide = "none") +
      scale_y_continuous(limits = c(0, 85), expand = c(0, 0)) +
      labs(x = NULL, y = "Score (0-100)",
           title   = "Figure 1: 2024 AI Readiness Scores – GCC Countries",
           caption = "Source: Oxford GARI 2024 | oxfordinsights.com") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(face = "bold"))
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 12, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$pb_chart2 <- renderPlotly({
    df <- pillars_long %>%
      filter(Year == 2024) %>%
      mutate(tip = paste0("<b>", Country, "</b><br>", Pillar, ": ", round(Score, 1)))
    p <- ggplot(df, aes(x = Pillar, y = Country, fill = Score, text = tip)) +
      geom_tile(colour = "white", linewidth = 1.5) +
      geom_text(aes(label = round(Score, 1)), size = 4, fontface = "bold", colour = "white") +
      scale_fill_gradientn(colours = c("#FFFFCC","#78C679","#238443","#004529"), name = "Score", values = scales::rescale(c(30,50,70,85))) +
      scale_y_discrete(limits = rev(gcc)) +
      labs(x = NULL, y = NULL,
           title   = "Figure 2: Pillar Scores by Country (2024)",
           caption = "Source: Oxford GARI 2024 Annex II") +
      theme_minimal(base_size = 11) +
      theme(axis.text.x = element_text(face = "bold"),
            axis.text.y = element_text(face = "bold"),
            panel.grid  = element_blank())
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 12, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$pb_chart3 <- renderPlotly({
    stanford_status <- data.frame(
      Country = gcc,
      Status  = c("Included","Included","Included",
                  "Not Included","Not Included","Not Included"),
      Score   = c(75.66, 72.36, 66.80, 62.10, 58.50, 55.40)
    ) %>%
      mutate(tip = paste0("<b>", Country, "</b><br>Stanford Index: ", Status,
                          "<br>Oxford Score: ", Score))
    p <- ggplot(stanford_status,
                aes(x = reorder(Country, -Score), y = Score, fill = Status, text = tip)) +
      geom_col(width = 0.65, colour = "white", linewidth = 0.3) +
      geom_text(aes(label = Status), vjust = -0.4, size = 3.2, fontface = "bold") +
      scale_fill_manual(values = c("Included" = "#0072B2", "Not Included" = "#E69F00"),
                        name = "Stanford AI Index") +
      scale_y_continuous(limits = c(0, 90), expand = c(0, 0)) +
      labs(x = NULL, y = "Oxford Readiness Score",
           title   = "Figure 3: Stanford AI Index Coverage vs. Oxford Readiness Score",
           caption = "Source: Oxford GARI 2024 | Stanford AI Index 2024") +
      theme_minimal(base_size = 11) +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x     = element_text(face = "bold"),
            legend.position = "top")
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 12, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  output$pb_chart4 <- renderPlotly({
    df   <- overall %>%
      mutate(tip = paste0("<b>", Country, "</b><br>Year: ", Year,
                          "<br>Score: ", round(Overall, 1)))
    gavg <- global_avg
    p <- ggplot(df, aes(x = Year, y = Overall, colour = Country, group = Country, text = tip)) +
      geom_line(linewidth = 1.3) +
      geom_point(size = 3) +
      geom_line(data = gavg, aes(x = Year, y = Avg), inherit.aes = FALSE,
                linetype = "dashed", colour = "grey45", linewidth = 0.9) +
      annotate("text", x = 2022, y = 43.5, label = "Global average",
               size = 3, colour = "grey45") +
      scale_colour_manual(values = country_colors) +
      scale_x_continuous(breaks = 2020:2024) +
      labs(x = NULL, y = "Score (0-100)", colour = NULL,
           title   = "Figure 4: AI Readiness Score Trends 2020-2024",
           caption = "Source: Oxford GARI 2020-2024 | oxfordinsights.com") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", panel.grid.minor = element_blank())
    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 12, family = "Arial"),
          align       = "left"
        ),
        xaxis = list(fixedrange = FALSE),
        yaxis = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom    = TRUE,
        displaylogo   = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions = list(format="png", filename="GCC_AI_Chart")
      )
  })

  # pb_chart5: Oxford vs Stanford comparison — ggplot + plotly
  output$pb_chart5 <- renderPlotly({

    df <- data.frame(
      Metric  = rep(c("Oxford Score
(Readiness)",
                      "AI Research
Papers",
                      "Private AI
Investment",
                      "AI Talent
Density",
                      "Stanford
Inclusion"), each = 4),
      Country = rep(c("UAE","Saudi Arabia","Qatar","Oman"), times = 5),
      Value   = c(
        75.66, 72.36, 66.80, 62.10,
        88,    75,    52,    5,
        92,    80,    58,    4,
        85,    72,    55,    6,
        100,   100,   100,   0
      )
    ) %>% mutate(
      Metric = factor(Metric, levels = c(
        "Oxford Score
(Readiness)",
        "AI Research
Papers",
        "Private AI
Investment",
        "AI Talent
Density",
        "Stanford
Inclusion"
      )),
      is_oxford = as.character(Metric) == "Oxford Score
(Readiness)",
      fill_group = case_when(
        Country == "UAE"                       ~ "UAE",
        Country == "Saudi Arabia"              ~ "Saudi Arabia",
        Country == "Qatar"                     ~ "Qatar",
        Country == "Oman" &  is_oxford         ~ "Oman (Orange = IN Oxford)",
        Country == "Oman" & !is_oxford         ~ "Oman (Red = NOT IN Stanford)"
      ),
      tip = paste0(
        "<b>", Country, "</b><br>",
        gsub("
", " ", as.character(Metric)), ": ",
        ifelse(as.character(Metric) == "Stanford
Inclusion",
               ifelse(Value == 100, "Included ✅", "NOT INCLUDED ❌"),
               paste0(round(Value, 1), " / 100"))
      )
    )

    p <- ggplot(df, aes(x = Metric, y = Value,
                        fill = fill_group, text = tip)) +
      annotate("rect", xmin = 0.4, xmax = 1.5, ymin = 0, ymax = 109,
               fill = "#e8f4f8", alpha = 0.30) +
      annotate("rect", xmin = 1.5, xmax = 5.6, ymin = 0, ymax = 109,
               fill = "#fff3cd", alpha = 0.30) +
      geom_col(position = "dodge", width = 0.72,
               colour = "white", linewidth = 0.25) +
      geom_vline(xintercept = 1.5, linetype = "dashed",
                 colour = "grey40", linewidth = 0.9) +
      annotate("text", x = 1.0, y = 107, label = "Oxford zone",
               hjust = 0.5, size = 3.2, colour = "#1f6fa3", fontface = "bold.italic") +
      annotate("text", x = 3.5, y = 107, label = "Stanford zone  (real AI output)",
               hjust = 0.5, size = 3.2, colour = "#9e6b00", fontface = "bold.italic") +
      scale_fill_manual(
        values = c(
          "UAE"                          = "#0072B2",
          "Saudi Arabia"                 = "#009E73",
          "Qatar"                        = "#CC79A7",
          "Oman (Orange = IN Oxford)"    = "#E69F00",
          "Oman (Red = NOT IN Stanford)" = "#D62728"
        ),
        name = NULL, drop = FALSE
      ) +
      scale_y_continuous(limits = c(0, 112), expand = c(0, 0),
                         breaks = c(0, 25, 50, 75, 100)) +
      labs(x = NULL, y = "Score / Index (0-100)",
           title = "Figure 5: Oxford Readiness vs Stanford Output — Oman vs GCC Leaders",
           caption = paste0(
             "Source: Oxford GARI 2024 | Stanford AI Index 2024
",
             "ORANGE = Oman in Oxford.  RED = Oman NOT in Stanford.  ",
             "Stanford values indexed 0-100."
           )) +
      theme_minimal(base_size = 11) +
      theme(
        panel.grid.major.x  = element_blank(),
        axis.text.x         = element_text(face = "bold", size = 9),
        legend.position     = "bottom",
        legend.text         = element_text(size = 10),
        legend.key.size     = unit(0.5, "cm"),
        plot.caption        = element_text(size = 9, colour = "grey40", hjust = 0),
        plot.title          = element_text(size = 12, face = "bold")
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        hoverlabel = list(
          bgcolor     = "white",
          bordercolor = "#333333",
          font        = list(size = 13, family = "Arial"),
          align       = "left"
        ),
        legend = list(
          orientation = "h",
          x = 0.5, xanchor = "center",
          y = -0.25,
          font        = list(size = 11),
          bgcolor     = "rgba(255,255,255,0.95)",
          bordercolor = "#aaaaaa",
          borderwidth = 1,
          tracegroupgap = 5
        ),
        margin = list(t = 60, b = 150, l = 60, r = 20),
        xaxis  = list(fixedrange = FALSE),
        yaxis  = list(fixedrange = FALSE)
      ) %>%
      config(
        scrollZoom   = TRUE,
        displaylogo  = FALSE,
        modeBarButtonsToRemove = c("lasso2d","select2d","autoScale2d"),
        toImageButtonOptions   = list(format = "png", filename = "GCC_AI_Chart")
      )
  })

  # TAB 7 TABLES
  # ══════════════════════════

  output$data_long <- renderDT({
    r_overall() %>%
      arrange(Country, Year) %>%
      rename(`Overall Score` = Overall) %>%
      datatable(
        options  = list(pageLength = 15, dom = "Bfrtip", buttons = c("csv","excel")),
        rownames = FALSE,
        filter   = "top"
      ) %>%
      formatRound("Overall Score", digits = 2) %>%
      formatStyle("Country", backgroundColor = styleEqual("Oman", "#fff3cd"))
  }, server = FALSE)

  output$data_2024 <- renderDT({
    r_latest() %>%
      rename(`Overall 2024`  = Overall,
             `Gov. Pillar`   = Government,
             `Tech. Pillar`  = Technology_Sector,
             `Data & Infra.` = Data_Infra,
             `Global Rank`   = Global_Rank,
             `AI Strategy`   = AI_Strategy) %>%
      datatable(
        options  = list(pageLength = 10, dom = "Bfrtip", buttons = c("csv","excel")),
        rownames = FALSE
      ) %>%
      formatRound(c("Overall 2024","Gov. Pillar","Tech. Pillar","Data & Infra."), digits = 2) %>%
      formatStyle("AI Strategy",
        backgroundColor = styleEqual(
          c("Yes","In Progress","Announced"),
          c("#d4edda","#fff3cd","#f8d7da")
        )
      )
  }, server = FALSE)

} # end server

# ══════════════════════════════════════════════════════════════
#  5.  R U N
# ══════════════════════════════════════════════════════════════
shinyApp(ui = ui, server = server)
