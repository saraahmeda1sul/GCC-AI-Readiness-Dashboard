# GCC AI Readiness Dashboard
### INFS 4475 – Group Project | Sultan Qaboos University | SP2026
**Theme B: AI & Digital Readiness – Where Does the GCC Stand?**

---

##  Project Overview

This interactive Shiny dashboard analyses AI readiness across the six GCC countries (UAE, Saudi Arabia, Qatar, Oman, Bahrain, and Kuwait) using the **Oxford Government AI Readiness Index (2020–2024)**.

**Working Claim:** GCC countries rank well above the global average on the Oxford AI Readiness Index, yet the gap between UAE/Saudi Arabia and Oman/Kuwait has widened since 2020, revealing uneven AI investment within the region.

**Target Audience:** Ministry policy advisors

---

##  Dashboard Tabs

| Tab | Description |
|-----|-------------|
| **Overview** | 2024 bar chart + global rank lollipop chart + 4 value boxes |
| **Trends & Distribution** | Line chart (2020–2024), slope graph, box plot, small multiples |
| **Pillar Breakdown** | Heatmap, pillar profile bar chart, single-pillar comparison |
| **Multivariate** | Interactive bubble chart (with year slider) + parallel coordinates |
| **Radar Chart** | Interactive radar with year slider + bar chart comparison |
| **Policy Brief** | Full policy brief with 4 insights and 4 recommendations |
| **Data Explorer** | Searchable/downloadable data tables |

---

## Required R Packages

Before running, install all required packages by running this in RStudio:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "tidyr",
  "DT",
  "scales",
  "plotly"
))
```

---

##  How to Run

1. Download `app.R` from this repository
2. Open it in **RStudio**
3. Click the **Run App** button at the top right, or run in the console:

```r
shiny::runApp()
```

The dashboard will open in your browser automatically.

---

## Data Sources

| Source | URL | License |
|--------|-----|---------|
| Oxford Government AI Readiness Index (2020–2024) | [oxfordinsights.com](https://oxfordinsights.com) | CC BY-SA 4.0 |
| Stanford AI Index 2024 | [aiindex.stanford.edu](https://aiindex.stanford.edu) | — |
| World Bank | [data.worldbank.org](https://data.worldbank.org) | Open |
| ITU DataHub | [datahub.itu.int](https://datahub.itu.int) | Open |

---

##  Design Principles Applied

- **Bertin (1983):** Hue used to encode country identity; value used in heatmap
- **Cleveland & McGill (1984):** Position on common scale for bar and line charts
- **Tufte (2001):** High data-ink ratio; small multiples; reference lines for context
- **Colorblind-safe palette:** Wong (2011) — *Nature Methods* 8(6), 441

---

##  Repository Structure

```
 ai-readiness-dashboard
 ┣ app.R          ← Main Shiny application (self-contained)
 ┗  README.md      ← This file
```

> The app is fully self-contained in a single `app.R` file. No external data files needed.

---

##  Team

**Course:** INFS 4475 – Information Visualization  
**Institution:** Sultan Qaboos University  
**Semester:** SP2026  
**Members:** Sara Al-Suliemani, Laiyan Al-Yaarubi, Reem Al-Hinai

---

##  Live Demo

> Deployed on shinyapps.io: *(add your link here after publishing)

*Data accessed May 2026 | Oxford GARI CC BY-SA 4.0*
