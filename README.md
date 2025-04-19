# 📊 Plots & Summary Statistics Generator - R Shiny App

This interactive Shiny application enables users to upload CSV datasets and automatically explore them using various statistical summaries, visualizations, and correlation heatmaps.

## 🚀 Features

- **CSV Upload & Basic Info Viewer**  
  Upload your dataset and instantly view basic metadata such as dimensions, column names, and data types.

- **Data Table Preview**  
  Preview the top rows of your uploaded dataset in a clean, scrollable, and sortable table.

- **Summary Statistics**  
  Automatically compute and display summary statistics (mean, median, SD, etc.) for all numeric columns.

- **Custom Plot Generator**  
  Generate and customize:
  - Scatter Plots  
  - Box Plots  
  - Violin Plots  
  - Histograms  

  With options to choose:
  - Variables for axes  
  - Axis labels  
  - Plot theme  
  - Plot color  
  - Plot size  

  Download plots as PNG images.

- **Correlation Heatmap**  
  Generate a beautifully styled correlation matrix for numeric columns using `ggcorrplot`. Downloadable as PNG.

- **Univariate & Bivariate Analysis**  
  - View numeric and categorical summaries grouped by a selected target variable.  
  - Interactive tables with download options.  
  - Bivariate analysis (target vs other variables) in tabular form.

---

## ▶️ How to Run

1. **Clone this repository**:
   ```bash
   git clone https://github.com/Hemanth-Mydugolam/dynamic-plot-app.git
   cd dynamic-plot-app

2. **Open the app in RStudio or run from the R console**:
  ```'r
    shiny::runApp()

  Or just click Run App button present right corner of the source window.
