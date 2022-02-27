# Repeated Measures for two measures
library(shiny)
library(colourpicker)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
# Application title
  titlePanel("Repeated measures: 2 measures"),
    sidebarLayout(
    sidebarPanel(

# Data input
      radioButtons("dots_or_slopes", label = "Graph type", choices = list("slopes", "dots")),
      textAreaInput("myData", "DATA", "", width = 200, height = 200, placeholder = "[Paste, from spreadsheet, 2 columns of data with non-number labels in top row; optionally, add a 3rd column containing datapoint labels]"),
      
# Hacks
      tags$style("input[type='checkbox']:checked+span{font-weight:bold;}"), # hack to get checkboxes to show up bold when unchecked
      tags$style("input[type='checkbox']:not(:checked)+span{font-weight:bold;}"), # hack to get checkboxes to show up bold when unchecked
      tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # hack to remove minor ticks on sliders


# Options to select from
####### SLOPES ######
conditionalPanel(condition="input.dots_or_slopes=='slopes'",
                 selectInput(inputId="options_s", label="OPTIONS (slopes):",
                             choices=c("*** select ***" = "select",
                                       "Slope visibility" = "slopevisibility",
                                       "Transform data" = "perc_s",
                                       "Axes & grids" = "axes_s",
                                       "Stats" = "stats_s",
                                       "Text & plot size" = "labels_s"),
                             selected = NULL)
                 ),
####### DOTS ######
conditionalPanel(condition="input.dots_or_slopes=='dots'",
                 selectInput(inputId="options", label="OPTIONS (dots):",
                             choices=c("*** select ***" = "select",
                                       "Data point visibility" = "dotvisibility",
                                       "Transform data" = "perc",
                                       "Reference lines" = "fit",
                                       "Axes & grids" = "axes",
                                       "Stats" = "stats",
                                       "Text & plot size" = "labels"),
                             selected = NULL)
                 ),

# SLOPES -- Data slope visibility
conditionalPanel(condition="input.options_s=='slopevisibility' & input.dots_or_slopes=='slopes'",
                 colourInput(inputId="color_dot_s", label=NULL, value = "black", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                 sliderInput(inputId = "ind_linewidths",
                             label = "individual line width",
                             min = 1,
                             max = 100,
                             value = 75),
                 sliderInput(inputId = "xjitter_s",
                             label = HTML("variable 1 jitter<sup>2</sup>"),
                             min = 0,
                             max = 100,
                             value = 0),
                 sliderInput(inputId = "yjitter_s",
                             label = HTML("variable 2 jitter<sup>2</sup>"),
                             min = 0,
                             max = 100,
                             value = 0),
                 sliderInput(inputId = "dotopacity_s",
                             label = "line opacity",
                             min = 0,
                             max = 100,
                             value = 50)
                 ),
# DOTS -- Data point visibility
conditionalPanel(condition="input.options=='dotvisibility' & input.dots_or_slopes=='dots'",
                 colourInput(inputId="color_dot", label=NULL, value = "black", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = FALSE, returnName = TRUE),
                 radioButtons("dottype", label = "point type", choiceNames = list("dot", "ring"), choiceValues = list("16", "1")),
                 sliderInput(inputId = "dotsize",
                             label = "individual point size",
                             min = 1,
                             max = 100,
                             value = 75),
                 sliderInput(inputId = "xjitter",
                             label = HTML("x jitter<sup>2</sup>"),
                             min = 0,
                             max = 100,
                             value = 0),
                 sliderInput(inputId = "yjitter",
                             label = HTML("y jitter<sup>2</sup>"),
                             min = 0,
                             max = 100,
                             value = 0),
                 sliderInput(inputId = "dotopacity",
                             label = "point opacity",
                             min = 0,
                             max = 100,
                             value = 50),
                 conditionalPanel(condition="input.dottype=='1'",
                                  sliderInput(inputId = "ringlw",
                                              label = "ring boldness",
                                              min = 1,
                                              max = 100,
                                              value = 20)
                                 )
                 ),

# SLOPES -- Transform data
conditionalPanel(condition="input.options_s=='perc_s' & input.dots_or_slopes=='slopes'",
                 checkboxInput('spearman_s', 'percentile ranks', FALSE)
),

# DOTS -- Transform data
conditionalPanel(condition="input.options=='perc' & input.dots_or_slopes=='dots'",
                 checkboxInput('spearman', 'percentile ranks', FALSE)
),


# DOTS -- Reference lines (none for SLOPES)
conditionalPanel(condition="input.options=='fit' & input.dots_or_slopes=='dots'",
                 checkboxInput('xyline', 'reference line (x=y)', TRUE),
                 conditionalPanel(condition="input.xyline",
                                  colourInput(inputId="xyline_color", label=NULL, value = "black", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  sliderInput(inputId = "lw_xyline",
                                              label = HTML("<span style='font-weight:normal;'>line width</span>"),
                                              min = 0,
                                              max = 100,
                                              value = 40)
                 ),
                 checkboxInput('lsline', 'fit line (least-squares)', FALSE),
                 conditionalPanel(condition="input.lsline",
                                  colourInput(inputId="lsline_color", label=NULL, value = "gray", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  sliderInput(inputId = "lw_lsline",
                                              label = HTML("<span style='font-weight:normal;'>line width</span>"),
                                              min = 0,
                                              max = 100,
                                              value = 40)
                 )
),


# SLOPES -- Axes & grids
conditionalPanel(condition="input.options_s=='axes_s' & input.dots_or_slopes=='slopes'",
                 textInput("axisranges_s", label = "y axis range", value = "", width = "50%", placeholder = "min,max"),
                 checkboxInput('addgrid_s', 'add grid lines', FALSE)
),

# DOTS -- Axes & grids
conditionalPanel(condition="input.options=='axes' & input.dots_or_slopes=='dots'",
                 textInput("axisranges", label = "axis ranges", value = "", width = "50%", placeholder = "min,max"),
                 checkboxInput('addgrid', 'add grid lines', FALSE),
                 checkboxInput('rug', 'project data onto axes', FALSE)
),


# SLOPES -- Stats
conditionalPanel(condition="input.options_s=='stats_s' & input.dots_or_slopes=='slopes'",
                 checkboxInput('addmean_s', 'mean', TRUE),
                 conditionalPanel(condition="input.addmean_s",
                                  colourInput(inputId="color_mean_s", label=NULL, value = "blue", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  sliderInput(inputId = "mean_linewidth_s",
                                              label = HTML("<span style='font-weight:normal;'>line width</span>"),
                                              min = 1,
                                              max = 100,
                                              value = 75),
                                  sliderInput(inputId = "meansize_s",
                                              label = HTML("<span style='font-weight:normal;'>point size</span>"),
                                              min = 1,
                                              max = 100,
                                              value = 1)
                                  #checkboxInput('add95ci_s', HTML("<span style='font-weight:normal;'>95% confidence interval (CI)<sup>1</sup></span>"), FALSE),
                                  #conditionalPanel(condition="input.add95ci",
                                  #                 colourInput(inputId="color_ci_s", label=NULL, value = "red", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  #                 sliderInput(inputId = "ci_lw_s",
                                  #                             label = HTML("<span style='font-weight:normal;'>line width</span>"),
                                  #                             min = 1,
                                  #                             max = 100,
                                  #                             value = 40)
                                  #)
                 ),
                 hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                 checkboxInput('addmedian_s', 'median', FALSE),
                 conditionalPanel(condition="input.addmedian_s",
                                  colourInput(inputId="color_median_s", label=NULL, value = "purple", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  sliderInput(inputId = "median_linewidth_s",
                                              label = HTML("<span style='font-weight:normal;'>line width</span>"),
                                              min = 1,
                                              max = 100,
                                              value = 75),
                                  sliderInput(inputId = "mediansize_s",
                                              label = HTML("<span style='font-weight:normal;'>point size</span>"),
                                              min = 1,
                                              max = 100,
                                              value = 1)
                 ),
                 hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                 numericInput("digits_s", label = "digits to right of decimal", value = 2, 0, 15, 1)  
),


# DOTS -- Stats
conditionalPanel(condition="input.options=='stats' & input.dots_or_slopes=='dots'",
                 checkboxInput('addmean', 'mean', TRUE),
                 conditionalPanel(condition="input.addmean",
                                  colourInput(inputId="color_mean", label=NULL, value = "blue", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  sliderInput(inputId = "meansize",
                                              label = HTML("<span style='font-weight:normal;'>point size</span>"),
                                              min = 1,
                                              max = 100,
                                              value = 40),
                                  checkboxInput('add95ci', HTML("<span style='font-weight:normal;'>95% confidence interval (CI)<sup>1</sup></span>"), FALSE),
                                  conditionalPanel(condition="input.add95ci",
                                                   colourInput(inputId="color_ci", label=NULL, value = "red", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                                   sliderInput(inputId = "ci_lw",
                                                               label = HTML("<span style='font-weight:normal;'>line width</span>"),
                                                               min = 1,
                                                               max = 100,
                                                               value = 40)
                                  )
                 ),
                 hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                 checkboxInput('addmedian', 'median', FALSE),
                 conditionalPanel(condition="input.addmedian",
                                  colourInput(inputId="color_median", label=NULL, value = "purple", showColour = c("both"), palette = c("square"), allowedCols = NULL, allowTransparent = TRUE, returnName = TRUE),
                                  sliderInput(inputId = "mediansize",
                                              label = HTML("<span style='font-weight:normal;'>point size</span>"),
                                              min = 1,
                                              max = 100,
                                              value = 40)
                 ),
                 hr(style = "margin: 0px 30px 10px 30px; border: .5px solid #a6a6a6"),
                 numericInput("digits", label = "digits to right of decimal", value = 2, 0, 15, 1)  
),

# SLOPES -- Text & plot size
conditionalPanel(condition="input.options_s=='labels_s' & input.dots_or_slopes=='slopes'",
                 #checkboxInput('showstats_s', 'show stats on plot', TRUE),
                 textInput("graphtitle_s", label = "Title", width = "50%", placeholder = "[title]"), 
                 textAreaInput("xvariablelabel_s", label = "x variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
                 textAreaInput("yvariablelabel_s", label = "y variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
                 textAreaInput("measurelabel_s", label = "measure label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
                 sliderInput(inputId = "plotsize_s",
                             label = "plot size",
                             min = 0,
                             max = 100,
                             value = 75) 
),

# DOTS -- Text & plot size
conditionalPanel(condition="input.options=='labels' & input.dots_or_slopes=='dots'",
                 #checkboxInput('showstats', 'show stats on plot', TRUE),
                 textInput("graphtitle", label = "Title", width = "50%", placeholder = "[title]"), 
                 textAreaInput("xvariablelabel", label = "x variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
                 textAreaInput("yvariablelabel", label = "y variable label", value = "", width = "100%", rows = "2", placeholder = "Use [return] to split label"),
                 sliderInput(inputId = "plotsize",
                             label = "plot size",
                             min = 0,
                             max = 100,
                             value = 75) 
)

),

    
# Show a plot of the generated distribution
mainPanel(
          uiOutput('ui_plot'),
      
          tags$h6(HTML(" ")),
          
          conditionalPanel(condition="input.dots_or_slopes=='slopes'",
                           downloadButton(outputId = "down_s", label = "Download graph as..."),
                           radioButtons("filetype_s", label = NULL, choices = list("pdf", "png")),
                           hr(style = "margin: 20px 30px 20px 30px; border: .5px solid #a6a6a6"),
                           tags$h6(HTML(" ")),
                           tags$h4("Statistics"),
                           htmlOutput("meantext_s"),
                           htmlOutput("mediantext_s"),
                           htmlOutput("meandifftext_s"),
                           htmlOutput("cohensdtext_raw_s"),
                           htmlOutput("cohensdtext_unbiased_s"),
                           htmlOutput("ntext_s"),
                           htmlOutput("corrtext_s"),
                           
                           #hr(style = "margin: 0px 30px 20px 30px; border: .5px solid #a6a6a6"),
                           tags$h6(HTML(" "))
          ),
          conditionalPanel(condition="input.dots_or_slopes=='dots'",
                           downloadButton(outputId = "down", label = "Download graph as..."),
                           radioButtons("filetype", label = NULL, choices = list("pdf", "png")),
                           hr(style = "margin: 20px 30px 20px 30px; border: .5px solid #a6a6a6"),
                           tags$h6(HTML(" ")),
                           tags$h4("Statistics"),
                           htmlOutput("meantext"),
                           htmlOutput("mediantext"),
                           htmlOutput("meandifftext"),
                           htmlOutput("cohensdtext_raw"),
                           htmlOutput("cohensdtext_unbiased"),
                           htmlOutput("ntext"),
                           htmlOutput("corrtext"),
                           
                           hr(style = "margin: 0px 30px 20px 30px; border: .5px solid #a6a6a6"),
                           tags$h6(HTML(" ")),
                           
                           checkboxInput("showdifferences", HTML("<span style='font-weight:normal;'>Show y-x differences</span>"), FALSE),
          ),

          hr(style = "margin: 0px 30px 20px 30px; border: .5px solid #a6a6a6"),
          tags$h6(HTML(" ")),
      
          tags$h5("Notes..."),
          tags$h6(HTML("1. 95% confidence interval (95% CI): best thought of as the 95% CI on the mean x-y difference, where x-y differences can be represented graphically as the vertical distance of each data point from the reference line where x=y and y-x=0; note the conceptual distinction between this <u>CI on a mean of difference</u> versus the <u>CI on the difference between means</u> that is used in independent groups analyses.")),
          tags$h6("2. Primacy of non-jittered data: all lines are fit, and all statistics are computed, using non-jittered data."),
          tags$h6("3. Jitter unit: unit is percentage of smallest distance between two dots, calculated separately for each variable; each point is randomly jittered over a range equal to this unit."),
          tags$h6("4. Cohen's d with CIs: computed via lambda prime approach described in Cousineau & Goulet-Pelletier (2021) and advocated at http://bit.ly/dwcis."),
          tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }")
    )
  )
))
