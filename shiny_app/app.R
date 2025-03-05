#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
library(shiny)
library(tidyverse)
library(dplyr)
library(survey)
library(ggplot2)
library(ggalluvial)
library(networkD3)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(ggmosaic)
library(sf)
library(sp)
library(spData)
library(ggthemes)
library(arrow)
library(dbplyr, warn.conflicts = FALSE)
library(bslib)
library(gganimate)
library(gifski)
library(ggpie)
library(transformr)
library(shinydashboard)

# Load data
gss_mh_2k <- read.csv("gss_mh_2k.csv")
hexgrid <- read_sf("us_states_hexgrid.geojson")
facilities18 <- read.csv("facilities18.csv")
facilities22 <- read.csv("facilities22.csv")
combined_facilities <- read.csv("combined_facilities.csv")
mhd_18 <- open_dataset("mhd_18")
mhd_22 <- open_dataset("mhd_22")
client2018 <- open_dataset("client2018")
client2022 <- open_dataset("client2022")
avg_mhd_st_18 <- read.csv("avg_mhd_st_18.csv")
avg_mhd_st_22 <- read.csv("avg_mhd_st_22.csv")
total_sud_st_18 <- read.csv("total_sud_st_18.csv")
total_sud_st_22 <- read.csv("total_sud_st_22.csv")
perc_sud_st_diff <- read.csv("perc_sud_st_diff.csv")
avg_mhd_st_diff <- read.csv("avg_mhd_st_diff.csv")
perc_sud_st_18 <- read.csv("perc_sud_st_18.csv")
perc_sud_st_22 <- read.csv("perc_sud_st_22.csv")

#rename gss variables
gss_mh_2k <- gss_mh_2k %>% rename("Region" = "region1",
                                  "Gender" = "sex1",
                                  "AgeGroup" = "age_group",
                                  "Race" = "race1", 
                                  "Degree" = "Degree")

gss_mh_2k <- gss_mh_2k %>% mutate(depress1 = case_when(depress == 1 ~ "Depression",
                                                       depress == 2 ~ "No Depression",
                                                       TRUE ~ "No Response"))

mhd_18 <- mhd_18 %>% mutate(veteran_status = case_when(veteran_status == "Yes" ~ "Veteran",
                                                       veteran_status == "No" ~ "Not a veteran"))

mhd_22 <- mhd_22 %>% mutate(veteran_status = case_when(veteran_status == "Yes" ~ "Veteran",
                                                       veteran_status == "No" ~ "Not a veteran"))

client2018 <- client2018 %>% mutate(veteran_status = case_when(veteran_status == "Yes" ~ "Veteran",
                                                       veteran_status == "No" ~ "Not a veteran"))

client2022 <- client2022 %>% mutate(veteran_status = case_when(veteran_status == "Yes" ~ "Veteran",
                                                               veteran_status == "No" ~ "Not a veteran"))

# Design the survey
options(survey.lonely.psu = "adjust")
gss_design <- svydesign(
  ~ vpsu,
  strata = ~ interaction(year, vstrat),
  data = gss_mh_2k,
  weights = ~ wtsscomp,
  nest = TRUE
)
dep_18 <- gss_mh_2k %>% filter(year == 2018) %>% drop_na(AgeGroup)
options( survey.lonely.psu = "adjust" )
dep_18_design <- svydesign(
  ~ vpsu ,
  strata = ~ interaction( year , vstrat ) ,
  data = dep_18,
  weights = ~ wtsscomp ,
  nest = TRUE)
dep_22 <- gss_mh_2k %>% filter(year == 2022) %>% drop_na(AgeGroup, Gender, Race)
options( survey.lonely.psu = "adjust" )
dep_22_design <- svydesign(
  ~ vpsu ,
  strata = ~ interaction( year , vstrat ) ,
  data = dep_22,
  weights = ~ wtsscomp ,
  nest = TRUE)

# define legend reference data
legend_ref <- data.frame(legend = c("Region", "Age Group","Gender","Race", "Degree"),
                         col_name = c("Region", "AgeGroup","Gender","Race", "Degree"))
boxplot_ref <- data.frame(legend = c("Region", "Age Group","Gender","Race"),
                          col_name = c("Region", "age_group","Gender","Race"))
map_ref <- data.frame(legend = c("Substance Use", "Mental Health"),
                      col_name = c("su", "mh"))
client_ref <- data.frame(legend = c("Region", "Age Group","Gender","Race", "Maritial Status", "Employment Status", "Residential Status", "Veteran Status"),
                         col_name = c("Region", "age_group","Gender","Race", "marital_status", "employment_status", "residential_status", "veteran_status"))

# define input choices data
var_choices <- c("Region" = "Region",
                 "Age Group" = "AgeGroup",
                 "Gender" = "Gender",
                 "Race" = "Race", 
                 "Degree" = "Degree")
var_choices_boxplot <- c("Region" = "Region",
                         "Age Group" = "age_group",
                         "Gender" = "Gender",
                         "Race" = "Race")
# define sankey color scale
color_scale_final <- 'd3.scaleOrdinal()
  .domain(["Midwest", "Northeast", "South", "West", "Black", "White","Other", "Depression", "No Depression", "No Response", "Female", "Male", "18-24", "25-44", "45-64", "65+", "Less than high school", "High School", "Associate/junior college", "Bachelor\'s", "Graduate"])
  .range(["#69B3A2", "steelblue", "purple", "orange", "pink", "red","yellow", "lightblue", "darkgreen", "darkred", "aquamarine", "brown2", "hotpink", "thistle", "coral2", "deepink4", "darkorange", "tan", "navy", "yellow4", "lightpink"])'
color_map_final <- c("Midwest" = "#69B3A2", "Northeast" = "steelblue", "South" = "purple", "West" = "orange",
                     "Black" = "pink4", "White" = "red","Other" = "yellow", "Depression" = "lightblue", "No Depression" = "darkgreen", "No Response" = "darkred", "Female" = "aquamarine",
                     "Male" = "brown2", "18-24" = "hotpink", "25-44" = "thistle", "45-64" = "coral2", "65+" = "deeppink4", "Less than \nhigh school" = "darkorange", "High school" = "tan", "Associate/\njunior college" = "navy", "Bachelor's" = "yellow4", "Graduate" = "lightpink")

# Define UI
ui <- fluidPage(
  theme = bs_theme(
    bg = "white",
    fg = "black",
    primary = "black",
    secondary = "#0072B2",
    success = "#009E73",
    base_font = font_google("Verdana"),
    code_font = font_google("Verdana")
  ),
  br(),
  fluidRow(titlePanel(strong("Analysis of Mental Health and Substance Use Disorders Pre- and Post-COVID-19")), align = "center"),
  fluidRow(h5("Published by: Bryanna Schaffer and Christie Song"), align = "center"),
  tabsetPanel(
    tabPanel(strong("Data Overview"),
             br(),
             mainPanel(width = 12,
                       h3(strong("Data Overview"), align = "center"),
                       textOutput("welcome"), 
                       br(),
                       h4(strong("Data Sources and Methodology"), align = "center"),
                       h5("General Social Survey (GSS) Data"), 
                       textOutput("gss"),
                       br(),
                       h5("Mental Health Client-Level Data (MH-CLD)"), 
                       textOutput("mhc"),
                       br(),
                       h5("National Substance Use and Mental Health Services Survey (N-SUMHSS)"),
                       textOutput("su"),
                       tags$div(
                         style = "display: flex; justify-content: space-between;",
                         tags$div(
                         tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQSnF6ndWXh7iIcdsDHLmXrQKYSGKA2iBOHvQ&s",
                                  heigh = 75, width = 125),
                         style = "bottom"
                       ),
                       tags$div(
                         tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTJyVIqYIy7aQjhayx2e-Z-Uz03IutOPTzDZA&s",
                                  height = 150, width = 125),
                         style = "bottom"
                       ),
                       tags$div(
                         tags$img(src = "https://ww2.amstat.org/meetings/jsm/2024/images/logo.png?v=2024",
                                  height = 125, width = 175),
                         style = "bottom"
                       )))
    ),
    tabPanel(strong("Mental Health Trends Over 20 Years"),
             br(),
             h3(strong("Analyzing Trends: Mental Health Patterns Across Demographic Subgroups"), align = "center"),
             textOutput("tab2_intro"),
             br(),
             fluidRow(
               column(4,
                      radioButtons("variable", strong("Select a Variable"),
                                  choices = c("Average Number of Poor Mental Health Past 30 Days" = "mntlhlth",
                                              "Average Number of Activity Limitation Past 30 Days" = "hlthdays",
                                              "Average Number of Miss Work for Health Past 30 Days" = "misswork"),
                                  inline = TRUE),
                      selectInput("subgroup", strong("Select a Subgroup"),
                                  choices = c("Region" = "Region",
                                              "Age Group" = "AgeGroup",
                                              "Gender" = "Gender",
                                              "Race" = "Race",
                                              "Degree" = "Degree"),
                                  selected = "Gender")
               ),
               conditionalPanel(condition = "input.variable.includes('mntlhlth')",
                                mainPanel(plotlyOutput("time_series_plot")),
                                wellPanel(style = "background-color: white; font-family: Verdana;
                                          border-color: white",
                                          fluidRow(uiOutput("tab2_mntlhlth")),
                                          col_widths = c(4,9))),
               conditionalPanel(condition = "input.variable.includes('hlthdays')",
                                mainPanel(plotlyOutput("time_series_plot_2")),
                                wellPanel(style = "background-color: white; font-family: Verdana;
                                          border-color: white",
                                          fluidRow(uiOutput("tab2_hlthdays")),
                                          col_widths = c(4,9))),
               conditionalPanel(condition = "input.variable.includes('misswork')",
                                mainPanel(plotlyOutput("time_series_plot_3")),
                                wellPanel(style = "background-color: white; font-family: Verdana;
                                          border-color: white",
                                          fluidRow(uiOutput("tab2_misswork")),
                                          col_widths = c(4,9)))
             )),
    tabPanel(strong("Depression"),
             br(),
             h3(strong("Examining Depression Trends Across Different Groups"), align = "center"),
             uiOutput("tab3_intro"),
             wellPanel(style = "font-family: Verdana; border-color: black; background-color: white",
                       fluidRow(
                         column(4,
                                checkboxGroupInput("plot1", strong("Select a Plot Type"),
                                         choices = c("Sankey Diagram" = "sankey",
                                                     "Mosaic Plot" = "mosaic"),
                                         selected = "sankey")),
                         column(4,
                                selectInput("var1", strong("Select the First Variable"),
                                            choices = c("Region" = "Region",
                                                        "Age Group" = "AgeGroup",
                                                        "Gender" = "Gender",
                                                        "Race" = "Race",
                                                        "Degree" = "Degree"))),
                         column(4,
                                selectInput("var2", strong("Select a Different Second Variable"),
                                            choices = c("Age Group" = "AgeGroup",
                                                        "Region" = "Region",
                                                        "Gender" = "Gender",
                                                        "Race" = "Race",
                                                        "Degree" = "Degree")))
             )),
             br(),
             fluidRow(
               column(6, h3(strong("2018"))),
               column(6, h3(strong("2022")))
             ),
             conditionalPanel(condition = "input.plot1.includes('sankey')",
                              fluidRow(
                                column(6,
                                       sankeyNetworkOutput("depression_sankey_plot_18")
                                       
                                ),
                                column(6,
                                       sankeyNetworkOutput("depression_sankey_plot_22")
                                )
                              )
             ),
             conditionalPanel(condition = "input.plot1.includes('mosaic')",
                              fluidRow(
                                column(6,
                                       plotlyOutput("mosaic_plot_18")
                                       
                                ),
                                column(6,
                                       plotlyOutput("mosaic_plot_22")
                                )
                              )
             )
    ),
    navbarMenu(strong("Client Level Data Analysis"),
               tabPanel(strong("Analysis of Mental Health Diagnosis"),
                        br(),
                        h3(strong("Overview of Mental Health Conditions: Key Insights"), align = "center"),
                        uiOutput("tab4_intro_mh"),
                        br(),
                        br(),
                        wellPanel(style = "font-family: Verdana;
                                  border-color: black; background-color: white",
                          fluidRow(
                            column(6,
                                   radioButtons("mh_plot", strong("Select A Plot Type"),
                                                choices = c("Map for the Average Number of Mental Health Diagnosis per State" = "mh_map",
                                                            "Pie Chart for the Total Number of Mental Health Diagnosis by Subgroup" = "mh_pie",
                                                            "Bar Chart for Each Mental Health Diagnosis by Subgroup" = "mh_bar"
                                                ))
                            ),
                            column(6,
                                   conditionalPanel(condition = "input.mh_plot == 'mh_pie' || input.mh_plot == 'mh_bar'",
                                                    selectInput("mh_var", strong("Select a Grouping Variable"),
                                                                choices = c("Region" = "Region",
                                                                            "Age Group" = "age_group",
                                                                            "Gender" = "Gender",
                                                                            "Race" = "Race",
                                                                            "Marital Status" = "marital_status",
                                                                            "Employment Status" = "employment_status",
                                                                            "Residential Status" = "residential_status",
                                                                            "Veteran Status" = "veteran_status"))
                                   ),
                                   conditionalPanel(condition = "input.mh_plot == 'mh_map'",
                                                    radioButtons("mh_map_var", strong("Select a Map Preference"),
                                                                 choices = c("Map for Two Years" = "mh_map_two_yr",
                                                                             "Map for the Average Difference" = "mh_map_diff"))
                                   )
                            )
                          ),
                          br()
                        ),
                        br(),
                        conditionalPanel(condition = "input.mh_plot == 'mh_map'&&input.mh_map_var == 'mh_map_two_yr'",
                                         uiOutput("tab4_mh_avg"),
                                         br(),
                                         br(),
                                         h3(strong("Average Number of Mental Health Diagnoses per State"), align = "center"),
                                         fluidRow(
                                           column(12, h5(strong("2018"), align = "left"),
                                                  plotlyOutput("mh_map_2018"))
                                         ),
                                         fluidRow(
                                           column(12, h5(strong("2022"), align ="left"),
                                                  plotlyOutput("mh_map_2022"))
                                         )), 
                        conditionalPanel(condition = "input.mh_plot == 'mh_map'&&input.mh_map_var == 'mh_map_diff'",
                                         uiOutput("tab4_mh_diff"),
                                         br(),
                                         br(),
                                         h3(strong("Difference in the Average Number of Mental Health Diagnoses per State (2022-2018)"), align = "center"),
                                         fluidRow(
                                           column(12,
                                                  plotlyOutput("mh_map_diff"))
                                         )), 
                        # condition for mh_map ends
                        conditionalPanel(condition = "input.mh_plot == 'mh_bar'",
                                         uiOutput("tab4_mh_bar"),
                                         br(),
                                         br(),
                                           h3(strong(textOutput("mh_bar_title")), align = "center"),
                                           column(12,
                                                  plotlyOutput("MH_bar"))
                                         
                        ),
                        conditionalPanel(condition = "input.mh_plot == 'mh_pie'",
                                         uiOutput("tab4_mh_pie"),
                                           br(),
                                         br(),
                                           h3(strong(textOutput("mh_pie_title")), align = "center"),
                                                  br(),
                                                  column(12,
                                                         plotlyOutput("mh_pie"))
                                         
                        )
               ),
               tabPanel(strong("Analysis of Substance Use Diagnosis"),
                        br(),
                        h3(strong("Overview of Substance Use Diagnosis: Key Insights"), align = "center"),
                        uiOutput("tab4_intro_su"),
                        br(),
                        br(),
                        wellPanel(style = "font-family: Verdana;
                                          border-color: black; background-color: white",
                          fluidRow(
                            column(6,
                                   radioButtons("su_plot", strong("Select A Plot Type"),
                                                choices = c("Map of the Percentage of Substance Use Problems per State" = "su_map",
                                                            "Pie Chart for the Total Substance Use Diagnosis by Subgroup" = "su_pie",
                                                            "Bar Chart for Each Substance Use Diagnosis by Subgroup" = "su_bar"
                                                ))
                            ),
                            column(6,
                                   conditionalPanel(condition = "input.su_plot == 'su_pie' || input.su_plot == 'su_bar'",
                                                    selectInput("su_var", strong("Select a Grouping Variable"),
                                                                choices = c("Region" = "Region",
                                                                            "Age Group" = "age_group",
                                                                            "Gender" = "Gender",
                                                                            "Race" = "Race",
                                                                            "Marital Status" = "marital_status",
                                                                            "Employment Status" = "employment_status",
                                                                            "Residential Status" = "residential_status",
                                                                            "Veteran Status" = "veteran_status"))
                                   ),
                                   conditionalPanel(condition = "input.su_plot == 'su_map'",
                                                    radioButtons("su_map_var", strong("Select a Map Preference"),
                                                                 choices = c("Map for Two Years" = "su_map_two_yr",
                                                                             "Map for the Percentage Difference" = "su_map_diff"))
                                   ),
                            )
                          ),
                          br()
                        ),
                        br(),
                        conditionalPanel(condition = "input.su_plot == 'su_map'&&input.su_map_var == 'su_map_two_yr'",
                                         uiOutput("tab4_su_perc"),
                                         br(),
                                         br(),
                                         h3(strong("Percentage of Substance Use Problems per State"), align = "center"),
                                         fluidRow(
                                           br(),
                                           column(12, h5(strong("2018"), align = "left"),
                                                  plotlyOutput("su_map_2018"))
                                         ),
                                         fluidRow(
                                           br(),
                                           column(12, h5(strong("2022"), align = "left"),
                                                  plotlyOutput("su_map_2022"))
                                         )
                        ), 
                        conditionalPanel(condition = "input.su_plot == 'su_map'&&input.su_map_var == 'su_map_diff'",
                                         uiOutput("tab4_su_diff"),
                                         br(),
                                         br(),
                                         h3(strong("Difference in the Percentage of Substance Use Problems per State (2022-2018)"), align = "center"),
                                         fluidRow(
                                           br(),
                                           column(12, 
                                                  #h5("Difference between 2018 and 2022", align = "left"),
                                                  plotlyOutput("su_map_diff"))
                                         )), 
                        # condition for su_map ends
                        conditionalPanel(condition = "input.su_plot == 'su_bar'",
                                         uiOutput("tab4_su_bar"),
                                         br(),
                                         br(),
                                         h4(strong(textOutput("su_bar_title")), align = "center"),
                                         column(12,
                                                plotlyOutput("SU_bar"))
                                         
                        ),
                        conditionalPanel(condition = "input.su_plot == 'su_pie'",
                                         uiOutput("tab4_su_pie"),
                                         br(),
                                         br(),
                                         fluidRow(h4(strong(textOutput("su_pie_title")), align = "center"),
                                                  br(),
                                                  column(12,
                                                         plotlyOutput("su_pie"))
                                         ),
                                         br()
                        )
               )),
    tabPanel(strong("Facilities"),
             br(),
             h3(strong("Exploration of Mental Health and Substance Use Facilities"), align = "center"),
             uiOutput("tab5_intro"), 
             br(), 
             br(),
             wellPanel(style = "font-family: Verdana;
                       border-color: black; background-color: white",
                       fluidRow(
                         column(6,
                                radioButtons("facilityvar", strong("Select Facility Type"),
                                             choices = c("Mental Health" = "mh",
                                                         "Substance Use" = "su"))
                                ),
                         column(6,
                                radioButtons("facilitytype", "Select A Plot Type",
                                             choices = c("View Animation" = "anima",
                                                         "View Data for Each Year" = "data",
                                                         "View Change in Two Years" = "diff"))
                                )
                         )
                       ),
             br(),
             conditionalPanel(condition = "input.facilityvar.includes('mh')",
                              uiOutput("tab5_mh"),
                              br()),
             conditionalPanel(condition = "input.facilityvar.includes('su')",
                              uiOutput("tab5_su"),
                              br()),
             conditionalPanel(condition = "input.facilitytype.includes('anima')&& input.facilityvar.includes('su')",
                              br(),
                              wellPanel(style = "font-family: Verdana;
                                        border-color: black; background-color: white" ,
                                        h3(strong("Substance Use Facilities Animated Map: 2018 vs. 2022"), align = "center"),
                                        tags$div(
                                          tags$img(src = "su_anima.gif", height = "600px", width = "800px",
                                                   style = "display: block; margin-left: auto; margin-right: auto;")
                                       )
                                )
                              ),
             conditionalPanel(condition = "input.facilitytype.includes('anima')&& input.facilityvar.includes('mh')",
                              br(),
                              wellPanel(style = "font-family: Verdana;
                                        border-color: black; background-color: white",
                                        h3(strong("Mental Health Facilities Animated Map: 2018 vs. 2022"), align = "center"),
                                        tags$div(
                                          tags$img(src = "mh_anima.gif", height = "600px", width = "800px",
                                                   style = "display: block; margin-left: auto; margin-right: auto;")
                                       )
                                )
                              ),
             conditionalPanel(condition = "input.facilitytype.includes('data')",
                              br(),
                              wellPanel(style = "font-family: Verdana;
                                        border-color: black; background-color: white",
                                        h3(strong("2018 Facilities")),
                                        plotlyOutput("map_2018"),
                                        h3(strong("2022 Facilities")),
                                        plotlyOutput("map_2022"))
                                ),
             conditionalPanel(condition = "input.facilitytype.includes('diff')",
                              br(),
                              wellPanel(style = "font-family: Verdana;
                                        border-color: black; background-color: white",
                                        h3(strong(textOutput("diff_title")), align = "center"),
                                        plotlyOutput("difference_map"))
                              )
             )
    )
  )


# Define server logic
server <- function(input, output, session) {
  # define time-series plot variable inputs
  observe({
    x <- input$var1
    freezeReactiveValue(input, "var2")
    if(!is.null(x)){
      updateSelectInput(session, "var2", choices = var_choices[var_choices != x])
    }
  })
  
  # define client-level boxplot variable inputs
  observe({
    y <- input$clientvar1
    freezeReactiveValue(input, "clientvar2")
    if(!is.null(y)){
      updateSelectInput(session, "clientvar2", choices = var_choices_boxplot[var_choices_boxplot != y])
    }
  })
  
  # define time_series plot legend names
  legend_name <- reactive({
    legend_ref %>%
      filter(input$subgroup == col_name) %>%
      pull(legend)
  })
  
  # define client-level bar legend names
  MH_legend <- reactive({
    client_ref %>%
      filter(input$mh_var == col_name) %>%
      pull(legend)
  })
  
  SU_legend <- reactive({
    client_ref %>%
      filter(input$su_var == col_name) %>%
      pull(legend)
  })
  

  # define facilities map legend names
  map_legend <- reactive({
    map_ref %>%
      filter(input$facilityvar == col_name) %>%
      pull(legend)
  })
  
  # color mapping for mosaic plot
  color_mapping <- reactive({
    # Combine unique values from both datasets
    unique_values <- unique(c(dep_18$depress1, dep_22$depress1))
    
    # Predefined colors
    predefined_colors <- c("purple3", "limegreen", "orange2")
    
    # Ensure the number of colors matches the number of unique values
    if (length(unique_values) > length(predefined_colors)) {
      stop("Not enough predefined colors for the unique values in the depress1 variable")
    }
    
    # Create the color mapping
    setNames(predefined_colors[seq_along(unique_values)], unique_values)
  })
  
  # Reactive sankey link df
  links_18 <- reactive({
    df1 <- svyby(as.formula(paste("~", input$var2)), as.formula(paste("~", input$var1)), dep_18_design, svytotal, na.rm = TRUE) %>%
      rename("source" = input$var1) %>%
      select(-starts_with("se.")) %>%
      pivot_longer(cols = -source, names_to = "target", values_to = "value") %>%
      mutate(target = gsub(input$var2, "", target))
    df2 <- svyby(~depress1, as.formula(paste("~", input$var2)), dep_18_design, svytotal, na.rm = TRUE) %>%
      rename("source" = input$var2) %>%
      select(-starts_with("se.")) %>%
      pivot_longer(cols = -source, names_to = "target", values_to = "value") %>%
      mutate(target= gsub("depress1", "", target))
    df_combined <- rbind(df1, df2)
    return(df_combined)
  })
  
  nodes_18 <- reactive({
    data.frame(name = select(links_18(), -"value") %>% unlist() %>% unique())
  })
  
  links_22 <- reactive({
    df1 <- svyby(as.formula(paste("~", input$var2)), as.formula(paste("~", input$var1)), dep_22_design, svytotal, na.rm = TRUE) %>%
      rename("source" = input$var1) %>%
      select(-starts_with("se.")) %>%
      pivot_longer(cols = -source, names_to = "target", values_to = "value") %>%
      mutate(target = gsub(input$var2, "", target))
    df2 <- svyby(~depress1, as.formula(paste("~", input$var2)), dep_22_design, svytotal, na.rm = TRUE) %>%
      rename("source" = input$var2) %>%
      select(-starts_with("se.")) %>%
      pivot_longer(cols = -source, names_to = "target", values_to = "value") %>%
      mutate(target= gsub("depress1", "", target))
    df_combined <- rbind(df1, df2)
    return(df_combined)
  })
  
  nodes_22 <- reactive({
    data.frame(name = select(links_22(), -"value") %>% unlist() %>% unique())
  })
  
  # mhd map combined legend
  mhd_combined_limits <- reactive({
    min_value <- min(c(avg_mhd_st_18[["avg_mhd"]],avg_mhd_st_22[["avg_mhd"]]), na.rm = TRUE)
    max_value <- max(c(avg_mhd_st_18[["avg_mhd"]], avg_mhd_st_22[["avg_mhd"]]), na.rm = TRUE)
    return(c(min_value, max_value))
  })
  
  # sud map combined legend
  sud_combined_limits <- reactive({
    min_value <- min(c(perc_sud_st_18[["perc_su_18"]],perc_sud_st_22[["perc_su_22"]]), na.rm = TRUE)
    max_value <- max(c(perc_sud_st_18[["perc_su_18"]], perc_sud_st_22[["perc_su_22"]]), na.rm = TRUE)
    return(c(min_value, max_value))
  })
  
  # facilities map combined legend
  combined_limits <- reactive({
    min_value <- min(c(facilities18[[input$facilityvar]], facilities22[[input$facilityvar]]), na.rm = TRUE)
    max_value <- max(c(facilities18[[input$facilityvar]], facilities22[[input$facilityvar]]), na.rm = TRUE)
    return(c(min_value, max_value))
  })
  
  calculate_mhd_limits <- function() {
    # Combine the data from both years
    combined_data <- bind_rows(
      avg_mhd_st_18 %>% rename(avg_mhd = avg_mhd), 
      avg_mhd_st_22 %>% rename(avg_mhd = avg_mhd)
    )
    
    # Compute limits
    c(min(combined_data$avg_mhd, na.rm = TRUE), max(combined_data$avg_mhd, na.rm = TRUE))
  }
  
  mhd_limits <- calculate_mhd_limits()
  
  calculate_su_limits <- function() {
    # Combine the data from both years
    combined_data <- bind_rows(
      perc_sud_st_18 %>% rename(perc_su = perc_su_18),
      perc_sud_st_22 %>% rename(perc_su = perc_su_22)
    )
    
    # Compute limits
    c(min(combined_data$perc_su, na.rm = TRUE), max(combined_data$perc_su, na.rm = TRUE))
  }
  
  su_limits <- calculate_su_limits()
  
  #end reactive
  
  # start plot/map
  #define time series plot
  output$time_series_plot <- renderPlotly({
    # Generate a plot for the variable by year and region
    time_series_data <- svyby(as.formula(paste("~", "mntlhlth")), as.formula(paste("~", input$subgroup, "+year")), gss_design, svymean, na.rm = TRUE)
    time_series_data <- time_series_data %>% filter(!(get("mntlhlth") == 0 & se == 0))
    variable_name <- switch("mntlhlth",
                            "mntlhlth" = "Average Number of Days of Poor Mental Health Past 30 Days")
    
    time_series_data %>%
      plot_ly(x = ~year, y = as.formula(paste("~", "mntlhlth")), color = as.formula(paste("~", input$subgroup)), mode = "marker+line",
              text = ~paste0("</br>Year: ", year,"</br>",
                             input$subgroup,": ", time_series_data[[input$subgroup]],
                             "</br>Average Days: ",round(time_series_data[["mntlhlth"]], 2)), hoverinfo = "text") %>%
      add_trace(legendgroup =as.formula(paste("~", input$subgroup))) %>%
      add_ribbons(ymin = as.formula(paste("~", "mntlhlth", "- se")), ymax = as.formula(paste("~", "mntlhlth", "+ se")),
                  alpha = 0.2, line = list(color = "rgba(1,100,150,0)"), showlegend = F,legendgroup = as.formula(paste("~", input$subgroup))) %>%
      layout(xaxis = list(title = "Year", dtick = 2),
             yaxis = list(title = "Average Number of Days"),
             title = paste(variable_name, "by", legend_name()))
    
  })
  
  output$time_series_plot_2 <- renderPlotly({
    # Generate a plot for the variable by year and region
    time_series_data <- svyby(as.formula(paste("~", "hlthdays")), as.formula(paste("~", input$subgroup, "+year")), gss_design, svymean, na.rm = TRUE)
    time_series_data <- time_series_data %>% filter(!(get("hlthdays") == 0 & se == 0))
    variable_name <- switch("hlthdays",
                            "hlthdays" = "Average Number of Days of Activity Limitation Past 30 Days")
    
    time_series_data %>%
      plot_ly(x = ~year, y = as.formula(paste("~", "hlthdays")), color = as.formula(paste("~", input$subgroup)), mode = "marker+line",
              text = ~paste0("</br>Year: ", year,"</br>",
                             input$subgroup,": ", time_series_data[[input$subgroup]],
                             "</br>Average Days: ",round(time_series_data[["hlthdays"]], 2)), hoverinfo = "text") %>%
      add_trace(legendgroup =as.formula(paste("~", input$subgroup))) %>%
      add_ribbons(ymin = as.formula(paste("~", "hlthdays", "- se")), ymax = as.formula(paste("~", "hlthdays", "+ se")),
                  alpha = 0.2, line = list(color = "rgba(1,100,150,0)"), showlegend = F,legendgroup = as.formula(paste("~", input$subgroup))) %>%
      layout(xaxis = list(title = "Year", dtick = 2),
             yaxis = list(title = "Average Number of Days"),
             title = paste(variable_name, "by", legend_name()))
    
  })
  
  output$time_series_plot_3 <- renderPlotly({
    # Generate a plot for the variable by year and region
    time_series_data <- svyby(as.formula(paste("~", "misswork")), as.formula(paste("~", input$subgroup, "+year")), gss_design, svymean, na.rm = TRUE)
    time_series_data <- time_series_data %>% filter(!(get("misswork") == 0 & se == 0))
    variable_name <- switch("misswork",
                            "misswork" = "Average Number of Miss Work Days for Health Past 30 Days")
    
    time_series_data %>%
      plot_ly(x = ~year, y = as.formula(paste("~", "misswork")), color = as.formula(paste("~", input$subgroup)), mode = "marker+line",
              text = ~paste0("</br>Year: ", year,"</br>",
                             input$subgroup,": ", time_series_data[[input$subgroup]],
                             "</br>Average Days: ",round(time_series_data[["misswork"]], 2)), hoverinfo = "text") %>%
      add_trace(legendgroup =as.formula(paste("~", input$subgroup))) %>%
      add_ribbons(ymin = as.formula(paste("~", "misswork", "- se")), ymax = as.formula(paste("~", "misswork", "+ se")),
                  alpha = 0.2, line = list(color = "rgba(1,100,150,0)"), showlegend = F,legendgroup = as.formula(paste("~", input$subgroup))) %>%
      layout(xaxis = list(title = "Year", dtick = 2),
             yaxis = list(title = "Average Number of Days"),
             title = paste(variable_name, "by", legend_name()))
    
  })
  
  # define depression sankey plot
  output$depression_sankey_plot_18 <- renderSankeyNetwork({
    links<-links_18() %>% as.data.frame()
    nodes <- nodes_18()
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1
    links$linkColour <- color_map_final[links$source]
    nodes$nodeColour <- color_map_final[nodes$name]
    # Make the Network
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", fontSize = 12,
                  colourScale = JS(color_scale_final), LinkGroup = "linkColour", NodeGroup = "nodeColour",
                  sinksRight=FALSE,iterations = 0)
  })
  output$depression_sankey_plot_22 <- renderSankeyNetwork({
    links<-links_22() %>% as.data.frame()
    nodes <- nodes_22()
    links$IDsource <- match(links$source, nodes$name)-1
    links$IDtarget <- match(links$target, nodes$name)-1
    links$linkColour <- color_map_final[links$source]
    nodes$nodeColour <- color_map_final[nodes$name]
    # Make the Network
    sankeyNetwork(Links = links, Nodes = nodes,
                  Source = "IDsource", Target = "IDtarget",
                  Value = "value", NodeID = "name", fontSize = 12,
                  colourScale = JS(color_scale_final), LinkGroup = "linkColour", NodeGroup = "nodeColour",
                  sinksRight=FALSE, iterations = 0)
    
  })
  # define depression mosaic plot
  output$mosaic_plot_18 <- renderPlotly({
    p18 <- ggplot(data = dep_18) +
      geom_mosaic(aes(x = product(!!sym(input$var1), !!sym(input$var2)), fill = depress1, weight = wtsscomp)) +
      scale_fill_manual(values = color_mapping()) +
      labs(y = input$var1, x = paste(input$var1, ":", input$var2), fill = "Depression") +
      facet_grid(facets = as.formula(paste("~", input$var2))) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank())
    ggplotly(p18)
  })
  
  output$mosaic_plot_22 <- renderPlotly({
    p22 <- ggplot(data = dep_22) +
      geom_mosaic(aes(x = product(!!sym(input$var1), !!sym(input$var2)), fill = depress1, weight = wtsscomp)) +
      scale_fill_manual(values = color_mapping()) +
      labs(y = input$var1, x = paste(input$var1, ":", input$var2), fill = "Depression") +
      facet_grid(facets = as.formula(paste("~", input$var2))) +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank(),
            panel.grid.major = element_blank(),
            panel.background = element_blank(),
            strip.background = element_blank())
    ggplotly(p22)
  })
  
  # Mental Health map 
  output$mh_map_2018 <- renderPlotly({
    suppressWarnings({
      hexgrid18_mhd <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(avg_mhd_st_18, by = c("google_name" = "stname"))
      
      hexgrid18_mhd <- hexgrid18_mhd %>%
        mutate(
          avg_mhd = ifelse(is.na(avg_mhd) | avg_mhd == 0, NA, avg_mhd),
          tooltip_text = ifelse(is.na(avg_mhd), 
                                paste("No Data\nState:", google_name), 
                                paste("Count:", round(avg_mhd, 4), "\nState:", google_name))
        )
      
      graph18_mph <- ggplot(hexgrid18_mhd, aes(fill = avg_mhd, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = "Average Number of Mental\nHealth Diagnosis") +
        theme_void() +
        scale_fill_distiller(
          palette = "Spectral", 
          limits = mhd_limits,
          breaks = seq(mhd_limits[1], mhd_limits[2], length.out = 4),
          labels = scales::number_format(accuracy = 0.01),
          na.value = "grey"  # Setting a color for NA values
        ) +
        coord_sf(lims_method = "geometry_bbox") +  # Handling spatial limits
        theme(axis.line = element_blank())
      
      ggplotly(graph18_mph, tooltip = "text")
    })
  })
  
  output$mh_map_2022 <- renderPlotly({
    suppressWarnings({
      hexgrid22_mhd <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(avg_mhd_st_22, by = c("google_name" = "stname"))
      
      hexgrid22_mhd <- hexgrid22_mhd %>%
        mutate(
          avg_mhd = ifelse(is.na(avg_mhd) | avg_mhd == 0, NA, avg_mhd),
          tooltip_text = ifelse(is.na(avg_mhd), 
                                paste("No Data\nState:", google_name), 
                                paste("Count:", round(avg_mhd, 4), "\nState:", google_name))
        )
      
      graph22_mph <- ggplot(hexgrid22_mhd, aes(fill = avg_mhd, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = "Average Number of Mental\nHealth Diagnosis") +
        theme_void() +
        scale_fill_distiller(
          palette = "Spectral", 
          limits = mhd_limits,
          breaks = seq(mhd_limits[1], mhd_limits[2], length.out = 4),
          labels = scales::number_format(accuracy = 0.01),
          na.value = "grey"  # Setting a color for NA values
        ) +
        coord_sf(lims_method = "geometry_bbox") +  # Handling spatial limits
        theme(axis.line = element_blank())
      
      ggplotly(graph22_mph, tooltip = "text")
    })
  })
  
  #substance use map 
  output$su_map_2018 <- renderPlotly({
    suppressWarnings({
      hexgrid18_sud <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(perc_sud_st_18, by = c("google_name" = "stname"))
      hexgrid18_sud <- hexgrid18_sud %>%
        mutate(
          perc_su_18_text = round(perc_su_18*100, 4),
          tooltip_text = paste(ifelse(is.na(perc_su_18), "No Data", paste(perc_su_18_text,"%")), "\nState:", google_name)
        )
      graph18_sud <- ggplot(hexgrid18_sud, aes(fill = perc_su_18, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = "Percentage of\nSubstance Use Problem") +
        theme_void() +
        scale_fill_distiller(
          palette = "Spectral",
          limits = su_limits,  # Consistent limits
          breaks = seq(su_limits[1], su_limits[2], length.out = 4),
          labels = scales::percent,
          na.value = "grey"  # Color for "No Data"
        ) +
        coord_sf(lims_method = "geometry_bbox") +  # Handling spatial limits
        theme(axis.line = element_blank())
      ggplotly(graph18_sud, tooltip = "text")
    })
  })
  
  output$su_map_2022 <- renderPlotly({
    suppressWarnings({
      hexgrid22_sud <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(perc_sud_st_22, by = c("google_name" = "stname"))
      hexgrid22_sud <- hexgrid22_sud %>%
        mutate(
          perc_su_22_text = round(perc_su_22*100, 4),
          tooltip_text = paste(ifelse(is.na(perc_su_22), "No Data", paste(perc_su_22_text,"%")), "\nState:", google_name)
        )
      graph22_sud <- ggplot(hexgrid22_sud, aes(fill = perc_su_22, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = "Percentage of\nSubstance Use Problem") +
        theme_void() +
        scale_fill_distiller(
          palette = "Spectral",
          limits = su_limits,  # Consistent limits
          breaks = seq(su_limits[1], su_limits[2], length.out = 4),
          labels = scales::percent,
          na.value = "grey"  # Color for "No Data"
        ) +
        coord_sf(lims_method = "geometry_bbox") +  # Handling spatial limits
        theme(axis.line = element_blank())
      ggplotly(graph22_sud, tooltip = "text")
    })
  })
  
  output$su_map_diff <- renderPlotly({
    suppressWarnings({
      hexgrid_diff_sud <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(perc_sud_st_diff, by = c("google_name" = "stname"))
      hexgrid_diff_sud <- hexgrid_diff_sud %>%
        mutate(perc_su_diff_text = round(perc_su_diff*100, 4),
               tooltip_text = paste(ifelse(is.na(perc_su_diff), "No Data", paste(perc_su_diff_text,"%")), "\nState:", google_name)
        )
      graph_diff_sud <- ggplot(hexgrid_diff_sud, aes(fill = perc_su_diff, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = "Difference in Percentage \nof Substance Use\nProblem (2022-2018)") +
        theme_void() +
        scale_fill_distiller(
          palette = "Spectral",
          limits = c(min(hexgrid_diff_sud$perc_su_diff, na.rm = T), max(hexgrid_diff_sud$perc_su_diff, na.rm = T)),
          breaks = seq(min(hexgrid_diff_sud$perc_su_diff, na.rm = T), max(hexgrid_diff_sud$perc_su_diff, na.rm = T), length.out = 5),
          labels = scales::percent,
          na.value = "grey"  # Setting a color for NA values
        ) +
        coord_sf(lims_method = "geometry_bbox") +  # Handling spatial limits
        theme(axis.line = element_blank())
      ggplotly(graph_diff_sud, tooltip = "text")
    })
  })


  # mh bar chart
  output$MH_bar <- renderPlotly({
    #collect 2018 data
    bar_18 <- mhd_18 %>%
      group_by(!!sym(input$mh_var), mental_health_diagnosis) %>%
      summarize(
        Count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()
    bar_18 <- bar_18 %>% mutate(Year = "2018")
    
    #collect 2022 data
    bar_22 <- mhd_22 %>%
      group_by(!!sym(input$mh_var), mental_health_diagnosis) %>%
      summarize(
        Count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()
    bar_22 <- bar_22 %>% mutate(Year = "2022")
    
    # combine bar 18 and 22
    bar_data <- rbind(bar_18, bar_22)
    
    if (input$mh_var == "age_group"){
      bar_data$age_group <- factor(bar_data$age_group, levels = c("Under 18", "18-24", "25-44", "45-64", "65+"))
    }
    
    # Calculate total counts for ordering
    total_counts <- bar_data %>%
      group_by(mental_health_diagnosis) %>%
      summarize(TotalCount = sum(Count)) %>%
      arrange(desc(TotalCount))
    # Order the mental_health_diagnosis factor (or the dynamic variable)
    bar_data <- bar_data %>%
      mutate(mental_health_diagnosis := factor(mental_health_diagnosis, levels = total_counts[["mental_health_diagnosis"]]))
    
    plot <- ggplot(bar_data, aes(x = Year, y = Count, fill = !!sym(input$mh_var))) +
      geom_col(position = "stack") +
      theme_void() +
      coord_flip() +
      labs(x = NULL, y = "Count", fill = MH_legend()) +
      scale_y_continuous(labels = scales::label_comma()) +
      theme(
        legend.position = "bottom",  # Remove legend
        panel.border = element_blank(),  # Remove panel borders
        panel.spacing.y = unit(.5, "lines"),
        axis.line = element_line(color = "black"),  # Ensure axis line is visible
        axis.text.y = element_text(margin = margin(r = 0)),
        # axis.line = element_blank(),  # Remove axis lines
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent")
      ) +
      facet_grid(as.formula(paste("mental_health_diagnosis","~.")), switch = "both")
    
    plotly_plot <- ggplotly(plot, height = 800)
    
    legend_x <- switch(input$mh_var,
                       default = "center")
    
    plotly_plot %>% layout(
      xaxis = list(
        title = "Number of Mental Health Diagnoses",
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        showticklabels = TRUE,
        exponentformat = "none"
      ),
      yaxis = list(
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        showticklabels = TRUE,
        exponentformat = "none"
      ),
      legend = list(orientation = "h",
                    x = legend_x,  # Center the legend
                    y = 1.03,  # Position the legend below the plot
                    yanchor = "bottom",
                    traceorder = "reversed"),
      margin = list(l = 0, r = 210, b = 100, t = 0),  # Adjust margins to accommodate labels
      paper_bgcolor = "white",  # Optional: set background color of the plot area
      plot_bgcolor = "white"    # Optional: set background color of the entire plot
    )
  })
  
  # su bar chart
  output$SU_bar <- renderPlotly({
    #collect 2018 data
    bar_18 <- mhd_18 %>%
      group_by(!!sym(input$su_var), substance_use_diagnosis) %>%
      summarize(
        Count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()
    bar_18 <- bar_18 %>% mutate(Year = "2018")
    
    #collect 2022 data
    bar_22 <- mhd_22 %>%
      group_by(!!sym(input$su_var), substance_use_diagnosis) %>%
      summarize(
        Count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()
    bar_22 <- bar_22 %>% mutate(Year = "2022")
    
    # combine bar 18 and 22
    bar_data <- rbind(bar_18, bar_22)
    
    if (input$su_var == "age_group"){
      bar_data$age_group <- factor(bar_data$age_group, levels = c("Under 18", "18-24", "25-44", "45-64", "65+"))
    }
    
    # Calculate total counts for ordering
    total_counts <- bar_data %>%
      group_by(substance_use_diagnosis) %>%
      summarize(TotalCount = sum(Count)) %>%
      arrange(desc(TotalCount))
    # Order the mental_health_diagnosis factor (or the dynamic variable)
    bar_data <- bar_data %>%
      mutate(substance_use_diagnosis := factor(substance_use_diagnosis, levels = total_counts[["substance_use_diagnosis"]]))
    
    plot <- ggplot(bar_data, aes(x = Year, y = Count, fill = !!sym(input$su_var))) +
      geom_col(position = "stack") +
      theme_void() +
      coord_flip() +
      labs(x = NULL, y = "Count", fill = SU_legend()) +
      scale_y_continuous(labels = scales::label_comma()) +
      theme(
        legend.position = "bottom",  # Remove legend
        panel.border = element_blank(),  # Remove panel borders
        panel.spacing.y = unit(.5, "lines"),
        axis.line = element_line(color = "black"), # Ensure axis line is visible
        axis.text.y = element_text(margin = margin(r = 0)),
        # axis.line = element_blank(),  # Remove axis lines
        axis.ticks = element_blank(),
        legend.background = element_rect(fill = "transparent"),
      ) +
      facet_grid(as.formula(paste("substance_use_diagnosis","~.")), switch = "both")
    
    plotly_plot <- ggplotly(plot, height = 800)
    
    legend_x <- switch(input$su_var,
                       default = "center")
    
    plotly_plot %>% layout(
      xaxis = list(
        title = "Number of Substance Use Diagnoses",
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        showticklabels = TRUE,
        exponentformat = "none"
      ),
      yaxis = list(
        showgrid = TRUE,
        zeroline = FALSE,
        showline = TRUE,
        showticklabels = TRUE,
        exponentformat = "none"
      ),
      legend = list(
        orientation = "h",
        x = legend_x,  # Center the legend
        y = 1.03,  # Position the legend below the title
        yanchor = "bottom",
        traceorder = "reversed"
      ),
      margin = list(l = 0, r = 200, b = 100, t = 0),  # Adjust margins to accommodate labels
      paper_bgcolor = "white",  # Optional: set background color of the plot area
      plot_bgcolor = "white"    # Optional: set background color of the entire plot
    )
  })
  
  
  # mh pie chart
  output$mh_pie_title <- renderText({
    title <- paste('Total Mental Health Diagnosis by', MH_legend(), ' (2018 vs. 2022)')
    title
  })
  
  # su pie chart
  output$su_pie_title <- renderText({
    title <- paste('Total Substance Use Diagnosis by', SU_legend(), ' (2018 vs. 2022)')
    title
  })
  
  output$mh_bar_title <- renderText({
    title <- paste('Comparing the Total Number of Mental Health Diagnoses by ', MH_legend(), ' in 2018 and 2022')
  })
  
  output$su_bar_title <- renderText({
    title <- paste('Comparing the Total Number of Substance Use Diagnoses by ', SU_legend(), ' in 2018 and 2022')
  })
  
  output$diff_title <- renderText({
    title <- paste('Difference in the Number of ', map_legend(), ' Facilities (2022-2018)')
  })
  
  output$mh_pie <- renderPlotly({
    pie_mhd_18 <- mhd_18 %>%
      group_by(!!sym(input$mh_var)) %>%
      dplyr::filter(!is.na(mental_health_diagnosis)) %>%
      summarize(
        count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()
    
    #collect 2022 data
    pie_mhd_22 <- mhd_22 %>%
      group_by(!!sym(input$mh_var)) %>%
      dplyr::filter(!is.na(mental_health_diagnosis)) %>%
      summarize(
        count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%
      collect()
    
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = pie_mhd_18, labels = as.formula(paste("~",input$mh_var)), values = ~count, type = 'pie',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF', size = 14),
                           outsidetextfont = list(size = 14),
                           hoverinfo = 'text',
                           text = ~paste0(MH_legend(), ": ", pie_mhd_18[[input$mh_var]],
                                          '\nCount: ', count),
                           marker = list(line = list(color = '#FFFFFF', width = 1)),
                           showlegend = FALSE,
                           name = "2018", 
                           domain = list(row = 0, column = 0),
                           rotation = -20)
    fig <- fig %>% add_pie(data = pie_mhd_22, labels = as.formula(paste("~",input$mh_var)), values = ~count, type = 'pie',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF',size = 14),
                           outsidetextfont = list(size = 14),
                           hoverinfo = 'text',
                           text = ~paste0(MH_legend(), ": ", pie_mhd_22[[input$mh_var]],
                                          '\nCount: ', count),
                           marker = list(line = list(color = '#FFFFFF', width = 1)),
                           showlegend = FALSE,
                           name = "2022", 
                           domain = list(row = 0, column = 1),
                           rotation = -20)
    fig <- fig %>% layout(showlegend = F,
                          grid=list(rows=1, columns=2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  
  # su pie chart
  output$su_pie <- renderPlotly({
    #collect 2018 data
    pie_sud_18 <- client2018 %>%
      group_by(!!sym(input$su_var)) %>%
      dplyr::filter(substance_use_problem == "Yes") %>%
      summarize(
        count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()
    #collect 2022 data
    pie_sud_22 <- client2022 %>%
      group_by(!!sym(input$su_var)) %>%
      dplyr::filter(substance_use_problem == "Yes") %>%
      summarize(
        count = n(),
        .groups = 'drop'
      ) %>%
      dplyr::filter(dplyr::across(everything(), ~ !is.na(.))) %>%  
      collect()

    fig <- plot_ly()
    fig <- fig %>% add_pie(data = pie_sud_18, labels = as.formula(paste("~",input$su_var)), values = ~count, type = 'pie',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF',size = 14),
                           outsidetextfont = list(size = 14),
                           hoverinfo = 'text',
                           text = ~paste0(SU_legend(), ": ", pie_sud_18[[input$su_var]],
                                          '\nCount: ', count),
                           marker = list(line = list(color = '#FFFFFF', width = 1)),
                           showlegend = FALSE,
                           name = "2018", 
                           domain = list(row = 0, column = 0),
                           rotation = -20)
    fig <- fig %>% add_pie(data = pie_sud_22, labels = as.formula(paste("~",input$su_var)), values = ~count, type = 'pie',
                           textinfo = 'label+percent',
                           insidetextfont = list(color = '#FFFFFF',size = 14),
                           outsidetextfont = list(size = 14),
                           hoverinfo = 'text',
                           text = ~paste0(SU_legend(), ": ", pie_sud_22[[input$su_var]],
                                          '\nCount: ', count),
                           marker = list(line = list(color = '#FFFFFF', width = 1)),
                           showlegend = FALSE,
                           name = "2022", 
                           domain = list(row = 0, column = 1),
                           rotation = 5)
    fig <- fig %>% layout(showlegend = F,
                          grid=list(rows=1, columns=2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    fig
  })
  
  
  # facilities data
  output$map_2018 <- renderPlotly({
    suppressWarnings({
      hexgrid18_sf <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>% 
        left_join(facilities18, by = c("iso3166_2" = "state_code"))
      hexgrid18_sf <- hexgrid18_sf %>%
        mutate(tooltip_text = paste("Count:", round(.data[[input$facilityvar]],3), "\nState:", .data$google_name))
      hex_map18 <- ggplot(hexgrid18_sf, aes_string(fill = input$facilityvar, text = "tooltip_text")) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = paste0("Number of ",map_legend(), "\nFacilities Per 10,000 People")) +
        theme_void() +
        scale_fill_distiller(palette = "Spectral", 
                             limits = combined_limits(),
                             breaks = seq(combined_limits()[1], combined_limits()[2], length.out = 4),
                             labels = scales::number_format(accuracy = 0.01)) +
        theme(axis.line = element_blank())
      ggplotly(hex_map18, tooltip = "text") %>%  
        layout(legend = list(x = 0.5,  # Center the legend
                             y = -0.15,  # Position the legend below the plot
                             yanchor = "top"))
    })
  })
  
  output$map_2022 <- renderPlotly({
    suppressWarnings({
      hexgrid22_sf <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>% 
        left_join(facilities22, by = c("iso3166_2" = "state_code"))
      hexgrid22_sf <- hexgrid22_sf %>%
        mutate(tooltip_text = paste("Count:", round(.data[[input$facilityvar]], 3), "\nState:", .data$google_name))
      hex_map22 <- ggplot(hexgrid22_sf, aes_string(fill = input$facilityvar, text = "tooltip_text")) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = paste0("Number of ", map_legend(), "\nFacilities Per 10,000 People")) +
        theme_void() +
        scale_fill_distiller(palette = "Spectral",
                             limits = combined_limits(),
                             breaks = seq(combined_limits()[1], combined_limits()[2], length.out = 4),
                             labels = scales::number_format(accuracy = 0.01)) +
        theme(axis.line = element_blank())
      ggplotly(hex_map22, tooltip = "text") %>%  
        layout(legend = list(x = 0.5,  # Center the legend
                             y = -0.15,  # Position the legend below the plot
                             yanchor = "top"))
    })
  })
  
  output$difference_map <- renderPlotly({
    suppressWarnings({
      # Calculate the difference between 2022 and 2018
      facilities_diff <- combined_facilities %>%
        select(state_code, year, all_of(input$facilityvar)) %>%
        spread(year, all_of(input$facilityvar)) %>%
        mutate(difference = `2022` - `2018`,
               difference = round(difference, 3))
      # Merge with hexgrid
      hexgrid_combined <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(facilities_diff, by = c("iso3166_2" = "state_code"))
      hexgrid_combined <- hexgrid_combined %>%
        mutate(tooltip_text = paste("Difference:", round(difference, 3), "\nState:", google_name))
      
      hex_map <- ggplot(hexgrid_combined, aes(fill = difference, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = paste0("Difference in the Number of \n", map_legend(), " Facilities \nPer 10,000 People \n(2022 - 2018)")) +
        theme_void() +
        scale_fill_distiller(limits = c(min(hexgrid_combined$difference),max(hexgrid_combined$difference)),
                             breaks = seq(min(hexgrid_combined$difference), max(hexgrid_combined$difference), length.out = 4),
                             palette = "Spectral", 
                             direction = -1, 
                             labels = scales::number_format(accuracy = 0.001)) +
        theme(axis.line = element_blank())
      
      ggplotly(hex_map, tooltip = "text")
    })
  })
  
  output$mh_map_diff <- renderPlotly({
    suppressWarnings({
      hexgrid_diff_mhd <- hexgrid %>%
        mutate(google_name = gsub(" \\(United States\\)", "", google_name)) %>%
        left_join(avg_mhd_st_diff, by = c("google_name" = "stname"))
      
      hexgrid_diff_mhd <- hexgrid_diff_mhd %>%
        mutate(tooltip_text = ifelse(is.na(avg_mhd_diff),
                                     paste("No Data\nState:", google_name),
                                     paste("Difference:", round(avg_mhd_diff,4), "\nState:", google_name))
        )
      
      graph_diff_mph <- ggplot(hexgrid_diff_mhd, aes(fill = avg_mhd_diff, text = tooltip_text)) +
        geom_sf() +
        geom_sf_text(aes(label = iso3166_2)) +
        labs(fill = "Difference in the Average\nNumber of Mental Health\nDiagnosis (2022-2018)") +
        theme_void() +
        scale_fill_distiller(
          palette = "Spectral",
          # limits = mhd_limits,
          # breaks = seq(mhd_limits[1], mhd_limits[2], length.out = 4),
          # labels = scales::number_format(accuracy = 0.01),
          na.value = "grey"  # Setting a color for NA values
        ) +
        coord_sf(lims_method = "geometry_bbox") +  # Handling spatial limits
        theme(axis.line = element_blank())
      
      ggplotly(graph_diff_mph, tooltip = "text")
    })
  })
  
  # writing for home tab
  output$welcome <- renderText({
    "This analysis leverages data from the General Social Survey (GSS), and the 
    Substance Abuse and Mental Health Services Administration (SAMHSA) to provide 
    comprehensive insights into the impact of the COVID-19 pandemic on mental 
    health and substance use. By examining these two data sources, we aim to uncover 
    significant trends and changes in mental health status, as well as the availability 
    and utilization of mental health and substance abuse facilities in the United States."
  })
  
  output$gss <- renderText({
    "The General Social Survey (GSS) is a bi-yearly survey collecting data on 
    American attitudes, behaviors, and attributes since 1972. It provides 
    comprehensive and longitudinal data on various aspects of social life, 
    including mental health. For this analysis, we used GSS data to compare mental 
    health indicators such as poor mental health days, activity limitation due to 
    mental health, the number of missed work days, and depression, before and 
    after the COVID-19 pandemic, primarily focusing on data from 2018 and 2022. 
    This allows us to identify trends and changes in self-reported mental health 
    status over time and across different demographic groups."
  })
  
  output$mhc <- renderText({
    "The Mental Health Client-Level Data (MH-CLD) dataset is a comprehensive source 
    of information on mental health treatment services at the client level. This 
    dataset provides detailed insights into various aspects of mental health services, 
    including demographics (age, gender, race, ethnicity, and socioeconomic status 
    of individuals receiving mental health services), treatment settings (types 
    of settings where mental health services are provided, such as inpatient, 
    outpatient, and community-based settings), and treatment outcomes (improvements 
    in mental health status, reductions in symptoms, and overall treatment 
    effectiveness). In addition to these elements, the MH-CLD dataset allows for 
    an examination of substance use problem diagnoses. It provides data on the 
    total number of substance use problem diagnoses per state, enabling a geographical 
    analysis of substance use trends. It also breaks down the total number of 
    substance use diagnoses by subgroups, allowing for a detailed understanding 
    of how different demographic groups are affected. This comprehensive dataset 
    supports a better understanding of both the reach and effectiveness of mental 
    health services across the United States, as well as the specific challenges 
    related to substance use."
  })
  
  output$su <- renderText({
    "The National Substance Use and Mental Health Services Survey (N-SUMHSS) is 
    an extensive survey that collects data on the characteristics of substance 
    abuse and mental health treatment facilities in the U.S. This survey includes 
    information on facility characteristics (data on the types of facilities 
    providing substance abuse and mental health services, including hospitals, 
    outpatient clinics, residential treatment centers, and community health 
    organizations) and services offered (information on the range of services 
    provided by these facilities, such as counseling, medication-assisted treatment, 
    detoxification, and rehabilitation programs). The N-SUMHSS dataset provides 
    a comprehensive overview of the infrastructure and capacity of mental health 
    and substance abuse treatment services in the U.S. Our analysis focuses on the 
    individual number of mental health and substance abuse facilities in 2018 and 
    2022. For our 2018 facility analysis, we joined two datasets, the National 
    Mental Health Services Survey (N-MHSS) and the National Survey of Substance 
    Abuse Treatment Services (N-SSATS), due to the limitation of data given by 
    the N-SUMHSS. It is important to note that some facilities may provide 
    treatment for both mental health and substance abuse, so the overcounting of 
    facilities is possible in our facility counts."
  })
  
  output$tab2_intro <- renderText({
    "The General Social Survey (GSS) data, adjusted for weight, provides valuable 
    insights into various health metrics over the years, starting in 2002. Our 
    analysis focuses on three primary indicators: the average number of days of 
    poor mental health, activity limitation, and missed work days due to health 
    reasons. The ribbon shows plus/minus one standard error."
  })
  
  output$tab2_mntlhlth <- renderUI({
    HTML("Our analysis of the &quot;Average Number of Days of Poor Mental Health Past 30 Days&quot;
    spans across two decades, from 2002 to 2022. This indicator is crucial for 
    understanding the mental well-being of different demographic groups over time. 
    Mental health is a significant aspect of overall health, influencing both quality 
    of life and productivity.<br><br>
    Data gaps in 2008 and 2020 are notable, yet the existing data reveals a concerning 
    trend: a steady increase in poor mental health days across almost every subgroup 
    from 2018 to 2022. This upward trend suggests growing mental health challenges 
    within the population, likely intensified by factors such as the COVID-19 
    pandemic, which has amplified stress, anxiety, and other mental health issues. 
    Notable findings include, but are not limited to, females and young adults 
    aged 18-24 consistently reporting more days of poor mental health compared to 
    their counterparts. This indicates heightened vulnerability within these groups, 
    possibly due to societal pressures or higher levels of stress and anxiety. 
    These findings underscore the necessity for targeted mental health interventions 
    and support systems to address the specific needs of these demographics.")
  })
  
  output$tab2_hlthdays <- renderUI({
    HTML("The &quot;Average Number of Days of Activity Limitation Past 30 Days&quot;
    indicator provides valuable insights into how mental health issues affect 
    daily functioning and overall quality of life. This is particularly important 
    for understanding the impact of mental health conditions and their impact on 
    the ability of individuals to engage in everyday activities. <br> <br>
    Despite data gaps in 2004, 2008, 2012, 2016, and 2020, the available data 
    indicates a consistent increase in activity limitation days across nearly every 
    subgroup from 2018 to 2022. This trend suggests that more individuals are 
    experiencing mental health issues severe enough to limit their daily activities, 
    likely exacerbated by the rise in anxiety, depression, and other mental health 
    conditions associated with the COVID-19 pandemic. The African American group 
    is an exception to this trend, not showing the same increase in activity 
    limitation days. This deviation warrants further investigation to understand 
    the underlying causes. Additionally, in 2022, females reported an average of 
    one more day of activity limitation than males. This gender disparity may 
    reflect differences in mental health status, access to mental healthcare, or 
    the impact of caregiving responsibilities, which are often more prevalent 
    among women.")
  })
  
  output$tab2_misswork <- renderUI({
    HTML("The &quot;Average Number of Missed Work Days for Health Past 30 Days&quot; 
    indicator highlights how mental health affects workforce participation and 
    productivity. This is crucial for understanding the broader impact of mental 
    health on individuals' ability to engage in work and daily activities. <br><br>
    Data gaps in 2016 and 2020 limit the continuity of the analysis. However, 
    data from other years shows that females consistently report more missed 
    work days due to health issues than males. This persistent gender difference, 
    especially noticeable in the context of the COVID-19 pandemic, underscores 
    the need for workplace health policies that better support women's mental health 
    needs. The increased frequency of missed work days among women suggests that 
    mental health challenges may be more impactful for them, highlighting the 
    importance of implementing supportive policies such as flexible work arrangements, 
    comprehensive mental health benefits, and programs addressing work-life balance.")
  })
  
  output$tab3_intro <- renderUI(
    HTML("Using the General Social Survey (GSS), this analysis focuses on the prevalence 
    of depression, classified as “Yes” or “No” based on whether individuals were 
    informed by a healthcare professional that they have depression. The “No 
    Response” category takes into account individuals who responded with “Inapplicable”, 
    “No Answer”, “Do not Know/Cannot Choose”, and “Skipped on Web”. The primary 
    objective is to examine the changes in depression rates in 2018 and 2022, a 
    period containing the onset and aftermath of the COVID-19 pandemic. We aim to 
    understand how these rates vary across different subgroups, including region, 
    age group, gender, race, and degree. <br><br>
    We constructed two types of interactive diagrams to visualize these changes: 
    Sankey and Mosaic plots. Our analysis reveals significant shifts in depression 
    rates from 2018 compared to 2022. Depression rates nearly doubled over this period, 
    increasing 1.92-fold overall. The 25-44 age group, the largest demographic 
    in the subgroup, saw a 2.21-fold rise in depression. Female depression rates, 
    though still predominant, decreased from 64.48% to 55.51%, while male depression 
    rates more than doubled. Racially, the proportion of whites with depression 
    decreased slightly from 84.9% to 82.8%, whereas the percentage of other races 
    increased from 6% to 10.6%. Educationally, there was a doubling of depression 
    cases among those with bachelor’s and graduate degrees, as well as those with 
    less than a high school diploma. Geographically, the South region saw the 
    highest depression rates, rising from 93 to 187 cases. These findings highlight 
    significant demographic shifts and trends in depression prevalence before and 
    after the pandemic period. <br><br>
    We invite you to explore the Sankey and mosaic diagrams to discover additional 
    insights and trends within the data.")
  )
  
  output$tab4_intro_mh <- renderUI(
    HTML("These visualizations comprehensively analyze mental health diagnoses using 
      the Mental Health Client-Level Data (MH-CLD). Explore trends in mental health 
      diagnoses across various states and demographic subgroups in 2018 and 2022. 
      Our maps and charts reveal the average number of diagnoses per state, changes 
      over the two years, and how these diagnoses are distributed among different 
      groups. By examining these visualizations, you can gain valuable insights 
      into variations and shifts in mental health needs, particularly in the 
      context of the COVID-19 pandemic. Discover how mental health patterns have 
      developed across the United States and gain a deeper understanding of these 
      trends.")
  )
  
  output$tab4_intro_su <- renderUI(
    HTML("Utilizing the Mental Health Client-Level Data (MH-CLD) from 2018 and 2022, 
         our interactive visualizations explore how substance use diagnoses vary 
         across states and demographic subgroups. These maps and charts enable 
         users to explore trends in substance use diagnoses with a focus on how 
         these patterns have shifted from before to after the COVID-19 pandemic. 
         By interacting with these visual tools, you can better understand changes 
         in substance use patterns and their implications.")
  )
  
  output$tab4_mh_avg <- renderUI(
    HTML("This map illustrates the average number of mental health diagnoses per 
          state, providing an overview of overall mental health trends. For 2018, 
          data is unavailable for Alaska, Kansas, Georgia, New Hampshire, and 
          Maine, with Maine additionally showing no data for 2022. The map reveals 
          significant increases in the average number of mental health diagnoses 
          across states such as Oregon, Wyoming, Utah, and Texas. Missouri has 
          seen the most substantial increase, while West Virginia shows the 
          largest decrease.")
  )
  
  output$tab4_mh_diff <- renderUI(
    HTML("This map displays the average number of differences in mental health 
    diagnoses per state between 2018 and 2022, highlighting notable changes. It 
    visualizes the increase or decrease in the average number of diagnoses, 
    making it easier to see where significant shifts have occurred. States without 
    data in 2018 are marked as “No Data,” reflecting the average difference 
    calculation for both years. States such as Oregon, Colorado, and Texas show 
    a marked increase in diagnoses, while West Virginia exhibits a significant 
    decrease.")
  )
  
  output$tab4_mh_pie <- renderUI(
    HTML("The pie chart reveals the distribution of total mental health diagnoses 
         across various subgroups, including region, age group, gender, race, 
         marital status, employment status, residential status, and veteran status. 
         The data indicates a percent decrease in diagnoses in the Northwest, 
         Midwest, and other regions, among individuals under 18 and those aged 45-64. 
         Diagnoses among females have increased, while those among males have decreased. 
         Racially, there has been a rise in diagnoses among American Indian/Alaska 
         Native and Native Hawaiian/Pacific Islander groups. Marital status data 
         shows a slight percentage increase in diagnoses among those currently 
         married or separated. Employment status trends reveal a percent decrease 
         in diagnoses among those not in the labor force compared to other 
         employment groups. Additionally, there has been a percent increase in 
         diagnoses among individuals residing in private residences and those with 
         veteran status, particularly post-COVID.")
  )
  
  output$tab4_mh_bar <- renderUI(
    HTML("The bar chart compares the prevalence of mental health diagnoses across 
    various subgroups of interest for 2018 and 2022. The data shows consistency 
    in the frequency of depression, anxiety, and trauma/stressor-related disorders 
    among different subgroups pre-and-post COVID-19. Furthermore, bipolar disorders 
    rank fourth among the employment status subgroup, whereas this disorder typically 
    ranks sixth or seventh among other subgroups. This chart provides a detailed 
    view of how specific mental health conditions are distributed among various 
    demographic and employment groups, highlighting any significant changes in the 
    prevalence of these conditions.")
  )
  
  output$tab4_su_perc <- renderUI(
    HTML("This interactive map allows you to explore the percentage of substance 
         use diagnoses reported in each state for 2018 and 2022. This percentage 
         was calculated based on whether or not an individual was diagnosed with 
         substance use disorder from their state’s total respondent population. 
         The map addresses data gaps from Alaska, Kansas, Mississippi, Georgia, 
         North Carolina, New Hampshire, Vermont, and Maine in 2018, and data gaps 
         in 2022 from South Dakota, Kansas, Vermont, and Maine. This map highlights 
         notable increases in several states such as Montana, North Dakota, and 
         Michigan, with Nebraska showing the greatest percent increase, indicating 
         rising substance use concerns. On the other hand, Oregon shows the most 
         substantial percentage decrease in substance use diagnoses, reflecting 
         change in substance use disorder patterns.")
  )
  
  output$tab4_su_diff <- renderUI(
    HTML("This map illustrates the change in the number of substance use disorders 
         in 2018 and 2022. This map provides a clear visual representation of 
         where substantial increases and decreases in substance use diagnoses 
         have occurred. States such as Nebraska demonstrate the most substantial 
         percentage increase, while Oregon has experienced a considerable decrease. 
         This map effectively highlights the most dramatic changes, providing 
         insights into the shifting substance use trends before and after the 
         COVID-19 pandemic.")
  )
  
  output$tab4_su_pie <- renderUI(
    HTML("The pie chart provides a breakdown of substance use diagnoses by various 
         demographic subgroups, including region, age group, gender, race, marital 
         status, employment status, residential status, and veteran status. It 
         reveals an increase in substance use diagnoses in the Northeast and 
         South region and among individuals under 18 and those aged 65 and older. 
         Female substance use diagnoses have increased, whereas diagnoses among 
         males have decreased, a similar trend to the mental health diagnoses. 
         There is also a rise in substance use diagnoses among Black/African American, 
         American Indian/Alaska Native, and Native Hawaiian/Pacific Islander groups. 
         Additionally, there has been an increase in diagnoses among unemployed 
         individuals and those residing in private residencies, as well as veterans, 
         indicating a broader impact across different subgroups.")
  )
  
  output$tab4_su_bar <- renderUI(
    HTML("The bar chart compares the uniqueness of various substance use diagnosis 
         types across different subgroups of interest for 2018 and 2022. Users 
         can filter by subgroup to see how diagnosis types vary among their given 
         subgroup of interest. In summary, this visualization shows that alcohol 
         dependence remains the most common diagnosis, followed by other substance 
         dependences such as sedative, hypnotic, or anxiolytic dependence, and 
         amphetamine dependence. Opioid and cannabis dependence also remain prominent 
         in substance use diagnosis types.")
  )
  
  output$tab5_intro <- renderUI(
    HTML("Utilizing data from the National Substance Use and Mental Health Services 
         Survey (N-SUMHSS), you can explore the changes in facility availability 
         per 10,000 people per state through three different visualization options: 
         animated comparisons, individual year maps, and difference maps showing 
         changes over time. The goal is to identify trends and shifts in facility 
         distribution and their potential impact on service access and treatment 
         outcomes.")
  )
  
  output$tab5_mh <- renderUI(
    HTML("This analysis visualizes the distribution and changes in the number of 
         mental health facilities per 10,000 people across the United States in 
         2018 and 2022. There is a decrease in facilities in South Dakota, which 
         is the only state to experience a reduction, of approximately -0.006 per 
         10,000 people, despite an increase in the average number of mental health 
         diagnoses by about 0.3. Although there is a slight decrease in South Dakota, 
         it is close to 0, meaning there was limited change in mental health facility 
         numbers in 2022 compared to 2018. Utah, on the other hand, saw the 
         greatest rise in mental health facilities, with an increase of 0.575 per 
         10,000 people, alongside a rise in the average number of mental health 
         diagnoses of about 0.1005. West Virginia experienced a decrease in the 
         number of mental health diagnoses in 2022 compared to 2018 but saw a 
         substantial increase in facilities, with an addition of about 0.152 per 
         10,000 people. Additionally, while all other states saw increases in the 
         number of mental health facilities, states such as Texas and Nevada, 
         which had lower facility densities in 2018, demonstrated noticeable 
         improvements by 2022. States like Maine, Alaska, and Vermont maintained 
         high facility densities in 2022. These changes reflect shifts in 
         accessibility and resource allocation in mental health services pre-and-post 
         COVID-19.")
  )
  
  output$tab5_su <- renderUI(
    HTML("This analysis illustrates the distribution and changes in the number of 
         substance-use facilities per 10,000 people across the United States in 
         2018 and 2022. In 2018, states like Texas and South Carolina had lower 
         facility densities, but by 2022, noticeable improvements were observed. 
         Maine, Kentucky, Alaska, and Montana maintained high facility densities 
         in 2022. The data shows a decrease in facilities in Hawaii and South 
         Dakota, with South Dakota being the only state to experience a reduction, 
         while all other states saw increases. Montana had the most significant 
         growth, adding 0.979 facilities per 10,000 people, which may be linked 
         to improved access to treatment and diagnosis for substance use problems. 
         Nebraska experienced the greatest percent increase in substance use problems 
         despite only a modest rise of about 0.094 facilities per 10,000 people. 
         Meanwhile, Kentucky faced an increase in both the percentage of residents 
         with substance use problems and the number of facilities, likely as a 
         response to rising diagnoses. While states such as Texas and South Carolina 
         showed improvements in facility densities, states like Maine, Alaska, 
         and Montana continued to maintain high facility densities in 2022. These 
         changes reveal possible shifts in accessibility and resource allocation 
         for substance use treatment pre-and-post COVID-19.")
  )
  
}
# Run the application
shinyApp(ui = ui, server = server)