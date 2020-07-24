require(shiny)
library(shinydashboard)
library(DT)
## TEXT ANALYSIS LIBRARIES
library(stringr)
library(tm)
library(qdap)
library(NLP)
## DATA MANIPULATION LIBRARIES
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
## VISUALIZATION LIBRARIES
library(scales)
library(RColorBrewer)
library(plotly)
library(wordcloud)
library(ggplot2)
library(ggrepel)
library(sf)
library(maps)

options(shiny.sanitize.errors = TRUE)

ui <- dashboardPage(
  
  dashboardHeader(title = "Yesterday Once More"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("chart-area")),
      menuItem("Speech Text Analysis", tabName = "byspeech", icon = icon("commenting")),
      menuItem("Compare All Speeches", tabName = "comparespeeches", icon = icon("microphone")),
      menuItem("Words/Topics Through Time", tabName = "throughtime", icon = icon("clock")),
      menuItem("Conference Information", tabName = "conference_info", icon = icon("group")),
      menuItem("About the Data", tabName = "about", icon = icon("question-circle")),
      tags$br(),tags$br(),
      HTML("<center>"),actionButton('glossary',"Glossary"),HTML("</center>"),
     tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      )
    )
  ),
  dashboardBody(
    tabItems(
      ###### WELCOME #####
      tabItem(tabName = "welcome",
              fluidRow(
                box(width=12,status="primary",
                  fluidRow(
                    column(12,
                           tags$h1("Yesterday Once More: ASA Presidential Addresses")
                    )
                  )
                ),
                box(title = "Welcome",width = 12,status="primary",solidHeader = TRUE,
                    fluidRow(
                        column(3,
                               tags$img(src = "http://jsm2020.s3.amazonaws.com/images/2020.jpg", 
                               alt="2020 President Wendy Martinez", class="president_image")
                               ),
                        column(9,
                               HTML(paste0('<blockquote><p class="quotation">',
                                      'I’m passionate about exploring data, discovering interesting stories in text, ',
                                      'and the history of our profession. Join me on this journey as we explore our ',
                                      'statistical and data science roots during virtual JSM 2020. </p>
                                    <footer class="footer_style"> Wendy Martinez, ASA President (2020)</footer>
                                    </blockquote> '))
                               )
                    )
                )
              ),
              fluidRow(
                box(title = "About This Site",width = 12,status="primary",solidHeader = TRUE,
                    includeHTML("./html/about_this_site.html")
                )
              ),
              fluidRow(
                box(title = "Acknowledgements",width = 12,status="primary",solidHeader = TRUE,
                    includeHTML("./html/acknowledgements.html")
                )
              )
      ),
      ##############################
      ###### BY SPEECH #####
      tabItem(tabName = "byspeech",
              fluidRow(
                box(title = "Select an ASA President",width = 12,status="primary",solidHeader = TRUE,
                    column(3, style="min-width:200px",
                           selectInput("presspeech","Select President:", 
                                       choices = allspeeches$yearpresident[1:nrow(allspeeches)-1],
                                       selected = "2019 - Karen Kafadar", selectize = FALSE, width=175),
                           htmlOutput("presSpeechImage1")
                    ),
                    column(8,
                           htmlOutput("pres_narrative"),
                           tags$div(style="display:inline-block", actionButton("showspeech","Read the Speech"))
                           )
                )
              ),
              fluidRow(
                valueBoxOutput("spcwords",width = 3),
                valueBoxOutput("len_rank",width = 3),
                valueBoxOutput("spcttr",width = 3),
                valueBoxOutput("spcfkgrade",width = 3)
              ),
              fluidRow(
                box(title="Word Frequency",width = 12,status = "primary",
                    solidHeader = TRUE,
                    column(6,
                           sliderInput("seltopn", "Top n Words:", 5, 50, 15, step=5),
                           plotOutput("freqplot", height="500")
                    ),
                    column(6,
                           sliderInput("selwords", "Max # of Words:", 50, 500, 250, step=50),
                           plotOutput("speechcloud", height="500")
                    )
                )
              ),
              fluidRow(
                box(title="Topic Modeling",width = 12,status = "primary",solidHeader = TRUE,
                    plotOutput("topic_models"),
                    actionButton("show_word_loadings", "Show Word/Topic Association"),
                    footer = "Topics extracted using LDA function in the topicmodels package"
                )
              ),
              fluidRow(
                box(title="Sentiment Analysis",width = 12,status = "primary",
                    solidHeader = TRUE,
                    column(6,
                           tags$h3("Sentiment Across Speech"),
                           tags$p("This plot shows the polarity, or the relative use of positively and negatively valenced words, for each sentence in the speech."),
                           tags$br(),
                           plotOutput("spsentplot", height="500")
                    ),
                    column(6,
                           sliderInput("selsentwords", "Max # of Words:", 50, 500, 250, step=50),
                           plotOutput("sentcloud", height="500")
                    )
                )
              ),
              fluidRow(
                box(title="New Words",width = 12,status = "primary",
                    solidHeader = TRUE,
                    tags$h3(textOutput("newwordslabel")),
                    tags$head(tags$style("#newwords{font-size:1.2em}")),
                    tags$p(htmlOutput("newwords")),
                    footer = "Some of the never before seen words are mispellings or words that were combined in error during transcription or data collection for this app."
                )
              ),
              fluidRow(
                box(title="People & Groups Extracted From Speech",width = 12,status = "primary",
                    solidHeader = TRUE,
                    column(6,
                           tags$h3(textOutput("ent_people_label")),
                           tags$hr(),
                           tags$head(tags$style("#ent_people{font-size:1.1em}")),
                           tags$p(htmlOutput("ent_people"))
                    ),
                    column(6,
                           tags$h3(textOutput("ent_groups_label")),
                           tags$hr(),
                           tags$head(tags$style("#ent_groups{font-size:1.1em}")),
                           tags$p(htmlOutput("ent_groups"))
                    ),
                    footer = "Entities extracted in Python using the spaCy package."
                )
              ),
              fluidRow(
                box(title="Words in Context",width = 12,status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             textInput("speechword","Words to Search:",
                                       value="statistics, data science",
                                       placeholder = "Enter terms separated by commas")
                      ),
                      column(2,
                             tags$br(),
                             actionButton("submitspeech", "Submit")
                      )),
                    tags$hr(style=("border-bottom:1px solid black;margin-top:2px")),
                    dataTableOutput("speechwordsincontext")
              
                )
              )
      ),
      ##############################
      ##### COMPARE SPEECHES #####
      tabItem(tabName = "comparespeeches",
              fluidRow(
                box(title="Compare All Speeches on Various Metrics",width = 12,height=700,status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                      column(4,
                             selectInput("xvarspeech","X-Axis Variable", 
                                         choices = list(Readibility = all_speech_dropdown$readibility,
                                                        Sentiment = all_speech_dropdown$sentiment,
                                                        Topics = all_speech_dropdown$topics),
                                         selected = "Number of Words",
                                         selectize = FALSE)
                      ),
                      column(4,
                             selectInput("yvarspeech","Y-Axis Variable", 
                                         choices = list(Readibility = all_speech_dropdown$readibility,
                                                        Sentiment = all_speech_dropdown$sentiment,
                                                        Topics = all_speech_dropdown$topics),
                                         selected = "Flesch-Kincaid Grade",
                                         selectize = FALSE)
                      ),
                      column(4,
                             selectInput("cvarspeech","Color Variable:",c("Affiliation", "Gender",
                                                                          "Era","Main Topic"),
                                         selected = "Affiliation")
                      )
                    ),
                    fluidRow(id="scatterdiv",
                             plotlyOutput("speechscatter", height=750)
                    )
                  )
                ),
              fluidRow(
                box(title="Most Used Words Across All Speeches",width = 12,status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                      column(8,
                             tags$img(src="overall_wordcloud.png", class="wordcloud_image")
                             ),
                      column(4,
                             includeHTML("./html/top_words_table.html")
                             )
                    ),
                    footer = paste0("These values were pre-computed due to resource requirements. Stopwords (",
                    "including words like statistical, statistics, and data) were removed for clarity.")
              )
              )
      ),
      ############################
      ##### WORDS THROUGH TIME #####
      tabItem(tabName = "throughtime",
              fluidRow(
                box(title="Word Usage Over Time",width = 12, status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                       column(4,
                               textInput("words","Words To Search:",value="data is, data are",
                                         placeholder = "Enter up to 3 terms separated by commas"),
                               
                        ),
                        column(2,
                               tags$br(),
                               actionButton("submit", "Submit")
                        ),
                        column(3,
                               selectInput("time_colorvar","Color Variable", c("Affiliation","Gender","None"),
                                           selected="Affiliation")
                              ),
                        column(3,
                               sliderInput("scale", "Circle Scale Size:", 4, 40, 20, step=4)
                               )
                    ),
                    fluidRow(
                        column(12,
                          plotOutput("timeplot", height=700,width = "100%"),
                          downloadButton('download_timeplot', 'Download Plot')
                        )
                    )
                )),
              fluidRow(
                box(title="Words by Decade",width = 12,height=700,status = "primary",
                    solidHeader = TRUE,
                    fluidRow(
                      column(2,style="min-width:200px",
                             uiOutput('decadetext')
                      ),
                      column(5,
                             sliderInput("decadeslider", "Select Decade:", min=1910, max=2010,step = 10, value="2010",
                                         sep="")
                      )
                    ),
                    fluidRow(
                      column(6,
                             plotOutput("decadefreqplot", height="500")
                      ),
                      column(6,
                             HTML("<center>"), plotOutput("decadecompcloud", height="450"), HTML("</center>"),
                             HTML(paste0("<small>A comparison wordcloud shows the relative frequency of word usage. ",
                                         "Word size in this cloud indicates how many <b>more</b> times it was used by one ",
                                         "group than the other.</small>"))
                      )
                    )
                    
                )
              ),
                fluidRow(
                      box(title="Topics Over Time",width = 12, status = "primary",
                          solidHeader = TRUE,
                          fluidRow(
                            column(3,
                                  selectInput("select_topic", "Topic", topic_crosswalk$topic_label,
                                              selected="Surveys")
                            ),
                            column(3,
                                   selectInput("topic_colorvar", "Color Variable", c("Affiliation","Gender","None"),
                                               selected="Affiliation")
                                   )
                          ),
                          fluidRow(
                            column(12,
                                   plotOutput("topicplot", height=700,width = "100%"),
                                   downloadButton('download_topicplot', 'Download Plot')
                            )
                          )
                    )
                  )
      ),
      #####################################
      ##### CONFERENCE INFORMATION ##########
      tabItem(tabName = "conference_info",height=1240,
            fluidRow(
              box(title = "Conference Details",width = 12,status="primary",solidHeader = TRUE,
                  fluidRow(
                    column(9,
                           sliderInput("yearslider", "Select Year:", min=1908, max=2020,step = 1, value="2019",
                                       sep="")
                    ),
                    column(3,
                           uiOutput('yeartext'),
                           uiOutput('no_meeting_warning')
                    )
                  ),
                  fluidRow(
                    column(4,
                           tags$h1('Conference', class='welcome_section_title'),
                           tags$hr(class="welcome_line_style"),
                           uiOutput("conference_table")
                    ),
                    column(8,
                           tags$h1('President', class='welcome_section_title'),
                           tags$hr(class="welcome_line_style"),
                           fluidRow(
                             column(3,
                                    htmlOutput("president_image")
                             ),
                             column(9,
                                    uiOutput("president_table")
                             )
                           )
                    )
                  ),
                  fluidRow(
                    column(12,
                           tags$h1('Location', class='welcome_section_title'),
                           tags$hr(class="welcome_line_style")    
                    )
                  ),
                  fluidRow(
                    column(7,
                           plotOutput("location_map"),
                           HTML("<small class='text-muted'>Size of point indicates the number of conferences hosted at that location.</small>")
                    ),
                    column(5,
                           htmlOutput("location_image"),
                           uiOutput("image_attribution"),
                           tags$br(),
                           uiOutput("location_table")
                    )
                  )
              )
            )  
              
      ),
      
      #############################
      ##### HELP AND ACKNOWLEDGEMENTS #####
      tabItem(tabName = "about",
              fluidRow(
                box(title="About the Data",width = 12,status = "primary", solidHeader = TRUE,
                    includeHTML("./html/data_description.html")
                    )
              ),
              fluidRow(
                box(title="About the Analyses",width = 12,status = "primary", solidHeader = TRUE,
                    includeHTML("./html/analysis_description.html")
                    )
              )
      )
      ######################################
      )
    )
)