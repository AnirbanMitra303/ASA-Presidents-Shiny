require(shiny)

server <- function(input, output, session) { 
  source("global.R", local=TRUE)
  
  ###### WELCOME ####################
  observeEvent(input$glossary, {
    showModal(modalDialog(
      title = "Glossary",
      includeHTML("./html/glossary.html"),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  pres_dat <- reactive({
    allspeeches[allspeeches$year == input$yearslider, ]
  })
  
  location_dat <- reactive({
    if(nrow(pres_dat()) == 1){
      locations[locations$location %in% unique(pres_dat()$location), ]
    }
  })
  
  states_dat <- reactive({
    map_data("state")
  })
  
  countries_dat <- reactive({
    map_data("world") %>%
      filter(region %in% c("USA","Canada"))
  })
  
  output$president_title <- renderUI({
    if(!input$yearslider %in% missing_data_years){
      HTML(paste0("<h3>", pres_dat()$president_name_forward, "</h3>"))
    }
  })
  
  output$yeartext <- renderUI({
    HTML(paste0("<h1 class='year_text small_margin'>", input$yearslider, "</h1>"))
  })
  
  output$no_meeting_warning <- renderUI({
    if(input$yearslider %in% missing_data_years){
      HTML(paste0("<h3 style='color:red' class='small_margin'>NO MEETING</h3>"))
    }
  })
  
  output$conference_table <- renderUI({
    if(!input$yearslider %in% missing_data_years){
      HTML(paste0("<table class='content_table'>",
                  "<tr class='content_row'><td style='min-width:60px'><b>JSM #: </b></td><td>", pres_dat()$jsm_number," Annual</td></tr>",
                  "<tr class='content_row'><td><b>Dates: </b></td><td>", pres_dat()$jsm_dates,"</td></tr>",
                  ifelse(!is.na(pres_dat()$jsm_theme), paste0("<tr class='content_row'><td><b>Theme: </b></td><td>", pres_dat()$jsm_theme,"</td></tr>"),""),
                  "</table>"))
    }
  })
  
  output$president_image <- renderUI({
    if(!input$yearslider %in% missing_data_years){
      imglink <- paste0("http://jsm2020.s3.amazonaws.com/images/",input$yearslider,".jpg")
      tags$img(src = imglink, alt=input$presspeech, class="president_image")
    }
  })
  
  output$president_table <- renderUI({
    if(!input$yearslider %in% missing_data_years){
      HTML(paste0("<table class='content_table'>",
                  "<tr class='content_row'><td style='min-width:120px'><b>Name: </b></td><td>", pres_dat()$president_name_forward,"</td></tr>",
                  "<tr class='content_row'><td><b>Affiliation: </b></td><td>", pres_dat()$affiliation,"</td></tr>",
                  "<tr class='content_row'><td><b>Pres #: </b></td><td>", pres_dat()$pres_number,"</td></tr>",
                  "<tr class='content_row'><td><b>Speech Title: </b></td><td>", pres_dat()$title,"</td></tr>",
                  "<tr class='content_row'><td><b>Speech Date: </b></td><td>", pres_dat()$date_of_address,"</td></tr>",
                  "</table>"))
    }
  })
  
  output$location_image <- renderUI({
    imglink <- location_dat()$image_url[[1]]
    tags$img(src = imglink, style="border-radius:7px; width:100%")
  })
  
  output$location_map <- renderPlot(({
    current_location <- location_dat()$location[[1]]
    plot_location(locations, states_dat(), countries_dat(), current_location)
  }))
  
  output$image_attribution <- renderUI({
    if(nrow(location_dat()) > 0){
      HTML(paste0("<small class='text-muted'>Source: Flickr, Credit: <a href='", location_dat()$photographer_page,"'>", 
                  location_dat()$image_credit,"</a>, License: ", location_dat()$image_license,"</small>"))
    }
  })
  
  output$location_table <- renderUI({
    if(nrow(location_dat()) > 0){
      HTML(paste0("<table class='content_table'>",
                  "<tr class='content_row'><td style='min-width:120px'><b>Location: </b></td><td>", location_dat()$location,"</td></tr>",
                  "<tr class='content_row'><td><b>Times Hosting: </b></td><td>", location_dat()$num_hosting,"</td></tr>",
                  "<tr class='content_row'><td><b>Years Hosting: </b></td><td>", location_dat()$years,"</td></tr>",
                  "</table>"))
    }
    
  })
  
  ####################################
  ####### BY SPEECH FUNCTIONS ######
  speech_text <- reactive({
    allspeeches[allspeeches$yearpresident == input$presspeech, c("speechtext","category")]
  })

  speech_tdm <- reactive({
    withProgress(message = 'Calculating Word Frequency', value = 0, {
      make_tdm(speech_text())
    })
  })
  
  speech_link_id <- reactive({
    allspeeches$id[allspeeches$yearpresident == input$presspeech]
  })
  
  speech_year <- reactive({
    allspeeches$year[allspeeches$yearpresident == input$presspeech]
  })
  
  speech_doi <- reactive({
    allspeeches$doi[allspeeches$yearpresident == input$presspeech]
  })
  

  output$freqplot <- renderPlot({
    withProgress(message = 'Building Most Used Terms Plot', value = 0, {
       topncount(speech_tdm(), top=input$seltopn)
    })
  })

  output$speechcloud <- renderPlot({
    withProgress(message = 'Building Wordcloud', value = 0, {
       suppressWarnings(cloud(speech_tdm(), words = input$selwords))
    })
  })

  output$spsentplot <- renderPlot({
    withProgress(message = 'Calculating Sentiment Analysis', value = 0, {
      spsentgraph(speech_text()$speechtext[[1]])
    })
  })

  output$sentcloud <- renderPlot({
    withProgress(message = 'Building Sentiment Wordcloud', value = 0, {
      sentiment <- allspeeches[allspeeches$yearpresident == input$presspeech, c("pos.words","neg.words")]
      suppressWarnings(spsentcloud(sentiment, maxwords = input$selsentwords))
    })
  })

  output$spcwords <- renderValueBox({
    words <- allspeeches[allspeeches$yearpresident == input$presspeech, "tokens"][[1]]
    valueBox(
      value = formatC(words, digits = 0, format = "f"),
      subtitle = "Number of Words",
      icon = icon("book"),
      color = if (words <= median(allspeeches$tokens, na.rm = T)) "green" else "red"
    )
  })

  output$len_rank <- renderValueBox({
    len_rank <- allspeeches[allspeeches$yearpresident == input$presspeech, "length_rank"][[1]]
    valueBox(
      value = formatC(len_rank, digits = 0, format = "f"),
      subtitle = "Length Rank",
      icon = icon("ruler"),
      color = if (len_rank >= length(len_rank)/2) "green" else "red"
    )
  })

  output$spcttr <- renderValueBox({
    ttr <- allspeeches[allspeeches$yearpresident == input$presspeech, "ttr"][[1]]
    valueBox(
      value = formatC(ttr, digits = 3, format = "f"),
      subtitle = "Type-Token Ratio (TTR)",
      icon = icon("calculator"),
      color = if (ttr <= median(allspeeches$ttr, na.rm=T)) "green" else "red"
    )
  })

  output$spcfkgrade <- renderValueBox({
    fkg <- allspeeches[allspeeches$yearpresident == input$presspeech, "fkage"][[1]]
    valueBox(
      value = formatC(fkg, digits = 1, format = "f"),
      subtitle = "Flesch-Kincaid Age Score",
      icon = icon("child"),
      color = if (fkg <= median(allspeeches$fkage, na.rm = T)) "green" else "red"
    )
  })

  output$presSpeechImage1 <- renderUI({
    filename <- unique(allspeeches[allspeeches$yearpresident == input$presspeech, "year"])
    imglink <- paste0("http://jsm2020.s3.amazonaws.com/images/",filename,".jpg")
    tags$img(src = imglink, alt=input$presspeech, class="president_image")
  })
  
  output$pres_narrative <- renderUI({
    HTML(speech_narrative(allspeeches, input$presspeech))
  })

  output$topic_models <- renderPlot({
    topic_plot(topics, speech_link_id())
  })
  
  output$speechwordsincontext <- renderDataTable({
    input$submitspeech
    withProgress(message = 'Extracting Words in Context', value = 0, {
      wordlist <- isolate(stringsplitter(input$speechword))
      context(speech=speech_text()[ ,"speechtext"],wordlist)
    })
  }, options = list(
    lengthMenu = list(c(10, 20, 30, -1), c('10', '20', '30', 'All')),
    pageLength = 10))

  output$newwords <- renderUI({
    new_words <- entities[entities$id == speech_link_id(), "new_word"][[1]]
    ents <- paste(sort(new_words), collapse = ", ")
    HTML(ents)
  })

  output$newwordslabel <- renderText({
    num_new_words <- length(entities[entities$id == speech_link_id(), "new_word"][[1]])
    paste(num_new_words, "words/names never before used in an ASA address")
  })

   output$ent_people <- renderUI({
     new_words <- entities[entities$id == speech_link_id(), "people"][[1]]
     ents <- paste(sort(new_words), collapse = ", ")
     HTML(ents)
   })
  
   output$ent_people_label <- renderText({
     num_new_words <- length(entities[entities$id == speech_link_id(), "people"][[1]])
     paste0("People (", num_new_words, ")")
   })
   
   output$ent_groups <- renderUI({
     new_words <- entities[entities$id == speech_link_id(), "groups"][[1]]
     ents <- paste(sort(new_words), collapse = ", ")
     HTML(ents)
   })
  
   output$ent_groups_label <- renderText({
     num_new_words <- length(entities[entities$id == speech_link_id(), "groups"][[1]])
     paste0("Groups (", num_new_words, ")")
   })

  observeEvent(input$showspeech, {
    showModal(modalDialog(
      title = "ASA Presidential Address",
      HTML(paste0("<iframe width='775' height='575' src='https://jsm2020.s3.amazonaws.com/pdfs/", 
                  speech_year(), ".pdf'></iframe>")),
      easyClose = TRUE,
      footer = HTML(paste0("<small class='text-muted'>Source: <a href='",speech_doi(),
      "' target='_blank' rel='nofollow'>",speech_doi(),"</a></small>"))
    ))
  })

  observeEvent(input$show_word_loadings, {
    showModal(modalDialog(width=1000,
      title = "Word/Topic Associations",
      HTML(paste0("<p>The following plot shows the top 10 words associated with ",
                  "each topic. These associations were used to choose the generic ",
                  "topic label.","</p>")),
      HTML("<img style='width:100%' src='word_topic_associations.png'>"),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  

  #######################################
  ###### COMPARE SPEECHES FUNCTIONS #####
  output$speechscatter <- renderPlotly({
    xInSp <- switch(input$xvarspeech,
                    "Number of Words" = "tokens",
                    "Number of Unique Words"="types",
                    "Average Sentence Length"="avg.sentc.length",
                    "Average Word Length"="avg.word.length",
                    "Length Rank" = "length_rank",
                    "Type Token Ratio (TTR)" = "ttr",
                    "Flesch-Kincaid Grade" = "fkgrade",
                    "Flesch-Kincaid Age" = "fkage",
                    "Sentiment" = "polarity",
                    "Number of Unique Positive Words"="num_pos_words",
                    "Number of Unique Negative Words"="num_neg_words",
                    "Year" = "year",
                    "Topic Loading for Government" = "topic_1",
                    "Topic Loading for Economics" = "topic_2",
                    "Topic Loading for Science" = "topic_3",
                    "Topic Loading for Education" = "topic_4",
                    "Topic Loading for Technology" = "topic_5",
                    "Topic Loading for Health" = "topic_6",
                    "Topic Loading for Surveys" = "topic_7",
                    "Topic Loading for Profession" = "topic_8",
                    "Topic Loading for Business" = "topic_9"
    )

    yInSp <- switch(input$yvarspeech,
                    "Number of Words" = "tokens",
                    "Number of Unique Words"="types",
                    "Average Sentence Length"="avg.sentc.length",
                    "Average Word Length"="avg.word.length",
                    "Length Rank" = "length_rank",
                    "Type Token Ratio (TTR)" = "ttr",
                    "Flesch-Kincaid Grade" = "fkgrade",
                    "Flesch-Kincaid Age" = "fkage",
                    "Sentiment" = "polarity",
                    "Number of Unique Positive Words"="num_pos_words",
                    "Number of Unique Negative Words"="num_neg_words",
                    "Topic Loading for Government" = "topic_1",
                    "Topic Loading for Economics" = "topic_2",
                    "Topic Loading for Science" = "topic_3",
                    "Topic Loading for Education" = "topic_4",
                    "Topic Loading for Technology" = "topic_5",
                    "Topic Loading for Health" = "topic_6",
                    "Topic Loading for Surveys" = "topic_7",
                    "Topic Loading for Profession" = "topic_8",
                    "Topic Loading for Business" = "topic_9"
    )

    cInSp <- switch(input$cvarspeech,
                    "Affiliation" = "category",
                    "Gender"="gender",
                    "Era" = "period",
                    "Main Topic"="topic_label"
    )

    df <- allspeeches[-107 ,c("president","year","yearpresident","id","category","gender",
                              "tokens","types","ttr","fkgrade","fkage","avg.sentc.length","avg.word.length",
                              "polarity","num_neg_words","num_pos_words","period","length_rank")]
    
    df <- left_join(df, topics) %>%
      mutate(main_topic = paste0("topic_", main_topic)) %>%
      left_join(topic_crosswalk, by=c("main_topic"="topic"))

    g <- ggplot(df,aes_string(x=xInSp, y=yInSp)) +
      geom_point(aes_string(color=cInSp,pch=cInSp, text="yearpresident"),size=2, alpha=0.5) +
      labs(
        x = input$xvarspeech,
        y = input$yvarspeech,
        color = input$cvarspeech,
        pch= input$cvarspeech
      ) +
      theme(plot.title = element_blank(),
            plot.background = element_rect(fill = 'white', colour = 'white'),
            panel.border = element_rect(fill = NA, color = 'white', size = 2),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 10, color="black", face="bold"),
            axis.title = element_text(size = 12, face="bold", color = "black"),
            axis.ticks = element_blank(),
            axis.line = element_line(colour = "black", size=1),
            legend.background = element_blank(),
            legend.key = element_blank(),
            legend.text = element_text(size = 10, color= "black"),
            legend.title = element_text(size = 12,face="bold"),
            legend.position = "right") +
      guides(colour = guide_legend(override.aes = list(size=4)),
             pch = guide_legend(override.aes = list(size=4)))

    (gg <- ggplotly(g, tooltip = "text", height=550, width=850))
  })

  ############################################  
  ###### WORDS THROUGH TIME FUNCTIONS ##########
  output$timeplot <- renderPlot({
    input$submit

    plotTimeplot(for_print=FALSE)
  })

  plotTimeplot <- function(for_print=FALSE) {
    colorIn <- switch(input$time_colorvar,
                    "Affiliation" = "category",
                    "Gender"="gender",
                    "None"=FALSE,
    )
    
    withProgress(message = 'Building Word Through Time Plot', value = 0, {
        wordlist <- isolate(stringsplitter(input$words))

        words_over_time(df=allspeeches,words = wordlist, stop=TRUE, colorvar=colorIn,
                      leg=input$time_colorvar, scale=input$scale, forprint=for_print)
    })
  }

  output$download_timeplot = downloadHandler(
    filename = 'words_through_time.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 2500, height = 2200,
                       res = 200, units = "px")
      }
      ggsave(file, plot = plotTimeplot(for_print=TRUE), device = device)
    })
  
  output$topicplot <- renderPlot({
    plotTopicplot(for_print=FALSE)
  })
  
  plotTopicplot <- function(for_print=FALSE) {
    dataIn <- switch(input$select_topic,
                     "Government" = "topic_1",
                     "Economics" = "topic_2",
                     "Science" = "topic_3",
                     "Education" = "topic_4",
                     "Technology" = "topic_5",
                     "Health" = "topic_6",
                     "Surveys" = "topic_7",
                     "Profession" = "topic_8",
                     "Business" = "topic_9"
    )
    
    colorIn <- switch(input$topic_colorvar,
                     "Affiliation" = "category",
                     "Gender"="gender",
                     "None"=FALSE,
    )
    
    withProgress(message = 'Building Topics Through Time Plot', value = 0, {
      wordlist <- isolate(stringsplitter(input$words))
      
      topics_over_time(topic = dataIn, topic_title=input$select_topic,  colorvar=colorIn,
                    legend_title=input$topic_colorvar, forprint=for_print)
    })
  }
  
  output$download_topicplot = downloadHandler(
    filename = 'topics_through_time.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = 2500, height = 2200,
                       res = 200, units = "px")
      }
      ggsave(file, plot = plotTopicplot(for_print=TRUE), device = device)
    })
  
  output$decadetext <- renderUI({
    HTML(paste0("<h1 class='decade_text small_margin'>", input$decadeslider, "s</h1>"))
  })
  
  decadetdm <- reactive({
      make_tdm(allspeeches[allspeeches$decade ==  input$decadeslider & allspeeches$category != "Unknown", 
                         c("speechtext","category")], collapse_var="category")
  })
  
  output$decadefreqplot <- renderPlot({
    withProgress(message = 'Building Decade Frequency Plot', value = 0, {
      topncount(decadetdm(), top=20)
    })
  })
  
  output$decadecompcloud <- renderPlot({
    withProgress(message = 'Building Decade Wordcloud', value = 0, {
      suppressWarnings(compcloud(decadetdm(),max=450))
    })
  })
  #######################################

  }