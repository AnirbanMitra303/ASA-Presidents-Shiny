allspeeches <- readRDS("./data/asa_data.rds")
allspeeches$decade <- paste0(substr(as.character(allspeeches$year),1,3),"0")
allspeeches$num_neg_words <- unlist(lapply(allspeeches$neg.words, length))
allspeeches$num_pos_words <- unlist(lapply(allspeeches$pos.words, length))

entities <- readRDS("./data/entities.rds")
locations <- readRDS("./data/locations.rds")
topics <- readRDS("./data/topics.rds")

#Load the Data
stop_words <- c("the","an","will","can","also", "that","thats", "asa","american","statistical","statistics",
                "statistician","statisticians","association","joint","meetings", "may", "might", "must", "one",
                "many","per", "new","one","two","first","second","third", "cent","even","upon","much","time",
                "year","years","use","used","must","good","great","better","best","way","bad","like","jsm",
                "get","make","now","however","often","well","say","just")

topic_crosswalk <- data.frame(topic = c("topic_1","topic_2","topic_3","topic_4","topic_5",
                                        "topic_6","topic_7","topic_8","topic_9"),
                              topic_label = c("Government","Economics","Science","Education","Technology",
                                              "Health","Surveys","Profession","Business"),
                              stringsAsFactors = F)

missing_data_years <- c(1909, 1911, 1913, 1916, 1920, 1942, 1943, 1971)

all_speech_dropdown <- list(readibility = c("Number of Words", "Number of Unique Words",
                         "Average Word Length", "Average Sentence Length", "Length Rank",
                          "Type Token Ratio (TTR)","Flesch-Kincaid Grade", "Flesch-Kincaid Age"),
                          sentiment = c("Sentiment","Number of Unique Positive Words", "Number of Unique Negative Words"),
                          topics = c("Topic Loading for Education","Topic Loading for Health",
                          "Topic Loading for Business","Topic Loading for Government",
                          "Topic Loading for Technology","Topic Loading for Surveys",
                          "Topic Loading for Economics","Topic Loading for Science",
                          "Topic Loading for Profession"))

#Frequency Charts from Words
strcounts <- function(data,list, only=TRUE) {
  cols <- c("term","year","period","name","category","gender","count")
  counts <- data.frame(matrix(nrow=1,ncol=7))
  names(counts) = cols
  
  text <- regexer(list, only)
  for(j in 1:nrow(data)){
    for(i in 1:length(text)){
      x <- str_count(data$speechtext[j], text[i])
      df <- data.frame(simpleCap(list[i]), data$year[j], data$period[j],data$president[j],
                       data$category[j],data$gender[j], x)
      names(df) = cols
      counts <- rbind(counts,df)
    }
  }
  counts <- counts[!is.na(counts$term), ]
  return(counts)
}

context <- function(speech, list, only=TRUE) {
  ct <- data.frame(matrix(nrow=1,ncol=2))
  names(ct) <- c("term", "sentence")
  
  text <- regexer(list, only)
  
  for(i in 1:length(text)){
    search <- paste0("(?i)((?=[^.\\n]*(",text[i],"))[^.\\n]+\\.?)")
    x <- str_extract_all(speech, search)
    if(length(x[[1]]) == 0){next}
    temp <- data.frame(simpleCap(list[i]), x)
    names(temp) = c("term", "sentence")
    ct <- rbind(ct,temp)
  }

  ct <- ct[!is.na(ct$sentence), ]
  return(ct)
}

regexer <- function(text, only=TRUE){
  
  x <- strsplit(text, "\\|")
  terms <- length(text)
  
  for(i in 1:terms){
    words <- length(x[[i]])
    if(str_count(text[i],"\\|") > 0){
      for(j in 1:words){
        l <- substr(x[[i]][[j]], 1, 1)
        m <- paste0("[",toupper(l),tolower(l), "]")
        x[[i]][[j]] <- paste0(m,substr(x[[i]][[j]], 2, nchar(x[[i]][[j]])))
      }
    }else {
      l <- substr(x[[i]], 1, 1)
      m <- paste0("[",toupper(l),tolower(l), "]")
      x[[i]] <- paste0(m,substr(x[[i]], 2, nchar(x[[i]])))      
    }
    x[[i]] <- paste(x[[i]], collapse="|")
  }
  
  x <- unlist(x)
  
  x <- strsplit(x, " ")
  terms <- length(x)
  
  for(i in 1:terms){
    words <- length(x[[i]])
    if(str_count(x[i], "\\s") > 0){
      for(j in 2:words){
        l <- substr(x[[i]][[j]], 1, 1)
        m <- paste0("[",toupper(l),tolower(l), "]")
        x[[i]][[j]] <- paste0(m,substr(x[[i]][[j]], 2, nchar(x[[i]][[j]])))
      }
    }
    x[[i]] <- paste(x[[i]], collapse=" ")
    if(only ==TRUE){ x[[i]] <- paste0("\\b",x[[i]],"\\b")}
  }
  x <- unlist(x)
  
  return(x)
}

simpleCap <- function(x) {
  s <- strsplit(x, "\\|")[[1]]
  s <- paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse="|")
  s <- strsplit(s, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2), sep="", collapse=" ")
}

stringsplitter <- function(wordstring){
  x <- strsplit(wordstring, ",")[[1]] %>%
    str_trim(side="both") %>%
    unlist(.)
  return(x)
}

make_tdm <- function(text, collapse_var=NA){
  
  if(!is.na(collapse_var)){
    text <- text %>%
      group_by_(collapse_var) %>%
      summarize(text = paste(speechtext, collapse=" "))
    levels <- text[, collapse_var]
  }else{
    text <- text %>%
      summarize(text = paste(speechtext, collapse=" "))
    levels <- "none"
  }
  
  docs <- VCorpus(VectorSource(text$text)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(removeWords, stop_words) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument)
  
  tdm <- TermDocumentMatrix(docs) %>%
    as.matrix()
  
  colnames(tdm) <- levels[[1]]
  tdm <- reshape2::melt(tdm)
  names(tdm) <- c("term","variable","freq")
  tdm <- arrange(tdm, variable, desc(freq))
  
  return(tdm)
}

topncount <- function(tdm, top=15, col=c("lightsteelblue3","olivedrab3","indianred3")){
  
  order <- tdm %>% group_by(term) %>%
    summarize(sumfreq=sum(freq)) %>%
    arrange(desc(sumfreq)) %>%
    slice(1:top)
  
  x <- tdm %>%
    filter(term %in% order$term) %>%
    mutate(term = factor(term, rev(order$term), ordered=TRUE),
           variable = factor(variable, c("Academia","Government","Industry","none")))
  
  p <- ggplot(x, aes(x=term, freq, fill=as.factor(variable), group=as.factor(variable))) +
    geom_bar(stat="identity", color="white",size=.3, alpha=0.7) +
    scale_fill_manual(values = col) +
    scale_y_continuous(expand=c(0,0), limits=c(0,max(order$sumfreq)+10)) +
    labs(
      title = "Word Count",
      x = "Word",
      y = "Count",
      fill = ""
    ) +
    theme(plot.title = element_blank(),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.border = element_rect(fill = NA, color = 'white', size = 2),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10, color="black", face="bold"),
          axis.title.x = element_text(size = 12, face="bold", color = "black"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(colour = "black", size=1),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 12, color= "black"),
          legend.title = element_text(size = 12,face="bold"),
          legend.position = "none") +
    coord_flip()
  
  if(length(unique(x$variable)) > 1){
    p <- p + theme(legend.position = "bottom") +
      guides(fill = guide_legend(override.aes = list(size=11)))
  }
  
  p
}

cloud <- function(tdm, words=500, color="RdBu"){
  par(mar = rep(0, 4))
  wordcloud(tdm$term, tdm$freq, max.words = words, min.freq =2, scale=c(4,0.5), random.order = FALSE,
            random.color = FALSE, colors= brewer.pal(8, color))
}

compcloud <- function(tdm, max=200, col=c("lightsteelblue3","olivedrab3","indianred3")){
  
  tdm <- tdm %>%
    data.frame(stringsAsFactors = F) %>%
    spread(key="variable",value="freq") %>%
    mutate(term = as.character(term))
  
  mat <- tdm
  
  row.names(mat) <- tdm$term
  mat <- mat[ ,2:ncol(mat)]
  
  par(mar = rep(0,4))
  comparison.cloud(mat, random.order=FALSE, scale = c(5, .5), 
                   rot.per=.2,min.freq=2,
                   colors = col,title.bg.colors = col,
                   fixed.asp=TRUE,title.size = 2, max.words=max)
}

spsentgraph <- function(speechtext){
  speech.df <- data.table(speech=speechtext)
  
  sentences <- data.table(sentSplit(speech.df, "speech"))
  # Add a sentence counter and remove unnecessary variables
  sentences[, sentence.num := seq(nrow(sentences))]
  sentences[, tot := NULL]
  setcolorder(sentences, c("sentence.num", "speech"))
  
  # Syllables per sentence
  sentences[, syllables := syllable_sum(gsub("[\\\\\\/|.]","x", speech))]
  sentences = sentences[!is.na(syllables)]
  # Add cumulative syllable count and percent complete as proxy for progression
  sentences[, syllables.cumsum := cumsum(syllables)]
  sentences[, pct.complete := syllables.cumsum / sum(sentences$syllables)]
  sentences[, pct.complete.100 := pct.complete * 100]
  
  pol.df <- polarity(sentences$speech)$all
  sentences[, words := pol.df$wc]
  sentences[, pol := pol.df$polarity]
  sentences[pol > 0, dir := 1]
  sentences[pol == 0, dir := 0]
  sentences[pol < 0, dir := -1]
  
  my.theme <- 
    theme(plot.title = element_blank(),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.border = element_rect(fill = NA, color = 'white', size = 2),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 10, color="black", face="bold"),
          axis.title.x = element_text(size = 12, face="bold", color = "black"),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_line(colour = "black", size=1),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.text = element_text(size = 10, color= "black"),
          legend.title = element_text(size = 12,face="bold"),
          legend.position = "none")
  
  CustomScatterPlot <- function(gg)
    return(gg + geom_point(aes(color=dir)) + # Lighten dots
             stat_smooth(method = 'loess', color="indianred3", fill="lightgray", size=1.4) + 
             xlab("Percent complete (by syllable count)") + 
             ylab("Sentiment (sentence-level polarity)") +
             ggtitle("Sentiment Across Speech") +
             scale_x_continuous(labels = percent) + my.theme)
  
  CustomScatterPlot(ggplot(sentences, aes(pct.complete, pol)))
}

spsentcloud <- function(sentimentwords,maxwords = 250, col = c("darkolivegreen4","indianred3")){
  docs <- VCorpus(VectorSource(sentimentwords)) %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers) %>%
    tm_map(tolower)  %>%
    tm_map(removeWords, stopwords("english")) %>%
    tm_map(stripWhitespace) %>%
    tm_map(PlainTextDocument) %>%
    tm_map(removeWords, c("the","an","vice"))
  
  tdm <- TermDocumentMatrix(docs) %>%
    as.matrix()
  colnames(tdm) <- c("Positive","Negative")
  
  par(mar = rep(0, 4))
  comparison.cloud(tdm, random.order=FALSE, scale = c(6.5,0.6),
                   colors = col, title.bg.colors=c("darkolivegreen4","indianred3"),
                   title.size=2.5, max.words=maxwords)
  
}

topic_plot <- function(df, doc){
  df %>%
    filter(id == doc) %>%
    tidyr::gather(key="topic",value="gamma",2:10) %>%
    left_join(topic_crosswalk) %>%
    mutate(topic_label = factor(topic_label, topic_crosswalk$topic_label, ordered = T)) %>%
    ggplot(aes(topic_label, gamma)) +
    labs(x="",
         y="Topic Loading (Gamma)") +
    geom_col(show.legend = FALSE, fill = "lightsteelblue3", alpha=0.7) +
    theme(plot.title = element_blank(),
          plot.background = element_rect(fill = 'white', colour = 'white'),
          panel.border = element_rect(fill = NA, color = 'white', size = 2),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(colour = "grey79", size=.3, linetype = 3),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14, color="black", face="bold"),
          axis.text.x = element_text(angle=40, hjust=1),
          axis.title = element_text(size = 16, face="bold", color = "black"),
          axis.ticks = element_blank(),
          axis.line = element_line(colour = "black", size=1),
          legend.position = "none")
}

words_over_time <- function(df,words,stop=TRUE,colorvar,leg,scale=20,sz=14, forprint=FALSE) {
  
  x <- strcounts(df, words, only=stop)
  
  max_count <- max(x$count)
  
  p <- ggplot(x, aes(as.numeric(year), term, size=count)) +
    geom_point(stat="identity", aes_string(color = colorvar), alpha=.5) +
    scale_x_continuous(breaks = seq(1900,2030,by=4)) +
    scale_size(range=c(0,scale), guide="none") +
    facet_wrap(~period, scales="free_x", ncol = 1) +
    labs(title = "",
         y = "",
         x = "Year",
         subtitle= paste0("Circles scaled by number of uses of the word (Maximum Uses: ",max_count,")"),
         color = leg) +
    scale_color_manual(values=c("lightsteelblue3","olivedrab3","indianred3","darkgoldenrod2")) +
    theme_overtime(sz) +
    guides(color = guide_legend(override.aes = list(size=11)))
  
  if(colorvar == FALSE){
    p <- p + theme(legend.position = "none")
  }
  
  if(forprint==TRUE){
    p <- p + labs(title = "ASA Presidential Addresses, Words Through Time",
                  caption="Downloaded from: https://osmrbls.shinyapps.io/jsm_2020")
  }
  
  p
}

topics_over_time <- function(topic, topic_title, colorvar, legend_title, sz=14, forprint=FALSE) {
  
  x <- left_join(topics[ ,c("id",topic)], allspeeches[ ,c("id","year","period", "category","gender")], by="id")
  
  p <- ggplot(x, aes_string("year", topic)) +
    geom_bar(stat="identity", aes_string(fill = colorvar), alpha=.5) +
    scale_x_continuous(breaks = seq(1900,2030,by=4)) +
    facet_wrap(~period, scales="free_x", ncol = 1) +
    labs(title = paste0("Topic Loadings for ", topic_title),
         y = "Topic Loading (Gamma)",
         x = "Year",
         fill = legend_title) +
    scale_fill_manual(values=c("lightsteelblue3","olivedrab3","indianred3","darkgoldenrod2")) +
    theme_overtime(sz) +
    guides(fill = guide_legend(override.aes = list(size=11)))

  if(colorvar == FALSE){
    p <- p + theme(legend.position = "none")
  }
  
  if(forprint==TRUE){
    p <- p + labs(title = "ASA Presidential Addresses, Topics Through Time",
                  subtitle = paste0("Topic Loadings for ", topic_title),
                  caption="Downloaded from: https://osmrbls.shinyapps.io/jsm_2020")
  }
  
  p
}

theme_overtime <- function(sz){
  theme(plot.title = element_text(size=22, face="bold"),
        plot.subtitle = element_text(size=15),
        plot.caption = element_text(size=10),
        plot.background = element_rect(fill = 'white'),
        panel.border = element_rect(fill=NA, size=1, color="grey30"),
        panel.background = element_blank(),
        panel.grid.major = element_line(size=.3, color="grey50",linetype = 3),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = sz-2,face="bold", color="black"),
        axis.title = element_text(size = sz, face="bold", color = "black"),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        strip.text = element_text(size=sz, face="bold"),
        strip.background = element_blank(),
        legend.title = element_text(size=sz, face="bold"),
        legend.text = element_text(size=sz-2),
        legend.key = element_rect(fill = NA),
        legend.position = "bottom") 
}

speech_narrative <- function(df, pres_name){
  pres <- df[df$yearpresident == pres_name, 
             c("president_name_forward","gender","affiliation","pres_number",
               "title","date_of_address")]
  
  if(pres$gender == "Male"){
    pronoun <- "He"
    posessive_pronoun <- "his"
  }else{
    pronoun <- "She"
    posessive_pronoun <- "her"
  }
  
  if(pres$affiliation != "Unknown"){
    affil <- paste0(", from the ", pres$affiliation, ",")
  }else{
    affil <- ""
  }
  
  sent1 <- paste0(pres$president_name_forward, affil, " was the ", pres$pres_number, " president of the ASA. ")
  sent2 <- paste0("This page provides text analysis of ", posessive_pronoun, " address entitled <i>'", 
                  pres$title, "'</i> delivered on ", format(as.Date(pres$date_of_address), format="%A, %B %e, %Y"), ".")
  
  paragraph <- paste0("<h3>",pres$president_name_forward, "</h3>",
                      "<p style='font-size:1.2em'>", sent1, sent2, "<br><br>",
                      "You can read the full text of the speech by clicking the button below.</p>")
  return(paragraph)
}

plot_location <- function(df, states, countries, current_location=NA){
  
  if(!is.na(current_location)){
    df$current_loc <- df$location == current_location
  }else{
    df$current_loc <- NA
  }
  
  g <- ggplot(countries, aes(x = long, y = lat)) +
    geom_polygon(aes(group=group), fill="transparent", color = "lightsteelblue2") +
    geom_polygon(data = states, aes(x=long, y=lat, group=group), color="lightsteelblue2", 
                 fill="transparent", size=0.25) +
    theme_minimal() +
    theme(legend.position = "none",
          plot.title = element_text(size=22, face="bold"),
          plot.subtitle = element_text(size=15),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),) +
    coord_fixed(ratio=1.45, xlim = c(-125, -66.4), ylim = c(24.24, 50.75), expand = FALSE)
  
  if(!is.na(current_location)){
    g <- g + 
      geom_point(data=df[df$location != current_location, ], aes(lon, lat, size=num_hosting),
                 color="indianred3", alpha=0.8) +
      geom_point(data=df[df$location == current_location, ], aes(lon, lat, size=num_hosting),
                 color="olivedrab4", pch=15) +
      geom_label_repel(data = df[df$location==current_location, ], 
                       aes(lon, lat, label=location), color="black", point.padding =0.5)
  }else{
    g <- g + geom_point(data=df, aes(lon, lat, size=num_hosting, color="indianred4"), 
                        alpha=0.8)
  }
  
  return(g)
}
