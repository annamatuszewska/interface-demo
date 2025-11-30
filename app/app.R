# app.R

source("global.R")
source("modules/filter_panel.R")

ui <- dashboardPage(
  dashboardHeader(title = "The Essen Collection"),
  
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Text Analysis",      tabName = "sentiment", icon = icon("chart-bar")),
                conditionalPanel(
                  condition = "input.tabs == 'sentiment'",
                  prettySwitch(
                    inputId = "valence_switch",
                    label = "Switch to Sentiment polarity",
                    value = FALSE,
                    status = "primary",
                    fill = TRUE
                  )
                ),
                menuItem("Music Dashboard",    tabName = "dashboard", icon = icon("dashboard")),
                menuItem("Text & Rhythm S1",   tabName = "ctra_s1",  icon = icon("wave-square")),
                menuItem("Text & Rhythm S2",   tabName = "ctra_s2",  icon = icon("wave-square")),
                menuItem("Data Table – S1",    tabName = "fulltable",  icon = icon("table")),
                menuItem("Data Table – S2",    tabName = "fulltable2", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        
        .small-box {
          height: 70px !important;
          width: 120px !important;
          margin-right: 10px !important;
          text-align: center;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          border-radius: 8px !important;
        }
        .small-box .inner h3 { font-size: 20px !important; margin: 0; padding: 0; }
        .small-box .inner p { font-size: 13px !important; margin: 0; padding: 0; }
        .small-box .icon { display: none !important; }

        .sidebar-section-header {
          padding: 6px 10px;
          margin-top: 10px;
          margin-bottom: 5px;
          font-weight: bold;
          font-size: 15px;
          background-color: #c7e4f4; /* Default blue-tinted */
          color: black;
          border-radius: 4px;
        }
      
        
        .main-panel .col-sm-5,
        .main-panel .col-sm-7 {
          padding-left: 0px;
          padding-right: 0px;
        }
        .plot-spacing {
          margin-left: 0px;
          margin-right: 0px;
        }
        
        .box.box-green {
         border-top-color: #437D69 !important;
        }
        .box.box-green > .box-header {
          background-color: #437D69 !important;
          color: white !important;
        }
    
        .box.box-amber {
          border-top-color: #D17953 !important;
        }
        .box.box-amber > .box-header {
          background-color: #D17953 !important;
          color: white !important;
        }
        
        .box.box-danger { border-top-color: #F2B2A5 !important; }
        .box.box-danger > .box-header { background-color: #F2B2A5 !important; color: black !important; }
        
        .sidebar-section-header {
          padding: 6px 10px;
          margin-top: 10px;
          margin-bottom: 5px;
          font-weight: bold;
          font-size: 15px;
          background-color: #c7e4f4; /* Default blue-tinted */
          color: black;
          border-radius: 4px;
        }
        
        .sidebar-section-header-set2 {
          background-color: #f5c9c7; /* Light red-tinted */
          color: black;
        }
        
        
      ")),
      
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/basket.js/0.5.2/basket.full.min.js"),
      tags$script(src = "https://plugin.humdrum.org/scripts/humdrum-notation-plugin-worker.js"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('renderHumdrumScore', function(message) {
          setTimeout(function() {
            const options = {
              pageWidth: 2500,
              pageHeight: 3000,
              scale: 40
            };
            
            
          console.log('Rendering Humdrum with options:', options);
        
            displayHumdrum({
              source: message.source,
              url: message.url,
              autoResize: true
            });
          }, 200);
        });
      "))
      
    ),
    tabItems(
      
      # ───────────────────────────────────────────────────────────────────────
      # Text Analysis (from App 1)
      tabItem(tabName = "sentiment",
              tags$div(
                style = "margin: 5px 10px 10px 10px; font-size: 16px;",
                HTML("The dashboard serves as a tool for exploring corpora and their subcorpora; after performing text analysis, 
                     filtered datasets can be exported to dashboards dedicated to rhythmic and melodic analyses. 
                     To proceed to the latter, click the button: <strong>[Apply for further analysis]</strong>.")
              ),
              div(class = "box box-solid box-green",
                  div(class = "box-header with-border",
                      h3(class = "box-title", "SET 1")
                  ),
                  div(class = "box-body",
                      fluidRow(
                        # Filter + image + download (SET 1)
                        column(3,
                               div(
                                 style = "background-color: #f5f5f5; padding: 15px; border-radius: 6px;",
                                 selectInput("set1_corpus", "Select Corpus", choices = sort(unique(lyrics_clean$corpus)), selected = "K1", multiple = TRUE),
                                 selectInput("set1_subcorpus", "Select Subcorpus", choices = sort(unique(lyrics_clean$subcorpus)), selected = NULL, multiple = TRUE),
                                 uiOutput("set1_count_ui"),
                                 div(
                                   uiOutput("set1_image_gallery_ui"),
                                   style = "margin-top:20px; background:white; padding:10px; border-radius:10px;"
                                 ),
                                 div(
                                   selectInput("set1_sentiments", "Select emotions for further analysis", choices = NULL, multiple = TRUE, width = "100%"),
                                   tags$p(
                                     "Switch in navigation panel to the left",
                                     style = "margin-top: 5px; font-size: 14px; color: #555;"
                                   ),
                                   fluidRow(
                                     column(5, downloadButton("set1_download_docs", "Download CSV"))
                                   ),
                                   style = "background:white; padding:10px; border-radius:10px; margin-top:20px;"
                                 )
                               )
                        ),
                        
                        # Charts (SET 1)
                        column(9,
                               
                               # EMOTION + SENTIMENT PLOTS (shown when switch is OFF)
                               conditionalPanel(
                                 condition = "input.valence_switch == false",
                                 fluidRow(
                                   column(4,
                                          uiOutput("set1_emotionPlot_ui"),
                                          class = "plot-spacing"
                                   ),
                                   column(8,
                                          uiOutput("set1_sentimentPlot_ui"),
                                          class = "plot-spacing"
                                   )
                                 )
                               ),
                               
                               # VALENCE PLOTS (shown when switch is ON)
                               conditionalPanel(
                                 condition = "input.valence_switch == true",
                                 fluidRow(
                                   column(4,
                                          uiOutput("set1_valencePlot_ui"),
                                          class = "plot-spacing"
                                   ),
                                   column(8,
                                          uiOutput("set1_valenceHeat_ui"),
                                          class = "plot-spacing"
                                   )
                                 )
                               )
                               
                        )
                      )
                  )
              ),
              
              div(class = "box box-solid box-amber",
                  div(class = "box-header with-border",
                      h3(class = "box-title", "SET 2")
                  ),
                  div(class = "box-body",
                      
                      fluidRow(
                        # Filter + image + download (SET 2)
                        column(3,
                               div(
                                 style = "background-color: #f5f5f5; padding: 15px; border-radius: 6px;",
                                 selectInput("set2_corpus", "Select Corpus", choices = sort(unique(lyrics_clean$corpus)), selected = "K1", multiple = TRUE),
                                 selectInput("set2_subcorpus", "Select Subcorpus", choices = sort(unique(lyrics_clean$subcorpus)), selected = NULL, multiple = TRUE),
                                 uiOutput("set2_count_ui"),
                                 div(
                                   uiOutput("set2_image_gallery_ui"),
                                   style = "margin-top:20px; background:white; padding:10px; border-radius:10px;"
                                 ),
                                 div(
                                   selectInput("set2_sentiments", "Select emotions for further analysis", choices = NULL, multiple = TRUE, width = "100%"),
                                   tags$p(
                                     "Switch in navigation panel to the left",
                                     style = "margin-top: 5px; font-size: 14px; color: #555;"
                                   ),
                                   fluidRow(
                                     column(5, downloadButton("set2_download_docs", "Download CSV"))
                                   ),
                                   style = "background:white; padding:10px; border-radius:10px; margin-top:20px;"
                                 )
                               )
                        ),
                        
                        # Charts (SET 2)
                        column(9,
                               
                               # EMOTION + SENTIMENT PLOTS (shown when switch is OFF)
                               conditionalPanel(
                                 condition = "input.valence_switch == false",
                                 fluidRow(
                                   column(4,
                                          uiOutput("set2_emotionPlot_ui"),
                                          class = "plot-spacing"
                                   ),
                                   column(8,
                                          uiOutput("set2_sentimentPlot_ui"),
                                          class = "plot-spacing"
                                   )
                                 )
                               ),
                               
                               # VALENCE PLOTS (shown when switch is ON)
                               conditionalPanel(
                                 condition = "input.valence_switch == true",
                                 fluidRow(
                                   column(4,
                                          uiOutput("set2_valencePlot_ui"),
                                          class = "plot-spacing"
                                   ),
                                   column(8,
                                          uiOutput("set2_valenceHeat_ui"),
                                          class = "plot-spacing"
                                   )
                                 )
                               )
                               
                        )
                      )
                  )
              )
      ),
      
      
      # ───────────────────────────────────────────────────────────────────────
      # Music Dashboard 
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width = 2,
                       tabBox(width = 12, 
                              tabPanel(
                                "Set 1 Filters",
                                # default blueish style from your module
                                filter_panel_ui(
                                  "dash1",
                                  data,
                                  prefix_style = "background-color: #E5F1FB; padding:10px;",
                                  header_class = "sidebar-section-header"
                                )
                              ),
                              tabPanel(
                                "Set 2 Filters",
                                # amber‐ish style for set 2
                                filter_panel_ui(
                                  "dash2",
                                  data,
                                  prefix_style = "background-color: #FCE6E3; padding:10px;",
                                  header_class = "sidebar-section-header sidebar-section-header-set2"
                                )
                              )
                       ),
                       downloadButton("download_excel", "Download Chart Data (.xlsx)")
                ),
                column(width = 10,
                       column(width = 5,
                              fluidRow(
                                valueBoxOutput("distinct_filenames", width = 3),
                                valueBoxOutput("max_pitch", width = 3),
                                valueBoxOutput("min_pitch", width = 3),
                                valueBoxOutput("ambitus", width = 3)
                              ),
                              fluidRow(
                                tabBox(width = 12,
                                       tabPanel("Meter", uiOutput("metersDistributionUI")),
                                       tabPanel("Basic Tone", uiOutput("tonalityDistributionUI"))
                                )
                              )
                       ),
                       column(width = 1),  # Spacer
                       column(width = 5,
                              fluidRow(
                                valueBoxOutput("distinct_filenames2", width = 3),
                                valueBoxOutput("max_pitch2", width = 3),
                                valueBoxOutput("min_pitch2", width = 3),
                                valueBoxOutput("ambitus2", width = 3)
                              ),
                              fluidRow(
                                tabBox(width = 12,
                                       tabPanel("Meter", uiOutput("metersDistributionUI2")),
                                       tabPanel("Basic Tone", uiOutput("tonalityDistributionUI2"))
                                )
                              )
                       ),
                       
                       fluidRow(
                         box(width = 11,
                             selectInput("chartType", "Chart Type", 
                                         choices = c("Interval Distribution",
                                                     "Rhythm analysis", 
                                                     "Degree Transitions (Sankey)")
                             ),
                             uiOutput("selectedChart")
                         )
                       )
                )
              )
      ),
      
      
      # ───────────────────────────────────────────────────────────────────────
      # Text & Rhythm Analysis – Set 1 (from App 1)
      tabItem(tabName = "ctra_s1",
              fluidRow(
                column(width = 2,
                       filter_panel_ui("ctra1", data, prefix_style = "background-color: #E5F1FB; padding:10px;")
                ),
                column(
                  width = 10,
                  uiOutput("ctra_multiple_scores")
                )
              )
      ),
      
      # ───────────────────────────────────────────────────────────────────────
      # Text & Rhythm Analysis – Set 2 (placeholder—for later enhancement)
      tabItem(tabName = "ctra_s2",
              fluidRow(
                column(width = 2,
                       filter_panel_ui("ctra2", data,
                                       prefix_style = "background-color: #FCE6E3; padding:10px;",
                                       header_class = "sidebar-section-header sidebar-section-header-set2")
                ),
                column(
                  width = 10,
                  uiOutput("ctra2_multiple_scores")
                )
              )
      ),
     
      # ───────────────────────────────────────────────────────────────────────
      tabItem(tabName = "fulltable",
              DTOutput("dataTable")
      ),
      tabItem(tabName = "fulltable2",
              DTOutput("dataTable2")
      )
      
    ) # /tabItems
  )   # /dashboardBody
)     # /dashboardPage


server <- function(input, output, session) {
  
  # At top of server:
  dash1_filters <- filter_panel_server("dash1", data, selected_docs1)
  dash2_filters <- filter_panel_server("dash2", data, selected_docs2)
  ctra1_filters <- filter_panel_server("ctra1", data, selected_docs1)
  ctra2_filters <- filter_panel_server("ctra2", data, selected_docs2)
  
  
  # ─── Update Set1 “sentiment” choices ────────────────────────────────────────
  observe({
    sents <- available_sentiments1()
    # Dashboard tab Set1
    updateSelectizeInput(
      session, "dash1-sentiment",
      choices  = sents,
      selected = isolate(input$`dash1-sentiment`[input$`dash1-sentiment` %in% sents])
    )
    # CTRA S1
    updateSelectizeInput(
      session, "ctra1-sentiment",
      choices  = sents,
      selected = isolate(input$`ctra1-sentiment`[input$`ctra1-sentiment` %in% sents])
    )
  })
  
  # ─── Update Set2 “sentiment” choices ────────────────────────────────────────
  observe({
    sents2 <- available_sentiments2()
    # Dashboard tab Set2
    updateSelectizeInput(
      session, "dash2-sentiment",
      choices  = sents2,
      selected = isolate(input$`dash2-sentiment`[input$`dash2-sentiment` %in% sents2])
    )
    # CTRA S2
    updateSelectizeInput(
      session, "ctra2-sentiment",
      choices  = sents2,
      selected = isolate(input$`ctra2-sentiment`[input$`ctra2-sentiment` %in% sents2])
    )
  })
  
  
  # ── Text Analysis servers (BOOKMARK 1) ──────────────────────────────────────────
  
  sentiment_colors <- c(
    "anger"        = "#D73027",
    "fear"         = "#4575B4",
    "disgust"      = "#1A9850",
    "sadness"      = "#91BFDB",
    "surprise"     = "#FDAE61",
    "anticipation" = "#FEE08B",
    "trust"        = "#66BD63",
    "joy"          = "#F46D43"
  )
  
  # Reactive list of image paths based on selected corpora
  gallery_images <- reactive({
    corpora <- selected_corpus()
    paths <- paste0("img/", corpora, ".png")
    full_paths <- file.path("www", paths)
    paths[file.exists(full_paths)]
  })
  
  
  note_unicode_map <- list(
    "crotchet" = "𝅘𝅥",
    "dotted crotchet" = "𝅘𝅥.",
    "minim" = "𝅗𝅥",
    "dotted minim" = "𝅗𝅥.",
    "semibreve" = "𝅝",
    "quaver" = "𝅘𝅥𝅮",
    "dotted quaver" = "𝅘𝅥𝅮.",
    "semiquaver" = "𝅘𝅥𝅯",
    "longa" = "𝆷"
  )
  
  emotion_filter_state <- reactiveValues(
    set1 = NULL,
    set2 = NULL
  )
  
  ###   ###   ###   ###   ###   ###   ### 
  ### MAKE DASHBOARD - FIRST DASHBAORD ###
  ###   ###   ###   ###   ###   ###   ### 
  
  make_dashboard_set <- function(id_prefix) {
    
    # Dynamic selectors
    selected_corpus <- reactive({
      sel <- input[[paste0(id_prefix, "_corpus")]]
      if (is.null(sel) || length(sel) == 0) unique(lyrics_clean$corpus) else sel
    })
    
    selected_subcorpus <- reactive({
      sel <- input[[paste0(id_prefix, "_subcorpus")]]
      if (is.null(sel) || length(sel) == 0) unique(lyrics_clean$subcorpus) else sel
    })
    
    observeEvent(selected_corpus(), {
      valid_subs <- lyrics_clean %>%
        filter(corpus %in% selected_corpus()) %>%
        pull(subcorpus) %>%
        unique() %>%
        sort()
      
      prev   <- input[[paste0(id_prefix, "_subcorpus")]]
      keep   <- intersect(prev, valid_subs)
      
      updateSelectInput(
        session,
        paste0(id_prefix, "_subcorpus"),
        choices  = valid_subs,
        selected = if (length(keep) > 0) keep else NULL
      )
    })
    
    
    output[[paste0(id_prefix, "_count_ui")]] <- renderUI({
      # 1) the base set of docs for corpus+subcorpus
      base_docs <- lyrics %>%
        filter(
          corpus    %in% selected_corpus(),
          subcorpus %in% selected_subcorpus()
        ) %>%
        distinct(doc_id) %>%
        pull(doc_id)
      
      # 2) pull the *current* sentiment selection
      sel_emotions <- input[[paste0(id_prefix, "_sentiments")]]
      
      # 3) if any are chosen, find which docs have them (via the same top3 logic)
      if (!is.null(sel_emotions) && length(sel_emotions) > 0) {
        docs_by_emotion <- top3_sentiments_per_doc() %>%
          filter(sentiment %in% sel_emotions) %>%
          distinct(doc_id) %>%
          pull(doc_id)
        docs <- intersect(base_docs, docs_by_emotion)
      } else {
        docs <- base_docs
      }
      
      # 4) render the count immediately
      HTML(paste0(
        "<strong>Number of selected pieces: </strong>",
        length(docs)
      ))
    })
    
    
    # Image Gallery
    gallery_images <- reactive({
      corpora <- selected_corpus()
      paths <- paste0("img/", corpora, ".png")
      full_paths <- file.path("www", paths)
      paths[file.exists(full_paths)]
    })
    
    output[[paste0(id_prefix, "_image_gallery_ui")]] <- renderUI({
      imgs <- gallery_images()
      if (length(imgs) == 0) return(NULL)
      
      tagList(
        if (length(imgs) > 1) {
          sliderInput(
            inputId = paste0(id_prefix, "_image_index"),
            label   = NULL,
            min     = 1,
            max     = length(imgs),
            value   = 1,
            step    = 1,
            width   = "100%",
            ticks   = FALSE,
            animate = animationOptions(interval = 3000, loop = TRUE)
          )
        },
        uiOutput(paste0(id_prefix, "_display_image_ui"))
      )
    })
    
    
    output[[paste0(id_prefix, "_display_image_ui")]] <- renderUI({
      imgs <- gallery_images()
      idx  <- input[[paste0(id_prefix, "_image_index")]] %||% 1
      if (length(imgs) == 0) return(NULL)
      
      # figure out which corpus we're on
      corpus_name <- tools::file_path_sans_ext(basename(imgs[[idx]]))
      
      # look up its metadata, or fall back to empty defaults
      info <- corpus_info[[corpus_name]] %||%
        list(description = "", url = NULL)
      
      # build the description panel
      description_ui <- div(
        style = "
      max-height: 250px;
      overflow-y: auto;
      padding-right: 8px;
      margin-top: 10px;
      margin-bottom: 5px;
      line-height: 1.4;
    ",
        HTML(info$description)
      )
      
      # build the "View in Digital Library" link if we have a URL
      source_link_ui <- if (!is.null(info$url)) {
        tags$p(
          tags$a(
            "View in Digital Library",
            href   = info$url,
            target = "_blank"
          ),
          style = "margin-top: 10px; text-align: center;"
        )
      } else {
        NULL
      }
      
      # arrange image + metadata side by side
      fluidRow(
        column(
          width = 6,
          tags$img(
            src   = imgs[[idx]],
            id    = paste0(id_prefix, "_corpus_image"),
            style = "max-width:100%; height:auto; border-radius: 5px;"
          ),
          source_link_ui
        ),
        column(
          width = 6,
          div(
            style = "padding-left: 10px;",
            description_ui
          )
        )
      )
    })
    
    
    output[[paste0(id_prefix, "_emotionPlot")]] <- renderPlotly({

      
      filtered <- lyrics_clean %>%
        filter(
          corpus    %in% selected_corpus(),
          subcorpus %in% selected_subcorpus()
        )
      
      lyrics_emot <- filtered %>%
        left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
        filter(!is.na(sentiment)) %>%
        mutate(sentiment = factor(sentiment))
      
      
      # get overall word count (across all corpora)
      total_words <- nrow(filtered)
      
      lyrics_sum <- lyrics_emot %>%
        filter(!sentiment %in% c("positive", "negative")) %>%
        group_by(sentiment) %>%
        summarise(
          sentiment_freq = n(),
          .groups = "drop"
        ) %>%
        mutate(
          percentage    = round(sentiment_freq / total_words * 100, 1),
          neg_percentage = -percentage,
          tooltip       = paste0(
            "Sentiment: ", sentiment,
            "<br>Percentage: ", percentage, "%",
            "<br>Total words: ", total_words
          )
        )
      
      sentiment_levels <- c("anger", "fear", "disgust", "sadness",
                            "surprise", "anticipation", "trust", "joy")
      lyrics_sum$sentiment <- factor(lyrics_sum$sentiment, levels = sentiment_levels)
      
      p <- ggplot(lyrics_sum, aes(
        x = sentiment,
        y = neg_percentage,
        fill = sentiment,
        text = tooltip
      )) +
        geom_col() +
        geom_text(aes(label = paste0(percentage, "%")), 
                  hjust = 1.5, size = 3, color = "black") +
        scale_fill_brewer(palette = "Spectral") +
        coord_flip() +
        theme_minimal() +
        theme(
          axis.text.y     = element_text(face = "bold"),
          axis.text.x     = element_blank(),
          axis.ticks      = element_blank(),
          axis.title      = element_blank(),
          panel.grid      = element_blank(),
          legend.position = "none"
        )
        ggplotly(p, tooltip = "text") %>%
          layout(margin = list(t = 10, b = 10, l = 10, r = 10))
      })
      
    
    output[[paste0(id_prefix, "_emotionPlot_ui")]] <- renderUI({
      tagList(
        h4("Emotion recognition", style = "text-align:center; margin-bottom:10px;"),
        plotlyOutput(paste0(id_prefix, "_emotionPlot"), height = "480px")
        
      )
    })
    
    
    output[[paste0(id_prefix, "_sentimentPlot")]] <- renderPlotly({
      # 1. define the core sentiments
      sentiments <- c("anger","fear","disgust","sadness",
                      "surprise","anticipation","trust","joy")
      
      # 2. filter & join
      heat_data <- lyrics_clean %>%
        filter(
          corpus    %in% selected_corpus(),
          subcorpus %in% selected_subcorpus()
        ) %>%
        left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
        filter(sentiment %in% sentiments)
      
      # 3. find the top 3 words per sentiment across *all* corpora
      top_words <- heat_data %>%
        group_by(sentiment, word) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(sentiment) %>%
        slice_max(order_by = n, n = 3, with_ties = FALSE) %>%
        ungroup()
      
      # 4. compute each word’s share *within* its sentiment
      plot_data <- top_words %>%
        group_by(sentiment) %>%
        mutate(
          score     = n / sum(n),
          tooltip   = paste0(
            "Sentiment: ", sentiment,
            "<br>Word: ", word,
            "<br>Count: ", n,
            "<br>Share: ", scales::percent(score, accuracy = 0.1)
          ),
          sentiment = factor(sentiment, levels = sentiments)
        ) %>%
        ungroup()
      
      # 5. pick a fill palette
      pal_sent <- RColorBrewer::brewer.pal(8, "Spectral")
      names(pal_sent) <- sentiments
      
      # 6. assign a gradient color by score
      plot_data <- plot_data %>%
        rowwise() %>%
        mutate(fill_color = scales::gradient_n_pal(c("white", pal_sent[sentiment]))(score)) %>%
        ungroup()
      
      # 7. single heatmap
      p <- ggplot(plot_data, aes(
        x    = word,
        y    = sentiment,
        fill = fill_color,
        text = tooltip
      )) +
        geom_tile(color = "#e0e0e0", linewidth = 0.5) +
        geom_text(aes(label = word), size = 4) +
        scale_fill_identity() +
        labs(x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          axis.text.x   = element_blank(),
          axis.ticks    = element_blank(),
          axis.title.x      = element_blank(),
          panel.grid    = element_blank(),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(t = 20, b = 20, l = 80, r = 20))
    })
    
    
    output[[paste0(id_prefix, "_sentimentPlot_ui")]] <- renderUI({
      tagList(
        h4("Top 3 words", style = "text-align:center; margin-bottom:10px;"),
        plotlyOutput(paste0(id_prefix, "_sentimentPlot"), height = "480px")
      )
    })
    
    # Top 3 sentiment docs
    top3_sentiments_per_doc <- reactive({
      core_sentiments <- c("anger", "fear", "disgust", "sadness",
                           "surprise", "anticipation", "trust", "joy")
      
      lyrics_clean %>%
        filter(
          corpus    %in% selected_corpus(),
          subcorpus %in% selected_subcorpus()
        ) %>%
        inner_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
        filter(sentiment %in% core_sentiments) %>%
        group_by(doc_id, sentiment) %>%
        summarise(n = n(), .groups = "drop") %>%
        group_by(doc_id) %>%
        slice_max(order_by = n, n = 3, with_ties = FALSE) %>%
        ungroup()
    })
    
    observe({
      sentiment_levels <- c("anger", "fear", "disgust", "sadness",
                            "surprise", "anticipation", "trust", "joy")
      
      sentiments <- top3_sentiments_per_doc() %>%
        distinct(sentiment) %>%
        filter(sentiment %in% sentiment_levels) %>%
        pull(sentiment)
      
      updateSelectInput(session, paste0(id_prefix, "_sentiments"),
                        choices = sort(unique(sentiments)))
    })
    
    
    # SEND the selection based on the emotions to other dashboards (filter filenames)
    filtered_docs_by_sentiment <- reactive({
      # 1) Grab the 3 top sentiments per doc within the current corpus+subcorpus
      top3 <- top3_sentiments_per_doc()  
      
      # 2) See what the user has *right now* selected
      sel_sents <- input[[paste0(id_prefix, "_sentiments")]]
      
      # 3) If they picked some emotions, keep only those rows; 
      #    otherwise keep all top3 rows (i.e. no filtering).
      if (!is.null(sel_sents) && length(sel_sents) > 0) {
        df <- top3 %>% filter(sentiment %in% sel_sents)
      } else {
        df <- top3
      }
      
      # 4) Now summarise per doc_id
      df %>%
        group_by(doc_id) %>%
        # sort by n descending so the collapse is in rank order
        arrange(doc_id, desc(n)) %>%  
        summarise(
          top_sentiments = paste(sentiment, collapse = ", "),
          .groups        = "drop"
        )
    })
    
    
    output[[paste0(id_prefix, "_download_docs")]] <- downloadHandler(
      filename = function() {
        paste0(id_prefix, "_selected_files_", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(filtered_docs_by_sentiment(), file, row.names = FALSE)
      }
    )
    
    # ─── Simplified overall Positive/Negative bar chart ─────────────────────────
    
    output[[paste0(id_prefix, "_valencePlot")]] <- renderPlotly({
      # 1) filter & join
      df <- lyrics_clean %>%
        filter(
          corpus    %in% selected_corpus(),
          subcorpus %in% selected_subcorpus()
        ) %>%
        left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
        filter(sentiment %in% c("positive", "negative")) %>%
        mutate(valence = sentiment)
      
      # 2) total words for base rate
      total_words <- nrow(df)
      
      # 3) summarise across all corpora
      val_summary <- df %>%
        group_by(valence) %>%
        summarise(count = n(), .groups = "drop") %>%
        mutate(
          percentage = round(count / total_words * 100, 1),
          neg_pct    = -percentage,
          tooltip    = paste0(
            "Valence: ", valence,
            "<br>Count: ", count,
            "<br>Share: ", percentage, "%"
          )
        )
      
      # 4) bar chart
      p <- ggplot(val_summary, aes(x = valence, y = neg_pct, fill = valence, text = tooltip)) +
        geom_col() +
        geom_text(aes(label = paste0(percentage, "%")), 
                  hjust = 1.1, size = 4, color = "black") +
        scale_fill_manual(values = c(positive = "#6BAED6", negative = "#990F02")) +
        coord_flip() +
        theme_minimal() +
        theme(
          axis.text     = element_blank(),
          axis.title    = element_blank(),
          panel.grid    = element_blank(),
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = "text") %>%
        layout(margin = list(t = 20, b = 20, l = 80, r = 20))
    })
    
    # ─── Combined heatmap of top‐7 words per valence ────────────────────────────
    
    output[[paste0(id_prefix, "_valenceHeat")]] <- renderPlotly({
      # 1) filter & join
      df <- lyrics_clean %>%
        filter(
          corpus    %in% selected_corpus(),
          subcorpus %in% selected_subcorpus()
        ) %>%
        left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
        filter(sentiment %in% c("positive", "negative")) %>%
        rename(valence = sentiment)
      
      # 2) pick top 7 words per valence (overall)
      top_words <- df %>%
        group_by(valence, word) %>%
        summarise(count = n(), .groups = "drop") %>%
        group_by(valence) %>%
        slice_max(order_by = count, n = 7, with_ties = FALSE) %>%
        ungroup()
      
      # 3) compute tooltip and relative scaling
      plot_data <- top_words %>%
        group_by(valence) %>%
        mutate(
          scaled_n = count / max(count),
          tooltip  = paste0(
            "Valence: ", valence,
            "<br>Word: ", word,
            "<br>Count: ", count
          )
        ) %>%
        ungroup()
      
      # 4) assign fill colors
      plot_data <- plot_data %>%
        mutate(
          fill_color = case_when(
            valence == "positive" ~ scales::gradient_n_pal(c("white", "#6BAED6"))(scaled_n),
            valence == "negative" ~ scales::gradient_n_pal(c("white", "#990F02"))(scaled_n)
          )
        )
      
      # 5) single heatmap
      p2 <- ggplot(plot_data, aes(
        x    = word,
        y    = valence,
        fill = fill_color,
        text = tooltip
      )) +
        geom_tile(color = "#e0e0e0", linewidth = 0.5) +
        geom_text(aes(label = word), size = 4) +
        scale_fill_identity() +
        labs(x = "Top 7 words", y = NULL) +
        theme_minimal() +
        theme(
          axis.text     = element_blank(),
          axis.ticks      = element_blank(),
          panel.grid      = element_blank(),
          legend.position = "none"
        )
      
      ggplotly(p2, tooltip = "text") %>%
        layout(margin = list(t = 20, b = 20, l = 80, r = 20))
    })
    
    
    output[[paste0(id_prefix, "_valencePlot_ui")]] <- renderUI({
      tagList(
        h4("Sentiment polarity", style = "text-align:center; margin-bottom:10px;"),
        plotlyOutput(paste0(id_prefix, "_valencePlot"), height = "300px")
      )
    })
    
    output[[paste0(id_prefix, "_valenceHeat_ui")]] <- renderUI({
      tagList(
        h4("Top 7 words", style = "text-align:center; margin-bottom:10px;"),
        plotlyOutput(paste0(id_prefix, "_valenceHeat"), height = "300px")
      )
    })
    
    emotion_prompt1 <- reactiveVal("")
    emotion_prompt2 <- reactiveVal("")
    
    observeEvent(input$set1_apply_emotions, {
      if (!is.null(input$set1_sentiments) && length(input$set1_sentiments) > 0) {
        selected_docs <- top3_sentiments_per_doc() %>%
          filter(sentiment %in% input$set1_sentiments) %>%
          distinct(doc_id) %>%
          pull(doc_id)
        
        emotion_filter_state$set1 <- selected_docs
        emotion_prompt1("✓ Filter applied to Set 1: dashboard will reflect selected emotions.")
      } else {
        emotion_filter_state$set1 <- NULL
        emotion_prompt1("✓ No emotion selected — full dataset will be used.")
      }
    })
    
    output$set1_filter_applied_msg <- renderText({
      emotion_prompt1()
    })
    
    
    observeEvent(input$set2_apply_emotions, {
      if (!is.null(input$set2_sentiments) && length(input$set2_sentiments) > 0) {
        selected_docs <- top3_sentiments_per_doc() %>%
          filter(sentiment %in% input$set2_sentiments) %>%
          distinct(doc_id) %>%
          pull(doc_id)
        
        emotion_filter_state$set2 <- selected_docs
        emotion_prompt2("✓ Filter applied to Set 2: dashboard will reflect selected emotions.")
      } else {
        emotion_filter_state$set2 <- NULL
        emotion_prompt2("✓ No emotion selected — full dataset will be used.")
      }
    })
    
    output$set2_filter_applied_msg <- renderText({
      emotion_prompt2()
    })
    
    
    
  }
  
  make_dashboard_set("set1")
  make_dashboard_set("set2")
  
  
  
  selected_docs1 <- reactive({
    req(input$set1_corpus)
    
    # fall back to *all* subcorpora if none explicitly chosen
    subs <- input$set1_subcorpus
    if (is.null(subs) || length(subs) == 0) {
      subs <- unique(lyrics_clean$subcorpus)
    }
    
    # now *use* `subs`, not `input$set1_subcorpus`
    base <- lyrics_clean %>%
      filter(
        corpus    %in% input$set1_corpus,
        subcorpus %in% subs
      )
    
    if (length(input$set1_sentiments) > 0) {
      base <- base %>%
        left_join(nrc_emotionen, by = "word") %>%
        filter(sentiment %in% input$set1_sentiments)
    }
    
    base %>%
      distinct(doc_id) %>%
      pull(doc_id)
  })
  
  
  selected_docs2 <- reactive({
    req(input$set2_corpus)
    
    subs <- input$set2_subcorpus
    if (is.null(subs) || length(subs) == 0) {
      subs <- unique(lyrics_clean$subcorpus)
    }
    
    base <- lyrics_clean %>%
      filter(
        corpus    %in% input$set2_corpus,
        subcorpus %in% subs
      )
    
    if (length(input$set2_sentiments) > 0) {
      base <- base %>%
        left_join(nrc_emotionen, by = "word") %>%
        filter(sentiment %in% input$set2_sentiments)
    }
    
    base %>%
      distinct(doc_id) %>%
      pull(doc_id)
  })
  
  # the master list of sentiments to show, based on SET1 text‐analysis filters
  available_sentiments1 <- reactive({
    req(input$set1_corpus, input$set1_subcorpus)
    lyrics_clean %>%
      filter(
        corpus    %in% input$set1_corpus,
        subcorpus %in% input$set1_subcorpus
      ) %>%
      left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
      # only keep the eight core sentiments
      filter(sentiment %in% c("anger","fear","disgust","sadness",
                              "surprise","anticipation","trust","joy")) %>%
      distinct(sentiment) %>%
      # preserve your desired order
      mutate(sentiment = factor(sentiment, levels = c("anger","fear","disgust",
                                                      "sadness","surprise",
                                                      "anticipation","trust","joy"))) %>%
      arrange(sentiment) %>%
      pull(sentiment)
  })
  
  available_sentiments2 <- reactive({
    req(input$set2_corpus, input$set2_subcorpus)
    lyrics_clean %>%
      filter(
        corpus    %in% input$set2_corpus,
        subcorpus %in% input$set2_subcorpus
      ) %>%
      left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
      filter(sentiment %in% c("anger","fear","disgust","sadness",
                              "surprise","anticipation","trust","joy")) %>%
      distinct(sentiment) %>%
      mutate(sentiment = factor(sentiment, levels = c("anger","fear","disgust",
                                                      "sadness","surprise",
                                                      "anticipation","trust","joy"))) %>%
      arrange(sentiment) %>%
      pull(sentiment)
  })
  
  
  
  
  
  # ─── Sync Text Analysis (master) → Dashboard & CTRA Module filters ─────────────────
  
  # 1) Corpus
  observeEvent(input$set1_corpus, {
    # Music Dashboard Set 1 filters
    updateSelectizeInput(
      session,
      "dash1-corpus",
      selected = input$set1_corpus
    )
    # Text & Rhythm S1 filters
    updateSelectizeInput(
      session,
      "ctra1-corpus",
      selected = input$set1_corpus
    )
  }, ignoreNULL = FALSE)
  
  # 2) Subcorpus
  observeEvent(input$set1_subcorpus, {
    updateSelectizeInput(
      session,
      "dash1-subcorpus",
      selected = input$set1_subcorpus
    )
    updateSelectizeInput(
      session,
      "ctra1-subcorpus",
      selected = input$set1_subcorpus
    )
  }, ignoreNULL = FALSE)
  
  
  # And the same for emotions
  observeEvent(input$set1_sentiments, {
    updateSelectizeInput(
      session, "mus_sentiment1",
      selected = input$set1_sentiments
    )
  }, ignoreNULL = FALSE)
  
  observeEvent(input$set2_corpus, {
    updateSelectizeInput(session, "mus_corpus2",
                         selected = input$set2_corpus)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$set2_subcorpus, {
    updateSelectizeInput(session, "mus_subcorpus2",
                         selected = input$set2_subcorpus)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$set2_sentiments, {
    updateSelectizeInput(session, "mus_sentiment2",
                         selected = input$set2_sentiments)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$set1_incipit, {
    sel <- input$set1_incipit
    updateSelectizeInput(session, "dash1-incipit", selected = sel)
    updateSelectizeInput(session, "ctra1-incipit", selected = sel)
  }, ignoreNULL = FALSE)
  
  observeEvent(input$set2_incipit, {
    sel <- input$set2_incipit
    updateSelectizeInput(session, "dash2-incipit", selected = sel)
    updateSelectizeInput(session, "ctra2-incipit", selected = sel)
  }, ignoreNULL = FALSE)
  
  
  # whenever the user navigates tabs, re-sync the Set1 corpus/subcorpus into both modules
  observeEvent(input$tabs, {
    # only do it when we hit the dashboard or CTRA pages
    if (input$tabs %in% c("dashboard", "ctra_s1", "ctra_s2")) {
      updateSelectizeInput(
        session, "dash1-corpus",
        selected = input$set1_corpus
      )
      updateSelectizeInput(
        session, "dash1-subcorpus",
        selected = input$set1_subcorpus
      )
      updateSelectizeInput(
        session, "ctra1-corpus",
        selected = input$set1_corpus
      )
      updateSelectizeInput(
        session, "ctra1-subcorpus",
        selected = input$set1_subcorpus
      )
      updateSelectizeInput(
        session, "dash1-sentiment",
        selected = input$set1_sentiments
      )
      updateSelectizeInput(
        session, "ctra1-sentiment",
        selected = input$set1_sentiments
      )
      updateSelectizeInput(
        session, "dash1-incipit",
        selected = input$set1_incipit
      )
      updateSelectizeInput(
        session, "ctra1-incipit",
        selected = input$set1_incipit
      )
    }
  }, ignoreInit = TRUE)
  
  # ─── Sync Text‐Analysis “sentiments” → Dashboard & CTRA filters ─────────────────
  
  observeEvent(input$set1_sentiments, {
    vals <- input$set1_sentiments
    
    # Music Dashboard Set 1
    updateSelectizeInput(
      session, "dash1-sentiment",
      selected = vals
    )
    
    # Text & Rhythm S1
    updateSelectizeInput(
      session, "ctra1-sentiment",
      selected = vals
    )
  }, ignoreNULL = FALSE)
  
  # If you also want it on Set 2:
  observeEvent(input$set2_sentiments, {
    vals <- input$set2_sentiments
    
    updateSelectizeInput(
      session, "dash2-sentiment",
      selected = vals
    )
    updateSelectizeInput(
      session, "ctra2-sentiment",
      selected = vals
    )
  }, ignoreNULL = FALSE)
  
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  # ── Music Dashboard servers (BOOKMARK 2) ───────────────────────────────────────
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  top3_sentiments_per_doc1 <- reactive({
    core_sentiments <- c("anger","fear","disgust","sadness",
                         "surprise","anticipation","trust","joy")
    
    lyrics_clean %>%
      filter(
        corpus    %in% input$set1_corpus,
        subcorpus %in% input$set1_subcorpus
      ) %>%
      inner_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
      filter(sentiment %in% core_sentiments) %>%
      group_by(doc_id, sentiment) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(doc_id) %>%
      slice_max(order_by = n, n = 3, with_ties = FALSE) %>%
      ungroup()
  })
  
  top3_sentiments_per_doc2 <- reactive({
    core_sentiments <- c("anger","fear","disgust","sadness",
                         "surprise","anticipation","trust","joy")
    
    lyrics_clean %>%
      filter(
        corpus    %in% input$set2_corpus,
        subcorpus %in% input$set2_subcorpus
      ) %>%
      inner_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
      filter(sentiment %in% core_sentiments) %>%
      group_by(doc_id, sentiment) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(doc_id) %>%
      slice_max(order_by = n, n = 3, with_ties = FALSE) %>%
      ungroup()
  })
  
  library(dplyr)
  library(tidyr)
  
  valence_per_doc1 <- reactive({
    # 1) don’t even try until the user has picked a corpus+subcorpus
    req(input$set1_corpus, input$set1_subcorpus)
    
    # 2) subset to just their lyrics
    df <- lyrics_clean %>%
      filter(
        corpus    %in% input$set1_corpus,
        subcorpus %in% input$set1_subcorpus
      )
    
    # 3) join NRC, keep only pos/neg, and count per doc_id×sentiment
    counts <- df %>%
      left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      count(doc_id, sentiment, name = "n")
    
    # 4) if there are no rows at all, return an empty lookup
    if (nrow(counts) == 0) {
      return(tibble(doc_id = character(0), dominant = character(0)))
    }
    
    # 5) pivot wider, filling missing sentiment‐columns with 0
    wide <- counts %>%
      pivot_wider(
        names_from   = sentiment,
        values_from  = n,
        values_fill  = list(n = 0)
      ) %>%
      # 6) make absolutely sure both cols exist
      mutate(
        positive = coalesce(positive, 0),
        negative = coalesce(negative, 0)
      ) %>%
      # 7) now safe to compute dominant
      mutate(
        dominant = case_when(
          positive > negative ~ "positive",
          negative > positive ~ "negative",
          TRUE                ~ "equal"
        )
      ) %>%
      select(doc_id, dominant)
    
    wide
  })
  
  
  valence_per_doc2 <- reactive({
    # 1) don’t even try until the user has picked a corpus+subcorpus
    req(input$set2_corpus, input$set2_subcorpus)
    
    # 2) subset to just their lyrics
    df <- lyrics_clean %>%
      filter(
        corpus    %in% input$set2_corpus,
        subcorpus %in% input$set2_subcorpus
      )
    
    # 3) join NRC, keep only pos/neg, and count per doc_id×sentiment
    counts <- df %>%
      left_join(nrc_emotionen, by = "word", relationship = "many-to-many") %>%
      filter(sentiment %in% c("positive", "negative")) %>%
      count(doc_id, sentiment, name = "n")
    
    # 4) if there are no rows at all, return an empty lookup
    if (nrow(counts) == 0) {
      return(tibble(doc_id = character(0), dominant = character(0)))
    }
    
    # 5) pivot wider, filling missing sentiment‐columns with 0
    wide <- counts %>%
      pivot_wider(
        names_from   = sentiment,
        values_from  = n,
        values_fill  = list(n = 0)
      ) %>%
      # 6) make absolutely sure both cols exist
      mutate(
        positive = coalesce(positive, 0),
        negative = coalesce(negative, 0)
      ) %>%
      # 7) now safe to compute dominant
      mutate(
        dominant = case_when(
          positive > negative ~ "positive",
          negative > positive ~ "negative",
          TRUE                ~ "equal"
        )
      ) %>%
      select(doc_id, dominant)
    
    wide
  })
  
  selected_interval <- reactiveVal(NULL)
  
  get_pie_data <- function(set_label) {
    interval_data() %>%
      filter(set == set_label) %>%
      group_by(interval_generic) %>%
      summarise(count = sum(count), .groups = "drop") %>%
      mutate(percent = round(100 * count / sum(count), 1)) %>%
      filter(percent >= 1)
  }
  
  get_filtered_data1 <- function(filters) {
    df <- data[data$filename %in% selected_docs1(), ]
    
    col_map <- list(
      corpus        = "corpus",
      subcorpus     = "subcorpus",
      region        = "region",
      scale_degrees = "scl_value",
      tonality      = "key",
      meters        = "meters",
      rh_pattern    = "note_length_name",
      titles        = "OTL",
      incipit       = "incipit",
      lyrics_search = "filename",
      valence       = "valence"
    )
    
    for (fld in names(filters)) {
      val <- filters[[fld]]
      
      if (is.null(val) || length(val) == 0) next
      
      if (fld == "lyrics_search") {
        if (!nzchar(trimws(val))) next
        matched <- matched_filenames1()
        df <- df[df$filename %in% matched, ]
        
      } else if (fld == "sentiment") {
        # use the global reactive we just defined
        matched_docs <- top3_sentiments_per_doc1() %>%
          filter(sentiment %in% val) %>%
          distinct(doc_id) %>%
          pull(doc_id)
        df <- df[df$filename %in% matched_docs, ]
        
      } else if (fld == "incipit") {
        df <- df[df$incipit %in% val, ]
      } else if (fld == "valence")  {
        # fld_val is a character vector like c("All") or c("positive","equal")
        keep_vals <- setdiff(val, "All")
        # if they only have “All” (or nothing) selected, don’t filter
        if (length(keep_vals) == 0) next
        
        # otherwise limit to docs whose dominant is in keep_vals
        matched_docs <- valence_per_doc1() %>%   # or valence_per_doc2()
          filter(dominant %in% keep_vals) %>%
          pull(doc_id)
        df <- df[df$filename %in% matched_docs, ]
      } else {
        # generic column‐based filtering, but only if we have a mapping
        col <- col_map[[fld]]
        if (is.null(col) || !(col %in% names(df))) {
          next
        }
        df <- df[df[[col]] %in% val, ]
      }
    }
    
    df
  }
  
  
  # replace your old get_filtered_data2() with:
  
  get_filtered_data2 <- function(filters) {
    df <- data[data$filename %in% selected_docs2(), ]
    
    col_map <- list(
      corpus        = "corpus",
      subcorpus     = "subcorpus",
      region        = "region",
      scale_degrees = "scl_value",
      tonality      = "key",
      meters        = "meters",
      rh_pattern    = "note_length_name",
      titles        = "OTL",
      incipit       = "incipit",
      lyrics_search = "filename",
      valence       = "valence"
    )
    
    for (fld in names(filters)) {
      val <- filters[[fld]]
      if (is.null(val) || length(val) == 0) next
      
      if (fld == "lyrics_search") {
        if (!nzchar(trimws(val))) next
        matched <- matched_filenames2()
        df <- df[df$filename %in% matched, ]
        
      } else if (fld == "sentiment") {
        matched_docs <- top3_sentiments_per_doc2() %>%
          filter(sentiment %in% val) %>%
          distinct(doc_id) %>%
          pull(doc_id)
        df <- df[df$filename %in% matched_docs, ]
        
      } else if (fld == "incipit") {
        df <- df[df$incipit %in% val, ]
      } else if (fld == "valence") {
        keep_vals <- setdiff(val, "All")
        if (length(keep_vals) == 0) next
        matched_docs <- valence_per_doc2() %>%   
          filter(dominant %in% keep_vals) %>%
          pull(doc_id)
        df <- df[df$filename %in% matched_docs, ]
      } else {
        col <- col_map[[fld]]
        if (is.null(col) || !(col %in% names(df))) {
          next
        }
        df <- df[df[[col]] %in% val, ]
      }
    }
    
    df
  }
  
  
  
  note_filter_choices1 <- reactive({
    df <- rhythm_chart_data()
    note_names <- unique(df$note_length_name[df$set == "Set 1"])
    available <- note_length_order[note_length_order %in% note_names]
    setNames(available, sapply(available, get_note_label))
  })
  
  
  note_filter_choices2 <- reactive({
    df <- rhythm_chart_data()
    note_names <- unique(df$note_length_name[df$set == "Set 2"])
    available <- note_length_order[note_length_order %in% note_names]
    setNames(available, sapply(available, get_note_label))
  })
  
  filtered_data <- reactive({
    filters <- dash1_filters()        # get the user’s current Dashboard filters
    df      <- get_filtered_data1(filters)
    
    df$meters <- clean_meters(df$meters)
    validate(
      need(nrow(df) > 0, "No data available for the current filter selection.")
    )
    df
  })
  
  
  filtered_data2 <- reactive({
    df <- get_filtered_data2(dash2_filters())
    
    df$meters <- clean_meters(df$meters)
    
    validate(need(nrow(df) > 0, "No data available for the current filter selection."))
    
    df
  })
  

  source_degrees_set1 <- reactive({
    df <- filtered_data()
    if (!"degree" %in% names(df)) return(NULL)
    
    df <- df |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      filter(!is.na(source_degree)) |>
      pull(source_degree) |>
      unique() |>
      sort()
    
    df
  })
  
  observe({
    req(input$chartType == "Degree Transitions (Sankey)")  # ⬅️ ensures the input exists
    
    degrees <- source_degrees_set1()
    
    if (is.null(degrees) || length(degrees) == 0) return()
    
    updateSelectInput(
      session,
      "selected_source_degree1",
      choices = degrees,
      selected = degrees[1]
    )
  })
  
  
  sankey_data_set1 <- reactive({
    req(input$selected_source_degree1)
    
    df <- filtered_data() |>
      arrange(filename, id) |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      group_by(filename) |>
      mutate(target_degree = lead(source_degree)) |>
      ungroup() |>
      filter(
        !is.na(source_degree),
        !is.na(target_degree),
        source_degree == input$selected_source_degree1
      ) |>
      count(source_degree, target_degree, name = "value")
    
    df
  })
  
  source_degrees_set2 <- reactive({
    df <- filtered_data2()
    if (!"degree" %in% names(df)) return(NULL)
    
    df <- df |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      filter(!is.na(source_degree)) |>
      pull(source_degree) |>
      unique() |>
      sort()
    
    df
  })
  
  observe({
    req(input$chartType == "Degree Transitions (Sankey)")
    
    degrees <- source_degrees_set2()
    
    if (is.null(degrees) || length(degrees) == 0) return()
    
    updateSelectInput(
      session,
      "selected_source_degree2",
      choices = degrees,
      selected = degrees[1]
    )
  })
  
  sankey_data_set2 <- reactive({
    req(input$selected_source_degree2)
    
    df <- filtered_data2() |>
      arrange(filename, id) |>
      mutate(source_degree = gsub("^[\\^v]+", "", degree)) |>
      group_by(filename) |>
      mutate(target_degree = lead(source_degree)) |>
      ungroup() |>
      filter(
        !is.na(source_degree),
        !is.na(target_degree),
        source_degree == input$selected_source_degree2
      ) |>
      count(source_degree, target_degree, name = "value")
    
    df
  })
  
  
  metrics <- reactive({
    df_filtered <- filtered_data()
    if (nrow(df_filtered) == 0) {
      return(data.frame(
        distinct_filenames = 0,
        max_pitch = NA,
        min_pitch = NA,
        ambitus = NA
      ))
    }
    
    data_summary <- df_filtered %>%
      group_by(filename) %>%
      summarise(
        max_pitch = ifelse(n() > 0 && !all(is.na(pitch)), max(pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0 && !all(is.na(pitch)), min(pitch, na.rm = TRUE), NA),
        num_notes = n(),
        num_pitch_zero = sum(ifelse(is.na(pitch), 0, pitch) == 0, na.rm = TRUE),
        ambitus = ifelse(!is.na(max_pitch) && !is.na(min_pitch), max_pitch - min_pitch, NA)
      )
    
    # Wyliczenie średniego ambitus
    avg_ambitus <- mean(data_summary$ambitus, na.rm = TRUE)
    
    data_summary %>%
      summarise(
        distinct_filenames = n_distinct(filename),
        max_pitch = ifelse(n() > 0, max(max_pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0, min(min_pitch, na.rm = TRUE), NA),
        ambitus = avg_ambitus
      )
  })
  
  metrics2 <- reactive({
    df_filtered <- filtered_data2()
    if (nrow(df_filtered) == 0) {
      return(data.frame(
        distinct_filenames = 0,
        max_pitch = NA,
        min_pitch = NA,
        ambitus = NA
      ))
    }
    
    data_summary <- df_filtered %>%
      group_by(filename) %>%
      summarise(
        max_pitch = ifelse(n() > 0 && !all(is.na(pitch)), max(pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0 && !all(is.na(pitch)), min(pitch, na.rm = TRUE), NA),
        num_notes = n(),
        num_pitch_zero = sum(ifelse(is.na(pitch), 0, pitch) == 0, na.rm = TRUE),
        ambitus = ifelse(!is.na(max_pitch) && !is.na(min_pitch), max_pitch - min_pitch, NA)
      )
    
    # Wyliczenie średniego ambitus
    avg_ambitus <- mean(data_summary$ambitus, na.rm = TRUE)
    
    data_summary %>%
      summarise(
        distinct_filenames = n_distinct(filename),
        max_pitch = ifelse(n() > 0, max(max_pitch, na.rm = TRUE), NA),
        min_pitch = ifelse(n() > 0, min(min_pitch, na.rm = TRUE), NA),
        ambitus = avg_ambitus
      )
  })
  
  meters_distribution <- reactive({
    df_filtered <- filtered_data()
    
    df_filtered$meters_grouped <- sapply(df_filtered$meters, function(x) {
      if (is.na(x) || grepl(",", as.character(x))) {
        return("mixed")
      } else {
        return(as.character(x))
      }
    })
    
    df_meters <- df_filtered %>%
      group_by(meters_grouped) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small categories under 2% into "Other"
    df_meters <- df_meters %>%
      mutate(group = ifelse(percentage < 2, "Other", meters_grouped)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_meters)[1] <- "meters_grouped"  # rename back for UI consistency
    
    df_meters
  })
  
  
  meters_distribution2 <- reactive({
    df_filtered <- filtered_data2()
    
    df_filtered$meters_grouped <- sapply(df_filtered$meters, function(x) {
      if (is.na(x) || grepl(",", as.character(x))) {
        return("mixed")
      } else {
        return(as.character(x))
      }
    })
    
    df_meters <- df_filtered %>%
      group_by(meters_grouped) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small categories under 2% into "Other"
    df_meters <- df_meters %>%
      mutate(group = ifelse(percentage < 2, "Other", meters_grouped)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_meters)[1] <- "meters_grouped"  # rename back for UI consistency
    
    df_meters
  })
  
  
  tonality_distribution <- reactive({
    df_filtered <- filtered_data()
    
    df_ground <- df_filtered %>%
      filter(!is.na(key)) %>%
      group_by(key) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small ones under 2% into "Other"
    df_ground <- df_ground %>%
      mutate(group = ifelse(percentage < 2, "Other", key)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_ground)[1] <- "key"  # for UI
    df_ground
  })
  
  
  tonality_distribution2 <- reactive({
    df_filtered <- filtered_data2()
    
    df_ground <- df_filtered %>%
      filter(!is.na(key)) %>%
      group_by(key) %>%
      summarise(count = n(), .groups = "drop") %>%
      mutate(percentage = round(count / sum(count) * 100, 2))
    
    # Group small ones under 2% into "Other"
    df_ground <- df_ground %>%
      mutate(group = ifelse(percentage < 2, "Other", key)) %>%
      group_by(group) %>%
      summarise(percentage = sum(percentage), .groups = "drop") %>%
      arrange(desc(percentage))
    
    names(df_ground)[1] <- "key"  # for UI
    df_ground
  })
  
  
  interval_data <- reactive({
    df1 <- filtered_data()
    df2 <- filtered_data2()
    
    df1 <- df1[!is.na(df1$interval_name) & !is.na(df1$interval) & !is.na(df1$mint) & df1$interval <= 10, ]
    df2 <- df2[!is.na(df2$interval_name) & !is.na(df2$interval) & !is.na(df2$mint) & df2$interval <= 10, ]
    
    df1$direction <- ifelse(grepl("^\\-", df1$mint), "down", "up")
    df2$direction <- ifelse(grepl("^\\-", df2$mint), "down", "up")
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    combined <- bind_rows(df1, df2)
    
    # Extract generic interval name
    combined$interval_generic <- sapply(strsplit(as.character(combined$interval_name), " "), tail, 1)
    
    # Create mapping: interval_generic → smallest interval number
    order_map <- combined %>%
      group_by(interval_generic) %>%
      summarise(order_val = min(interval, na.rm = TRUE), .groups = "drop")
    
    grouped <- combined %>%
      group_by(interval_generic, direction, set) %>%
      summarise(count = n(), .groups = "drop") %>%
      filter(count > 0) %>%
      mutate(signed_count = ifelse(direction == "down", -count, count)) %>%
      group_by(set) %>%
      mutate(percent = round(count / sum(count) * 100, 2)) %>%
      ungroup()
    
    # Merge back the sorting order
    grouped <- left_join(grouped, order_map, by = "interval_generic")
    grouped <- grouped %>% arrange(order_val)
    
    # Set factor levels for proper X-axis ordering
    grouped$interval_generic <- factor(grouped$interval_generic, levels = unique(grouped$interval_generic))
    
    grouped
  })
  
  
  note_length_order <- c(
    "semiquaver", "quaver", "dotted quaver",  "crotchet", 
    "dotted crotchet", "minim", "dotted minim", "semibreve"
  )
  
  interval_order <- c(
    "unison", "minor second", "major second",
    "minor third", "major third",
    "perfect fourth", "tritone", "perfect fifth",
    "minor sixth", "major sixth",
    "minor seventh", "major seventh", "octave"
  )
  
  consonant_labels <- c(
    "unison", "minor third", "major third",
    "perfect fourth", "perfect fifth", "major sixth", "octave"
  )
  
  consonant_set <- c(
    "unison", "minor third", "major third",
    "perfect fourth", "perfect fifth",
    "minor sixth", "major sixth", "octave"
  )
  
  interval_quality_data <- reactive({
    df1 <- filtered_data()
    df2 <- filtered_data2()
    
    df1 <- df1[!is.na(df1$interval_name) & !is.na(df1$interval_numerical) &
                 !is.na(df1$mint) & df1$interval_numerical <= 10, ]
    
    df2 <- df2[!is.na(df2$interval_name) & !is.na(df2$interval_numerical) &
                 !is.na(df2$mint) & df2$interval_numerical <= 10, ]
    
    df1$direction <- ifelse(grepl("^\\-", df1$mint), "down", "up")
    df2$direction <- ifelse(grepl("^\\-", df2$mint), "down", "up")
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    combined <- bind_rows(df1, df2)
    
    combined <- combined %>%
      mutate(
        interval_generic = sub(".* ", "", interval_name),
        consonance_group = ifelse(as.character(interval_name) %in% consonant_labels, "Consonant", "Dissonant"),
        interval_name = factor(interval_name, levels = interval_order),
        direction = factor(direction, levels = c("up", "down"))
      )
    
    grouped <- combined %>%
      group_by(interval_name, interval_generic, interval_numerical, direction, consonance_group, set) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(set) %>%
      mutate(
        signed_count = ifelse(direction == "down", -count, count),
        percent = round(count / sum(count) * 100, 1)
      ) %>%
      ungroup()
    
    grouped
  })
  
  
  rhythm_chart_data <- reactive({
    
    df1 <- filtered_data()
    df2 <- filtered_data2()
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    df <- bind_rows(df1, df2)
    df <- df[!is.na(df$note_length_name), ]
    
    df %>%
      group_by(set, note_length_name) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(set) %>%
      mutate(percent = round(count / sum(count) * 100, 2)) %>%
      ungroup()
    
  })
  
  
  #KPIs SET 1
  output$distinct_filenames <- renderValueBox({
    valueBox(metrics()$distinct_filenames, "Distinct Filenames", color = "blue")
  })
  
  output$max_pitch <- renderValueBox({
    valueBox(metrics()$max_pitch, "Max Pitch", color = "blue")
  })
  
  output$min_pitch <- renderValueBox({
    valueBox(metrics()$min_pitch, "Min Pitch", color = "blue")
  })
  
  output$ambitus <- renderValueBox({
    valueBox(round(metrics()$ambitus, 1), "Ambitus (Avg)", color = "blue")
  })
  
  #KPIs SET 2
  output$distinct_filenames2 <- renderValueBox({
    valueBox(metrics2()$distinct_filenames, "Distinct Filenames", color = "red")
  })
  
  output$max_pitch2 <- renderValueBox({
    valueBox(metrics2()$max_pitch, "Max Pitch", color = "red")
  })
  
  output$min_pitch2 <- renderValueBox({
    valueBox(metrics2()$min_pitch, "Min Pitch", color = "red")
  })
  
  output$ambitus2 <- renderValueBox({
    valueBox(round(metrics2()$ambitus, 1), "Ambitus (Avg)", color = "red")
  })
  
  #SET1
  output$metersDistributionUI <- renderUI({
    df_meters <- meters_distribution()
    fluidRow(
      lapply(1:nrow(df_meters), function(i) {
        column(width = 2, align = "center",
               strong(df_meters$meters_grouped[i]), br(),
               span(paste0(df_meters$percentage[i], "%")))
      })
    )
  })
  
  output$tonalityDistributionUI <- renderUI({
    df_ground <- tonality_distribution()
    fluidRow(
      lapply(1:nrow(df_ground), function(i) {
        column(width = 2, align = "center",
               strong(df_ground$key[i]), br(),
               span(paste0(df_ground$percentage[i], "%")))
      })
    )
  })
  
  #SET2
  output$metersDistributionUI2 <- renderUI({
    df_meters <- meters_distribution2()
    fluidRow(
      lapply(1:nrow(df_meters), function(i) {
        column(width = 2, align = "center",
               strong(df_meters$meters_grouped[i]), br(),
               span(paste0(df_meters$percentage[i], "%")))
      })
    )
  })
  
  output$tonalityDistributionUI2 <- renderUI({
    df_ground <- tonality_distribution2()
    fluidRow(
      lapply(1:nrow(df_ground), function(i) {
        column(width = 2, align = "center",
               strong(df_ground$key[i]), br(),
               span(paste0(df_ground$percentage[i], "%")))
      })
    )
  })
  
  pie_data_set1 <- reactive({
    get_pie_data("Set 1")
  })
  
  output$intervalChartSet1 <- renderPlotly({
    df_summary <- pie_data_set1()
    
    p <- plot_ly(
      df_summary,
      labels = ~interval_generic,
      values = ~count,
      type = "pie",
      source = "set1_pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(interval_generic, ": ", percent, "% (", count, ")"),
      domain = list(x = c(0.05, 0.95), y = c(0.05, 0.95))
    ) %>%
      layout(title = "Set 1 Intervals",
             showlegend = FALSE,
             margin = list(l = 40, r = 40, b = 40, t = 50) 
      ) %>% event_register("plotly_click")
    
  })
  
  observeEvent(req(plotly::event_data("plotly_click", source = "set1_pie")), {
    event <- plotly::event_data("plotly_click", source = "set1_pie")
    df <- pie_data_set1()
    
    if (!is.null(event$pointNumber)) {
      clicked_label <- as.character(df$interval_generic[event$pointNumber + 1])
      
      if (identical(selected_interval(), clicked_label)) {
        selected_interval(NULL)  # toggle off
      } else {
        selected_interval(clicked_label)
      }
    }
  })
  
  
  filtered_quality_set1 <- reactive({
    df <- interval_quality_data() %>%
      filter(set == "Set 1")
    
    if (!is.null(selected_interval())) {
      df <- df %>% filter(interval_generic == selected_interval())
    }
    
    total <- sum(df$count, na.rm = TRUE)
    
    df <- df %>%
      mutate(
        signed_count = ifelse(direction == "down", -count, count),
        percent = 100 * count / total
      ) %>%
      filter(percent >= 1)
    
    df$interval_name <- factor(df$interval_name, levels = unique(df$interval_name))
    
    df
  })
  
  
  pie_data_set2 <- reactive({
    get_pie_data("Set 2")
  })
  
  output$intervalChartSet2 <- renderPlotly({
    df_summary <- pie_data_set2()
    
    p <- plot_ly(
      df_summary,
      labels = ~interval_generic,
      values = ~count,
      type = "pie",
      source = "set2_pie",
      textinfo = "label+percent",
      hoverinfo = "text",
      text = ~paste(interval_generic, ": ", percent, "% (", count, ")"),
      domain = list(x = c(0.05, 0.95), y = c(0.05, 0.95))
    ) %>%
      layout(title = "Set 2 Intervals",
             showlegend = FALSE,
             margin = list(l = 40, r = 40, b = 40, t = 50)
      ) %>% event_register("plotly_click")
  })
  
  observeEvent(req(plotly::event_data("plotly_click", source = "set2_pie")), {
    event <- plotly::event_data("plotly_click", source = "set2_pie")
    df <- pie_data_set2()
    
    if (!is.null(event$pointNumber)) {
      clicked_label <- as.character(df$interval_generic[event$pointNumber + 1])
      
      if (identical(selected_interval(), clicked_label)) {
        selected_interval(NULL)  # toggle off
      } else {
        selected_interval(clicked_label)
      }
    }
  })
  
  filtered_quality_set2 <- reactive({
    df <- interval_quality_data() %>%
      filter(set == "Set 2")
    
    if (!is.null(selected_interval())) {
      df <- df %>% filter(interval_generic == selected_interval())
    }
    
    total <- sum(df$count, na.rm = TRUE)
    
    df <- df %>%
      mutate(
        signed_count = ifelse(direction == "down", -count, count),
        percent = 100 * count / total
      ) %>%
      filter(percent >= 1)
    
    df$interval_name <- factor(df$interval_name, levels = unique(df$interval_name))
    
    df
  })
  
  combined_detail_quality <- reactive({
    df1 <- filtered_quality_set1()
    df2 <- filtered_quality_set2()
    
    if (nrow(df1) == 0 && nrow(df2) == 0) return(NULL)
    
    df1$set <- "Set 1"
    df2$set <- "Set 2"
    
    df <- bind_rows(df1, df2)
    
  })
  
  
  output$combinedDetailChart <- renderPlotly({
    df <- combined_detail_quality()
    req(df)
    
    plot_ly(
      df,
      x = ~interval_name,
      y = ~signed_count,
      color = ~interaction(set, direction, lex.order = TRUE),
      colors = c(
        "Set 1.up" = "steelblue", "Set 1.down" = "lightblue",
        "Set 2.up" = "salmon", "Set 2.down" = "lightcoral"
      ),
      type = "bar",
      text = ~paste0("Set: ", set,
                     "<br>Interval: ", interval_name,
                     "<br>Direction: ", direction,
                     "<br>Count: ", count,
                     "<br>Percent: ", sprintf("%.1f", percent), "%"),
      textposition = "none",
      hoverinfo = "text"
    ) %>%
      layout(
        title = if (is.null(selected_interval())) {
          "Select an interval to see detailed view"
        } else {
          NULL
        },
        barmode = "group",  # 👈 side-by-side grouping here
        xaxis = list(title = "Interval Name"),
        yaxis = list(title = "Signed Count"),
        legend = list(title = list(text = "Set + Direction"))
      )
  })
  
  
  note_unicode_map <- list(
    "crotchet" = "𝅘𝅥",
    "dotted crotchet" = "𝅘𝅥.",
    "minim" = "𝅗𝅥",
    "dotted minim" = "𝅗𝅥.",
    "semibreve" = "𝅝",
    "quaver" = "𝅘𝅥𝅮",
    "dotted quaver" = "𝅘𝅥𝅮.",
    "semiquaver" = "𝅘𝅥𝅯"
  )
  
  get_note_label <- function(name) {
    unicode <- note_unicode_map[[tolower(name)]]
    if (is.null(unicode)) return(name)
    paste(name, unicode)
  }
  
  rhythm_chart_data_filtered1 <- reactive({
    df <- rhythm_chart_data()
    df1 <- df[df$set == "Set 1", ]
    
    if (!is.null(input$note_symbol_filter1) && length(input$note_symbol_filter1) > 0) {
      df1 <- df1[df1$note_length_name %in% input$note_symbol_filter1, ]
    }
    
    df1 %>%
      mutate(
        percent = round(100 * count / sum(count), 2),
        label = sapply(note_length_name, get_note_label),
        symbol = sapply(note_length_name, function(x) {
          val <- note_unicode_map[[tolower(x)]]
          if (is.null(val)) NA_character_ else val
        }, USE.NAMES = FALSE)
      ) %>%
      filter(!is.na(symbol))
  })
  
  rhythm_chart_data_filtered2 <- reactive({
    df <- rhythm_chart_data()
    df2 <- df[df$set == "Set 2", ]
    
    if (!is.null(input$note_symbol_filter2) && length(input$note_symbol_filter2) > 0) {
      df2 <- df2[df2$note_length_name %in% input$note_symbol_filter2, ]
    }
    
    df2 %>%
      mutate(
        percent = round(100 * count / sum(count), 2),
        label = sapply(note_length_name, get_note_label),
        symbol = sapply(note_length_name, function(x) {
          val <- note_unicode_map[[tolower(x)]]
          if (is.null(val)) NA_character_ else val
        }, USE.NAMES = FALSE)
      ) %>%
      filter(!is.na(symbol))
  })
  
  
  output$rhythmChart1 <- renderPlotly({
    df1 <- rhythm_chart_data_filtered1()
    if (nrow(df1) == 0) return(NULL)
    
    plot_ly(
      df1,
      x = ~symbol,
      y = ~percent,
      type = "bar",
      marker = list(color = "steelblue"),
      text = ~paste0(label, "<br>Percent: ", percent, "%<br>Count: ", count),
      textposition = "none",
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Set 1: Rhythmic Note Distribution",
        xaxis = list(title = "Note Symbol"),
        yaxis = list(title = "Count")
      )
  })
  
  output$rhythmChart2 <- renderPlotly({
    df2 <- rhythm_chart_data_filtered2()
    
    if (nrow(df2) == 0) return(NULL)
    
    plot_ly(
      df2,
      x = ~symbol,
      y = ~percent,
      type = "bar",
      marker = list(color = "salmon"),
      text = ~paste0(label, "<br>Percent: ", percent, "%<br>Count: ", count),
      textposition = "none",
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Set 2: Rhythmic Note Distribution",
        xaxis = list(title = "Note Symbol"),
        yaxis = list(title = "Count")
      )
  })
  
  output$sankeyChart1 <- renderPlotly({
    df <- sankey_data_set1()
    if (nrow(df) == 0) return(NULL)
    
    nodes <- unique(c(df$source_degree, df$target_degree))
    df$source_id <- match(df$source_degree, nodes) - 1
    df$target_id <- match(df$target_degree, nodes) - 1
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(label = nodes),
      link = list(source = df$source_id, target = df$target_id, value = df$value)
    ) |>
      layout(title = "Set 1 – Degree Transitions (Sankey)")
  })
  
  output$sankeyChart2 <- renderPlotly({
    df <- sankey_data_set2()
    if (nrow(df) == 0) return(NULL)
    
    nodes <- unique(c(df$source_degree, df$target_degree))
    df$source_id <- match(df$source_degree, nodes) - 1
    df$target_id <- match(df$target_degree, nodes) - 1
    
    plot_ly(
      type = "sankey",
      arrangement = "snap",
      node = list(label = nodes),
      link = list(source = df$source_id, target = df$target_id, value = df$value)
    ) |>
      layout(title = "Set 2 – Degree Transitions (Sankey)")
  })
  
  
  observeEvent(input$reset_interval, {
    selected_interval(NULL)
  })
  
  
  observe({
    if (is.null(selected_interval())) {
      shinyjs::hide("reset_interval")
    } else {
      shinyjs::show("reset_interval")
    }
  })
  
  group_by_consonance1 <- reactiveVal(FALSE)
  group_by_consonance2 <- reactiveVal(FALSE)
  
  observeEvent(input$toggle_grouping1, {
    group_by_consonance1(!group_by_consonance1())
  })
  
  observeEvent(input$toggle_grouping2, {
    group_by_consonance2(!group_by_consonance2())
  })
  
  
  observe({
    df <- filtered_data2()
    
    if (!"degree" %in% names(df)) return()
    
    degrees <- gsub("^[\\^v]+", "", df$degree)
    degrees <- na.omit(degrees)
    degrees <- unique(degrees)
    degrees <- sort(degrees)
    
    updateSelectInput(session, "selected_source_degree2",
                      choices = if (length(degrees) > 0) degrees else c("No valid degrees" = ""),
                      selected = if ("1" %in% degrees) "1" else degrees[1]
    )
  })
  
  
  output$selectedChart <- renderUI({
    chart_type <- input$chartType
    
    if (chart_type == "Interval Distribution") {
      tagList(
        fluidRow(
          column(6,
                 plotlyOutput("intervalChartSet1", height = "300px"),
                 tags$div(style = "height: 40px;",
                          shinyjs::hidden(
                            actionButton("reset_interval", "Reset Selection", icon = icon("undo"))
                          )
                 )
          ),
          column(6,
                 plotlyOutput("intervalChartSet2", height = "300px")
          )
        ),
        fluidRow(
          column(12, plotlyOutput("combinedDetailChart"))
        )
      )
    } else if (chart_type == "Rhythm analysis") {
      
      fluidRow(
        column(6,
               selectizeInput(
                 "note_symbol_filter1",
                 label = "Set 1: Filter by Note Symbol",
                 choices = note_filter_choices1(),
                 multiple = TRUE,
                 selected = NULL,
                 options = list(placeholder = "Select symbols...")
               ),
               plotlyOutput("rhythmChart1")
        ),
        column(6,
               selectizeInput(
                 "note_symbol_filter2",
                 label = "Set 2: Filter by Note Symbol",
                 choices = note_filter_choices2(),
                 multiple = TRUE,
                 selected = NULL,
                 options = list(placeholder = "Select symbols...")
               ),
               plotlyOutput("rhythmChart2")
        )
      )
    } else if (chart_type == "Degree Transitions (Sankey)") {
      fluidRow(
        column(6,
               selectInput(
                 inputId = "selected_source_degree1",
                 label = "Set 1: Select Source Degree",
                 choices = NULL
               ),
               plotlyOutput("sankeyChart1")
        ),
        column(6,
               selectInput(
                 inputId = "selected_source_degree2",
                 label = "Set 2: Select Source Degree",
                 choices = NULL
               ),
               plotlyOutput("sankeyChart2")
        )
      )
    } else {
      span("No chart selected.")
    }
  })
  
  
  
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  # ── Charts etc. for Text Analysis (BOOKMARK 3) ──────────────────────────────────────────
  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###
  
  # Lyrics search
  matched_filenames1 <- reactive({
    term <- tolower(trimws(input$mus_lyrics_search1))
    if (term == "") return(NULL)           # or return(all filenames) if you prefer
    lyrics_df %>%
      filter(!is.na(lyrics), grepl(term, tolower(lyrics))) %>%
      pull(filename)
  })
  

  matched_filenames2 <- reactive({
    term <- tolower(trimws(input$mus_lyrics_search2))
    if (term == "") return(NULL)           # or return(all filenames) if you prefer
    lyrics_df %>%
      filter(!is.na(lyrics), grepl(term, tolower(lyrics))) %>%
      pull(filename)
  })
  
  
  current_score_page_files <- reactive({
    # Use the CTRA module’s filters (not the dashboard filters)
    df <- get_filtered_data1(ctra1_filters())
    
    files <- unique(df$filename)
    # return all files (possibly empty)
    files
  })
  
  subdir_map <- c(
    K1 = "am_altdeu",
    K2 = "am_boehme",
    K3 = "am_fink",
    K4 = "am_erk",
    K5 = "am_zuccal"
  )
  ## In server, replace your existing renderUI with:
  
  output$ctra_multiple_scores <- renderUI({
    files <- current_score_page_files()
    if (length(files) == 0) {
      return(h4("No scores available for current filters."))
    }

    
    boxes <- lapply(seq_along(files), function(i) {
      fname  <- files[i]
      suffix <- paste0("_", i)
      
      
      cor   <- data$corpus[data$filename == fname][1]
      sub   <- subdir_map[[cor]]
      # 2) build the GitHub URL
      github_url <- paste0(
        "https://github.com/annamatuszewska/The-Essen-Collection/",
        "blob/main/krn/", sub, "/", fname, ".krn"
      )
      
      # build the box title with two actionButtons
      box_title <- tagList(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          span(
            style = "font-weight: bold;",
            data$OTL[data$filename == fname][1]
          ),
          div(
            style = "display: flex; gap: 10px; margin-left: 30px;",
            actionButton(
              inputId = paste0("vhv_button", suffix),
              label   = tagList(icon("external-link-alt"), "View in VHV"),
              class   = "btn-sm btn-default"
            ),
            tags$a(
              href   = github_url,
              target = "_blank",
              class  = "btn btn-default btn-sm",
              icon("github"), "View in GitHub"
            )
          )
        )
      )
      
      # the Humdrum script + trigger
      humdrum_id <- paste0("humdrum", suffix)
      file_path  <- paste0("kern/", fname, ".krn")
      
      box(
        width       = 12,
        title       = box_title,
        status      = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        # Humdrum placeholder
        HTML(sprintf(
          "<div><script id='%s' type='text/x-humdrum'></script></div>",
          humdrum_id
        )),
        tags$script(HTML(sprintf("
        setTimeout(function() {
          Shiny.setInputValue('trigger_score_render%s', {
            source: '%s',
            url: '%s'
          }, {priority: 'event'});
        }, 500);
      ", suffix, humdrum_id, file_path))),
        
        # charts
        fluidRow(
          column(9,
                 div(
                   style = "background-color:#f9f9f9; padding:10px; border-radius:10px; border:1px solid #ccc;",
                   h4("Rhythmical Course", align="center"),
                   plotlyOutput(paste0("note_length_chart", suffix), height = "300px")
                 )
          ),
          column(3,
                 div(
                   style = "background-color:#f9f9f9; padding:10px; border-radius:10px; border:1px solid #ccc;",
                   h4("Sentiment polarity", align="center"),
                   uiOutput(paste0("score_kpi_panel", suffix))
                 )
          )
        )
      )
    })
    
    do.call(tagList, boxes)
  })
  
  ## Then, immediately after that, re‐wire all the per‐file outputs:
  subdir_map <- c(
    K1 = "am_altdeu",
    K2 = "am_boehme",
    K3 = "am_fink",
    K4 = "am_erk",
    K5 = "am_zuccal"
  )
  
  observe({
    files <- current_score_page_files()
    lapply(seq_along(files), function(i) {
      fname  <- files[i]
      suffix <- paste0("_", i)
      file_path <- paste0("kern/", fname, ".krn")
      
      
      # Humdrum render trigger
      observeEvent(input[[paste0("trigger_score_render", suffix)]], {
        session$sendCustomMessage(
          "renderHumdrumScore",
          input[[paste0("trigger_score_render", suffix)]]
        )
      }, once = TRUE)
      
      # Placeholder observers for your two buttons
      observeEvent(input[[paste0("vhv_button", suffix)]], {
        # 1) lookup which corpus/folder this file lives in
        cor    <- data$corpus[data$filename == fname][1]
        folder <- subdir_map[[cor]]
        
        # 2) build the *raw* GitHub URL
        raw_url <- sprintf(
          "https://raw.githubusercontent.com/annamatuszewska/The-Essen-Collection/main/krn/%s/%s.krn",
          folder,
          fname
        )
        
        # 3) now embed that into the Verovio demo URL
        verovio_url <- paste0(
          "https://verovio.humdrum.org/?file=",
          URLencode(raw_url)
        )
        
        # 4) pop it open in a new tab
        shinyjs::runjs(sprintf("window.open('%s','_blank')", verovio_url))
      })
      
      # Rhythm chart
      output[[paste0("note_length_chart", suffix)]] <- renderPlotly({
        df <- get_filtered_data1(ctra1_filters())  # or ctra2_filters()
        df <- df[df$filename == fname, ]
        req(nrow(df) > 0)
        
        library(dplyr)
        library(ggplot2)
        library(plotly)
        
        df <- df %>%
          mutate(
            # cumulative X position
            x_position    = cumsum(lag(sixteenth_value, default = 0)),
            # show the measure number only at the first note of each measure
            label_measure = ifelse(!duplicated(measure), as.character(measure), NA_character_),
            # map note lengths to Unicode (rest→"r")
            note_unicode  = vapply(
              note_length_name,
              function(x) note_unicode_map[[x]] %||% "r",
              character(1)
            ),
            tooltip       = paste0(
              "Measure: ", measure,
              "<br>Note: ", note_unicode,
              "<br>Duration: ", sixteenth_value
            ),
            fill_color    = scales::col_numeric(
              palette = c("#abd9e9","#5ea8c6","#2c7fb8","#4575b4"),
              domain  = range(sixteenth_value, na.rm = TRUE)
            )(sixteenth_value)
          )
        
        # build Y-axis breaks and labels so they match
        y_breaks <- sort(unique(df$sixteenth_value))
        y_labels <- sapply(y_breaks, function(v) {
          # pick the first symbol at that duration
          df$note_unicode[df$sixteenth_value == v][1]
        })
        
        # build X-axis breaks/labels where label_measure is not NA
        x_sel    <- which(!is.na(df$label_measure))
        x_breaks <- df$x_position[x_sel]
        x_labels <- df$label_measure[x_sel]
        
        p <- ggplot(df, aes(
          x = x_position,
          y = sixteenth_value,
          text = tooltip
        )) +
          geom_col(aes(fill = I(fill_color)), width = 2, color = "gray50", size = 0.2) +
          scale_y_continuous(breaks = y_breaks, labels = y_labels) +
          scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = expansion(mult = c(0.01, 0.01))) +
          labs(
            x = NULL,           # drop the "x_position" label
            y = "Note Type",
            title = NULL
          ) +
          theme_minimal() +
          theme(
            axis.title.y      = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position    = "none"
          )
        
        ggplotly(p, tooltip = "text") %>%
          layout(hoverlabel = list(font = list(size = 12)))
      })
      
      
      # KPI panel
      output[[paste0("score_kpi_panel", suffix)]] <- renderUI({
        filtered <- lyrics_clean %>% filter(doc_id == fname)
        if (nrow(filtered)==0) return(NULL)
        
        # 1) build word counts
        word_count <- filtered %>%
          left_join(nrc_emotionen, by="word", relationship="many-to-many") %>%
          filter(!is.na(sentiment)) %>%
          count(sentiment, name="words")
        
        # 2) your pill helpers
        valence_colors <- c(positive="#6BAED6", negative="#990F02")
        valence_order  <- c("positive","negative")
        
        valence_pill <- function(label,n){
          tags$div(
            style=paste0(
              "background-color:", valence_colors[label],
              "; color:white; padding:6px 12px; border-radius:20px;",
              "margin:4px; font-weight:bold; text-align:center; width:100%;"
            ),
            paste0(label, ": ", n)
          )
        }
        
        emotion_pill <- function(label,n){
          tags$div(
            style=paste0(
              "background-color:#e6e6e6; color:black; padding:6px 12px;",
              "border-radius:20px; margin:4px; font-weight:bold;",
              "text-align:center; width:100%;"
            ),
            paste0(label, ": ", n)
          )
        }
        
        # 3) valence pills
        valence_tags <- lapply(valence_order, function(v){
          n <- word_count$words[word_count$sentiment==v] %||% 0
          valence_pill(v,n)
        })
        
        # 4) split emotions into positive vs negative
        pos_emotions <- c("surprise","anticipation","trust","joy")
        neg_emotions <- c("anger","fear","disgust","sadness")
        
        pos_tags <- lapply(pos_emotions, function(e){
          n <- word_count$words[word_count$sentiment==e] %||% 0
          emotion_pill(e,n)
        })
        neg_tags <- lapply(neg_emotions, function(e){
          n <- word_count$words[word_count$sentiment==e] %||% 0
          emotion_pill(e,n)
        })
        
        # 5) build two‐column layout
        tagList(
          # your valence pills in a row:
          div(
            style="display:flex; justify-content: space-around; margin-bottom:10px;",
            valence_tags
          ),
          # then two columns of emotions
          div(
            style="display:flex; justify-content: space-between;",
            # left column: positive emotions
            div(style="flex:1; display:flex; flex-direction:column;", pos_tags),
            # right column: negative emotions
            div(style="flex:1; display:flex; flex-direction:column;", neg_tags)
          )
        )
      })
      
    })
  })
  
  
  
  current_score_page_files2 <- reactive({
    df <- get_filtered_data2(ctra2_filters())
    unique(df$filename)
  })
  
  # ─── CTRA Set 2: Dynamic UI ────────────────────────────────────────────────
  output$ctra2_multiple_scores <- renderUI({
    files <- current_score_page_files2()
    if (length(files) == 0) {
      return(h4("No scores available for current filters."))
    }
    
    subdir_map <- c(
      K1 = "am_altdeu",
      K2 = "am_boehme",
      K3 = "am_fink",
      K4 = "am_erk",
      K5 = "am_zuccal"
    )
    
    boxes <- lapply(seq_along(files), function(i) {
      fname     <- files[i]
      # give each box a unique suffix (here we prefix with "_2_")
      suffix    <- paste0("_2_", i)
      humdrum_id <- paste0("humdrum", suffix)
      file_path  <- paste0("kern/", fname, ".krn")
      
      cor   <- data$corpus[data$filename == fname][1]
      sub   <- subdir_map[[cor]]
      # 2) build the GitHub URL
      github_url <- paste0(
        "https://github.com/annamatuszewska/The-Essen-Collection/",
        "blob/main/krn/", sub, "/", fname, ".krn"
      )
      
      box(
        width       = 12,
        title       = tagList(
          div(
            style = "display: flex; justify-content: space-between; align-items: center;",
            # Left: Title
            span(style = "font-weight: bold;", data$OTL[data$filename == fname][1]),
            # Right: two buttons with unique IDs
            div(
              style = "display: flex; gap: 10px; margin-left: 30px;",
              actionButton(
                inputId = paste0("vhv_button", suffix),
                label   = tagList(icon("external-link-alt"), "View in VHV"),
                class   = "btn-sm btn-default"
              ),
              tags$a(
                href   = github_url,
                target = "_blank",
                class  = "btn btn-default btn-sm",
                icon("github"), "View in GitHub"
              )
            )
          )
        ),
        status = "danger",
        solidHeader = TRUE,
        collapsible = TRUE,
        
        # Humdrum script placeholder + trigger
        HTML(sprintf("<div><script id='%s' type='text/x-humdrum'></script></div>", humdrum_id)),
        tags$script(HTML(sprintf("
        setTimeout(function() {
          Shiny.setInputValue('trigger_score_render%s', {
            source: '%s',
            url: '%s'
          }, {priority: 'event'});
        }, 500);
      ", suffix, humdrum_id, file_path))),
        
        # Chart + KPI
        fluidRow(
          column(9,
                 div(
                   style = "background-color:#f9f9f9; padding:10px; border-radius:10px; border:1px solid #ccc;",
                   h4("Rhythmical Course", style = "margin-top: 5px; margin-bottom: 10px; text-align: center;"),
                   plotlyOutput(paste0("note_length_chart", suffix), height = "300px")
                 )
          ),
          column(3,
                 div(
                   style = "background-color:#f9f9f9; padding:10px; border-radius:10px; border:1px solid #ccc;",
                   h4("Sentiment polarity", style = "margin-top: 5px; margin-bottom: 10px; text-align: center;"),
                   uiOutput(paste0("score_kpi_panel", suffix))
                 )
          )
        )
      )
    })
    
    do.call(tagList, boxes)
    
  })
  
  # ─── CTRA Set 2: Wire up each box’s outputs & buttons ───────────────────────
  observe({
    files <- current_score_page_files2()
    lapply(seq_along(files), function(i) {
      fname  <- files[i]
      suffix <- paste0("_2_", i)
      
      # 1) Humdrum render trigger
      observeEvent(input[[paste0("trigger_score_render", suffix)]], {
        session$sendCustomMessage(
          "renderHumdrumScore",
          input[[paste0("trigger_score_render", suffix)]]
        )
      }, once = TRUE)
      
      # 2) Placeholder for your buttons
      observeEvent(input[[paste0("vhv_button", suffix)]], {
        showNotification(paste("VHV clicked for", fname), type = "message")
        # later: browseURL(...) or other logic
      })
      
      # Rhythm chart
      output[[paste0("note_length_chart", suffix)]] <- renderPlotly({
        df <- get_filtered_data1(ctra2_filters())  # or ctra2_filters()
        df <- df[df$filename == fname, ]
        req(nrow(df) > 0)
        
        library(dplyr)
        library(ggplot2)
        library(plotly)
        
        df <- df %>%
          mutate(
            # cumulative X position
            x_position    = cumsum(lag(sixteenth_value, default = 0)),
            # show the measure number only at the first note of each measure
            label_measure = ifelse(!duplicated(measure), as.character(measure), NA_character_),
            # map note lengths to Unicode (rest→"r")
            note_unicode  = vapply(
              note_length_name,
              function(x) note_unicode_map[[x]] %||% "r",
              character(1)
            ),
            tooltip       = paste0(
              "Measure: ", measure,
              "<br>Note: ", note_unicode,
              "<br>Duration: ", sixteenth_value
            ),
            fill_color    = scales::col_numeric(
              palette = c("#abd9e9","#5ea8c6","#2c7fb8","#4575b4"),
              domain  = range(sixteenth_value, na.rm = TRUE)
            )(sixteenth_value)
          )
        
        # build Y-axis breaks and labels so they match
        y_breaks <- sort(unique(df$sixteenth_value))
        y_labels <- sapply(y_breaks, function(v) {
          # pick the first symbol at that duration
          df$note_unicode[df$sixteenth_value == v][1]
        })
        
        # build X-axis breaks/labels where label_measure is not NA
        x_sel    <- which(!is.na(df$label_measure))
        x_breaks <- df$x_position[x_sel]
        x_labels <- df$label_measure[x_sel]
        
        p <- ggplot(df, aes(
          x = x_position,
          y = sixteenth_value,
          text = tooltip
        )) +
          geom_col(aes(fill = I(fill_color)), width = 2, color = "gray50", size = 0.2) +
          scale_y_continuous(breaks = y_breaks, labels = y_labels) +
          scale_x_continuous(breaks = x_breaks, labels = x_labels, expand = expansion(mult = c(0.01, 0.01))) +
          labs(
            x = NULL,           # drop the "x_position" label
            y = "Note Type",
            title = NULL
          ) +
          theme_minimal() +
          theme(
            axis.title.y      = element_blank(),
            panel.grid.major.x = element_blank(),
            legend.position    = "none"
          )
        
        ggplotly(p, tooltip = "text") %>%
          layout(hoverlabel = list(font = list(size = 12)))
      })
      
      
      # KPI panel
      output[[paste0("score_kpi_panel", suffix)]] <- renderUI({
        filtered <- lyrics_clean %>% filter(doc_id == fname)
        if (nrow(filtered)==0) return(NULL)
        
        # 1) build word counts
        word_count <- filtered %>%
          left_join(nrc_emotionen, by="word", relationship="many-to-many") %>%
          filter(!is.na(sentiment)) %>%
          count(sentiment, name="words")
        
        # 2) your pill helpers
        valence_colors <- c(positive="#6BAED6", negative="#990F02")
        valence_order  <- c("positive","negative")
        
        valence_pill <- function(label,n){
          tags$div(
            style=paste0(
              "background-color:", valence_colors[label],
              "; color:white; padding:6px 12px; border-radius:20px;",
              "margin:4px; font-weight:bold; text-align:center; width:100%;"
            ),
            paste0(label, ": ", n)
          )
        }
        
        emotion_pill <- function(label,n){
          tags$div(
            style=paste0(
              "background-color:#e6e6e6; color:black; padding:6px 12px;",
              "border-radius:20px; margin:4px; font-weight:bold;",
              "text-align:center; width:100%;"
            ),
            paste0(label, ": ", n)
          )
        }
        
        # 3) valence pills
        valence_tags <- lapply(valence_order, function(v){
          n <- word_count$words[word_count$sentiment==v] %||% 0
          valence_pill(v,n)
        })
        
        # 4) split emotions into positive vs negative
        pos_emotions <- c("surprise","anticipation","trust","joy")
        neg_emotions <- c("anger","fear","disgust","sadness")
        
        pos_tags <- lapply(pos_emotions, function(e){
          n <- word_count$words[word_count$sentiment==e] %||% 0
          emotion_pill(e,n)
        })
        neg_tags <- lapply(neg_emotions, function(e){
          n <- word_count$words[word_count$sentiment==e] %||% 0
          emotion_pill(e,n)
        })
        
        # 5) build two‐column layout
        tagList(
          # your valence pills in a row:
          div(
            style="display:flex; justify-content: space-around; margin-bottom:10px;",
            valence_tags
          ),
          # then two columns of emotions
          div(
            style="display:flex; justify-content: space-between;",
            # left column: positive emotions
            div(style="flex:1; display:flex; flex-direction:column;", pos_tags),
            # right column: negative emotions
            div(style="flex:1; display:flex; flex-direction:column;", neg_tags)
          )
        )
      })
      
    })
  })

  
  
  ###  ###  ###  ###  ###  ###  ###  ###
  ### dataTable - on the other tab   ###
  ###  ###  ###  ###  ###  ###  ###  ###
  
  output$dataTable <- renderDT({
    df <- get_filtered_data1(dash1_filters())

    df_display <- df %>%
      distinct(filename, .keep_all = TRUE) %>%
      select(-ambitus) %>%
      left_join(ambitus_data, by = "filename",
                  suffix = c(".old", "")) %>%
      select(
        `Song title` = OTL,
        `Region` = ARE,
        `Social function` = GTL,
        `Meter` = meters,
        `Scale` = AMD,
        `Basic tone` = key,
        `Scale degrees` = scl_value,
        `Max Pitch` = max_pitch,
        `Min Pitch` = min_pitch,
        `Ambitus` = ambitus,
        `URL scan` = `URL-scan`,
        filename
      )

    datatable(
      df_display,
      options = list(scrollX = TRUE, pageLength = 25),
      rownames = FALSE
    )
  })
  
  
  output$dataTable2 <- renderDT({
    df <- get_filtered_data2(dash2_filters())
    
    df_display <- df %>%
      distinct(filename, .keep_all = TRUE) %>%
      select(-ambitus) %>% 
      left_join(ambitus_data, by = "filename",
                  suffix = c(".old", "")) %>%
      select(
        `Song title` = OTL,
        `Region` = ARE,
        `Social function` = GTL,
        `Meter` = meters,
        `Scale` = AMD,
        `Basic tone` = key,
        `Scale degrees` = scl_value,
        `Max Pitch` = max_pitch,
        `Min Pitch` = min_pitch,
        `Ambitus` = ambitus,
        `URL scan` = `URL-scan`,
        filename
      )
    
    datatable(
      df_display,
      options = list(scrollX = TRUE, pageLength = 25),
      rownames = FALSE
    )
  })
  
  
  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("esac_filtered_chart_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      # Interval pie chart (summarized)
      intervals1 <- interval_data() %>%
        filter(set == "Set 1") %>%
        group_by(interval_generic) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      intervals2 <- interval_data() %>%
        filter(set == "Set 2") %>%
        group_by(interval_generic) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      # Interval Quality w/ direction
      quality1 <- interval_quality_data() %>%
        filter(set == "Set 1") %>%
        group_by(interval_name, direction) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      quality2 <- interval_quality_data() %>%
        filter(set == "Set 2") %>%
        group_by(interval_name, direction) %>%
        summarise(count = sum(count), .groups = "drop") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      # Note length bar charts
      bar1 <- rhythm_chart_data() %>%
        filter(set == "Set 1") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      bar2 <- rhythm_chart_data() %>%
        filter(set == "Set 2") %>%
        mutate(percent = round(100 * count / sum(count), 2))
      
      # Export sheets
      sheets <- list(
        Set1_Intervals = intervals1,
        Set2_Intervals = intervals2,
        Set1_Quality_Direction = quality1,
        Set2_Quality_Direction = quality2,
        Set1_Rhythm = bar1,
        Set2_Rhythm = bar2
      )
      
      writexl::write_xlsx(sheets, path = file)
    }
  )
  
  
}

shinyApp(ui, server)
