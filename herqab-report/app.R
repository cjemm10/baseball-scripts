library(shiny)
library(dplyr)
library(ggplot2)
library(assertive, warn.conflicts = FALSE)

hitter_report = read.csv("www/hitter-report.csv", header = TRUE)

ui <- fluidPage(
  navbarPage(
    "Maryland Baseball Hitting 2021-2022",
    tabPanel("Team & Individual H.E.R & Q.A.B Report",
      fileInput("file", "Choose CSV File", accept = ".csv"),
      fluidRow(
        column(width = 12,
               "Hitter Efficiency Rating (H.E.R)",
               tableOutput("her_table")
        ),
        column(width = 12,
               "Quality of At Bat (Q.A.B)",
               tableOutput("qab_table")
        ),
         column(width = 5,
                "Pitch Locations by Pitch Category",
                plotOutput("pitch_category_plot", height = 500, width = 600)
         ),
         column(width = 5,
                "Pitch Locations by Pitch Result",
                plotOutput("pitch_result_plot", height = 500, width = 600)
         )
       ),
       hr(),
       wellPanel(
         fluidRow(
           column(width = 2, align = "left",
                  selectInput("GameType", label = "Game Type:", choices = "No choices yet"),
                  selectInput("PitcherThrows", label = "LHP vs. RHP:", choices = "No choices yet")
           ),
           column(width = 2, align = "left",
                  selectInput("Opponent", label = "Opponent:", choices = "No choices yet"),
                  selectInput("pitch_category", label = "Pitch Category:", choices = "No choices yet")
           ),
           column(width = 2, align = "left",
                  selectInput("Date", label = "Date:", choices = "No choices yet"),
                  selectInput("count_type", label = "Count Type:", choices = "No choices yet")
           ),
           column(width = 2, align = "center",
                  selectInput("playerID", label = "Batter Name:", choices = "No choices yet"),
                  selectInput("GameID", label = "GameID:", choices = "No choices yet")
           ),
           column(width = 5, align = "center",
                  sliderInput("pitchVelo", label = "Pitch Velocity:", min = 50, max = 105, value = c(50,105)),
                  downloadButton("report", "Generate report")
           )
         )
       )
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$file, {
    
    hitter_report <- read.csv(input$file$datapath, header = TRUE)
    
    updateSelectInput(session, "GameType", label = "Game Type:", choices = append("All", as.list(hitter_report$GameType)))
    updateSelectInput(session, "PitcherThrows", label = "LHP vs. RHP:", choices = append("All", as.list(hitter_report$PitcherThrows)))
    updateSelectInput(session, "Opponent", label = "Opponent:", choices = append("All", as.list(hitter_report$PitcherTeam)))
    updateSelectInput(session, "pitch_category", label = "Pitch Category:", choices = append("All", as.list(hitter_report$pitch_category)))
    updateSelectInput(session, "Date", label = "Date:", choices = append("All", as.list(hitter_report$Date)))
    
    updateSelectInput(session, "count_type", label = "Count Type:", choices = append("All", as.list(hitter_report$count_type)))
    updateSelectInput(session, "playerID", label = "Batter:", choices = append("All", as.list(hitter_report$Batter)))
    updateSelectInput(session, "GameID", label = "GameID:", choices = append("All", as.list(unique(hitter_report$GameID))))
    
    updateSelectInput(session, "pitch_category", label = "Pitch Category:", choices = append("All", as.list(hitter_report$pitch_category)))
    updateSliderInput(session, "pitchVelo", label = "Pitch Velocity:",  min = min(unlist(hitter_report$RelSpeed), na.rm = TRUE),
                                                                        max = max(unlist(hitter_report$RelSpeed), na.rm = TRUE),
                                                                        value = c(min(unlist(hitter_report$RelSpeed), na.rm = TRUE), max(unlist(hitter_report$RelSpeed), na.rm = TRUE)))
    
  })
  
  output$pitch_category_plot <- renderPlot({
    p <- pitch_category_plot()
    print(p)
  })
  
  output$pitch_result_plot <- renderPlot({
    p <- pitch_result_plot()
    print(p)
  })
  
  pitch_category_plot <- reactive({
    if (input$GameType == "All"){
      bp <- hitter_report
    } else {
      bp <- hitter_report %>% filter(hitter_report$GameType == input$GameType)
    }
    
    if (input$PitcherThrows == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherThrows == input$PitcherThrows)
    }
    
    if (input$Opponent == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherTeam == input$Opponent)
    }
    
    if (input$pitch_category == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$pitch_category == input$pitch_category)
    }
    
    if (input$Date == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Date == input$Date)
    }
    
    if (input$count_type == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$count_type == input$count_type)
    }
    
    if (input$playerID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Batter == input$playerID)
    }
    
    if (input$GameID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$GameID == input$GameID)
    }
    
    min_velo = (input$pitchVelo)[1]
    max_velo = (input$pitchVelo)[2]
    
    bp <- bp %>% filter(as.numeric(RelSpeed) >= min_velo) %>% filter(as.numeric(RelSpeed) <= max_velo) 
    
    pitch_height <- bp$PlateLocHeight
    pitch_width <- bp$PlateLocSide
    
    ggplot(data = bp, aes(pitch_width, pitch_height, color = pitch_category)) + 
      
    geom_point(shape=19, size=2) +
    xlim(-4,4) +
    ylim(-2,6) +
    
    # Add Edge Zone (EZ) to plot
    annotate("rect", xmin = -1, xmax =  1, ymin = 1.25, ymax = 3.85, color = "chartreuse3", fill="darkgreen", alpha=.2) +
    
    geom_segment(aes(x = -1.0, xend = -1.0, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =  1.0, xend =  1.0, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =   -4, xend =    4, y = 1.25, yend = 1.25), color = "black") +
    geom_segment(aes(x =   -4, xend =    4, y =  3.85, yend = 3.85), color = "black") +
      
    # Add Damage Zone (DZ) to plot
    annotate("rect", xmin = -0.6, xmax =  0.6, ymin = 1.75, ymax = 3.35, color = "darkred", fill="darkred", alpha=.2) +
      
    geom_segment(aes(x = -0.6, xend = -0.6, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =  0.6, xend =  0.6, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =   -4, xend =    4, y = 1.75, yend = 1.75), color = "black") +
    geom_segment(aes(x =   -4, xend =   4, y =  3.35, yend = 3.35), color = "black")
  })
  
  pitch_result_plot <- reactive({
    if (input$GameType == "All"){
      bp <- hitter_report
    } else {
      bp <- hitter_report %>% filter(hitter_report$GameType == input$GameType)
    }
    
    if (input$PitcherThrows == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherThrows == input$PitcherThrows)
    }
    
    if (input$Opponent == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherTeam == input$Opponent)
    }
    
    if (input$pitch_category == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$pitch_category == input$pitch_category)
    }
    
    if (input$Date == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Date == input$Date)
    }
    
    if (input$count_type == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$count_type == input$count_type)
    }
    
    if (input$playerID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Batter == input$playerID)
    }
    
    if (input$GameID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$GameID == input$GameID)
    }
    
    min_velo = (input$pitchVelo)[1]
    max_velo = (input$pitchVelo)[2]
    
    bp <- bp %>% filter(as.numeric(RelSpeed) >= min_velo) %>% filter(as.numeric(RelSpeed) <= max_velo) 
    
    bp <- bp %>% filter(result != "") 
    
    pitch_height <- bp$PlateLocHeight
    pitch_width <- bp$PlateLocSide
    
    ggplot(data = bp, aes(pitch_width, pitch_height, color = result)) + 
      
    geom_point(shape=19, size=2) +
    xlim(-4,4) +
    ylim(-2,6) +
    
    # Add Edge Zone (EZ) to plot
    annotate("rect", xmin = -1, xmax =  1, ymin = 1.25, ymax = 3.85, color = "chartreuse3", fill="darkgreen", alpha=.2) +
    
    geom_segment(aes(x = -1.0, xend = -1.0, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =  1.0, xend =  1.0, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =   -4, xend =    4, y = 1.25, yend = 1.25), color = "black") +
    geom_segment(aes(x =   -4, xend =    4, y =  3.85, yend = 3.85), color = "black") +
    
    # Add Damage Zone (DZ) to plot
    annotate("rect", xmin = -0.6, xmax =  0.6, ymin = 1.75, ymax = 3.35, color = "darkred", fill="darkred", alpha=.2) +
    
    geom_segment(aes(x = -0.6, xend = -0.6, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =  0.6, xend =  0.6, y =   -2, yend =    6), color = "black") +
    geom_segment(aes(x =   -4, xend =    4, y = 1.75, yend = 1.75), color = "black") +
    geom_segment(aes(x =   -4, xend =   4, y =  3.35, yend = 3.35), color = "black")
    
  })
  
  output$her_table <- renderTable(
    her_table()
  )
  
  her_table <- reactive({
    if (input$GameType == "All"){
      bp <- hitter_report
    } else {
      bp <- hitter_report %>% filter(hitter_report$GameType == input$GameType)
    }
    
    if (input$PitcherThrows == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherThrows == input$PitcherThrows)
    }
    
    if (input$Opponent == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherTeam == input$Opponent)
    }
    
    if (input$pitch_category == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$pitch_category == input$pitch_category)
    }
    
    if (input$Date == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Date == input$Date)
    }
    
    if (input$count_type == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$count_type == input$count_type)
    }
    
    if (input$playerID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Batter == input$playerID)
    }
    
    if (input$GameID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$GameID == input$GameID)
    }
    
    pitches_seen <- bp %>% filter(bp$pa_count == 1)
    
    pitches_seen <- setNames(aggregate(cbind(PitchofPA) ~ Batter, data = pitches_seen, FUN=sum),
                             c("Batter", "Seen"))
      
    bp <- setNames(aggregate(cbind(pa_count, dz_swing_pts, dz_take_pts, cz_swing_pts, cz_take_pts, p_score, walks_count, hbp_count, strikeout_count, total_bases, rbi_qab, r_score) ~ Batter, data = bp, FUN=sum),
             c("Batter", "PA", "DZ Swing", "DZ Take", "CZ Swing", "CZ Take", "P-Score", "BB", "HBP", "K", "TB", "RBI", "R-Score"))
    
    bp <- merge(bp, pitches_seen, by="Batter")
    
    bp["Total Score"] <- bp["R-Score"] + bp["P-Score"]
    bp <- bp[, c(1,2,14,3,4,5,6,7,8,9,10,11,12,13,15)]
  })
  
  output$qab_table <- renderTable(
    qab_table()
  )
  
  qab_table <- reactive({
    if (input$GameType == "All"){
      bp <- hitter_report
    } else {
      bp <- hitter_report %>% filter(hitter_report$GameType == input$GameType)
    }
    
    if (input$PitcherThrows == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherThrows == input$PitcherThrows)
    }
    
    if (input$Opponent == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$PitcherTeam == input$Opponent)
    }
    
    if (input$pitch_category == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$pitch_category == input$pitch_category)
    }
    
    if (input$Date == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Date == input$Date)
    }
    
    if (input$count_type == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$count_type == input$count_type)
    }
    
    if (input$playerID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$Batter == input$playerID)
    }
    
    if (input$GameID == "All"){
      bp <- bp
    } else {
      bp <- bp %>% filter(bp$GameID == input$GameID)
    }
    
    bp[is.na(bp)] <- 0
    
    bp <- setNames(aggregate(cbind(pa_count, hit_count, hard_hit_count, ab_length, sacrifice, walk_hbp_qab, runner_moved, rbi_qab, qab_yesno) ~ Batter, data = bp, FUN=sum),
                  c("Batter", "PA", "Hit", "Hard Hit", "8+ Pitches", "Sacrifice", "BB/HBP", "Runner Moved", "RBI", "QAB Total"))
    
    bp["QAB%"] <- bp["QAB Total"] / bp["PA"] * 100
    
    bp
  })
  
  output$report <- downloadHandler(
    filename = function(){
      if (input$playerID == "All") {
        filename = paste("Team-HER-QAB-Report", ".pdf", sep = "")
      } else {
        filename = paste("Individual-HER-QAB-Report-", input$playerID, ".pdf", sep = "")
      }
    },
    
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      params <- list(Date=input$Date, GameType = input$GameType, PitcherThrows = input$PitcherThrows, Opponent = input$Opponent,
                     pitch_category = input$pitch_category, count_type = input$count_type, playerID = input$playerID,
                     GameID = input$GameID, pitchVelo = input$pitchVelo, plot1 = pitch_category_plot(), plot2 = pitch_result_plot(), her_table = her_table(),
                     qab_table = qab_table(), pwd = getwd())
      
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
                        
      )
    }
  )
}

shinyApp(ui = ui, server = server)