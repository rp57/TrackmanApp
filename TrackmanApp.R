library(shiny)
library(tidyverse)
library(gt)
library(dplyr)

ui <- fluidPage(
  titlePanel(strong("Projected Pitch Success (0 - 1) from Trackman Data"),
             windowTitle = "Projected Pitch Success (0 - 1) from Trackman Data" ),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload a Trackman File:"),
      strong("How to use the App:"),
      p(" - Upload Trackman File (ensure it is a Trackman File)"),
      p(" - Wait (it may take a couple of minutes) for the output"),
      p(" - Model Output will be displayed on the right"),
      strong("Model Scale:"),
      p("0: Lowest Possible Pitch Success Metric"),
      p("~0.5: Average"),
      p("1: Highest Possible Pitch Succcess Metric"),
      strong("Note: If your data has missing data for key pich metrics (velocity, break, etc.), those will be filtered out and affect the reliability of your results."),
      p(),
      p("Check Out my Article for More!:"),
      tags$a(href = "https://medium.com/@xRPx7/a-shiny-app-to-summarize-pitch-quality-from-a-trackman-file-23e5f37516d2",
             "Article Link"),
    ),
    
    mainPanel(
      tableOutput("contents"),
    )
  )
)

# Reliant on an accurate tagged type
server <- function(input, output, session) {
  uploaded_file <- reactiveVal(NULL)
 
  
  check_column_names <- function(file_data) {
    comp <- read.csv("20230217-Joe Miller Ballpark-1.csv") # Example Trackman Data
    
    if (all(colnames(file_data) == colnames(comp))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
  
  observeEvent(input$file, {
    req(input$file)
    
    if (check_column_names(read.csv(input$file$datapath))) {
      uploaded_file(read.csv(input$file$datapath))
    } else {
      print("Wrong File Type, Please Upload Trackman Data!", type = "error")
      uploaded_file(NULL)
    }
  })
  
  processing <- reactive({
    games_data_test <- as.data.frame(uploaded_file())
    
    if(nrow(games_data_test) == 0) {
      return(NULL)
    }
    
    games_data_test$success_pitch <- 0
    
    
    vector_cols <- as.vector(c(
      "RelSpeed", "VertRelAngle", "HorzRelAngle",    
      "SpinRate", "SpinAxis", "Extension",      
      "VertBreak", "InducedVertBreak", "HorzBreak",       
      "PlateLocHeight", "PlateLocSide", "ZoneSpeed",       
      "VertApprAngle", "ZoneTime", "RelSide",      
      "success_pitch", "Pitcher", "TaggedPitchType", "RelHeight"
    ))
    
    
    
    
    games_data_test <- games_data_test %>%
      filter(TaggedHitType != "Bunt")
    
    
    
    any_missing <- any(colSums(is.na(games_data_test[, vector_cols])) == nrow(games_data_test))
    
    if(any_missing) {
      showNotification("Data has empty column(s), can't perform calculations!", type = "error")
      return(NULL)
    }
    
    which_miss <- as.vector(which(is.na(games_data_test[, vector_cols])))
    
    if(!is_empty(which_miss)) {
      games_data_test <- games_data_test[-which_miss,]
    }
    
    
    for (i in 1:nrow(games_data_test)) {
      if (games_data_test$PitchCall[i] == "FoulBall") {
        games_data_test$success_pitch[i] <- 0.5
      } else if (games_data_test$PitchCall[i] == "StrikeCalled" || games_data_test$PitchCall[i] == "StrikeSwinging") {
        games_data_test$success_pitch[i] <- 1
      } else if (games_data_test$TaggedPitchType[i] == "ChangeUp" || games_data_test$TaggedPitchType[i] == "Cutter" 
                 || games_data_test$TaggedPitchType[i] == "Sinker") {
        if (games_data_test$TaggedHitType[i] == "GroundBall") {
          if (!is.na(games_data_test$ExitSpeed[i]) && games_data_test$ExitSpeed[i] < 95) {
            games_data_test$success_pitch[i] <- 1
          }
        } else {
          games_data_test$success_pitch[i] <- 0
        }
      } else if (games_data_test$TaggedPitchType[i] == "Fastball") {
        if (games_data_test$TaggedHitType[i] == "Popup") {
          games_data_test$success_pitch[i] <- 1
        } else if (games_data_test$TaggedHitType[i] == "FlyBall" && !is.na(games_data_test$ExitSpeed[i]) && games_data_test$ExitSpeed[i] < 95) {
          games_data_test$success_pitch[i] <- 1
        }
      }
    }
    
    
    stats_test <- games_data_test %>%
      group_by(Pitcher,TaggedPitchType) %>%
      select(vector_cols)
    
  
    pitcher_fastballs_test <- stats_test %>% filter(TaggedPitchType %in% c("Fastball", "Cutter", "Sinker")) %>% group_by(Pitcher) %>% 
      summarize(fb_velo = mean(RelSpeed, na.rm = T), fb_max_ivb = max(InducedVertBreak, na.rm = T), fb_max_x = max(HorzBreak, na.rm = T), fb_min_x = min(HorzBreak, na.rm = T), 
                fb_max_velo = max(RelSpeed, na.rm = T),fb_axis = mean(SpinAxis, na.rm = T), pitches = n())
    
    statsPitch_test <- stats_test %>% left_join(pitcher_fastballs_test, by = "Pitcher") %>% mutate(spin_dif = SpinAxis - fb_axis, velo_dif = RelSpeed - fb_velo,
                                                                                    ivb_dif = fb_max_ivb - InducedVertBreak, break_dif = (fb_max_x*.5+fb_min_x*.5)-HorzBreak)
    
    final_vars_test <- statsPitch_test %>% select(success_pitch, starts_with("fb_"), RelSpeed, SpinRate,
                                        Extension, RelSide, RelHeight, HorzBreak, VertBreak,
                                        TaggedPitchType, SpinAxis, spin_dif, velo_dif, ivb_dif, break_dif) 
    library(caret)
    library(xgboost)
    
    dmy_test <- dummyVars(" ~ TaggedPitchType", data = final_vars_test)
    trsf_test <- data.frame(predict(dmy_test, newdata = final_vars_test))
    
    vars_test <- cbind(final_vars_test, trsf_test) %>% select(-TaggedPitchType) 
    
    vars_test <- vars_test[,-1]
    
    set.seed(42)
    indices <- sample(1:nrow(vars_test), 0.7 * nrow(vars_test))
    train1 <- vars_test[indices, ]
    test1 <- vars_test[-indices, ]
    
    
    
    x1 <- as.matrix(train1[,c(-2,-1)])
    y1 <- train1$success_pitch
    
    
    xgb_data_test <- xgb.DMatrix(data = x1, label = y1)
    
    xgb_model_test <- xgboost(data = x1, label = y1, nrounds = 10)
    
    predictions_test <- predict(xgb_model_test, as.matrix(test1[,c(-2,-1)]))
    
    
    predictions_test <- cbind(test1, predictions_test)
    
    
    colnames(predictions_test)[ncol(predictions_test)] <- "predict_success_x"
    
    full_data_test <- rbind(test1,train1)
    
    predictions_full_test <- predict(xgb_model_test, as.matrix(full_data_test[,c(-2,-1)]))
    
    predictions_full_test <- cbind(full_data_test, predictions_full_test)
    colnames(predictions_full_test)[ncol(predictions_full_test)] <- "predict_success_test"
    
    extract_pitch_type <- function(col_name) {
      gsub("^TaggedPitchType", "", col_name)
    }
    
    predictions_full_test$pitch_type <- apply(predictions_full_test[, startsWith(names(predictions_full_test), "TaggedPitchType")], 1, function(row) {
      pitch_type_columns <- which(row == 1) 
      if (length(pitch_type_columns) == 0) {
        return(NA)  
      } else {
        return(extract_pitch_type(names(row)[pitch_type_columns])) 
      }
    })
    

    predictions_full_pitches_test <- predictions_full_test %>%
      group_by(Pitcher, pitch_type) %>%
      summarize(Predicted_mean_pitch_success = mean(predict_success_test),
                PitchesThrown = n())%>%
      arrange(-Predicted_mean_pitch_success)
    
    predictions_full_pitches_test <- predictions_full_pitches_test %>% 
      filter(!(pitch_type == "Undefined")) 
    
    predictions_full_pitches_test <- predictions_full_pitches_test %>% 
      filter(!(pitch_type == "Other")) 
    
    predictions_full_pitches_test <- separate(predictions_full_pitches_test, Pitcher, into = c("last_name", "first_name"), sep = ", ")
  
    predictions_full_pitches_test$Pitcher <- paste(predictions_full_pitches_test$first_name, predictions_full_pitches_test$last_name)
    
    predictions_full_pitches_test$Predicted_mean_pitch_success <- round(predictions_full_pitches_test$Predicted_mean_pitch_success, 3)
    
    pal <- colorRamp(c("blue", "red"))
    
    gt_table <-
      predictions_full_pitches_test %>% 
      ungroup() %>%
        select(
          Pitcher = Pitcher, `Pitch Type` = pitch_type,
          Pitches = PitchesThrown,
          `Predicted Success` = Predicted_mean_pitch_success, 
        ) %>% 
        gt() %>%
      tab_header(title = "Predicted Pitch Success (0 - 1)",
                 subtitle = "by Rohan Patel") %>%

      
      data_color(
        columns = `Predicted Success`,
        palette = pal
      )
    return(gt_table)
    
  })
  
  output$contents <- render_gt({
    processing()
  })
}



shinyApp(ui = ui, server = server)




