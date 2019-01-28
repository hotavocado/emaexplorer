#Subject Dashboard Page 2: Subject Trajetory Browse------------------------------------------------------------------------------------------------------------------------------
  
  ##Select main y axis variable:
  output$subbrowsevar <- renderUI({selectInput('subbrowsevar', 'Main Variable:', c(names(dataset())), selected = subbrowsevariables$var, selectize=TRUE)})
  
  ##Color and order datasets:
  stratify_vars3.2 <- reactiveValues(df = NULL, df2 = NULL, df3 = NULL)
  
  ##Default datasets for color and order
  observeEvent(input$go, priority = -1, {
    
    stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s = 1)
    stratify_vars3.2$df2 <- stratify_vars$df_full %>% select_("ID", "weekday") %>% rename(weekday_s = weekday)
    stratify_vars3.2$df3 <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_o=1)
    
    varnames3.2$df <- names(stratify_vars$df_full)
    
    
  })
  
  
  ##create main dataset with the appropriate level
  dataset_levels <- reactiveValues(df = NULL)
  
  observeEvent({input$subbrowse1}, priority = 1, ignoreInit = T,  {
    
    if (input$subbrowselevel %in% "Day") {
      
      dataset_levels$df <- dataset() %>% 
        select_("ID", "day", input$subbrowsevar) %>% mutate(dummy=1) %>%
        group_by(ID, day) %>%
        summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup()
      
      
    }
    
    else if (input$subbrowselevel %in% "Assessment") {
      
      dataset_levels$df <- dataset() %>% select_("ID", "timeindex", "day", input$subbrowsevar) %>% mutate(dummy=1)
      
      
      
    }
  })
  
  
  ##Using varnames so updating stratify_vars3.2$df2 doesn't refresh subbrowsecolor
  varnames3.2 <- reactiveValues(df=NULL)
  
  ##Level select for color variable, choices depend on the level of main dataset:
  
  output$subbrowsecolorlevel <- renderUI({
    
    if (input$subbrowselevel %in% "Assessment") {
      radioButtons("subbrowsecolorlevel", "Color Variable Level:", c("Subject", "Day", "Assessment"), selected = "Assessment", inline = T)
    }
    
    else if (input$subbrowselevel %in% "Day") {
      radioButtons("subbrowsecolorlevel", "Color Variable Level:", c("Subject", "Day"), selected = "Day", inline = T)
    }
    
  })
  
  
  
  ##Key variable for each combination of raw/normalized and assessment/day/suject for coloring variable
  colorvarkey3.2 <- reactive({paste0(input$subbrowsecolortype, "-", input$subbrowsecolorlevel)})
  
  ##create the appropriate stratify_vars3.2$df 
  observeEvent({input$subbrowse1}, priority = 1, ignoreInit = T, {
    
    if(!input$subbrowsecolor %in% "ID") {
      
      if (colorvarkey3.2() %in% "Raw-Subject") {
        stratify_vars3.2$df <- stratify_vars$df_full %>%
          group_by(ID) %>%
          summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup()
        
      }
      
      else if (colorvarkey3.2() %in% "Raw-Day") {
        stratify_vars3.2$df <- stratify_vars$df_full %>%
          group_by(ID, day) %>%
          summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
        
      }
      
      else if (colorvarkey3.2() %in% "Raw-Assessment") {
        stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$subbrowsecolor)
      }
      
      else if (colorvarkey3.2() %in% "Subject Normalized-Subject") {
        stratify_vars3.2$df <-  stratify_vars$df_full %>%
          group_by(ID) %>%
          summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() 
        
      }
      
      else if (colorvarkey3.2() %in% "Subject Normalized-Day") {
        stratify_vars3.2$df <- stratify_vars$df_full %>%
          group_by(ID, day) %>%
          summarise_at(input$subbrowsecolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), NA))
          return(y)}) %>% 
          ungroup() %>% group_by(ID) %>%
          mutate_at(input$subbrowsecolor, funs(normalize)) %>%
          ungroup()
      }
      
      else if (colorvarkey3.2() %in% "Subject Normalized-Assessment") {
        stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$subbrowsecolor) %>%
          group_by(ID) %>%
          mutate_at(input$subbrowsecolor, funs(normalize)) %>%
          ungroup()
      }
      
    }
    
    else {
      
      if (colorvarkey3.2() %in% c("Raw-Assessment", "Subject Normalized-Assessment")) {
        
        stratify_vars3.2$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s=1)}
      
      else if (colorvarkey3.2() %in% c("Raw-Day", "Subject Normalized-Day")) {
        
        stratify_vars3.2$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID, day) %>% summarise(dummy_s=mean(dummy))}
      
      else if (colorvarkey3.2() %in% c("Raw-Subject", "Subject Normalized-Subject")) {
        
        stratify_vars3.2$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID) %>% summarise(dummy_s=mean(dummy))}
      
    }
    
  })
  
  
  ##Select coloring variable (subject level, can bin into quantiles)
  output$subbrowsecolor <- renderUI({selectInput('subbrowsecolor', 'Color By:', varnames3.2$df, selected=subbrowsevariables$color, selectize=TRUE)})
  
  ###varcolor3.2$l is used so selecting color input doesn't automatically update plot
  varcolor3.2 <- reactiveValues(l="weekday")
  
  observeEvent({input$subbrowsecolor}, ignoreInit = T, { if(input$subbrowsecolor %in% "ID") {varcolor3.2$l <- input$subbrowsecolor}
    else {varcolor3.2$l <- paste0(input$subbrowsecolor, "_s")} 
  })
  
  #output$test1 <- renderText(names(subbrowsedata$l))
  
  #output$test2 <- renderText(input$subbrowsecolor)
  
  ##Color variable dataset join vars, depending on level and type
  df2_join_vars <- reactive({ if (input$subbrowsecolorlevel %in% "Subject") {c("ID")}
    else if (input$subbrowsecolorlevel %in% "Day") {c("ID", "day")}
    else if (input$subbrowsecolorlevel %in% "Assessment") {c("ID", "timeindex")}
  })
  
  ##Color vars dataset, upon action button, stratify_vars3.2$df2 will update based on variable and quantile selection
  
  observeEvent(input$subbrowse1, priority = 1, ignoreInit = T, {
    
    stratify_vars3.2$df2 <-  
      
      if(input$subbrowsecolor %in% "ID") {stratify_vars3.2$df}
    
    else {
      if(input$subbrowseradio %in% "Auto"){
        stratify_vars3.2$df %>% select_(df2_join_vars()[1], df2_join_vars()[length(df2_join_vars())], input$subbrowsecolor) %>%
          mutate_at(input$subbrowsecolor, function (x) { if (is.character(x) | is.factor(x)) {x}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$subbrowsentile)}
            else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
            else {x=NA}
          }) %>% 
          rename_at(vars(input$subbrowsecolor), ~ paste0(input$subbrowsecolor, "_s"))
      }
      
      else if (input$subbrowseradio %in% "On"){
        stratify_vars3.2$df %>% select_(df2_join_vars()[1], df2_join_vars()[length(df2_join_vars())], input$subbrowsecolor) %>%
          mutate_at(input$subbrowsecolor, ~my_ntiles(.x, input$subbrowsentile)) %>%
          rename_at(vars(input$subbrowsecolor), ~ paste0(input$subbrowsecolor, "_s"))
        
      }
      
      else if (input$subbrowseradio %in% "Off"){
        stratify_vars3.2$df %>% 
          select_(df2_join_vars()[1], df2_join_vars()[length(df2_join_vars())], input$subbrowsecolor) %>%
          rename_at(vars(input$subbrowsecolor), ~ paste0(input$subbrowsecolor, "_s"))
        
      }
    }
    
    
  })
  
  ##Select subject ordering variable 
  output$subbrowseorder <- renderUI({selectInput('subbrowseorder', 'Order Subjects By:', varnames3.2$df, selected = subbrowsevariables$order, selectize=TRUE)})
  
  ##Ordered list of subject ID
  subjectorder3.2 <- reactiveValues(l=NULL, m=NULL)
  
  observeEvent(input$go, {
    subjectorder3.2$l=as.character(stratify_vars$df_sub[["ID"]])
  })
  
  
  
  ##Ordering vars dataset, upon action button, stratify_vars3.2$df3 will update based on variable, ordered vector of subjectIDs will be created
  observeEvent(input$subbrowse1, priority = 1, ignoreInit = T, {
    stratify_vars3.2$df3 <-  
      if (!input$subbrowseorder %in% "ID"){
        stratify_vars$df_full %>% 
          group_by(ID) %>%
          summarise_at(input$subbrowseorder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                       ifelse(is.numeric(x), mean(x, na.rm = T), "Error"))
          return(y)}) %>% 
          ungroup() %>%
          arrange_(input$subbrowseorder) %>%
          rename_at(vars(input$subbrowseorder), ~ paste0(input$subbrowseorder, "_o")) %>%
          mutate(ID_val = paste0(ID, " | ", input$subbrowseorder, "=", ordershow(.data[[paste0(input$subbrowseorder, "_o")]])))
      }
    
    else {
      stratify_vars$df_sub %>% 
        select(1) %>% 
        arrange_("ID") %>%
        mutate(ID_val = ID)
    }
    
    
    #mutate(ID_val = paste0(ID, " Order Var: ", ordershow(.data[[paste0(input$subbrowseorder, "_o")]])))
    #mutate(ID_val = ID)
    
    subjectorder3.2$l <- stratify_vars3.2$df3[["ID"]]
    subjectorder3.2$m <- stratify_vars3.2$df3[["ID_val"]]
    
    
  })
  
  ##Page Selector
  
  pagenum3.2 <- reactiveValues(l=1)
  
  output$subbrowsepage <- renderUI({selectInput('subbrowsepage', 'Go To Page:', c(1:ceiling(nrow(stratify_vars$df_sub)/4)), selectize=TRUE)})
  
  observeEvent(input$subbrowsepage, {pagenum3.2$l <- as.numeric(input$subbrowsepage)})
  
  observeEvent(input$subbrowseprev, {
    if (pagenum3.2$l > 1) {pagenum3.2$l <- pagenum3.2$l - 1}
  }) 
  
  observeEvent(input$subbrowsenext, {
    if (pagenum3.2$l < ceiling(nrow(stratify_vars$df_sub)/4)) {pagenum3.2$l <- pagenum3.2$l + 1}
  }) 
  
  ##display page
  
  output$pagenum3.2_display <- renderText(print(pagenum3.2$l))
  
  
  ##Pull increments of 16 subjects based on the value of page selector, the IDs are stored as subjectorder16
  subjectorder16_3.2 <- reactive({ 
    #when page number is max:
    if (pagenum3.2$l==ceiling(nrow(stratify_vars$df_sub)/4)) {subjectorder3.2$l[((as.numeric(pagenum3.2$l) - 1)*4 + 1) : length(subjectorder3.2$l)]}
    #otherwise: ie 1-16, 17-32, and so on
    else {subjectorder3.2$l[((pagenum3.2$l - 1)*4 + 1) : (pagenum3.2$l*4)]}
    
  }) 
  
  subjectorder16_3.2_val <-reactive({ 
    #when page number is max:
    if (pagenum3.2$l==ceiling(nrow(stratify_vars$df_sub)/4)) {subjectorder3.2$m[((as.numeric(pagenum3.2$l) - 1)*4 + 1) : length(subjectorder3.2$m)]}
    #otherwise: ie 1-16, 17-32, and so on
    else {subjectorder3.2$m[((pagenum3.2$l - 1)*4 + 1) : (pagenum3.2$l*4)]}
    
  }) 
  
  #reset page number on new plot
  
  observeEvent(input$subbrowse1, {pagenum3.2$l <- 1})
  
  
  #Random plot
  
  #make plot update after new variables are selected
  #subbrowserandomcheck <- reactiveValues(l=0, m=0)
  
  
  subbrowsevariables <- reactiveValues(var = "ID", color = "weekday", order = "ID")
  
  subbrowserandbutton <- reactiveValues(r1=NULL, r2=NULL, r3=NULL)
  
  observeEvent(input$subbrowserandom, priority = 2, ignoreInit = T, {
    
    subbrowserandbutton$r1 <- sample(1:length(names(dataset())), 1)
    
    subbrowserandbutton$r2 <- sample(1:length(varnames3.2$df), 1)
    
    subbrowserandbutton$r3 <- sample(1:length(varnames3.2$df), 1)
    
    
    if("Main Var." %in% input$subbrowserand_choice) {subbrowsevariables$var <- names(dataset())[[subbrowserandbutton$r1]]}
    
    if("Color Var." %in% input$subbrowserand_choice) {subbrowsevariables$color <- varnames3.2$df[[subbrowserandbutton$r2]]}
    
    if ("Order Var." %in% input$subbrowserand_choice) {subbrowsevariables$order <- varnames3.2$df[[subbrowserandbutton$r3]]}
    
    
  })
  
  
  subbrowsedata <- reactiveValues(l=NULL)
  
  observeEvent( c(input$subbrowse1, pagenum3.2$l), ignoreInit = T, {
    
    subbrowsedata$l <- 
      
      
      
      
      