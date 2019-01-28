#Response Page 5: Scatterplot-----------------------------------------------------------------------------------------------------

##data table for scatterplot
output$table7 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))

##Select y axis variable:
output$scatterplot_y_var <- renderUI({selectInput('scatterplot_y_var', 'Y-Axis Variable:', c(names(dataset())), selected = scatterplotvariables$y_var, selectize=TRUE)})

##Select x axis variable
output$scatterplot_x_var <- renderUI({selectInput('scatterplot_x_var', 'X-Axis Variable:', c(names(dataset())), selected = scatterplotvariables$x_var, selectize=TRUE)})

##Color/interaction datasets
stratify_vars2.5 <- reactiveValues(df = NULL, df2 = NULL)

##Using varnames so updating stratify_vars3.2$df2 doesn't refresh subbrowsecolor
varnames2.5 <- reactiveValues(df=NULL)

##Default datasets for color/interaction
observeEvent(input$go, priority = -1, {
  
  stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID") %>% mutate(dummy_s = 1)
  stratify_vars2.5$df2 <- stratify_vars$df_full %>% select_("ID") %>% mutate(dummy_s = 1)
  
  varnames2.5$df <- names(stratify_vars$df_full)
  
})


##create main dataset with the appropriate level
dataset_levels2.5 <- reactiveValues(df = NULL)

observeEvent({input$scatterplot1}, priority = 1, ignoreInit = T,  {
  
  if (input$scatterplotlevel %in% "Subject") {
    
    dataset_levels2.5$df <- dataset() %>% 
      select_("ID", input$scatterplot_y_var, input$scatterplot_x_var) %>% mutate(dummy=1) %>%
      group_by(ID) %>%
      summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                              ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup()
    
    
  }

  
  else if (input$scatterplotlevel %in% "Day") {
    
    dataset_levels2.5$df <- dataset() %>% 
      select_("ID", "day", input$scatterplot_y_var, input$scatterplot_x_var) %>% mutate(dummy=1) %>%
      group_by(ID, day) %>%
      summarise_all(function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                              ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup()
    
    
  }
  
  else if (input$subbrowselevel %in% "Assessment") {
    
    dataset_levels2.5$df <- dataset() %>% select_("ID", "timeindex", "day", input$scatterplot_y_var, input$scatterplot_x_var) %>% mutate(dummy=1)
  }
})


##Level select for color variable, choices depend on level of main dataset

output$scatterplotcolorlevel <- renderUI({
  
  if (input$scatterplotlevel %in% "Assessment") {
    radioButtons("scatterplotcolorlevel", "Color Variable Level:", c("Subject", "Day", "Assessment"), selected = "Assessment", inline = T)
  }
  
  else if (input$scatterplotlevel %in% "Day") {
    radioButtons("scatterplotcolorlevel", "Color Variable Level:", c("Subject", "Day"), selected = "Day", inline = T)
  }
  
  else if (input$scatterplotlevel %in% "Subject") {
    radioButtons("scatterplotcolorlevel", "Color Variable Level:", c("Subject"), selected = "Subject", inline = T)
  }
  
})


##Key variable for each combination of raw/normalized and assessment/day/subject for coloring variable
colorvarkey2.5 <- reactive({paste0(input$scatterplotcolortype, "-", input$scatterplotcolorlevel)})

##create the appropriate stratify_vars2.5$df , default color/interaction dataset
observeEvent({input$scatterplot1}, priority = 1, ignoreInit = T, {
  
  if(!input$scatterplotcolor %in% c("ID", "None")) {
    
    if (colorvarkey2.5() %in% "Raw-Subject") {
      stratify_vars2.5$df <- stratify_vars$df_full %>%
        group_by(ID) %>%
        summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                     ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup()
      
    }
    
    else if (colorvarkey2.5() %in% "Raw-Day") {
      stratify_vars2.5$df <- stratify_vars$df_full %>%
        group_by(ID, day) %>%
        summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                     ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() 
      
    }
    
    else if (colorvarkey2.5() %in% "Raw-Assessment") {
      stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$scatterplotcolor)
    }
    
    else if (colorvarkey2.5() %in% "Subject Normalized-Subject") {
      stratify_vars2.5$df <-  stratify_vars$df_full %>%
        group_by(ID) %>%
        summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                     ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() 
      
    }
    
    else if (colorvarkey2.5() %in% "Subject Normalized-Day") {
      stratify_vars2.5$df <- stratify_vars$df_full %>%
        group_by(ID, day) %>%
        summarise_at(input$scatterplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                     ifelse(is.numeric(x), mean(x, na.rm = T), NA))
        return(y)}) %>% 
        ungroup() %>% group_by(ID) %>%
        mutate_at(input$scatterplotcolor, funs(normalize)) %>%
        ungroup()
    }
    
    else if (colorvarkey2.5() %in% "Subject Normalized-Assessment") {
      stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex", input$scatterplotcolor) %>%
        group_by(ID) %>%
        mutate_at(input$scatterplotcolor, funs(normalize)) %>%
        ungroup()
    }
    
  }
  
  else {
    
    if (colorvarkey2.5() %in% c("Raw-Assessment", "Subject Normalized-Assessment")) {
      
      stratify_vars2.5$df <- stratify_vars$df_full %>% select_("ID", "timeindex") %>% mutate(dummy_s=1)}
    
    else if (colorvarkey2.5() %in% c("Raw-Day", "Subject Normalized-Day")) {
      
      stratify_vars2.5$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID, day) %>% summarise(dummy_s=mean(dummy))}
    
    else if (colorvarkey2.5() %in% c("Raw-Subject", "Subject Normalized-Subject")) {
      
      stratify_vars2.5$df <- stratify_vars$df_full %>% mutate(dummy=1) %>% group_by(ID) %>% summarise(dummy_s=mean(dummy))}
    
  }
  
})

##Select coloring variable
output$scatterplotcolor <- renderUI({selectInput('scatterplotcolor', 'Color By:', varnames2.5$df, selected=scatterplotvariables$color, selectize=TRUE)})

###varcolor2.5$l is used so selecting color input doesn't automatically update plot
varcolor2.5 <- reactiveValues(l="None")

observeEvent({input$scatterplotcolor}, ignoreInit = T, { if(input$scatterplotcolor %in% c("ID", "None")) {varcolor3.2$l <- input$scatterplotcolor}
  else {varcolor3.2$l <- paste0(input$scatterplotcolor, "_s")} 
})

##Color variable dataset join vars, depending on level and type
df2_join_vars2.5 <- reactive({ if (input$scatterplotcolorlevel %in% "Subject") {c("ID")}
  else if (input$scatterplotcolorlevel %in% "Day") {c("ID", "day")}
  else if (input$scatterplotcolorlevel %in% "Assessment") {c("ID", "timeindex")}
})


##Color vars dataset, upon action button, stratify_vars2.5$df2 will update based on variable and quantile selection

observeEvent(input$scatterplot1, priority = 1, ignoreInit = T, {
  
  stratify_vars2.5$df2 <-  
    
    if(input$scatterplotcolor %in% "ID") {stratify_vars3.2$df}
  
  else {
    if(input$scatterplotradio %in% "Auto"){
      stratify_vars3.2$df %>% select_(df2_join_vars2.5()[1], df2_join_vars2.5()[length(df2_join_vars2.5())], input$scatterplotcolor) %>%
        mutate_at(input$scatterplotcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$scatterplotntile)}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
          else {x=NA}
        }) %>% 
        rename_at(vars(input$scatterplotcolor), ~ paste0(input$scatterplotcolor, "_s"))
    }
    
    else if (input$scatterplotradio %in% "On"){
      stratify_vars3.2$df %>% select_(df2_join_vars2.5()[1], df2_join_vars2.5()[length(df2_join_vars2.5())], input$scatterplotcolor) %>%
        mutate_at(input$scatterplotcolor, ~my_ntiles(.x, input$scatterplotntile)) %>%
        rename_at(vars(input$scatterplotcolor), ~ paste0(input$scatterplotcolor, "_s"))
      
    }
    
    else if (input$scatterplotradio %in% "Off"){
      stratify_vars3.2$df %>% 
        select_(df2_join_vars2.5()[1], df2_join_vars2.5()[length(df2_join_vars2.5())], input$scatterplotcolor) %>%
        rename_at(vars(input$scatterplotcolor), ~ paste0(input$scatterplotcolor, "_s"))
      
    }
  }
  
  
})

#Random plot

#make plot update after new variables are selected

scatterplotvariables <- reactiveValues(var_x = "ID", var_y = "ID", color = "weekday")

scatterplotrandbutton <- reactiveValues(r1=NULL, r2=NULL, r3=NULL)

observeEvent(input$scatterplotrandom, priority = 2, ignoreInit = T, {
  
  scatterplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  scatterplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  scatterplotrandbutton$r3 <- sample(1:length(varnames2.5$df), 1)
  
  
  if("X-Axis Var." %in% input$scatterplotrand_choice) {scatterplotvariables$var_x <- names(dataset())[[scatterplotrandbutton$r1]]}
  
  if("Y-Axis Var." %in% input$scatterplotrand_choice) {scatterplotvariables$var_y <- varnames3.2$df[[scatterplotrandbutton$r2]]}
  
  if ("Color Var." %in% input$scatterplotrand_choice) {scatterplotvariables$color <- varnames3.2$df[[scatterplotrandbutton$r3]]}
  
  
})

##datasets for scatterplot

scatterplotdata <- reactiveValues(l=NULL)

observeEvent(input$scatterplot1, ignoreInit = T, {
  
  scatterplotdata$l <- 
    
    if (input$scatterplotraw %in% "Raw" | input$scatterplot_y_var %in% "ID") {
      
      dataset_levels2.5$df %>% 
        left_join(stratify_vars2.5$df2, by=c(df2_join_vars2.5()))
    }
  
    else if (input$scatterplotraw %in% "Subject Normalized") {
    
    dataset_levels$df %>% 
      group_by_("ID") %>%
      mutate_at(input$scatterplotvar, funs(normalize)) %>%
      ungroup() %>%
      left_join(stratify_vars2.5$df2, by=c(df2_join_vars2.5())) 
      
    }
  
})


#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

scatterplot_dummy <- reactiveValues(l=0)

observeEvent(input$scatterplot1, priority = 0, ignoreInit = T, {
  if (scatterplot_dummy$l==0) {scatterplot_dummy$l <- 1}
  else NULL
})


##Reset plot when dataset changes

observeEvent(input$go, {
  scatterplot_dummy$l <- 0
  
})



##Instructions that appear before create plot button is clicked

output$scatterplot_instr <-  renderText(
  if(scatterplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)


##Scatterplot 

output$scatterplot <- renderPlotly({
  
  input$scatterplot1
  
  isolate(
    
    if(subbrowseplot_dummy$l==0) NULL
    
    else{
        
      if (varcolor2.5$l %in% "None") {
          
        ggplotly(ggplot(scatterplotdata$l, aes_string(x=input$scatterplot_x_var, y=input$scatterplot_y_var, label = "ID")) +
                  geom_point() +
                  geom_smooth(method='lm', formula=y~x)+
                  theme_bw(),  height=800, width=1000) 
          
        }
        
        else {
          
        ggplotly(ggplot(scatterdata(), aes_string(x=input$scatterplot_x_var, y=input$scatterplot_y_var, label = "ID", color = varcolor2.5$l)) +
                  geom_point() +
                  geom_smooth(method='lm', formula=y~x)+
                  theme_bw(), height=800, width=1000)
          
        }
    }
  )
})
        


