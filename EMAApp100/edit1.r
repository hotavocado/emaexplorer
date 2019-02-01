##data table for boxplot
output$table4 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))


#Select main variable
output$rboxplotvar <- renderUI({selectInput('rboxplotvar', 'Main Variable:', c(names(dataset())), selected = rboxplotvariables$var, selectize=TRUE)})



#Select coloring variable
output$rboxplotcolor <- renderUI({selectInput('rboxplotcolor', 'Color By:', c("None", varnames2.2$df), selected = rboxplotvariables$color, selectize=TRUE)})


#boxplot color variable
varcolor2.2 <- reactiveValues(l="ID")

observeEvent(input$rboxplot1, {if (input$rboxplotcolor %in% c("ID", "None")) {varcolor2.2$l <- input$rboxplotcolor}
  else {varcolor2.2$l <- paste0(input$rboxplotcolor, "_s")} 
})

# output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})


##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
varnames2.2 <- reactiveValues(df=NULL)


##Color datasets
stratify_vars2.2 <- reactiveValues(df = NULL, df2 = NULL)

##Default datasets
observeEvent(input$go, priority = -1, {
  
  stratify_vars2.2$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
  stratify_vars2.2$df2 <- stratify_vars2.2$df
  
  varnames2.2$df <- names(stratify_vars$df_full)
  
  
})


##create the appropriate stratify_vars2.2$df 
observeEvent(input$rboxplot1, {
  
  if(!input$rboxplotcolor %in% c("ID", "None")) {
    stratify_vars2.2$df <- stratify_vars$df_full %>% select_("ID", input$rboxplotcolor) %>%
      group_by(ID) %>%
      summarise_at(input$rboxplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                  ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup() 
  }
  
  else {stratify_vars2.2$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
  
})

##Color vars dataset, upon action button, stratify_vars2.2$df2 will update based on variable and quantile selection

observeEvent(input$rboxplot1, ignoreInit = T, {
  
  stratify_vars2.2$df2 <-  
    
    
    if(input$rboxplotcolor %in% c("ID", "None")) {stratify_vars2.2$df}
  
  else {
    if(input$rboxplotradio %in% "Auto"){
      stratify_vars2.2$df %>% 
        mutate_at(input$rboxplotcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$rboxplotntile)}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
          else {x=NA}
        }) %>% 
        rename_at(vars(input$rboxplotcolor), ~ paste0(input$rboxplotcolor, "_s"))
    }
    
    else if (input$rboxplotradio %in% "On"){
      stratify_vars2.2$df %>% 
        mutate_at(input$rboxplotcolor, ~my_ntiles(.x, input$rboxplotntile)) %>%
        rename_at(vars(input$rboxplotcolor), ~ paste0(input$rboxplotcolor, "_s"))
      
    }
    
    else if (input$rboxplotradio %in% "Off"){
      stratify_vars2.2$df %>% 
        rename_at(vars(input$rboxplotcolor), ~ paste0(input$rboxplotcolor, "_s"))
      
    }
  }
  
})


#Random plot

#make plot update after new variables are selected

rboxplotvariables <- reactiveValues(var = "ID", color = "None")

#Default boxplot variable
observeEvent(input$go, priority = -1, {
  
  rboxplotvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
  
})

rboxplotrandbutton <- reactiveValues(r1=NULL, r2=NULL)

observeEvent(input$rboxplotrandom, priority = 2, ignoreInit = T, {
  
  rboxplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  rboxplotrandbutton$r2 <- sample(1:length(varnames2.2$df), 1)
  
  
  if("Main Var." %in% input$rboxplotrand_choice) {rboxplotvariables$var <- names(dataset())[[rboxplotrandbutton$r1]]}
  
  if("Color Var." %in% input$rboxplotrand_choice) {rboxplotvariables$color <- varnames2.2$df[[rboxplotrandbutton$r2]]}
  
})


##Data for response boxplots

rboxplotdata <- reactiveValues(l=NULL, m=NULL)

observeEvent(input$rboxplot1, ignoreInit = T, {
  
  
  rboxplotdata$l <- 
    dataset() %>% 
    select_("ID", "timepoint", input$rboxplotvar) %>%
    group_by_("ID") %>%
    summarise_all(mymean) %>%
    left_join(stratify_vars2.2$df2, by="ID")
  
  #time of day boxplot
  rboxplotdata$m <- 
    dataset() %>% 
    select_("ID", "timepoint", "timeofday", input$rboxplotvar) %>%
    group_by_("ID", "timeofday") %>%
    summarise_all(mymean)
  
})



#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

rboxplot_dummy <- reactiveValues(l=0)

observeEvent(input$rboxplot1, ignoreInit = T, {
  if (rboxplot_dummy$l==0) {rboxplot_dummy$l <- 1}
  else NULL
})

##Reset plot when dataset changes

observeEvent(input$go, {
  rboxplot_dummy$l <- 0
  
})

##Instructions that appear before create plot button is clicked

output$rboxplot_instr <-  renderText(
  if(rboxplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)


##Main boxplot code
output$boxplot_dR <- renderPlotly({
  
  input$rboxplot1
  
  isolate(
      