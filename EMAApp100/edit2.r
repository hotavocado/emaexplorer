##datatable for histogram
output$table3 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))




#Select main variable
output$rhistvar <- renderUI({selectInput('rhistvar', 'Main Variable:', c(names(dataset())), selected = rhistvariables$var, selectize=TRUE)})


#Select coloring variable
output$rhistcolor <- renderUI({selectInput('rhistcolor', 'Color By:', c("None", varnames2.1$df), selected = rhistvariables$color, selectize=TRUE)})


#hist color variable
varcolor2.1 <- reactiveValues(l="ID")

observeEvent(input$rhist1, {if (input$rhistcolor %in% c("ID", "None")) {varcolor2.1$l <- input$rhistcolor}
  else {varcolor2.1$l <- paste0(input$rhistcolor, "_s")} 
})

# output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})


##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
varnames2.1 <- reactiveValues(df=NULL)


##Color datasets
stratify_vars2.1 <- reactiveValues(df = NULL, df2 = NULL)

##Default datasets
observeEvent(input$go, priority = -1, {
  
  stratify_vars2.1$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
  stratify_vars2.1$df2 <- stratify_vars2.1$df
  
  varnames2.1$df <- names(stratify_vars$df_full)
  
  
})


##create the appropriate stratify_vars2.1$df 
observeEvent(input$rhist1, {
  
  if(!input$rhistcolor %in% c("ID", "None")) {
    stratify_vars2.1$df <- stratify_vars$df_full %>% select_("ID", input$rhistcolor)

  }
  
  else {stratify_vars2.1$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
  
})

##Color vars dataset, upon action button, stratify_vars2.1$df2 will update based on variable and quantile selection

observeEvent(input$rhist1, ignoreInit = T, {
  
  stratify_vars2.1$df2 <-  
    
    
    if(input$rhistcolor %in% c("ID", "None")) {stratify_vars2.1$df}
  
  else {
    if(input$rhistradio %in% "Auto"){
      stratify_vars2.1$df %>% 
        mutate_at(input$rhistcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$rhistntile)}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
          else {x=NA}
        }) %>% 
        rename_at(vars(input$rhistcolor), ~ paste0(input$rhistcolor, "_s"))
    }
    
    else if (input$rhistradio %in% "On"){
      stratify_vars2.1$df %>% 
        mutate_at(input$rhistcolor, ~my_ntiles(.x, input$rhistntile)) %>%
        rename_at(vars(input$rhistcolor), ~ paste0(input$rhistcolor, "_s"))
      
    }
    
    else if (input$rhistradio %in% "Off"){
      stratify_vars2.1$df %>% 
        rename_at(vars(input$rhistcolor), ~ paste0(input$rhistcolor, "_s"))
      
    }
  }
  
})



#Random plot

#make plot update after new variables are selected

rhistvariables <- reactiveValues(var = "ID", color = "None")


#Default hist variable
observeEvent(input$go, priority = -1, {
  
  rhistvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
  
})

rhistrandbutton <- reactiveValues(r1=NULL)

observeEvent(input$rhistrandom, priority = 2, ignoreInit = T, {
  
  rhistrandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  rhistrandbutton$r2 <- sample(1:length(varnames2.2$df), 1)
  
  
  if("Main Var." %in% input$rhistrand_choice) {rhistvariables$var <- names(dataset())[[rhistrandbutton$r1]]}
  
  if("Color Var." %in% input$rhistrand_choice) {rhistvariables$color <- varnames2.1$df[[rhistrandbutton$r2]]}
  
})


##Data for response hists

rhistdata <- reactiveValues(l=NULL, m=NULL)

observeEvent(input$rhist1, ignoreInit = T, {

  rhistdata$l <- 
    dataset() %>% 
    select_("ID", "timepoint", input$rhistvar) %>%
    left_join(stratify_vars2.2$df2, by="ID")
  
})



#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

rhist_dummy <- reactiveValues(l=0)

observeEvent(input$rhist1, ignoreInit = T, {
  if (rhist_dummy$l==0) {rhist_dummy$l <- 1}
  else NULL
})

##Reset plot when dataset changes

observeEvent(input$go, {
  rhist_dummy$l <- 0
  
})

##Instructions that appear before create plot button is clicked

output$rboxplot_instr <-  renderText(
  if(rboxplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)

##code for histogram

output$histR <- renderPlotly({ 
  
  input$rhist1
  
  isolate(
    
      
  if(varcolor2.1 %in% c("ID", "None")) {
   
   ggplotly(ggplot(data=rhistdata$l, aes_string(x=input$rhistvar, fill = "blue", color = "blue")) +
              geom_histogram() +
              theme_bw())
 }
  
  else{
  
 ggplotly(ggplot(data=rhistdata$l, aes_string(x=input$rhistvar, fill = varcolor2.1, color = varcolor2.1)) +
            geom_histogram() +
            theme_bw())
    
  }
)

})


