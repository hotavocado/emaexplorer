#Responses Page 3, heatmap of responses-----------------------------------------------------------------------------------------------------

##datatable for heatmaps
output$table5 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))


#Select main variable
output$rheatmapvar <- renderUI({selectInput('rheatmapvar', 'Main Variable:', c(names(dataset())), selected = rheatmapvariables$var, selectize=TRUE)})



#Select ordering variable
output$rheatmaporder <- renderUI({selectInput('rheatmaporder', 'order By:', c("None", "Response", varnames2.3$df), selected = rheatmapvariables$order, selectize=TRUE)})


#heatmap order variable
varorder2.3 <- reactiveValues(l="ID")

observeEvent(input$rheatmap1, {if (input$rheatmaporder %in% c("ID", "None", "Response")) {varorder2.3$l <- input$rheatmaporder}
  else {varorder2.3$l <- paste0(input$rheatmaporder, "_s")} 
})


##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowseorder
varnames2.3 <- reactiveValues(df=NULL)


##order datasets
stratify_vars2.3 <- reactiveValues(df = NULL, df2 = NULL)

##Default datasets
observeEvent(input$go, priority = -1, {
  
  stratify_vars2.3$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
  stratify_vars2.3$df2 <- stratify_vars2.3$df
  
  varnames2.3$df <- names(stratify_vars$df_full)
  
  
})


##create the appropriate stratify_vars2.3$df 
observeEvent(input$rheatmap1, {
  
  stratify_vars2.3$df2 <- 
  
  if(!input$rheatmaporder %in% c("ID", "None")) {
    stratify_vars2.3$df <- stratify_vars$df_full %>% select_("ID", input$rheatmaporder) %>%
      group_by(ID) %>%
      summarise_at(input$rheatmaporder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                  ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup() 
  }
  
  else {stratify_vars2.3$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
  
})

##Color vars dataset, upon action button, stratify_vars2.2$df2 will update based on variable and quantile selection

observeEvent(input$rheatmap1, ignoreInit = T, {
  
  stratify_vars2.3$df2 <-  
    
    
    if(input$rheatmaporder %in% c("ID", "None", "Response")) {stratify_vars2.3$df}
  
    else {stratify_vars2.3$df %>% 
          rename_at(vars(input$rheatmaporder), ~ paste0(input$rheatmaporder, "_s"))
      
    }
})

#Random plot

#make plot update after new variables are selected

rheatmapvariables <- reactiveValues(var = "ID", order = "Response")

rheatmaprandbutton <- reactiveValues(r1=NULL, r2=NULL)

observeEvent(input$rheatmaprandom, priority = 2, ignoreInit = T, {
  
  rheatmaprandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  rheatmaprandbutton$r2 <- sample(1:length(varnames2.3$df), 1)
  
  
  if("Main Var." %in% input$rheatmaprand_choice) {rheatmapvariables$var <- names(dataset())[[rheatmaprandbutton$r1]]}
  
  if("order Var." %in% input$rheatmaprand_choice) {rheatmapvariables$order <- varnames2.3$df[[rheatmaprandbutton$r2]]}
  
})


##Data for response boxplots

rheatmapdata <- reactiveValues(l=NULL, m=NULL)

observeEvent(input$rheatmap1, ignoreInit = T, {
  
  #Main heatmap
  rheatmapdata$l <- 
    
    if(input$rheatmaporder %in% "None"){
    
      dataset() %>% 
      select_("ID", "timepoint", input$rheatmapvar) %>%
      group_by_("ID") %>%
      summarise_all(mymean) %>%
      left_join(stratify_vars2.3$df2, by="ID")
      
    }
  
    else if (input$heatmaporder %in% "Response") {
      
      dataset() %>% 
        select_("ID", "timepoint", input$rheatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID") %>%
        arrange(input$rheatmapvar)
    }
      
    else {
      
      dataset() %>% 
        select_("ID", "timepoint", input$rheatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID") %>%
        arrange(input$rheatmaporder, input$rheatmapvar)
    }
  
  #time of day heatmap
  rheatmapdata$m <- 
    
    if (input$rheatmapraw %in% "raw") {
     
       dataset() %>% 
        select_("ID", "timeofday", "weekday", input$rheatmapvar) %>%
        group_by_("ID", input$rheatmapstrat) %>%
        summarise_all(mymean) %>%
        left_join(stratify_vars2.3$df2, by="ID")
    }
  
   else if (input$rheatmapraw %in% "subject normalized") {
    
      dataset() %>% 
      select_("ID", "timepoint", "timeofday", input$rheatmapvar) %>%
      group_by_("ID") %>%
      mutate_at(input$rheatmapvar, .funs = funs(normalize)) %>%
      ungroup() %>%
      group_by_("ID", "timeofday") %>%
      summarise_all(mymean) %>%
      left_join(stratify_vars2.3$df2, by="ID")
     
   }
})



#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

rheatmap_dummy <- reactiveValues(l=0)

observeEvent(input$rheatmap1, ignoreInit = T, {
  if (rheatmap_dummy$l==0) {rheatmap_dummy$l <- 1}
  else NULL
})

##Reset plot when dataset changes

observeEvent(input$go, {
  rheatmap_dummy$l <- 0
  
})

##Instructions that appear before create plot button is clicked

output$rheatmap_instr <-  renderText(
  if(rheatmap_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)


##Main boxplot code
output$heatmapR <- renderPlotly({
  
  input$rheatmap1
  
  isolate(
    
    if(rheatmap_dummy$l==0) NULL
    
    else {
      
      if (varorder2.3$l  %in% c("ID_s", "None_s", "Response_s")) {
            
            ggplotly(ggplot(rheatmapdata$l) + 
                       geom_tile(aes_string(y="ID", x=factor("All"), fill=input$heatmapvar)) +
                       labs(y="", x="", title=input$heatmapvar, fill="") +
                       scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                       scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$l$ID")))) +
                       theme_bw() +
                       theme(plot.margin = margin(t = 30, b = 10),
                             axis.text.y = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.title.y = element_blank(),
                             legend.title = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank(),
                             panel.grid = element_blank())
            )
          }
          
          else {
            
            ggplotly(ggplot(rheatmapdata$l, aes_string(label=varorder2.3)) + 
                       geom_tile(aes_string(y="ID", x=factor("All"), fill=input$heatmapvar)) +
                       labs(y="", x="", title=input$heatmapvar, fill="") +
                       scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                       scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$l$ID")))) +
                       theme_bw() +
                       theme(plot.margin = margin(t = 30, b = 10),
                             axis.text.y = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.title.y = element_blank(),
                             legend.title = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank(),
                             panel.grid = element_blank())
                     
            )
          } 
    }
  )
})
        
        
##Code for timeofday/weekday heatmap
output$heatmapTODR <- renderPlotly({

  input$rheatmap1
  
  isolate(
    
    if(rheatmap_dummy$l==0) NULL
    
    else {
      
      if (varorder2.3$l  %in% c("ID_s, None_s", "Response_s")) {
    
        gplotly(ggplot(rheatmapdata$m) + 
                       geom_tile(aes_string(x=input$heatstratR, y="ID", fill=input$heatmapvar)) +
                       labs(x="", y="Subject ID", title=paste0(input$heatmapvar, " ", input$heatstratR), fill="") +
                       scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                       scale_x_discrete(limits = levels(eval(parse(text=paste0("rheatmapdata$m$", input$heatstratR))))) +
                       scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$m$ID")))) +
                       theme_bw() +
                       theme(plot.margin = margin(t = 30, b = 10),
                             axis.text.y = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.title.y = element_blank(),
                             legend.title = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank(),
                             panel.grid = element_blank())
                     
            ) 
          }
          
          else {
            
            ggplotly(ggplot(heatdataR(), aes_string(label=input$heatorder)) + 
                       geom_tile(aes_string(x=input$heatstratR, y="ID", fill=input$heatmapvar)) +
                       labs(x="", y="Subject ID", title=paste0(input$heatmapvar, " ", input$heatstratR), fill="") +
                       scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                       scale_x_discrete(limits = levels(eval(parse(text=paste0("rheatmapdata$m$", input$heatstratR))))) +
                       scale_y_discrete(limits=eval(parse(text=paste0("rheatmapdata$m$ID")))) +
                       theme_bw() +
                       theme(plot.margin = margin(t = 30, b = 10),
                             axis.text.y = element_blank(),
                             axis.ticks.x = element_blank(),
                             axis.ticks.y = element_blank(),
                             axis.title.y = element_blank(),
                             legend.title = element_blank(),
                             panel.border = element_blank(),
                             panel.background = element_blank(),
                             panel.grid = element_blank())
          )
          }
    }
  )
})