#Compliances Page 3, heatmap of Compliances-----------------------------------------------------------------------------------------------------

##datatable for heatmaps
output$table2 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))


#Select main variable
output$heatmapvar <- renderUI({selectInput('heatmapvar', 'Main Variable:', c(names(dataset())), selected = heatmapvariables$var, selectize=TRUE)})



#Select ordering variable
output$heatmaporder <- renderUI({selectInput('heatmaporder', 'Order By:', c("None", "Compliance", varnames1.4$df), selected = heatmapvariables$order, selectize=TRUE)})


#heatmap order variable
varorder1.4 <- reactiveValues(l="ID")

observeEvent(input$heatmap1, {if (input$heatmaporder %in% c("ID", "None", "Compliance")) {varorder1.4$l <- input$heatmaporder}
  else {varorder1.4$l <- paste0(input$heatmaporder, "_s")} 
})


##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowseorder
varnames1.4 <- reactiveValues(df=NULL)


##order datasets
stratify_vars1.4 <- reactiveValues(df = NULL, df2 = NULL)

##Default datasets
observeEvent(input$go, priority = -1, {
  
  stratify_vars1.4$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
  stratify_vars1.4$df2 <- stratify_vars1.4$df
  
  varnames1.4$df <- names(stratify_vars$df_full)
  
  
})


##create the appropriate stratify_vars1.4$df 
observeEvent(input$heatmap1, {
  
  if(!input$heatmaporder %in% c("ID", "None", "Compliance")) {
    stratify_vars1.4$df <- stratify_vars$df_full %>% select_("ID", input$heatmaporder) %>%
      group_by(ID) %>%
      summarise_at(input$heatmaporder, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                  ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup() 
  }
  
  else {stratify_vars1.4$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
  
})

##Color vars dataset, upon action button, stratify_vars2.2$df2 will update based on variable and quantile selection

observeEvent(input$heatmap1, ignoreInit = T, {
  
  stratify_vars1.4$df2 <-  
    
    if(input$heatmaporder %in% c("None", "Compliance")) {stratify_vars1.4$df}
  
  else {stratify_vars1.4$df %>% 
      rename_at(vars(input$heatmaporder), ~ paste0(input$heatmaporder, "_s"))
    
  }
})

#Random plot

#make plot update after new variables are selected
heatmapvariables <- reactiveValues(var = "ID", order = "Compliance")

#default heatmap var
observeEvent(input$go, priority = -1, {
  
  heatmapvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
  
})



heatmaprandbutton <- reactiveValues(r1=NULL, r2=NULL)

observeEvent(input$heatmaprandom, priority = 2, ignoreInit = T, {
  
  heatmaprandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  heatmaprandbutton$r2 <- sample(1:length(varnames1.4$df), 1)
  
  
  if("Main Var." %in% input$heatmaprand_choice) {heatmapvariables$var <- names(dataset())[[heatmaprandbutton$r1]]}
  
  if("Order Var." %in% input$heatmaprand_choice) {heatmapvariables$order <- varnames1.4$df[[heatmaprandbutton$r2]]}
  
})


##Data for Compliance boxplots

heatmapdata <- reactiveValues(l=NULL, m=NULL)

observeEvent(input$heatmap1, ignoreInit = T, {
  
  #Main heatmap
  
  heatmapdata$l <-   
    
    if(input$heatmaporder %in% "None"){
      
      dataset() %>% 
        select_("ID", "timepoint", input$heatmapvar) %>%
        group_by_("ID") %>%
        summarise_all(countna) %>%
        left_join(stratify_vars1.4$df2, by="ID")
      
    }
  
  else if (input$heatmaporder %in% "Compliance") {
    
    dataset() %>% 
      select_("ID", "timepoint", input$heatmapvar) %>%
      group_by_("ID") %>%
      summarise_all(countna) %>%
      left_join(stratify_vars1.4$df2, by="ID") %>%
      arrange_(input$heatmapvar)
  }
  
  else {
    
    dataset() %>% 
      select_("ID", "timepoint", input$heatmapvar) %>%
      group_by_("ID") %>%
      summarise_all(countna) %>%
      left_join(stratify_vars1.4$df2, by="ID") %>%
      arrange_(varorder1.4$l, input$heatmapvar)
  }
  
  #Time of day plot
  
  heatmapdata$m <- 
    
    if (input$heatmapraw %in% "raw") {
      
      dataset() %>% 
        select_("ID", "timeofday", "weekday", input$heatmapvar) %>%
        group_by_("ID", input$heatmapstrat) %>%
        summarise_all(countna) %>%
        left_join(stratify_vars1.4$df2, by="ID")
    }
  
  else if (input$heatmapraw %in% "subject normalized") {
    
    if (input$heatmapvar %in% "ID") {
      
      dataset() %>% 
        select_("ID", "timeofday", "weekday", input$heatmapvar) %>%
        group_by_("ID", input$heatmapstrat) %>%
        summarise_all(countna) %>%
        left_join(stratify_vars1.4$df2, by="ID")
    }
    
    else {
      
      dataset() %>% 
        select_("ID", "timeofday", "weekday", input$heatmapvar) %>%
        group_by_("ID") %>%
        mutate_at(input$heatmapvar, .funs = funs(normalize)) %>%
        ungroup() %>%
        group_by_("ID", input$heatmapstrat) %>%
        summarise_all(countna) %>%
        left_join(stratify_vars1.4$df2, by="ID")
    }
  }
  
})


#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

heatmap_dummy <- reactiveValues(l=0)

observeEvent(input$heatmap1, ignoreInit = T, {
  if (heatmap_dummy$l==0) {heatmap_dummy$l <- 1}
  else NULL
})

##Reset plot when dataset changes

observeEvent(input$go, {
  heatmap_dummy$l <- 0
  
})

##Instructions that appear before create plot button is clicked

output$heatmap_instr <-  renderText(
  if(heatmap_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)

output$heatmap <- renderPlotly({
  
  input$heatmap1
  
  isolate(
    
    if(heatmap_dummy$l==0) NULL
    
    else {
      
      if (varorder1.4$l  %in% c("None", "Compliance")) {
        
        ggplotly(ggplot(heatmapdata$l) + 
                   geom_tile(aes_string(y="ID", x=factor("All"), fill=input$heatmapvar)) +
                   labs(y="", x="", title=input$heatmapvar, fill="") +
                   scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                   scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$l$ID")))) +
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
        
        ggplotly(ggplot(heatmapdata$l, aes_string(label=varorder1.4$l)) + 
                   geom_tile(aes_string(y="ID", x=factor("All"), fill=input$heatmapvar)) +
                   labs(y="", x="", title=input$heatmapvar, fill="") +
                   scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                   scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$l$ID")))) +
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
output$heatmapTOD <- renderPlotly({
  
  input$heatmap1
  
  isolate(
    
    if(heatmap_dummy$l==0) NULL
    
    else {
      
      if (varorder1.4$l  %in% c("None", "Compliance")) {
        
        ggplotly(ggplot(heatmapdata$m) + 
                   geom_tile(aes_string(x=input$heatmapstrat, y="ID", fill=input$heatmapvar)) +
                   labs(x="", y="Subject ID", title=paste0(input$heatmapvar, " ", input$heatmapstrat), fill="") +
                   scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                   scale_x_discrete(limits = levels(eval(parse(text=paste0("heatmapdata$m$", input$heatmapstrat))))) +
                   scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$m$ID")))) +
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
        
        ggplotly(ggplot(heatmapdata$m, aes_string(label=varorder1.4$l)) + 
                   geom_tile(aes_string(x=input$heatmapstrat, y="ID", fill=input$heatmapvar)) +
                   labs(x="", y="Subject ID", title=paste0(input$heatmapvar, " ", input$heatmapstrat), fill="") +
                   scale_fill_distiller(palette = "RdYlGn", direction = 1) +
                   scale_x_discrete(limits = levels(eval(parse(text=paste0("heatmapdata$m$", input$heatmapstrat))))) +
                   scale_y_discrete(limits=eval(parse(text=paste0("heatmapdata$m$ID")))) +
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



