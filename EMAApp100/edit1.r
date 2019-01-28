#Compliance Page 2, boxplot of compliance-----------------------------------------------------------------------------------------------------

##data table for boxplot
output$table1 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))


#Select main variable
output$boxplotvar <- renderUI({selectInput('boxplotvar', 'Main Variable:', c(names(dataset())), selected = boxplotvariables$var, selectize=TRUE)})



#Select coloring variable
output$boxplotcolor <- renderUI({selectInput('boxplotcolor', 'Color By:', c("None", varnames1.2$df), selected = boxplotvariables$color, selectize=TRUE)})


#boxplot color variable
varcolor1.2 <- reactiveValues(l="ID")

observeEvent(input$boxplot1, {if (input$boxplotcolor %in% c("ID", "None")) {varcolor1.2$l <- input$boxplotcolor}
  else {varcolor1.2$l <- paste0(input$boxplotcolor, "_s")} 
})

# output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})


##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
varnames1.2 <- reactiveValues(df=NULL)


##Color datasets
stratify_vars1.2 <- reactiveValues(df = NULL, df2 = NULL)

##Default datasets
observeEvent(input$go, priority = -1, {
  
  stratify_vars1.2$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
  stratify_vars1.2$df2 <- stratify_vars1.2$df
  
  varnames1.2$df <- names(stratify_vars$df_full)
  
  
})


##create the appropriate stratify_vars1.2$df 
observeEvent(input$boxplot1, {
  
  if(!input$boxplotcolor %in% c("ID", "None")) {
    stratify_vars1.2$df <- stratify_vars$df_full %>% select_("ID", input$boxplotcolor) %>%
      group_by(ID) %>%
      summarise_at(input$boxplotcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                  ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup() 
  }
  
  else {stratify_vars1.2$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
  
})

##Color vars dataset, upon action button, stratify_vars1.2$df2 will update based on variable and quantile selection

observeEvent(input$boxplot1, ignoreInit = T, {
  
  stratify_vars1.2$df2 <-  
    
    
    if(input$boxplotcolor %in% c("ID", "None")) {stratify_vars1.2$df}
  
  else {
    if(input$boxplotradio %in% "Auto"){
      stratify_vars1.2$df %>% 
        mutate_at(input$boxplotcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {my_ntiles(x, input$boxplotntile)}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {factor(x, ordered = T, exclude = c(NA, "NaN"))}
          else {x=NA}
        }) %>% 
        rename_at(vars(input$boxplotcolor), ~ paste0(input$boxplotcolor, "_s"))
    }
    
    else if (input$boxplotradio %in% "On"){
      stratify_vars1.2$df %>% 
        mutate_at(input$boxplotcolor, ~my_ntiles(.x, input$boxplotntile)) %>%
        rename_at(vars(input$boxplotcolor), ~ paste0(input$boxplotcolor, "_s"))
      
    }
    
    else if (input$boxplotradio %in% "Off"){
      stratify_vars1.2$df %>% 
        rename_at(vars(input$boxplotcolor), ~ paste0(input$boxplotcolor, "_s"))
      
    }
  }
  
})


#Random plot

#make plot update after new variables are selected

boxplotvariables <- reactiveValues(var = "ID", color = "None")

#Default boxplot variable
observeEvent(input$go, priority = -1, {
  
  boxplotvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
  
})

boxplotrandbutton <- reactiveValues(r1=NULL, r2=NULL)

observeEvent(input$boxplotrandom, priority = 2, ignoreInit = T, {
  
  boxplotrandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  boxplotrandbutton$r2 <- sample(1:length(varnames1.2$df), 1)
  
  
  if("Main Var." %in% input$boxplotrand_choice) {boxplotvariables$var <- names(dataset())[[boxplotrandbutton$r1]]}
  
  if("Color Var." %in% input$boxplotrand_choice) {boxplotvariables$color <- varnames1.2$df[[boxplotrandbutton$r2]]}
  
})


##Data for response boxplots

boxplotdata <- reactiveValues(l=NULL, m=NULL)

observeEvent(input$boxplot1, ignoreInit = T, {
  
  
  boxplotdata$l <- 
    dataset() %>% 
    select_("ID", "timepoint", input$boxplotvar) %>%
    group_by_("ID") %>%
    summarise_all(countna) %>%
    left_join(stratify_vars1.2$df2, by="ID")
  
  #time of day boxplot
  boxplotdata$m <- 
    dataset() %>% 
    select_("ID", "timepoint", "timeofday", input$boxplotvar) %>%
    group_by_("ID", "timeofday") %>%
    summarise_all(countna)
  
})



#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

boxplot_dummy <- reactiveValues(l=0)

observeEvent(input$boxplot1, ignoreInit = T, {
  if (boxplot_dummy$l==0) {boxplot_dummy$l <- 1}
  else NULL
})

##Reset plot when dataset changes

observeEvent(input$go, {
  boxplot_dummy$l <- 0
  
})

##Instructions that appear before create plot button is clicked

output$boxplot_instr <-  renderText(
  if(boxplot_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)


##Main boxplot code
output$boxplot_dR <- renderPlotly({
  
  input$boxplot1
  
  isolate(
    
    if(boxplot_dummy$l==0) NULL
    
    else {
      
      if (varcolor1.2$l  %in% "None") {
        
        ggplotly(ggplot(data=boxplotdata$l, aes_string(x=factor(0),
                                                        y=input$boxplotvar,
                                                        label="ID"))+ 
                   geom_boxplot(size=.5, fatten=1)+
                   geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                   stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                position=position_dodge(width=0.75), shape=4, color="firebrick")+
                   labs(y=NULL, x=NULL, title=input$boxplotvar)+
                   theme_bw()+
                   theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                         axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                         axis.text.y = element_blank(),
                         axis.line = element_line(color="gray65", size=0.5),
                         axis.ticks.y = element_blank(),
                         axis.ticks.x = element_line(colour = "gray"),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(), 
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         #plot.margin = unit( c(0,3,3,0) , "in"),
                         aspect.ratio = 0.3) +
                   #legend.position="none")+
                   coord_flip()
                 #coord_flip(ylim=c(0, max(boxdata_dR()[4])), expand = c(0.1,0))+
                 #scale_y_continuous(breaks=seq(0, max(boxdata_dR()[2]), by = 4))
        )
      }
      
      else {  
        
        ggplotly(ggplot(data=boxplotdata$l, aes_string(x=varcolor1.2$l ,
                                                        y=input$boxplotvar,
                                                        label="ID", color=varcolor1.2$l))+ 
                   geom_boxplot(size=.5, fatten=1)+
                   geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                   stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                position=position_dodge(width=0.75), shape=4, color="firebrick")+
                   #stat_summary(fun.data = give.n, geom = "text", color="firebrick", size=4) +
                   labs(y=NULL, x=NULL, title=input$boxplotvar)+
                   theme_bw()+
                   theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                         axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                         axis.text.y = element_text(size=10),
                         axis.line = element_line(color="gray65", size=0.5),
                         axis.ticks.y = element_blank(),
                         axis.ticks.x = element_line(colour = "gray"),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(), 
                         panel.background = element_blank(),
                         panel.border = element_blank(),
                         plot.margin = unit( c(0.5,0.5,0.5,1) , "cm"),
                         aspect.ratio = 0.3)+
                   #legend.position="none")+
                   coord_flip()
                 #coord_flip(ylim=c(0, max(boxdata_dR()[4])), expand = c(0.1,0))+
                 #scale_y_continuous(breaks=seq(0, max(boxdata_dR()[2]), by = 4))
        )
        
        
      }
    }
    
  )
  
})


##Timeofday boxplot code

output$boxplotR <- renderPlotly({
  
  input$boxplot1
  
  isolate(
    
    if(boxplot_dummy$l==0) NULL
    
    else {
      
      ggplotly(ggplot(data=boxplotdata$m, aes_string(x="timeofday", 
                                                      y=input$boxplotvar, 
                                                      color="timeofday", label="ID"))+
                 geom_boxplot(size=.5, fatten=1)+
                 geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                 stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                              position=position_dodge(width=0.75), shape=4, color="firebrick")+
                 labs(y=NULL, x=NULL, title="")+
                 scale_x_discrete(limits = rev(levels(boxplotdata$m[["timeofday"]])))+
                 theme_bw()+
                 theme(axis.title.x=element_blank(), axis.title.y=element_text(size=8), 
                       axis.text.x = element_text(size=8, vjust = 1, color="gray65"), 
                       axis.text.y = element_text(size=10),
                       axis.line = element_line(color="gray65", size=0.5),
                       axis.ticks.y = element_blank(),
                       axis.ticks.x = element_line(colour = "gray"),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(), 
                       panel.background = element_blank(),
                       panel.border = element_blank(),
                       #plot.margin = unit( c(0,3,3,0) , "in"),
                       aspect.ratio = 0.2,
                       legend.position="none") +
                 coord_flip()
               # scale_y_continuous(breaks=c(0:max(boxdataR()[3])))
      )
      
    }
    
  )
  
})

      
      
      