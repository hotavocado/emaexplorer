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
output$boxplot_dR <- renderPlotly({
  
  input$rheatmap1
  
  isolate(
    
    if(rheatmap_dummy$l==0) NULL
    
    else {
      
      if (varcolor2.3$l  %in% "None_s") {
        
        ggplotly(ggplot(data=rheatmapdata$l, aes_string(x=factor(0),
                                                        y=input$rheatmapvar,
                                                        label="ID"))+ 
                   geom_boxplot(size=.5, fatten=1)+
                   geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                   stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                position=position_dodge(width=0.75), shape=4, color="firebrick")+
                   labs(y=NULL, x=NULL, title=input$rheatmapvar)+
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
        
        ggplotly(ggplot(data=rheatmapdata$l, aes_string(x=varcolor2.3$l ,
                                                        y=input$rheatmapvar,
                                                        label="ID", color=varcolor2.3$l))+ 
                   geom_boxplot(size=.5, fatten=1)+
                   geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                   stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                                position=position_dodge(width=0.75), shape=4, color="firebrick")+
                   #stat_summary(fun.data = give.n, geom = "text", color="firebrick", size=4) +
                   labs(y=NULL, x=NULL, title=input$rheatmapvar)+
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
  
  input$rheatmap1
  
  isolate(
    
    if(rheatmap_dummy$l==0) NULL
    
    else {
      
      ggplotly(ggplot(data=rheatmapdata$m, aes_string(x="timeofday", 
                                                      y=input$rheatmapvar, 
                                                      color="timeofday", label="ID"))+
                 geom_boxplot(size=.5, fatten=1)+
                 geom_jitter(alpha=0.2, size=1.2, shape=19, position = position_jitter(w = 0.5, h = 0.1))+
                 stat_summary(fun.data = mean_cl_normal, geom="pointrange", size=2, alpha=.8, 
                              position=position_dodge(width=0.75), shape=4, color="firebrick")+
                 labs(y=NULL, x=NULL, title="")+
                 scale_x_discrete(limits = rev(levels(rheatmapdata$m[["timeofday"]])))+
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