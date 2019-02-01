#Responses Page 4, trajectory of responses ------------------------------------------------------------------------------


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
output$rtrajvar <- renderUI({selectInput('rtrajvar', 'Main Variable:', c(names(dataset())), selected = rtrajvariables$var, selectize=TRUE)})


#Select coloring variable
output$rtrajcolor <- renderUI({selectInput('rtrajcolor', 'Color By:', c("None", varnames2.4$df), selected = rtrajvariables$color, selectize=TRUE)})


#traj color variable
varcolor2.4 <- reactiveValues(l="ID")

observeEvent(input$rtraj1, {if (input$rtrajcolor %in% c("ID", "None")) {varcolor2.4$l <- input$rtrajcolor}
  else {varcolor2.4$l <- paste0(input$rtrajcolor, "_s")} 
})

# output$boxselectR <- renderUI({selectInput('boxselectR', 'Stratify by:', c("None", varnames$l), selectize=TRUE)})


##Use varnames so updating stratify_vars3.1$df2 doesn't refresh subbrowsecolor
varnames2.4 <- reactiveValues(df=NULL)


##Color datasets
stratify_vars2.4 <- reactiveValues(df = NULL, df2 = NULL)

##Default color datasets
observeEvent(input$go, priority = -1, {
  
  stratify_vars2.4$df <- stratify_vars$df_sub %>% select("ID") %>% mutate(dummy_s=1)
  stratify_vars2.4$df2 <- stratify_vars2.4$df
  
  varnames2.4$df <- names(stratify_vars$df_full)
  
  
})


##create the appropriate stratify_vars2.4$df 
##create the appropriate stratify_vars2.2$df 
observeEvent(input$rtraj1, {
  
  if(!input$rtrajcolor %in% c("ID", "None")) {
    stratify_vars2.4$df <- stratify_vars$df_full %>% select_("ID", input$rtrajcolor) %>%
      group_by(ID) %>%
      summarise_at(input$rtrajcolor, function(x) { y <- ifelse(is.character(x) | is.factor(x), getmode(x),
                                                                  ifelse(is.numeric(x), mean(x, na.rm = T), NA))
      return(y)}) %>% 
      ungroup() 
  }
  
  else {stratify_vars2.4$df <- stratify_vars$df_sub %>% select_("ID") %>% mutate(dummy_s = 1)}
  
})

##Color vars dataset, upon action button, stratify_vars2.4$df2 will update based on variable and quantile selection

observeEvent(input$rtraj1, ignoreInit = T, {
  
  stratify_vars2.4$df2 <-  
    
    
    if(input$rtrajcolor %in% c("ID", "None")) {stratify_vars2.4$df}
  
  else {
    if(input$rtrajradio %in% "Auto"){
      stratify_vars2.4$df %>% 
        mutate_at(input$rtrajcolor, function (x) { if (is.character(x) | is.factor(x)) {x}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) > 20) {as.character(my_ntiles(x, input$rtrajntile))}
          else if ((is.numeric(x) | is.integer(x)) & length(unique(x)) <= 20) {as.character(factor(x, ordered = T, exclude = c(NA, "NaN")))}
          else {x=NA}
        }) %>% 
        rename_at(vars(input$rtrajcolor), ~ paste0(input$rtrajcolor, "_s"))
    }
    
    
    
    else if (input$rtrajradio %in% "On"){
      stratify_vars2.4$df %>% 
        mutate_at(input$rtrajcolor, ~my_ntiles(.x, input$rtrajntile)) %>%
        rename_at(vars(input$rtrajcolor), ~ paste0(input$rtrajcolor, "_s"))
      
    }
    
    else if (input$rtrajradio %in% "Off"){
      stratify_vars2.4$df %>% 
        mutate_at(input$rtrajcolor, as.character) %>%
        rename_at(vars(input$rtrajcolor), ~ paste0(input$rtrajcolor, "_s"))
      
    }
  }
  
})



#Random plot

#make plot update after new variables are selected

rtrajvariables <- reactiveValues(var = "ID", color = "weekday")


#Default traj variable
observeEvent(input$go, priority = -1, {
  
  rtrajvariables$var <- names(dataset())[[sample(1:length(names(dataset())), 1)]]
  
})

rtrajrandbutton <- reactiveValues(r1=NULL)

observeEvent(input$rtrajrandom, priority = 2, ignoreInit = T, {
  
  rtrajrandbutton$r1 <- sample(1:length(names(dataset())), 1)
  
  rtrajrandbutton$r2 <- sample(1:length(varnames2.2$df), 1)
  
  
  if("Main Var." %in% input$rtrajrand_choice) {rtrajvariables$var <- names(dataset())[[rtrajrandbutton$r1]]}
  
  if("Color Var." %in% input$rtrajrand_choice) {rtrajvariables$color <- varnames2.4$df[[rtrajrandbutton$r2]]}
  
})


##Data for response trajs

rtrajdata <- reactiveValues(l=NULL, m=NULL)


observeEvent(input$rtraj1, ignoreInit = T, {

  
 if (input$trajraw %in% "Raw" | input$trajvar %in% "ID") {
  
    rtrajdata$l <- 
    dataset() %>% 
    select_("ID", "timepoint", "weekday", "weekday_n", "weektime_n", "day",  input$rtrajvar) %>%
    left_join(stratify_vars2.4$df2, by="ID") 
 
 }
  
 else if (input$trajraw %in% "Subject Normalized") {
   
   rtrajdata$l <- 
     dataset() %>% 
     select_("ID", "timepoint", "weekday", "weekday_n", "weektime_n", "day",  input$rtrajvar) %>%
     group_by("ID") %>%
     mutate_at(input$trajvar, funs(normalize)) %>%
     ungroup() %>%
     left_join(stratify_vars2.4$df2, by="ID") 
 }
  
  
})

#output$test1 <- renderText(names(rtrajdata$m))


#dummy variable to control plot, since plot runs automatically when page is selected, will make it dependent on a reactive value that updates 
#once the "create plot" button is clicked.

rtraj_dummy <- reactiveValues(l=0)

observeEvent(input$rtraj1, ignoreInit = T, {
  if (rtraj_dummy$l==0) {rtraj_dummy$l <- 1}
  else NULL
})

##Reset plot when dataset changes

observeEvent(input$go, {
  rtraj_dummy$l <- 0
  
})

##Instructions that appear before create plot button is clicked

output$rtraj_instr <-  renderText(
  if(rtraj_dummy$l==0) {"Click the [Create/Update Plot] button to generate plot!"}
  else NULL 
)

##code for trajogram

output$trajR <- renderPlotly({ 
  
  input$rtraj1
  
  
  isolate(
    
    if(rtraj_dummy$l==0) NULL
    
    else {
      
    if(input$rtrajplottype %in% "Group Means") {
      
      if(input$rtrajaxis %in% "weekday_n") {
        
        if(varcolor2.4$l %in% c("ID", "None")) {
          
          ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, group = "weekday")) +
                     labs(title = input$rtrajvar) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8, alpha = 0.7)+
                     stat_summary(fun.y = mean,
                                  geom = "line", size = 1, alpha = 0.7) +
                     
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 14) +
                     theme(panel.grid = element_blank()))
        }
        
        else{
          
          ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, color = input$rtrajcolor)) +
                     labs(title = input$rtrajvar) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8, alpha = 0.7)+
                     stat_summary(fun.y = mean,
                                  geom = "line", size = 1,  alpha = 0.7, mapping = aes_string(linetype = "weekday")) +
                     
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 14) +
                     theme(panel.grid = element_blank()))
          
          
          
        }
      }
      
      else {
        
        if(varcolor2.4$l %in% c("ID", "None")) {
          
          ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar)) +
                     labs(title = input$rtrajvar) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8, alpha = 0.7)+
                     stat_summary(fun.y = mean,
                                  geom = "line", size = 1, alpha = 0.7) +
                     
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 14) +
                     theme(panel.grid = element_blank()))
        }
        
        else{
          
          ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, color = input$rtrajcolor)) +
                     labs(title = input$rtrajvar) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8, alpha = 0.7)+
                     stat_summary(fun.y = mean,
                                  geom = "line", size = 1, alpha = 0.7) +
                     
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 14) +
                     theme(panel.grid = element_blank()))
          
        }
      }
    }
      
    else if (input$rtrajplottype %in% "Subject Traces") {
      
      if(varcolor2.4$l %in% c("ID", "None")) {
        
        ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, group = "ID")) +
                   labs(title = input$rtrajvar) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   stat_summary(fun.y = mean,
                                geom = "line", size = 0.5, alpha = 0.2) +
                   
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 14) +
                   theme(panel.grid = element_blank()))
      }
      
      else{
        
        ggplotly(ggplot(rtrajdata$l, aes_string(x=input$rtrajaxis, y=input$rtrajvar, group = "ID", color = input$rtrajcolor)) +
                   labs(title = input$rtrajvar) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   stat_summary(fun.y = mean,
                                geom = "line", size = 0.5,  alpha = 0.2) +
                   
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 14) +
                   theme(panel.grid = element_blank()))
        
      
      }
    }
  }
      
  )
  
})



