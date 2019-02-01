#Responses Page 4, Trajectory of responses-----------------------------------------------------------------------------------------------------

##Table for Trajectory Plots
output$table6 <- DT::renderDataTable(data(), selection = list(selected = 8, mode = 'single'),
                                     options = list(columnDefs = list(list(
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 12 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 12) + '...</span>' : data;",
                                         "}")
                                     ))), callback = JS('table.page(3).draw(false);'))



##Data for trajectory
Trajdata <- reactive({withProgress(message = 'Loading Data for Plots', {
  
  if (input$trajnorm %in% "raw") {
    dataset() %>% 
      select_(names(dataset())[[1]], "timepoint", "weekday_n", "weekday", "weektime_n", names(dataset())[[input$table6_rows_selected]], "timeofday") %>%
      mutate_at(names(dataset())[[input$table6_rows_selected]], .funs = funs(as.numeric)) %>%
      left_join(dataset_vars())
    
  }
  ##Data for subject normalized trajectory
  else {
    dataset() %>% 
      select_(names(dataset())[[1]], "timepoint", "weekday_n", "weekday", "weektime_n", names(dataset())[[input$table6_rows_selected]], "timeofday") %>%
      group_by_(names(dataset())[[1]]) %>%
      mutate_at(names(dataset())[[input$table6_rows_selected]], .funs = funs(normalize)) %>%
      left_join(dataset_vars())
    
    
  }
})
})


##Choose which traces to visualize:
output$trajcheck <- renderUI({checkboxGroupInput("trajcheck", "View Groups:", choices = c(as.character(unique(dataset_vars()[[input$trajselectR]]))), selected = c(as.character(unique(dataset_vars()[[input$trajselectR]]))))})

##Choose group to stratify by:
output$trajselectR <- renderUI({selectInput('trajselectR', 'Stratify by:', c("None", names(dataset_vars())[2:ncol(dataset_vars())]), selectize=TRUE)})

##Subset data based on checked
Trajdatasub <- reactive({Trajdata() %>% filter_(interp(~v %in% input$trajcheck , v=as.name(input$trajselectR)))})


##Data for traces plots

###For within day:

Trajdata_Day <- reactive({Trajdata() %>% 
    group_by_(names(Trajdata())[[1]], "timepoint") %>%
    summarise_at(names(Trajdata())[[6]], mymean) %>%
    ungroup() %>%
    left_join(dataset_vars())
})

Trajdata_Daysub <- reactive({Trajdata_Day() %>% filter_(interp(~v %in% input$trajcheck , v=as.name(input$trajselectR)))})

###For within day, day level

varval <- reactive ({interp(~paste0(y, "; day ", z) , .values = list(y = as.name(names(dataset())[[1]]), z = as.name("day")))})

Trajdata_Day2 <- reactive({
  
  if (input$trajnorm %in% "raw") {
    dataset() %>% 
      select_(names(dataset())[[1]], "day", "timepoint", "weekday_n", "weekday", "weektime_n", names(dataset())[[input$table6_rows_selected]]) %>%
      mutate_(.dots = setNames(list(varval()), "ID_day")) %>%
      left_join(dataset_vars())
  }
  
  else {
    dataset() %>% 
      select_(names(dataset())[[1]], "day", "timepoint", "weekday_n", "weekday", "weektime_n", names(dataset())[[input$table6_rows_selected]]) %>%
      group_by_(names(dataset())[[1]]) %>%
      mutate_at(names(dataset())[[input$table6_rows_selected]], .funs = funs(normalize)) %>%
      ungroup() %>%
      mutate_(.dots = setNames(list(varval()), "ID_day")) %>%
      left_join(dataset_vars())
  }
  
})

Trajdata_Daysub2 <- reactive({Trajdata_Day2() %>% filter_(interp(~v %in% input$trajcheck , v=as.name(input$trajselectR)))})



###For within week:

Trajdata_Week <- reactive({Trajdata() %>% 
    group_by_(names(Trajdata())[[1]], "weekday_n") %>%
    summarise_at(names(Trajdata())[[6]], mymean) %>%
    ungroup() %>%
    left_join(dataset_vars())
})

Trajdata_Weeksub <- reactive({Trajdata_Week() %>% filter_(interp(~v %in% input$trajcheck , v=as.name(input$trajselectR)))})

###For 28 timepoints:

trajweek <- reactive({Trajdata() %>% 
    group_by_(names(Trajdata())[[1]], "weektime_n") %>%
    summarise(weekday = unique(weekday), timeofday = timeofday[[1]])})

Trajdata_Weektime <- reactive({Trajdata() %>% 
    group_by_(names(Trajdata())[[1]], "weektime_n") %>%
    summarise_at(names(Trajdata())[[6]], mymean) %>%
    ungroup() %>%
    left_join(trajweek()) %>%
    left_join(dataset_vars())
})

Trajdata_Weektimesub <- reactive({Trajdata_Weektime() %>% filter_(interp(~v %in% input$trajcheck , v=as.name(input$trajselectR)))})


output$trajdayplot <- renderPlotly({ withProgress(message = 'Processing Plot', {
  
  #Plots for means
  
  if(input$trajtype %in% "means") {
    if (input$trajaxis %in% "weektime_n") {
      if (input$trajselectR %in% "None") {
        
        ggplotly(ggplot(Trajdata(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]],  color = input$weektime_color)) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8, alpha = 0.7)+
                   stat_summary(fun.y = mean,
                                geom = "line", size = 1, alpha = 0.7) +
                   
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())
        )
      }
      
      else {
        ggplotly(ggplot(Trajdatasub(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], color = input$trajselectR)) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8,  alpha = 0.7)+
                   stat_summary(fun.y = mean,
                                geom = "line", size = 1,  alpha = 0.7, mapping = aes_string(linetype = "weekday")) +
                   
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())  
        )
      }
      
    }  
    else {
      if (input$trajselectR %in% "None") {
        ggplotly(ggplot(Trajdata(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]])) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8, alpha = 0.7, color = "deepskyblue2")+
                   stat_summary(fun.y = mean,
                                geom = "line", size = 1, alpha = 0.7,  color = "deepskyblue2") +
                   
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())
                 
        )
      }
      
      else {
        ggplotly(ggplot(Trajdatasub(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], color = input$trajselectR)) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   stat_summary(fun.data = mean_cl_normal, geom = "pointrange", size = 0.8,  alpha = 0.7)+
                   stat_summary(fun.y = mean,
                                geom = "line", size = 1,  alpha = 0.7) +
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())  
        )
      }
      
      
    }
  }
  
  #Plots for traces
  
  else if (input$trajtype %in% "subject traces") {
    if (input$trajaxis %in% "weektime_n") {
      if (input$trajselectR %in% "None") {
        
        ggplotly(ggplot(Trajdata_Weektime(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], group = names(Trajdata())[[1]],  color = input$weektime_color)) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   geom_line(size = 0.5, alpha = 0.2)+
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())
        )
      }
      
      else {
        ggplotly(ggplot(Trajdata_Weektimesub(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], group = names(Trajdata())[[1]], color = input$trajselectR)) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   geom_line(size = 0.5, alpha = 0.2)+
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())  
                 
        )
        
      }
    }  
    else if (input$trajaxis %in% "timepoint") {
      
      if(input$daytraces %in% "subject mean") {
        
        if (input$trajselectR %in% "None") {
          ggplotly(ggplot(Trajdata_Day(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], group = names(Trajdata())[[1]],  color = names(Trajdata())[[1]])) +
                     labs(title = paste0(names(Trajdata())[[6]])) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     geom_line(size = 0.5, alpha = 0.1)+
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 11) +
                     theme(panel.grid = element_blank())
          )
        }
        
        else {
          ggplotly(ggplot(Trajdata_Daysub(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], group = names(Trajdata())[[1]],  color = input$trajselectR)) +
                     labs(title = paste0(names(Trajdata())[[6]])) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     geom_line(size = 0.5, alpha = 0.1)+
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 11) +
                     theme(panel.grid = element_blank())  
          )
        }
        
      }
      
      else if (input$daytraces %in% "day level") {
        
        if (input$trajselectR %in% "None") {
          ggplotly(ggplot(Trajdata_Day2(), aes_string(x=input$trajaxis, y=names(Trajdata_Day2())[[7]], group = "ID_day", color = names(Trajdata_Day2())[[1]])) +
                     labs(title = paste0(names(Trajdata())[[6]])) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     geom_line(size = 0.5, alpha = 0.1)+
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 11) +
                     theme(panel.grid = element_blank())
          )
        }
        
        else {
          ggplotly(ggplot(Trajdata_Daysub2(), aes_string(x=input$trajaxis, y=names(Trajdata_Day2())[[7]], group = "ID_day",  color = input$trajselectR)) +
                     labs(title = paste0(names(Trajdata())[[6]])) +
                     #stat_summary(fun.y = mean,
                     #fun.ymin = function(x) mean(x) - sd(x),
                     #fun.ymax = function(x) mean(x) + sd(x),
                     #geom = "pointrange", size = .3)+
                     geom_line(size = 0.5, alpha = 0.1)+
                     #scale_x_discrete(limits=c(7:22))+
                     theme_bw(base_size = 11) +
                     theme(panel.grid = element_blank())  
          )
        }
        
      }
      
      
      
    }
    
    else if (input$trajaxis %in% "weekday_n") {
      if (input$trajselectR %in% "None") {
        ggplotly(ggplot(Trajdata_Week(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], group = names(Trajdata())[[1]])) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   geom_line(size = 0.5, alpha = 0.2, color = "deepskyblue2")+
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())
        )
      }
      
      else {
        ggplotly(ggplot(Trajdata_Weeksub(), aes_string(x=input$trajaxis, y=names(Trajdata())[[6]], group = names(Trajdata())[[1]],  color = input$trajselectR)) +
                   labs(title = paste0(names(Trajdata())[[6]])) +
                   #stat_summary(fun.y = mean,
                   #fun.ymin = function(x) mean(x) - sd(x),
                   #fun.ymax = function(x) mean(x) + sd(x),
                   #geom = "pointrange", size = .3)+
                   geom_line(size = 0.5, alpha = 0.2)+
                   #scale_x_discrete(limits=c(7:22))+
                   theme_bw(base_size = 11) +
                   theme(panel.grid = element_blank())  
        )
      }
      
      
    }
    
    
  }
  
})
  
})

