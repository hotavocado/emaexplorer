heatdata_dR <- reactive({withProgress(message = 'Loading Data for Heatmap 1', value = 0, {
  
  if (input$heatselectR %in% "None") {
    
    dataset() %>% 
      select_(names(dataset())[[1]], names(dataset())[[input$table5_rows_selected]]) %>%
      mutate_at(names(dataset())[1], funs(factor)) %>%
      group_by_(names(dataset())[1]) %>%
      summarise_all(mymean) %>%
      left_join(stratify_vars$df2)
  }
  
  else if (input$heatselectR %in% "Response") {
    
    dataset() %>% 
      select_(names(dataset())[[1]], names(dataset())[[input$table5_rows_selected]]) %>%
      mutate_at(names(dataset())[1], funs(as.character)) %>%
      group_by_(names(dataset())[1]) %>%
      summarise_all(mymean) %>%
      left_join(stratify_vars$df2) %>%
      arrange_(names(dataset())[input$table5_rows_selected])
  }
  
  else {
    
    dataset() %>% 
      select_(names(dataset())[[1]], names(dataset())[[input$table5_rows_selected]]) %>%
      mutate_at(names(dataset())[1], funs(as.character)) %>%
      group_by_(names(dataset())[1]) %>%
      summarise_all(mymean) %>%
      left_join(stratify_vars$df2) %>%
      arrange_(input$heatselectR, names(dataset())[input$table5_rows_selected])
  }
  
  
})
})

##Data for timeofday heatmap
heatdataR <- reactive({withProgress(message = 'Loading Data for Heatmap 2', value = 0, {
  
  if (input$heatdata %in% "raw") {
    
    dataset() %>% 
      select_(names(dataset())[[1]], "timeofday", "weekday",  names(dataset())[[input$table5_rows_selected]]) %>%
      mutate_at(names(dataset())[1], funs(factor)) %>%
      group_by_(names(dataset())[1], input$heatstratR) %>%
      summarise_all(mymean) %>%
      left_join(select(stratify_vars$df2, -timeofday, -weekday))
    
  }  
  
  else if (input$heatdata %in% "subject normalized") {
    dataset() %>% 
      select_(names(dataset())[[1]], "timeofday", "weekday",  names(dataset())[[input$table5_rows_selected]]) %>%
      mutate_at(names(dataset())[1], funs(factor)) %>%
      group_by_(names(dataset())[1]) %>%
      mutate_at(names(dataset())[[input$table5_rows_selected]], .funs = funs(normalize)) %>%
      ungroup() %>%
      group_by_(names(dataset())[1], input$heatstratR) %>%
      summarise_all(mymean) %>%
      left_join(select(stratify_vars$df2, -timeofday, -weekday))
    
  }
})
})


##Code for main heatmap
output$heatmapR <- renderPlotly({
  
  if(input$heatselectR %in% c("None", "Response")) {
    
    ggplotly(ggplot(heatdata_dR()) + 
               geom_tile(aes_string(y=input$heatmapvar, x=factor("All"), fill=names(heatdata_dR())[2])) +
               labs(y="", x="", title=names(heatdata_dR())[2], fill="") +
               scale_fill_distiller(palette = "RdYlGn", direction = 1) +
               scale_y_discrete(limits=eval(parse(text=paste0("heatdata_dR()$", names(heatdata_dR())[1])))) +
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
    
    ggplotly(ggplot(heatdata_dR(), aes_string(label=input$heatselectR)) + 
               geom_tile(aes_string(y=names(heatdata_dR())[1], x=factor("All"), fill=names(heatdata_dR())[2])) +
               labs(y="", x="", title=names(heatdata_dR())[2], fill="") +
               scale_fill_distiller(palette = "RdYlGn", direction = 1) +
               scale_y_discrete(limits=eval(parse(text=paste0("heatdata_dR()$", names(heatdata_dR())[1])))) +
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
  
})


##Code for timeofday/weekday heatmap
output$heatmapTODR <- renderPlotly({
  
  if (input$heatselectR %in% c("None", "Response")) {  
    
    ggplotly(ggplot(heatdataR()) + 
               geom_tile(aes_string(x=input$heatstratR, y=names(heatdataR())[1], fill=names(heatdataR())[4])) +
               labs(x="", y="Subject ID", title=paste0(names(heatdataR())[4], " ", input$heatstratR), fill="") +
               scale_fill_distiller(palette = "RdYlGn", direction = 1) +
               scale_x_discrete(limits = levels(eval(parse(text=paste0("heatdataR()$", input$heatstratR))))) +
               scale_y_discrete(limits=eval(parse(text=paste0("heatdata_dR()$", names(heatdata_dR())[1])))) +
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
    
    ggplotly(ggplot(heatdataR(), aes_string(label=input$heatselectR)) + 
               geom_tile(aes_string(x=input$heatstratR, y=names(heatdataR())[1], fill=names(heatdataR())[4])) +
               labs(x="", y="Subject ID", title=paste0(names(heatdataR())[4], " ", input$heatstratR), fill="") +
               scale_fill_distiller(palette = "RdYlGn", direction = 1) +
               scale_x_discrete(limits = levels(eval(parse(text=paste0("heatdataR()$", input$heatstratR))))) +
               scale_y_discrete(limits=eval(parse(text=paste0("heatdata_dR()$", names(heatdata_dR())[1])))) +
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
})