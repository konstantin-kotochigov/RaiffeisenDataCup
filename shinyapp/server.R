library(shiny)
library(ggplot2)

result <- read.table("result_test.csv", sep=";", header=T)
# result$size <- pmax(result$size_x, result$size_y)
# result$zoom <-  ifelse(result$size < 0.15, 12, ifelse(result$size<0.30, 11, 10))
# result <- result[,c("customer_id","pos_atm_orig_lat","pos_atm_orig_lon","home_orig_lat","home_orig_lon","work_lat","work_lon","top_city_lon","top_city_lat","mcc","left","right","top","bottom","clus")]
# write.table(result, "result_test.csv", sep=";", row.names=F)
customers <- unique(result$customer_id)



# Define server logic required to plot various variables against mpg
shinyServer(function(input, output,session) {
  
  observeEvent(input$next_button, {
    new_value = as.numeric(input$cust_id) + 1
    updateTextInput(session, "cust_id", value=as.character(new_value))
  })
  
  observeEvent(input$prev_button, {
    new_value = as.numeric(input$cust_id) - 1
    new_value = ifelse(new_value < 1, as.numeric(input$cust_id), new_value)
    updateTextInput(session, "cust_id", value=as.character(new_value))
  })
  
  observeEvent(input$clear_button, {
    updateCheckboxInput(session, "check_all", value=F)
    updateCheckboxInput(session, "check_6011", value=F)
    updateCheckboxInput(session, "check_5411", value=F)
    updateCheckboxInput(session, "check_5814", value=F)
    updateCheckboxInput(session, "check_5912", value=F)
    updateCheckboxInput(session, "check_5812", value=F)
    updateCheckboxInput(session, "check_5541", value=F)
    updateCheckboxInput(session, "check_5499", value=F)
    updateCheckboxInput(session, "check_4111", value=F)
    updateCheckboxInput(session, "check_5691", value=F)
    updateCheckboxInput(session, "check_5977", value=F)
    updateCheckboxInput(session, "check_5921", value=F)
    updateCheckboxInput(session, "check_5331", value=F)
    updateCheckboxInput(session, "check_5999", value=F)
    updateCheckboxInput(session, "check_5261", value=F)
    updateCheckboxInput(session, "check_5661", value=F)
  })
  
  observeEvent(input$fill_button, {
    updateCheckboxInput(session, "check_all", value =T)
    updateCheckboxInput(session, "check_6011", value=T)
    updateCheckboxInput(session, "check_5411", value=T)
    updateCheckboxInput(session, "check_5814", value=T)
    updateCheckboxInput(session, "check_5912", value=T)
    updateCheckboxInput(session, "check_5812", value=T)
    updateCheckboxInput(session, "check_5541", value=T)
    updateCheckboxInput(session, "check_5499", value=T)
    updateCheckboxInput(session, "check_4111", value=T)
    updateCheckboxInput(session, "check_5691", value=T)
    updateCheckboxInput(session, "check_5977", value=T)
    updateCheckboxInput(session, "check_5921", value=T)
    updateCheckboxInput(session, "check_5331", value=T)
    updateCheckboxInput(session, "check_5999", value=T)
    updateCheckboxInput(session, "check_5261", value=T)
    updateCheckboxInput(session, "check_5661", value=T)
  })
  
  output$info <- renderText(
    {
      ifelse(is.na(input$plot_click$x),"", paste("lon=",input$plot_click$x,", lat=", input$plot_click$y,sep=""))
    }
  )

  
  
  output$plot <- renderPlot({

    
    current_transactions <- result[result$customer_id == customers[c(as.numeric(input$cust_id))],,]
    
    filter <- c()
    
    if (input$check_6011) filter <- c(filter, "6011")
    if (input$check_5411) filter <- c(filter, "5411")
    if (input$check_5814) filter <- c(filter, "5814")
    if (input$check_5912) filter <- c(filter, "5912")
    if (input$check_5812) filter <- c(filter, "5812")
    if (input$check_5541) filter <- c(filter, "5541")
    if (input$check_5499) filter <- c(filter, "5499")
    if (input$check_4111) filter <- c(filter, "4111")
    if (input$check_5691) filter <- c(filter, "5691")
    if (input$check_5977) filter <- c(filter, "5977")
    if (input$check_5921) filter <- c(filter, "5921")
    if (input$check_5331) filter <- c(filter, "5331")
    if (input$check_5999) filter <- c(filter, "5999")
    if (input$check_5261) filter <- c(filter, "5261")
    if (input$check_5661) filter <- c(filter, "5661")
    
    ymin = current_transactions$left[1] 
    ymax = current_transactions$right[1]
    xmin = current_transactions$bottom[1] 
    xmax = current_transactions$top[1]
    
    homework_coords = current_transactions[1,c("home_orig_lat","home_orig_lon","work_lat","work_lon")]
    
    
    
    if (input$check_all)
      current_transactions <- current_transactions
    else
      current_transactions <- current_transactions[current_transactions$mcc %in% filter,]
    
    
    
    
    
    
    
    
    # print(nrow(current_transactions))
    # area <- areaCoordinates[customer_id==custid,]

    # bgMap = get_map(location=c(mean(current_transactions$pos_atm_orig_lon, na.rm=T), mean(current_transactions$pos_atm_orig_lat,na.rm=T)), zoom=x$zoom[1], source = "google", color="bw")
    # ggmap(bgMap) + geom_point(aes(x = current_transactions$pos_atm_orig_lon, y = current_transactions$pos_atm_orig_lat), data = current_transactions, alpha = .75, size=3, color="red")
  
    # print(nrow(current_transactions))  
    
    p <- ggplot() + theme_light() + geom_point(aes(x=pos_atm_orig_lon, y=pos_atm_orig_lat), data = current_transactions, color=factor(current_transactions$clus), alpha = .75, size=3, color="black") +
      xlim(xmin, xmax) + ylim(ymin,ymax) + xlab("Longitude") + ylab("Latitude")
    
    # if (input$check4)  
    #   p <- p + geom_point(aes(x=home_orig_lon, y=home_orig_lat), data = homework_coords, alpha = .75, size=5, pch=3, color="red")
    
    # if (input$check5)
    #   p <- p + geom_point(aes(x=work_lon,      y=work_lat),      data = homework_coords, alpha = .75, size=5, pch=3, color="blue")
      # geom_point(aes(x=top_city_lon,      y=top_city_lat),      data = current_transactions, alpha = .75, size=5, pch=2, color="yellow")

    p    

  }, width=640, height=640)
})