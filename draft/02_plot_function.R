fun_plot <- function(data,var1,var2){

              data_plot1 <<- data %>% 
                group_by(across(var1),across(var2)) %>% 
                summarise(n = n()) %>% 
                ungroup()
              
              data_plot2 <<- data %>% 
                group_by(across(var1)) %>% 
                summarise(n_DriverAge = n()) %>% 
                ungroup()
              
              print(as.character(as.name(var1)))
              data_plot <<- merge(data_plot1, data_plot2, by = var1)
              
              data_plot$freq = data_plot$n/data_plot$n_DriverAge
              
              data_plot <<- data_plot
  
              plot_DriverAge <- plot_ly(data_plot) %>%
                add_trace(x =as.formula(paste0("~", var1)), y =~ n, type = 'bar', color =as.formula(paste0("~", var2))) %>% 
                layout(barmode = 'stack',
                       yaxis = list(title = "Absolute frequency"),
                       title = "Claim Number by Driver Age")
              print(plot_DriverAge)
              
              plot_DriverAge_100 <- plot_ly(data_plot) %>%
                add_trace(x =as.formula(paste0("~", var1)), y =~ freq, type = 'bar', color =as.formula(paste0("~", var2)), opacity = 0.8) %>% 
                layout(barmode = 'stack',
                       yaxis = list(title = "proportion"),
                       title = "Claim Number by Driver Age")
              print(plot_DriverAge_100)
              
              
}



fun_plot(data = freMTPLfreq,var1 = "CarAge",var2 = "ClaimNb")
fun_plot(data = freMTPLfreq,var1 = "Driver",var2 = "ClaimNb")


