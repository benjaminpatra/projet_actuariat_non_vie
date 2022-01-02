library(dplyr)
library(plotly)
library(comprehenr)
my_colors <- c(RColorBrewer::brewer.pal(6, "Blues")[1:6])

fun_plot <- function(data,var1,var2){

              data_plot1 <<- data %>% 
                group_by(across(var1),across(var2)) %>% 
                summarise(n = n()) %>% 
                ungroup()
              
              data_plot2 <<- data %>% 
                group_by(across(var1)) %>% 
                summarise(n_var1 = n()) %>% 
                ungroup()
              
              print(as.character(as.name(var1)))
              data_plot <<- merge(data_plot1, data_plot2, by = var1)
              
              data_plot$freq = data_plot$n/data_plot$n_var1
              
              data_plot <<- data_plot
  
              plot <- plot_ly(data_plot) %>%
                add_trace(x = as.formula(paste0("~", var1)), y =~ n, 
                          type = 'bar', color = as.formula(paste0("~","as.factor(", var2,")")),
                          colors = "Reds") %>% 
                layout(barmode = 'stack',
                       yaxis = list(title = "Absolute frequency"),
                       title = paste0(var2," by ", var1))
              print(plot)
              
              plot_100 <- plot_ly(data_plot) %>%
                add_trace(x = as.formula(paste0("~", var1)), y =~ freq, 
                          type = 'bar', color = as.formula(paste0("~","as.factor(", var2,")")), 
                          colors = "Reds") %>% 
                layout(barmode = 'stack',
                       yaxis = list(title = "proportion"),
                       title = paste0(var2," by ", var1))
              print(plot_100)
              
              
}
# fun_plot(data = freMTPLfreq,var1 = "CarAge",var2 = "ClaimNb")
# fun_plot(data = freMTPLfreq,var1 = "DriverAge",var2 = "ClaimNb")



fun_boxplot <- function(data, var1, var2){
  data_plot <- data %>% rename(y = eval(var2))
  m = quantile(data_plot$y, 0.9)

  fig <- plot_ly(data = data, color = "grey25") %>% 
    add_trace(x = as.formula(paste0("~","as.factor(", var1,")")),
              y = as.formula(paste0("~", var2)), 
              type = "box",
              colors = "grey") %>%
    layout(boxmode = "group",
           xaxis = list(title = var1),
           yaxis = list(title = var2, range = c(0, 0.8*m)),
           title = paste0(var2," by ", var1))
  print(fig)
}
#fun_boxplot(freMTPL_filtered, "DriverAge", "ClaimAmount")



mean_variance <- function(data, var_group, var_nb, weight = F, var_expo = NULL){
  if (!weight){
  data_plot <- data %>% rename(group = eval(var_group), nb = eval(var_nb)) %>% 
    group_by(group) %>%
    summarise(m = mean(nb),
              v = var(nb)) %>%
    filter(! is.na(v))
  }else
    data_plot <- data %>% rename(group = eval(var_group), nb = eval(var_nb), expo = eval(var_expo)) %>% 
      group_by(group) %>%
      summarise(m = weighted.mean(nb, expo),
                v = weighted.var(nb, expo)) %>%
      filter(! is.na(v))
  
  # Linear regression
  linear <- lm(I(v-m) ~  I(m^2) -1, data = data_plot)
  coef <- summary(linear)$coefficients[1]
  
  # plot
  plot(data_plot$m, data_plot$v,
       main = paste0("Mean-Variance relationship by ", var_group),
       sub = paste("a =", round(coef, 3)),
       xlab = "mean", ylab = "variance")
  
  # Plot regression line
  x0 <- seq(0.9*min(data_plot$m), 1.1*max(data_plot$m), length = 20)  ## prediction grid
  y0 <- to_vec(for(x in x0) x + x^2 *coef)
  lines(x0, y0, col = "red")  
}
# mean_variance(freMTPLfreq, 'Region', 'ClaimNb')
# mean_variance(freMTPLfreq, 'Region', 'ClaimNb', T, "Exposure")



amountvsnb <- function(x, add=FALSE, horiz=FALSE, main, ...)
{
  n <- length(x)
  if(missing(main))
    main <- "aggregate amount vs. claim number"
  if(!add)
  {
    z <- cumsum(sort(x))/sum(x)
    y <- (1:n)/n
    plot(z, y, type="l", ylim=0:1, 
         ylab="% claim nb.", xlab="% agg. claim amount",
         main=main, ...)
    if(horiz)
      abline(h=1:10/10, lty=3, col="grey")
    else
      for(i in 3:19)
      {
        p <- i/20
        ind <- head(which(abs(y - p) < 1e-3), 1)
        lines(c(-1, z[ind]), c(p, p), lty=2, col="grey25")
        lines(c(z[ind], z[ind]), c(-1, p), lty=2, col="grey25")
        print(c(p, z[ind]))
      }
  }else
    lines(cumsum(sort(x))/sum(x), (1:n)/n, ...)
  
}



amountvsnb_grouped <- function(data, var_group, var_claim_amount, ...){
  data_plot <- data %>% rename(group = eval(var_group), amount = eval(var_claim_amount)) 
  data_plot <- data_plot %>% arrange(group) 
  lev <- unique(data_plot$group)
  colors=c(RColorBrewer::brewer.pal(length(lev),"Set1"))
  title = paste0("agg./nb. per ", var_group)
  i = 1
  for (g in lev){
    if (i == 1) {
      baseG = data_plot %>% filter(group == g)
      amountvsnb(baseG$amount, horiz = T, col = colors[i], main = title)
      i = i+1
      
    }else{
      baseG = data_plot %>% filter(group == g)
      amountvsnb(baseG$amount, horiz = T, add = T, col = colors[i])
      i = i+1
    }
  }
  legend("bottomright",
         legend=lev,
         col=colors,
         lty = 1, cex = 0.7)
}



plotgroupresiduals <- function(object, m=100, trim=TRUE, ...)
{
  yh <- fitted.values(object)
  re <- residuals(object)
  if(trim)
    ind <- abs(re) <= quantile(abs(re), probs=.99) & abs(yh) <= quantile(abs(yh), probs=.99)
  else
    ind <- 1:length(re)
  yh <- yh[ind]
  re <- re[ind]
  
  n <- length(yh)
  ind <- sample(1:n, n)
  yh <- yh[ind]
  re <- re[ind]
  
  #group
  if(m > 1)
  {
    yhg <- rowMeans(matrix(yh, ncol=m))
    reg <- rowMeans(matrix(re, ncol=m))
    plot(yhg, reg, ylab = "Group residuals", xlab="Group fitted values", ...)
  }else
    plot(yh, re, ylab = "Residuals", xlab="Fitted values", ...)
  
  abline(h=0, lty=3, col="grey")
}



pred_group <- function(data, model, var_nb, var_group){
  data_plot <- data %>% rename(group = eval(var_group),
                               nb = eval(var_nb))
  
  data_count <- data_plot %>% 
    group_by(group) %>% 
    summarise(n = n(), obs_freq = mean(nb))
  print(data_count)
  ggplot(data_count) +
    geom_bar(aes(x = as.factor(group), y = obs_freq), stat = 'identity') +
    geom_point(aes(x = as.factor(group), y = obs_freq))
  
  # my_model = model
  # predict(my_model, newdata, type="response")
}

#pred_group(freMTPLfreq, fpois2, "ClaimNb", "DriverAgeG")
