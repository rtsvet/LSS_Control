library(dplyr)
#library(data.table)
library(ggplot2)
library(cusumcharter)
library(ggExtra)

df <- data.frame(rnorm(31, mean = 3 ), c(20211201:20211231) )
names(df) <- c("Val", "Date")

# as.Date & as.character functions
df$DateD <- as.Date(as.character(df$Date), format = "%Y%m%d") 
View(df)

par(mfrow=c(2,1))

ggplot(df,aes(DateD, Val)) + 
  geom_line() + 
  geom_point() +
  theme_minimal() +
  labs(x = NULL, y = NULL) + 
  ggExtra::rotateTextX() +
  theme_linedraw()

controls <- cusum_control(df$Val, target = 3, std_dev = 1)

View(controls)
controls$Date <- df$DateD

  
cusum_control_plot(cusum_control(df$Val, target = 3, std_dev = 1),xvar = obs)

cusum_control_plot(controls, xvar = Date) + theme_linedraw()


df$Val[4] <- 6 
df$Val[5] <- 6.4 


controls <- cusum_control(df$Val, target = 3, std_dev = 1, h = 3)
controls$Date <- df$DateD
cusum_control_plot(controls, xvar = Date) + theme_linedraw()


### Really Simple 
test_vec3 <- c(1,1,2,3,5,7,11,7,5,7,8,9,5)
controls <- cusum_control(test_vec3, target = 4)
cusum_control_plot(controls, xvar = obs)


df1 <- data.frame(rnorm(29, mean = 2 ), c(20220101:20220129) )
names(df1) <- c("Val", "Date")

data <- rbind(df,df1)
data$Date <- as.Date(as.character(data$Date), format = "%Y%m%d") 
View(data)
library(dplyr)
library(data.table)

library(runcharter)

runcharter(data, direction = "both", 
           runlength = 0, 
           yval = data$Val, datecol = data$Date, grpvar = NULL)


runcharter(df = signals,
           med_rows = 13,
           runlength = 9,
           direction = "below",
           datecol = NULL,
           grpvar = NULL,
           yval = NULL,
           facet_cols = NULL,
           facet_scales = "fixed",
           chart_title = NULL,
           chart_subtitle = NULL,
           chart_caption = NULL,
           chart_breaks = NULL,
           line_colr = "#005EB8", # blue
           point_colr ="#005EB8", # blue
           median_colr = "#E87722", # orange
           highlight_fill = "#DB1884") # magenta 

View(signals)

runcharter(signals,
           direction = "below",
           datecol = date, 
           grpvar = grp,
           yval = y, 
           chart_title = "Runs identified",
           chart_subtitle = "Runs below the median signalling improvement")

ddd <- data
names(ddd) <- c("y", "date")
runcharter(ddd,
           direction = "below",
           datecol = date, 
           yval = y,
           grpvar = NULL, 
           chart_title = "Runs identified",
           chart_subtitle = "Runs below the median signalling improvement")

summary(signals)

