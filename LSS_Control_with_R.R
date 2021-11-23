library(dplyr)
#library(data.table)
library(ggplot2)
library(cusumcharter)
library(ggExtra)

df <- data.frame(rnorm(31, mean = 3 ), c(20211201:20211231) )
names(df) <- c("Val", "Date")

df1 <- data.frame(rnorm(29, mean = 2 ), c(20220101:20220129) )
names(df1) <- c("Val", "Date")

# as.Date & as.character functions
df$Date <- as.Date(as.character(df$Date), format = "%Y%m%d") 
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

cusum_control_plot(controls, xvar = df$Date) + theme_linedraw()


df$Val[4] <- 6 
df$Val[5] <- 6.4 


controls <- cusum_control(df$Val, target = 3, std_dev = 1, h = 3)
controls$Date <- df$DateD
cusum_control_plot(controls, xvar = Date) + theme_linedraw()


### Really Simple 
test_vec3 <- c(1,1,2,3,5,7,11,7,5,7,8,9,5)
controls <- cusum_control(test_vec3, target = 4)
cusum_control_plot(controls, xvar = obs)

#
# runchart example
#



df <- rbind(df,df1)
df$Date <- as.Date(as.character(df$Date), format = "%Y%m%d") 
View(df)

library(runcharter)

runcharter(df,
           direction = "below", 
           runlength = 0, 
           yval = df$Val,
           datecol = df$Date,
           grpvar = NULL)


## Better package
## https://cran.r-project.org/web/packages/qicharts2/vignettes/qicharts2.html
##

df <- data.frame(rnorm(31, mean = 3 ), c(20211201:20211231) )
names(df) <- c("Val", "Date")
df1 <- data.frame(rnorm(29, mean = 2 ), c(20220101:20220129) )
names(df1) <- c("Val", "Date")
df <- rbind(df,df1)
df$Date <- as.Date(as.character(df$Date), format = "%Y%m%d") 
View(df)

library(qicharts2)
qic(y = df$Val, x = df$Date, part = 31, part.labels = c('Baseline', 'Intervention')) 

 


