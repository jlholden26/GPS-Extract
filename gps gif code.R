library(tidyverse)
library(data.table)
library(ggforce)
library(gganimate) # devtools::install_github('thomasp85/gganimate')

load("GPSsample.RData")

ven_len <- 165
ven_wid <- 130

GPS_edit <- GPSsample %>%
  mutate(actualTime = as.POSIXct(as.numeric(timeEpoch)/1000, origin = "1970-01-01")) %>%
  arrange(actualTime) %>%
  mutate(qStart = countdown - lag(countdown) > 1000 & countup < 10,
         qEnd = lead(timeon) - timeon < -60 | is.na(lead(timeon)),
         qNum = ifelse(lag(cumsum(qEnd)==cumsum(qStart)), NA_integer_, cumsum(qStart))
  )

plot <- GPS_edit %>%
  subset(qNum==3 & countup > 1923 & countup < 1945 &
           name %in% c("LJackson","COliver","CPetracca","JViney",
                       "TLiberatore","TEnglish","MBontempelli","ATreloar")) %>%
  mutate(x = x * ven_len,
         y = -y * ven_len) %>%
  {ggplot(.) +
      geom_path(aes(y, x, color = as.character(home), group = name)) +
      geom_text(aes(y, x, color = as.character(home), label = name, group = name), size = 3) +
      scale_color_manual(values = c("blue","red")) +
      stat_ellipse(data = data.frame(x=c(.5,0,-.5,0) * ven_len,
                                     y=c(0,ven_wid/(ven_len*2),0,-ven_wid/(ven_len*2)) * ven_len),
                   aes(y,x),
                   type="t", level = 0.535, size = 1) +
      coord_fixed() +
      geom_segment(data = mtcars, aes(y = -ven_len/2, yend = -ven_len/2+10, x = -5, xend = -5)) +
      geom_segment(data = mtcars, aes(y = -ven_len/2, yend = -ven_len/2+10, x = 5, xend = 5)) +
      geom_segment(data = mtcars, aes(y = -ven_len/2+10, yend = -ven_len/2+10, x = -5, xend = 5)) +
      geom_segment(data = mtcars, aes(y = ven_len/2, yend = ven_len/2-10, x = -5, xend = -5)) +
      geom_segment(data = mtcars, aes(y = ven_len/2, yend = ven_len/2-10, x = 5, xend = 5)) +
      geom_segment(data = mtcars, aes(y = ven_len/2-10, yend = ven_len/2-10, x = -5, xend = 5)) +
      geom_segment(data = mtcars, aes(y = -25, yend = -25, x = -25, xend = 25)) +
      geom_segment(data = mtcars, aes(y = 25, yend = 25, x = -25, xend = 25)) +
      geom_segment(data = mtcars, aes(y = -25, yend = 25, x = 25, xend = 25)) +
      geom_segment(data = mtcars, aes(y = 25, yend = -25, x = -25, xend = -25)) +
      geom_circle(data = mtcars, aes(r = 3, x0 = 0, y0 = 0)) +
      geom_circle(data = mtcars, aes(r = 10, x0 = 0, y0 = 0)) +
      geom_arc(data = mtcars, aes(y0=ven_len/2, x0=0, r = 50, start = pi*0.65, end = pi*1.35), color = "black") +
      geom_arc(data = mtcars, aes(y0=-ven_len/2, x0=0, r = 50, start = -pi*0.35, end = pi*0.35), color = "black") +
      theme_bw() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "none") +
      scale_y_continuous(name = "", expand=c(0,0)) +
      scale_x_continuous(name = "", expand=c(0,0))}

plot + transition_reveal(actualTime)

anim_save("oliver_goal.gif")
