

summary(lm(qci$accuracy ~ qci$workcond))

library(ggplot2)

 ggplot(qci, aes(workcond, accuracy)) + 
   geom_point(position = position_jitter(width = .05, height = 0)) + 
   stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) + 
   expand_limits(x = 0)
   

