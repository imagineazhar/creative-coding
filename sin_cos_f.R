library(tidyverse)
seq(-3,3,by=.01) %>%
  expand.grid(x=., y=.) %>%
  ggplot(aes(x=(x^2-sin(y^4)), y=(y^2-cos(x^2)))) +
  geom_point(alpha=.1, shape=20, size=0.1, color="white")+
  theme_void()+
  coord_fixed()+
  theme(panel.background = element_rect(fill="black"))+
  coord_polar()