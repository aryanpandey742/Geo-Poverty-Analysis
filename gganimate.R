#transition through time

install.packages("gganimate")
library(gganimate)
library(tidyverse)
library(dplyr)
install.packages("gapminder")
library(gapminder)
library(ggthemes)
library(extrafont)
install.packages("gifski")
library(gifski)
view(gapminder)
install.packages("magick")
library(magick)
graph1=gapminder %>%  
  ggplot(aes(x=gdpPercap, y=lifeExp, color=continent, size=pop))+  
  geom_point(alpha=0.7, stroke=0)+
  theme_fivethirtyeight()+
  scale_size(range=c(2,12), guide="none")+
  scale_x_log10()+
  labs(title="Life Expectancy vs GDP Per Capita by Country",
       x="Income per Person(GDP/Capita)",
       y="Life Expectancy (years)",
       color="continent",
       caption="Gapminder")+
  guides(color = guide_legend(override.aes = list(size = 6)))+
  theme(axis.title=element_text(family="Rubik"),
        legend.text=element_text(size=14),legend.title = element_text(size = 16),legend.key.size = unit(1.2, "cm") )+
  scale_color_brewer(palette="Set1")
graph1
graph1.animation=graph1 +
  transition_time(year)+ 
  labs(subtitle="Year:{frame_time}")+
  shadow_wake(wake_length = 0.1)
graph1.animation
animate(graph1.animation, height = 500, width = 800, fps = 30, duration = 10, end_pause = 60, res=100)
anim_save("gapminder.gif")

#letting data gradually appear
install.packages("readr")
install.packages("ggthemes")
library(readr)
library(tidyr)
library(tidyverse)
library(ggthemes)
game_sales = read_csv("vgsales.csv")
view(game_sales)

game_sales = read_csv("vgsales.csv") %>% 
mutate(Year=as.numeric (Year)) %>% 
  filter(Platform == "PS3", Genre%in% c("Action","Shooter", "Sports", "Racing", "Simulation")) %>% 
  drop_na() %>% 
  group_by(Year, Genre) %>% 
  summarise(Sales= sum(Global_Sales, na.rm=TRUE))
view(game_sales)

graph2 = game_sales %>%  
  ggplot(aes(x=Year, y=Sales, color=Genre)) +
  geom_line()+geom_point() 
graph2.animation = graph2+
  transition_reveal(Year)+view_follow(fixed_y=TRUE)
graph2.animation
animate(graph2.animation, height = 500, width = 800, fps = 30, duration = 10, end_pause = 60, res=100)
