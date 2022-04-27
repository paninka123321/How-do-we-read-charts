library(data.table)
library(dplyr)
library(ggplot2)
library(patchwork)
#Avengers
Wanda_Maximoff_height <- 168
Wanda_Maximoff_mass <- 58

agent_Phil_Coulson_mass <- 92
agent_Phil_Coulson_height <- 175 

Quicksilver_height <- 180
Quicksilver_mass <- 82

Avengers <- data.table(
  height = c(Wanda_Maximoff_height, agent_Phil_Coulson_height, Quicksilver_height),
  mass = c(Wanda_Maximoff_mass, agent_Phil_Coulson_mass, Quicksilver_mass),
  movie = c("Avengers","Avengers","Avengers"),
  name = c("Wanda", "agent Coulson", "Quicksilver")
)

#Wladca_Pierscieni
Legolas_height <- 180
Legolas_mass <- 77

Elrond_height <- 188
Elrond_mass <-82

Aragorn_height <- 180
Aragorn_mass <- 81

Wladca_Pierscieni <- data.table(
  height = c(Legolas_height, Elrond_height, Aragorn_height),
  mass = c(Legolas_mass, Elrond_mass, Aragorn_mass),
  movie = c("Władca Pierścieni","Władca Pierścieni","Władca Pierścieni"),
  name = c("Legolas", "Elrond", "Aragorn"))

Starwars_movie <- starwars %>% 
  select(name, height, mass) %>% 
  filter(height <=172 & height > 162 | name == "Luke Skywalker") %>% 
  filter(mass <= 72 | name == "Luke Skywalker") %>% 
  filter(height != 166) %>% 
  filter(height != 170) %>% 
  filter(height != 168)
Starwars_movie <- na.omit(Starwars_movie) %>% 
  mutate(movie = c("Starwars", "Starwars", "Starwars"))
dt <- rbind(Starwars_movie, Wladca_Pierscieni)

dt <- dt[order(dt$movie),] 

height_color <- dt %>% 
  ggplot(aes(x = name, y = height)) +
  geom_col(aes(colour = movie)) +
  theme(axis.text.x = element_text(angle=45),
        legend.position = "none",
        axis.title = element_blank())+
  labs(title = "Wzrost postaci filmowych")+
  plot_annotation(caption = " Słupki z czerwoną obramówką dotyczą postaci z Avengersów,\n z niebieską - postaci z Władcy Pierścieni,\n natomias z zieloną - postaci ze Starworsów")
height_color

height_grey <- dt %>% 
  ggplot(aes(x = name, y = height, fill = movie)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=45),
        legend.position = "none",
        axis.title = element_blank())+
  labs(title = "Wzrost postaci filmowych")+
  scale_fill_grey()+
  plot_annotation(caption = " Słupki carne - postaci z Avengersów,\n ciemno szare - postaci z Władcy Pierścieni,\n jasno szare - postaci ze Starworsów")
height_grey


level_order <- c('Luke Skywalker', 'Ben Quadinaros', 'Padmé Amidala',
                 'Legolas','Elrond', 'Aragorn')
height <- dt %>% 
  ggplot(aes(x = factor(name, level = level_order), y = height)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=45),
        legend.position = "none",
        axis.title = element_blank())+
  labs(title = "Wzrost postaci filmowych")+
  plot_annotation(caption = "Pierwsze 3 słupki dotyczą postaci z Avengersów,\n następne 4 słupki - postaci ze Starworsów,\n ostatnie 3 - postaci z Władcy Pierścieni")
height

height_bez_kolejnosci <-dt %>% 
  ggplot(aes(x = name, y = height)) +
  geom_col()+
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  ylab("Wzrost") +
  xlab("Postać") +
  labs(title = "Wzrost wybranych postaci filmowych")+
  plot_annotation(caption = "Luke Skywalker, Ben Quadinaros, Padmé Amidala - postaci ze Starwarsów,\n Legolas,Elron, Aragorn - postaci z Władcy Pierścieni") &
  theme(plot.caption = element_text(size = 16))+
  theme(text=element_text(size = 22), axis.text.x = element_text(size = 22))
height_bez_kolejnosci

height_better <- dt %>% 
  ggplot(aes(x = name, y = height, fill = movie)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=45))

weight <- dt %>% 
  ggplot(aes(x = name, y = mass, fill = movie)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=45))+
  labs(title = "Waga postaci filmowych", 
       axis.title = element_blank(),
       fill = "film")



  theme(plot.caption = element_text(size = 16))+
  theme(text=element_text(size = 22), axis.text.x = element_text(size = 22))
height_bez_kolejnosci

height_better <- dt %>% 
  ggplot(aes(x = name, y = height, fill = movie)) +
  geom_col() +
  theme(axis.text.x = element_text(angle=45))

weight <- dt %>% 
  ggplot(aes(x = name, y = mass, fill = movie)) +
  geom_col() +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  labs(title = "Waga wybranych postaci filmowych", 
       fill = "film")+
  ylab("Waga") +
  xlab("Postać") +
  theme(plot.caption = element_text(size = 16))+
  theme(text=element_text(size = 22), axis.text.x = element_text(size = 22))

weight +height_bez_kolejnosci 
weight
© 2022 GitHub, Inc.
Terms
Privacy
Security
Status
Docs
Contact GitHub
Pricing
API
Training
Blog
About
