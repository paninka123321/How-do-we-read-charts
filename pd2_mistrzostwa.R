library("patchwork")
library(ggplot2)
df <- data.frame(x = c("Wyjdziemy z grupy", "Nie wyjdziemy"), y = c(0.4, 0.6),
                 group = c("Wyjdziemy z grupy", "Nie wyjdziemy"))

df2 <- data.frame(x = c("Nie awansujemy do finału","Awansujemy do finału"), y = c(0.9, 0.1),
                  group = c("Nie awansujemy do finału","Awansujemy do finału"))

p1 <- ggplot(df) +
  aes(x,y, fill = group)+
  geom_col(fill = c("#DE2020", "#63F53F")) +
  labs(title = "Szanse na wyjście Polski z grupy", subtitle =  "na Mistrzostwach Świata 2022")+
  xlab("")+
  ylab("szanse")+
  scale_y_continuous(labels=scales::percent) +
  theme( text=element_text(size = 22), axis.text.x = element_text(size = 22))

p2 <- ggplot(df2) +
  aes(x,y, fill = group)+
  geom_col(fill = c("#DE2020", "#63F53F")) +
  labs(title = "Szanse na wyjście Polski na awans do finału", subtitle =  "na Mistrzostwach Świata 2022")+
  xlab("")+
  ylab("szanse")+
  scale_y_continuous(labels=scales::percent) +
  theme( text=element_text(size = 22), axis.text.x = element_text(size = 22))

p1 + p2
