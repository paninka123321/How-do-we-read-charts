library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
Ankieta <- read_csv("Downloads/Formularz bez nazwy.csv")
View(Ankieta)
 #W ankiecie wzięlo udział 98 osób

Ankieta <- Ankieta[!(is.na(Ankieta$`Ile masz lat?`)),]

#jedna osoba nie podała wieku

#dodatkowo jedna osoba podałą wiek 4 lata a jedna .. <- nie prawdziwe dane:
Ankieta <- Ankieta %>% 
  filter(`Ile masz lat?` != 4 & `Ile masz lat?` != "..")

#po wyelminowaniu nieprawdziwych lub braku danych laczna liczba osob bioracych udzial w ankiecie wynosi 95



#posortowana liczba osob po płci
Plec <- Ankieta %>% 
  group_by(`Jaka jest Twoja płeć`) %>% 
  summarise(Liczność = n())

Plec_wykres <- Plec %>% ggplot(aes(x = `Jaka jest Twoja płeć`, y = Liczność)) +
  geom_col()+
  xlab("Płeć") +
  geom_text(aes(label = Liczność), vjust = -0.2)

#z podziałem na wiek
Ankieta <- Ankieta %>% 
  mutate(Przedzial_wiekowy = case_when(
    `Ile masz lat?` >=12 & `Ile masz lat?` <= 15 ~ "12-15",
    `Ile masz lat?` > 15 & `Ile masz lat?` < 60 ~ "16-59",
    `Ile masz lat?` >= 60 ~ ">59"))
Wiek <- Ankieta %>% 
  group_by(Przedzial_wiekowy) %>% 
  summarise(Liczność = n())

Wiek_wykres <- Wiek %>% 
  ggplot(aes(x = Przedzial_wiekowy, y = Liczność)) + 
  geom_col()+
  geom_text(aes(label = Liczność), vjust = -0.2)

# z podziałem na wiek i plec
W_P <- Ankieta %>% 
  group_by(`Jaka jest Twoja płeć`, Przedzial_wiekowy) %>% 
  summarise(Liczność = n(), .groups = "drop")

#Oki ogolnie moze byc podpis albo na gorze albo w srodku, moim zdaniem, lepiej na gorze, 
#dla porownania zosatwaim ci kod do podpisania w srodku (POROWNAJ!!!!)
Wiek_Plec_wykres <- W_P %>% 
  ggplot(aes(x = Przedzial_wiekowy, y = Liczność, fill = `Jaka jest Twoja płeć`))+
  geom_col(position = "dodge")+
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))
  #geom_text(aes(label = Liczność), vjust = 1.2, colour = "white", position = position_dodge(.9))

# Pierwsze pytanie - prawidlowa odpowiedz 40%
names(Ankieta)[2:7] <- c('Wiek', 'Płeć', 'pierwsze_pytanie', 'drugie_pytanie',
                         'trzecie_pytanie', 'czwarte_pytanie') #odechcialo mi sie tu starych nazw

#dziele ich na rozne przedzialy wiekowe zeby wykresy ladnie wygladaly
dzieci <- Ankieta %>% 
  filter(Przedzial_wiekowy == "12-15")
dorosli <- Ankieta %>% 
  filter(Przedzial_wiekowy == "16-59")
starsi <- Ankieta %>% 
  filter(Przedzial_wiekowy == ">59")


pierwsze_dzieci <- dzieci %>% 
  group_by( Płeć, pierwsze_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
pierwsze_dzieci_wykres <- pierwsze_dzieci %>% 
  ggplot(aes(x = pierwsze_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dzieci")+
  xlab("Udzielona odpowiedź")+
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0",
                             "#a57c1b"))

pierwsze_dorosli <- dorosli %>% 
  group_by( Płeć, pierwsze_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
pierwsze_dorosli_wykres <- pierwsze_dorosli %>% 
  ggplot(aes(x = pierwsze_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dorośli")+
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))+
  theme(legend.position="none")

pierwsze_starsi <- starsi %>% 
  group_by( Płeć, pierwsze_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
pierwsze_starsi_wykres <- pierwsze_starsi %>% 
  ggplot(aes(x = pierwsze_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Osoby w wieku emerytalnym") +
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))+
  theme(legend.position="none")
#tu warto zaznaczyc ze mezczyzni, ktorzy brali udzial w naszej ankiecie byli powyzej 65 roku zycia - czyli wieku emerytalnego mezczyzn

pierwsze_dzieci_wykres + pierwsze_dorosli_wykres + pierwsze_starsi_wykres + 
  plot_layout(guides = 'collect') + 
  plot_annotation("Jak odpowiadali ankietowani na pytanie dotyczące wykresu\nz zamienionymi kolorami?")


#Drugie pytanie - prawidłowa odpowiedź Luke Skywalker
drugie_dzieci <- dzieci %>% 
  group_by(Płeć, drugie_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
drugie_dzieci_wykres <- drugie_dzieci %>% 
  ggplot(aes(x = drugie_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dzieci")+
  xlab("Udzielona odpowiedź")+
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0",
                             "#a57c1b"))


  
drugie_dorosli <- dorosli %>% 
  group_by( Płeć, drugie_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
drugie_dorosli_wykres <- drugie_dorosli %>% 
  ggplot(aes(x = drugie_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dorośli")+
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))+
  theme(legend.position="none")
drugie_dorosli_wykres
#zobacz drugie dorosli wykres i porownaj nastepnie z tym nizszym

dla_doroslych <-  dorosli %>% 
  select(Płeć, Przedzial_wiekowy, drugie_pytanie) %>% 
  mutate(Wybrana_postac = case_when(
    drugie_pytanie == 'Elrond' ~ "Elrond",
    drugie_pytanie == 'Luke Skywalker' ~ "Luke Skywalker", 
    TRUE ~ "Inna"))
drugie_dorosli <- dla_doroslych %>% 
  group_by( Płeć, Wybrana_postac) %>% 
  summarise(Liczność = n(), .groups = "drop")
drugie_dorosli_wykres <- drugie_dorosli %>% 
  ggplot(aes(x = Wybrana_postac, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dorośli")+
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))
drugie_dorosli_wykres
#moim zdaniem tak lepiej widać odpowiedzi



drugie_starsi <- starsi %>% 
  group_by( Płeć, drugie_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
drugie_starsi_wykres <- drugie_starsi %>% 
  ggplot(aes(x = drugie_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Osoby w wieku emerytalnym") +
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))+
  theme(legend.position="none")

drugie_dzieci_wykres + drugie_dorosli_wykres + drugie_starsi_wykres + 
  plot_layout(guides = 'collect') + 
  plot_annotation("Jak odpowiadali ankietowani na pytanie dotyczące wzrostu postaci z filmu\nz przyporządkowaniem postaci do flimu za pomocą podpisu pod wykresem?")


#4 pytanie
#zobaczysz ze potem sie przyda...
Ankieta <- Ankieta %>% 
  mutate(czwarte_pytanie = case_when(
    czwarte_pytanie == "Nie da się odczytać z wykresu" ~ "Nie da się odczytać",
    czwarte_pytanie == "Legolas" ~ "Legolas",
    czwarte_pytanie == "Padmé Amidala" ~ "Padmé Amidala",
    czwarte_pytanie == "Elrond" ~ "Elrond",
    czwarte_pytanie == "Nie wiem" ~ "Nie wiem",
    czwarte_pytanie == "Aragorn" ~ "Aragorn"
  ))
dorosli <- Ankieta %>% 
  filter(Przedzial_wiekowy == "16-59")
#to koniec wcinki xdd
czwarte_dzieci <- dzieci %>% 
  group_by( Płeć, czwarte_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
czwarte_dzieci_wykres <- czwarte_dzieci %>% 
  ggplot(aes(x = czwarte_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dzieci")+
  xlab("Udzielona odpowiedź")+
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0",
                             "#a57c1b"))+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

czwarte_dorosli <- dorosli %>% 
  group_by( Płeć, czwarte_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
czwarte_dorosli_wykres <- czwarte_dorosli %>% 
  ggplot(aes(x = czwarte_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Dorośli")+
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))

czwarte_starsi <- starsi %>% 
  group_by( Płeć, czwarte_pytanie) %>% 
  summarise(Liczność = n(), .groups = "drop")
czwarte_starsi_wykres <- czwarte_starsi %>% 
  ggplot(aes(x = czwarte_pytanie, y = Liczność, fill = Płeć)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = Liczność), vjust = -0.2, position = position_dodge(.9))+
  labs(title = "Osoby w wieku emerytalnym") +
  xlab("Udzielona odpowiedź") +
  scale_fill_manual(values=c("#e14b31",
                             "#22a7f0"))+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge=2))
#tu warto zaznaczyc ze mezczyzni, ktorzy brali udzial w naszej ankiecie byli powyzej 65 roku zycia - czyli wieku emerytalnego mezczyzn

czwarte_dzieci_wykres + czwarte_dorosli_wykres + czwarte_starsi_wykres + 
  plot_layout(guides = 'collect') + 
  plot_annotation("Jak odpowiadali ankietowani na pytanie dotyczące wagi postaci z filmu\nz podziałem na film za pomocą wypełenienia kolorem słupka?")

