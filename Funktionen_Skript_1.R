# Funktionen-R-Skript 1


# Funktionen a, b, c: Alina, Hannah

























# Funktionen c, d, f: Maike, Julia

##########################################################
# Code für Funktion f
##########################################################
# noch nicht fertige Funktion, jedoch eine Visualisierung fuer Interesse und Studienfach 

install.packages("ggplot2")
library(ggplot2)
# hier muss noch geschaut werden, wie man bei der x-Achse die Beschriftung ändert, sonst für 3 Variablen
ggplot(data,                                                                          # ggplot wird vom Datensatz erstellt
       aes(y = factor(Mathe_Int,                                                      # Interesse für Mathe auf y-Achse
                      labels = c("sehr geringes Interesse = 1",                       # Beschriftung für die Punkte
                                 "2", "3", "4", "5", "6",
                                 "sehr hohes Interesse = 7")), 
           x = Prog_Int,                                                              # auf die x-Achse kommt Interesse für Programmieren
           color = Studienfach)) +                                                    # es wird farblich nach Studienfach unterschieden
  geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
              size = 1.5) + 
  labs(title = "Interesse nach Studienfach",                                          # Die jeweiligen Achsen werden beschriftet
       x = "Interesse an Programmiern", 
       y = "Interesse an Mathe", las = 1) 






















