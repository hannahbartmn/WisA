# Erstellung des Datensatzes
# Maike und Julia



# TOP SECRET





# ACHTUNG Hannah und Alina nicht weiterlesen :)


# Interesse an Mathe  



Mathe_Int <- sample(0, size= 100, replace = TRUE)   # Es wird ein leerer Vektor mit 100 Nullen erstellt, diese werden im Folgenden ersetzt

# Die Zuordnung fuer das Studienfach Statistik mit verschiedenen Wahrscheinlichkeiten
Mathe_Int[which(Studienfach == "Statistik")] <- sample(1:7, size = length(which(Studienfach == "Statistik")),  # fuer das Sampling wird die Anzahl der Studienfaecher gezaehlt
                                                      replace = TRUE, prob = c(, , , , , , ))  # hier fehlen noch die einzelnen Wahrscheinlichkeiten
# Studienfach Data Science
Mathe_Int[which(Studienfach == "Data Science")] <- sample(1:7, size = length(which(Studienfach == "Data Science")),
                                                          replace = TRUE, prob = c(, , , , , , )) # Wahrscheinlichkeiten fehlen
# Studienfach Mathe
Mathe_Int[which(Studienfach == "Mathe")] <- sample(1:7, size = length(which(Studienfach == "Mathe")),
                                                   replace = TRUE, prob = c(, , , , , , ))  # Wahrscheinlichkeiten fehlen
# Studienfach Informatik
Mathe_Int[which(Studienfach == "Informatik")] <- sample(1:7, size = length(which(Studienfach == "Informatik")),
                                                        replace = TRUE, prob = c(, , , , , , ))  # Wahrscheinlichkeiten fehlen













