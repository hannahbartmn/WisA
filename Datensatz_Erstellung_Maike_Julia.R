# Erstellung des Datensatzes
# Maike und Julia



# TOP SECRET





# ACHTUNG Hannah und Alina nicht weiterlesen :)


# hier wird die ID erstellt
ID <- 1:100   # durchzaehlen von 1 bis 100




# Interesse an Mathe  
# hier wird ein Vektor erstellt, bei dem je nach Studienfach die Wahrscheinlichkeit am Interesse an Mathe variiert 
# hierbei steht 1 fuer sehr geringes Interesse und 7 fuer sehr hohes Interesse.


Mathe_Int <- sample(0, size= 100, replace = TRUE)   # Es wird ein leerer Vektor mit 100 Nullen erstellt, diese werden im Folgenden ersetzt

# Die Zuordnung fuer das Studienfach Statistik mit verschiedenen Wahrscheinlichkeiten
set.seed(0902)
Mathe_Int[which(Studienfach == "Statistik")] <- sample(1:7, size = length(which(Studienfach == "Statistik")),  # fuer das Sampling wird die Anzahl der Studienfaecher gezaehlt
                                                      replace = TRUE, prob = c(, , , , , , ))  # hier fehlen noch die einzelnen Wahrscheinlichkeiten
# Studienfach Data Science
set.seed(0902)
Mathe_Int[which(Studienfach == "Data Science")] <- sample(1:7, size = length(which(Studienfach == "Data Science")),
                                                          replace = TRUE, prob = c(, , , , , , )) # Wahrscheinlichkeiten fehlen
# Studienfach Mathe
set.seed(0902)
Mathe_Int[which(Studienfach == "Mathe")] <- sample(1:7, size = length(which(Studienfach == "Mathe")),
                                                   replace = TRUE, prob = c(, , , , , , ))  # Wahrscheinlichkeiten fehlen
# Studienfach Informatik
set.seed(0902)
Mathe_Int[which(Studienfach == "Informatik")] <- sample(1:7, size = length(which(Studienfach == "Informatik")),
                                                        replace = TRUE, prob = c(, , , , , , ))  # Wahrscheinlichkeiten fehlen






# Interesse an Programmieren 
# hier wird ein Vektor erstellt, bei dem je nach Studienfach die Wahrscheinlichkeit am Interesse an Programmieren variiert 
# hierbei steht 1 fuer sehr geringes Interesse und 7 fuer sehr hohes Interesse.


Prog_Int <- sample(0, size= 100, replace = TRUE)   # Es wird ein leerer Vektor mit 100 Nullen erstellt, diese werden im Folgenden ersetzt


# Studienfach Statistik, die Anzahl wird hier wieder je nach Groesse der Anzahl der Studienfachs bestimmt
set.seed(0902)
Prog_Int[which(Studienfach == "Statistik")] <- sample(1:7, size = length(which(Studienfach == "Statistik")),   
                                                      replace = TRUE, prob = c(, , , , , , ))   # Wahrscheinlichkeit fehlt
# Studienfach Data Science
set.seed(0902)
Prog_Int[which(Studienfach == "Data Science")] <- sample(1:7, size = length(which(Studienfach == "Data Science")),
                                                         replace = TRUE, prob = c(, , , , , , )) # Wahrscheinlichkeit fehlt
# Studienfach Mathe
set.seed(0902)
Prog_Int[which(Studienfach == "Mathe")] <- sample(1:7, size = length(which(Studienfach == "Mathe")),
                                                  replace = TRUE, prob = c(, , , , , , )) # Wahrscheinlichkeit fehlt
# Studienfach Inforamtik 
set.seed(0902)
Prog_Int[which(Studienfach == "Informatik")] <- sample(1:7, size = length(which(Studienfach == "Informatik")),
                                                       replace = TRUE, prob = c(, , , , , , )) # Wahrscheinlichkeit fehlt







