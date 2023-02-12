# Erstellung des Datensatzes
# Maike und Julia



# TOP SECRET





# ACHTUNG Hannah und Alina nicht weiterlesen :)





# Variablen:


# hier wird die ID erstellt
ID <- 1:100   # durchzaehlen von 1 bis 100



# Alter
# Normalverteilung, EW 25, sd 2
set.seed(2)
Alter <- round(rnorm(100, mean = 25, sd = 2))
# 100 Zufallszahlen aus der Normalverteilung gezogen mit EW 25, sd 2
# gerundet auf volle Jahre


# Studienfach 
# Statistik und Data Science gleiche Wkeit, Informatik geringere Wkeit,
# Mathe geringste Wkeit
set.seed(5)
Studienfach <- sample(c("Statistik", "Data Science", "Informatik", "Mathe"), 
       100, replace = TRUE, prob = c(0.35, 0.35, 0.2, 0.1))
# 100 Ziehungen aus den vier Studienfächern, mit den Wahrscheinlichkeiten
# Statistik & Data Science jeweils 0.35, Informatik 0.2, Mathe 0.1






# Interesse an Mathe  
# hier wird ein Vektor erstellt, bei dem je nach Studienfach die Wahrscheinlichkeit am Interesse an Mathe variiert 
# hierbei steht 1 fuer sehr geringes Interesse und 7 fuer sehr hohes Interesse.


Mathe_Int <-NULL   # Es wird ein leerer Vektor erstellt


# Die Zuordnung fuer das Studienfach Statistik mit verschiedenen Wahrscheinlichkeiten
set.seed(0902)
Mathe_Int[which(Studienfach == "Statistik")] <- sample(1:7, size = length(which(Studienfach == "Statistik")),  # fuer das Sampling wird die Anzahl der Studienfaecher gezaehlt
                                                      replace = TRUE, prob = c(0, 0.01, 0.09, 0.15, 0.2, 0.25, 0.3))  


# Studienfach Data Science
set.seed(1002)
Mathe_Int[which(Studienfach == "Data Science")] <- sample(1:7, size = length(which(Studienfach == "Data Science")),
                                                          replace = TRUE, prob = c(0.0001, 0.0049, 0.005, 0.2, 0.24, 0.3, 0.25)) 

# Studienfach Mathe
set.seed(1102)
Mathe_Int[which(Studienfach == "Mathe")] <- sample(1:7, size = length(which(Studienfach == "Mathe")),
                                                   replace = TRUE, prob = c(0, 0, 0.05, 0.1, 0.15, 0.3, 0.4))  

# Studienfach Informatik
set.seed(1202)
Mathe_Int[which(Studienfach == "Informatik")] <- sample(1:7, size = length(which(Studienfach == "Informatik")),
                                                        replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.25, 0.2, 0.15, 0.1))  





# Interesse an Programmieren 
# hier wird ein Vektor erstellt, bei dem je nach Studienfach die Wahrscheinlichkeit am Interesse an Programmieren variiert 
# hierbei steht 1 fuer sehr geringes Interesse und 7 fuer sehr hohes Interesse.


Prog_Int <- NULL   # Es wird ein leerer Vektor erstellt


# Studienfach Statistik, die Anzahl wird hier wieder je nach Groesse der Anzahl der Studienfachs bestimmt
set.seed(99)
Prog_Int[which(Studienfach == "Statistik")] <- sample(1:7, size = length(which(Studienfach == "Statistik")),   
                                                      replace = TRUE, prob = c(0, 0.05, 0.1, 0.15, 0.25, 0.25, 0.2))   
# Studienfach Data Science
set.seed(100)
Prog_Int[which(Studienfach == "Data Science")] <- sample(1:7, size = length(which(Studienfach == "Data Science")),
                                                         replace = TRUE, prob = c(0.05, 0.1, 0.1, 0.15, 0.15, 0.2, 0.25)) 
# Studienfach Mathe
set.seed(64)
Prog_Int[which(Studienfach == "Mathe")] <- sample(1:7, size = length(which(Studienfach == "Mathe")),
                                                  replace = TRUE, prob = c(0.05, 0.15, 0.25, 0.25, 0.15, 0.075, 0.075)) 
# Studienfach Inforamtik 
set.seed(23)
Prog_Int[which(Studienfach == "Informatik")] <- sample(1:7, size = length(which(Studienfach == "Informatik")),
                                                       replace = TRUE, prob = c(0, 0, 0.01, 0.09, 0.15, 0.3, 0.45)) 




# Mathe LK

# Idee: Zusammenhang nur zwischen Mathe LK und Interesse an Mathematik.
# Interesse an Mathematik haengt bereits in gewisser Weise mit Studienfach zusammen 

Mathe_LK <- NULL # leerer Vektor

# Ziehung von 100mal ja/nein mit Wkeit abhaengig davon, ob man Interesse an Mathe groesser oder kleiner gleich 4 angegeben hat

set.seed(9)
Mathe_LK[Mathe_Int > 4] <- sample(c("ja", "nein"), length(Mathe_Int[Mathe_Int > 4]), replace = TRUE, prob = c(0.75, 0.25))
# wenn das Interesse an Mathematik groesser als 4 ist, ist die Wkeit, Mathe_LK gehabt zu haben, 0.75


set.seed(12)
Mathe_LK[Mathe_Int <= 4] <- sample(c("ja", "nein"), length(Mathe_Int[Mathe_Int <= 4]), replace = TRUE, prob = c(0.35, 0.65))
# wenn das Interesse an Mathematik kleiner gleich 4 ist, ist die Wkeit, Mathe_LK gehabt zu haben, 0.35







# Dataframe erstellen

data <- data.frame(ID = ID, Alter = Alter, Studienfach = Studienfach, Mathe_Int = Mathe_Int, Prog_Int = Prog_Int, Mathe_LK = Mathe_LK)



