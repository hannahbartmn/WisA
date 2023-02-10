# Erstellung des Datensatzes
# Maike und Julia



# TOP SECRET





# ACHTUNG Hannah und Alina nicht weiterlesen :)








# Variablen:
# Alter
# Normalverteilung, EW 25, sd 2

set.seed(2)
Alter <- round(rnorm(100, mean = 25, sd = 2))
# 100 Zufallszahlen aus der Normalverteilung gezogen mit EW 25, sd 2
# gerundet auf volle Jahre


# Studienfach 
# Wahrscheinlichkeiten muessen noch eintragen werden: 
# Statistik und Data Science gleiche Wkeit, Informatik geringere Wkeit,
# Mathe geringste Wkeit
set.seed(5)
Studienfach <- sample(c("Statistik", "Data Science", "Informatik", "Mathe"), 
       100, replace = TRUE, prob = c())
# 100 Ziehungen aus den vier Studienfächern, mit den angegebenen Wahrscheinlichkeiten









# Mathe LK

# Idee: Zusammenhang nur zwischen Mathe LK und Interesse an Mathematik.
# Interesse an Mathematik haengt vermutlich bereits in gewisser Weise mit Studienfach zusammen 

Mathe_LK <- NULL
set.seed(9)
# default mal als <4 fuer hoehere Weit, Mathe LK gehabt zu haben, koennen wir noch abaendern
Mathe_LK[Mathe_Int > 4] <- sample(c("ja", "nein"), length(Mathe_Int[Mathe_Int > 4]), replace = TRUE, prob = c())

Mathe_LK[Mathe_Int <= 4] <- sample(c("ja", "nein"), length(Mathe_Int[Mathe_Int <= 4]), replace = TRUE, prob = c())
# Wkeit fuer ja/nein noch eintragen
# Ziehung von 100mal ja/nein mit Wkeit abhaengig davon, ob man Interesse an Mathe groesser oder kleiner gleich 4 angegeben hat







