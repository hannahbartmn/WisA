# Funktionen-R-Skript 2
# Helfer-Funktionen (interne Funktionen)
# muss mindestens eine Funktion enthalten




# Hilfsfunktion fuer Funktion (e)
# Funktion zum Umwandeln einer ordinal skalierten Variable zu einer reellwertigen/numerischen variable

# levels sind die angenommenen Auspraegungen der variable und muessen in der Reihenfolge der natuerlichen Ordnung der variable angegeben werden

ordtonum <- function(x, levels){
  if(length(levels) != length(table(x))){stop("levels muss ein Vektor mit den angenommenen Auspraegungen von x sein")}
  # ueberprueft, ob die Anzahl der levels und die Anzahl der angenommenen Auspraegungen der Variable uebereinstimmen
  
a <- factor(x, levels = levels, labels = 1:length(table(x)))
# Umwandlung der Variable in einen Faktor mit den angegebene levels und mit Stufen von 1 bis zur Anzahl an Auspraegungen
A <- list(levels, a) # fuer die Ausgabe wird eine Liste erstellt mit den levels und dem Faktor, welcher die Stufen anzeigt.
# So sieht man anhand der Reihenfolge direkt, welche Auspraegung welcher Stufe zugeteilt wurde.
names(A)<- c("Auspraegungen:", "umgewandelte Variable:") # die "Ueberschriften" der Listenelemente
A # Ausgabe der Liste
}

# die umgewandelte Variable kann mithilfe von Indizierung aus der Ausgabe extrahiert werden

# Beispiel:
bsp <- c("a", "b", "b", "c", "a", "c", "a", "b")
# ordinalskalierte Variable, hat Ordnung entsprechend des Alphabets

ordtonum(bsp, levels = c("a", "b", "c"))
# $`Auspraegungen:`
# [1] "a" "b" "c"

# $`umgewandelte Variable:`
# [1] 1 2 2 3 1 3 1 2
# Levels: 1 2 3




