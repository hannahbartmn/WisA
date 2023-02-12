# Funktionen-R-Skript 1


# Funktionen a, b, c: Alina, Hannah

























# Funktionen  d, e, f: Maike, Julia



## d)

# Grundgeruest der Funktion fuer Zusammenhang zwischen einer metrischen und einer dichotomen Variable

# Es werden 2 Summaries der metrischen Variable ausgegeben, getrennt nach den beiden Auspraegungen der dichotomen Variable
# Eingabe: fuer "dich" muss eine dichotome, fuer "met" eine metrische Variable eingegeben werden
# "dich" und "met" sollten zwei Variablen sein, deren n-te Eintraege jeweils von derselben Person stammen

dichmetsummary <- function(dich, met){
  
  if(length(table(dich)) != 2){stop("dich muss eine dichotome Variable sein")} 
     #ueberprueft, ob "dich" genau zwei Auspraegungen hat
  
  if(!is.numeric(met)){stop("met muss eine metrische Variable sein")} 
     # ueberprueft, ob "met" eine metrische variable ist
  
  if(length(met) != length(dich)){stop("dich und met muessen gleiche Laenge haben")} 
     # uberprueft, ob "dich" und "met" die gleiche Laenge haben.


dichfact <- factor(dich, levels = c(names(table(dich))), labels = c(0, 1))
# Umwandlung der dichotomen variable in einen Factor
  
# die Summaries werden als 2-elementige Liste zurueckgegeben:
rgabe <- list(
  # Summary der metrischen Variable nach der ersten im table ausgefuehrten Auspraegung der dichotomen Variable
  summary(met[dichfact == 0]), 
  
  # Summary der metrischen Variable nach der zweiten im table ausgefuehrten Auspraegung der dichotomen Variable
  summary(met[dichfact == 1])
)


names(rgabe) <- c(names(table(dich))[1], names(table(dich))[2])
# zu jedem Listenelement wird hinzugefuegt, auf welcher Auspraegung der dichotomen Variable die Summary basiert

return(rgabe) # Ausgabe der beiden Summaries
  }

  
  




















