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








  
  
##f)

# es werden Visualisierungen fuer drei Variablen erstellt. Dabei werden zwei Variablen fuer die Achsen verwendet und die dritte durch unterschiedliche
# Farben gezeigt. Die Positionen der drei Variablen sind dabei fest gewaehlt. Man kann nur unterscheiden welche drei von Mathe_Int, Prog_Int, Studienfach und
# Mathe_LK man gleichzeitig angezeigt werden sollen. Genutzt wird dabei das Paket "ggplot2". 


# noch nicht fertige Funktion, jedoch die Visualisierung fuer 3 Variablen

# muss vorher geladen
install.packages("ggplot2")
library(ggplot2)

Visual_data <- function(data = data, n = 3, Int_Mathe = TRUE, Int_Prog = TRUE, data_Studiengang = TRUE, MatheLK = FALSE){   # n steht dafuer, wie viele Variablen genutzt werden, moeglich sind 3 oder 4
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == FALSE){
    print(ggplot(data,                                                                          # ggplot wird vom Datensatz erstellt
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
           y = "Interesse an Mathe", las = 1) )
  }
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == FALSE & MatheLK == TRUE){
    print( ggplot(data,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Mathe_Int,                                                      # Interesse für Mathe auf y-Achse
                          labels = c("sehr geringes Interesse = 1",                       # Beschriftung für die Punkte
                                     "2", "3", "4", "5", "6",
                                     "sehr hohes Interesse = 7")), 
               x = Prog_Int,                                                              # auf die x-Achse kommt Interesse für Programmieren
               color = Mathe_LK)) +                                                    # es wird farblich nach Mathe LK unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Interesse nach Mathe LK",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Interesse an Programmiern", 
           y = "Interesse an Mathe", las = 1) )
  }
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == FALSE & data_Studiengang == TRUE & MatheLK == TRUE){
    print( ggplot(data,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Mathe_Int,                                                      # Interesse für Mathe auf y-Achse
                          labels = c("sehr geringes Interesse = 1",                       # Beschriftung für die Punkte
                                     "2", "3", "4", "5", "6",
                                     "sehr hohes Interesse = 7")), 
               x = Studienfach,                                                              # auf die x-Achse kommt das Studienfach
               color = Mathe_LK)) +                                                    # es wird farblich nach Mathe LK unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Mathe LK nach Studenfach und Int. Mathe",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Studienfach", 
           y = "Interesse an Mathe", las = 1))
  }
  if(n == 3 & Int_Mathe == FALSE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == TRUE){
    print(ggplot(data,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Prog_Int,                                                      # Interesse für Programmieren auf y-Achse
                          labels = c("sehr geringes Interesse = 1",                       # Beschriftung für die Punkte
                                     "2", "3", "4", "5", "6",
                                     "sehr hohes Interesse = 7")), 
               x = Studienfach,                                                              # auf die x-Achse kommt das Studienfach
               color = Mathe_LK)) +                                                    # es wird farblich nach Mathe LK unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Mathe LK bei Studienfach und Int. Programmieren",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Studienfach", 
           y = "Interesse an Programmieren", las = 1) )
  }
  if(n==4 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == TRUE){
    # Hier muss der Code fuer vier Variablen rein
  }
# Hier werden die Warnings geschrieben, falls die Variablen unzulaessig sind  
  
  # bei den Fall das n=4 ist, wird eine Warnung gegeben, falls nicht alle vier Folgevariablen TRUE gesetzt wurden
  if(n == 4 & !(Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == TRUE )){    
    warning("Bei n=4 muessen alle vier Variablen auf TRUE gesetzt werden")
  }
  # bei den Fall das n=3 ist:
  # falls vier statt drei Variablen TRUE gesetzt wurden
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == TRUE){
    warning("Bei n=3 duerfen nur drei Variablen TRUE sein")
  }
  # falls zu wenig Variablen TRUE gesetzt wurden
  if(n== 3 & (Int_Mathe == FALSE & Int_Prog == FALSE & data_Studiengang == TRUE & MatheLK == TRUE | Int_Mathe == FALSE & Int_Prog == TRUE & data_Studiengang == FALSE & MatheLK == TRUE |
              Int_Mathe == FALSE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == FALSE | Int_Mathe == TRUE & Int_Prog == FALSE & data_Studiengang == FALSE & MatheLK == TRUE | 
              Int_Mathe == TRUE & Int_Prog == FALSE & data_Studiengang == TRUE & MatheLK == FALSE | Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == FALSE & MatheLK == FALSE |
              Int_Mathe == FALSE & Int_Prog == FALSE & data_Studiengang == FALSE & MatheLK == FALSE | Int_Mathe == FALSE & Int_Prog == FALSE & data_Studiengang == FALSE & MatheLK == TRUE |
              Int_Mathe == FALSE & Int_Prog == FALSE & data_Studiengang == TRUE & MatheLK == FALSE | Int_Mathe == FALSE & Int_Prog == TRUE & data_Studiengang == FALSE & MatheLK == FALSE |
              Int_Mathe == TRUE & Int_Prog == FALSE & data_Studiengang == FALSE & MatheLK == FALSE )){
    warning("Bei n=3 muessen genau drei Variablen TRUE sein")
  }
  
  # falls das n selbst unzulaessig ist
  if(n != 3 & n!= 4){
    warning("Das n ist unzulaessig. Es wird nur n=3 oder n=4 angenommen")
  }
 }




















