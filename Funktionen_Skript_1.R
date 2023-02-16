# Funktionen-R-Skript 1


# Funktionen a, b, c: Alina, Hannah

# Funktion a) 
# Eine Funktion, die verschiedene geeignete deskriptive Statistiken für metrische 
# Variablen berechnet und ausgibt.

# My_Plots gibt bei plot = 0 fuer den eingegebenen Vektor an Daten ein Histogramm, ein Barplot, ein Boxplot und ein Scatterplot.
# Diese Einstellung ist der default, gibt man der Variable plot einen Wert von 1 bis 4, gibt die Funktion fuer
# 1 das Histogramm, fuer 2 den Barplot, fuer 3 den Boxplot und fuer 4 den Scatterplot aus.

random <- rnorm(50, mean = 25, sd = 2)

My_Plots <- function(x, main, plot = 0){
  # x = Daten
  # main = Überschrift für alle Plots 
  if(plot == 0){ 
    par(mfrow = c(2,2)) # vier Grafiken in einen Plot 
    # Die unterschiedlichen Grafiken 
    hist(x, main = main) 
    barplot(table(x), main = main)
    boxplot(x,horizontal = TRUE, main = main)
    plot(x, main = main) 
    par(mfrow = c(1,1)) #setze die parameter zurueck 
  }
  if(plot == 1){
    hist(x, main = main) 
  }
  if(plot == 2){
    barplot(table(x), main = main) 
  }
  if(plot == 3){
    boxplot(x, main = main) 
  }
  if(plot == 4){
    plot(x, main = main) 
  }
}

My_Plots(random, main = "Versuch")

# Funktion b) 
# Eine Funktion, die verschiedene geeignete desktiptive Statistiken für 
# kategoriale Variablen berechnet und ausgibt 

S <- c("KA", "RA", "Abi", "HA") # Ein Datenbeispiel zum ausprobieren 
W <- sample(S, size = 50, replace = TRUE)
O <- sample(1:7, size=50, replace = TRUE) # Ein ordinales Datenbeispiel

# der parameter ordinal kann auf FALSE gesetzt werden, wenn ein Boxplot der eingegebenen Daten keinen Sinn ergibt

My_Plots_2 <- function(x, ordinal = TRUE){
  if(ordinal == TRUE){
    #Die Diagramme
      par(mfrow = c(2,1))
      #Ein Stabdiagramm
      barplot(table(x), main = c("Saeulendiagramm fuer", deparse(substitute(x))), las = 1)
      #Ein Boxplot
      boxplot(x, main = c("Boxplot fuer", deparse(substitute(x))), horizontal = TRUE)
      par(mfrow = c(1,1))
    #Die Haufigkeitstabelle 
      print(table(x, dnn = deparse(substitute(x))))
  }
  
  if(ordinal == FALSE){
    #Das Stabdiagramm
      barplot(table(x), main = c("Balkendiagramm fuer", deparse(substitute(x))), horiz = TRUE, las = 1)
    #Die Häufigkeitstabelle
      print(table(x, dnn = deparse(substitute(x))))
  }
}
My_Plots_2(O, ordinal = TRUE)
My_Plots_2(W, ordinal = FALSE)



## c) 
#Eine Funktion, die geeignete deskriptive bivariate Statistiken fuer den Zusammenhang zwischen 
#zwei kategorialen Variablen berechnet ausgibt. 

#My_Plot_3 gibt zwei Kreuztabellen, eine mit absoluten Haufigkeiten und eine mit relativen Haufigkeiten, in der Konsole aus.

#Datenvektoren zum ausprobieren
O <- sample(1:7, size=50, replace = TRUE)
P <- sample(1:7, size=50, replace = TRUE)

My_Plots_3 <- function(x,y,both.ordinal = FALSE){
  #x erster Datenvektor, y zweiter Datenvektor
  tabelle <- table(x,y,
                   dnn = c(deparse(substitute(x)), deparse(substitute(y)))) #uebernimmt den Namen des Datenvektors
  cat('Tabelle mit absoluten Haufigkeiten: \n\n')
  print(tabelle)
  cat('\n Tabelle mit relativen Haeufigkeiten: \n\n')
  print(prop.table(tabelle))

  #Wenn beide Merkmale ordinal sind, kann man Spearman Korrelation berechnen:
  if(both.ordinal == TRUE){
    c <- cor(x,y,use = 'complete.obs', method = 'spearman')
    cat('\n\n Die Spearman-Korrelation der beiden Merkmale betraegt:', c)
  }
}

My_Plots_3(O,P,both.ordinal = TRUE)









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
  
  
  # als Idee noch einen Table dazu
  #  tabmetdich <- table(met, dich)
  #  names(tabmetdich) <- deparse(substitute(c(met, dich))) 

return(rgabe) # Ausgabe der beiden Summaries
  }







## e)
# Die Funktion kategorisiert eine numerische Variable quantilbasiert in "niedrig", "mittel" und "hoch"

# ordinalskalierte Variablen koennen mit der Hilfsfunktion "ordtonum"
# in Variablen mit numerischen Werten entsprechend ihrer Ordnung
# umgewandelt werden

quantkat <- function(x){
  newob <- NULL # leeres Objekt wird erstellt
uquant <- quantile(as.numeric(names(table(x))), 0.25) # das untere Quartil der sortierten numerischen Werte wird bestimmt
oquant <- quantile(as.numeric(names(table(x))), 0.75) # ebenso das obere Quartil

newob[x <= uquant] <- "niedrig" 
# das untere Quartil dient als obere Grenze der Kategorie "niedrig".
# alles, was unterhalb dieses Quartils liegt, wird der Kategorie "niedrig" zugeordnet.

newob[x > uquant & x < oquant] <- "mittel" 
# die Werte zwischen den beiden Quartilen werden der Kategorie "mittel" zugeordnet

newob[x >= oquant] <- "hoch" 
# das obere Quartil ist die untere Grenze fuer die Kategorie "hoch", alles was darueber liegt, wird "hoch" zugeordnet

newob 
# das in "niedrig", "mittel" und "hoch" kategorisierte Objekt wird ausgegeben.
}




# Beispiel
b <- c(2, 5, 2, 6, 3, 6)

quantkat(b)
# [1] "niedrig" "mittel"  "niedrig" "hoch"    "mittel"  "hoch"





# man koennte auch 0.33 und 0.66 Quantile nehmen, oder eben die Quartile. 
# bei den Quartilen waere die mittlere Kategorie doppelt so gross wie jeweils die Extremen











  
  
##f)

# es werden Visualisierungen fuer drei oder vier Variablen erstellt. Dabei werden zwei Variablen fuer die Achsen verwendet und die dritte durch unterschiedliche
# Farben gezeigt. Die Positionen der drei/ vier Variablen sind dabei fest gewaehlt. Man kann bei drei Variablen nur unterscheiden welche drei von Mathe_Int, Prog_Int, 
# Studienfach und Mathe_LK man gleichzeitig angezeigt werden sollen. Genutzt wird dabei das Paket "ggplot2". 




# muss vorher geladen
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)


Visual_data <- function(D = data, n = 3, Int_Mathe = TRUE, Int_Prog = TRUE, data_Studiengang = TRUE, MatheLK = FALSE){   # n steht dafuer, wie viele Variablen genutzt werden, moeglich sind 3 oder 4
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == FALSE){
    print(ggplot(D,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Mathe_Int),                                                     # Interesse für Mathe auf y-Achse
               x = Prog_Int,                                                              # auf die x-Achse kommt Interesse für Programmieren
               color = Studienfach)) +                                                    # es wird farblich nach Studienfach unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Interesse nach Studienfach",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Interesse an Programmiern", 
           y = "Interesse an Mathe", las = 1) )
  }
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == FALSE & MatheLK == TRUE){
    print( ggplot(D,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Mathe_Int),                                                       # Interesse für Mathe auf y-Achse
               x = Prog_Int,                                                              # auf die x-Achse kommt Interesse für Programmieren
               color = Mathe_LK)) +                                                    # es wird farblich nach Mathe LK unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Interesse nach Mathe LK",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Interesse an Programmiern", 
           y = "Interesse an Mathe", las = 1) )
  }
  if(n == 3 & Int_Mathe == TRUE & Int_Prog == FALSE & data_Studiengang == TRUE & MatheLK == TRUE){
    print( ggplot(D,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Mathe_Int),                                                      # Interesse für Mathe auf y-Achse
               x = Studienfach,                                                              # auf die x-Achse kommt das Studienfach
               color = Mathe_LK)) +                                                    # es wird farblich nach Mathe LK unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Mathe LK nach Studenfach und Int. Mathe",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Studienfach", 
           y = "Interesse an Mathe", las = 1))
  }
  if(n == 3 & Int_Mathe == FALSE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == TRUE){
    print(ggplot(D,                                                                          # ggplot wird vom Datensatz erstellt
           aes(y = factor(Prog_Int),                                                      # Interesse für Programmieren auf y-Achse 
               x = Studienfach,                                                              # auf die x-Achse kommt das Studienfach
               color = Mathe_LK)) +                                                    # es wird farblich nach Mathe LK unterschieden
      geom_jitter(alpha = 1,                                                              # Farbstaerke und Groesse der Punkte im Gitter
                  size = 1.5) + 
      labs(title = "Mathe LK bei Studienfach und Int. Programmieren",                                          # Die jeweiligen Achsen werden beschriftet
           x = "Studienfach", 
           y = "Interesse an Programmieren", las = 1) )
  }
  if(n==4 & Int_Mathe == TRUE & Int_Prog == TRUE & data_Studiengang == TRUE & MatheLK == TRUE){
    print(D %>%
      ggplot(aes(y = Mathe_Int, x = Prog_Int, color = Mathe_LK)) +    # ggplot wird erstellt, bei dem die Interessen auf die Achsen kommen und farblich
      # zwischen Mathe_LK ja oder nein entschieden wird
      geom_point() +                                                 # es werden Punkte erstellt
      facet_wrap(vars(Studienfach)) +                                # es wird nach der vierten Variable/ Studienfach unterschieden
      labs(title ="Abhaengigkeit aller vier Variablen",              # Beschriftung wird hinzugef
           subtitle = "Aufgeteilt nach Studienfach",
           x = "Interesse an Programmieren",
           y = "Interesse an Mathe"))
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




















