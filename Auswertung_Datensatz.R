# R_Code um den Datensatz runterzuladen 
library (readr)

urlfile <- "https://raw.githubusercontent.com/hannahbartmn/WisA/main/data.csv"

D <-read.csv(file = urlfile, sep = ";", header = TRUE)
View(D)

#####
# erstes Angucken des Datensatz: 

str(D)
# Der Datensatz enthaelt die sechs Variabeln ID (int), Alter (int), Studienfach (chr), Mathe_Int (int), Prog_Int (int), Mathe_LK (chr)
# Alter ist metrisch: Funktion a, d, e
# Studienfach ist nominal mit Auspraegungen Data Science, Informatik, Mathe und Statistik: Funktionen b, c, f
# Mathe_Int und Prog_Int sind beide metrisch auf einer Skala von 1 bis 7: Funktionen b, c, d, e
# 1 = sehr geringes Interesse, und 7 = sehr hohes Interesse
# Mathe_LK ist dichotom nominalskaliert: b, c, d, f

#####

table(quantkat(D$Mathe_Int))
table(quantkat(D$Prog_Int))
# Der Anteil der Personen, die ein niedriges Interesse an Mathe bzw. Programmieren angeben, liegt bei 13% bzw. 9%.
Visual_data(D = D, MatheLK = TRUE, data_Studiengang = FALSE)
#Visualisiert man die Angabe nach Interesse und Mathe-Lk, scheint das Interesse an Mathe durch die Angabe Mathe-Lk = ja beguenstigt zu sein.

My_Plots_2(D$Mathe_Int)
# Im Mittel liegt das Interesse an Mathe bei 5.17 und die Standardabweichung bei 1.48. Das Interesse an Mathe in dem Datensatz scheint also sehr hoch zu sein.
# Erkennen kann man dies auch daran, dass bei dem Boxplot 75% der Befragten einen Wert von 4 oder hoeher angegeben haben, wobei 7 sehr hohem Interesse entspricht.
dichmetsummary(D$Mathe_LK, D$Mathe_Int)
# Es scheint ein Zusammenhang zu bestehen zwischen der Wahl eines Mathe-LKs und dem Interesse an Mathe. Diejenigen, die kein Mathe-Lk hatten, haben bezueglich des 
# Interesses im Mittel einen Wert von 4.622 und die mittleren 50% liegen zwischen den Werten 4 und 6. An der Tabelle erkennt man, dass bei dieser Gruppe haeufig
# der mittlere Wert 4 gewaehlt wurde.
# Bei denjenigen, die Mathe-Lk hatten, liegt das mittlere Interesse bei 5.618 und die mittleren 50 % liegen zwischen den Werten 5 und 7.
# Es laesst sich festhalten, dass diejenigen, die Mathe-Lk hatten, einen hoeheres Interesse aufweisen, als diejenigen, die kein Mathe-Lk hatten, auch wenn dort das
# Interesse auch nicht niedrig ist. 

My_Plots_2(D$Prog_Int)
# Im Mittel liegt das Interesse an Programmieren bei 5.19 also quasi genauso hoch wie das Interesse an Mathe. Die Werte streuen mit einer Standardabweichung von
# 1.56 etwas mehr. Ausserdem gibt es weniger Auspraegungen von dem hoechsten Werte: Bei Interesse an Mathe lag das obere Quantil bei 7 und hier bei dem Interesse
# an Programmieren liegt das obere QUantil bei 6.
dichmetsummary(D$Mathe_LK, D$Prog_Int)
# Bei dem Interesse am Programmieren und dem Mathe-Lk liegt nicht so eine grosse Verbingung wie bei dem Intersse an Mathe. Waehrend das Interesse bei der Gruppe, die 
# an Mathe hatten im Mittel um ca. einen Punkt hoeher war, liegt die Differenz im Mittel nur bei 0.3 bei dem Interesse an Programmieren.

My_Plots_3(D$Mathe_Int, D$Prog_Int, both.ordinal = TRUE)
# Die Korrelation zwischen Interesse an Mathe und Interesse an Programmieren liegt aber mit -0.055 nicht sehr hoch. Das Interesse an Mathe und Programmieren scheint
# nicht miteinander verknuepft zu sein. 

#####

# Altersverteilung 
mean(D$Alter)
sd(D$Alter)
My_Plots(D$Alter, plot = 2)
legend("topleft", legend = c("mean = 24.94", "     sd = 2.35"))
# Das Durchschnittssalter liegt bei ca 25 und die Standartabweichung bei 2.35

# Verteilung der Studienfächer
My_Plots_2(D$Studienfach, ordinal = FALSE)
# Um sich die Häufigkeiten der einzelnen Studiengänge anzuschauen 

# Studium/ Mathe und Programmieren Interesse 

My_Plots_3(D$Mathe_Int, D$Studienfach == "Mathe")
My_Plots_3(D$Mathe_Int, D$Studienfach == "Statistik")
My_Plots_3(D$Mathe_Int, D$Studienfach == "Informatik")
My_Plots_3(D$Mathe_Int, D$Studienfach == "Data Science")
# Nur Personen mit einem mittelmäßigen oder hohen Intersesse an Mathe, 
# studieren auch Data Science  
# In den Studiengängen Mathe, Statistik und Data Science herrscht ein 
# mittelmäßiges bis hohes interesse an Mathe. Beim Studiengang Informatik 
# nur ein mittelmäßiges 

My_Plots_3(D$Prog_Int, D$Studienfach == "Data Science")
My_Plots_3(D$Prog_Int, D$Studienfach == "Informatik")
My_Plots_3(D$Prog_Int, D$Studienfach == "Statistik")
My_Plots_3(D$Prog_Int, D$Studienfach == "Mathe")
# Besonders in den Studiengängen Informatik und Data Science liegt ein großes 
# Interesse am programmieren vor 

Visual_data(D = D, n = 3, Int_Mathe = TRUE, Int_Prog = TRUE, data_Studiengang = TRUE)
# Hier die Ergenisse nochmal alle in einem Plot dargestellt 

#######
# Zusammenfassung der Ergebnisse 

# Durchschnittsalter: 25 Jahre, Std.abweichung von 2.35

# Studienfächer: Statistik ist am häufigsten vertreten (36%), dann Data Science (28%), dann Informatik (22%)
# und Mathe ist am wenigsten vertreten (14%)

# Es besteht ein Zusammenhang zwischen dem Interesse an Mathe/Programmieren mit dem Studiengang: 
# In den Studiengängen Mathe, Statistik und Data Science herrscht ein mittelmäßiges bis hohes interesse an Mathe,
# während beim Studiengang Informatik nur ein mittelmäßiges Interesse an Mathe besteht.
# Bei den Studiengängen Informatik und Data Science liegt ein großes Interesse am Programmieren vor.

# Diejenigen, die Mathe-Lk hatten scheinen ein stärkeres Interesse an Mathe zu haben als diejenigen ohne Mathe-Lk.

# Der Mathe-Lk scheint aber keinen/bzw. nur einen geringen Einfluss auf das Interesse an Programmieren zu haben.

# Es besteht keine Korrelation zwischen dem Interesse an Mathe und dem Interesse an Programmieren.








