# R_Code um den Datensatz runterzuladen 
library (readr)

urlfile <- "https://raw.githubusercontent.com/hannahbartmn/WisA/main/data.csv"

mydata<-read.csv(file = urlfile, sep = ";", header = TRUE)
D <- mydata
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
My_Plots_2(D$Mathe_Int)
# Im Mittel liegt das Interesse an Mathe bei 5.17 und die Standardabweichung bei 1.48. Das Interesse an Mathe in dem Datensatz scheint also sehr hoch zu sein.
# Erkennen kann man dies auch daran, dass bei dem Boxplot 75% der Befragten einen Wert von 4 oder hoeher angegeben haben, wobei 7 sehr hohem Interesse entspricht.
dichmetsummary(D$Mathe_LK, D$Mathe_Int)
# Es scheint ein Zusammenhang zu bestehen zwischen der Wahl eines Mathe-LKs und dem Interesse an Mathe. Diejenigen, die kein Mathe-Lk hatten, haben im Mittel
# einen Wert von 