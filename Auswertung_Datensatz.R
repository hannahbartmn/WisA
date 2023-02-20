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
# Alter ist metrisch: Funktion a, FUnktion d, Funktion e
table(D$Studienfach)
# Studienfach ist nominal mit Auspraegungen Data Science, Informatik, Mathe und Statistik: Funktionen b, c, f
table(D$Mathe_Int)
table(D$Prog_Int)
# Mathe_Int und Prog_Int sind beide metrisch auf einer Skala von 1 bis 7: Funktionen a, b, c, d, e
# 1 = sehr geringes Interesse, und 7 = sehr hohes Interesse
table(D$Mathe_LK)
# Mathe_LK ist dichotom nominalskaliert: b, c, d, f

#####
My_Plots(D$Mathe_Int)
