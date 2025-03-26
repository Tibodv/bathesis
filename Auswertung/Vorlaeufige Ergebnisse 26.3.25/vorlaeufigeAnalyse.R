#housekeeping
setwd("C:/Users/tiboa/Documents/BaThesis/Auswertung/Vorlaeufige Ergebnisse 26.3.25")

library(dplyr)




#import data
experimentalGruppe <- read.csv("ergebnisseExperimental_26.3.25.csv", stringsAsFactors = FALSE)

kontrollGruppe <- read.csv("ergebnisseKontrolle_26.3.25.csv", stringsAsFactors = FALSE)

# Check the first few rows of each dataset
head(experimentalGruppe)
head(kontrollGruppe)

# Check the structure (data types, columns, etc.)
str(experimentalGruppe)
str(kontrollGruppe)

#UEQ-S means pro person
#dafür ein neuer df um die daten nicht zu verpesten

data <- experimentalGruppe

data$mean_response_ueqs <- rowMeans(experimentalGruppe[, 6:13], na.rm = TRUE)

dataKontroll <- kontrollGruppe

dataKontroll$mean_response_ueqs <- rowMeans(kontrollGruppe[, 6:13], na.rm = TRUE)



#application intention means erstellen
#likert coding zu zahlen
#experimentalgruppe
data$aint.SQ001. <- factor(data$aint.SQ001.,
                        levels = c("Stimme überhaupt nicht zu", "Stimme nicht zu", "Stimme nicht ganz zu", "Weder noch",
                                   "Stimme ein wenig zu", "Stimme zu", "Stimme absolut zu"),
                        ordered = TRUE)
data$aint1_numeric <- as.numeric(data$aint.SQ001.)

data$aint.SQ002. <- factor(data$aint.SQ002.,
                           levels = c("Stimme überhaupt nicht zu", "Stimme nicht zu", "Stimme nicht ganz zu", "Weder noch",
                                      "Stimme ein wenig zu", "Stimme zu", "Stimme absolut zu"),
                           ordered = TRUE)
data$aint2_numeric <- as.numeric(data$aint.SQ002.)

data$aint.SQ003. <- factor(data$aint.SQ003.,
                           levels = c("Stimme überhaupt nicht zu", "Stimme nicht zu", "Stimme nicht ganz zu", "Weder noch",
                                      "Stimme ein wenig zu", "Stimme zu", "Stimme absolut zu"),
                           ordered = TRUE)
data$aint3_numeric <- as.numeric(data$aint.SQ003.)

#kontrollgruppe

dataKontroll$aint.SQ001. <- factor(dataKontroll$aint.SQ001.,
                           levels = c("Stimme überhaupt nicht zu", "Stimme nicht zu", "Stimme nicht ganz zu", "Weder noch",
                                      "Stimme ein wenig zu", "Stimme zu", "Stimme absolut zu"),
                           ordered = TRUE)
dataKontroll$aint1_numeric <- as.numeric(dataKontroll$aint.SQ001.)

dataKontroll$aint.SQ002. <- factor(dataKontroll$aint.SQ002.,
                                   levels = c("Stimme überhaupt nicht zu", "Stimme nicht zu", "Stimme nicht ganz zu", "Weder noch",
                                              "Stimme ein wenig zu", "Stimme zu", "Stimme absolut zu"),
                                   ordered = TRUE)
dataKontroll$aint2_numeric <- as.numeric(dataKontroll$aint.SQ002.)

dataKontroll$aint.SQ003. <- factor(dataKontroll$aint.SQ003.,
                                   levels = c("Stimme überhaupt nicht zu", "Stimme nicht zu", "Stimme nicht ganz zu", "Weder noch",
                                              "Stimme ein wenig zu", "Stimme zu", "Stimme absolut zu"),
                                   ordered = TRUE)
dataKontroll$aint3_numeric <- as.numeric(dataKontroll$aint.SQ003.)


#mean aus den zahlen

data$mean_response_aint <- rowMeans(data[, 45:47], na.rm = TRUE)

dataKontroll$mean_response_aint <- rowMeans(dataKontroll[, 21:23], na.rm = TRUE)


#kombination aus beiden dataframes für weitere analysen
# !achtung: bind_rows ist eine funktion von dplyr

data$group <- "experimental"
dataKontroll$group <- "control"

combinedData <- bind_rows(data,dataKontroll)

# t test ueqs
#geschrieben bevor ich die dfs kombiniert habe, frag nicht

t_test_result <- t.test(data$mean_response_ueqs, dataKontroll$mean_response_ueqs, alternative = "two.sided", var.equal = FALSE)
print(t_test_result)


# t test von application intention gegeneinander


t_test_result <- t.test(mean_response_aint ~ group, data = combinedData, 
                        alternative = "two.sided", var.equal = FALSE)
print(t_test_result)

#korrelationen aint und ueqs score

# For experimental group:
cor_experimental <- cor(data$mean_response_aint, data$mean_response_ueqs, method = "pearson")

# For control group:
cor_control <- cor(dataKontroll$mean_response_aint, dataKontroll$mean_response_ueqs, method = "pearson")


# visualisierung mit ggplot (tidyverse)

library(ggplot2)

ggplot(combinedData, aes(x = mean_response_ueqs, y = mean_response_aint, color = group)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Application Intention vs. Website Quality",
       x = "Website Quality",
       y = "Application Intention")

#model interaction website quality und application intention

model_aintVsueqs <- lm(mean_response_aint ~ mean_response_ueqs * group, data = combinedData)
summary(model)


















