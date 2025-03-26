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


#anderer ansatz, ausprobiert für utaut2

library(dplyr)

utaut2_cols <- c(names(data)[14:37])

likert_levels <- c("Stimme überhaupt nicht zu", 
                   "Stimme nicht zu",
                   "Stimme nicht ganz zu", 
                   "Weder noch", 
                   "Stimme ein wenig zu", 
                   "Stimme zu",
                   "Stimme absolut zu")


data <- data %>%
  mutate(across(all_of(utaut2_cols), ~ as.numeric(factor(.x, levels = likert_levels, ordered = TRUE))))


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


#mehr visualisierung

library(ggplot2)

ggplot(combinedData, aes(x = mean_response_ueqs, 
                         y = mean_response_aint, 
                         color = group)) +
  geom_point() +                               # Plot raw data points
  geom_smooth(method = "lm", se = TRUE) +       # Add regression lines with confidence bands
  labs(title = "Application Intention vs. Website Quality",
       x = "Website Quality",
       y = "Application Intention")

ggplot(combinedData, aes(x = mean_response_ueqs, y = mean_response_aint)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ group)



#utaut2 mean über die einzelnen faktoren

data$PE <- rowMeans(data[, c("PEGA.PE1GA.","PEGA.PE2GA.","PEGA.PE3GA.","PEGA.PE4GA.")], na.rm = TRUE)
data$EE <- rowMeans(data[, c("EEGA.EEGA1.","EEGA.EEGA2.","EEGA.EEGA3.","EEGA.EEGA4.")], na.rm = TRUE)
data$SI <- rowMeans(data[, c("SIGA.SIGA1.","SIGA.SIGA2.","SIGA.SIGA3.")], na.rm = TRUE)
data$HM <- rowMeans(data[, c("HMGA.HMGA1.","HMGA.HMGA2.","HMGA.HMGA3." )], na.rm = TRUE)
data$FC <- rowMeans(data[, c("FCGA.FCGA1.","FCGA.FCGA2.","FCGA.FCGA3.","FCGA.FCGA4.")], na.rm = TRUE)
data$BI <- rowMeans(data[, c("BIGA.BIGA1.","BIGA.BIGA2.","BIGA.BIGA3.")], na.rm = TRUE)
data$UB <- rowMeans(data[, c("UBGA.UBGA1.","UBGA.UBGA2.","UBGA.UBGA3.")], na.rm = TRUE)

#u2 sem

library(lavaan)


library(lavaan)

model <- '
  
  # Structural model
  BI ~ PE + EE + SI + FC + HM
'
fit <- sem(model, data = data)
summary(fit, fit.measures = TRUE)










