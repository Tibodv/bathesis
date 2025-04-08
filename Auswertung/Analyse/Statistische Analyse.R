#Statistische Analyse für BA - Tibo de Vries - R Version: 4.4.2
#Struktur ----
#(0. Housekeeping)
#1.  Datenimport
#2.  Kodierung
#3.  Mittelwertbildung/person für U2 und UEQ-S
#4.  Analyse der Stichprobe
#5.  Lageparameter, Streuungsparameter und Verteilung zentraler Variablen
#6.  Reliabilitätsanalyse -> Cronbachs Alpha für beide Konstrukte
#7.  Between Subjects Analyse für UEQ-S -> T-Test, Varianzen, Verteilung, Korrelation mit Application Intention
#8.  UTUAUT2 Analyse (PLS-SEM, vllt Multivariate Analyse, Modelqualitätskennzahlen)
#9. Regressionsanalyse UTAUT2 und Application Intention
#10. Visualisierung
#



# 0. Housekeeping  ----
#working directory
#setwd("C:/Users/tiboa/Documents/BaThesis/Auswertung/Vorlaeufige Ergebnisse 7.4.25")
#library import
library(dplyr)
library(ggplot2)
library(lavaan)
#library(tidySEM)
library(psych)
library(plspm)
library(seminr)

#Funktion zur Berechnung zentraler Kennzahlen

descriptive_stats <- function(x) {
  stats <- list(
    Mittelwert = mean(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Standardabweichung = sd(x, na.rm = TRUE),
    Varianz = var(x, na.rm = TRUE),
    Minimum = min(x, na.rm = TRUE),
    Maximum = max(x, na.rm = TRUE)
  )
  return(stats)
}



#1. Datenimport ------

experimentalGruppe <- read.csv("ExperimentalGruppe_7.4.25.csv", stringsAsFactors = FALSE)
kontrollGruppe <- read.csv("KontrollGruppe_7.4.25.csv", stringsAsFactors = FALSE)

#Kombinierter DF
experimentalGruppe$gruppe <- "experimental"
kontrollGruppe$gruppe <- "kontroll"
combinedDF <- bind_rows(experimentalGruppe,kontrollGruppe)

#umbennennung demographische daten

combinedDF <- combinedDF %>%
  rename(
    alter = G03Q03,
    geschlecht = G03Q04,
    berufsstand = G03Q05
  )

#2. Kodierung  ------
#likertkodierung
likert_levels <- c("Stimme überhaupt nicht zu", 
                   "Stimme nicht zu",
                   "Stimme nicht ganz zu", 
                   "Weder noch", 
                   "Stimme ein wenig zu", 
                   "Stimme zu",
                   "Stimme absolut zu")


likert_cols <- c(names(combinedDF)[14:40])

combinedDF <- combinedDF %>%
  mutate(across(all_of(likert_cols), ~ as.numeric(factor(.x, levels = likert_levels, ordered = TRUE))))

#auch für experimental, weil der df später noch gebraucht wird
likert_cols <- c(names(experimentalGruppe)[14:40])

experimentalGruppe <- experimentalGruppe %>%
  mutate(across(all_of(likert_cols), ~ as.numeric(factor(.x, levels = likert_levels, ordered = TRUE))))







#3. Mittelwertbildung für UEQ-S und UTAUT2 Konstrukte  ------
# * UEQ-S  ----

combinedDF$mean_response_ueqs <- rowMeans(combinedDF[, 6:13], na.rm = TRUE)

# * UTAUT2 Konstrukte  --------

combinedDF$PE <- rowMeans(combinedDF[, 14:17])
combinedDF$EE <- rowMeans(combinedDF[, 18:21])
combinedDF$SI <- rowMeans(combinedDF[, 22:24])
combinedDF$HM <- rowMeans(combinedDF[, 25:27])
combinedDF$FC <- rowMeans(combinedDF[, 28:31])
combinedDF$BI <- rowMeans(combinedDF[, 32:34])
combinedDF$UB <- rowMeans(combinedDF[, 35:37])

# * AINT Mittelwert  ----
combinedDF$mean_response_aint <- rowMeans(combinedDF[,38:40], na.rm = TRUE)

#4.  Analyse der Stichprobe  ----------

# * Anzahl  ------
n_total <- nrow(combinedDF)
n_experimental <- sum(combinedDF$gruppe == "experimental")
n_kontroll <- sum(combinedDF$gruppe == "kontroll")

# * Alter  ------

summaryAlter <- summary(combinedDF$alter)
sdAlter <- sd(combinedDF$alter, na.rm = TRUE)

# * Geschlecht ----

summaryGeschlecht <- table(combinedDF$geschlecht)




#5.  Lageparameter, Streuungsparameter und Verteilung zentraler Variablen  ------

#Kennzahlen für UEQ-S mithilfe der früher definierten Funktion
cat("Deskriptive Statistik für UEQ-S (mean_response_ueqs):\n")
ueqs_stats <- descriptive_stats(combinedDF$mean_response_ueqs)
print(ueqs_stats)

#Liste der UTAUT2-Dimensionen
utaut_vars <- c("PE", "EE", "SI", "HM", "FC", "BI", "UB")

#Schleife über die UTAUT2-Dimensionen mithilfe der früher definierten Funktion
for (var in utaut_vars) {
  cat("\nDeskriptive Statistik für", var, ":\n")
  stats <- descriptive_stats(combinedDF[[var]])
  print(stats)
}

# Normalitätstest für UEQ-S
shapiro_result <- shapiro.test(combinedDF$mean_response_ueqs)
cat("\nShapiro-Wilk-Test für mean_response_ueqs:\n")
print(shapiro_result)


#6.  Reliabilitätsanalyse -> Cronbachs Alpha für beide Konstrukte  --------


# 6.1 Cronbach's Alpha für die UEQ-S Skala (Items in den Spalten 6 bis 13)
cat("Reliabilitätsanalyse für UEQ-S:\n")
alpha_ueqs <- psych::alpha(combinedDF[, 6:13])
print(alpha_ueqs)
alpha_ueqs_value <- alpha_ueqs$total$raw_alpha

# 6.2 Cronbach's Alpha für die einzelnen UTAUT2 Konstrukte
utaut2_scales <- list(
  PE = combinedDF[, 14:17],
  EE = combinedDF[, 18:21],
  SI = combinedDF[, 22:24],
  HM = combinedDF[, 25:27],
  FC = combinedDF[, 28:31],
  BI = combinedDF[, 32:34],
  UB = combinedDF[, 35:37]
)

# Schleife über die UTAUT2-Dimensionen
for (scale in names(utaut2_scales)) {
  cat("\nReliabilitätsanalyse für", scale, ":\n")
  alpha_result <- psych::alpha(utaut2_scales[[scale]])
  print(alpha_result)
}

utaut2_alphas <- sapply(utaut2_scales, function(x) {
  psych::alpha(x)$total$raw_alpha
})

#Ausgabe von allen Cronbach Alphas
print(alpha_ueqs_value)
print(utaut2_alphas)


#7.  Between Subjects Analyse für UEQ-S -> T-Test, Varianzen, Verteilung, Korrelation mit Application Intention  -------

# 7.1 T-Test: Vergleiche der UEQ-S Scores zwischen Experimental- und Kontrollgruppe
t_test_result <- t.test(mean_response_ueqs ~ gruppe, data = combinedDF)
cat("T-Test Ergebnisse für UEQ-S:\n")
print(t_test_result)

# 7.2 Vergleich der Varianzen zwischen den Gruppen (F-Test)
var_test_result <- var.test(mean_response_ueqs ~ gruppe, data = combinedDF)
cat("\nVarianzenvergleich (F-Test):\n")
print(var_test_result)

# 7.3 Überprüfung der Normalverteilung in beiden Gruppen
# Shapiro-Wilk-Test für die Experimentalgruppe
cat("\nShapiro-Wilk-Test für UEQ-S in der Experimentalgruppe:\n")
print(shapiro.test(combinedDF$mean_response_ueqs[combinedDF$gruppe == "experimental"]))

# Shapiro-Wilk-Test für die Kontrollgruppe
cat("\nShapiro-Wilk-Test für UEQ-S in der Kontrollgruppe:\n")
print(shapiro.test(combinedDF$mean_response_ueqs[combinedDF$gruppe == "kontroll"]))

# 7.4 Korrelation zwischen UEQ-S und Application Intention (AINT)
# Hier wird der Pearson-Korrelationskoeffizient berechnet
cor_test_result <- cor.test(combinedDF$mean_response_ueqs, combinedDF$mean_response_aint, method = "pearson")
cat("\nKorrelation zwischen UEQ-S und Application Intention:\n")
print(cor_test_result)



#8.  UTUAUT2 Analyse (PLS-SEM, vllt Multivariate Analyse, Modelqualitätskennzahlen)  --------

# * monte carlo simulation für die optimale stichprobe bei 80% power ----


# * Maximum Likelihood Estimation ----
# Pfadmodell mit den berechneten Mittelwerten der UTAUT2 Konstrukte

utaut2_path <- '
  # Strukturmodell:
  BI ~ PE + EE + SI + HM + FC
  UB ~ BI
'

# Schätze das Modell basierend auf den beobachteten Mittelwerten
fit_path <- sem(utaut2_path, data = combinedDF, estimator = "MLR")
summary(fit_path, fit.measures = TRUE, standardized = TRUE)


# * Partial Least Squares SEM ----


# Beispiel für ein PLS-SEM-Modell mit seminr:
# 1. Definiere die Messmodelle (outer model) für die latenten Variablen
outer_model <- constructs(
  composite("PE", multi_items("PE", 1:4)),
  composite("EE", multi_items("EE", 1:4)),
  composite("SI", multi_items("SI", 1:3)),
  composite("HM", multi_items("HM", 1:3)),
  composite("FC", multi_items("FC", 1:4)),
  composite("BI", multi_items("BI", 1:3)),
  composite("UB", multi_items("UB", 1:3))
)

# 2. Definiere das Strukturelle Modell (inner model)
inner_model <- relationships(
  paths(from = c("PE", "EE", "SI", "HM", "FC"), to = "BI"),
  paths(from = "BI", to = "UB")
)

# 3. Schätze das Modell
sem_model <- estimate_pls(
  data = experimentalGruppe,
  measurement_model = outer_model,
  structural_model = inner_model,
  #inner_weights = path_weighting_scheme("centroid")
)

# 4. Ergebnisse zusammenfassen
summary(sem_model)










#9. Regressionsanalyse UTAUT2 und Application Intention  -----------
#10. Visualisierung  -------
# * normalverteilung ueqs ----
ggplot(combinedDF, aes(sample=mean_response_ueqs)) +
  stat_qq() + stat_qq_line()



cat("Code Beendet.")














