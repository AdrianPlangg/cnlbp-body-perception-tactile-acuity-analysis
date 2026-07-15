# =====================================================
# Public analysis script for MSc thesis
# =====================================================
# This script contains the analysis code used for the MSc thesis
# on chronic non-specific low back pain, body perception disturbance,
# tactile acuity, and pain intensity.
#
# Data are not included in this repository due to ethical,
# contractual, and data governance restrictions.
#
# File paths below are placeholders and must be adapted by users
# with authorised access to the corresponding datasets.
# =====================================================

# Clear workspace for reproducible analysis
rm(list = ls())

# -----------------------------------------------------
# Load required packages
# -----------------------------------------------------
library(tidyverse)
library(readr)
library(readxl)
library(janitor)
library(Hmisc)
library(car)
library(broom)
library(gtsummary)
library(flextable)
library(officer)
library(gridExtra)

# -----------------------------------------------------
# Create output folders
# -----------------------------------------------------
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

# -----------------------------------------------------
# Placeholder paths for authorised datasets
# -----------------------------------------------------
# Data are not included in this public repository.
# Replace these paths with local files if you have authorised data access.
path_ch_data <- "path/to/swiss_cohort_dataset.csv"
path_au_main <- "path/to/australian_main_dataset.csv"
path_au_tpd <- "path/to/australian_tpd_dataset.xlsx"
path_au_logbook <- "path/to/australian_logbook.xlsx"

############################################################
## 1. Load data ----
############################################################

# Load authorised Swiss cohort dataset
ch_raw <- readr::read_csv(path_ch_data, show_col_types = FALSE) %>%
  janitor::clean_names()

# Inspect structure
dplyr::glimpse(ch_raw)
names(ch_raw)

#######################################################
## 2. Schweizer Daten aufbereiten ----
#######################################################
# TPD-Mittelwert berechnen
ch_data <- ch_raw %>%
  mutate(
    tpd_mean = rowMeans(
      select(., tpd_up_1, tpd_down_1, tpd_up_2, tpd_down_2),
      na.rm = TRUE
    )
  )

# Kontrolle
ch_data %>%
  select(record_id, tpd_up_1, tpd_down_1,
         tpd_up_2, tpd_down_2, tpd_mean)

# ---------------------------------
# AU main data
# ---------------------------------

au_main <- read_csv(path_au_main, show_col_types = FALSE)

glimpse(au_main)
names(au_main)

# ---------------------------------
# Variablen checken
# ---------------------------------

grep("tpd", names(au_main), ignore.case = TRUE, value = TRUE)
grep("frebaq", names(au_main), ignore.case = TRUE, value = TRUE)
grep("nrs", names(au_main), ignore.case = TRUE, value = TRUE)
grep("age", names(au_main), ignore.case = TRUE, value = TRUE)
grep("sex", names(au_main), ignore.case = TRUE, value = TRUE)
grep("pain", names(au_main), ignore.case = TRUE, value = TRUE)
# ---------------------------------
# TPD RAW laden
# ---------------------------------

tpd_raw <- read_excel(path_au_tpd, col_names = FALSE)

print(tpd_raw, n = 20)
dim(tpd_raw)
# --------------------------------------------------
# TPD Rohdaten einlesen (Excel)
# Header manuell setzen (Excel ist nicht tidy aufgebaut)
# --------------------------------------------------
tpd <- read_excel(path_au_tpd, skip = 3, col_names = FALSE)

# Erste Zeile als Spaltennamen setzen
colnames(tpd) <- as.character(tpd[1, ])

# Erste Zeile entfernen (war Header)
tpd <- tpd[-1, ]
# --------------------------------------------------
# Spalten umbenennen für bessere Lesbarkeit
# --------------------------------------------------
names(tpd) <- c(
  "participant",
  "timepoint",
  "date",
  "area_pain",
  "asc1",
  "desc1",
  "asc2",
  "desc2",
  "tpd_mean"
)
# --------------------------------------------------
# TPD Daten bereinigen
# - fehlende Werte ersetzen
# - numerische Variablen konvertieren
# --------------------------------------------------
tpd_clean <- tpd %>%
  fill(participant, area_pain, .direction = "down") %>%
  mutate(across(everything(), ~na_if(.x, "Rx Drop Out"))) %>%
  mutate(across(everything(), ~na_if(.x, "#DIV/0!"))) %>%
  mutate(
    asc1     = as.numeric(asc1),
    desc1    = as.numeric(desc1),
    asc2     = as.numeric(asc2),
    desc2    = as.numeric(desc2),
    tpd_mean = as.numeric(tpd_mean)
  )
# --------------------------------------------------
# Nur Baseline-Messungen auswählen
# --------------------------------------------------
tpd_baseline <- tpd_clean %>%
  filter(timepoint == "Baseline")

glimpse(tpd_clean)
head(tpd_baseline)
summary(tpd_baseline$tpd_mean)
table(tpd_baseline$area_pain, useNA = "ifany")
head(au_main$ID)
class(au_main$ID)
head(tpd_baseline$participant)
class(tpd_baseline$participant)
glimpse(tpd_clean)
head(tpd_baseline)
head(au_main$ID)

#ID angleichen#
# --------------------------------------------------
# Teilnehmer-ID an AU-Datensatz anpassen
# (ID001 -> 1, ID002 -> 2, ...)
# --------------------------------------------------
tpd_baseline <- tpd_baseline %>%
  mutate(ID = as.numeric(gsub("^ID", "", participant)))
# --------------------------------------------------
# TPD-Daten mit AU-Hauptdatensatz zusammenführen
# --------------------------------------------------
au_with_tpd <- au_main %>%
  left_join(
    tpd_baseline %>%
      select(ID, area_pain, asc1, desc1, asc2, desc2, tpd_mean),
    by = "ID"
  )
# --------------------------------------------------
# Überprüfen, ob TPD korrekt gemerged wurde
# --------------------------------------------------

# Anzahl Personen mit TPD-Daten
sum(!is.na(au_with_tpd$tpd_mean))

# Überblick
glimpse(au_with_tpd)
# Überblick
glimpse(au_with_tpd)

############################################################
# Schmerzdauer berechnen (Pain duration)
############################################################

# Überblick über Schmerzdauer
summary(au_with_tpd$pain.dur)
table(au_with_tpd$pain.dur.units)

# Umrechnung in Monate
au_with_tpd <- au_with_tpd %>%
  mutate(
    pain_duration_months = case_when(
      pain.dur.units == 1 ~ pain.dur * 12,  # Jahre → Monate
      pain.dur.units == 0 ~ pain.dur,       # Monate bleiben
      TRUE ~ NA_real_
    )
  )

# Check
summary(au_with_tpd$pain_duration_months)
# Schmerzdauer plausibilisieren
hist(au_with_tpd$pain_duration_months)
############################################################
# Relevante Variablen im AU-Datensatz suchen
############################################################
# --------------------------------------------------
# Relevante Variablen im AU-Datensatz suchen
# --------------------------------------------------

grep("pain", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("back", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("neuro", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("fremantle", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("body", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("awareness", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("Q19", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("Q20", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
names(au_with_tpd)[850:980]
names(au_with_tpd)[980:1120]

# --------------------------------------------------
# Logbook / codebook used to identify Australian variables
# --------------------------------------------------

# Optional logbook/codebook for identifying Australian variables
# Replace with an authorised local file if needed.
# path_au_logbook <- "path/to/australian_logbook.xlsx"
file.exists(path_au_logbook)
library(readxl)

excel_sheets(path_au_logbook)

logbook <- read_excel(path_au_logbook)
names(logbook)

glimpse(logbook)
names(logbook)
####
head(tpd_baseline$ID)
class(tpd_baseline$ID)
# ==================================================
# LOGBOOK EINLESEN (roh, ohne Header)
# Ziel: herausfinden, wo die echte Kopfzeile ist
# ==================================================

logbook_raw <- read_excel(path_au_logbook, col_names = FALSE)

print(logbook_raw, n = 15)
dim(logbook_raw)
# ==================================================
# LOGBOOK KORREKT EINLESEN (ohne skip!)
# ==================================================

logbook <- read_excel(path_au_logbook)

names(logbook)
glimpse(logbook)
# ==================================================
# META-ZEILE ENTFERNEN (Zeile 1 nach Header)
# ==================================================

logbook <- logbook[-1, ]
head(logbook)
# ==================================================
# FREBAQ SUCHEN
# ==================================================

logbook %>% 
  filter(grepl("frebaq|fremantle|body|awareness", construct, ignore.case = TRUE))
# ==================================================
# NRS / PAIN SUCHEN
# ==================================================

logbook %>% 
  filter(grepl("pain|nrs|intensity", construct, ignore.case = TRUE))
names(logbook)
logbook %>% 
  filter(grepl("frebaq|pain", `variable id`, ignore.case = TRUE))
logbook %>% 
  filter(grepl("frebaq|fremantle|pain", construct, ignore.case = TRUE))
# ==================================================
# FREBAQ / Fremantle gezielt suchen
# ==================================================

logbook %>%
  filter(
    grepl("fremantle|body perception|body awareness|back-specific body", 
          construct, ignore.case = TRUE) |
      grepl("fremantle|body perception|body awareness|back-specific body", 
            measure, ignore.case = TRUE)
  )# ==================================================
# Q19-Block anschauen
# Ziel: prüfen, ob Q19 = FreBAQ ist
# ==================================================

logbook %>%
  filter(grepl("^5\\.Q19|^6\\.Q19|Q19", `variable id`)) %>%
  select(`variable id`, construct, measure, `higher score means`, `summary score required`)
# ==================================================
# Q19 Summary-Score finden
# ==================================================

logbook %>%
  filter(grepl("^5\\.Q19|^6\\.Q19|Q19", `variable id`)) %>%
  filter(
    grepl("summary", measure, ignore.case = TRUE) |
      grepl("yes|y", `summary score required`, ignore.case = TRUE)
  ) %>%
  select(`variable id`, construct, measure, `summary score required`)
# ==================================================
# NRS / Schmerzintensität gezielter suchen
# ==================================================

logbook %>%
  filter(
    grepl("intensity|numerical rating|nrs|average pain|mean pain", construct, ignore.case = TRUE) |
      grepl("intensity|numerical rating|nrs|average pain|mean pain", measure, ignore.case = TRUE)
  ) %>%
  select(`variable id`, construct, measure, `higher score means`)
# ==================================================
# Q20-Block anschauen
# Ziel: prüfen, ob dort NRS / Pain Intensity liegt
# ==================================================

logbook %>%
  filter(grepl("^5\\.Q20|^6\\.Q20|Q20", `variable id`)) %>%
  select(`variable id`, construct, measure, `higher score means`, `summary score required`)
# ==================================================
# FREBAQ im Datensatz finden
# ==================================================

grep("frbq", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
summary(au_with_tpd$frbq)
summary(au_with_tpd$DEIN_VARIABLENNAME)
range(au_with_tpd$frbq, na.rm = TRUE)
# ==================================================
# FREBAQ ITEMS CHECKEN
# ==================================================

au_with_tpd %>%
  select(starts_with("frbq")) %>%
  summary()
# ==================================================
# FREBAQ SCORE BERECHNEN (Summe aller Items)
# ==================================================

au_with_tpd <- au_with_tpd %>%
  mutate(
    frebaq = rowSums(select(., starts_with("frbq")), na.rm = TRUE)
  )
# ==================================================
# CHECK FREBAQ SCORE
# ==================================================

summary(au_with_tpd$frebaq)
range(au_with_tpd$frebaq, na.rm = TRUE)
sapply(au_with_tpd %>% select(starts_with("frbq")), class)
au_with_tpd <- au_with_tpd %>%
  mutate(across(starts_with("frbq"), as.numeric))
# ==================================================
# UNIQUE VALUES CHECKEN
# ==================================================

lapply(au_with_tpd %>% select(starts_with("frbq")), unique)
# ==================================================
# FREBAQ / FRBQ im AU-Datensatz
# Wichtiger Hinweis:
# frbq.t1 bis frbq.t7 sind keine Einzelitems,
# sondern bereits berechnete Gesamtwerte über mehrere Zeitpunkte.
# Für die Baseline-Analyse wird frbq.t1 verwendet.
# ==================================================
# Baseline-FRBAQ prüfen
# ==================================================

summary(au_with_tpd$frbq.t1)
range(au_with_tpd$frbq.t1, na.rm = TRUE)
# ==================================================
# NRS VARIABLE SUCHEN (REAL DATA)
# ==================================================

grep("q2", names(au_with_tpd), value = TRUE)
grep("pain", names(au_with_tpd), ignore.case = TRUE, value = TRUE)
grep("q2.1", names(au_with_tpd), value = TRUE)
grep("dur", names(au_with_tpd), ignore.case = TRUE, value = TRUE)

# ==================================================
# ANALYSE-DATENSATZ (AU)
# ==================================================

analysis_data_au <- au_with_tpd %>%
  transmute(
    ID,
    dataset = "AU",
    age,
    sex,
    nrs = pain.t1,
    frebaq = frbq.t1,
    tpd = tpd_mean,
    pain_duration_months
  )
summary(analysis_data_au)
names(au_with_tpd)
summary(au_with_tpd$pain_duration_months)
table(au_with_tpd$pain_duration_months < 3, useNA = "ifany")
#summary(model_duration)

#model_duration <- lm(
#  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
#  data = analysis_data_au
#)

#summary(model_duration)
#######

#analysis_data_filtered <- analysis_data_au %>%
#  filter(pain_duration_months >= 3)

#model_filtered <- lm(
#  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
 # data = analysis_data_filtered
#)

#summary(model_filtered)

summary(au_with_tpd$pain.dur)
table(au_with_tpd$pain.dur.units, useNA = "ifany")
exists("au_with_tpd")
exists("ch_data")
# =====================================================
# ANALYSE-DATENSATZ (CH): Harmonisierung für gepoolte Analyse
# =====================================================
analysis_data_ch <- ch_data %>%
  transmute(
    ID = record_id,
    dataset = "CH",
    age,
    sex = case_when(
      sex == 1 ~ 0,   # female
      sex == 2 ~ 1,   # male
      TRUE ~ NA_real_
    ),
    nrs = nrs_7d_mean,
    # FreBAQ: CH version scored 0–36; rescaled to AU metric 0–27
    frebaq = frebaq_total * 0.75, # harmonised from 0–36 to 0–27 scale
    tpd = tpd_mean,
    pain_duration_months
  )

# Kontrolle der harmonisierten CH-Geschlechterkodierung:
# 0 = female, 1 = male
stopifnot(all(analysis_data_ch$sex %in% c(0, 1)))
stopifnot(sum(analysis_data_ch$sex == 0, na.rm = TRUE) == 7)
stopifnot(sum(analysis_data_ch$sex == 1, na.rm = TRUE) == 8)

# =====================================================
# POOLED ANALYSE-DATENSATZ (AU + CH)
# =====================================================

pooled_data <- bind_rows(
  analysis_data_au,
  analysis_data_ch
)

# Kontrolle
glimpse(pooled_data)
table(pooled_data$dataset)
summary(pooled_data)
# =========================================================
# TABLE 1 – PARTICIPANT CHARACTERISTICS
# =========================================================

# Stichprobengrössen
pooled_data %>%
  count(dataset)

# Geschlechterverteilung
pooled_data %>%
  count(sex, dataset)

# Fehlende Werte
pooled_data %>%
  summarise(
    missing_age = sum(is.na(age)),
    missing_sex = sum(is.na(sex)),
    missing_nrs = sum(is.na(nrs)),
    missing_frebaq = sum(is.na(frebaq)),
    missing_tpd = sum(is.na(tpd)),
    missing_pain_duration = sum(is.na(pain_duration_months))
  )
# ==================================================
# DATEN-CHECK
# ==================================================

summary(analysis_data_au)

# vollständige Fälle
sum(complete.cases(analysis_data_au))

# =========================================================
# LINEARE REGRESSION (gepoolte Daten)
# NRS ~ FreBAQ + TPD + Alter + Geschlecht + Schmerzdauer
# =========================================================

model_main <- lm(
  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
  data = pooled_data
)

summary(model_main)

# Sensitivitätsanalyse mit Dataset als Kovariate

model_dataset <- lm(
  nrs ~ frebaq + tpd + age + sex +
    pain_duration_months + dataset,
  data = pooled_data
)

summary(model_dataset)
# =========================================================
# LINEARE REGRESSION (EXPLORATIVE ANALYSEN / SUBGROUP ANALYSES)
# NRS ~ FreBAQ + TPD + Alter + Geschlecht
# ==================================================
### AU ###
model_au <- lm(
  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
  data = analysis_data_au
)

summary(model_au)
### CH ###
model_ch <- lm(
  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
  data = analysis_data_ch
)

summary(model_ch)
############################################################
## 2. Deskriptive Statistiken ----
############################################################

# Gesamtdeskriptive Werte für zentrale Variablen
analysis_data_au %>%
  summarise(
    n_total     = n(),
    mean_nrs    = mean(nrs, na.rm = TRUE),
    sd_nrs      = sd(nrs, na.rm = TRUE),
    mean_frebaq = mean(frebaq, na.rm = TRUE),
    sd_frebaq   = sd(frebaq, na.rm = TRUE),
    mean_tpd    = mean(tpd, na.rm = TRUE),
    sd_tpd      = sd(tpd, na.rm = TRUE),
    mean_age    = mean(age, na.rm = TRUE),
    sd_age      = sd(age, na.rm = TRUE)
  )

# Geschlechterverteilung
table(analysis_data_au$sex)

############################################################
## 3. Korrelationen ----
############################################################
# Complete-case sample for inferential analyses
analysis_complete <- pooled_data %>%
  filter(
    complete.cases(
      nrs,
      frebaq,
      tpd,
      age,
      sex,
      pain_duration_months
    )
  )

nrow(analysis_complete)
analysis_complete %>%
  select(nrs, frebaq, tpd, age) %>%
  cor(use = "complete.obs")

# Mit p-Werten
corr_data <- analysis_complete %>%
  select(nrs, frebaq, tpd, age) %>%
  as.matrix()

rcorr(corr_data)



############################################################
## 4. Grafiken für die Präsentation ----
############################################################

## 4.1 Histogramme aller numerischen Variablen
p_hist <- analysis_data_au %>%
  pivot_longer(
    cols = c(nrs, frebaq, tpd, age),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 20) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(title = "Verteilung der zentralen Variablen")

# Plot anzeigen
print(p_hist)

# Als PNG speichern
ggsave(
  "outputs/figures/hist_all_vars.png",
  p_hist,
  width = 8,
  height = 5,
  dpi = 300
)

## 4.2 Scatterplot NRS vs FreBAQ
p_scatter_frebaq <- ggplot(analysis_data_au, aes(x = frebaq, y = nrs)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Zusammenhang zwischen FreBAQ und NRS",
    x = "FreBAQ Gesamtscore",
    y = "Schmerzintensität (NRS)"
  )

print(p_scatter_frebaq)

ggsave(
  "outputs/figures/scatter_nrs_frebaq.png",
  p_scatter_frebaq,
  width = 6,
  height = 4,
  dpi = 300
)

## 4.3 Scatterplot NRS vs TPD
p_scatter_tpd <- ggplot(analysis_data_au, aes(x = tpd, y = nrs)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  theme_minimal() +
  labs(
    title = "Zusammenhang zwischen TPD und NRS",
    x = "TPD",
    y = "Schmerzintensität (NRS)"
  )

print(p_scatter_tpd)

ggsave(
  "outputs/figures/scatter_nrs_tpd.png",
  p_scatter_tpd,
  width = 6,
  height = 4,
  dpi = 300
)

############################################################
## 5. Regressionsmodelle ----
############################################################

## 5.1 Primäres Modell
# NRS ~ FreBAQ + TPD + Alter + Geschlecht

model_main <- lm(
  nrs ~ frebaq + tpd + age + sex,
  data = analysis_data_au
)

summary(model_main)

# Multikollinearität prüfen
vif(model_main)

############################################################
## 5.2 Modell mit Schmerzdauer (optional, sehr sinnvoll!)
############################################################

model_duration <- lm(
  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
  data = analysis_data_au
)

summary(model_duration)
vif(model_duration)
# Vergleich der Modelle
summary(model_main)
summary(model_duration)
############################################################
## 5.3 Z-standardisierte Prädiktoren (vergleichbare Effekte)
############################################################

analysis_data_au_z <- analysis_data_au %>%
  mutate(
    frebaq_z = scale(frebaq)[,1],
    tpd_z    = scale(tpd)[,1],
    age_z    = scale(age)[,1]
  )

model_z <- lm(
  nrs ~ frebaq_z + tpd_z + age_z + sex,
  data = analysis_data_au_z
)

summary(model_z)

############################################################
## 6. Modell-Diagnostik ----
############################################################

# Diagnoseplots
png("outputs/figures/model_diagnostics.png", width = 1000, height = 800)
par(mfrow = c(2,2))
plot(model_main)
dev.off()

# zurücksetzen
par(mfrow = c(1,1))

############################################################
## 6. Modell-Diagnostik ----
############################################################

# Diagnoseplots für Site-Modell (das wirst du wahrscheinlich zeigen)
png("outputs/figures/model_diagnostics_m_site.png", width = 1000, height = 800, res = 150)
par(mfrow = c(2, 2))
plot(model_duration)
par(mfrow = c(1, 1))
dev.off()

# Einzelne Plots ggf. auch direkt im RStudio anschauen:
# plot(m_site)

############################################################
## 7. Regressionsergebnisse als Textdatei speichern ----
############################################################

# Ergebnisse als Textdatei speichern
sink("outputs/tables/regression_results.txt")

cat("===== Primäres Modell =====\n")
print(summary(model_main))

cat("\n===== Modell mit Schmerzdauer =====\n")
print(summary(model_duration))

cat("\n===== Z-standardisiertes Modell =====\n")
print(summary(model_z))

sink()

cat("Analyse fertig. Plots und Tabellen liegen im Ordner 'outputs/'.\n")
# =====================================================
# 8. RESULTS TABLES
# =====================================================
# Pakete installieren, falls nötig
# Required packages are loaded at the beginning of this script.

library(gtsummary)
library(flextable)
library(officer)
library(dplyr)

# =========================================================
# FINAL TABLE 1 – PARTICIPANT CHARACTERISTICS
# =========================================================

table1 <- pooled_data %>%
  mutate(
    dataset = factor(dataset, levels = c("AU", "CH"),
                     labels = c("Australian cohort", "Swiss cohort")),
    sex = factor(sex, levels = c(0, 1),
                 labels = c("Female", "Male"))
  ) %>%
  select(dataset, age, sex, nrs, frebaq, tpd, pain_duration_months) %>%
  tbl_summary(
    by = dataset,
    statistic = list(
      age ~ "{mean} ({sd})",
      nrs ~ "{mean} ({sd})",
      tpd ~ "{mean} ({sd})",
      frebaq ~ "{median} ({p25}, {p75})",
      pain_duration_months ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1,
    missing = "ifany",
    missing_text = "Missing",
    label = list(
      age ~ "Age, years",
      sex ~ "Sex",
      nrs ~ "Pain intensity, NRS 0–10",
      frebaq ~ "FreBAQ total score",
      tpd ~ "Two-point discrimination threshold (mm)",
      pain_duration_months ~ "Pain duration, months"
    )
  ) %>%
  add_overall(last = FALSE) %>%
  add_p(
    test = list(
      all_continuous() ~ "wilcox.test",
      all_categorical() ~ "fisher.test"
    )
  ) %>%
  modify_header(
    label ~ "**Variable**",
    stat_0 ~ "**Overall (N = 288)**",
    stat_1 ~ "**Australian cohort (n = 273)**",
    stat_2 ~ "**Swiss cohort (n = 15)**",
    p.value ~ "**p-value**"
  ) %>%
  remove_footnote_header(everything()) %>%
  bold_labels()
  class(table1)
  table1_flex <- as_flex_table(table1)

# Alte automatische Caption und automatische Footnotes entfernen
table1_flex <- table1_flex %>%
  set_caption(caption = NULL) %>%
  delete_part(part = "footer")

doc <- read_docx() %>%
  body_add_par(
    "Table 1. Participant characteristics and clinical measures",
    style = "Normal"
  ) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(table1_flex) %>%
  body_add_par(
    "Values are presented as mean (SD) for age, pain intensity and TPD, median (interquartile range) for FreBAQ and pain duration, and n (%) for categorical variables. NRS = Numerical Rating Scale; FreBAQ = Fremantle Back Awareness Questionnaire; TPD = two-point discrimination threshold.",
    style = "Normal"
  )

print(
  doc,
  target = "outputs/tables/table1_participant_characteristics.docx"
)
# =========================================================
# TABLE 2 – CORRELATION MATRIX
# =========================================================
# Complete-case sample for inferential analyses
corr_data <- analysis_complete %>%
  select(
    nrs,
    frebaq,
    tpd,
    age,
    pain_duration_months
  )

nrow(analysis_complete)

# Korrelationsmatrix
cor_matrix <- cor(
  corr_data,
  use = "complete.obs",
  method = "pearson"
)
library(Hmisc)

corr_results <- rcorr(
  as.matrix(corr_data),
  type = "pearson"
)

cor_matrix <- corr_results$r
p_matrix <- corr_results$P
################################################################################
# ERGÄNZUNG zu Tabelle 2: Exakte p-Werte zusätzlich zu den Sternchen
#
# Dieser Code baut auf dem bereits vorhandenen Code auf (cor_matrix, p_matrix
# aus rcorr() müssen bereits existieren - siehe Zeilen ~868-874 deines Skripts).
#
# Zwei Varianten:
#   A) Eine ZWEITE Tabelle nur mit p-Werten (z.B. für Anhang)
#   B) EINE Tabelle, bei der r UND p in jeder Zelle stehen (z.B. "0.25 (p<.001)")
################################################################################


# ------------------------------------------------------------------------
# VARIANTE A: Separate p-Werte-Tabelle (zusätzlich zur r-Tabelle)
# ------------------------------------------------------------------------

# p-Werte runden/formatieren (sehr kleine p-Werte als "<.001" statt "0.000")
format_pval <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "<.001",
                sprintf("%.3f", p)))
}

p_table_chr <- matrix(
  format_pval(p_matrix),
  nrow = nrow(p_matrix),
  ncol = ncol(p_matrix)
)
diag(p_table_chr) <- "—"

p_table_df <- as.data.frame(p_table_chr)
rownames(p_table_df) <- c(
  "Pain intensity (NRS)",
  "FreBAQ total score",
  "TPD (mm)",
  "Age",
  "Pain duration"
)

colnames(p_table_df) <- c(
  "Pain intensity (NRS)",
  "FreBAQ total score",
  "TPD (mm)",
  "Age",
  "Pain duration (months)"
)

# Flextable für die p-Werte-Tabelle
p_flex <- flextable::flextable(
  tibble::rownames_to_column(p_table_df, var = "Variable")
) %>%
  width(j = 1, width = 2.3) %>%
  width(j = 2:6, width = 1.2) %>%
  add_footer_lines(
    paste0(
      "Two-tailed p-values for the Pearson correlation coefficients reported in Table 2. ",
      "Analyses were based on complete cases (n = ", nrow(corr_data), ")."
    )
  ) %>%
  fontsize(size = 9, part = "footer") %>%
  italic(part = "footer")

p_flex  # in Viewer ansehen


# ------------------------------------------------------------------------
# VARIANTE B: Kombinierte Tabelle - r-Wert UND exakter p-Wert in einer Zelle
# (ersetzt die bisherige Sternchen-Logik, falls gewünscht)
# ------------------------------------------------------------------------

cor_table_chr_combined <- matrix(
  ifelse(
    is.na(p_matrix), "",
    paste0(
      sprintf("%.2f", cor_matrix),
      " (",
      format_pval(p_matrix),
      ")"
    )
  ),
  nrow = nrow(cor_matrix),
  ncol = ncol(cor_matrix)
)
diag(cor_table_chr_combined) <- "—"

cor_table_df_combined <- as.data.frame(cor_table_chr_combined)
rownames(cor_table_df_combined) <- c(
  "Pain intensity (NRS)",
  "FreBAQ total score",
  "TPD (mm)",
  "Age",
  "Pain duration"
)

colnames(cor_table_df_combined) <- c(
  "Pain intensity (NRS)",
  "FreBAQ total score",
  "TPD (mm)",
  "Age",
  "Pain duration (months)"
)

cor_flex_combined <- flextable::flextable(
  tibble::rownames_to_column(cor_table_df_combined, var = "Variable")
) %>%
  width(j = 1, width = 2.3) %>%
  width(j = 2:6, width = 1.0)

cor_flex_combined  # in Viewer ansehen
# Beispiel-Zelle sieht dann so aus: "0.25 (<.001)" statt "0.25***"




# Ausgabe
round(cor_matrix, 2)
# Korrelationsmatrix runden
cor_table <- round(cor_matrix, 2)
# Sternchen-Funktion für Signifikanz
sig_stars <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.001, "***",
                ifelse(p < 0.01,  "**",
                       ifelse(p < 0.05,  "*", ""))))
}
cor_table_chr <- matrix(
  paste0(sprintf("%.2f", cor_matrix), sig_stars(p_matrix)),
  nrow = nrow(cor_matrix),
  ncol = ncol(cor_matrix)
)

diag(cor_table_chr) <- "—"

# In Data Frame umwandeln
cor_table_df <- as.data.frame(cor_table_chr)

# Variable Namen schöner machen
rownames(cor_table_df) <- c(
  "Pain intensity (NRS)",
  "FreBAQ total score",
  "TPD (mm)",
  "Age",
  "Pain duration"
)

colnames(cor_table_df) <- c(
  "Pain intensity (NRS)",
  "FreBAQ total score",
  "TPD (mm)",
  "Age",
  "Pain duration (months)"
)

# Flextable erstellen
cor_flex <- flextable::flextable(
  tibble::rownames_to_column(cor_table_df, var = "Variable")
)

# Spaltenbreiten optimieren
cor_flex <- cor_flex %>%
  width(j = 1, width = 2.3) %>%
  width(j = 2:6, width = 1.2)
# Word-Dokument exportieren
doc_cor <- read_docx() %>%
  body_add_par(
    "Table 2. Pearson correlation matrix of study variables",
    style = "Normal"
  ) %>%
  body_add_par("", style = "Normal") %>%
  body_add_flextable(cor_flex) %>%
  body_add_par(
    paste0(
      "Values represent Pearson correlation coefficients (r). ",
      "Analyses were based on complete cases (n = ",
      nrow(corr_data),
      "). *p < 0.05, **p < 0.01, ***p < 0.001."
    ),
    style = "Normal"
  )

print(
  doc_cor,
  target = "outputs/tables/table2_correlation_matrix.docx"
)
# =========================================================
# TABLE 3 – MULTIPLE LINEAR REGRESSION
# =========================================================

library(dplyr)
library(broom)
library(car)
library(flextable)
library(officer)

# Output-Ordner sicherstellen
if (!dir.exists("outputs/tables")) {
  dir.create("outputs/tables", recursive = TRUE)
}

# ---------------------------------------------------------
# 1. Finalen Complete-Case-Datensatz verwenden
#    Sex coding: 0 = female, 1 = male
# ---------------------------------------------------------

model_data <- analysis_complete

# Sicherheitskontrollen
stopifnot(nrow(model_data) == 280)
stopifnot(all(model_data$sex %in% c(0, 1)))

stopifnot(
  all(
    complete.cases(
      model_data[
        c(
          "nrs",
          "frebaq",
          "tpd",
          "age",
          "sex",
          "pain_duration_months"
        )
      ]
    )
  )
)

# ---------------------------------------------------------
# 2. Hauptmodell
# ---------------------------------------------------------

model_main <- lm(
  nrs ~ frebaq + tpd + age + sex + pain_duration_months,
  data = model_data
)

# Sicherstellen, dass wirklich 280 Fälle verwendet werden
stopifnot(nobs(model_main) == 280)

# ---------------------------------------------------------
# 3. Modellgüte und Multikollinearität
# ---------------------------------------------------------

model_main_glance <- broom::glance(model_main)
model_main_vif <- car::vif(model_main)

print(summary(model_main))
print(model_main_glance)
print(model_main_vif)
# -----------------------------------------------------
# Supplementary Table S1: Variance inflation factors
# -----------------------------------------------------

vif_table <- tibble::tibble(
  Predictor = names(model_main_vif),
  VIF = as.numeric(model_main_vif)
) %>%
  mutate(
    Predictor = dplyr::recode(
      Predictor,
      "frebaq" = "FreBAQ total score",
      "tpd" = "TPD threshold (mm)",
      "age" = "Age, years",
      "sex" = "Sex",
      "pain_duration_months" = "Pain duration, months"
    ),
    VIF = round(VIF, 3)
  )

vif_flex <- flextable::flextable(vif_table) %>%
  flextable::bold(part = "header") %>%
  flextable::align(align = "left", part = "all") %>%
  flextable::align(j = "VIF", align = "center", part = "all") %>%
  flextable::autofit()

doc_vif <- officer::read_docx() %>%
  officer::body_add_par(
    "Supplementary Table S1. Variance inflation factors for the primary regression model",
    style = "Normal"
  ) %>%
  officer::body_add_par("", style = "Normal") %>%
  flextable::body_add_flextable(vif_flex) %>%
  officer::body_add_par(
    "VIF = variance inflation factor. All values were close to 1, indicating no evidence of problematic multicollinearity among predictors in the primary regression model.",
    style = "Normal"
  )

print(
  doc_vif,
  target = "outputs/tables/supplementary_table_s1_vif.docx"
)
# -----------------------------------------------------
# Supplementary Figure S1: Diagnostic plots
# -----------------------------------------------------

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

png(
  filename = "outputs/figures/supplementary_figure_s1_diagnostic_plots.png",
  width = 2000,
  height = 2000,
  res = 300
)

par(mfrow = c(2, 2))
plot(model_main)

dev.off()

doc_diag <- officer::read_docx() %>%
  officer::body_add_par(
    "Supplementary Figure S1. Diagnostic plots for the primary regression model",
    style = "Normal"
  ) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_img(
    src = "outputs/figures/supplementary_figure_s1_diagnostic_plots.png",
    width = 6,
    height = 6
  ) %>%
  officer::body_add_par(
    "Supplementary Figure S1. Diagnostic plots for the primary multiple linear regression model. The plots were used to visually assess linearity, normality of residuals, homoscedasticity, and influential observations.",
    style = "Normal"
  )

print(
  doc_diag,
  target = "outputs/figures/supplementary_figure_s1_diagnostic_plots.docx"
)
# -----------------------------------------------------
# Supplementary Figure S2: Unadjusted associations
# -----------------------------------------------------

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

fig_s2a <- ggplot(model_data, aes(x = frebaq, y = nrs)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "FreBAQ total score",
    y = "Pain intensity (NRS 0–10)",
    title = "A. FreBAQ and pain intensity"
  ) +
  theme_minimal()

fig_s2b <- ggplot(model_data, aes(x = tpd, y = nrs)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Two-point discrimination threshold (mm)",
    y = "Pain intensity (NRS 0–10)",
    title = "B. TPD and pain intensity"
  ) +
  theme_minimal()

png(
  filename = "outputs/figures/supplementary_figure_s2_unadjusted_associations.png",
  width = 2400,
  height = 1200,
  res = 300
)

gridExtra::grid.arrange(fig_s2a, fig_s2b, ncol = 2)

dev.off()

doc_s2 <- officer::read_docx() %>%
  officer::body_add_par(
    "Supplementary Figure S2. Unadjusted associations of FreBAQ and TPD with pain intensity",
    style = "Normal"
  ) %>%
  officer::body_add_par("", style = "Normal") %>%
  officer::body_add_img(
    src = "outputs/figures/supplementary_figure_s2_unadjusted_associations.png",
    width = 6.5,
    height = 3.5
  ) %>%
  officer::body_add_par(
    "Supplementary Figure S2. Unadjusted associations of FreBAQ total score and two-point discrimination threshold with pain intensity. Panel A shows the unadjusted association between FreBAQ total score and pain intensity. Panel B shows the unadjusted association between two-point discrimination threshold and pain intensity. Lines represent unadjusted linear fits with 95% confidence intervals.",
    style = "Normal"
  )

print(
  doc_s2,
  target = "outputs/figures/supplementary_figure_s2_unadjusted_associations.docx"
)
# ---------------------------------------------------------
# 4. Regressionskoeffizienten aufbereiten
# ---------------------------------------------------------

reg_table_clean <- broom::tidy(
  model_main,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = dplyr::recode(
      term,
      "frebaq" = "FreBAQ total score",
      "tpd" = "TPD (mm)",
      "age" = "Age, years",
      "sex" = "Sex (male vs female)",
      "pain_duration_months" = "Pain duration (months)"
    ),
    B = sprintf("%.4f", estimate),
    `95% CI` = paste0(
      sprintf("%.4f", conf.low),
      " to ",
      sprintf("%.4f", conf.high)
    ),
    `p-value` = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  ) %>%
  select(
    Predictor,
    B,
    `95% CI`,
    `p-value`
  )

# ---------------------------------------------------------
# 5. Fussnote automatisch erzeugen
# ---------------------------------------------------------

model_p_text <- ifelse(
  model_main_glance$p.value < 0.001,
  "< 0.001",
  paste0("= ", sprintf("%.3f", model_main_glance$p.value))
)

table3_footnote <- paste0(
  "Values are unstandardized regression coefficients (B) from a multiple ",
  "linear regression model adjusted for all variables shown. ",
  "Analyses were based on ",
  nobs(model_main),
  " complete cases. ",
  "Overall model: F(",
  as.integer(model_main_glance$df),
  ", ",
  as.integer(model_main_glance$df.residual),
  ") = ",
  sprintf("%.2f", model_main_glance$statistic),
  ", p ",
  model_p_text,
  "; adjusted R² = ",
  sprintf("%.3f", model_main_glance$adj.r.squared),
  "."
)
# ---------------------------------------------------------
# 6. Flextable erstellen
# ---------------------------------------------------------

reg_flex <- flextable(reg_table_clean) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  padding(padding.left = 0, part = "all") %>%
  width(j = 1, width = 2.6) %>%
  width(j = 2, width = 1.0) %>%
  width(j = 3, width = 1.8) %>%
  width(j = 4, width = 1.0) %>%
  set_table_properties(
    layout = "fixed",
    align = "left"
  )

# ---------------------------------------------------------
# 7. Word-Datei exportieren
# ---------------------------------------------------------

doc_reg <- read_docx() %>%
  body_add_par(
    "Table 3. Multiple linear regression model for pain intensity (NRS)",
    style = "Normal"
  ) %>%
  body_add_flextable(reg_flex) %>%
  body_add_par(
    table3_footnote,
    style = "Normal"
  )

print(
  doc_reg,
  target = "outputs/tables/table3_regression.docx"
)

# ---------------------------------------------------------
# 8. Abschliessende Kontrollen
# ---------------------------------------------------------

nobs(model_main)
model_main$call
reg_table_clean
table3_footnote
model_main_vif
# ===================================================
# TABLE 4 – SENSITIVITY ANALYSIS (DATASET ADJUSTMENT)
# ===================================================

# ---------------------------------------------------------
# 1. Gleiche 280 Complete Cases wie im Hauptmodell verwenden
# ---------------------------------------------------------

model_data_dataset <- model_data %>%
  mutate(
    dataset = factor(dataset, levels = c("AU", "CH"))
  )

stopifnot(nrow(model_data_dataset) == 280)
stopifnot(!anyNA(model_data_dataset))
stopifnot(all(model_data_dataset$sex %in% c(0, 1)))
stopifnot(levels(model_data_dataset$dataset)[1] == "AU")

# ---------------------------------------------------------
# 2. Sensitivitätsmodell
# Referenzkategorie für dataset: AU
# ---------------------------------------------------------

model_dataset <- lm(
  nrs ~ frebaq + tpd + age + sex + pain_duration_months + dataset,
  data = model_data_dataset
)

stopifnot(nobs(model_dataset) == 280)

# ---------------------------------------------------------
# 3. Modellresultate
# ---------------------------------------------------------

model_dataset_tidy <- broom::tidy(
  model_dataset,
  conf.int = TRUE
)

model_dataset_glance <- broom::glance(model_dataset)

# ---------------------------------------------------------
# 4. Tabelle aufbereiten
# ---------------------------------------------------------

reg_table_dataset_clean <- model_dataset_tidy %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Predictor = dplyr::recode(
      term,
      "frebaq" = "FreBAQ total score",
      "tpd" = "TPD (mm)",
      "age" = "Age, years",
      "sex" = "Sex (male vs female)",
      "pain_duration_months" = "Pain duration (months)",
      "datasetCH" = "Dataset (CH vs AU)"
    ),
    B = sprintf("%.4f", estimate),
    `95% CI` = paste0(
      sprintf("%.4f", conf.low),
      " to ",
      sprintf("%.4f", conf.high)
    ),
    `p-value` = case_when(
      p.value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p.value)
    )
  ) %>%
  select(
    Predictor,
    B,
    `95% CI`,
    `p-value`
  )

# ---------------------------------------------------------
# 5. Fußnote
# ---------------------------------------------------------

model_dataset_p <- pf(
  model_dataset_glance$statistic,
  model_dataset_glance$df,
  model_dataset_glance$df.residual,
  lower.tail = FALSE
)

model_dataset_p_text <- ifelse(
  model_dataset_p < 0.001,
  "< 0.001",
  paste0("= ", sprintf("%.3f", model_dataset_p))
)

table4_footnote <- paste0(
  "Values are unstandardized regression coefficients (B) from a multiple ",
  "linear regression model adjusted for all variables shown and dataset origin. ",
  "AU was the reference category for dataset origin. ",
  "Analyses were based on ",
  nobs(model_dataset),
  " complete cases. ",
  "Overall model: F(",
  as.integer(model_dataset_glance$df),
  ", ",
  as.integer(model_dataset_glance$df.residual),
  ") = ",
  sprintf("%.2f", model_dataset_glance$statistic),
  ", p ",
  model_dataset_p_text,
  "; adjusted R² = ",
  sprintf("%.3f", model_dataset_glance$adj.r.squared),
  "."
)

# ---------------------------------------------------------
# 6. Flextable im gleichen Format wie Table 3
# ---------------------------------------------------------

reg_dataset_flex <- flextable(reg_table_dataset_clean) %>%
  theme_booktabs() %>%
  bold(part = "header") %>%
  align(j = 1, align = "left", part = "all") %>%
  align(j = 2:4, align = "center", part = "all") %>%
  padding(padding.left = 0, part = "all") %>%
  width(j = 1, width = 2.6) %>%
  width(j = 2, width = 1.0) %>%
  width(j = 3, width = 1.8) %>%
  width(j = 4, width = 1.0) %>%
  set_table_properties(
    layout = "fixed",
    align = "left"
  )

# ---------------------------------------------------------
# 7. Word-Datei exportieren
# ---------------------------------------------------------

doc_dataset <- read_docx() %>%
  body_add_par(
    "Table 4. Sensitivity analysis adjusted for dataset origin",
    style = "Normal"
  ) %>%
  body_add_flextable(reg_dataset_flex) %>%
  body_add_par(
    table4_footnote,
    style = "Normal"
  )

print(
  doc_dataset,
  target = "outputs/tables/table4_sensitivity_analysis.docx"
)

# ---------------------------------------------------------
# 8. Kontrollen
# ---------------------------------------------------------

nobs(model_dataset)
model_dataset$call
reg_table_dataset_clean
table4_footnote

# Optional: Modellvergleich

anova(model_main, model_dataset)
summary(model_dataset)
nrow(au_main)
table(pooled_data$dataset)
sum(pooled_data$dataset == "AU")
sum(pooled_data$dataset == "CH")
pooled_data %>%
  group_by(dataset) %>%
  summarise(
    n = n(),
    missing_regression = sum(!complete.cases(nrs, frebaq, tpd, age, sex, pain_duration_months))
  )

# End of public analysis script.
# Running this script requires authorised access to the original datasets.
# Data are not distributed with this repository.
