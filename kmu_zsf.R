# Ein paar Kennzahlen zu KMU in Deutschland

# Daten stammen vom Statistischen Bundesamt, GENESIS Datenbank
# Datensatz id: 48121-0002
if (!require(sqldf)) install.packages("sqldf")
if (!require(ggplot2)) install.packages("ggplot2")
library(sqldf)
library(ggplot2)


rm(list = ls())

# Vor dem Einlesen csv Datei per Excel im UTF-8 encoding gespeichert,
# sonst Probleme mit Umlauten im df.

# Daten einlesen, Pfad anpassen

data <- read.csv("~//48121-0002_$F_utf8.csv",
                 sep =";", header = FALSE)

View(data)

# Zeile 7+8 zu column names machen. Dafür vorher Zeile 7 und 8 zusammenfügen

col_pasted <- paste(data[8, ], data[9, ], sep ="_")
new_colnames <- c("jahr", "unt_kategorie", "id_nr", "branche", tail(col_pasted, -4))
new_colnames <- tolower(new_colnames)

colnames(data) <- new_colnames

# Präambel und Anhang entfernen

data <- data[10:919, ]

# Ab 2018 werden die Unternehmensanzahlen unter eu-Anzahl geführt, also 
# Spalten unternehmen_anzahl und unternehmen (eu)_anzahl zusammenführen

data[, "unternehmen_anzahl"] <- as.integer(data[, "unternehmen_anzahl"])
data[, "unternehmen (eu)_anzahl"] <- as.integer(data[, "unternehmen (eu)_anzahl"])
data[is.na(data)] <- 0
data["anzahl"] <- data["unternehmen_anzahl"] + data["unternehmen (eu)_anzahl"]

# Relevante Spalten herausfiltern

data_relevant <- data[ , c("jahr", "unt_kategorie", "branche", "anzahl")]

# Irrelevante Unternehmenkategorien rausfiltern

data_relevant <- subset(data_relevant, !(unt_kategorie %in% c("Insgesamt", 
                                                              "Großunternehmen")))

# unt_kategorien sind bereits zusammengerechnet pro Jahr.

data_relevant <- subset(data_relevant, branche != "Insgesamt")

# summary(data_relevant)

# Datentypen anpassen

data_relevant[, "jahr"] <- as.integer(data_relevant[, "jahr"])
data_relevant[, "unt_kategorie"] <- as.factor(data_relevant[, "unt_kategorie"])
data_relevant[, "branche"] <- as.factor(data_relevant[, "branche"])

# group by mit Jahr und KMU Summe

data_jahr_kat <- sqldf(
      "SELECT 
        jahr
        , unt_kategorie
        , sum(anzahl) AS summe
      FROM 
        data_relevant
      GROUP BY 
        jahr
        , unt_kategorie")

data_jahr_kat_aggr <- sqldf(
      "SELECT 
        jahr
        , sum(summe)
      FROM 
        data_jahr_kat
      GROUP BY 
        jahr")

data_jahr_kat_aggr
colnames(data_jahr_kat_aggr) <- c("jahr", "gesamtzahl_kmu")

# Veränderung mit jahreslag 1

data_jahr_kat_aggr["diff_1"] <- c(NA, diff(data_jahr_kat_aggr$gesamtzahl_kmu))
data_jahr_kat_aggr["diff_1_rel"] <- c(NA, diff(data_jahr_kat_aggr$gesamtzahl_kmu)/
  data_jahr_kat_aggr$gesamtzahl_kmu[-length(data_jahr_kat_aggr$gesamtzahl_kmu)])
data_jahr_kat_aggr["diff_1_rel"] <- round(data_jahr_kat_aggr["diff_1_rel"], 2)

# View(data_jahr_kat_aggr)

################################################################################
# Plots:
# Zahlen verträglicher für plot machen

data_jahr_kat_aggr["gesamtzahl_kmu"] = round(data_jahr_kat_aggr["gesamtzahl_kmu"] 
                                             / 1000000, 2)
data_jahr_kat_aggr["diff_1"] <- round(data_jahr_kat_aggr["diff_1"] / 1000, 0)
data_jahr_kat_aggr["diff_1_rel"] <- round(data_jahr_kat_aggr["diff_1_rel"], 2) * 100


# ggplot theme
ggplot2_theme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Times'),
        legend.title=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line.y=element_blank(),
        axis.text.x=element_text(size = 12),
        legend.key.size = unit(2, 'cm'),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        plot.title = element_text(hjust = 0.5))



# Plot für Anzahl KMU

ggplot(data = data_jahr_kat_aggr, aes(x = jahr, y = gesamtzahl_kmu, 
                                      label = gesamtzahl_kmu)) +
  geom_line(color = "blue", size = 1.2) +
  xlab("Jahr") +
  ylab("") +
  ylim(0, 3) +
  scale_x_continuous(breaks = min(data_jahr_kat_aggr["jahr"]):
                       max(data_jahr_kat_aggr["jahr"])) +
  ggtitle("Anzahl KMU Deutschland in Mio") +
  geom_text(hjust = 0, vjust = -1, 
            aes(label = ifelse(jahr %in% c(2008, 2011, 2014, 2018, 2020),
                             gesamtzahl_kmu, ''))) +
  ggplot2_theme

# Plot für Veränderung KMU

ggplot(data = tail(data_jahr_kat_aggr, -1), 
       aes(x = jahr, y = diff_1, label = diff_1)) +
  geom_line(color = "blue", size = 1.2) +
  xlab("Jahr") +
  ylab("") +
  ylim(-200, 400) +
  scale_x_continuous(breaks = min(data_jahr_kat_aggr["jahr"]):
                       max(data_jahr_kat_aggr["jahr"])) +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Wachstumsrate KMU Deutschland in Tausend") +
  geom_text(hjust = 0, vjust = -0.5,
            aes(label = ifelse(jahr %in% c(2009, 2011, 2014, 2016, 2018, 2020),
                             diff_1, ''))) +
  geom_text(hjust = 0, vjust = 1.5,
            aes(label = ifelse(jahr == 2015,
                             diff_1, ''))) +
  ggplot2_theme

# Plot für prozentuale Veränderung

ggplot(data = tail(data_jahr_kat_aggr, -1), 
       aes(x = jahr, y = diff_1_rel, label = diff_1_rel)) +
  geom_line(color = "blue", size = 1.2) +
  xlab("Jahr") +
  ylab("") +
  ylim(-10, 20) +
  scale_x_continuous(breaks=min(data_jahr_kat_aggr["jahr"]):
                       max(data_jahr_kat_aggr["jahr"])) +
  geom_hline(yintercept = 0, color = "red") +
  ggtitle("Wachstumsrate KMU Deutschland in Prozent") +
  geom_text(hjust = 0, vjust = -0.5,
            aes(label=ifelse(jahr %in% c(2009, 2011, 2014, 2016, 2018, 2020),
                             paste(diff_1_rel, "%", sep = ""), ''))) +
  geom_text(hjust=0, vjust=1.5,
            aes(label = ifelse(jahr == 2015,
                             paste(diff_1_rel, "%", sep = ""), ''))) +
  ggplot2_theme

# Momente für Wachstum:

mean(data_jahr_kat_aggr$diff_1, na.rm = TRUE)
mean(data_jahr_kat_aggr$diff_1_rel, na.rm = TRUE)
sd(data_jahr_kat_aggr$diff_1, na.rm = TRUE)
sd(data_jahr_kat_aggr$diff_1_rel, na.rm = TRUE)