# BIA-Messung 
# 26.02.25 GS 



# Pakete laden ------------------------------------------------------------

if (!require("pacman")){
  install.packages("pacman")
}


pacman::p_load(
  tidyverse,
  readr,
  here,
  ggtext,
  ggwordcloud,
  ggpubr,
  janitor,
  lubridate
)




# DataFrame (BIA) -------------------------------------------------------------

df_data_BIA <- tibble(
  ID = c("P1", "P2", "P1", "P2"),
  Teilnehmer = c("nicht Exponiert", "Exponiert", "nicht Exponiert", "Exponiert"),
  Erhebung = c("Baseline", "Baseline", "Follow up", "Follow up"),
  Geschlecht = c(1, 1, 1, 1), # Männer = 1, Frauen = 0
  Alter = c(28, 31, 28, 31),
  Widerstand_Rz = c(532, 450, 570, 425),
  Reaktanz_Xc = c(52, 55, 60, 45),
  Groesse = c(186, 178, 186, 178),
  Gewicht = c(90, 107, 90, 105)
)

# Berechnung und Visualisierung -------------------------------------------
# Formel aus: https://www.egofit.de/biadata-org/

df_BIA_viz <- df_data_BIA %>%
  group_by(Teilnehmer) %>%
  mutate(
    FFM = -4.104 + (0.518 * (Groesse^2) / Widerstand_Rz) +
      (0.231 * Gewicht) + (0.130 * Reaktanz_Xc) +  # fettfreie Masse
      (4.229 * Geschlecht),
    ASMM = -4.211 + (0.267 * (Groesse^2) / Widerstand_Rz) +
      (0.095 * Gewicht) + (1.909 * Geschlecht) +  # Muskelmasse
      (-0.012 * Alter) + (0.058 * Reaktanz_Xc),
    FM = Gewicht - FFM,                           # Fettmasse
    ASMM_Prozent = (ASMM / Gewicht) * 100,        # Muskelmasse in (%)
    FM_Prozent = (FM / Gewicht) * 100,            # Fettmasse in (%)
    FFM_Prozent = ((FFM / Gewicht) * 100)         # ASMM_Prozent      # fettfreie Masse in (%)
  ) %>%
  mutate(
    BMI = Gewicht/(Groesse/100)^2
  ) %>% 
  ungroup() %>%
  mutate(
    across(
      c(13:16), ~ round(., digits = 1)
    )
  ) %>%
  pivot_longer(
    cols = c(13:15),
    names_to = "Messung",
    values_to = "Werte"
  ) %>%
  mutate(
    Messung = factor(
      Messung,
      levels = c(
        "FFM_Prozent",
        "ASMM_Prozent",
        "FM_Prozent"
      )
    )
  ) %>%
  mutate(
    Teilnehmer = factor(
      Teilnehmer,
      levels = c(
        "nicht Exponiert",
        "Exponiert"
      )
    )
  )


# BIA - Plot
df_BIA_viz %>%
  ggplot(
    aes(
      x = Teilnehmer,
      y = Werte,
      fill = Messung,
      alpha = Teilnehmer
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "stack",
    width = 0.6
  ) +
  geom_text(
    aes(label = scales::percent(Werte / 100, accuracy = 1)),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 5
  ) +
  facet_wrap(
    ~Erhebung
  ) +
  scale_fill_manual(
    values = c(
      "FFM_Prozent" = "#0072B2",
      "ASMM_Prozent" = "darkorange4",
      "FM_Prozent" = "#E69F00"
    )
  ) +
  scale_alpha_manual(
    values = c(
      "nicht Exponiert" = 0.6, # Transparenz für nicht Exponiert
      "Exponiert" = 1
    )
  ) +
  labs(
    title = "**3. Abbildung**: Veränderung von <span style = 'color:#0072B2'>**fettfreier Masse**</span>, <span style = 'color:darkorange4'>**Muskelmasse**</span> und <span style = 'color:#E69F00'>**Fettmasse**</span> \nnach 4 Wochen Lauftraining",
    subtitle = "Eine Analyse der Körperzusammensetzung",
    x = "Teilnehmer",
    y = "Werte in (%)",
    fill = "Körperzusammensetzung"
  ) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1)
  ) +
  theme_light(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold"),
    plot.title.position = "plot",
    legend.position = "none",
    plot.title = element_markdown(margin = margin(b = 10)),
    text = element_text(family = "Arial"),
    panel.grid = element_blank()
  )

ggsave(here("Plots/Plot_BIA.png"))


# BMI - Plot
df_BIA_viz %>%
  ggplot(
    aes(
      x = Teilnehmer,
      y = BMI,
      fill = Erhebung,
      alpha = Teilnehmer
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    width = 0.6
  ) +
  geom_text(
    aes(label = BMI),  
    position = position_dodge(width = 0.5),  
    color = "white",
    size = 5,
    vjust = 15
  ) +
  labs(
    title = "**2. Abbildung**: Vergleich des BMI zwischen <span style = 'color:cadetblue4'>**Baseline**</span> und <span style = 'color:chocolate1'>**Follow-up**"
  ) + 
  theme_light(base_size = 12) +
  scale_alpha_manual(
    values = c(
      "nicht Exponiert" = 0.4, # Transparenz für nicht Exponiert
      "Exponiert" = 1
    )
  ) +
  scale_fill_manual(
    values = c(
      "Baseline" = "cadetblue4",
      "Follow up" = "chocolate1"
    )
  ) + 
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(margin = margin(b = 15)),
    panel.grid = element_blank()
  )

ggsave(here("Plots/Plot_BMI.png"))


# Strava Aktivitäten ------------------------------------------------------
## Strava Daten einelesen 

df_runs <- read.csv(
  here("Data/activities.csv")
  ) %>% 
  clean_names() #%>% 
  select(
    activity_id,
    activity_date,
    activity_name,
    activity_type,
    distance
  ) %>% 
  mutate(
    activity_date = format(
      mdy_hms(activity_date),
      format = "%m-%d-%Y"
    )
  ) %>% 
  filter(
    activity_type == "Run" & 
    between(activity_date, ("02-01-2025"), "03-02-2025") & 
    str_detect(activity_date, "2025")
  ) %>% 
  mutate(
    weeks = case_when(
      between(activity_date, "02-01-2025", "02-07-2025") ~ "Woche 1",
      between(activity_date, "02-08-2025", "02-14-2025") ~ "Woche 2",
      between(activity_date, "02-15-2025", "02-21-2025") ~ "Woche 3",
      between(activity_date, "02-22-2025", "03-30-2025") ~ "Woche 4",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(weeks) %>% 
  mutate(
    sum_distance = round(sum(distance), 1),
    anzahl_Runs = n()
  ) 

# Strava Plot
df_runs %>% 
  ggplot(
    aes(
      x = weeks,
      y = sum_distance
    )
  ) +
  geom_bar(
    stat = "identity",
    position = "dodge",
    fill = "chocolate1"
  ) +
  geom_point(
    color = "cadetblue4", 
    size = 3
  )+
  geom_line(
    group = 1,
    color = "cadetblue4", 
    size = 1
  )+
  geom_text(
    aes(label = str_glue("{sum_distance}km \n n = {anzahl_Runs}")),  
    position = position_dodge(width = 0.5),  
    color = "white",
    size = 5,
    vjust = 3
  ) +
  labs(
    title = "**1. Abbildung**: Wöchentliche Summe der Laufdistanz und Anzahl der Läufe (4 Wochen)",
    subtitle = str_glue("Total Distanz: **{sum(df_runs$distance)}**km, Gesamtanzahl der Läufe: **{nrow(df_runs)}**"),
    x = NULL,
    y = "Distanz"
  ) +
  theme_light(base_size = 12) +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(margin = margin(b = 10)),
    plot.subtitle = element_markdown(),
    panel.grid = element_blank()
  ) +
  scale_y_continuous(labels = scales::unit_format(unit = "km")) 



# Save 
ggsave(here("Plots/Plot_Runs_test.png"))



  
  





