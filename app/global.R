library(shiny)
library(tidyverse)
library(ggiraph)
library(ggrepel)
library(DBI)
library(dbplyr)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_shinyappar.R", encoding = "utf-8", echo = FALSE)

# ---- DB-uppkoppling ----
con     <- shiny_uppkoppling_las("oppna_data")
tbl_brp <- tbl(con, dbplyr::in_schema("tillvaxtverket", "brp_plus_normaliserad"))

# ---- Hämta max-år som sträng (matchar DB-kolumnens typ) ----
max_year_int <- tbl_brp %>%
  summarise(max_year = max(year, na.rm = TRUE)) %>%
  dplyr::pull(max_year) %>%
  as.integer()

max_year    <- as.character(max_year_int)
max_year_t5 <- as.character(max_year_int - 5L)

# ---- Hämta bara de två relevanta åren ----
brp_plus_raw <- tbl_brp %>%
  filter(year %in% c(max_year, max_year_t5)) %>%
  collect()

# ---- Rensa och pivotera ----
# Pivoterar både normaliserat index (varde_norm) och verkligt värde (value).
# Verkligt värde saknas för sammansatta index (Regionindex m.fl.) -> NA där.
brp_plus_df <- brp_plus_raw %>%
  filter(gender == "Båda könen") %>%
  select(municipality_id, municipality, municipality_type,
         huvudomrade, fraga, vand_varde, year, varde_norm, value) %>%
  distinct() %>%
  pivot_wider(names_from = year, values_from = c(varde_norm, value)) %>%
  select(
    municipality_id, municipality, municipality_type, huvudomrade, fraga, vand_varde,
    index_t5 = !!paste0("varde_norm_", max_year_t5),
    value_t  = !!paste0("varde_norm_", max_year),
    real_t5  = !!paste0("value_", max_year_t5),
    real_t   = !!paste0("value_", max_year)
  ) %>%
  # Förändring i absoluta indexpoäng (för sammansatta index)
  mutate(forandring = value_t - index_t5) %>%
  filter(
    !is.na(index_t5),
    !is.na(value_t)
  )

# ---- Riksreferenser per fraga ----
riks_ref <- brp_plus_df %>%
  group_by(huvudomrade, fraga) %>%
  summarise(
    riks_index_t5   = mean(index_t5,   na.rm = TRUE),
    riks_forandring = mean(forandring,  na.rm = TRUE),
    .groups = "drop"
  )

# ---- Urval för filter i UI ----

# Geografi: alla unika municipality sorterade
geo_choices <- brp_plus_df %>%
  select(municipality_id, municipality, municipality_type) %>%
  distinct() %>%
  arrange(municipality_type, municipality) %>%
  mutate(label = paste0(municipality, ifelse(municipality_type == "L", " (län)", " (kommun)")))

# Nyckeltal: fraga-värden sorterade per geografi-typ
fraga_choices <- brp_plus_df %>%
  arrange(huvudomrade, fraga) %>%
  dplyr::pull(fraga) %>%
  unique()

fraga_choices_lan <- brp_plus_df %>%
  filter(municipality_type == "L") %>%
  arrange(huvudomrade, fraga) %>%
  dplyr::pull(fraga) %>%
  unique()

fraga_choices_kommun <- brp_plus_df %>%
  filter(municipality_type == "K") %>%
  arrange(huvudomrade, fraga) %>%
  dplyr::pull(fraga) %>%
  unique()

dalarna_id <- geo_choices %>%
  filter(str_detect(municipality, "Dalarna")) %>%
  slice(1) %>%
  dplyr::pull(municipality_id)

livskvalitet_fraga <- fraga_choices[str_detect(fraga_choices, "Livskvalitet")][1]

# Län: för markerings-filtret i kommunvy (de 21 länen)
lan_choices <- brp_plus_df %>%
  filter(municipality_type == "L") %>%
  select(municipality_id, municipality) %>%
  distinct() %>%
  arrange(municipality) %>%
  mutate(lan_prefix = substr(municipality_id, 3, 4))  # 0020 -> "20"

# ============================================================
# Data för spindeldiagram (tema-index, alla år, län + kommun)
# ============================================================

spider_raw <- tbl_brp %>%
  filter(gender == "Båda könen") %>%
  collect()

# De två huvudområdena
spider_huvudomraden <- c("Livskvalitet", "Hållbarhet")

# Axlar = tema-nivåns index: fraga som innehåller "Regionindex" (län) eller
# "Kommunindex" (kommun) men INTE huvudområdesnamnet (då fångar vi tema-indexen,
# ej huvudområdes- eller indikatornivå)
spider_df <- spider_raw %>%
  filter(
    str_detect(fraga, "Regionindex|Kommunindex"),
    !str_detect(fraga, paste(spider_huvudomraden, collapse = "|")),
    huvudomrade %in% spider_huvudomraden
  ) %>%
  select(municipality_id, municipality, municipality_type, year, huvudomrade, fraga, varde_norm)

# Tillgängliga år (senaste förvalt)
spider_ar <- sort(unique(spider_df$year), decreasing = TRUE)

# Geo-val för spindeln (län + kommun), default Dalarna
spider_geo_choices <- spider_df %>%
  distinct(municipality_id, municipality, municipality_type) %>%
  arrange(municipality_type, municipality) %>%
  mutate(label = paste0(municipality, ifelse(municipality_type == "L", " (län)", " (kommun)")))

spider_dalarna_id <- spider_geo_choices %>%
  filter(municipality_type == "L", str_detect(municipality, "Dalarna")) %>%
  slice(1) %>%
  dplyr::pull(municipality_id)
