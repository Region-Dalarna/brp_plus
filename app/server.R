library(shiny)
library(shinyWidgets)
library(tidyverse)
library(ggiraph)
library(ggrepel)

# coord_radar: raka, slutna linjer i spindeldiagrammet
coord_radar <- function(theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggplot2::ggproto("CoordRadar", ggplot2::CoordPolar,
                   theta = theta, r = r, start = start,
                   direction = sign(direction),
                   is_linear = function(coord) TRUE)
}

server <- function(input, output, session) {

  # ---- Fyll geo-filter ----
  updatePickerInput(session, "val_geo",
                    choices  = setNames(geo_choices$municipality_id, geo_choices$label),
                    selected = dalarna_id
  )

  updatePickerInput(session, "val_fraga",
                    choices  = fraga_choices,
                    selected = livskvalitet_fraga
  )

  # ---- Reaktiv: vald geografi-typ ----
  vald_type <- reactive({
    req(input$val_geo)
    geo_choices %>%
      filter(municipality_id == input$val_geo) %>%
      pull(municipality_type)
  })

  # ---- Visa/dölj läns-filter för conditionalPanel ----
  output$visa_lan_filter <- reactive({
    req(vald_type())
    vald_type() == "K"
  })
  outputOptions(output, "visa_lan_filter", suspendWhenHidden = FALSE)

  # ---- Filtrera nyckeltal baserat på vald geografi ----
  observeEvent(input$val_geo, {
    req(input$val_geo)
    typ <- vald_type()
    req(length(typ) > 0)

    nya_val <- if (typ == "L") fraga_choices_lan else fraga_choices_kommun

    nuvarande <- if (!is.null(input$val_fraga) && input$val_fraga %in% nya_val) {
      input$val_fraga
    } else {
      nya_val[str_detect(nya_val, "Livskvalitet")][1]
    }

    updatePickerInput(session, "val_fraga",
                      choices  = nya_val,
                      selected = nuvarande
    )

    # Uppdatera läns-filtret till vald kommuns eget län
    if (typ == "K") {
      vald_lan_prefix <- substr(input$val_geo, 1, 2)
      forvalt_lan <- lan_choices %>%
        filter(lan_prefix == vald_lan_prefix) %>%
        pull(municipality_id)

      updatePickerInput(session, "val_lan_markera",
                        choices  = setNames(lan_choices$municipality_id, lan_choices$municipality),
                        selected = if (length(forvalt_lan) > 0) forvalt_lan else lan_choices$municipality_id[1]
      )
    }
  })

  # ---- Årsinfo ----
  output$ar_info <- renderUI({
    div(
      tags$span(class = "brp-ar-label", "Data avser:"),
      tags$span(class = "brp-ar-value",
                paste0(max_year_t5, " och ", max_year))
    )
  })

  # ---- Reaktivt dataset för valt nyckeltal ----
  df_fraga <- reactive({
    req(input$val_fraga)
    brp_plus_df %>%
      filter(fraga == input$val_fraga)
  })

  riks_fraga <- reactive({
    req(input$val_fraga)
    riks_ref %>%
      filter(fraga == input$val_fraga)
  })

  # ---- Scatter-diagram ----
  output$diagram_brp <- renderGirafe({

    df  <- df_fraga()
    typ <- vald_type()

    validate(
      need(nrow(df) > 0, "Ingen data för valt nyckeltal.")
    )

    # Visa bara enheter av samma typ som vald (län ELLER kommun)
    df <- df %>% filter(municipality_type == typ)

    validate(
      need(nrow(df) > 0, "Ingen data för valt nyckeltal.")
    )

    # Medel beräknas över de visade enheterna (samma typ)
    riks_x <- mean(df$index_t5,   na.rm = TRUE)
    riks_y <- mean(df$forandring, na.rm = TRUE)

    # Länsprefix för vald enhet
    vald_lan_prefix <- substr(input$val_geo, 1, 2)

    # Länsprefix för markerat län (kommunvy): 0020 -> "20"
    markerat_lan_prefix <- if (typ == "K" && !is.null(input$val_lan_markera)) {
      substr(input$val_lan_markera, 3, 4)
    } else {
      NULL
    }

    df <- df %>%
      mutate(
        ar_vald      = municipality_id == input$val_geo,
        ar_markerat_lan = if (!is.null(markerat_lan_prefix)) {
          !ar_vald & substr(municipality_id, 1, 2) == markerat_lan_prefix
        } else {
          FALSE
        },
        tooltip_text = paste0(
          "<b>", municipality, "</b><br>",
          "Index ", max_year_t5, ": ", round(index_t5, 1), "<br>",
          "F\u00f6r\u00e4ndring: ", round(forandring, 1), " %"
        )
      )

    # Axelgränser med marginal
    x_min <- min(df$index_t5,   na.rm = TRUE)
    x_max <- max(df$index_t5,   na.rm = TRUE)
    y_min <- min(df$forandring, na.rm = TRUE)
    y_max <- max(df$forandring, na.rm = TRUE)
    x_pad <- (x_max - x_min) * 0.08
    y_pad <- (y_max - y_min) * 0.12

    etikett_x_vanster <- x_min - x_pad * 0.3
    etikett_x_hoger   <- x_max + x_pad * 0.3
    etikett_y_topp    <- y_max + y_pad * 0.55
    etikett_y_botten  <- y_min - y_pad * 0.55

    p <- ggplot(df, aes(x = index_t5, y = forandring)) +

      # Referenslinjer
      geom_hline(yintercept = riks_y, colour = "#E8681A", linewidth = 0.9) +
      geom_vline(xintercept = riks_x, colour = "#F5C518", linewidth = 0.9) +

      # Trendlinje
      geom_smooth(aes(linetype = "Linjär trend"),
                  method = "lm", formula = y ~ x, se = FALSE,
                  colour = "#5b8db8", linewidth = 0.7) +
      scale_linetype_manual(name = NULL, values = c("Linjär trend" = "dashed")) +

      # Referensetiketten
      annotate("label",
               x = riks_x, y = riks_y,
               label = "Medel",
               size = 3.2, fill = "white", colour = "#333333",
               label.padding = unit(0.25, "lines")) +

      # Övriga punkter
      geom_point_interactive(
        data = df %>% filter(!ar_vald, !ar_markerat_lan),
        aes(tooltip = tooltip_text, data_id = municipality),
        colour = "#aac4d8", size = 2.5, alpha = 0.8
      ) +

      # Markerat läns kommuner
      geom_point_interactive(
        data = df %>% filter(ar_markerat_lan),
        aes(tooltip = tooltip_text, data_id = municipality),
        colour = "#e07b39", size = 3, alpha = 0.95
      ) +

      # Vald geografi, markerad med diamant
      geom_point_interactive(
        data = df %>% filter(ar_vald),
        aes(tooltip = tooltip_text, data_id = municipality),
        colour = "#158daf", size = 5, shape = 18
      ) +

      # Namnetiketter: alltid för län, bara vald + markerat län för kommuner
      geom_text_repel(
        data = if (typ == "L") df else df %>% filter(ar_vald | ar_markerat_lan),
        aes(label = municipality),
        size = 2.8, colour = "#333333",
        segment.colour = "#aaaaaa", segment.size = 0.3,
        max.overlaps = 20, seed = 42
      ) +

      # Kvadrantetiketter
      annotate("label", x = etikett_x_vanster, y = etikett_y_topp,
               label = "Kommer ikapp", size = 3, fontface = "italic",
               fill = "#f5f5f5", colour = "#666", hjust = 0) +
      annotate("label", x = etikett_x_hoger, y = etikett_y_topp,
               label = "Drar ifr\u00e5n", size = 3, fontface = "italic",
               fill = "#f5f5f5", colour = "#666", hjust = 1) +
      annotate("label", x = etikett_x_vanster, y = etikett_y_botten,
               label = "Halkar efter", size = 3, fontface = "italic",
               fill = "#f5f5f5", colour = "#666", hjust = 0) +
      annotate("label", x = etikett_x_hoger, y = etikett_y_botten,
               label = "Tappar fart", size = 3, fontface = "italic",
               fill = "#f5f5f5", colour = "#666", hjust = 1) +

      # Axlar och titlar
      scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      labs(
        title = paste0(input$val_fraga, " \u2013 f\u00f6r\u00e4ndring ",
                       max_year_t5, "\u2013", max_year),
        x     = paste0("Index \u00e5r ", max_year_t5),
        y     = paste0("F\u00f6r\u00e4ndring ", max_year_t5, "\u2013", max_year, " (%)")
      ) +

      theme_minimal(base_family = "Poppins") +
      theme(
        plot.title       = element_text(size = 13, colour = "#333333", margin = margin(b = 8)),
        axis.title       = element_text(size = 10, colour = "#555555"),
        axis.text        = element_text(size = 9,  colour = "#555555"),
        panel.grid.major = element_line(colour = "#e8e8e8"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        legend.text      = element_text(size = 9),
        plot.background  = element_rect(fill = "white", colour = NA),
        plot.margin      = margin(12, 20, 12, 12)
      )

    girafe(
      ggobj = p,
      width_svg  = 10,
      height_svg = 6,
      options = list(
        opts_hover(css = "fill:#158daf; stroke:#0d6d89; r:5px;"),
        opts_tooltip(
          css = "background:#fff; border:1px solid #ccc; border-radius:4px;
                 padding:6px 10px; font-family:Poppins,sans-serif; font-size:12px;",
          use_fill = TRUE
        ),
        opts_toolbar(saveaspng = TRUE, position = "topright",
                     hidden = c("lasso_select", "lasso_deselect"))
      )
    )
  })

  # ============================================================
  # Spindeldiagram
  # ============================================================

  updatePickerInput(session, "spider_lan",
                    choices  = setNames(spider_geo_choices$municipality_id, spider_geo_choices$label),
                    selected = spider_dalarna_id
  )

  updatePickerInput(session, "spider_ar",
                    choices  = spider_ar,
                    selected = spider_ar[1]
  )

  # ---- Hjälpfunktion: bygg spindeldiagram för ett huvudområde ----
  bygg_spindel <- function(valt_huvudomrade) {
    req(input$spider_lan, input$spider_ar)

    # Vald enhet och dess typ (referenser jämförs inom samma typ)
    vald_info <- spider_geo_choices %>%
      filter(municipality_id == input$spider_lan)
    vald_namn <- vald_info$municipality
    vald_typ  <- vald_info$municipality_type

    req(length(vald_typ) > 0)

    df_ar <- spider_df %>%
      filter(year == input$spider_ar,
             huvudomrade == valt_huvudomrade,
             municipality_type == vald_typ)

    validate(
      need(nrow(df_ar) > 0, paste0("Ingen data för ", valt_huvudomrade, "."))
    )

    # Referenser: max/min bland alla enheter av samma typ per fraga (tema)
    ref <- df_ar %>%
      group_by(fraga) %>%
      summarise(
        positiv = max(varde_norm, na.rm = TRUE),
        negativ = min(varde_norm, na.rm = TRUE),
        .groups = "drop"
      )

    vald <- df_ar %>%
      filter(municipality_id == input$spider_lan) %>%
      select(fraga, vald_varde = varde_norm)

    # Etiketter: ta bort både "- Regionindex" och "- Kommunindex"
    strip_suffix <- function(x) str_remove(x, " - (Region|Kommun)index")

    # Referenstext: "länet" eller "kommunen" beroende på typ
    ref_enhet <- if (vald_typ == "L") "länet" else "kommunen"

    axis_order <- sort(ref$fraga)

    # Min-max-normalisering per axel: negativ referens -> 0 (innerst),
    # positiv referens -> 100 (ytterst), enhetens värde placeras däremellan.
    # Råvärdet (varde_norm) behålls för tooltipen.
    ref_span <- ref %>%
      mutate(span = positiv - negativ)

    plot_df <- bind_rows(
      vald %>% transmute(fraga, serie = "vald_varde", value_raw = vald_varde),
      ref  %>% transmute(fraga, serie = "positiv",    value_raw = positiv),
      ref  %>% transmute(fraga, serie = "negativ",    value_raw = negativ)
    ) %>%
      left_join(ref_span %>% select(fraga, negativ, span), by = "fraga") %>%
      mutate(
        value = ifelse(span > 0, (value_raw - negativ) / span * 100, 50),
        axis_label   = strip_suffix(fraga),
        axis_label   = factor(axis_label, levels = strip_suffix(axis_order)),
        serie        = factor(serie, levels = c("vald_varde", "positiv", "negativ")),
        tooltip_text = ifelse(
          serie == "vald_varde",
          paste0("<b>", strip_suffix(fraga), "</b><br>",
                 "Index: ", round(value_raw, 1), "<br>",
                 round(value), " % av v\u00e4gen mot b\u00e4sta ", ref_enhet),
          paste0("<b>", strip_suffix(fraga), "</b><br>",
                 "Index: ", round(value_raw, 1))
        )
      )

    serie_labels <- c(
      vald_varde = vald_namn,
      positiv    = "Positiv referens",
      negativ    = "Negativ referens"
    )
    serie_colors <- c(
      vald_varde = "#158daf",
      positiv    = "#4F7A28",
      negativ    = "#A52A2A"
    )

    p <- ggplot(plot_df, aes(x = axis_label, y = value, group = serie, colour = serie)) +
      geom_polygon(fill = NA, linewidth = 0.8) +
      geom_point_interactive(
        aes(tooltip = tooltip_text, data_id = paste(serie, axis_label)),
        size = 2.2
      ) +
      coord_radar() +
      scale_y_continuous(limits = c(0, 100)) +
      scale_colour_manual(values = serie_colors, labels = serie_labels, name = NULL) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
      labs(title = valt_huvudomrade, x = NULL, y = NULL) +
      theme_minimal(base_family = "Poppins") +
      theme(
        plot.title       = element_text(size = 13, colour = "#333333", hjust = 0.5, margin = margin(b = 8)),
        axis.text.x      = element_text(size = 7.5, colour = "#444444"),
        axis.text.y      = element_blank(),
        panel.grid.major = element_line(colour = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        legend.position  = "bottom",
        legend.text      = element_text(size = 8),
        plot.background  = element_rect(fill = "white", colour = NA),
        plot.margin      = margin(8, 28, 8, 28)
      )

    girafe(
      ggobj = p,
      width_svg  = 5.5,
      height_svg = 6,
      options = list(
        opts_hover(css = "r:5px;"),
        opts_tooltip(
          css = "background:#fff; border:1px solid #ccc; border-radius:4px;
                 padding:6px 10px; font-family:Poppins,sans-serif; font-size:12px;",
          use_fill = TRUE
        ),
        opts_toolbar(saveaspng = TRUE, position = "topleft",
                     hidden = c("lasso_select", "lasso_deselect"))
      )
    )
  }

  output$diagram_spindel_livs  <- renderGirafe({ bygg_spindel("Livskvalitet") })
  output$diagram_spindel_hallb <- renderGirafe({ bygg_spindel("Hållbarhet") })

} # server

shinyServer(server)
