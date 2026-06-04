library(shiny)
library(ggiraph)

shinyUI(tagList(

  tags$head(
    tags$title("BRP+ \u2013 experimentyta"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$link(rel = "stylesheet", type = "text/css", href = "regiondalarna_ruf.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "app.css")
  ),

  # ---- Header ----
  tags$header(
    class = "rd-header",
    div(
      class = "rd-header__title",
      "BRP+ \u2013 experimentyta för visualiseringar"
    ),
    tags$a(
      href = "https://www.regiondalarna.se/verksamhet/regional-utveckling/statistik-och-rapporter/",
      target = "_blank",
      class = "rd-header__right",
      tags$img(src = "logo_liggande_fri_vit.png", alt = "Region Dalarna"),
      tags$span("Samhällsanalys")
    )
  ),

  # ---- Huvudinnehåll ----
  fluidPage(

    tabsetPanel(

      # ----------------------------
      # Flik 1: Utveckling över tid
      # ----------------------------
      tabPanel("Utveckling över tid",

               div(class = "brp-layout",

                   fluidRow(

                     # Vänster: filter
                     column(
                       width = 3,
                       class = "brp-controls",

                       # Geografi-filter
                       selectInput(
                         inputId   = "val_geo",
                         label     = "Välj län eller kommun:",
                         choices   = NULL,
                         selectize = TRUE
                       ),

                       # Nyckeltal-filter
                       selectInput(
                         inputId   = "val_fraga",
                         label     = "Välj nyckeltal:",
                         choices   = NULL,
                         selectize = TRUE
                       ),

                       # Läns-filter (visas bara i kommunvy)
                       conditionalPanel(
                         condition = "output.visa_lan_filter",
                         selectInput(
                           inputId   = "val_lan_markera",
                           label     = "Markera kommuner i län:",
                           choices   = NULL,
                           selectize = TRUE
                         )
                       ),

                       # Årsinfo
                       div(
                         class = "brp-ar-info",
                         uiOutput("ar_info")
                       )
                     ),

                     # Höger: diagram
                     column(
                       width = 9,
                       div(class = "brp-diagram-cell",
                           girafeOutput("diagram_brp", width = "100%", height = "100%")
                       )
                     )

                   ) # fluidRow
               ) # div.brp-layout
      ), # tabPanel

      # ----------------------------
      # Flik 2: Spindeldiagram
      # ----------------------------
      tabPanel("Spindeldiagram",

               div(class = "brp-layout",

                   fluidRow(

                     # Vänster: filter
                     column(
                       width = 3,
                       class = "brp-controls",

                       selectInput(
                         inputId   = "spider_lan",
                         label     = "Välj län eller kommun:",
                         choices   = NULL,
                         selectize = TRUE
                       ),

                       selectInput(
                         inputId   = "spider_ar",
                         label     = "Välj år:",
                         choices   = NULL,
                         selectize = TRUE
                       ),

                       div(
                         class = "brp-ar-info",
                         HTML("Diagrammet visar länets regionindex per huvudområde
                     (0\u2013100). <b>Positiv referens</b> är högsta värdet bland
                     alla län, <b>negativ referens</b> det lägsta.")
                       )
                     ),

                     # Höger: två spindeldiagram bredvid varandra
                     column(
                       width = 9,
                       fluidRow(
                         column(
                           width = 6,
                           div(class = "brp-diagram-cell",
                               girafeOutput("diagram_spindel_livs", width = "100%", height = "100%")
                           )
                         ),
                         column(
                           width = 6,
                           div(class = "brp-diagram-cell",
                               girafeOutput("diagram_spindel_hallb", width = "100%", height = "100%")
                           )
                         )
                       )
                     )

                   ) # fluidRow
               ) # div.brp-layout
      ), # tabPanel Spindeldiagram

      # ----------------------------
      # Flik 3: Om BRP+
      # ----------------------------
      tabPanel("Om BRP+",

               div(
                 style = "max-width: 800px; margin-top: 24px; color: #444; font-size: 14px; line-height: 1.7;",
                 HTML("
            <p style='padding: 10px 12px; background: #f0f7fa; border-left: 3px solid #158daf; border-radius: 3px;'>
              <b>Observera:</b> Detta är en experimentyta där Samhällsanalys vid Region Dalarna,
              tillsammans med länets kommuner, prövar olika sätt att visualisera och
              tillgängliggöra BRP+. Innehåll och utformning kan ändras, och diagrammen är inte
              att betrakta som färdiga publikationer.
            </p>

            <h3>Vad är BRP+?</h3>
            <p>
              BRP+ (Bruttoregionalprodukt Plus) är ett sammansatt livskvalitetsindex som
              regionerna tagit fram inom ramen för Reglab i samarbete med Tillväxtverket som
              också förvaltar verktyget. BRP+ har tagits fram för att komplettera den
              traditionella BRP-statistiken med mått på livskvalitet mätt i olika dimensioner
              och där hållbarheten är en viktig faktor. Indexet bygger på ett antal indikatorer,
              exempelvis inom områdena ekonomi, hälsa, miljö, trygghet och demokrati.
            </p>
            <p>
              Läs mer hos Tillväxtverket:
              <a href='https://tillvaxtverket.se/tillvaxtverket/statistikochanalys/statistikomregionalutveckling/breddatmattparegionalutvecklingbrp.1624.html' target='_blank'>Breddat mått på regional utveckling (BRP+)</a>.<br>
              Utforska data direkt: <a href='https://kolada.se/verktyg/jamforaren/?focus=27508&report=150159' target='_blank'>BRP+ på Kolada</a>.
            </p>

            <h3>Fliken \u201dUtveckling över tid\u201d</h3>
            <p>
              Här visas hur länen (eller kommunerna) förhåller sig till varandra avseende ett valt
              nyckeltal. X-axeln visar indexvärdet fem år tidigare (år t&#8209;5) och y-axeln
              förändringen under femårsperioden (i procent). Referenslinjerna markerar
              medelvärdet för alla enheter:
            </p>
            <ul>
              <li><b>Orange horisontell linje</b> – medelvärde för förändringen (y-axeln)</li>
              <li><b>Gul vertikal linje</b> – medelvärde för indexnivån (x-axeln)</li>
            </ul>
            <p>De fyra kvadranterna tolkas som:</p>
            <ul>
              <li><i>Kommer ikapp</i> – låg startnivå men snabb förbättring</li>
              <li><i>Drar ifrån</i> – hög startnivå och fortsatt förbättring</li>
              <li><i>Halkar efter</i> – låg startnivå och försämring</li>
              <li><i>Tappar fart</i> – hög startnivå men försämring</li>
            </ul>
            <p>
              Den streckade linjen visar en linjär trend för sambandet mellan startnivå och
              förändring. Väljer du en kommun lyfts den fram, och du kan markera kommunerna i ett
              valfritt län i en egen färg för jämförelse.
            </p>

            <h3>Fliken \u201dSpindeldiagram\u201d</h3>
            <p>
              Spindeldiagrammen visar ett valt läns regionindex per tema, uppdelat på de två
              huvudområdena <b>Livskvalitet</b> och <b>Hållbarhet</b>. Varje axel är
              normaliserad så att <b>positiv referens</b> (det län som ligger bäst till) hamnar
              ytterst och <b>negativ referens</b> (det län som ligger sämst till) innerst.
              Länets punkt placeras däremellan, vilket gör det lätt att se var länet står i
              förhållande till övriga län. Håll muspekaren över en punkt för att se det faktiska
              indexvärdet.
            </p>

            <h3>Källa och kontakt</h3>
            <p>
              Visualisering: Samhällsanalys, Region Dalarna.<br>
              Fr\u00e5gor: <a href='mailto:samhallsanalys@regiondalarna.se'>samhallsanalys@regiondalarna.se</a>
            </p>
          ")
               )

      ) # tabPanel Om BRP+

    ) # tabsetPanel

  ), # fluidPage

  # ---- Footer ----
  tags$footer(
    class = "rd-footer",
    HTML(
      "Samh\u00e4llsanalys, Region Dalarna &middot; ",
      "<a href='mailto:samhallsanalys@regiondalarna.se'>samhallsanalys@regiondalarna.se</a>"
    )
  )

)) # shinyUI / tagList
