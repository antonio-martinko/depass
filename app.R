
# pacman::p_load(tidyverse, 
#                activAnalyzer,
#                patchwork, 
#                jpeg, 
#                ggpubr,
#                ggforce,
#                fmsb,
#                rsconnect,
#                shinyscreenshot)

# install.packages(c("tidyverse", "activAnalyzer", "patchwork", 
#                    "jpeg", "ggpubr", "ggforce", 
#                    "fmsb", "rsconnect", "shinyscreenshot"))

library(tidyverse)
library(readr)
library(activAnalyzer)
library(patchwork)
library(jpeg)
library(ggpubr)
library(ggforce)
library(fmsb)
library(rsconnect)
library(shinyscreenshot)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    
    title = "DEPASS izvještaj",
    
    tags$head(
      tags$style(HTML(".navbar-nav > li > a, .navbar-default .navbar-brand {
                            padding-top: 35px !important; 
                            padding-bottom: 0 !important;
                            height: 80px;
                            font-size: 25px;
                            color: #2874A6;
                            }
                           .navbar {
                           min-height: 80px !important;
                      }"))
    ),
    
    tabPanel("O aplikaciji",
             
             h1("Dobrodošli u aplikaciju za predstavljanje izvještaja o vašim rezultatima!",
                style="text-align:center"),
             
             br(),

             tags$style(
             "#logo-grid {
             margin: 0 auto;
             width: 400px;
             display: grid;
             grid-template-columns: 150px 80px 80px;
             column-gap: 20px;
             }"),
             
             div(id = "logo-grid",
                 
                 div(
                   img(
                     src="depass_logo.jpg",
                     width="150px;")
                   ),
                 
                 div(
                   img(
                     src="unizg_logo.jpg",
                     width="80px;")
                 ),
                 
                 div(
                   img(
                     src="kif_logo.jpg",
                     width="80px;")
                 )
             ),
             
             br(),
             
             h2("Ova aplikacija vam dopušta stvaranje izvještaja o vašim rezultatima, ali i njihovo preuzimanje.",
                style="text-align:center"),
    
             h2("Kako bi se koristili aplikacijom slijedite sljedeće korake:",
                style="text-align:center"),
             
             br(),
             
             h3("1. U prostoru za unos", code("lozinke"),"upišite jedinstvenu lozinku koja vam je dodijeljena.",
                style="text-align:center"),
             h3("Pripazite na unos velikih/malih slova!",
                style="text-align:center"),
             h3("2. U idućem koraku izaberite roditelj (P), brat/sestra (S) ili dijete (C) među ponuđenim opcijama.",
                style="text-align:center"),
             h3("3. Pod", code("Spol"), "odaberite između opcija M ili Ž.",
                style="text-align:center"),
             h3("Ovo će biti bitno zbog nekih od izračuna unutar aplikacije.",
                style="text-align:center"),
             h3("4. Pod", code("Dob"), "unesite vaše godine.",
                style="text-align:center"),
             h3("5. Pod", code("Tjelesna visina"), "unesite vašu visinu u centimetrima.",
                style="text-align:center"),
             h3("6. Pod", code("Tjelesna masa"), "unesite vašu tjelesnu masu u kilogramima.",
                style="text-align:center"),
             h3("Kod unosa ovih vrijednosti se prihvaćaju samo cijeli brojevi!",
                style="text-align:center"),
             h3("7. Ako želite preuzeti izvještaj, kliknite", code("Download"), ".",
                style="text-align:center"),
             
             br(),
             
             h3("I to je to. Sada možete iskoristiti ovu aplikaciju kako bi stvorili i preuzeli svoj izvještaj.",
                style="text-align:center"),
             
             br(),
    ),
    
    tabPanel("Tjelesna aktivnost", 
             
             tags$style("#picture-grid {
                      display: grid;
                      grid-template-columns: 100px 1fr;
                      grid-gap: 75px;
                      }"),
             
             h1("Saznajte nešto više o tjelesnoj aktivnosti!"),
             
             br(),
             
             h2("Tjelesna aktivnost je svaki pokret tijela koji se izvodi aktivacijom skeletnih mišića i koji zahtijeva potrošnju energije, bilo to na poslu, putu na posao ili u slobodno vrijeme. Svaka od tih sastavnica uvelike pridonosi našem zdravlju - zdravlju srca i pluća, našem hormonskom, imunološkom i probavnom sustavu te ne manje važno, mentalnom zdravlju."),
             
             br(),
             
             h3("Dobrobiti tjelesne aktivnosti uključuju:",
                style="text-align:center"),
             
             br(),
             
             div(
             h3("-   Jače kosti 🦴"),
             h3("-   Bolje raspoloženje 😄"),
             h3("-   Smanjen rizik od pretilosti ⚖️"),
             h3("-   Poboljšanje kardiovaskularnog sustava ❤️"),
             h3("-   Veća mišićna masa 💪️"),
             style="text-align:center"),
             
             br(),
             
             h2("Ovim izvještajem dobit ćete na uvid koliko (vrijeme) i kojim intenzitetom (lagano, umjereno, žustro) ste Vi tjelesno aktivni, te ćete podatke lako moći usporediti s preporukama Svjetske zdravstvene organizacije (WHO) za tjelesnu aktivnost."),
             
             br(),
             
             h3("Biti", strong("sedentaran", .noWS = c("after")), ", odnosno ", strong("sjedilačko ponašanje", .noWS = c("after"))," se definira kao bilo koje ponašanje tijekom budnosti (isključujući san) tijekom kojeg sjedite ili ležite, trošeći niske razine energije."),
             
             br(),
             
             h3(strong("Aktivnosti laganog intenziteta", .noWS = c("after")), " zahtijevaju najmanje napora u usporedbi s umjerenim i intenzivnim aktivnostima, ali su prihvatljive jer potiču ljude da se više kreću kako bi ostvarili dodatne zdravstvene koristi, što može potaknuti višu razinu tjelesne aktivnosti. Test govorom: ", em("Možete pjevati dok obavljate aktivnost", .noWS = c("after")),"."),
             
             br(),
             
             div(id = "picture-grid",
                 
             div(
               img(
               src = "light_walking.jpg", 
               style = "border-radius: 150%", 
               width = "150px")
               ),
                 
             div(h3("Primjeri:"),
             
             h4("-   Lagana šetnja ili vožnja biciklom"),
             h4("-   Istezanje"),
             h4("-   Lagani trening s utezima"),
             h4("-   Rekreacijski sportovi (stolni tenis, igranje lovice, ribolov)"),
             h4("-   Lagani poslovi u vrtu i kući"))
             ),

             br(),
             
             h3(strong("Aktivnosti umjerenog intenziteta", .noWS = c("after")), " znače da radite dovoljno naporno da povećate puls i počnete se znojiti. Test govorom: ", em("Možete razgovarati, ali ne možete pjevati dok obavljate aktivnost", .noWS = c("after")), "."),
             
             br(),
             
             div(id = "picture-grid",
                 
                 div(
                   img(
                     src = "cycling_moderate.jpg", 
                     style = "border-radius: 150%", 
                     width = "150px")
                 ),
                 
                 div(h3("Primjeri:"),
                     
                     h4("-   Žustro hodanje"),
                     h4("-   Joga"),
                     h4("-   Trening s utezima"))
             ),
             
             br(),
             
             h3(strong("Aktivnosti žustrog intenziteta", .noWS = c("after")), " znače da jako i brzo dišete te da vam je puls znatno povećan. Test govorom: ", em("Ne možete reći više od nekoliko riječi bez pauze za dah dok obavljate aktivnost", .noWS = c("after")), "."),
             
             br(),
             
             div(id = "picture-grid",
                 
                 div(
                   img(
                     src = "running_vigorous.jpg", 
                     style = "border-radius: 150%", 
                     width = "150px")
                 ),
                 
                 div(h3("Primjeri:"),
                     
                     h4("-   Trčanje"),
                     h4("-   Planinarenje"),
                     h4("-   Aerobik visokog intenziteta"),
                     h4("-   Borilački sportovi (npr. karate, judo,...)"),
                     h4("-   Ekipni sportovi (npr. košarka, nogomet,...)"))
             ),
             
             br(),
             
             h1(strong("Savjeti za postizanje vaših ciljeva", .noWS = c("after"))),
             
             h2("Hodanje ima velike koristi za vaše tjelesno i mentalno zdravlje. Ako želite biti sigurni da ćete napraviti preporučeni broj koraka svaki dan, postoji nekoliko stvari koje možete učiniti kako biste sebi dali malo prednosti."),
             
             h3("1. Prošećite s prijateljima: Sam razgovor o vašim ciljevima i zajedničko vježbanje povećava šanse da ćete ih ispuniti. Imati nekoga s kime možete hodati može aktivnost učiniti manje usamljenom, a istovremeno vam daje nekoga tko će vam pomoći i držati vas odgovornima za postizanje postavljenih ciljeva."),
             h3("2. Podijelite hodanje u dijelove: Ako nemate cijeli sat u dnevnom rasporedu odvojite 4 ili 5 blokova od 15 minuta tijekom dana. Ova strategija će vam pomoći da se osjećate manje preopterećeno i učiniti ciljeve lakšim za postizanje. Istraživanja su pokazala da samo 2-5 minuta hodanja svaki sat može poništiti negativne učinke sat vremena sjedenja."),
             h3("3. Neka vam hodanje postane rutina: navike je teško stvoriti, ali ih je još teže riješiti se. Ako želite biti sigurni da ćete ostvariti svoj cilj neka hodanje postane dnevna navika i uskoro će to postati samo refleks."),
             h3("4. Hodajte brže: iako ćete napraviti manji broj koraka po kilometru što brže hodate, ukupno ćete napraviti više koraka u istom vremenskom razdoblju."),
             
             br(),
             
             tags$style(
             "#bordered {
             border: 5px solid #000;
             padding-left: 20px;
             }"),
             
             div(
               id = "bordered",
               h3("Smjernice za korake:"),
               h4("Za djecu od 6-12 godina, minimalni preporučeni dnevni broj koraka je 11 000 - 12 000 koraka dnevno za djevojčice i 13 000 - 15 000 koraka dnevno za dječake (Tudor Locke et al 2004.)"),
               h4("Centers for Disease Control and Prevention (CDC) preporuča 10 000 koraka po danu.")
               ),

             br(),
             
             h2("WHO (World Health Organization) preporuke za ", strong("odrasle", .noWS = c("after")), ":"),
             
             br(),
             
             h4("-   Sve odrasle osobe (18–65) trebale bi provoditi najmanje 150 do 300 minuta tjedno umjerene intenzivne ili 75 do 150 minuta tjedno visoko intenzivne aerobne aktivnosti."),
             h4("-   Kombinacije aktivnosti umjerenog i visokog intenziteta mogu se izvoditi kako bi se ispunila ova preporuka."),
             h4("-   Također, svatko bi trebao provoditi aktivnosti za jačanje mišićna minimalno 2 dana u tjednu te bi vježbe trebale uključivati sve veće mišićne skupine (noge, leđa, ruke, prsa, trbuh)."),
             h4("-   Zamjena sedentarnog ponašanja bilo kojom tjelesnom aktivnosti pruža dobrobiti za zdravlje."),
             h4("-   Zbog odnosa doze i odgovora između tjelesne aktivnosti i zdravlja, osobe koje žele dodatno poboljšati svoju kondiciju, smanjiti rizik od kroničnih bolesti i invaliditeta, i/ili spriječiti nezdravo dobivanje na težini mogu imati koristi od prekoračenja minimalno preporučenih količina tjelesne aktivnost."),
             
             br(),
             
             h2("Preporuke za ", strong("djecu", .noWS = c("after")), ":"),
             
             br(),
             
             h4("-   Djeca i adolescenti bi trebali dnevno provoditi barem 60 minuta u prosjeku u umjereno do visoko intenzivnoj, pretežno aerobnoj, tjelesnoj aktivnosti tijekom tjedna."),
             h4("-   Aerobne aktivnosti visokog intenziteta, kao i one koje jačaju mišiće i kosti, bi se trebale provoditi najmanje 3 dana u tjednu."),
             h4("-   Djeca i adolescenti bi trebali ograničiti vrijeme provedeno u sjedećem položaju, posebno vrijeme provedeno ispred ekrana u slobodnom vremenu.")
    ),
    
    tabPanel("Moj izvještaj",
             titlePanel("Stvorite svoj osobni izvještaj"),
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "lozinka", 
                           label = "Lozinka"),
                 tags$hr(),
                 radioButtons(inputId = "uzrast", 
                              label = "Izaberite opciju roditelj (P), brat/sestra (S) ili dijete (C)", 
                              choices = c("P", "S", "C"), 
                              selected = "C"),
                 radioButtons(inputId = "sex", 
                              label = "Spol", 
                              choices = c("M" = "male", "Ž" = "female"), 
                              selected = "male"),
                 numericInput(inputId = "age", 
                              label = "Dob", 
                              value = 0, 
                              min = 5, max = 75),
                 numericInput(inputId = "weight", 
                              label = "Tjelesna masa (u kg)", 
                              value = 0, 
                              min = 10, max = 175),
                 numericInput(inputId = "height", 
                              label = "Tjelesna visina (u cm)", 
                              value = 0, 
                              min = 0, max = 240)
               ),
               
               mainPanel(
                 
                 h2("Ukupna količina aktivnosti"),
                 
                 br(),
                 
                 plotOutput("activity_dots"),
                 
                 actionButton("screen_totalactivity", "Preuzmi!"),
                 
                 # plotOutput("plot_sed"),
                 # plotOutput("plot_lpa"),
                 # plotOutput("plot_mvpa"),
                 
                 br(),
                 
                 h2("Ukupan broj koraka"),
                 
                 plotOutput("plot_steps"),
                 
                 actionButton("screen_totalsteps", "Preuzmi!"),
                 
                 br(),
                 
                 h2("Dnevna količina aktivnosti"),
                 plotOutput("plot_daily"),
                 
                 actionButton("screen_activitydaily", "Preuzmi!"),
                 
                 br(),
                 
                 h2("Dnevni broj koraka"),
                 plotOutput("plot_dailysteps"),
                 
                 actionButton("screen_stepsdaily", "Preuzmi!"),
                 
                 br(),
                 
                 h2("Koliko koraka u kilometru, pitate se? Pa… ovisi. Saznajte koliko kilometara bi napravili na dan s najviše koraka!"),
                 h2(textOutput("distance")),
                 
                 br()
                 
                 )
               )
             )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # previously, read in the .agd file in a new R script file
  # use prepare_dataset to process the raw data
  # write the data frame as either excel or .csv file and then store it on Github
  # every other time in the app import the excel or .csv file and not a raw .agd file
  
  mydata <- reactive({
    
    data <- read_csv(
      paste0(
        "https://raw.githubusercontent.com/antonio-martinko/depass/main/user_files/",
        input$lozinka,
        ".csv"),
        col_types = "Tiiiiiiiiin") %>%
        as.data.frame()
    
    return(data)
    
  })
  
  cutpoints <- reactive({
    calculate_cutpoints(input$uzrast)
  })
  
  mydata_with_wear_marks <- reactive({
    mydata() %>%
      mark_wear_time(
        to_epoch = 10,
        cts = "vm",
        frame = 90, 
        allowanceFrame = 2, 
        streamFrame = 30
      )
  })
  
  # for children Evenson (2008)
  mydata_with_intensity_marks <- reactive({
    cutpoints <- if (input$uzrast == "P") { # Troiano (2008)
      list(
        sed_cutpoint = 99,
        mpa_cutpoint = 2020,
        vpa_cutpoint = 5999
      )
    } else { # Evenson (2008)
      list(
        sed_cutpoint = 100,
        mpa_cutpoint = 2296,
        vpa_cutpoint = 4012
      )
    }
    
    mark_intensity(
      data = mydata_with_wear_marks(), 
      col_axis = "vm", 
      equation = "Sasaki et al. (2011) [Adults]",
      sed_cutpoint = cutpoints$sed_cutpoint, 
      mpa_cutpoint = cutpoints$mpa_cutpoint, 
      vpa_cutpoint = cutpoints$vpa_cutpoint, 
      age = input$age,
      weight = input$weight,
      sex = input$sex
    )
  })
  
  
  results_by_day <- reactive({
    mydata_with_intensity_marks() %>%
      recap_by_day(
        age = input$age,
        weight = input$weight,
        sex = input$sex,
        valid_wear_time_start = "00:00:00",
        valid_wear_time_end = "23:59:00",
        start_first_bin = 0,
        start_last_bin = 10000,
        bin_width = 500)
  })
  
  mean_results <- reactive({
    results_by_day$df_all_metrics()  %>%
      average_results(minimum_wear_time = 8, fun = "mean")
  })
  
  median_results <- reactive({
    results_by_day$df_all_metrics()  %>%
      average_results(minimum_wear_time = 8, fun = "median")
  })
  
  # sum up the weekly results for all outcomes of interest
  summed_results <- reactive({
    results <- results_by_day()
    results$df_all_metrics %>%
      select(c(wear_time, minutes_SED, minutes_LPA, minutes_MPA, minutes_VPA,
               minutes_MVPA, percent_SED, percent_LPA, percent_MPA, percent_VPA,
               percent_MVPA, ratio_mvpa_sed, pal, total_steps)) %>%
      filter(wear_time >= 480) %>%
      summarise(across(where(is.numeric), sum)) %>%
      round(., digits = 0) %>%
      as.data.frame()
  })
  
  # summed weekly results only for activities
  activity_results <- reactive({
    results <- results_by_day()
    activity <- results$df_all_metrics %>%
      select(c(minutes_SED, minutes_LPA, minutes_MVPA)) %>%
      summarise(across(where(is.numeric), sum)) %>%
      round(., digits = 0) %>%
      pivot_longer(cols = c(minutes_SED, minutes_LPA, minutes_MVPA), 
                   names_to = "Ponašanje", 
                   values_to = "Trajanje") %>%
      mutate(Ponašanje = case_when(Ponašanje == "minutes_SED" ~ "Sedentarno ponašanje (min.)",
                                   Ponašanje == "minutes_LPA" ~ "Aktivnost niskog intenziteta (min.)",
                                   Ponašanje == "minutes_MVPA" ~ "Umjerena-do-žustra tjelesna aktivnost (min)")) %>%
      as.data.frame()
    
  })
  
  # sum up the weekly results for all outcomes of interest
  daily_results <- reactive({
    results <- results_by_day()
    results$df_all_metrics %>%
      filter(wear_time >= 480) %>%
      as.data.frame()
  })
  
  daily_plotresults <- reactive({
    daily_results() %>%
      select(c(date, minutes_SED, minutes_LPA, 
               # minutes_MPA, minutes_VPA,
               minutes_MVPA, 
               # percent_SED, percent_LPA, percent_MPA, percent_VPA,
               # percent_MVPA, ratio_mvpa_sed, 
               total_steps)) %>%
      tidyr::pivot_longer(cols = c(minutes_SED, minutes_LPA, 
                                   # minutes_MPA, minutes_VPA, 
                                   minutes_MVPA, 
                                   # percent_SED, percent_LPA, 
                                   # percent_MPA, percent_VPA, percent_MVPA, ratio_mvpa_sed, 
                                   total_steps), 
                          names_to = "variable", 
                          values_to = "value") %>% 
      dplyr::mutate(date = as.factor(date),
                    variable = forcats::fct_relevel(variable, 
                                                    "minutes_SED", "minutes_LPA",
                                                    # "minutes_MPA", "minutes_VPA", 
                                                    "minutes_MVPA", 
                                                    # "percent_SED", "percent_LPA", "percent_MPA", 
                                                    # "percent_VPA", "percent_MVPA", "ratio_mvpa_sed",
                                                    "total_steps"), 
                    variable = forcats::fct_recode(variable,
                                                   "Sjedilačko ponašanje (min)" = "minutes_SED",
                                                   "Vrijeme lagane tjelesne aktivnosti (min)" = "minutes_LPA",
                                                   # "Vrijeme umjerene tjelesne aktivnosti (min)" = "minutes_MPA",
                                                   # "Vrijeme žustre tjelesne aktivnosti (min)" = "minutes_VPA",
                                                   "Vrijeme umjerene-do-žustre tjelesne aktivnosti (min)" = "minutes_MVPA",
                                                   # "Udio vremena sjedilačkog ponašanja (%)" = "percent_SED",
                                                   # "Udio vremena lagane tjelesne aktivnosti (%)" = "percent_LPA",
                                                   # "Udio vremena umjerene tjelesne aktivnosti (%)" = "percent_MPA",
                                                   # "Udio vremena žustre tjelesne aktivnosti (%)" = "percent_VPA",
                                                   # "Udio vremena umjerene-do-žustre tjelesne aktivnosti (%)" = "percent_MVPA",
                                                   # "Omjer umjerene-do-žustre tjelesne aktivnosti / sjedilačkog ponašanja" = "ratio_mvpa_sed",
                                                   "Ukupni koraci" = "total_steps"))
  })
  
  # import jpegs to insert in ggplot backgrounds
  # sitting_sed <- readJPEG("sitting_sed.jpg")
  # gardening_lpa <- readJPEG("gardening_lpa.jpg")
  # weights_mpa <- readJPEG("weights_mpa.jpg")
  # hiking_vpa <- readJPEG("hiking_vpa.jpg")
  # heart_mvpa <- readJPEG("heart_mvpa.jpg")
  # walking_steps <- readJPEG("walking_steps.jpg")
  
  depass_logo <- readJPEG("depass_logo.jpg")
  unizg_logo <- readJPEG("unizg_logo.jpg")
  kif_logo <- readJPEG("kif_logo.jpg")
  
  sedentary <- readJPEG("sedentary.jpg")
  light_walking <- readJPEG("light_walking.jpg")
  cycling_moderate <- readJPEG("cycling_moderate.jpg")
  running_vigorous <- readJPEG("running_vigorous.jpg")
  
  # plot of summed results for all activity intensities
  output$activity_dots <- renderPlot({
    
    activity_results() %>%
      ggdotchart(
        x = "Ponašanje", y = "Trajanje", label = "Trajanje", label.rectangle = TRUE,
        add = "segment", sorting = "descending", palette = "lancet",
        ylab = "Trajanje (min.)", title = "Vrijeme provedeno u različitim oblicima ponašanja",
        rotate = TRUE, color = "Ponašanje", legend = "none", xlab = "", size = 2, dot.size = 3
        )
      
    })
  
  # Take a screenshot
  observeEvent(input$screen_totalactivity, {
    screenshot(
      selector = "#activity_dots",
      scale = 1.5,
      filename = "ukupno ponašanja"
    )
  })
  
  # plot of summed results for SED
  # output$plot_sed <- renderPlot({
  #   summed_results() %>%
  #     ggplot() +
  #     # background_image(raster.img = sitting_sed) +
  #     geom_hline(aes(yintercept = minutes_SED), colour = 4, linewidth = 3) +
  #     geom_text(aes(0, minutes_SED,
  #                   label = paste("Vaša tjedna količina sedentarnog ponašanja je",
  #                                 minutes_SED, "minuta."), vjust = 2),
  #               size = 6, fontface = "bold") +
  #     # geom_hline(yintercept = 150, color = "red", linewidth = 3) +
  #     # geom_text(aes(0, 150,
  #     #               label = paste("Preporučena tjedna količina sjedilačkog ponašanja je 150 minuta."), 
  #     #             vjust = -2), size = 4,
  #     #           fontface = "bold") +
  #     labs(title = "Vrijeme sjedilačkog ponašanja (min.)",
  #          x = NULL, 
  #          y = NULL) +
  #     theme_minimal() +
  #     theme(axis.title.x = element_blank(),
  #           axis.text.x = element_blank())
  # })
  
  # plot of summed results for LPA
  # output$plot_lpa <- renderPlot({
  #   summed_results() %>%
  #     ggplot() +
  #     # background_image(raster.img = gardening_lpa) +
  #     geom_hline(aes(yintercept = minutes_LPA), colour = 4, linewidth = 3) +
  #     geom_text(aes(0, minutes_LPA,
  #                   label = paste("Vaša tjedna količina lagane tjelesne aktivnosti je",
  #                                 minutes_LPA, "minuta."), vjust = 2),
  #               size = 6, fontface = "bold") +
  #     # geom_hline(yintercept = 150, color = "red", linewidth = 3) +
  #     # geom_text(aes(0, 150,
  #     #               label = paste("Preporučena tjedna količina lagane tjelesne aktivnosti je 150 minuta."), 
  #     #             vjust = -2), size = 4,
  #     #           fontface = "bold") +
  #     labs(title = "Vrijeme lagane tjelesne aktivnosti (min.)",
  #          x = NULL, 
  #          y = NULL) +
  #     theme_minimal() +
  #     theme(axis.title.x = element_blank(),
  #           axis.text.x = element_blank())
  # })

  # # plot of summed results for MVPA
  # output$plot_mvpa <- renderPlot({
  #   summed_results() %>%
  #     ggplot() +
  #     # background_image(raster.img = heart_mvpa) +
  #     geom_hline(aes(yintercept = minutes_MVPA), colour = 4, linewidth = 3) +
  #     geom_text(aes(0, minutes_MVPA,
  #                   label = paste("Vaša tjedna količina umjerene-do-žustre tjelesne aktivnosti je",
  #                                 minutes_MVPA, "minuta."), vjust = 2),
  #               size = 6, fontface = "bold") +
  #     # geom_hline(yintercept = 150, color = "red", linewidth = 3) +
  #     # geom_text(aes(0, 150,
  #     #               label = paste("Preporučena tjedna količina umjerene-do-žustre tjelesne aktivnosti je 150-300 minuta."), 
  #     #               vjust = -2), size = 3.5,
  #     #           fontface = "bold") +
  #     labs(title = "Vrijeme umjerene-do-žustre tjelesne aktivnosti (min.)",
  #          x = NULL, 
  #          y = NULL) +
  #     theme_minimal() +
  #     theme(axis.title.x = element_blank(),
  #           axis.text.x = element_blank())
  # })
  
  # plot of summed results for steps
  output$plot_steps <- renderPlot({
    
    summed_results() %>%
      ggplot() +
      geom_link(aes(x = 1, xend = 1,
                    y = 0, yend = total_steps), 
                size = 17, lineend = "round", color = "#bb353c") +
      geom_link(aes(x = 1, xend = 1,
                    y = 0, yend = total_steps, color = "#bb353c"), 
                size = 16, lineend = "round") +
      geom_label(aes(x = 1, y = 1, 
                     label = paste0("Ukupan broj koraka", ": ", total_steps)), hjust = 0.8) +
      geom_point(aes(x = 1, y = total_steps), size = 9, 
                 shape = 21, fill = "white") +
      geom_point(aes(x = 1, y = total_steps), size = 6, 
                 shape = 21, fill = "white") +
      scale_x_continuous(limits = c(0, 1)) +
      scale_y_continuous(limits = c(0, 100000)) +
      guides(color = "none") +
      coord_polar(theta = "y") + 
      theme_void()

  })
  
  
  # Take a screenshot
  observeEvent(input$screen_totalsteps, {
    screenshot(
      selector = "#plot_steps",
      scale = 1.5,
      filename = "ukupno koraci"
    )
  })
  
  output$plot_daily <- renderPlot({
    daily_plotresults() %>%
      ggplot(aes(x = date, y = value, fill = variable)) +
      ggtitle("Ishodi vezani za aktivnost") + 
      geom_bar(stat = "identity") +
      geom_point(size = 2, color = "black") + 
      geom_line(aes(group = 1), linewidth = 0.7, color = "black") + 
      geom_text(aes(label = ifelse(is.na(value), 
                                   "NA", round(value, 1))), size = 3, 
                vjust = -0.6, color = "black") + 
      labs(x = "Datum", y = "", fill = "") + 
      scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
      theme_bw() + 
      theme(legend.position = "none",
            axis.title.x = element_blank(), 
            axis.text.x = element_text(size = 10, 
                                       angle = 45, hjust = 1, vjust = 1.1), 
            axis.text.y = element_blank(), 
            axis.ticks = element_blank(), 
            plot.background = element_rect(fill = "gainsboro", 
                                           color = "dimgray"), 
            plot.margin = margin(1, 1, 0.5, 1, "cm"), 
            strip.text.x = element_text(size = 10), 
            plot.title = element_text(size = 15, color = "grey30", 
                                      face = "bold")) +
      facet_wrap(. ~ variable, scales = "free_y", ncol = 2) +
      guides(color = "none")
  })
  
  # Take a screenshot
  observeEvent(input$screen_activitydaily, {
    screenshot(
      selector = "#plot_daily",
      scale = 1.5,
      filename = "dnevna količina aktivnosti"
    )
  })
  
  # guidelines for steps plot
  output$plot_dailysteps <- renderPlot({
    
    total_steps <- daily_plotresults() %>%
      filter(variable == "Ukupni koraci")
    
    rect_data <- data.frame(xmin_date = c(total_steps$date[1], total_steps$date[2],
                                          total_steps$date[3], total_steps$date[4],                                                       total_steps$date[5], total_steps$date[6]),
                            xmax_date = c(total_steps$date[2], total_steps$date[3],                                                       total_steps$date[4], total_steps$date[5],                                                       total_steps$date[6], total_steps$date[7]),
                            ymin_steps = 0, ymax_steps = 16000,
                            col = c("red", "green", "blue", "gray", "yellow", "violet"),
                            x = c(1.5, 2.5, 3.5),
                            xend = c(1.5, 2.5, 3.5), y = c(11000, 13000, 7000), yend =                                       c(12000, 15000, 8000), age_group = c("Girls", "Boys",                                                                                "Adults"))
    
    daily_plotresults() %>%
      filter(variable == "Ukupni koraci") %>%
      ggplot() + 
      geom_point(aes(x = date, y = value, color = date), size = 5) + 
      scale_fill_identity() +
      ggalt::geom_xspline(aes(x = as.numeric(date), y = value), linetype = 3, size = 1.5) +
      geom_rect(data = rect_data, 
                aes(xmin = xmin_date, xmax = xmax_date, 
                    ymin = ymin_steps, ymax = ymax_steps, fill = col),
                alpha = 0.2) +
      geom_segment(data = rect_data, 
                   aes(x = x, xend = xend, y = y, yend = yend, color = age_group), 
                   size = 2, lineend = "round") +
      annotate(geom = "text", label = c("Djevojčice", "Dječaci", "Odrasli"), 
               x = c(1.5, 2.5, 3.5), y = c(10500, 12500, 6500), fontface = "bold",
               size = 5) +
      annotate(geom = "text", label = c("11000-12000", "13000-15000", "7000-8000"), 
               x = c(1.5, 2.5, 3.5), y = c(9500, 11500, 5500), fontface = "italic",
               size = 3) +
      annotate("text", label = "*Strelice pokazuju MVPA smjernice", 
               x = 1, y = 15700, hjust = 0, fontface = "italic", 
               size = 3) + 
      labs(x = "", y = "Broj koraka dnevno",
           caption = "Ref: Tudor-Locke C, et al. How many steps/day are enough? for children and adolescents. Int J Behav Nutr Phys Act. 2011;8:78.") +
      theme_minimal() +
      theme(legend.position = "null")
    
  })
  
  # Take a screenshot
  observeEvent(input$screen_stepsdaily, {
    screenshot(
      selector = "#plot_dailysteps",
      scale = 1.5,
      filename = "dnevni broj koraka"
    )
  })
  
  output$distance <- renderText({
    
    most_steps <- daily_plotresults() %>%
      filter(variable == "Ukupni koraci") %>%
      summarise(max_steps = max(value))
    
    distance <- (most_steps[1] * (input$height * 0.414)) / 100000
    
    paste0("Na dan s najviše koraka ukupno bi skupio/la: ", round(x = distance, digits = 2), "km")

    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
