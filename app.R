{
  library(haven)
  library(janitor)
  library(tidyverse)
  library(readxl)
  library(lubridate)
  library(fst)
  library(collapse)
  library(ggplot2)
  library(highcharter)
  library(bs4Dash)
  library(bslib)
  library(tmap)
  library(mapview)
  library(aws.s3)
  library(DBI)
  library(DT)
  library(flextable)
  library(fresh)
  library(glue)
  library(memoise)
  library(openssl)
  library(openxlsx)
  library(plotly)
  library(pool)
  library(rmapshaper)
  library(RPostgres)
  library(scales)
  library(sf)
  library(shinyalert)
  library(shinycssloaders)
  library(shinyFeedback)
  library(shinyFiles)
  library(shinyjs)
  library(shinymanager)
  library(shinyvalidate)
  library(shinyWidgets)
  library(sodium)
  library(sp)
  library(stringr)
  library(uuid)
  library(shadowtext)
  library(ggtext)
  library(data.table)
  library(feather)
  library(shiny)
  library(tmaptools)
  library(raster)
  library(RColorBrewer)
  options(viewer = NULL)
  tmap_mode("view")
  shinyOptions(cache = cachem::cache_disk("./bind-cache"))
  set.seed(123)
  options(scipen = 9999)
}  
{
  BLUE <- "#076fa2"
  RED <- "#E3120B"
  BLACK <- "#202020"
  GREY <- "grey50"  
  {db2 <- 'semea'
    host_db2 <- "semea.choww6kimn1s.eu-north-1.rds.amazonaws.com"
    db_port2 <- '5432'
    db_user2 <- "postgres"
    db_password2 <- "FAR;2030,"
   
    far_pool <- dbPool(RPostgres::Postgres(), dbname = db2, host=host_db2, port=db_port2, user=db_user2, password=db_password2)
    onStop(function() {poolClose(far_pool)})
  }########### DATABASE CONNECTION
}
{
novos <- DBI::dbGetQuery(far_pool, "SELECT * FROM vistas.seleccao WHERE data BETWEEN '2024-01-01' AND CURRENT_DATE")
Geocoded_Households <- read_feather("baseline_households.feather")
meta <- read_feather("metas.feather")
respondentes <- read_feather("respondentes.feather")
beneficiarios <- read_feather("prelistas.feather")
total <- read_feather("total.feather")
old_hh <- read_feather("selecionados.feather")
beneficiarios <- read_feather("prelistas.feather")
postos_administrativos =st_read("posto_2024.shp") %>% st_make_valid() %>% dplyr::select(-Shape_Leng)

Distritos  <- ms_dissolve(postos_administrativos, field = c("ADM2_PT"))
Districts <- st_simplify(Distritos, preserveTopology = TRUE, dTolerance = 100)


agregados <- respondentes



seleccao <- head(old_hh,10) %>% dplyr::bind_rows(novos) %>% mutate(proposta = ifelse(is.na(proposta), "Assistência Integral", proposta))
selected <- seleccao %>% dplyr::filter(!is.na(latitude)) %>%  st_as_sf(coords = c('longitude', 'latitude'), crs = "WGS84")

alcancados <-  old_hh %>% dplyr::bind_rows(novos)
realizados <- alcancados %>% 
  mutate(sexo = dplyr::case_when ((is.na(sexo)) ~ "Homens", 
                                  sexo %in% c("F", "Mulher", "NULL", "Mulheres", "Feminino", "Femenino",  "Não definido") ~ "Mulheres", 
                                  sexo %in% c("M", "Homem",  "Masculino") ~ "Homens", 
                                  TRUE ~ sexo)) %>% 
  mutate(faixa = dplyr::case_when (is.na(ano) ~ "Sem idade", ano < 1989 ~ "Adultos", ano >= 1989 ~ "Jovens", TRUE ~ "Sem idade")) %>% 
  mutate(faixa_genero = paste0(sexo, " ", faixa)) %>% 
  group_by(faixa_genero, posto_id) %>% 
  summarise(Beneficiarios = fndistinct(person_id)) %>% 
  pivot_wider(names_from = "faixa_genero", values_from = "Beneficiarios") %>% 
  adorn_totals("col") %>% 
  dplyr::filter(!is.na(posto_id))

posts <- st_simplify(postos_administrativos, preserveTopology = TRUE, dTolerance = 200) %>% 
  left_join(realizados, by = "posto_id")

Linha_base <- Geocoded_Households %>% dplyr::filter(!is.na(latitude)) %>% st_as_sf(coords = c('longitude', 'latitude'), crs = "WGS84")
Linha_base$tratamento <- as.factor(Linha_base$tratamento)

Breaks <- c(0, 1 ,25, 50, 75, 100, 150, 200, 250, Inf)
Labels <-c("0", "1-25" ,"25-50", "50-75", "75-100", "100-150", "150-200", "200-250","250+")
title <- tags$a(href='https://finance.far.org',tags$img(src="FARFPNEW.png", height = '150', width = '240'), '', target="_blank")
user_image <- "https://img.icons8.com/color/96/000000/circled-user-male-skin-type-6--v1.png"

provincias  <- c('CABO DELGADO', 'GAZA', 'INHAMBANE', 'MANICA', 'MAPUTO', 'MAPUTO CIDADE', 'NAMPULA', 'NIASSA', 'SOFALA', 'TETE', 'ZAMBÉZIA')
# agregados <- read_fst("respondentes.fst")

beneficiarios <- beneficiarios %>% dplyr::filter(!is.na(inquirido))

} ########### LOAD DATASETS
{
two_decimals <- scales::label_comma(accuracy = .2, big.mark = ".", decimal.mark = ",")
zero_decimals <- function (numero){prettyNum(numero, big.mark = ",")}
grafico_barras <- function(data, categoria, valor, meta){
  data <- data %>% mutate(valores := {{valor}}) %>% 
    mutate(categoria := as.character({{categoria}}),
           valor := as.numeric({{valor}}),
           categoria := fct_reorder({{categoria}}, {{valor}}),
           fill := ifelse(valor == max({{valor}}), "Dark", "Light"))
  
  no_y_grid_plot <- ggplot(data, aes(valor, categoria)) +
    geom_col(aes(x = valor, y=categoria, fill  = fill))+
    theme_minimal(base_size = 14)+
    geom_text(
      data = data,
      mapping = aes(x = valor, y = categoria, label = valor),
      hjust = 1,
      nudge_x = -0.1,
      color = 'white',
      fontface = 'bold',
      size = 4.5
    ) +
    scale_fill_manual(values = c('#008000', '#A2AD9C'))+
    geom_text(
      data = data,
      mapping = aes(x = 0, y = categoria, label = categoria),
      hjust = 0,
      nudge_x = 0.25,
      color = 'white',
      fontface = 'bold',
      size = 4.5
    )+
    # geom_vline(xintercept=40,col = "red", lty=2) +
    # geom_vline(xintercept = 0) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0, 0.01))) +
    scale_y_discrete(breaks = NULL) +
    labs(x = element_blank(), y = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), legend.position = "none")
  no_y_grid_plot
}

box_title_js <- '
  Shiny.addCustomMessageHandler("box_title", function(title) {
  if(title.includes("mpg")){
    colour = "red"
  } else {
    colour = "blue"
  }
    $("#box_plot h3").html(title)
    $("#box_plot .card-body").css("background-color", colour)
  });
'


initComplete = JS(
  "function(settings, json) {",
  "$(this.api().table().header()).css({'background-color': '#228B22', 'color': '#fff'});",
  "$(this.api().table().body()).css({'font-weight': 'normal'});",
  "}"
) 
}########### FUNCTIONS
{
N <- 20

x <- cumsum(rnorm(N)) + 0.5 * cumsum(runif(N))
x <- round(200*x)

df <- data.frame(x = sort(as.Date(Sys.time() - lubridate::days(1:N))), y = abs(x))

hc_theme_sparkline_vb <- function(...) {
 theme <- list(chart = list( backgroundColor = NULL,margins = c(0, 0, 0, 0),spacingTop = 0,spacingRight = 0, spacingBottom = 0, spacingLeft = 0, plotBorderWidth = 0, borderWidth = 0, style = list(overflow = "visible")),
    xAxis = list(visible = FALSE, endOnTick = FALSE, startOnTick = FALSE),
    yAxis = list(visible = FALSE, endOnTick = FALSE,  startOnTick = FALSE),
    tooltip = list(outside = FALSE, shadow = FALSE, borderColor = "transparent", botderWidth = 0, backgroundColor = "transparent", style = list(textOutline = "5px white")),
    plotOptions = list(series = list(marker = list(enabled = FALSE),
        lineWidth = 2, shadow = FALSE, fillOpacity = 0.25, color = "#FFFFFFBF",
        fillColor = list(linearGradient = list(x1 = 0, y1 = 1, x2 = 0, y2 = 0), stops = list(list(0.00, "#FFFFFF00"), list(0.50, "#FFFFFF7F"), list(1.00, "#FFFFFFFF"))))),
    credits = list(enabled = FALSE, text = ""))
  
  theme <- structure(theme, class = "hc_theme")
  if (length(list(...)) > 0) {theme <- hc_theme_merge(theme, hc_theme(...))}
  theme
}

bullet_chart <- function(respondentes){
  realizado <- respondentes %>% group_by(categoria = distrito) %>% summarize(realizado = fndistinct(codigo))
  targeted <- meta %>% group_by(categoria = distrito)  %>% summarize(meta = fsum(as.numeric(metas)))
  realizacoes <- head(realizado,15) %>% left_join(targeted, by = "categoria")
  
  realizacoes %>%  mutate(categoria := fct_reorder(categoria, realizado)) %>% 
    ggplot(aes(x = realizado, y = categoria)) +
    geom_col(aes(x = meta), fill = "gray") +
    geom_col(width = 0.6, fill = "#008000") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())+
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(), axis.text.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_text(size=15),
          plot.title = element_markdown(lineheight = 1.1),
          legend.text = element_markdown(size = 15))+
          # size = rel(1))+
    coord_cartesian(ylim = c(10, max(realizacoes$meta)+10)) +
    coord_cartesian(xlim = c(10,  max(realizacoes$meta))) +
    geom_shadowtext(
      data = subset(realizacoes, meta < 8),
      aes(meta, y = categoria, label = categoria),
      hjust = 0, nudge_x = 0.3, colour = BLUE,
      bg.colour = "white", bg.r = 0.2)+
    # geom_text(data = subset(realizacoes, meta >= 8), aes(0, y = categoria, label = categoria), nudge_x = 1.0, colour = "white", size = 7)+
    geom_text(data = realizacoes, mapping = aes(x = realizado, y = categoria, label = realizado), hjust = 1.5, nudge_x = -0.2, color = 'lightgreen', fontface = 'bold', size = 7)+
    geom_text(data = realizacoes, mapping = aes(x = meta, y = categoria, label = meta), hjust = 1.5, nudge_x = -0.2, color = 'white', fontface = 'bold', size =7)
}  ########### CHART FUNCTION

bullet_chart4 <- function(respondentes){
  total <- total %>% mutate(label_meta = ifelse(meta*0.95 <= realizado, NA, meta))
  
  total %>%  mutate(categoria := fct_reorder(categoria, realizado)) %>% 
    mutate(meta := ifelse(situacao == 'Alcance', 0, meta)) %>% 
    ggplot(aes(x = realizado, y = categoria)) +
    geom_col(aes(x = meta), fill = "gray") +
    geom_col(width = 0.6, fill = "#008000") +
    theme_minimal() +
    theme(panel.grid.major.y = element_blank())+
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title.x=element_blank(), axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          plot.title = element_markdown(lineheight = 1.1),
          legend.text = element_markdown(size = 15))+
    labs(
      title = " <span style='font-size:18pt'> **Inquérito de Avaliação de Resultados (COI): Estudo de Base do PROCAVA** <br> </span>
    <span style='font-size:18pt'> Agregados familiares
    <span style='color:#008000;'>entrevistados </span>em comparação com os
    <span style='color:#708090;'>planificados</span>
    </span>",
      caption = "Fonte: FAR-FP | PROCAVA")+
    coord_cartesian(ylim = c(10, max(total$meta)+10)) +
    coord_cartesian(xlim = c(10,  max(total$meta))) +
    geom_shadowtext(
      data = subset(total, meta < 8),
      aes(meta, y = categoria, label = categoria),
      hjust = 0, nudge_x = 0.3, colour = BLUE,
      bg.colour = "white", bg.r = 0.2)+
    geom_text(data = subset(total, meta >= 8), aes(0, y = categoria, label = categoria), hjust = -0.4, nudge_x = 0.0, colour = "white", size = 7)+
    geom_text(data = total, mapping = aes(x = realizado, y = categoria, label = realizado), hjust = 1.5, nudge_x = -0.2, color = 'white', fontface = 'bold', size = 7)+
    geom_text(data = total, mapping = aes(x = meta, y = categoria, label = label_meta), hjust = 1.5, nudge_x = -0.2, color = 'white', fontface = 'bold', size =7)
}  ########### CHART FUNCTION

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, icon = NULL, color = "aqua", width = 3, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
    ),
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    if (!is.null(icon)) div(class = "icon-sm icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

hc <- hchart(df, "area", hcaes(x, y), name = "Participantes")  %>% hc_size(height = 20) %>% hc_credits(enabled = TRUE) %>% hc_add_theme(hc_theme_sparkline_vb()) 
hc2 <- hchart(df, "line", hcaes(x, y), name = "Beneficiários")  %>% hc_size(height = 20) %>% hc_credits(enabled = FALSE) %>% hc_add_theme(hc_theme_sparkline_vb()) 
hc3 <- hchart(df, "column", hcaes(x, y), name = "Agregados familiares")  %>% hc_size(height = 20) %>% hc_credits(enabled = FALSE) %>% hc_add_theme(hc_theme_sparkline_vb()) 


} ############ VALUE BOX FUNCTIONS

ui <- dashboardPage(
  dashboardHeader(title =  title, rightUi = userOutput("user_names")),
  dashboardSidebar(disable = FALSE, minified = F, collapsed = TRUE, sidebarMenu(
    menuItem("ASSISTÊNCIA", startExpanded = FALSE, icon = icon("motorcycle"),
             menuSubItem("Famílias", tabName = "farfp_outreach", icon = icon("arrows-down-to-people")),
             menuSubItem("Grupos", tabName = "op_msme", icon = icon("users-between-lines"))
             ),
    menuItem("INVESTIMENTOS", startExpanded = FALSE, icon = icon("industry"),
             menuSubItem("Infraestruturas", tabName = "technicians_trainings", icon = icon("warehouse")),
             menuSubItem("Equipamentos", tabName = "beneficiaries_trainings", icon = icon("tractor"))
                                 ))),
  dashboardBody(tags$head(tags$style("#test .modal-dialog {width: fit-content !important;}")),

    tabItems(
      tabItem("farfp_outreach", 
              fluidRow(
                valueBoxOutput("vbox", width = 2),
                valueBoxOutput("vbox2", width = 2),
                valueBoxOutput("vbox3", width = 2),
                valueBoxOutput("vbox4", width = 2),
                valueBoxOutput("vbox5", width = 2),
                valueBoxOutput("vbox6", width = 2)
                ),
              
              fluidRow(
                box(title = "AGREGADOS FAMILIARES ENTREVISTADOS", closable = TRUE, maximizable = TRUE, width = 4, 
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    plotOutput("desempenho_comparativo", height=700)),

                box(title = "INQUÉRITOS DIÁRIOS POR INQUIRIDOR", closable = TRUE, maximizable = TRUE, width = 4, 
                    height = "720px",  solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    plotOutput("desempenho_pessoal", height=700)),
                
                box(title = "MAPA", closable = TRUE, maximizable = TRUE, width = 4, 
                    height = "720px",  header = FALSE, solidHeader = FALSE,  background = NULL, status = "success",  collapsible = TRUE,
                    tmapOutput("map", height="100%", width="100%"))
                 )
              )
  
  )),
  
  footer = bs4DashFooter(left = a(href = "www.farfp.com", target = "_blank", "Direitos reservados ao FAR-FP"), right = paste0("Actualização: ", format(Sys.Date(), "%b"), " ", format(Sys.Date(), "%Y"))),
  controlbar = dashboardControlbar(id = "controlbar", collapsed = TRUE, overlay = TRUE,
                                   selectizeInput("selected_province", label = h5("Províncias"), c("Todas", provincias), selected = "Todas"),
                                   selectizeInput("selected_intervention", "Intervenções", c("Estudo de base" = "baseline", "Selecção  de beneficiarios" = "targeting", "Assistência" = "assistence", "Insumos" = "inputs", "Equipment" = "Equipamento")),
                                   selectizeInput("intervencao_selecionada", "Intervenções", choices = c("Selecionar"), selected = 'Todas')
                                   )
)

server <- function(input, output, session) {
  
  output$user_names <- renderUser({
    dashboardUser(
      name = "PROCAVA",
      image = user_image, 
      title = "Oficial de M&A",
      subtitle = NULL,
      footer = NULL
    )
  })

  respondentes <- reactive({
    if(input$selected_province != "Todas"){beneficiarios <- fsubset(beneficiarios, provincia == input$selected_province)}
      beneficiarios
  })
  
  
  entrevistados <- reactive({
    if(input$selected_province != "Todas"){agregados <- fsubset(agregados, provincia == input$selected_province)}
    agregados
  })

  output$vbox <- renderValueBox({
     agregados_familiares <- as.data.frame(entrevistados())

    vb <- valueBoxSpark(
      value = fndistinct(agregados_familiares$codigo),
      title = toupper("Respondentes"), sparkobj = hc, subtitle = tagList(HTML("&uarr;"), "Zero (0) nas últimas 24 horas"),
      info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
      icon = icon("users"), width = 2, color = "teal", href = NULL)
    vb})
  
  output$vbox2 <- renderValueBox({
    agregados_familiares <- as.data.frame(entrevistados()) %>% fsubset(sexo %in% c('F', 'Mulheres') & member_id == 1)
    vb <- valueBoxSpark(
      value =  fndistinct(agregados_familiares$codigo),
      title = toupper("Mulheres"), sparkobj = hc2,
      subtitle = tagList(HTML("&uarr;"), "Zero (0) nas últimas 24 horas"),
      info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
      icon = icon("venus"), width = 2, color = "red", href = NULL)
    vb})
  
  output$vbox3 <- renderValueBox({
    agregados_familiares <- as.data.frame(entrevistados()) %>% fsubset(faixa %in% c('Jovens', 'Jovem') & member_id == 1)
    
    vb3 <- valueBoxSpark(
      value = fndistinct(agregados_familiares$codigo),
                         title = toupper("Jovens inquiridos"), sparkobj = hc3,
                         subtitle = tagList(HTML("&darr;"), "Zero (0) nas últimas 24 horas"),
                         info = "Corresponde ao número de agregados familiares assistidos que são representados por mulheres",
                         icon = icon("children"), width = 2, color = "yellow", href = NULL)
    vb3})
  
  output$vbox4 <- renderValueBox({
    agregados_familiares <- as.data.frame(entrevistados()) %>% fsubset(!is.na(duracao_minutos))
    
    vb4 <- valueBoxSpark(
      value =  round(mean(agregados_familiares$duracao_minutos)/60,2),
      title = toupper("Horas por Inquérito"), sparkobj = hc3,
      subtitle = tagList(HTML("&uarr;"), "Zero (0) nas últimas 24 horas"),
      info = "This is the lines of code I've written in the past 20 days! That's a lot, right?",
      icon = icon("wheelchair"), width = 2, color = "yellow", href = NULL)
    
    vb4})
  
  output$vbox5 <- renderValueBox({
    agregados_familiares <- as.data.frame(entrevistados()) %>% fsubset(mulher_fertil == 'SIM')
    
    vb5 <- valueBoxSpark(
      value = fndistinct(agregados_familiares$codigo),
                         title = toupper("Mulheres em Idade Fértil"),
                         sparkobj = hc3,
                         subtitle = tagList(HTML("&uarr;"), "Zero (0) nas últimas 24 horas"),
                         info =     tags$span(tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "message")),
                         icon = icon("person-drowning"),
                         width = 2,
                         color = "yellow",
                         href = NULL)
  vb5})
  
  output$vbox6 <- renderValueBox({
    agregados_familiares <- as.data.frame(entrevistados()) %>% fsubset(antropometria == "SIM")

    vb6 <- valueBoxSpark(
      value = fndistinct(agregados_familiares$codigo),
      title = toupper("Famílias com menores de 5 anos"),
      sparkobj = hc3,
      subtitle = tagList(HTML("&uarr;"), "Zero (0) nas últimas 24 horas"),
      info = tags$span("Fixed ratio", tags$i(class = "glyphicon glyphicon-info-sign", style = "color:#0072B2;", title = "message")),
      icon = icon("map"),
      width = 2, color = "yellow", href = NULL)

    vb6})

  
  output$map <- renderTmap({
  # 
   tm <-  tm_basemap(server = "OpenStreetMap") +
   tm_shape(posts, is.master = FALSE) + tm_borders(alpha = 0)+
     tm_polygons("Total",
                   id = toupper("ADM2_PT"),
                   palette = "BrBG", n = 9, contrast = c(0.3, 1),
                   breaks = Breaks,
                   borders = NULL,
                  colorNA = NULL,
                   legend.is.portrait = FALSE,
                   labels = Labels,
                   popup.vars=c("Distrito:"="ADM2_PT", 
                                "Posto Administrativo:"="ADM3_PT", 
                                "AF Assistidos:"="Total",
                                "Homens adultos:"="Homens Adultos", 
                                "Homens jovens:"="Homens Jovens",
                               "Mulheres adultas:"="Mulheres Adultos",
                                "Mulheres jovens:"="Mulheres Jovens"),
                   style = "fixed", 
                   title = "AF alcançados", 
                   alpha = 0.5,
                   legend.stack = "horizontal")+
       tm_shape(Linha_base) +
       
       tm_dots("Linha de Base", col="tratamento", size = c(.001), scale = 5, 
               shape ="tratamento",
               popup.vars = c("Distrito:" = "distrito",
                              "Tipologia:" = "tratamento",
                              "Membros do AF:" = "hh_members", 
                              "Sexo:" = "sexo",
                              "Idade:" = "idade",
                              "Código do AF:" = "codigo"),
               palette=c("red", "green"))+
      
      tm_symbols(size = "hh_members", title.size = "Membros do AF:",
                  shape = "tratamento", title.shape = "Tipo:")+
       
       tm_shape(selected) +
       tm_dots(
         group = "sexo",
         col = "papel",
         palette = "Reds",
         popup.vars = c("Distrito:" = "distrito", "Nome:" = "nome", "Sexo:" = "sexo", "Nascimento:" = "ano", "Localidade:" = "localidade", "Povoado:" = "povoado", "Papel:" = "papel", "Propostas:" = "proposta")
       ) +
       ############ Classify these bubles into treatment and control
       tm_view(set.view = c(lon = 37.33, lat = -16.88, zoom = 9))+
       qtm(Districts, 
           fill = NULL,
          text = "ADM2_PT",
          text.size = 1.7,
           group = NULL,
           text.col = "#A9A9A9",
           fillCol = "ADM2_PT",
           borders = "#C0C0C0", scale = 1, polygons.id = "ADM2_PT")
     
   })
  
  
  output$desempenho_comparativo <- renderPlot({
    bullet_chart(entrevistados())
  })
  
  output$desempenho_pessoal <- renderPlot({
    rever <- entrevistados() %>% dplyr::filter(!is.na(nome_inq)) %>% group_by(nome_inq) %>%
      summarise(realizados = round(fndistinct(codigo)/fndistinct(data_entrevista),2)) %>% 
      arrange(-realizados) %>% 
      head(10) %>% 
      mutate(
        color = case_when(
          row_number() == 1 ~ "#008000",
          row_number() == 2 ~ "#32CD32",
          row_number() == 3 ~ "#7CFC00",
          row_number() >= 9 ~ "#FFC300",
          TRUE ~ "gray70"))
    
    rever %>%  mutate(nome_inq = fct_reorder(nome_inq, realizados)) %>% 
      ggplot(aes(x = nome_inq, y = realizados, fill = color)) +
      geom_col() +
      coord_flip()+
      geom_text(aes(label = realizados),  size = 8, hjust = 1.5,  vjust = 0.5) +
      scale_fill_identity(guide = "none") +
      theme_void()+
      theme(axis.text.y = element_text(size = 15, angle = 0, vjust=0, hjust=0))
  })

}

shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
