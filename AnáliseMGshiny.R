library(shiny)
library(tidyverse)
library(dplyr)
library(sf)
library(geobr)
library(readxl)
library(tmap)
library(irr)
library(plotly)
library(shinydashboard)

#LENDO OS SHAPES DE MG, DADOS DO CAMS E VON DONKELAR 
cams = read.csv("PM2.5_diario_2023.csv.opdownload")
von_donkelar = read_excel("dados_completos_consolidado_donkelar.xlsx")

mg_cams = cams |> 
  mutate(data = ymd(Date),
         mes = month(data),
         Cod = as.character(Cod)) |> 
  filter(str_starts(Cod,'31')) |> 
  select(Cod,data,mes,PM2.5)


mg_von = von_donkelar |> 
  filter(SIGLA_UF == 31) |> 
  mutate(CD_MUN = as.character(CD_MUN))

shp=read_sf("MG_Municipios_2024.shp")

mg_von_mensal = mg_von |> 
  mutate(mes = factor(Mes, 
                      levels = 1:12, 
                      labels = c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun", 
                                 "Jul", "Ago", "Set", "Out", "Nov", "Dez")))|>
  group_by(mes) |> 
  summarise(VON = mean(Media_PM25))


mg_cams_mensal = mg_cams |> 
  mutate(mes = factor(mes, 
                      levels = 1:12, 
                      labels = c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun", 
                                 "Jul", "Ago", "Set", "Out", "Nov", "Dez")))|>
  group_by(mes) |> 
  summarise(CAMS = mean(PM2.5))


mg_mensal1 = mg_von_mensal |> 
  left_join(mg_cams_mensal,by = "mes")

mg_mensal = mg_mensal1 |> 
  pivot_longer(cols = c(VON,CAMS),
               names_to = "pm",
               values_to = "valor")

# tabelas pros gráficos de dispersão

mg_cams_medio = mg_cams %>% 
  group_by(Cod,mes) %>% 
  summarize(pm2.5_mensal = mean(PM2.5))

mg_total = mg_von |> 
  left_join(mg_cams_medio,join_by("CD_MUN" == "Cod","Mes" == "mes"))

mg_total = mg_total |> 
  mutate(diferenca = Media_PM25 -pm2.5_mensal,
         media = (Media_PM25 + pm2.5_mensal)/2)

mg_disp = mg_cams_mensal %>% left_join(mg_von_mensal,join_by(mes)) %>% 
  mutate(diferenca = VON - CAMS,
         media = (VON+CAMS)/2)


# tabela pro gráfico boxplot
mg_longo <- mg_total |> 
  pivot_longer(cols = c(Media_PM25,pm2.5_mensal), 
               names_to = "pm", 
               values_to = "valor") |> 
  mutate(mes = factor(Mes, 
                      levels = 1:12, 
                      labels = c("Jan", "Fev", "Mar", "Abril", "Mai", "Jun", 
                                 "Jul", "Ago", "Set", "Out", "Nov", "Dez")),
         PM = case_when(pm %in% c("Media_PM25") ~ "VON",
                        pm %in% c("pm2.5_mensal") ~ "CAMS"))
# tabela pros mapas

cams_cod = mg_cams |> 
  group_by(Cod) |> 
  summarise(pm2.5_cod_cams = mean(PM2.5))

von_cod = mg_von |> 
  group_by(CD_MUN) |> 
  summarise(pm2.5_cod_von = mean(Media_PM25))


cams_shape = left_join(x = shp,
                       y = cams_cod,
                       join_by("CD_MUN" == "Cod"))

von_shape = left_join(x = shp,
                      y = von_cod,
                      join_by("CD_MUN" == "CD_MUN"))

# Define UI for application that draws a histogram
ui <- dashboardPage(
  header<-dashboardHeader(title = "PM2.5 em Minas Gerais"),
  sidebar<-dashboardSidebar(
    sidebarMenu(
      menuItem("Análise Mensal", icon = icon("bar-chart"),
               menuSubItem("Gráfico de Barras", tabName = "subitem1"),
               menuSubItem("Série", tabName = "subitem2"),
               menuSubItem("Boxplot", tabName = "subitem3")),
      menuItem("Comparando os Satélites", icon = icon("bar-chart"),
               menuSubItem("Comparação 1", tabName = "subitem4"),  # Juntamos os pares
               menuSubItem("Comparação 2", tabName = "subitem5")),  # em abas separadas
      menuItem("Mapas", icon = icon("map"),
               menuSubItem("Comparação de Mapas", tabName = 'subitem6'))
      )
    
    ),
  body<-dashboardBody(
    tabItems(
      tabItem("subitem1",
              plotOutput("barras")),
      tabItem("subitem2", plotlyOutput("serie")),
      tabItem("subitem3", plotlyOutput("boxplot")),
      tabItem("subitem4",
              fluidRow(
                box(width = 6, plotOutput("dispersao1")),
                box(width = 6, plotOutput("blandaltman1"))
              ),
              fluidRow(
                box(width = 12,
                    h4("Análise Comparativa 1"),
                    p("O gráfico à esquerda mostra a relação de dispersão entre as medições dos dois satélites, sendo a dispersão por cada ponto uma observação mensal de um município"),
                    p("O gráfico à direita apresenta a análise Bland-Altman, que avalia a concordância entre as medições, com as linhas tracejadas mostrando os limites de concordância de 95%."),
                    p(strong("Análise de Concordância (ICC):")),
                    p(paste0("ICC(C,1) = ", ValorICC1, " indica uma confiabilidade ", 
                             ifelse(ValorICC1 > 0.6, "excelente", 
                                    ifelse(ValorICC1 > 0.4, "boa", "fraca")), ".")),
                    p(paste0("Intervalo de Confiança 95%: [", IC95_ICC1[1], ", ", IC95_ICC1[2], "]")),
                    p("O teste F rejeita a hipótese nula (ICC = 0), indicando que há concordância significativa entre os métodos."),
                    style = "margin-top: 20px; border-top: 2px solid #eee; padding-top: 15px;"
                )
              )
      ),
      tabItem("subitem5",
              fluidRow(
                box(width = 6, plotOutput("dispersao2")),
                box(width = 6, plotOutput("blandaltman2"))
              ),
              fluidRow(
                box(width = 12,
                    h4("Análise Comparativa 2"),
                    p("O gráfico de dispersão (esquerda) compara as medições dos dois métodos."),
                    p("O gráfico Bland-Altman (direita) mostra as diferenças entre os métodos."),
                    p(strong("Análise de Concordância (ICC):")),
                    p(paste0("ICC(C,1) = ", ValorICC2, " indica uma confiabilidade ", 
                             ifelse(ValorICC2 > 0.6, "excelente", 
                                    ifelse(ValorICC2 > 0.4, "boa", "fraca")), ".")),
                    p(paste0("Intervalo de Confiança 95%: [", IC95_ICC2[1], ", ", IC95_ICC2[2], "]")),
                    p(" Valor p = 0.341 (não significativo, p > 0.05), ou seja, não há evidências para rejeitar a hipótese nula (ICC = 0)"),
                    p("O tamanho amostral (n = 12) é pequeno, o que pode limitar a precisão"),
                    style = "margin-top: 20px; border-top: 2px solid #eee; padding-top: 15px;"
                    )
                )
              ),
      tabItem("subitem6", 
              fluidRow(
                box(title = "Mapa CAMS", width = 6, plotOutput("mapaCAMS")),
                box(title = "Mapa Von-Donkelar", width = 6, plotOutput("mapaVON"))
              ))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$barras <- renderPlot({
      
      ggplot(mg_mensal,aes(x = mes, y = valor,fill = pm)) +
        geom_bar(stat= 'identity',position = 'dodge') +
        labs(title = "Média de PM 2.5 por mês e fonte",x="Mês",y="PM 2.5 Médio",fill = "Satélite") +
        scale_fill_manual(values = c("#56B4E9", "#E69F00")) +
        theme_minimal()
    })
    
    output$serie = renderPlotly({
      
      ggplot(mg_mensal,aes(x = mes, y = valor, color = pm, group = pm)) +
        geom_line(linewidth = 1.2) +
        geom_point(size=2)+
        labs(title = "Média de PM 2.5 por mês ",x="Mês",y="PM 2.5 Médio",color = "Satélites") +
        scale_color_manual(values = c("#56B4E9","#E69F00")) +
        theme_minimal()
      
    })
    
    output$boxplot = renderPlotly({
      # boxplot
      ggplot(mg_longo,aes(x = mes,  y = valor, fill = PM)) +
        geom_boxplot() +
        labs(title = "Distribuição da Média de PM 2.5 por mês",x="Mês",y="PM 2.5 Médio", fill = "Satélites") +
        scale_fill_manual(values = c("#56B4E9","#E69F00")) +
        theme_minimal()
    })
    
    output$dispersao1 = renderPlot({
      ggplot(mg_total, aes(x = pm2.5_mensal, y = Media_PM25)) +
        geom_point() +
        labs(x = "PM2.5 CAMS", y = "PM2.5 VON", title = "Dispersão entre os PM2.5") +
        theme_minimal()
    })
    
    output$blandaltman1 = renderPlot({
      media_diff <- mean(mg_total$diferenca)
      dp_diff <- sd(mg_total$diferenca)
      limite_superior <- media_diff + 1.96 * dp_diff
      limite_inferior <- media_diff - 1.96 * dp_diff
      
      ggplot(mg_total, aes(x = media, y = diferenca)) +
        geom_point() +
        geom_hline(yintercept = media_diff, color = "blue") +
        geom_hline(yintercept = limite_superior, color = "red", linetype = "dashed") +
        geom_hline(yintercept = limite_inferior, color = "red", linetype = "dashed") +
        labs(y = "Diferenças (VON - CAMS)", x = "Médias", title = "Bland-Altman") +
        theme_minimal()
    })
    
    output$dispersao2 = renderPlot({
      ggplot(mg_disp, aes(x = CAMS, y = VON)) +
        geom_point() +
        labs(x = "PM2.5 CAMS", y = "PM2.5 VON", title = "Dispersão entre os PM2.5") +
        theme_minimal()
    })
    
    output$blandaltman2 = renderPlot({
      media_diff = mean(mg_disp$diferenca)
      dp_diff = sd(mg_disp$diferenca)
      limite_superior = media_diff + 1.96 * dp_diff
      limite_inferior = media_diff - 1.96 * dp_diff
      
      ggplot(mg_disp, aes(x = media, y =diferenca)) +
        geom_point() +
        geom_hline(yintercept = media_diff, color = "blue") +
        geom_hline(yintercept = limite_superior, color = "red", linetype = "dashed") +
        geom_hline(yintercept = limite_inferior, color = "red", linetype = "dashed") +
        labs(y = "Diferenças (VON - CAMS)", x = "Médias", title = "Bland-Altman") +
        theme_minimal()
    })
    
    output$mapaCAMS = renderPlot({
      ggplot(cams_shape) +
        geom_sf(aes(fill=pm2.5_cod_cams)) +
        scale_fill_gradient(name = "PM2.5 μg/m3",
                            low = "#66bb6a", high = "#ef5350",
                            limits = c(0, max(cams_shape$pm2.5_cod_cams))) +
        labs(title = 'Distribuição de PM2.5 médio do CAMS por município',
             caption = 'Fontes: Copernicus Atmosphere Monitoring Service \nDon Vonkelar')+
        theme_minimal()
      
    })
    
    output$mapaVON = renderPlot({
      ggplot(von_shape) +
        geom_sf(aes(fill=pm2.5_cod_von)) +
        scale_fill_gradient(name = "PM2.5 μg/m3",
                            low = "#66bb6a", high = "#ef5350",
                            limits = c(min(von_shape$pm2.5_cod_von), max(von_shape$pm2.5_cod_von))) +
        labs(title = 'Distribuição de PM2.5 médio do Von por município',
             caption = 'Fontes: Copernicus Atmosphere Monitoring Service \nDon Vonkelar')+
        theme_minimal()
    })
    
    # Cálculo do ICC (coloque no início do server)
    avaliacoes = reactive({
      mg_total[, c("pm2.5_mensal", "Media_PM25")]
    })
    
    icc1 = reactive({
      icc(avaliacoes(), model = "twoway", type = "consistency", unit = "single")
    })
    
    output$icc_text <- renderText({
      icc_val <- round(icc1()$value, 2)
      icc_ci <- paste0("[", round(icc1()$lbound, 3), ", ", round(icc1()$ubound, 3), "]")
      
      paste("ICC(C,1) =", icc_val, "indica uma confiabilidade",
            ifelse(icc_val > 0.6, "excelente", ifelse(icc_val > 0.4, "boa", "fraca")),
            "\nIntervalo de Confiança 95%:", icc_ci,
            "\nO teste F rejeita a hipótese nula (ICC = 0), indicando concordância significativa.")
    })
    
    # Cálculo do ICC2
    avaliacoes2 = reactive({
      mg_mensal1[, c("CAMS", "VON")]
    })
    
    icc2 = reactive({
      icc(avaliacoes2(), model = "twoway", type = "consistency", unit = "single")
    })
    
    # Valores para exibição
    output$icc2_text <- renderText({
      icc_val <- round(icc2()$value, 3)
      icc_ci <- paste0("[", round(icc2()$lbound, 3), ", ", round(icc2()$ubound, 3), "]")
      p_val <- round(icc2()$p.value, 3)
      
      paste(
        "ICC(C,1) =", icc_val, "\n",
        "Intervalo de Confiança 95%:", icc_ci, "\n",
        "Valor p =", p_val, ifelse(p_val > 0.05, "(não significativo)", "(significativo)"), "\n",
        "Tamanho amostral: n =", nrow(avaliacoes2())
      )
    })
}




# Run the application 
shinyApp(ui = ui, server = server)
