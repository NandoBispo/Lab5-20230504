# PACOTES ----

if (!require(pacman)) install.packages("pacman")

pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools,
               kableExtra, moments, ggpubr, formattable, gridExtra, 
               glue, corrplot, sessioninfo, readxl, writexl, ggthemes,
               patchwork,  plotly, lmtest, olsrr, gglm, ggplot2,
               tidymodels, GGally, skimr, qqplotr, performance)

## Identificação dos pacotes ----
# - tidyverse
# - janitor: Pacote para arrumação do conjunto dos dados e padronizar os nomes das variáveis com o comando "janitor::clean_names()"
# - skimr: Pacote que gera um mini relatório dos dados e identifica dados faltantes como comando "skimr::skim()".
# - qqplotr: Pacote que gera gráficos qq
# performance::check_model gera gráficos de análise de residuos para MRLS

# VERSIONAMENTO ----
# https://curso-r.githud.io/zen-do-r/git-githud.html
# gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()
# _________________________________________________

# Parte 1 ----
## DADOS 1 ----
### Importação ----
dados1 <- datasets::trees

dados1 <- dados1|>
  janitor::clean_names()

dados1 <- dados1|>
  mutate(
    height = height*0.3048,
    volume = volume*0.02831685,
    girth = girth*0.3048
  )

### Medidas Resumo ----
summarytools::st_options(lang = "pt")

# options(knitr.table.format = "latex")

dados1|>
  rename(
    "Altura" = height,
    "Volume" = volume,
    "Circunferência" = girth
  )|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv", "Skewness", "Kurtosis"),
    justify = "c",
    style = "rmarkdown",
    transpose = T
  )
  kbl(
    caption = "Medidas Resumo dos dados",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T, 
    booktabs = T
    # col.names =
    #   c("Min", "Q1", "Med", "Média", "Q3", "Max", "D.Padrão", "CV")
  )|>
  footnote(general = "Fonte: StatLib - Carnegie Mellon University") |>
  # kable_material(c(
  #   "striped", # listrado
  #   "hover", 
  #   "condensed"))|>
  kable_styling(
    full_width = F,
    position = 'center', 
    # latex_options = 'HOLD_position',
    latex_options = c("striped", "HOLD_position", "scale_down")
  )|>
  kable_material()



### Correlação ----
cor.test(dados1$volume, dados1$height)


### Ajuste do Modelo + Gráfico ----
dados1|>
  ggplot(aes(x = height, y = volume)) +
  geom_point(
    color = "#234B6E"
    )+
  labs(
    title = "Modelo Ajustado entre o Volume e Altura",
    y = 'Volume (m³)',
    x = 'Altura (m)'
  )+
  ggpubr::stat_cor(
    aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "~`; `~")),
    cor.coef.name = c("R"),
    label.sep = "; ", geom = "text",
    color="red",
    method = "pearson",
    label.x = 63, label.y = 70, show.legend = F,
    p.accuracy = 0.001, r.accuracy = 0.0001,
    size = 3)+
  ggpubr::stat_regline_equation(
      aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~`; `~")),
          geom = "text", label.x = 63, label.y = 60, 
      position = "identity", 
      color="red",
      size = 3, show.legend = F
  )+
  geom_smooth(
    method=lm, se=T, formula = "y ~ x", color = "tomato")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(60,90,5))+
  scale_y_continuous(
    breaks = seq(0, 85, 20),
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
    theme(legend.position = "none",
          axis.line = element_line(size = 0.8, color = "#222222"))
          # axis.line = element_line(size = 0.8, color = "tomato"))|>
  # ggpubr::ggpar(palette = "grey", font.x = "bold") #não funciona
  
  
(mFit <- lm(volume~height, data = dados1))
  
### Resíduos ----
#### Gráficos RBase ----
  par(mfrow = c(2, 2))
  
  plot(mFit)
  
  par(mfrow = c(1, 1))
  
  # _____________________________________________
#### Gráficos GGplot2 ----
  mFit_resid <- broom::augment(mFit)
  dplyr::glimpse(mFit_resid)
  
  # Gráfico de Resíduos contra Valor Médio
  mFit_resid|>
    ggplot(aes(x = .fitted, y = .resid)) + 
    geom_point(color = "#234B6E") +
    geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
    geom_smooth(
      se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
    labs(
      x = "Valores Médios Ajustados",
      y = "Resíduos Ordinários",
      title = "Gráfico de Resíduos vs. Valores Ajustados"
    )+
    # scale_x_continuous(breaks = seq(0,30,5))+
    theme_minimal(base_size = 7.5)+
    theme(legend.position = "none",
          axis.line = element_line(size = 0.8, color = "#222222"))
  
  ## Gráfico de normalidade dos resíduos
  mFit_resid %>% 
    ggplot(aes(sample = .std.resid)) + 
    qqplotr::stat_qq_band(alpha = 0.3) + # Plota a banda de confiança
    qqplotr::stat_qq_point(color = "#234B6E") + # Plota os pontos
    qqplotr::stat_qq_line(linetype = 2, size = 0.2) + # Plota a reta
    labs(
      x = "Quantil Teórico",
      y = "Quantil Amostral",
      title = "Gráfico quantil-quantil normal"
    )+
    # scale_x_continuous(breaks = seq(-3,3,1))+
    theme_minimal(base_size = 7.5)+
    theme(legend.position = "none",
          axis.line = element_line(size = 0.8, color = "#222222"))
  
#### Gráfico Homogeneidade de Variâncias (Locação-Escala) ----
  mFit_resid %>% 
    ggplot(aes(x = .fitted, y = sqrt(abs(.std.resid)))) + 
    geom_point(color = "#234B6E") +
    # geom_hline(yintercept = 0, linetype = 2, size = 0.2) +
    geom_smooth(
      se = T, color = "tomato", method = 'loess', formula = 'y ~ x')+
    # ylab("$\\sqrt(Resíduos Padronizados)$")+
    # ggtitle("Teste")+
    labs(
      x = "Valores Ajustados",
      y = "√|Resíduos Padronizados|",
      title = "Homogeneidade de Variâncias (Locação-Escala)"
    )+
    theme_minimal(base_size = 7.5)+
    theme(legend.position = "none",
          axis.line = element_line(size = 0.8, color = "#222222"))
  
# __________________________________________________________________
performance::check_model(mFit, 
                         check = c("linearity", "qq", "homogeneity", "outliers"))
# __________________________________________________________________
  

# Parte 2 ----
## DADOS 2 ----
dados2 <- read.csv2("Dados2.csv")
