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

# PARTE 1 ----
## Dados 1 - Import ----
dados1 <- datasets::trees

### Arrumação ----
dados1 <- dados1|>
  janitor::clean_names()

dados1 <- dados1|>
  mutate(
    height = height*0.3048,
    volume = volume*0.02831685,
    girth = girth*0.3048
  )
## Análises ----
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
  )|>
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
  footnote(general = "---") |>
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

corrplot::corrplot(cor(dados1), method = "number", type = "lower")


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
    label.x = 19.2, label.y = 2, show.legend = F,
    p.accuracy = 0.001, r.accuracy = 0.0001,
    size = 3)+
  ggpubr::stat_regline_equation(
      aes(label = paste(..eq.label.., ..adj.rr.label.., sep = "~`; `~")),
          geom = "text", label.x = 19.2, label.y = 1.75, 
      position = "identity", 
      color="red",
      size = 3, show.legend = F
  )+
  geom_smooth(
    method=lm, se=T, formula = "y ~ x", color = "tomato")+
  theme_minimal()+
  scale_x_continuous(breaks = seq(19,27,1))+
  scale_y_continuous(
  breaks = seq(0, 2.5, 0.5),
    labels = scales::number_format(
      big.mark = ".",
      decimal.mark = ","
    ))+
    theme(legend.position = "none",
          axis.line = element_line(size = 0.8, color = "#222222"))
          # axis.line = element_line(size = 0.8, color = "tomato"))|>
  # ggpubr::ggpar(palette = "grey", font.x = "bold") #não funciona
  
  
(mFit1 <- lm(volume~height, data = dados1))
  
### Significância ----




###Ana.  Resíduos ----
{
#### Gráficos RBase ----
  par(mfrow = c(2, 2))
  
  plot(mFit1)
  
  par(mfrow = c(1, 1))
  
# _____________________________________________
  
### Gráficos GGplot2 ----
  mFit1_resid <- broom::augment(mFit1)
  dplyr::glimpse(mFit1_resid)
  
  library(ggthemes)
  
##### Histograma dos resíduos padronizados + curva normal ----
  mFit1_resid|>
    ggplot2::ggplot(aes(x = .std.resid))+
    geom_histogram(aes(y = ..density..), fill = "skyblue", 
                   color = "blue", binwidth = 0.3, alpha = 0.5)+
    geom_density(fill = "red", alpha = 0.2)+
    stat_function(fun = dnorm, 
                  args = (
                    list(
                      mean = mean(mFit1_resid$.std.resid), 
                      sd = sd(mFit1_resid$.std.resid))), 
                  geom = "polygon", fill = "blue", alpha = 0.5,
                  color = "black", size = 0.5)+
    labs(
      x = "Resíduos Padronizados",
      y = "Densidade",
      title = "Histograma dos Resíduos Padronizados"
    )+
    scale_x_continuous(breaks = seq(-3, 3, 1))+
    scale_y_continuous(
      # labels = scales::percent
      labels = scales::number_format(
        big.mark = ".",
        decimal.mark = ","
      )
      )+
    # theme_minimal()+
    theme(legend.position = "none",
          axis.line = element_line(size = 0.8, color = "#222222"))
    ggthemes::theme_economist()
  
##### Gráfico de Resíduos contra Valor Médio ----
d1 <- mFit1_resid|>
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
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))
  
##### Gráfico de normalidade dos resíduos ----
d2 <- mFit1_resid %>% 
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
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))
  
##### Gráfico Homogeneidade de Variâncias (Locação-Escala) ----
d3 <- mFit1_resid %>% 
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
    title = "Homogeneidade de Variâncias (Locação-Escala)")+
  scale_x_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  scale_y_continuous(
    labels = scales::number_format(
      big.mark = ".", decimal.mark = ","))+
  theme_minimal()+
  theme(
    legend.position = "none",
    plot.title = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 8, face = "plain"),
    axis.line = element_line(size = 0.8, color = "#222222"))
  
    
d1 + d2 + d3 + 
  plot_layout(ncol = 2) +
  plot_annotation(
    title = "Figura 5: Análise de resíduos do modelo ajustado",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ",
    tag_sep = ".", tag_suffix = ":") &
  theme(
    legend.position = "none",
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 5.5, hjust = 0, vjust = -0.4)
  )

###### Pacote Performance ----
performance::check_model(mFit1, 
                         check = c("linearity", "qq", "homogeneity", "outliers"))
# __________________________________________________________________
  
}
  
#### Testes ----  

res1 <- residuals(mFit1) # Resíduo Ordinário

##### Teste de normalidade dos resíduos ----
  #H0: normalidade
  #H1: não normalidade

# KS
(t_ks <- ks.test(res1, "pnorm", mean(res1), sd(res1)))
# pnorm = acumulada da normal, o "p" antes da distribuição indica ser a acumulada.

# SW*
(t_sw <- shapiro.test(res1))

##### Teste de homoscedasticidade dos resíduos ----
  #H0: resíduos homoscedásticos - Variância constante
  #H1: resíduos heteroscedásticos - Variância NÃO constante

# GQ
(t_gq <- lmtest::gqtest(mFit1))

# BP*
(t_bp <- lmtest::bptest(mFit1, studentize = F))

lmtest::bptest(mFit1, studentize = T) # Teste

# PARK

summary(lm(res1^2 ~ dados1$height))

# Teste deF para linearidade


# Teste de correlação serial lag 1 (Independência dos erros)
  #H0: correlacionados - existe correlação serial
  #H1: não correlacionados - não existe correlação serial ///ficou confuso no vídeo as hipoteses///

# DW
(t_dw <- lmtest::dwtest(mFit1))








resultados <- rbind(
  t_ks$statistic,
  t_sw$statistic,
  t_gq$statistic,
  t_bp$statistic,
  t_dw$statistic)

aux <- rbind(
  t_ks$p_value,
  t_sw$p_value,
  t_gq$p_value,
  t_bp$p_value,
  t_dw$p_value)

resultados <- cbind(resultados, aux)

rownames(resultados) <- c("Kolmogorov-Smirnov", "Shapiro-Wilks", "Goldfeld-Quandt", "Breush-Pagan", "Durbin-Watson")

colnames(resultados) <- c("Estatística teste", "p-valor")

resultados|>
  kbl(
    caption = "Testes de Diagnósticos dos Resíduos",
    digits = 5,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", row.names = T, booktabs = T
  )|>
  kable_styling(
    full_width = F, position = 'center', 
    latex_options = c("striped", "HOLD_position", "repeat_header")
  )|>
  column_spec(1, bold = T
  )|>
  # footnote(
  #   general = "Teste realizado com 5% de significância",
  #   general_title = "Nota:",
  #   footnote_as_chunk = T
  # )|>
  kable_material()
  
# PARTE 2 ----
## Dados 2 - Import ----
dados2 <- read.csv2("Dados2.csv")

### Arrumação ----
dados2 <- dados2|>
  janitor::clean_names()

## Análises ----