######
#AULA 1#
######
#INSTALANDO AS BIBLIOTECAS
install.packages("tidyverse")
install.packages("ggthemes")
install.packages("scales")
install.packages("broom")
install.packages("purrr")

#Conteúdo para o R: manipulação de dados com tidyverse, elaboração de gráficos com ggplot2, 
#explorando a Penn World Tables.

library(tidyverse)
library(ggthemes)
library(scales)
library(broom)
library(purrr)

#importar dados
#(código)- dados <- read.table(NomeDoArquivo.txt", header=TRUE, sep=",")

#Carregar dados

load("PWT100a.Rda")

#Selecionar e filtrar: select e filter

filter(select(pwt, countrycode, rgdpo, year), countrycode == "BRA")

#pipes: %>%

pwt %>%
  select(countrycode, year, rgdpo) %>%
  filter(countrycode == "BRA")

#cria uma nova dataframe
pib_BR <- pwt %>%
  select(code = countrycode, ano = year, pib = rgdpo) %>%
  filter(code == "BRA")

#A PWT apresenta três medidas de PIB: rgdpo, rgdpe e rgdpna, suponha que
#queiramos as três medidas para Brasil e Argentina em 2019

pwt %>%
  select(code = countrycode, pais = country, ano = year, starts_with("rgdp")) %>%
  filter(code %in% c("ARG", "BRA"), ano == 2019)

#modificar variáveis: mutate

#PIB per capita do Brasil desde 2011

pwt %>%
  select(code = countrycode, ano = year, pib = rgdpna, pop) %>%
  filter(code == "BRA", ano >= 2011) %>%
  mutate(pib.pc = pib/pop) %>%
  select(-code, -pib, - pop)

#####
#AULA 2
#####


#group_by e summarise

#PIB per capita de Brasil, Argentina, Col?mbia, Peru, Bol?via e M?xico, 
#calcular a m?dia de 2015 a 2019

paises <- c("BRA", "ARG", "COL", "PER", "BOL", "MEX")

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  group_by(code, pais) %>%
  summarise(m_pib.pc = mean(pib.pc)) %>%
  ungroup()

#Fazer novamente aparecendo c?digo e nome de cada pais e a m?dia, o desvio padr?o,o m?ximo, o m?nimo e a mediana

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  group_by(code, pais) %>%
  summarise(m_pib.pc = mean(pib.pc),
            sd_pib.pc = sd(pib.pc),
            max_pib.pc = max(pib.pc),
            min_pib.pc = min(pib.pc),
            med_pib.pc = median(pib.pc)) %>%
  ungroup()

#Arrume o resultado em ordem decrescente da m?dia do PIB per capita  

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  group_by(code, pais) %>%
  summarise(m_pib.pc = mean(pib.pc),
            sd_pib.pc = sd(pib.pc),
            max_pib.pc = max(pib.pc),
            min_pib.pc = min(pib.pc),
            med_pib.pc = median(pib.pc)) %>%
  ungroup() %>%
  arrange(desc(m_pib.pc))

#pivot_longer e pivot_wider

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  head(20)

# os dados est?o no formato long (empilhados), suponha que voc? quer os dados de PIB e PIB per capita lado 
# a lado (wide)

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  select(-pais, -pop) %>%
  pivot_wider(names_from = code, values_from = pib:pib.pc)

#empilhar

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  select(-pais, -pop) %>%
  pivot_wider(names_from = code, values_from = pib:pib.pc) %>%
  pivot_longer(names_to = "variavel", values_to = "valores", - ano) %>%
  head(20)

#melhorar colunas (tidy, uma vari?vel em cada coluna)

pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code %in% paises, ano >= 2015) %>%
  mutate(pib.pc = pib/pop) %>%
  select(-pais, -pop) %>%
  pivot_wider(names_from = code, values_from = pib:pib.pc) %>%
  pivot_longer(names_to = "variavel", values_to = "valores", - ano) %>%
  separate(variavel, c("variavel", "code"), sep="_") %>%
  head(20)

#Constru??o do df usado nas aulas

#pa?ses com mais de 5 milh?es de habitantes em 1960

pop5_1960 <- pwt %>%
  select(code = countrycode, pop, year) %>%
  filter(year == 1960, pop > 5) %>%
  pull(code)

df_pib.pc <- pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpo, pop, emp) %>%
  filter(code %in% pop5_1960) %>%
  filter(ano %in% c(1960, 1990, 2019)) %>%
  mutate(pib.pc = pib/(1000*pop),
         l_pib.pc = log(pib.pc),
         pib.emp = pib/(1000*emp),
         l_pib.emp = log(pib.emp))

#Cuidado com NAs!

5 + NA
5 * NA

x <- c(1,2,3,4,5,NA)
mean(x)
sd(x)
median(x)

mean(x, na.rm = TRUE)
sd(x, na.rm = TRUE)
median(x, na.rm = TRUE)

#procurar por NAs

is.na(x)

#em um df grande pode ser pouco util
is.na(df_pib.pc)

#tem algum NA?
any(is.na(x))
#Onde est??
which(is.na(x))
#Quanto NA tem?
sum(is.na(x))

sum(is.na(df_pib.pc))

x.limpo <- na.exclude(x)
x.limpo

#str e summary

str(df_pib.pc)
summary(df_pib.pc)

#Selecionar os 10 pa?ses com maiores PIB per capita em 2019

df_pib.pc %>%
  filter(ano == 2019) %>%
  select(pais, pib.pc) %>%
  arrange(desc(pib.pc)) %>%
  top_n(10)

df_pib.pc %>%
  filter(ano == 2019) %>%
  select(pais, pib.pc) %>%
  slice_max(order_by = pib.pc, n = 10)

#10% mais ricos
df_pib.pc %>%
  filter(ano == 2019) %>%
  select(pais, pib.pc) %>%
  slice_max(order_by = pib.pc, prop = 0.1)

#10 mais pobres
df_pib.pc %>%
  filter(ano == 2019) %>%
  select(pais, pib.pc) %>%
  slice_min(order_by = pib.pc, n = 10)

#Exerc?cio: Encontre a m?dia das taxas de investimento de todos os pa?ses da amostra para
#todos os anos entre 1980 e 2019, mostre os cinco anos com maiores taxas de investimento.

pwt %>%
  select(code = countrycode, ano = year, tx_i = csh_i) %>%
  filter(code %in% pop5_1960, ano >= 1980) %>%
  group_by(ano) %>%
  summarise(m_tx_i = mean(tx_i)) %>%
  ungroup() %>%
  slice_max(order_by = m_tx_i, n=5)

#df com crescimento dos pa?ses

df_cresc <- pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop, grupo = group) %>%
  mutate(pib.pc = pib/pop) %>%
  filter(ano %in% c(1960, 2019), code %in% pop5_1960) %>%
  select(-pib, -pop) %>%
  pivot_wider(names_from = ano, values_from = pib.pc) %>%
  mutate(cresc = 100*((`2019`/`1960`)^(1/59) - 1),
         l_pib.pc60 = log(`1960`)) %>%
  select(code, pais, grupo, l_pib.pc60, cresc)

#######
#Aula 3
#######

#Gr?ficos com ggplot2: gram?tica dos gr?ficos


#Crescimento e PIB per capita
#cada ponto ? um pa?s
ggplot(df_cresc, aes(x=l_pib.pc60, y=cresc)) +
  geom_point()
#mesmo c?digo do q o acima, entretanto, vou emplmenetando novos c?digos

ggplot(df_cresc, aes(l_pib.pc60, cresc)) +
  geom_point() +
  geom_text(aes(label = code))

#mesmo c?digo do q o acima, entretanto, vou emplmenetando novos c?digos

ggplot(df_cresc, aes(l_pib.pc60, cresc, color = grupo)) +
  geom_text(aes(label = code))

#M?dia do crescimento de cada grupo de pa?ses

df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(grupo, m_cresc)) +
  geom_col()

#ordenar as colunas e associar cor a grupos

df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(reorder(grupo, m_cresc), m_cresc, fill = grupo)) +
  geom_col() +
  scale_x_discrete(labels = NULL)

#girar

df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(reorder(grupo, m_cresc), m_cresc, fill = grupo)) +
  geom_col() +
  scale_x_discrete(labels = NULL) +
  coord_flip()

#colocar texto

df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(reorder(grupo, m_cresc), m_cresc, fill = grupo)) +
  geom_col() +
  geom_text(aes(label = m_cresc)) +
  scale_x_discrete(labels = NULL)

df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(reorder(grupo, m_cresc), m_cresc, fill = grupo)) +
  geom_col() +
  geom_text(aes(label = percent(m_cresc, scale = 1)), vjust = 1.5, color = "darkblue", size=6) +
  scale_x_discrete(labels = NULL)

#labs

df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(reorder(grupo, m_cresc), m_cresc, fill = grupo)) +
  geom_col() +
  geom_text(aes(label = percent(m_cresc, scale = 1)), vjust = 1.5, color = "darkblue", size=6) +
  scale_x_discrete(labels = NULL) +
  labs(title = "M?dia da taxa de crescimento dos pa?ses de cada grupo",
       subtitle = "1960 a 2019",
       x = NULL,
       y = "taxa de crescimento, %",
       caption = "Fonte: PWT 10.0")
  scale_fill_brewer(name = "", palette = "Set1")

#temas: ggthemes
  
df_cresc %>%
  group_by(grupo) %>%
  summarise(m_cresc = mean(cresc)) %>%
  ggplot(aes(reorder(grupo, m_cresc), m_cresc, fill = grupo)) +
  geom_col() +
  geom_text(aes(label = percent(m_cresc, scale = 1)), vjust = 1.5, color = "darkblue", size=6) +
  scale_x_discrete(labels = NULL) +
  labs(title = "M?dia da taxa de crescimento dos pa?ses de cada grupo",
         subtitle = "1960 a 2019",
         x = NULL,
         y = "taxa de crescimento, %",
         caption = "Fonte: PWT 10.0") +
    scale_fill_brewer(name = "", palette = "Set1", labels = c("Pa?ses Avan?ados", 
                                                              "Emergentes da ?sia",
                                                              "Emergentes da Europa",
                                                              "Am?rica Latina e Caribe",
                                                              "Oriente M?dio",
                                                              "?frica Subsaariana")) +
    theme_economist()

#linhas de regress?o: geom_smooth

ggplot(df_cresc, aes(l_pib.pc60, cresc)) +
  geom_text(aes(label = code, color = grupo)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue") +
  labs(title = "Crescimento e PIB per capita em 1960",
       subtitle = "crescimento entre 1960 e 2019",
       x = "log do PIB per capita em 1960",
       y = "crescimento, %",
       caption = "Fonte: PWT 10.0") +
  scale_color_brewer(name = "", palette = "Paired", labels = c("Pa?ses Avan?ados", 
                                                               "Emergentes da ?sia",
                                                               "Emergentes da Europa",
                                                               "Am?rica Latina e Caribe",
                                                               "Oriente M?dio",
                                                               "?frica Subsaariana")) +
  theme_hc()

#######
#Aula 4
#######

#Pacote broom: augment, glance, tidy

dados.BR <- pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpna, pop) %>%
  filter(code == "BRA") %>%
  mutate(l_pib.pc = log(pib/pop))

reg1 <- lm(l_pib.pc ~ ano, data = dados.BR)
summary(reg1)
names(reg1)
reg1$fitted.values
reg1$residuals

reg1.td <- tidy(reg1)
reg1.gl <- glance(reg1)
reg1.ag <- augment(reg1)

ggplot(reg1.ag, aes(ano, .resid)) +
  geom_col()

reg1.ag %>%
  select(ano, l_pib.pc, fit = .fitted) %>%
  pivot_longer(names_to = "tipo", values_to = "valor", -ano) %>%
  ggplot(aes(ano, valor, color = tipo)) +
  geom_line(size=1.5) + scale_color_brewer(palette = "Set1")

#purr: map

reg_purr <- pwt %>%
  select(code = countrycode, ano = year, pib = rgdpo, pop) %>%
  filter(code %in% pop5_1960, ano >= 1960) %>%
  mutate(l_pib.pc = log(pib/pop)) %>%
  select(-pib, -pop) %>%
  nest(data = -code) %>%
  mutate(fit = map(data, ~ lm(l_pib.pc ~ ano, data = .x)),
         glanced = map(fit, glance)) %>%
  unnest(glanced) %>%
  select(code, r2 = r.squared)

df_grupos <- pwt %>%
  select(code = countrycode, pais = country, ano = year, grupo = group) %>%
  filter(code %in% pop5_1960, ano == 2010) %>%
  select(code, pais, grupo) %>%
  mutate(grupo2 = case_when(grupo == "Advanced" ~ "Pa?ses Ricos",
                            grupo %in% c("Emerging Asia", "Latin America and Caribbean", "Emerging Europe") ~ "Emergentes",
                            TRUE ~ "Outros"),
         grupo2 = as.factor(grupo2))

df_r2 <- left_join(df_grupos, reg_purr, by = "code")

ggplot(df_r2, aes(grupo2, r2)) +
  geom_boxplot(color = "steelblue") +
  labs(title = "Boxplot do R2 com tend?ncia linear",
       subtitle = "Regress?es entre log do PIB per capita e ano, 1960 a 2019",
       x = NULL,
       y = "R2",
       caption = "Fonte: PWT 10.0") +
  theme_hc()

#Exercícios:

#1) faça um gráfico com o PIB per capita do Brasil, do Chile e da Argentina entre 1960 e 2019, use rgdpo. (3 pts)
pwt %>%
  select(code = countrycode, ano = year, pib = rgdpo, pop) %>%
  filter(code == "BRA", ano >= 2011) %>%
  mutate(pib.pc = pib/pop) %>%
  select(-code, -pib, - pop)





paises <- c("BRA", "ARG", "CHL")
pwt %>%
  select(code = countrycode, pais = country, ano = year, pib = rgdpo, pop) %>%
  filter(code %in% paises, ano >= 1960) %>%
  mutate(pib.pc = pib/pop) %>%
  select(-pais, -pib, -pop)
pib.pc <- 


ggplot(pib.pc, aes(pib, ano)) + geom_text(aes(label = code, color = grupo)) +
  labs(title = "Crescimento e PIB per capita entre 1960 e 2019",
       subtitle = "Brasil, Argentina e Chile",
       x = "Tempo (Anos)",
       y = "PIB per capita",
       caption = "Fonte: PWT 10.0") +
  scale_color_brewer(name = "", palette = "Paired", labels = c("Países Avançados", 
                                                               "Emergentes da Ásia",
                                                               "Emergentes da Europa")) +
  theme_hc()

#2) faça um gráfico do PIB per capita do Brasil, do Chile e da Argentina como proporção do PIB per capita
#Estados Unidos entre 1960 e 2019, use rgdpna. (3 pts)
#3) Para cada país com mais de 5 milhões de habitantes em 1960 faça uma regressão entre PIB per capita e o ano,
#use rgdpna, guarde o R2 ajustado de cada regressão. Para cada grupo de países (países ricos, emergentes e outros)
#calcule a média, mediana, máximo e mínimo do R2. (4 pts)










