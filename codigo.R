library(tidyverse) #install.packages("tidyverse")
library(sf) #install.packages("sf")
library(DT) #install.packages("DT")
library(ggThemeAssist) #install.packages("ggThemeAssist")
library(patchwork) #install.packages("patchwork")
library(plotly) #-install.packages("plotly")
library(knitr) #install.packages("plotly")
library(tibble) #install.packages("tibble")
library(ggthemes) #install.packages("ggthemes)
library(gifski) #install.packages("gifski")
library(gt) #install.packages("gt")
library(kableExtra) #install.packages("kableExtra")
library(maps) #install.packages("maps")



dir.create("datos")

options(scipen=999)

coches <- rio::import("./datos/coches.csv")

coches <- coches %>% 
  mutate(Marca = make) %>%
  mutate(Modelo = model) %>%
  mutate(Precio = price) %>%
  mutate(Combustible = fuel) %>%
  mutate(Año = year) %>%
  mutate(Kilometros = kms) %>%
  mutate(Profesional = is_professional) %>%
  mutate(Provincia = province) %>%
  select("Marca", "Modelo", "Precio", "Combustible", "Año", "Kilometros", "Profesional", "Provincia") 

coches$Profesional <- as.character(coches$Profesional)


Marcas <- coches %>% 
  count(Marca) %>%
  arrange(desc(n))
  
masMarcas <- Marcas %>%
  filter(n > 1500)

menMarcas <- Marcas %>%
  filter(n < 1500) %>%
  filter(n > 5)

  


Grafico_masMarcas <- ggplot(masMarcas) +
  aes(x = reorder(Marca,n), y = n, fill = Marca) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  ggthemes::theme_base() +
  theme(legend.position = "none") +
  coord_flip() +labs(x = "Marca", y = "Número de coches en venta")

Grafico_menMarcas <- ggplot(menMarcas) +
  aes(x = reorder(Marca,n), y = n, fill = Marca) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  ggthemes::theme_base() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Marca", y = "Número de coches en venta")



Provincias <- coches %>% 
  count(Provincia) %>%
  arrange(desc(n))
  
Provincias <- Provincias[1:52,]

españamun <- read_sf("./shapefiles", "mun")

españamun <- españamun %>% 
  select("NAMEUNIT", "geometry") %>%
  mutate(Provincia = case_when(NAMEUNIT == "Alacant/Alicante" ~ "Alicante",
                               NAMEUNIT == "Araba/Álava" ~ "Álava",
                               NAMEUNIT == "Castelló/Castellón" ~ "Castellón",
                               NAMEUNIT == "València/Valencia" ~ "Valencia",
                               NAMEUNIT == "Gipuzkoa" ~ "Guipúzcoa",
                               NAMEUNIT == "Bizkaia" ~ "Vizcaya",
                               NAMEUNIT == "Illes Balears" ~ "Baleares",
                               NAMEUNIT == "Ourense" ~ "Orense",
                               TRUE ~ NAMEUNIT)) %>%
  select("Provincia", "geometry")

españamun <- as_tibble(españamun)


Provincias <- full_join(Provincias, españamun, by = "Provincia" )

azul_fuerte <- "#0c32f2"
azul_medio <- "#6e86ff"
azul <- "#c7f9ff"

paleta <- colorRampPalette(c(azul, azul_medio , azul_fuerte))
colores_mapa <- paleta(10)

theme_custom_map <- function(base_size = 11,
                             base_family = "",
                             base_line_size = base_size / 22,
                             base_rect_size = base_size / 22) {
  theme_bw(base_size = base_size, 
           base_family = base_family,
           base_line_size = base_line_size) %+replace%
    theme(
      axis.title = element_blank(), 
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      complete = TRUE
    )
}




MAPAESPAÑA <- ggplot(data = Provincias) + 
  geom_sf(aes(fill= n, geometry = geometry)) + 
  labs(fill = "COCHES ANUNCIADOS") + scale_fill_gradientn(colours = colores_mapa) + 
  theme_custom_map() + 
  theme(axis.text = element_text(size = 1)) +
  labs(title = "MAPA DE COCHES DE SEGUNDA MANO ANUNCIADOS EN ESPAÑA")


Prov_pre <- coches %>% select("Provincia", "Precio") %>%
  arrange(Provincia)

Prov_pre <- Prov_pre[7:50000,]

Precio_medio <- Prov_pre %>% 
  group_by(Provincia) %>%
  summarise(Media = mean(Precio)) %>%
  ungroup()

Grafico_Precio_medio <- ggplot(Precio_medio) +
  aes(x = reorder(Provincia,Media), y = Media, fill = Provincia) +
  geom_col() +
  scale_fill_hue(direction = 1) +
  coord_flip() +
  ggthemes::theme_pander() +
  theme(legend.position = "none")+
  labs(x = "Provincias", y = "Precio Medio")

KM_año <- coches %>% 
  select("Año", "Kilometros") %>%
  group_by(Año) %>%
  summarise(Media =mean(Kilometros)) %>%
  filter(!is.na(Año))%>%
  mutate(Media = as.integer(round(Media)))

Grafico_KM_año <- ggplot(KM_año) +
  aes(x = Año, y = Media) +
  geom_line(size = 0.5, colour = "#FF0000") +
  labs(
    x = "Año",
    y = "Kilometros",
    title = "Comparación KM/Año de los coches de segunda mano en España",
    subtitle = "Muestreo de 50000 coches "
  ) +
  ggthemes::theme_calc()  



