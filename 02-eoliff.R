library(stars)
library(tidyverse)
library(glue)
library(lubridate)
library(metR)
# metR dependency ?
library(spam)
library(magick)
library(av)
# nécessite version spécifique de rtweet pour uploader des mp4 ou gif
# remotes::install_github("tylermorganwall/rtweet@media-fixes")
library(rtweet)


################### INFOS ##############################
# Données de météo france :
# base Arome : DONNÉES DE MODÈLE ATMOSPHÉRIQUE À AIRE LIMITÉE À HAUTE RÉSOLUTION
# https://donneespubliques.meteofrance.fr/?fond=produit&id_produit=131&id_rubrique=51
# accès en téléchargement via requête après inscription
# et récupération d'un token
# /!\ ne pas lancer le script avant 16h. Avant les prévisions ne sont pas disponibles


################### PARAMÈTRES ##############################
######### 1. Généraux #########
# Token d'accès au geoservice météo france
token <- Sys.getenv("token")

# résolution du modèle 0,001 deg ou 0,0025 deg
modele <- '0025'
# indicateurs à récupérer parmi l'ensemble des variables de la base Arome
u_indic <- 'U_COMPONENT_OF_WIND__SPECIFIC_HEIGHT_LEVEL_ABOVE_GROUND'
v_indic <- 'V_COMPONENT_OF_WIND__SPECIFIC_HEIGHT_LEVEL_ABOVE_GROUND'
indic <- c(u = u_indic, v = v_indic)

######### 2. temporels #########
# Jours
j <- Sys.Date() -1
j1 <- j +1
j2 <- j +2

# Temps de référence : date et heure de création des prévisions
# disponible à partir de 15h45 le jour j
ref_time <- glue('{j}T12:00:00Z')


# prévisions sur 24h à partir de 6h du matin
# j + 1 de 6h à 23h
h_j1 <- c(sprintf('0%d', seq(6,9)), seq(10,23))
# j + 2 de minuit à 5h
h_j2 <- sprintf('0%d', seq(0,5)) 

# Date et heure de prévision
forecast <- c(glue('{j1}T{h_j1}:00:00Z'),
              glue('{j2}T{h_j2}:00:00Z'))

######### 3. géographiques #########
# emprise géographique en wgs84 (39, 53, -7, 12)
bbox <- c(lat_min = 39, lat_max = 53, lon_min = -7, lon_max = 12)
bornes_lat <- glue('{bbox["lat_min"]},{bbox["lat_max"]}')
bornes_long <- glue('{bbox["lon_min"]},{bbox["lon_max"]}')

################### TÉlÉCHARGEMENT DES TIFF ##############################
# fonction de liens des requêtes
url_model <- function(i, fc) {
  glue('https://geoservices.meteofrance.fr/api/{token}\\
       /MF-NWP-HIGHRES-AROME-{modele}\\
       -FRANCE-WCS?SERVICE=WCS&VERSION=2.0.1&REQUEST=GetCoverage&format=image/tiff&geotiff:compression=DEFLATE&coverageId=\\
       {i}___{ref_time}\\
       &subset=time({fc})\\
       &subset=lat({bornes_lat})&subset=long({bornes_long})&subset=height(10)')
}
# génération des liens
urls_request <- url_model(i = indic["u"], fc = forecast) %>%
  append(url_model(i = indic["v"], fc = forecast))

# nommage des fichiers téléchargés  
files_name <- glue('u_{1:24}_') %>%
  append(glue('v_{1:24}_'))

# téléchargement avec pause + retry si warning et arrêt complet au bout de 10 retry successifs
# d'parès https://rdrr.io/bioc/recount/src/R/download_retry.R
source("01-function-download_retry.R")
# affection du dossier temporaire
td = tempdir()
# téléchargement dans le dossier temporaire
walk2(urls_request, files_name, ~ download_retry(.x, destfile = file.path(td, glue('{.y}.tiff')), method = "libcurl"))

message("Fichiers tiff téléchargés")

################### TRAITEMENT DES DONNÉES ##############################
# Utilisation du package stars : https://r-spatial.github.io/stars/index.html
# /!\ crs des tiff en WGS84 (EPSG:4326)
wind <- read_stars(file.path(td, glue('{files_name}.tiff'))) %>%
  setNames(files_name)

message("Fichiers tiff chargés")

######### 1. Reprojection #########
# Reprojeter en Lambert 93 en conservant une grille régulière
# https://r-spatial.github.io/stars/articles/stars5.html#warping-a-raster-1

# définir une bbox qui rogne dans le tiff pour ne pas avoir de blanc
# sinon le pkg metR considère ça comme une grille non régulière
l93_bbox <- st_bbox(c(xmin = 27000, xmax = 1300000, ymax = 7300000, ymin = 5900000),
                    crs = 2154)

# création de la nouvelle grille
l93 <- wind %>%
  st_transform(2154) %>%
  st_bbox() %>%
  st_as_stars() %>%
  st_crop(l93_bbox)

# warping
wind_l93 <- wind %>%
  st_warp(l93)

######### 2. conversion en dataframe #########
wind_df <- st_as_sf(wind_l93, as_points = TRUE) %>%
  mutate(x = as.numeric(st_coordinates(geometry)[,1]),
         y = as.numeric(st_coordinates(geometry)[,2])) %>%
  st_drop_geometry()

message("Conversion en dataframe réussie")

######### PLOT #########
# Streamlines générées par le package {metR}
# paramètres OK dans ce cas: res (= densité des traits de 1 très serré à 5 espacé, S = 10
# Tidyevualation in ggplot : https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

# habillage carto => natural earth
ne_border <- st_read(file.path("data", "ne_10m_borders.shp"))
cities <- st_read(file.path("data", "cities.shp"))
relief <- image_read(file.path("data", "relief.png"))

# vitesse de vent sous forme de classe discrète plutôt que continue
# assure la comparaison entre les cartes
# discrétisation (cut) effectuée à la volé dans geom_streamline pour les différentes step
brks <- c(0,10,20,40,60,80,100,130,160,400)
lbls <- c("0-10", "10-20", "20-40", "40-60", "60-80", "80-100",
          "100-130", "130-160", "> 160")
# gamme de couleurs des streamlines
color_palette <- c('#f9f5c1', '#fdd395', '#f5a673', '#ef7a5f',
                   '#ea5562', '#c93c72', '#a32f7e', '#7c2181', '#532378')

# barre de progression sous le titre
subtitle <- "6h ============ 12h ============ 18h ============ 24h ============ 6h"
progress_bar <- str_sub(subtitle, start = 1, end = c(seq(5, 15, by = 2),
                                                    seq(22, 32, by = 2),
                                                    seq(39, 49, by = 2),
                                                    seq(56, 64, by = 2), 69))

# fonction du plot
wind_plot <- function(u, v, f, progressBar, destfile) {
  # affichage de la date et l'heure de la prévision dans le titre
  jour_semaine <- ymd_hms(f) %>% weekdays() %>% tolower
  jour <- day(ymd_hms(f))
  mois <- months(ymd_hms(f))
  heure <- hour(ymd_hms(f))
  
  p <- ggplot() +
    # habillage carto : relief + border + cities
    annotation_raster(relief, 21513.31, 1300094.61, 5896186.99, 7302626.43) +
    geom_sf(data = ne_border, fill = NA, color= "#d9dada") +
    geom_sf(data = cities, size = 0.5) +
    # limite extraite de st_bbox(wind_l93)
    coord_sf(datum = NA, xlim = c(21513.31, 1300094.61), ylim = c(5896186.99, 7302626.43), expand = FALSE) +
    # STREAMLINES
    geom_streamline(data = wind_df,
                    aes(x = x, y = y,
                               dx = .data[[u]], dy = .data[[v]],
                         # discrétisation manuelle selon vitesse du vent
                         # conversion en km/h (1 m/s = 3,6 km/h)
                         colour = cut((sqrt(stat(dx)^2 + stat(dy)^2) * 3.6), brks),
                         size = stat(step),
                         alpha = stat(step)),
                    res = 2, S = 10,
                    arrow = NULL, lineend = "round",
                    key_glyph = "rect") +
    # Couleurs des flux + titre légende + labels + afficher ensemble des valeurs
    scale_colour_discrete(type = color_palette,
                          name = "Vitesse (km/h)",
                          labels = lbls,
                          drop = FALSE) +
    # taille des streamlines selon step
    scale_size(range = c(0, 0.7), guide = "none") +
    # transparence selon step
    scale_alpha(guide = "none") +
    # labels
    geom_sf_text(data = cities, aes(label = name, hjust = 0), nudge_x = 10000, size = 3) +
    # habillage texte
    labs(title = glue('Prévisions de vent : {jour_semaine} {jour} {mois} à {heure}h'),
         subtitle = progressBar,
         tag = "Source : Météo France, base Arome.\nThomas Ansart, CC-BY-NC 4.0") +
    # positionnement et personnalisation
    theme(plot.title = element_text(size = 20, face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = c(0.90, 0.85),
          legend.background = element_blank(),
          legend.key.height = unit(0.4,"cm"),
          plot.tag = element_text(size = 9, hjust = 0),
          plot.tag.position = c(0.04, 0.06),
          plot.margin = margin(0,0,-25,-14),
          # changer le fond gris de geom_sf
          panel.background = element_blank()) +
   ggsave(destfile, path = td, height = 20, width = 17, units = "cm")
}

# générer les plots
pwalk(list(u = files_name[1:24], v = files_name[25:48],
           f = forecast, progressBar = progress_bar,
           destfile = glue('plot_{1:24}.jpg')), wind_plot)

warnings()
message("Plots générés")

# création du gif avec ImageMagick
gif_name <- glue('previsions-vent-{date(ymd_hms(forecast[1]))}.mp4')
list.files(path = td, pattern = "plot", full.names = TRUE) %>%
  str_sort(numeric = TRUE) %>% 
  map(image_read) %>% 
  image_join() %>% 
  image_scale("x1080") %>%
  image_write_video(glue("{td}/{gif_name}"), framerate = 5)

message("Gif crée")

######### TWITTER #########
# authentification au compte twitter @Eole_Gif
token_twitter <- create_token(
  app = "eolif",
  consumer_key = Sys.getenv("api_key"),
  consumer_secret = Sys.getenv("api_secret_key"),
  access_token = Sys.getenv("access_token"),
  access_secret = Sys.getenv("access_token_secret"),
  set_renv = FALSE
  )

# /!\ version officielle de rtweet non compatible avec envoi de fichier gif ou mp4
# Tyler morgan wall à proposé un fixe dispo dans son fork
# https://github.com/tylermorganwall/rtweet/tree/media-fixes
# remotes::install_github("tylermorganwall/rtweet@media-fixes")
post_tweet(status = glue("Prévisions de vent du {date(ymd_hms(forecast[1]))}"),
           media = glue("{td}/{gif_name}"),
          token = token_twitter)

message("Fin du script")
