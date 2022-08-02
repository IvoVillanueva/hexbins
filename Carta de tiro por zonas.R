library(tidyverse)
library(ggtext)
library(viridis)
library(ggimage)
library(densityvis)
library(hexbin)
library(rcartocolor)

theme_ivo <- function() {
  library(ggtext)
  theme_minimal(base_size = 9, base_family = "Inconsolata") %+replace%
    theme(
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#fff5e3", color = "#fff5e3"),
      plot.caption = element_markdown(size = 5, family = "Chivo", color = "#626060")
    )
}


# Load FIBA court dimensions from github
devtools::source_url("https://raw.githubusercontent.com/IvoVillanueva/fibabasketballcourt/main/court5")

teams <- read.csv("https://raw.githubusercontent.com/IvoVillanueva/acbTeams2022/main/acb.csv") %>% # logos
  select(abb, tm, logo, color)

pbp_acb_22 <- read_csv("pbp_Df2022.csv",
  show_col_types = FALSE
)

pbp_acb_21 <- read_csv("pbp_Df2021.csv",
  show_col_types = FALSE
)
options(scipen = 999)


# coordenadas plot canasta---------------------------------------------------

cancha <-
  ggplot() +
  geom_path(
    data = court_points,
    aes(x = x, y = y, group = desc, linetype = dash),
    color = "black", size = .2
  ) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(1, 12, .25)) +
  scale_x_continuous(limits = c(-7.5, 7.5), breaks = seq(-7.5, 7.5, .25)) +
  scale_linetype_manual(values = c("solid", "longdash"), guide = "none")


# funcion para la densidad ------------------------------------------------


get_density <- function(x, y, ...) {
  density_out <- MASS::kde2d(x, y, ...)
  int_x <- findInterval(x, density_out$x)
  int_y <- findInterval(y, density_out$y)
  comb_int <- cbind(int_x, int_y)
  return(density_out$z[comb_int])
}

# data con zonas ----------------------------------------------------------

pbp_rf <- rbind(pbp_acb_22, pbp_acb_21) %>%
  select(license_licenseStr, license_licenseStr15, id_phase, edition_year, type_description, team_team_abbrev_name, team_team_actual_name, y = posX, x = posY) %>%
  filter(edition_year == 2021, id_phase == 107) %>%
  filter(type_description %in% c("Canasta de 3", "Intento fallado de 3", "Canasta de 2", "Intento fallado de 2", "Mate", "Mate Fuera")) %>%
  mutate(
    x = x / 1000 * -1,
    y = y / 1000 + hoop_center_y,
    type_description = case_when(
      type_description == "Mate" ~ "Canasta de 2",
      type_description == "Mate Fuera" ~ "Intento fallado de 2",
      type_description == "Canasta de 2" ~ "si_2",
      type_description == "Intento fallado de 2" ~ "no_2",
      type_description == "Intento fallado de 3" ~ "no_3",
      type_description == "Canasta de 3" ~ "si_3",
      TRUE ~ type_description
    ),
    zona = case_when(
      str_detect(type_description, "3") & x < -6.5 & y < 3 ~ "latizd3",
      str_detect(type_description, "3") & x > 6.5 & y < 3 ~ "latdch3",
      str_detect(type_description, "3") & x < -2.5 & y > 3 ~ "izd3",
      str_detect(type_description, "3") & x > 2.5 & y > 3 ~ "dch3",
      str_detect(type_description, "3") & x > -2.5 & x < 2.5 & y > 7.9 ~ "cnt3",
      str_detect(type_description, "2") & x < -2.5 & y < 4 ~ "latizd2",
      str_detect(type_description, "2") & x > 2.5 & y < 4 ~ "latdch2",
      str_detect(type_description, "2") & x < -2.5 & y > 3 ~ "izd2",
      str_detect(type_description, "2") & x > 2.5 & y > 3 ~ "dch2",
      str_detect(type_description, "2") & x > -2.5 & x < 2.5 & y > 4 ~ "cnt2",
      str_detect(type_description, "2") & x > -2.5 & x < 2.5 & y < 4 ~ "cnt21"
    )
  ) %>%
  na.omit() %>%
  group_by(zona) %>%
  mutate(densidad  = get_density(x, y, n = 100)) %>%
  ungroup()

# plot de pruebas ---------------------------------------------------------
set.seed(101)
x <- rnorm(10000)
y <- rnorm(10000)
(bin <- hexbin(x, y))
## or
plot(hexbin(x, y + x*(x+1)/4),
     main = "(X, X(X+1)/4 + Y) where X,Y ~ rnorm(10000)")
## Using plot method for hexbin objects:
plot(bin, style = "nested.lattice")
hbi <- hexbin(y ~ x, xbins = 80, IDs= TRUE)
str(hbi)
tI <- table(hbi@cID)
stopifnot(names(tI) == hbi@cell,
          tI == hbi@count)
## NA's now work too:
x[runif(6, 0, length(x))] <- NA
y[runif(7, 0, length(y))] <- NA
hbN <- hexbin(x,y)
summary(hbN)

p <- pbp_rf %>%
  filter( type_description %in% c("si_2", "si_3"))
px <- p$x
py<- p$y
bin <- hexbin(px, py)
plot(bin, style = "lattice")+
  geom_path(
    data = court_points,
    aes(x = x, y = y, group = desc, linetype = dash),
    color = "black", size = .5
  )

# plot de puntos ----------------------------------------------------------


p <- pbp_rf %>%
  filter( type_description %in% c("si_2", "si_3")) %>%
  ggplot(aes(x = x, y = y)) +
  geom_point( aes(alpha = .45, color = zona,  size = densidad))+
  coord_fixed() +
  scale_fill_viridis(option = "A")
  geom_path(
    data = court_points,
    aes(x = x, y = y, group = desc, linetype = dash),
    color = "black", size = .5
  ) +
  scale_y_continuous(limits = c(0, 12), breaks = seq(1, 12, 2)) +
  scale_x_continuous(limits = c(-7.5, 7.5), breaks = seq(-8, 8, 2)) +
  scale_linetype_manual(values = c("solid", "longdash"), guide = "none") +
  theme_ivo() +
  guides(color = guide_colourbar(order = 1))


# prueba de calculos------------------------------------------------------


pbp_rf3 <- rbind(pbp_acb_22, pbp_acb_21) %>%
  select(license_licenseStr, license_licenseStr15, id_phase, edition_year, type_description, team_team_abbrev_name, team_team_actual_name, y = posX, x = posY) %>%
  filter(edition_year == 2021, id_phase == 107) %>%
  filter(type_description %in% c("Canasta de 3", "Intento fallado de 3")) %>%
  mutate(
    x = x / 1000 * -1,
    y = y / 1000 + hoop_center_y,
    type_description = ifelse(type_description == "Canasta de 3", "si_3", "no_3")
  ) %>%
  na.omit()

# de 3 --------------------------------------------------------------------


pct_latizd3 <- pbp_rf3 %>%
  filter(x < -6.5 & y < 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    latizd3 = no_3 + si_3,
    pct_latizd3 = si_3 / latizd3
  ) %>%
  select(latizd3, pct_latizd3) %>%
  pivot_longer(cols = c(latizd3, pct_latizd3), names_to = "spot")

pct_latdch3 <- pbp_rf3 %>%
  filter(x > 6.5 & y < 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    latdch3 = no_3 + si_3,
    pct_latdch3 = si_3 / latdch3
  ) %>%
  select(latdch3, pct_latdch3) %>%
  pivot_longer(cols = c(latdch3, pct_latdch3), names_to = "spot")


pct_izd3 <- pbp_rf3 %>%
  filter(x < -2.5 & y > 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    izd3 = no_3 + si_3,
    pct_izd3 = si_3 / izd3
  ) %>%
  select(izd3, pct_izd3) %>%
  pivot_longer(cols = c(izd3, pct_izd3), names_to = "spot")


pct_dch3 <- pbp_rf3 %>%
  filter(x > 2.5 & y > 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    dch3 = no_3 + si_3,
    pct_dch3 = si_3 / dch3
  ) %>%
  select(dch3, pct_dch3) %>%
  pivot_longer(cols = c(dch3, pct_dch3), names_to = "spot")


pct_cnt3 <- pbp_rf3 %>%
  filter(x > -2.5 & x < 2.5, y > 7.9) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    cnt3 = no_3 + si_3,
    pct_cnt3 = si_3 / cnt3
  ) %>%
  select(cnt3, pct_cnt3) %>%
  pivot_longer(cols = c(cnt3, pct_cnt3), names_to = "spot")

de_3 <- rbind(
  pct_latizd3,
  pct_latdch3,
  pct_izd3,
  pct_dch3,
  pct_cnt3
) %>%
  tibble()



# de2 ---------------------------------------------------------------------


pbp_rf2 <- rbind(pbp_acb_22, pbp_acb_21) %>%
  select(license_licenseStr, license_licenseStr15, id_phase, edition_year, type_description, team_team_abbrev_name, team_team_actual_name, y = posX, x = posY) %>%
  filter(edition_year == 2021, id_phase == 107) %>%
  filter(type_description %in% c("Canasta de 2", "Intento fallado de 2", "Mate", "Mate Fuera")) %>%
  mutate(
    x = x / 1000 * -1,
    y = y / 1000 + hoop_center_y,
    type_description = case_when(
      type_description == "Mate" ~ "Canasta de 2",
      type_description == "Mate Fuera" ~ "Intento fallado de 2",
      TRUE ~ type_description
    ),
    type_description = ifelse(type_description == "Canasta de 2", "si_2", "no_2")
  ) %>%
  na.omit()


pct_latizd2 <- pbp_rf2 %>%
  filter(x < -2.5 & y < 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    latizd2 = no_2 + si_2,
    pct_latizd2 = si_2 / latizd2
  ) %>%
  select(latizd2, pct_latizd2) %>%
  pivot_longer(cols = c(latizd2, pct_latizd2), names_to = "spot")

pct_latdch2 <- pbp_rf2 %>%
  filter(x > 2.5 & y < 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    latdch2 = no_2 + si_2,
    pct_latdch2 = si_2 / latdch2
  ) %>%
  select(latdch2, pct_latdch2) %>%
  pivot_longer(cols = c(latdch2, pct_latdch2), names_to = "spot")


pct_izd2 <- pbp_rf2 %>%
  filter(x < -2.5 & y > 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    izd2 = no_2 + si_2,
    pct_izd2 = si_2 / izd2
  ) %>%
  select(izd2, pct_izd2) %>%
  pivot_longer(cols = c(izd2, pct_izd2), names_to = "spot")


pct_dch2 <- pbp_rf2 %>%
  filter(x > 2.5 & y > 3) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    dch2 = no_2 + si_2,
    pct_dch2 = si_2 / dch2
  ) %>%
  select(dch2, pct_dch2) %>%
  pivot_longer(cols = c(dch2, pct_dch2), names_to = "spot")


pct_cnt2 <- pbp_rf2 %>%
  filter(x > -2.5 & x < 2.5 & y > 4) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    cnt2 = no_2 + si_2,
    pct_cnt2 = si_2 / cnt2
  ) %>%
  select(cnt2, pct_cnt2) %>%
  pivot_longer(cols = c(cnt2, pct_cnt2), names_to = "spot")


pct_cnt21 <- pbp_rf2 %>%
  filter(x > -2.5 & x < 2.5 & y < 4) %>%
  count(type_description) %>%
  pivot_wider(names_from = type_description, values_from = n) %>%
  mutate(
    cnt21 = no_2 + si_2,
    pct_cnt21 = si_2 / cnt21
  ) %>%
  select(cnt21, pct_cnt21) %>%
  pivot_longer(cols = c(cnt21, pct_cnt21), names_to = "spot")


df_pct <- rbind(
  de_3,
  pct_latizd2,
  pct_latdch2,
  pct_izd2,
  pct_dch2,
  pct_cnt2,
  pct_cnt21
) %>%
  tibble()
