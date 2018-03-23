##install.packages(c("tidyverse", "lubridate", "zoo", "treemapify", "pxweb"))
library(tidyverse)
library(lubridate)
library(zoo)
library(treemapify)
library(pxweb)

## pxweb-paketin avulla voi ottaa yhteyttä myös lukuisiin muihin rajapintoihin.
## Seuraavilla komennoilla voit päivittää rajapintojen katalogin ja selata
## eri tietolähteitä.
update_pxweb_apis()
interactive_pxweb()

## Haetaan Työvoimatutkimus-tilaston ensimmäinen taulukko Tilastokeskuksen
## rajapinnasta. Samaista taulukkoa voi tarkastella myös nettiselaimella
## osoitteessa http://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti/statfin_tyti_pxt_001.px/
rawdata <-
  get_pxweb_data("http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyti/statfin_tyti_pxt_001.px",
                 list(Vuosi = c('*'),
                      "Kuukausi-, vuosineljännes- ja vuosikeskiarvo" =
                        c('01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12'),
                      Sukupuoli = c('*'),
                      Ikäluokka = c('*'),
                      Tiedot = c('*')),
                 clean = TRUE) %>% as_tibble
rawdata

## Siistitään raakadata muotoon, jossa sitä on helpompi analysoida.
data <-
  rawdata %>%
  ## Muutetaan ajankohtaa määrittävät sarakkeet date-muotoon
  mutate(Päivämäärä = ymd(str_c(Vuosi, `Kuukausi-; vuosineljännes- ja vuosikeskiarvo`, 
                                15, sep = "-"))) %>%
  select(-Vuosi, -`Kuukausi-; vuosineljännes- ja vuosikeskiarvo`) %>%
  ## Erotellaan Tiedot-sarakkeesta muuttujan nimi ja mittayksikkö
  ## omiin sarakkeisiinsa
  mutate(Yksikkö = str_split_fixed(Tiedot, ", ", n = 2)[,2]) %>%
  mutate(Tiedot = str_split_fixed(Tiedot, ", ", n = 2)[,1]) %>%
  ## Poistetaan rivit, joista puuttuu havaintoarvo
  drop_na(values)
data

data %>%
  filter(Yksikkö == "1000 henkeä" &
           Sukupuoli == "Sukupuolet yhteensä" &
           Ikäluokka == "15-74") %>%
  ggplot(aes(x = Päivämäärä, y = values, color = Tiedot)) +
  geom_line() +
  labs(title = "Työvoimatutkimuksen määrätiedot, 15-74-vuotiaat",
       y = "1000 henkeä", caption = "Lähde: Tilastokeskus.")


data %>%
  filter(Yksikkö == "%" &
           Sukupuoli %in% c("Miehet", "Naiset") &
           Ikäluokka == "15-74") %>%
  ggplot(aes(x = Päivämäärä, y = values, color = Sukupuoli, linetype = Tiedot)) +
  geom_line() +
  labs(title = "Työllisyysaste ja työttömyysaste sukupuolittain, 15-74-vuotiaat",
       y = "%", caption = "Lähde: Tilastokeskus.")

data %>% distinct(Ikäluokka)
valitutIkäluokat <- c("15-24", "25-34", "35-44", "45-54", "55-64")
data %>%
  filter(Tiedot == "Työttömyysaste" &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä") %>%
  ggplot(aes(x = Päivämäärä, y = values)) +
  geom_line() +
  facet_wrap(~Ikäluokka) +        
  labs(title = "Työttömyysaste ikäluokittain",
       y = "%", caption = "Lähde: Tilastokeskus.")

data2 <- 
  data %>%
  filter(Tiedot == "Työttömät" &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä") %>%
  group_by(Ikäluokka) %>%
  mutate("Liukuva keskiarvo, 12 kk" =
           rollmean(values, 12, na.pad = T, align = "right")) %>%
  ungroup %>%
  rename("Kuukausihavainto" = "values") %>%
  gather("Kuukausihavainto", "Liukuva keskiarvo, 12 kk",
         key = "Muuttuja", value = "values") %>%
  drop_na(values)

data2 %>%
  ggplot(aes(x = Päivämäärä, y = values, color = Muuttuja)) +
  geom_line() +
  facet_wrap(~Ikäluokka) +        
  labs(title = "Työttömyysaste ikäluokittain",
       y = "%", caption = "Lähde: Tilastokeskus.")

