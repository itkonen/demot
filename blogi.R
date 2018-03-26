                                        # -*- coding: iso-8859-15 -*-
## Laskelmat edellyttävät seuraavassa lueteltuja R-paketteja. Mikäli niitä ei
## vielä ole koneellasi, voit asentaa ne install.packages -komennolla.
##install.packages(c("tidyverse", "lubridate", "zoo", "treemapify", "pxweb"))
library(tidyverse)
library(lubridate)
library(zoo)
library(treemapify)
library(pxweb)


## Haetaan Työvoimatutkimus-tilaston ensimmäinen taulukko Tilastokeskuksen
## rajapinnasta. Samaista taulukkoa voi tarkastella myös nettiselaimella
## osoitteessa http://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti/statfin_tyti_pxt_001.px/
rawdata <-
    get_pxweb_data(
        "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyti/statfin_tyti_pxt_001.px",
        list(Vuosi = c('*'),
             "Kuukausi-, vuosineljännes- ja vuosikeskiarvo" =
                 c('01', '02', '03', '04', '05', '06',
                   '07', '08', '09', '10', '11', '12'),
             Sukupuoli = c('*'),
             Ikäluokka = c('*'),
             Tiedot = c('*')),
        clean = TRUE) %>% as_tibble
rawdata

## Siistitään raakadata muotoon, jossa sitä on helpompi analysoida.
data <-
    rawdata %>%
    ## Muutetaan ajankohtaa määrittävät sarakkeet date-muotoon
    mutate(Päivämäärä = ymd(str_c(Vuosi, `Kuukausi-; vuosineljännes- ja vuosikeskiarvo`, 1, sep = "-"))) %>%
    select(-Vuosi, -`Kuukausi-; vuosineljännes- ja vuosikeskiarvo`) %>%
    ## Erotellaan Tiedot-sarakkeesta muuttujan nimi ja mittayksikkö
    ## omiin sarakkeisiinsa
    mutate(Yksikkö = str_split_fixed(Tiedot, ", ", n = 2)[,2]) %>%
    mutate(Tiedot = str_split_fixed(Tiedot, ", ", n = 2)[,1]) %>%
    ## Poistetaan rivit, joista puuttuu havaintoarvo
    drop_na(values)
data


## Piirretään tilaston määrätiedoista yksinkertainen kuva, joka antaa
## yleiskäsityksen datasta.
data %>%
    filter(Yksikkö == "1000 henkeä" &
           Sukupuoli == "Sukupuolet yhteensä" &
           Ikäluokka == "15-74") %>%
    ggplot(aes(x = Päivämäärä, y = values, color = Tiedot)) +
    geom_line() +
    labs(title = "Työvoimatutkimuksen määrätiedot",
         subtitle = "Sukupuolet yhteensä, 15-74-vuotiaat",
         y = "1000 henkeä", caption = "Lähde: Tilastokeskus.")
ggsave("yleiskuva.png")


## Kuvasta havaitaa, että tiedoissa on merkittävää kausivaihtelua. Piirretään
## seuraavaksi kuvasarja tiedoista ikäluokittain. Joista havaitaan, että
## kausivaihtelu liittyy erityisesti nuorten työmarkkina-asemaan.
data %>% distinct(Ikäluokka)
valitutIkäluokat <- c("15-24", "25-34", "35-44", "45-54", "55-64")
valitutTiedot <- c("Työlliset", "Työttömät", "Työvoiman ulkopuolella olevat")
data %>%
    filter(Tiedot %in% valitutTiedot&
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä") %>%
    ggplot(aes(x = Päivämäärä, y = values, color = Tiedot)) +
    geom_line() +
    facet_wrap(~Ikäluokka) +        
    labs(title = "Työttömyysaste ikäluokittain",
         y = "%", caption = "Lähde: Tilastokeskus.")


## Lasketaan seuraavaksi kuukausihavainnoille 12 kuukauden liukuvat keskiarvot,
## joihin kausivaihtelu ei vaikuta, ja tallennetaan tiedot uuteen taulukkoon.
data2 <- 
    data %>%
    group_by(Tiedot, Sukupuoli, Ikäluokka) %>%
    mutate("Liukuva keskiarvo, 12 kk" =
               rollmean(values, 12, fill = NA, align = "right")) %>%
    ungroup %>%
    rename("Kuukausihavainto" = "values") %>%
    gather("Kuukausihavainto", "Liukuva keskiarvo, 12 kk",
           key = "Muuttuja", value = "values") %>%
    drop_na(values)
data2


## Piirretään aiempi kuva käyttäen liukuvia vuosikeskiarvoja,
## jolloin havaintaan paremmin kehityksen trendit.
data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Muuttuja == "Liukuva keskiarvo, 12 kk") %>% 
    ggplot(aes(x = Päivämäärä, y = values, color = Tiedot)) +
    geom_line() +
    facet_wrap(~Ikäluokka) +        
    labs(title = "Työmarkkina-asema ikäluokittain",
         subtitle = "Liukuva keskiarvo, 12 kk",
         y = "1000 henkeä", caption = "Lähde: Tilastokeskus.")


## Samat tiedot voidaan vielä esittää ns. pinottuina aluekuvina,
## jolloin havaitaan paremmin ikäluokkien koon ja työmarkkina-asemien
## jakauman kehitys.
data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Muuttuja == "Liukuva keskiarvo, 12 kk") %>% 
    ggplot(aes(x = Päivämäärä, y = values, fill = Tiedot)) +
    geom_area(alpha = 0.7, color = "lightgray") +
    facet_wrap(~Ikäluokka) +
    theme(legend.position = c(0.85, 0.3)) +
    labs(title = "Työmarkkina-asema ikäluokittain",
         subtitle = "Liukuva keskiarvo, 12 kk",
         y = "1000 henkeä", caption = "Lähde: Tilastokeskus.")
ggsave("ikäluokittain.png")


## Piirretään seuraavaksi kuva työttömien kokonaismäärän kehitystä eriteltynä 
## ikäluokittain.
data2 %>%
    filter(Tiedot == "Työttömät" &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Muuttuja == "Liukuva keskiarvo, 12 kk") %>%
    ggplot(aes(x = Päivämäärä, y = values, fill = Ikäluokka)) +
    geom_area(alpha = 0.75, color = "lightgray") +
    scale_x_date(date_breaks = "2 years", date_label = "%Y") +
    labs(title = "Työttömien määrä ikäluokittain",
         y = "1000 henkeä", caption = "Lähde: Tilastokeskus.")
ggsave("työttömätIkäluokittain.png")


## Tarkastellaan vielä viimeisimmän vuosikeskiarvon työmarkkina-asemiaen
## jakaumaa ikäluokittain.
viimeisinHavainto <- max(data2$Päivämäärä)
data2 %>%
    filter(Päivämäärä == viimeisinHavainto &
           Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Muuttuja == "Liukuva keskiarvo, 12 kk") %>%
    ggplot(aes(x = Ikäluokka, y = values, fill = Tiedot)) +
    geom_col(color = "black", alpha = 0.6) +
    labs(title = "Työmarkkina-asema ikäluokittain",
         subtitle = paste("Keskiarvo,",
                          format(viimeisinHavainto - months(11), "%B %Y"), "-",
                          format(viimeisinHavainto, "%B %Y")),
         y = "1000 henkeä", caption = "Lähde: Tilastokeskus.")


## Työmarkkina-asemien ja ikäluokkien kehitystä voidaan havainnolistaa myös
## animaation avulla. Käytetään animaatiossa ns. puukarttakuvio, joka kuvaa
## luontevalla tavalla eri väestöryhmien suhteellisia osuuksia. Kuviossa
## väestöryhmät esitetään tasoon ryhmiteltyinä laatikoina, joiden koko vastaa
## väestöryhmän kokoa.
## Valitaan ensin animaatioon tarvittava osajoukko aineistosta.
data_animaatio <- 
    data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Muuttuja == "Liukuva keskiarvo, 12 kk") %>%
    mutate(Ikäluokka = as.character(Ikäluokka)) 
dates <- distinct(data_animaatio, Päivämäärä) %>% pluck("Päivämäärä")
dir.create("animaatio")

## Luodaan funktio, joka piirää ja tallentaa kuvan valitulle ajankohdalle.
plot_frame <- function(i) {
    print(i)
    filter(data_animaatio, Päivämäärä == dates[i]) %>%
        ggplot(aes(area = values,
                   fill = Tiedot,
                   subgroup = Ikäluokka, 
                   label = format(1000*values, digits = 0,
                                  nsmall = 0, scientific = F))) +
        geom_treemap(fixed = T) +
        geom_treemap_text(colour = "white", fontface = "bold",
                          place = "bottomright", fixed = T) +
        geom_treemap_subgroup_border(colour = "black", fixed = T) +
        geom_treemap_subgroup_text(colour = "black", 
                                   place = "centre", fixed = T) +
        theme(legend.position = "top",
              plot.title = element_text(size=18)) +
        labs(title = paste("Työmarkkina-asema ikäluokittain, 12 kk liukuva keskiarvo,",
                           format(dates[i], "%Y-%m")),
             caption = "Lähde: Tilastokeskus.") +
    ggsave(paste0("animaatio/työmarkkinat", sprintf("%04d", i), ".png"),
           scale = 1.3, dpi = 150, width = 6, height=5)
}

## Piirretään kuva jokaiselle aineston kuukaudelle.
for(i in seq_along(dates))
    plot_frame(i)

## Lopuksi yhdistetän kuvat animaatioksi käyttäen FFmpeg-ohjelmistoa, jonka voi ladata ilmaiseksi osoitteesta http://ffmpeg.org/ . 
system("ffmpeg -framerate 12 -i animaatio/työmarkkinat%04d.png -c:v libx264 -crf 18 -r 25 animaatio.mp4")


## pxweb-paketin avulla voi ottaa yhteyttä myös lukuisiin muihin rajapintoihin.
## Seuraavilla komennoilla voit päivittää rajapintojen katalogin ja selata
## eri tietolähteitä.
update_pxweb_apis()
interactive_pxweb()

