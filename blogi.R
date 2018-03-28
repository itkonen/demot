                                        # -*- coding: iso-8859-15 -*-
## Laskelmat edellyttävät seuraavassa lueteltuja R-paketteja. Mikäli niitä ei
## vielä ole koneellasi, voit asentaa ne install.packages -komennolla.
##install.packages(c("tidyverse", "lubridate", "seasonal", "ggmosaic", "pxweb"))
library(tidyverse)
library(lubridate)
library(seasonal)
library(ggmosaic)
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
    ## Lasketaan määrätiedot 65-74 vuotiaiden ikäluokalle, joka
    ## puuttuu tilastosta, mutta on laskettavissa.
    spread(Ikäluokka, values) %>%
    mutate(`65-74` = `15-74` - `15-64`) %>%
    gather(key = "Ikäluokka", value = "values", -(1:3)) %>%
    ## ## Lasketaan työllisyys- ja työttömyysaste 65-74 vuotiaiden ikäluokalle
    spread(Tiedot, values)  %>%
    mutate(`Työllisyysaste, %` =
               ifelse(Ikäluokka == "65-74",
                      `Työlliset, 1000 henkeä`/`Väestö, 1000 henkeä`,
                      `Työllisyysaste, %`)) %>%
    mutate(`Työttömyysaste, %` =
               ifelse(Ikäluokka == "65-74",
                      `Työttömät, 1000 henkeä`/`Työvoima, 1000 henkeä`,
                      `Työttömyysaste, %`)) %>%
    gather(key = "Tiedot", value = "values", -(1:3)) %>%
    ## Erotellaan Tiedot-sarakkeesta muuttujan nimi ja mittayksikkö
    ## omiin sarakkeisiinsa
    mutate(Yksikkö = str_split_fixed(Tiedot, ", ", n = 2)[,2]) %>%
    mutate(Tiedot = str_split_fixed(Tiedot, ", ", n = 2)[,1]) %>%
    ## Korvataan aineiston alkupään puuttuvia havaintoja nollalla
    mutate(values = ifelse(Päivämäärä < "1995-01-01" &
                           is.na(values), 0, values)) %>%
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
ggsave("yleiskuva.png", height = 4)


## Kuvasta havaitaa, että tiedoissa on merkittävää kausivaihtelua. Piirretään
## seuraavaksi kuvasarja tiedoista ikäluokittain. Joista havaitaan, että
## kausivaihtelu liittyy erityisesti nuorten työmarkkina-asemaan.
data %>% distinct(Ikäluokka)
valitutIkäluokat <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74")
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

## Lasketaan seuraavaksi aineistolle kausivaihtelusta ja satunnaisvaihtelusta
## puhdistetut luvut, ja tallennetaan tiedot uuteen taulukkoon.
kausitasoitus <- function(df){
    tryCatch(
    {
        s <- seas(ts(df$values, frequency = 12, start = 1989),
                  na.action = na.exclude)
        df %>%
            mutate(Trendi = as.numeric(trend(s)),
                   Kausitasoitettu = as.numeric(final(s))) %>%
            rename("Alkuperäinen" = "values") %>%
            gather(key = "Sarjatyyppi", value = "values",
                   "Kausitasoitettu", "Trendi", "Alkuperäinen")
    }, error = function(cond) {
        ## Jos kausitasoitusohjelman automatiikka epäonnistuu,
        ## käytetään alkuperäistä sarjaa.
        df %>%
            mutate(Trendi = values,
                   Kausitasoitettu = values) %>%
            rename("Alkuperäinen" = "values") %>%
            gather(key = "Sarjatyyppi", value = "values",
                   "Kausitasoitettu", "Trendi", "Alkuperäinen")
    })
}

data2 <- 
    data %>%
    group_by(Tiedot, Sukupuoli, Ikäluokka) %>%
    nest %>% 
    mutate(model = map(data, kausitasoitus)) %>%
    unnest(model) 
    

## Piirretään aiempi kuva käyttäen kausitasoitettuja lukuja,
## jolloin havaintaan paremmin kehityksen suunta.
data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Sarjatyyppi == "Kausitasoitettu") %>% 
    ggplot(aes(x = Päivämäärä, y = values, color = Tiedot)) +
    geom_line() +
    facet_wrap(~Ikäluokka) +        
    labs(title = "Työmarkkina-asema ikäluokittain",
         subtitle = "Kausitastoitettu",
         y = "1000 henkeä",
         caption = "Lähteet: Tilastokeskus ja Suomen Pankin laskelmat.")


## Samat tiedot voidaan vielä esittää ns. pinottuina aluekuvina,
## jolloin havaitaan paremmin ikäluokkien koon ja työmarkkina-asemien
## jakauman kehitys.
data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Sarjatyyppi == "Trendi") %>% 
    ggplot(aes(x = Päivämäärä, y = values, fill = Tiedot)) +
    geom_area(alpha = 0.7, color = "lightgray") +
    facet_wrap(~Ikäluokka) +
    theme(legend.position = c(0.85, 0.2)) +
    labs(title = "Työmarkkina-asema ikäluokittain",
         subtitle = "Trendi",
         y = "1000 henkeä",
         caption = "Lähteet: Tilastokeskus ja Suomen Pankin laskelmat.")
ggsave("ikäluokittain.png", height = 4)


## Piirretään seuraavaksi kuva työttömien kokonaismäärän kehitystä eriteltynä 
## ikäluokittain.
data2 %>%
    filter(Tiedot == "Työttömät" &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Sarjatyyppi == "Trendi") %>%
    ggplot(aes(x = Päivämäärä, y = values, fill = Ikäluokka)) +
    geom_area(alpha = 0.75, color = "lightgray") +
    scale_x_date(date_breaks = "2 years", date_label = "%Y") +
    labs(title = "Työttömien määrä ikäluokittain",
         subtitle = "Liukuva keskiarvo, 12 kk",
         y = "1000 henkeä",
         caption = "Lähteet: Tilastokeskus ja Suomen Pankin laskelmat.")
ggsave("työttömätIkäluokittain.png", height = 4)


## Tarkastellaan vielä viimeisimmän vuosikeskiarvon työmarkkina-asemiaen
## jakaumaa ikäluokittain.
viimeisinHavainto <- max(data2$Päivämäärä)
data2 %>%
    filter(Päivämäärä == viimeisinHavainto &
           Tiedot %in% valitutTiedot &
           Ikäluokka %in% valitutIkäluokat &
           Sukupuoli == "Sukupuolet yhteensä" &
           Sarjatyyppi == "Kausitasoitettu") %>%
    ggplot(aes(x = Ikäluokka, y = values, fill = Tiedot)) +
    geom_col(color = "black", alpha = 0.6) +
    labs(title = paste("Työmarkkina-asema ikäluokittain,",
                       format(viimeisinHavainto, "%B %Y")),
         subtitle = "Kausitasoitettu",
         y = "1000 henkeä",
         caption = "Lähteet: Tilastokeskus ja Suomen Pankin laskelmat.")


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
           Sarjatyyppi == "Trendi") %>%
    mutate(Ikäluokka = as.character(Ikäluokka)) 
dates <- distinct(data_animaatio, Päivämäärä) %>% pluck("Päivämäärä")
dir.create("animaatio")

## Luodaan funktio, joka piirää ja tallentaa kuvan valitulle ajankohdalle.
plot_frame <- function(i) {
    print(i)
    d <- filter(data_animaatio, Päivämäärä == dates[i]) 
    g <-
        ggplot(d, aes(weight = values,
                      x = product(Tiedot, Ikäluokka),
                      fill = factor(Tiedot),
                      label = values)) +
        geom_mosaic(offset = 0, color = "lightgray") +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "top",
              plot.title = element_text(size=16)) +
        guides(fill = guide_legend(title = "")) +
        labs(x = "Ikäluokka", y = "Osuus ikäluokasta",
             title = paste("Työmarkkina-asema ikäluokittain, trendi,",
                           format(dates[i], "%Y-%m")),
             caption = "Lähteet: Tilastokeskus ja Suomen Pankin laskelmat.")   
    temp <-
        ggplot_build(g)$data %>%
                      as.data.frame %>%
                      mutate(prop = as.character(round(ymax - ymin, 3)),
                             x.position = (xmax + xmin) / 2,
                             y.position = (ymax + ymin) / 2) %>%
                      mutate(label = ifelse(.wt < 5, "",
                                            format(1000*.wt, digits = 0,
                                                   nsmall = 0, scientific = F)))
    g2 <- g + geom_text(
                  x = temp$x.position,
                  y = temp$y.position,
                  label = temp$label) 
    ggsave(paste0("animaatio/työmarkkinat", sprintf("%04d", i), ".png"),
           plot = g2, scale = 1.3, dpi = 150, width = 6, height=5.1)
}

## Piirretään kuva jokaiselle aineston kuukaudelle.
for(i in seq_along(dates))
    plot_frame(i)

## Lopuksi yhdistetän kuvat animaatioksi käyttäen FFmpeg-ohjelmistoa, jonka voi ladata ilmaiseksi osoitteesta
## http://ffmpeg.org/ . Ubuntu-pohjaisissa Linux-järjestelmissä asennus onnistuu komennolla:
## sudo apt install ffmpeg
## Tämän jälkeen voi ajaa komennon:
system("ffmpeg -framerate 12 -i animaatio/työmarkkinat%04d.png -vcodec libx264 -pix_fmt yuv420p -r 25 -strict -2 animaatio.mp4")

## Windows-käyttöjärjestelmään tarkkoitettu versio FFmpeg-ohjelmistosta löytyy täältä:
## https://ffmpeg.zeranoe.com/builds/
## ja asennus ohjeet täältä:
## https://www.wikihow.com/Install-FFmpeg-on-Windows
## Tämän jälkeen voit ajaa komentokehotteessa komennon:
## ffmpeg -framerate 12 -i animaatio\työmarkkinat%04d.png -vcodec libx264 -pix_fmt yuv420p -r 25 -strict -2 animaatio.mp4


## pxweb-paketin avulla voi ottaa yhteyttä myös lukuisiin muihin rajapintoihin.
## Seuraavilla komennoilla voit päivittää rajapintojen katalogin ja selata
## eri tietolähteitä.
update_pxweb_apis()
interactive_pxweb()



