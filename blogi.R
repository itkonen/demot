                                        # -*- coding: iso-8859-15 -*-
## Versio: 1.0
## Tekij�: Juha Itkonen @ Suomen Pankki

## Laskelmat edellytt�v�t seuraavassa lueteltuja R-paketteja. Mik�li niit� ei
## viel� ole koneellasi, voit asentaa ne install.packages -komennolla.
##install.packages(c("tidyverse", "lubridate", "seasonal", "ggmosaic", "pxweb"))
library(tidyverse)
library(lubridate)
library(seasonal)
library(ggmosaic)
library(pxweb)


## Haetaan Ty�voimatutkimus-tilaston ensimm�inen taulukko Tilastokeskuksen
## rajapinnasta. Samaista taulukkoa voi tarkastella my�s nettiselaimella
## osoitteessa http://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti/statfin_tyti_pxt_001.px/
rawdata <-
    get_pxweb_data(
        "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyti/statfin_tyti_pxt_001.px",
        list(Vuosi = c('*'),
             "Kuukausi-, vuosinelj�nnes- ja vuosikeskiarvo" =
                 c('01', '02', '03', '04', '05', '06',
                   '07', '08', '09', '10', '11', '12'),
             Sukupuoli = c('*'),
             Ik�luokka = c('*'),
             Tiedot = c('*')),
        clean = TRUE) %>% as_tibble
rawdata

## Siistit��n raakadata muotoon, jossa sit� on helpompi analysoida.
data <-
    rawdata %>%
    ## Muutetaan ajankohtaa m��ritt�v�t sarakkeet date-muotoon
    mutate(P�iv�m��r� = ymd(str_c(Vuosi, `Kuukausi-; vuosinelj�nnes- ja vuosikeskiarvo`, 1, sep = "-"))) %>%
    select(-Vuosi, -`Kuukausi-; vuosinelj�nnes- ja vuosikeskiarvo`) %>%
    ## Lasketaan m��r�tiedot 65-74 vuotiaiden ik�luokalle, joka
    ## puuttuu tilastosta, mutta on laskettavissa.
    spread(Ik�luokka, values) %>%
    mutate(`65-74` = `15-74` - `15-64`) %>%
    gather(key = "Ik�luokka", value = "values", -(1:3)) %>%
    ## ## Lasketaan ty�llisyys- ja ty�tt�myysaste 65-74 vuotiaiden ik�luokalle
    spread(Tiedot, values)  %>%
    mutate(`Ty�llisyysaste, %` =
               ifelse(Ik�luokka == "65-74",
                      `Ty�lliset, 1000 henke�`/`V�est�, 1000 henke�`,
                      `Ty�llisyysaste, %`)) %>%
    mutate(`Ty�tt�myysaste, %` =
               ifelse(Ik�luokka == "65-74",
                      `Ty�tt�m�t, 1000 henke�`/`Ty�voima, 1000 henke�`,
                      `Ty�tt�myysaste, %`)) %>%
    gather(key = "Tiedot", value = "values", -(1:3)) %>%
    ## Erotellaan Tiedot-sarakkeesta muuttujan nimi ja mittayksikk�
    ## omiin sarakkeisiinsa
    mutate(Yksikk� = str_split_fixed(Tiedot, ", ", n = 2)[,2]) %>%
    mutate(Tiedot = str_split_fixed(Tiedot, ", ", n = 2)[,1]) %>%
    ## Korvataan aineiston alkup��n puuttuvia havaintoja nollalla
    mutate(values = ifelse(P�iv�m��r� < "1995-01-01" &
                           is.na(values), 0, values)) %>%
    ## Poistetaan rivit, joista puuttuu havaintoarvo
    drop_na(values)           
data


## Piirret��n tilaston m��r�tiedoista yksinkertainen kuva, joka antaa
## yleisk�sityksen datasta.
data %>%
    filter(Yksikk� == "1000 henke�" &
           Sukupuoli == "Sukupuolet yhteens�" &
           Ik�luokka == "15-74") %>%
    ggplot(aes(x = P�iv�m��r�, y = values, color = Tiedot)) +
    geom_line() +
    labs(title = "Ty�voimatutkimuksen m��r�tiedot",
         subtitle = "Sukupuolet yhteens�, 15-74-vuotiaat",
         y = "1000 henke�", caption = "L�hde: Tilastokeskus.")
ggsave("yleiskuva.png", height = 4)

## Kuvasta havaitaa, ett� tiedoissa on merkitt�v�� kausivaihtelua. Piirret��n
## seuraavaksi kuvasarja tiedoista ik�luokittain. Joista havaitaan, ett�
## kausivaihtelu liittyy erityisesti nuorten ty�markkina-asemaan.
data %>% distinct(Ik�luokka)
valitutIk�luokat <- c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74")
valitutTiedot <- c("Ty�lliset", "Ty�tt�m�t", "Ty�voiman ulkopuolella olevat")
data %>%
    filter(Tiedot %in% valitutTiedot&
           Ik�luokka %in% valitutIk�luokat &
           Sukupuoli == "Sukupuolet yhteens�") %>%
    ggplot(aes(x = P�iv�m��r�, y = values, color = Tiedot)) +
    geom_line() +
    facet_wrap(~Ik�luokka) +        
    labs(title = "Ty�tt�myysaste ik�luokittain",
         y = "%", caption = "L�hde: Tilastokeskus.")

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
            rename("Alkuper�inen" = "values") %>%
            gather(key = "Sarjatyyppi", value = "values",
                   "Kausitasoitettu", "Trendi", "Alkuper�inen")
    }, error = function(cond) {
        ## Jos kausitasoitusohjelman automatiikka ep�onnistuu,
        ## k�ytet��n alkuper�ist� sarjaa.
        df %>%
            mutate(Trendi = values,
                   Kausitasoitettu = values) %>%
            rename("Alkuper�inen" = "values") %>%
            gather(key = "Sarjatyyppi", value = "values",
                   "Kausitasoitettu", "Trendi", "Alkuper�inen")
    })
}

## Luodaan uusi taulukko, jossa on mukana my�s kausitasoitetut ja trendi sarjat.
data2 <- 
    data %>%
    group_by(Tiedot, Sukupuoli, Ik�luokka) %>%
    nest %>% 
    mutate(model = map(data, kausitasoitus)) %>%
    unnest(model) 
    

## Piirret��n aiempi kuva k�ytt�en kausitasoitettuja lukuja,
## jolloin havaintaan paremmin kehityksen suunta.
data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ik�luokka %in% valitutIk�luokat &
           Sukupuoli == "Sukupuolet yhteens�" &
           Sarjatyyppi == "Kausitasoitettu") %>% 
    ggplot(aes(x = P�iv�m��r�, y = values, color = Tiedot)) +
    geom_line() +
    facet_wrap(~Ik�luokka) +        
    labs(title = "Ty�markkina-asema ik�luokittain",
         subtitle = "Kausitastoitettu",
         y = "1000 henke�",
         caption = "L�hteet: Tilastokeskus ja Suomen Pankin laskelmat.")


## Samat tiedot voidaan viel� esitt�� ns. pinottuina aluekuvina,
## jolloin havaitaan paremmin ik�luokkien koon ja ty�markkina-asemien
## jakauman kehitys.
data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ik�luokka %in% valitutIk�luokat &
           Sukupuoli == "Sukupuolet yhteens�" &
           Sarjatyyppi == "Trendi") %>% 
    ggplot(aes(x = P�iv�m��r�, y = values, fill = Tiedot)) +
    geom_area(alpha = 0.7, color = "lightgray") +
    facet_wrap(~Ik�luokka) +
    theme(legend.position = c(0.85, 0.2)) +
    labs(title = "Ty�markkina-asema ik�luokittain",
         subtitle = "Trendi",
         y = "1000 henke�",
         caption = "L�hteet: Tilastokeskus ja Suomen Pankin laskelmat.")
ggsave("ik�luokittain.png", height = 4)


## Piirret��n seuraavaksi kuva ty�tt�mien kokonaism��r�n kehityst� eriteltyn� 
## ik�luokittain.
data2 %>%
    filter(Tiedot == "Ty�tt�m�t" &
           Ik�luokka %in% valitutIk�luokat &
           Sukupuoli == "Sukupuolet yhteens�" &
           Sarjatyyppi == "Trendi") %>%
    ggplot(aes(x = P�iv�m��r�, y = values, fill = Ik�luokka)) +
    geom_area(alpha = 0.75, color = "lightgray") +
    scale_x_date(date_breaks = "2 years", date_label = "%Y") +
    labs(title = "Ty�tt�mien m��r� ik�luokittain",
         subtitle = "Trendi",
         y = "1000 henke�",
         caption = "L�hteet: Tilastokeskus ja Suomen Pankin laskelmat.")
ggsave("ty�tt�m�tIk�luokittain.png", height = 4)


## Tarkastellaan viel� viimeisimm�n vuosikeskiarvon ty�markkina-asemiaen
## jakaumaa ik�luokittain.
viimeisinHavainto <- max(data2$P�iv�m��r�)
data2 %>%
    filter(P�iv�m��r� == viimeisinHavainto &
           Tiedot %in% valitutTiedot &
           Ik�luokka %in% valitutIk�luokat &
           Sukupuoli == "Sukupuolet yhteens�" &
           Sarjatyyppi == "Kausitasoitettu") %>%
    ggplot(aes(x = Ik�luokka, y = values, fill = Tiedot)) +
    geom_col(color = "black", alpha = 0.6) +
    labs(title = paste("Ty�markkina-asema ik�luokittain,",
                       format(viimeisinHavainto, "%B %Y")),
         subtitle = "Kausitasoitettu",
         y = "1000 henke�",
         caption = "L�hteet: Tilastokeskus ja Suomen Pankin laskelmat.")


## Ty�markkina-asemien ja ik�luokkien kehityst� voidaan havainnolistaa my�s
## animaation avulla. K�ytet��n animaatiossa ns. mosaiikkikuviota, joka kuvaa
## eri v�est�ryhmien suhteellisia osuuksia. Kuviossa v�est�ryhm�t esitet��n
## tasoon ryhmiteltyin� laatikoina, joiden koko vastaa v�est�ryhm�n kokoa.
## Valitaan ensin animaatioon tarvittava osajoukko aineistosta.
data_animaatio <- 
    data2 %>%
    filter(Tiedot %in% valitutTiedot &
           Ik�luokka %in% valitutIk�luokat &
           Sukupuoli == "Sukupuolet yhteens�" &
           Sarjatyyppi == "Trendi") %>%
    mutate(Ik�luokka = as.character(Ik�luokka)) 
dates <- distinct(data_animaatio, P�iv�m��r�) %>% pluck("P�iv�m��r�")
dir.create("animaatio")

## Luodaan funktio, joka piir�� ja tallentaa kuvan valitulle ajankohdalle.
plot_frame <- function(i) {
    print(i)
    d <- filter(data_animaatio, P�iv�m��r� == dates[i]) 
    g <-
        ggplot(d, aes(weight = values,
                      x = product(Tiedot, Ik�luokka),
                      fill = factor(Tiedot),
                      label = values)) +
        geom_mosaic(offset = 0, color = "lightgray") +
        scale_y_continuous(labels = scales::percent) +
        theme(legend.position = "top",
              plot.title = element_text(size = 16)) +
        guides(fill = guide_legend(title = "")) +
        labs(x = "Ik�luokka", y = "Osuus ik�luokasta",
             title = paste("Ty�markkina-asema ik�luokittain, trendi,",
                           format(dates[i], "%Y-%m")),
             caption = "L�hteet: Tilastokeskus ja Suomen Pankin laskelmat.")   
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
    ggsave(paste0("animaatio/ty�markkinat", sprintf("%04d", i), ".png"),
           plot = g2, scale = 1.3, dpi = 150, width = 6, height = 5.1)
}

## Piirret��n kuva jokaiselle aineston kuukaudelle.
for(i in seq_along(dates))
    plot_frame(i)

## Lopuksi yhdistet�n kuvat animaatioksi k�ytt�en FFmpeg-ohjelmistoa, jonka voi ladata ilmaiseksi osoitteesta
## http://ffmpeg.org/ . Ubuntu-pohjaisissa Linux-j�rjestelmiss� asennus onnistuu komennolla:
## sudo apt install ffmpeg
## T�m�n j�lkeen voi ajaa R:ss� komennon:
## system("ffmpeg -framerate 12 -i animaatio/ty�markkinat%04d.png -vcodec libx264 -pix_fmt yuv420p -r 25 -strict -2 animaatio.mp4")

## Windows-k�ytt�j�rjestelm��n tarkkoitettu versio FFmpeg-ohjelmistosta l�ytyy t��lt�:
## https://ffmpeg.zeranoe.com/builds/
## ja asennus ohjeet t��lt�:
## https://www.wikihow.com/Install-FFmpeg-on-Windows
## T�m�n j�lkeen voit ajaa komentokehotteessa komennon:
## ffmpeg -framerate 12 -i animaatio\ty�markkinat%04d.png -vcodec libx264 -pix_fmt yuv420p -r 25 -strict -2 animaatio.mp4


## pxweb-paketin avulla voi ottaa yhteytt� my�s lukuisiin muihin rajapintoihin.
## Seuraavilla komennoilla voit p�ivitt�� rajapintojen katalogin ja selata
## eri tietol�hteit�.
update_pxweb_apis()
interactive_pxweb()

