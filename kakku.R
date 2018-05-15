## Title: Kakkuanimaatio
## Author: Juha Itkonen
## Version: 2018-05-15
library(tidyverse)
library(magrittr)
library(tweenr)
library(pxweb)
library(png)

data <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/kan/vtp/statfin_vtp_pxt_019.px",
             dims = list(Taloustoimi = c('*'),
                         Vuosi = c('*')),
             clean = TRUE) %>% as_tibble

data %>% distinct(Taloustoimi)

x <- data %>%
    filter(str_detect(Taloustoimi, "B1GMHT/CAP")) %$%
    values
x <- x/max(x)

dir.create("kakkukuva")
download.file("https://emojipedia-us.s3.amazonaws.com/thumbs/160/facebook/92/birthday-cake_1f382.png",
              "cake.png")
img <- readPNG("cake.png")
a <- 0.5 - sqrt(x)/2
b <- 0.5 + sqrt(x)/2
index <- c(rep(1,10),
           seq_along(x),
           rep(length(x),20))

for(j in seq_along(index)) {
    i <- index[j]
    png(filename =
            paste0("kakkukuva/cake", sprintf("%04d", j), ".png"))
    plot.new()
    text(0.5,0.0,i+1859)
    text(0.5,1,"Bruttokansantuote henkeä kohti")
    mtext("Lähde: Tilastokeskus, Kansantalouden tilinpito, Historiasarjat.",1,col="gray")
    rasterImage(img,a[i],a[i],b[i],b[i])
    dev.off()
}

system("ffmpeg -framerate 10 -i kakkukuva/cake%04d.png -vcodec libx264 -pix_fmt yuv420p -r 25 -strict -2 kakku.mp4")

#####################################

data2 <- 
  get_pxweb_data(url = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tul/tjt/statfin_tjt_pxt_014.px",
             dims = list(Tulokymmenys = c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'),
                         Vuosi = c('*'),
                         Tulokäsite = c('SL2'),
                         Tiedot = c('Keskiarvo')),
             clean = TRUE) %>%
    as_tibble %>%
  mutate(Vuosi = parse_integer(Vuosi)) %>%
  mutate(Tulokymmenys = factor(Tulokymmenys, unique(Tulokymmenys)))
  
dt <- 
    data2 %>%
    select(Tulokymmenys,Vuosi,values) %>%
    mutate(ease = "linear") %>%
    mutate(Tulokymmenys =
               as.numeric(Tulokymmenys)) %>%
    tween_elements(time = "Vuosi",
                   group = "Tulokymmenys",
                   ease = "ease",
                   timerange = 1966:2016,
                   nframes = 1) %>%
    as_tibble %>%
    mutate(size = values/max(values))

download.file("https://upload.wikimedia.org/wikipedia/commons/thumb/a/a0/Emojione_1F370.svg/512px-Emojione_1F370.svg.png",
              "cakepiece.png")
img <- readPNG("cakepiece.png")

index <- c(rep(1,5),
           seq_along(unique(dt$.frame)),
           rep(length(unique(dt$.frame)),10)) -1

for(k in seq_along(index)) {
    f <- index[k]
    png(filename =
            paste0("kakunjako/cake", sprintf("%04d", k), ".png"),
        width = 640, height = 640)
    plot.new()
    d <- dt %>%
        filter(.frame == f) %$% sort(size)
    p <- matrix(sqrt(d)/10, 5)
    y <- matrix(rep(1:5/5-0.1, 2), 5)
    x <- matrix(rep(c(0.3, 0.7), 5), 5, byrow = T)
    for(j in 1:2) {
        for(i in 1:5) {
            rasterImage(img,
                        x[i,j]-p[i,j],
                        y[i,j]-p[i,j],
                        x[i,j]+p[i,j],
                        y[i,j]+p[i,j])
        }
    }
    mtext("Käytettävissä olevat tulot tulokymmenyksittäin\n(ml. asuntotulo ja myyntivoitot)",3, cex = 1.5)
    text(0.5,0.0,round(f+1966), cex = 1.5)
    mtext("Lähde: Tilastokeskus, Tulonjakotilasto.",1,col="gray")
    dev.off()
}

system("ffmpeg -framerate 5 -i kakunjako/cake%04d.png -vcodec libx264 -pix_fmt yuv420p -r 25 -strict -2 -y kakunjako.mp4")

system("ffmpeg -y -i kakunjako.mp4 -vf fps=10,scale=320:-1:flags=lanczos,palettegen palette.png")

system("ffmpeg -y -i kakunjako.mp4 -i palette.png -filter_complex 'fps=12,scale=640:-1:flags=lanczos[x];[x][1:v]paletteuse' kakunjako.gif")


