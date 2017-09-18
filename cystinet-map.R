### CYSTINET MAP
### http://projects.cbra.be/cystinet/

## required packages
library(bd)
library(readxl)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(magrittr)

## read data
x <- read_excel("labs.xlsx", 1)

## sanitize special characters
x$LAB <- sanitize_specials(x$LAB, "html")
x$STREET <- sanitize_specials(x$STREET, "html")
x$CITY <- sanitize_specials(x$CITY, "html")
x$CONTACT <- sanitize_specials(x$CONTACT, "html")

## define control structure
control <-
  paste(sep = "\n",
        "<div align='center' style='padding:10px; font-size:18px; color:#333; font-style:oblique;'>Taeniosis/Cysticercosis <br> Diagnostic Labs</div>",
        #"<div align='center' style='padding:10px; font-style:oblique;'>~ brought to you by ~</div>",
        "<a href='http://www.cystinet.org/'><img src='http://projects.cbra.be/cystinet/img/Cystinet_logo.png' width=255></a>",
        "<a href='http://www.cystinet.org/'><div align='center' style='font-size:8px; background-color:#383838; color:#fff; margin-top:-5px;'><b>EUROPEAN NETWORK ON TAENIOSIS/CYSTICERCOSIS<b></div></a>")

address <-
function(x) {
  paste0(x[["STREET"]], ", ",
         x[["ZIP"]], " ", x[["CITY"]], ", ",
         toupper(x[["COUNTRY"]]))
}

lab <-
function(x) {
  if (is.na(x["URL"])) {
    x["LAB"]

  } else {
    paste0("<a href='", x["URL"], "'>", x["LAB"], "</a>")
  }
}

name <-
function(x) {
  if (is.na(x["EMAIL"])) {
    x["CONTACT"]

  } else {
    paste0("&#9993; <a href='mailto:", x["EMAIL"], "'>", x["CONTACT"], "</a>")
  }
}

popup <-
function(x) {
  paste(sep = "\n",
        paste0("<div align='center' style='font-weight:bold; font-size:14px;'>", lab(x), "</div>"),
        paste0("<div align='center' style='margin-top:10px;'>", address(x), "</div>"),
        paste0("<div align='center' style='margin-bottom:10px;'>", name(x), " | &#9742; ", x["TEL"], "</div>"),
        paste0("<hr style='border:0; height:1px; background:#333; background-image:linear-gradient(to right,#ccc,#333,#ccc);'>"),
        paste0("<div align='left' style='margin:5px; margin-top:10px;'>",
               "<b>Available assays</b>",
               "<ul style='list-style-position:inside; padding:0;margin-top:0'>",
               ifelse(x["TSOL_CC_AB"]=="yes", "<li><em>Taenia solium</em> (neuro)cysticercosis &ndash; antibody</li>", ""),
               ifelse(x["TSOL_CC_AG"]=="yes", "<li><em>Taenia solium</em> (neuro)cysticercosis &ndash; antigen</li>", ""),
               ifelse(x["TSOL_HT"]=="yes", "<li><em>Taenia solium</em> taeniosis</li>", ""),
               ifelse(x["TSAG_HT"]=="yes", "<li><em>Taenia saginata</em> taeniosis</li>", ""),
               "</ul>",
               "</div>")
  )
}

mapIcons <- icons(
  iconUrl = ifelse(x[["CYSTINET"]]=="yes",
    "http://projects.cbra.be/cystinet/img/marker_green.png",
    "http://projects.cbra.be/cystinet/img/marker_blue.png"
  ),
  iconWidth = 50, iconHeight = 50,
  iconAnchorY = 50, iconAnchorX = 25,
  popupAnchorY = -40, popupAnchorX = 0.01
)

lgnd <-
  ifelse(x[["CYSTINET"]] == "yes",
         "CYSTINET member",
         "non CYSTINET member")

map <-
  leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(x$LONG,
             x$LAT,
             icon = mapIcons,
             popup = apply(x, 1, popup),
             clusterOptions = markerClusterOptions()) %>%
  addControl(control, "topright") %>%
  addLegend("topright",
            pal = colorFactor(c("#B9EA62", "#97E9FC"),
                              unique(lgnd)),
            values = lgnd,
            opacity = 1) %>%
  setView(15, 54, 4)

## save map
saveWidget(widget = map,
           file = "cystinet-map.html",
           selfcontained = TRUE)

