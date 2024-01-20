---
title:                "Kirjoittaminen vakiovirheeseen"
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Standard error on ihmisen ja ohjelman välinen kommunikaatioreitti virheviesteille. Ohjelmoijat käyttävät sitä raportoimaan ongelmia, jolloin normaali toiminta ja virheviestit ovat helposti eroteltavissa.

## How to:
Kirjoita virhe stderr:iin näin:

```Fish Shell
echo "Tämä on virheilmoitus" >&2
```

Jos haluat ohjata virheet tiedostoon:

```Fish Shell
echo "Tallennetaan virhe" >&2 2> error_log.txt
```

Kokeile ja näet:

```Fish Shell
function oletus
    echo "Normaali tulostus"
    echo "Todellinen virhe" >&2
end

oletus 2> virheet.txt
```

`virheet.txt` sisältää nyt "Todellinen virhe".

## Deep Dive
Stderr juontaa juurensa Unix-järjestelmistä ja on peruskomponentti ohjelman ja käyttöjärjestelmän välisessä kommunikaatiossa. Alternatiivit kuten logitiedostot ovat hyviä pitkäaikaista tallennusta varten. Fish käsittää stderrin omana tiedostovirtanaan, jonka numero on 2.

## See Also
- Fish Shell dokumentaatio: https://fishshell.com/docs/current/index.html
- Unix standardivirran historia: https://en.wikipedia.org/wiki/Standard_streams
- Opas virheenkäsittelyyn shell-skripteissä: https://mywiki.wooledge.org/BashFAQ/105