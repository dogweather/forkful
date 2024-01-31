---
title:                "Kahden päivämäärän vertailu"
date:                  2024-01-20T17:32:56.677975-07:00
model:                 gpt-4-1106-preview
simple_title:         "Kahden päivämäärän vertailu"

category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
(Mitä & Miksi?)

Vertaamme kahta päivämäärää tunnistaaksemme niiden eron. Ohjelmoijat tekevät tämän aikataulujen hallintaan, aikaleimojen tarkkailuun tai vanhentumispäivien tsekkaamiseen.

## How to:
(Miten:)

```Fish Shell
# Asenna 'dateutils' työkalut
sudo apt install dateutils

# Vertaa kahta päivämäärää käyttäen dateutils-diff
set date1 (date -ud '2023-03-01' +%s)
set date2 (date -ud '2023-04-01' +%s)
dateutils.ddiff $date1 $date2 -f '%ddays'

# Tulostettava tulos
31days
```

## Deep Dive
(Syväsukellus)

Alun perin päivämäärien vertailussa käytettiin yksinkertaisia Unix-ajanhetkiä (epoch), jotka ilmaisevat aikaa sekunteina vuoden 1970 alusta. `dateutils` on uuden polven työkalu, joka helpottaa aikaleimojen vertailua. Se käsittää useita käskyjä, kuten `dateutils.ddiff` päivämääräerojen laskemiseen. Fish Shell sopii hyvin yhteen tämänkaltaisten apuohjelmien kanssa, koska sen syntaksi on yksinkertainen ja helposti laajennettavissa. Vaihtoehtoisesti voit käyttää myös Fishin sisäänrakennettuja aikafunktioita pienemmissä skripteissä.

## See Also
(Katso Myös)

- Fish Shell dokumentaatio: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- `dateutils` dokumentaatio: [http://www.fresse.org/dateutils/](http://www.fresse.org/dateutils/)
- Unix-ajanhetkiä käsittelevä artikkeli: [https://en.wikipedia.org/wiki/Unix_time](https://en.wikipedia.org/wiki/Unix_time)
