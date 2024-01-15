---
title:                "Tulevaisuuden tai menneen päivän laskeminen"
html_title:           "Gleam: Tulevaisuuden tai menneen päivän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivän laskeminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi: Päivämäärän laskeminen tulevaisuutta tai menneisyyttä varten
Monet sovellukset ja ohjelmistot vaativat päivämäärän laskemista tulevaisuuteen tai menneisyyteen liittyvissä toiminnoissaan. Esimerkiksi kalenterisovellus voi näyttää jäljellä olevien päivien määrän tärkeään tapahtumaan tai tehtävänmääräpäivään. Gleamin avulla tämä prosessi voidaan tehdä helposti ja tarkasti.

## Miten: Koodiesimerkkejä ja tulosteenäytteitä
```Gleam
import Gleam.Date

let tulevaPäivä = Date.addDays(Date.today(), 30) // Lisätään 30 päivää nykyiseen päivään

let menneiTapahtuma = Date.subtractMonths(Date.today(), 3) // Vähennetään 3 kuukautta nykyisestä päivästä

let päivämäärä = Date.fromString("2022-01-01") // Muunnetaan merkkijono päivämääriksi

let ero = Date.diff(Date.fromString("2022-01-01"), Date.fromString("2021-10-15")) // Lasketaan aikaeron kahden päivämäärän välillä

let onkoTuleva = Date.isAfter(Date.addDays(Date.today(), 30), Date.today()) // Tarkistetaan, onko tuleva päivä myöhemmin kuin nykyinen päivä

let viikonPäivä = Date.getDayName(Date.today()) // Palauttaa nykyisen viikonpäivän nimen

let kuukaudenPäivä = Date.getDayOfMonth(Date.today()) // Palauttaa nykyisen kuukauden päivän numeron

io.print("21. huhtikuuta vuonna 2021 on ", Date.getDayOfWeek(Date.fromString("2021-04-21")), ".") // Tulostaa tiedon, onko kyseinen päivä viikonloppu

```

## Syvempää tietoa: Päivämäärälaskennan taustaa
Gleamin Date-moduuli tarjoaa monipuoliset työkalut päivämäärän laskentaan tulevaisuutta tai menneisyyttä varten. Moduuli sisältää erilaisia ​​toimintoja, kuten päivien, kuukausien ja vuosien lisäämistä ja vähentämistä, päivämäärien muuntamista merkkijonoista, päivämäärien välisten aikaerojen laskemista ja viikonpäivien sekä kuukauden päivämäärien palauttamista. Näitä toimintoja voidaan hyödyntää monissa erilaisissa sovelluksissa ja ne mahdollistavat tarkat ja joustavat päivämääräkäsittelyt.

## Katso myös
- [Gleamin virallinen dokumentaatio](https://gleam.run/documentation/)
- [Gleam Date -moduulin tiedostokohtainen lähtö](https://github.com/gleam-lang/date/blob/master/src/gleam/calendar.gleam)