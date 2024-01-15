---
title:                "Kahden päivämäärän vertailu"
html_title:           "Gleam: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi

Tässä artikkelissa opimme, miten vertaillaan kahta eri päivämäärää Gleam-ohjelmoinnissa. Tämä taito on hyödyllinen, kun haluamme esimerkiksi laskea päiviä kahden päivämäärän välillä tai tarkistaa, kumpi päivämäärä on myöhempi.

## Miten

Vertaillaan ensin kahden päivämäärän eroa. Käytämme Gleam-ohjelmoinnissa `Date.diff()` -funktiota, jolle annamme kaksi päivämäärää parametreina. Tämän jälkeen voimme käyttää `Date.days()` -funktiota saadaksemme eron päivissä.

```Gleam
import gleam/date.{days, diff}

let date1 = Date.new(2021, 10, 25)
let date2 = Date.new(2021, 10, 29)

let difference = diff(date1, date2)
let days_between = days(difference)
// Tulostaa: 4
```

Nyt voimme myös tarkistaa, kumpi päivämäärä on myöhempi käyttämällä `Date.compare()` -funktiota. Funktiolle annetaan kaksi päivämäärää parametreina ja se palauttaa `Ordering` -tyypin, joka voi olla `Lt` (ensimmäinen päivämäärä on ennen), `Eq` (päivämäärät ovat samat) tai `Gt` (ensimmäinen päivämäärä on jälkeen).

```Gleam
import gleam/date.{compare}

let date1 = Date.new(2021, 10, 25)
let date2 = Date.new(2021, 10, 29)

let order = compare(date1, date2)
// Tulostaa: Gt
```

## Syvempi sukellus

Gleamilla on myös `DateTime` -tyyppi, joka sisältää myös ajan tiedot. Voimme verrata kahta `DateTime` -tyyppiä käyttämällä `DateTime.diff()` ja `DateTime.compare()` -funktioita samalla tavalla kuin `Date` -tyyppien kanssa.

On myös tärkeää huomata, että Gleam käyttää Gregoriaanista kalenteria, joten päivämäärät ennen vuotta 1582 eivät toimi oikein. Tämä ei kuitenkaan ole ongelma useimmissa tapauksissa.

## Katso myös

- [Gleamin virallinen dokumentaatio](https://gleam.run/)
- [Gleam eli funktionaalinen ja mobiiliohjelmointi kokoelman hallinnassa](https://fi.wikipedia.org/wiki/Gleam)