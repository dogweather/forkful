---
title:                "Gleam: Kahden päivämäärän vertailu"
simple_title:         "Kahden päivämäärän vertailu"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Miksi
Useimmat ohjelmat ja sovellukset käsittelevät tietoa, joka liittyy päivämääriin, kuten syntymäpäivät, tapahtumien päivämäärät, ja paljon muuta. On tärkeää, että ohjelmoijat taitavat verrata kahta päivämäärää toisiinsa, jotta he voivat suorittaa oikeita toimintoja ja laskutoimituksia tietojen kanssa. Gleam-kielessä on useita tapoja tehdä tämä ja tämä blogipostaus auttaa sinua ymmärtämään niitä paremmin.

## Kuinka tehdä
Gleam tarjoaa useita hyödyllisiä funktioita päivämäärien vertailuun. Yksi niistä on `Date.isBefore`, joka tarkistaa, onko ensimmäinen päivämäärä ennen toista päivämäärää. Voit käyttää sitä seuraavasti:

```Gleam
use Date

let date1 = Date.from_calendar(1995, 5, 16) // 16. toukokuuta 1995
let date2 = Date.from_calendar(2021, 5, 16) // 16. toukokuuta 2021

Date.isBefore(date1, date2) // palauttaa true
```

Toinen hyödyllinen funktio on `Date.compare`, joka vertaa kahta päivämäärää ja palauttaa negatiivisen luvun, jos ensimmäinen päivämäärä on ennen toista, nollan, jos päivämäärät ovat samat, ja positiivisen luvun, jos ensimmäinen päivämäärä on jälkeen toista. Voit käyttää sitä seuraavasti:

```Gleam
use Date

let date1 = Date.from_calendar(1985, 11, 24) // 24. marraskuuta 1985
let date2 = Date.from_calendar(1986, 1, 1) // 1. tammikuuta 1986

Date.compare(date1, date2) // palauttaa -1
```

Voit myös käyttää `Date.diff`, joka laskee päivämäärien välisen päivien määrän. Voit käyttää sitä seuraavasti:

```Gleam
use Date

let date1 = Date.from_calendar(2021, 6, 1) // 1. kesäkuuta 2021
let date2 = Date.from_calendar(2021, 6, 15) // 15. kesäkuuta 2021

Date.diff(date1, date2) // palauttaa 14
```

## Syvenevä sukellus
Gleam-kielessä päivämäärän vertailu tehdään sisäisesti `Calendar.Comparable`-protokollan avulla. Tämä protokolla määrittelee, miten päivämääriä vertaillaan ja se voi olla erilainen eri kielissä. Siksi on aina tärkeää tarkistaa dokumentaatiosta, miten haluamasi kielialue käsittelee päivämäärän vertailua. 

Toinen tärkeä huomio on, että päivämääriä ei voi vertailla suoraan käyttämällä `==` tai `!=` operaattoreita. Sen sijaan on aina käytettävä Gleamin tarjoamia `Date.isEqual` tai `Date.isNotEqual` -funktioita.

## Katso myös
- Gleam-kielessä on muitakin tärkeitä funktioita päivämäärän käsittelyyn, tutustu niihin Gleamin dokumentaatiosta: https://gleam.run/libraries/calendar
- Lisätietoja `Calendar.Comparable`-protokollasta löydät täältä: https://hexdocs.pm/calendar/Calendar.Comparable.html