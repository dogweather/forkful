---
title:                "Nykyisen päivämäärän hankkiminen"
html_title:           "Haskell: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Kotlinilla päivämäärän haku. Käymme läpi mitä se on ja miksi se on tärkeää

## Mikä & Miksi?

Päivämäärän haku viittaa nykyisen päivämäärän saamiseen ohjelmoijien tarpeisiin. Sitä tarvitaan ajanilmaisujen, lokielementtien ja aikasidonnaisten tietojen käsittelyyn. 

## Miten se tehdään:

```Kotlin
import java.time.LocalDate

fun main() {
    val currentDate = LocalDate.now()
    println("Nykyinen päivämäärä on: $currentDate")
}
````

Suoritettaessa yllä olevan koodin tuloste on:

```Kotlin
Nykyinen päivämäärä on: YYYY-MM-DD (esimerkiksi 2023-12-12)
```

## Syvemmälle:

Historiallisesti päivämäärän hakeminen on ollut osa ohjelmointikielien ydintoimintoja. Kotlinissa, joka on nykyversiona 1.5.30 (kirjoitushetkellä), päivämäärän haku on integroitu suoraan `java.time.LocalDate` -luokkaan.

Vaihtoehtoisesti voidaan käyttää `java.util.Date`, mutta se on vanhentunut ja siinä on joitakin ongelmia, kuten kuukausien indeksointi aloittaminen nollasta.

Nykyisen päivämäärän saaminen Kotlinissa on varsin suoraviivaista. Yllä esitetyn esimerkin `LocalDate.now()` -metodi palauttaa nykyisen päivämäärän.

## Katso myös:

1. Kotlinin virallinen dokumentaatio [`java.time.LocalDate`](https://kotlinlang.org/api/latest/jvm/stdlib/java.time/-local-date/index.html)
2. Stack Overflow -keskustelu nykyisen päivämäärän saamisesta [`LocalDate`](https://stackoverflow.com/questions/66907223/how-to-get-current-date-in-kotlin)
3. Verkkosivusto, josta tarjoaa monia esimerkkejä päivämäärän ja ajan kanssa työskentelystä Kotlinissa: [Baeldung](https://www.baeldung.com/kotlin-dates)