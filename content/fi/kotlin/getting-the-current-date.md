---
title:                "Kotlin: Hanki nykyinen päivämäärä"
simple_title:         "Hanki nykyinen päivämäärä"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit saada nykyisen päivämäärän ohjelmassa? Päivämäärää ja aikaa käytetään usein ohjelmoinnissa erilaisiin tarkoituksiin, kuten aikaleimojen tallentamiseen, tapahtumien seuraamiseen ja tapahtumien ajoittamiseen. On tärkeää pystyä hallitsemaan päivämäärää ja aikaa, jotta ohjelmat toimivat oikein.

## Miten

Kotlin tarjoaa helpon tavan saada nykyinen päivämäärä koodissa. Voit käyttää `LocalDate` -luokkaa saadaksesi nykyisen päivämäärän Java 8: n `java.time` -kirjastosta.

```Kotlin
import java.time.LocalDate

val today = LocalDate.now()
println(today)
```

Tämä koodinpätkä tuottaa seuraavan tulosteen:

```text
2021-10-05
```

Voit myös asettaa halutun aikavyöhykkeen käyttämällä `ZoneId` -luokkaa ja `ZonedDateTime` -luokkaa.

```Kotlin
import java.time.LocalDate
import java.time.ZoneId
import java.time.ZonedDateTime

val zoneId = ZoneId.of("Europe/Helsinki")
val today = ZonedDateTime.now(zoneId)
println(today)
```

Tämä tuottaa seuraavan tulosteen:

```text
2021-10-05T09:45:00.953379300+03:00[Europe/Helsinki]
```

## Syväkatsaus

Päivämäärän ja ajan käsittely on tärkeä osa ohjelmointia. Kotlinin `LocalDate` -luokka tarjoaa helpon tavan käsitellä ja hallita päivämääriä. Voit myös käyttää lisäkirjastoja, kuten `java.time.zone` tai `java.time.chrono`, saadaksesi lisätoimintoja ja tuen eri aikavyöhykkeille ja kalentereille.

## Katso myös

- [Kotlinin virallinen dokumentaatio](https://kotlinlang.org/docs/)
- [Java 8 `java.time` -kirjasto](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java 8 `java.time.zone` -kirjasto](https://docs.oracle.com/javase/8/docs/api/java/time/zone/package-summary.html)
- [Java 8 `java.time.chrono` -kirjasto](https://docs.oracle.com/javase/8/docs/api/java/time/chrono/package-summary.html)