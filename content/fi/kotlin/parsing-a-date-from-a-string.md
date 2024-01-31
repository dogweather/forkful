---
title:                "Merkkijonosta päivämäärän jäsentäminen"
date:                  2024-01-20T15:37:03.742990-07:00
html_title:           "Bash: Merkkijonosta päivämäärän jäsentäminen"
simple_title:         "Merkkijonosta päivämäärän jäsentäminen"

category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Päivämäärän jäsentäminen merkkijonosta muuttaa tekstimuodossa olevan päivämäärän ohjelmointikielessämme käsiteltäväksi tiedoksi. Tämä on tarpeen, sillä ohjelmat joutuvat käsittelemään päivämääriä monenlaisissa ympäristöissä, kuten käyttöliittymissä, tietokannoissa ja API-kutsuissa.

## How to: (Kuinka tehdään:)
Kotlinissa päivämäärän jäsentämiseen käytetään `java.time`-kirjastoa. Tässä on esimerkki:

```Kotlin
import java.time.LocalDate
import java.time.format.DateTimeFormatter

fun main() {
    val dateAsString = "2023-04-01"
    val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
    val parsedDate = LocalDate.parse(dateAsString, formatter)

    println("Päivämäärä: $parsedDate")
}
```

Tuloste:
```
Päivämäärä: 2023-04-01
```

## Deep Dive (Syväsukellus)
Jäsenneissä, kuten `SimpleDateFormat` Java-klassikossa, ajan jäsentäminen meni oikein, mutta se ei ollut turvallinen säikeille ja aiheutti päivämäärien virheellisiä esityksiä. Java 8:n myötä esiteltiin `java.time`, joka on säieturvallinen ja paremmin suunniteltu. Kotlin hyödyntää näitä luokkia päivämääräkäsittelyssä.

Vaihtoehtona on käyttää kirjastoja, kuten Joda-Time, mutta `java.time` on nykyään suositeltava tapa. Kun tietomuotoja jäsentää, on tärkeä ymmärtää kohdekulttuurin päivämääräformaatit.

Javan `DateTimeFormatter` käsittelee jäsentämisen ja muotoilun, ja se tukee laajalti erilaisia muotoja. Määritä ensin muoto `ofPattern`-metodilla ja välitä se sitten `parse`-funktiolle merkkijonon kera.

## See Also (Katso myös)
- `java.time`-paketin [dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [DateTimeFormatter](https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html)
- [Kotlin-dokumentaatio](https://kotlinlang.org/docs/reference/)
- Aika- ja päivämääräkäsittelyyn liittyvät artikkelit Stack Overflow'ssa: [Aika- ja päivämääräkäsittely](https://stackoverflow.com/questions/tagged/date+time+kotlin)
