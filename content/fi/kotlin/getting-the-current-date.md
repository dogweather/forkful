---
title:                "Nykyisen päivämäärän hankkiminen"
date:                  2024-01-20T15:15:13.899445-07:00
html_title:           "Bash: Nykyisen päivämäärän hankkiminen"
simple_title:         "Nykyisen päivämäärän hankkiminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? 
"Päivämäärän haku ja merkitys"
Päivämäärän hakeminen tarkoittaa nykyisen päivän tiedon saamista ohjelmassasi. Sitä käytetään, kun tarvitaan ajanhetken leimausta, käyttäjän tapahtumien aikamerkintöjä tai päivittäisiä tehtäviä.

## How to:
"Kuinka hakea päivämäärä:"
Kotlin tekee päivämäärän hakemisen suoraviivaiseksi. Kokeile näitä esimerkkejä.

```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    println(today) // Tulostaa tämänhetkisen päivämäärän muodossa vvvv-kk-pp
}
```

Koodi antaa tulokseksi esim. `2023-04-01`, jos ajat sen 1. huhtikuuta 2023.

## Deep Dive
"Syväluotaus"
Kotlin käyttää JDK:n `java.time`-kirjastoa päivämäärän haussa - se on moderni Java-kirjasto ajan käsittelyyn, joka tuli standardiksi Java 8 -versiossa. Historiallisesti Java-kehittäjät käyttivät `java.util.Date`- tai `java.util.Calendar`-luokkia, jotka olivat monimutkaisia ja eivät aina kovin intuitiivisia. `java.time`-kirjasto korjasi näitä ongelmia ja tarjoaa paremman API:n ajan käsittelyyn.

Vaihtoehtoinen tapa Kotlinissa on käyttää `java.util.Calendar`-luokkaa:

```Kotlin
import java.util.Calendar

fun main() {
    val today = Calendar.getInstance()
    println("${today.get(Calendar.YEAR)}-${today.get(Calendar.MONTH) + 1}-${today.get(Calendar.DAY_OF_MONTH)}")
    // Huomaa +1 kuukaudelle, koska Calendar-luokka indeksoi kuukaudet alkaen 0.
}
```
Tämä antaa sinulle saman tuloksen, mutta saattaa olla kömpelömpää.

Tietoa ajantarkkuudesta: `LocalDate.now()` antaa päivän tarkkuudella ajan hetken ilman kellonaikaa. Jos tarvitset kellonajan, voit käyttää `LocalDateTime.now()`.

## See Also
"Lisätietoja"
- [Kotlin-dokumentaatio: Basic Types and Operations](https://kotlinlang.org/docs/basic-types.html)
- [Oracle Java-dokumentaatio: Date Time API](https://docs.oracle.com/javase/tutorial/datetime/)
- [Baeldung: Introduction to the Java 8 Date/Time API](https://www.baeldung.com/java-8-date-time-intro)

Kun kaipaat lisää, nuo linkit vievät syvemmälle Kotlinin ja Javan päivämäärä- ja aikakäsittelyyn.