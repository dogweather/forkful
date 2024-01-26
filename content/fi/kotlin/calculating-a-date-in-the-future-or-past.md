---
title:                "Tulevan tai menneen päivämäärän laskeminen"
date:                  2024-01-20T17:31:53.464183-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tulevan tai menneen päivämäärän laskeminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Mitä ja miksi? Lasketaan tulevaisuuden tai menneisyyden päivämääriä, koska elämme kalentereissa ja aikatauluissa. Ohjelmoijina tarvitsemme tätä toimintoa määrittämään eräpäiviä, muistutuksia tai aikavälejä.

## How to:
Miten:

Kotlinissa päivämäärän laskeminen tehdään `LocalDate`-luokan avulla. Seuraavilla esimerkeillä näytetään, kuinka lisätä ja vähentää päivämääriä.

```Kotlin
import java.time.LocalDate

fun main() {
    val today = LocalDate.now()
    val plusTenDays = today.plusDays(10)
    val minusOneMonth = today.minusMonths(1)

    println("Tänään: $today")
    println("Kymmenen päivää tästä eteenpäin: $plusTenDays")
    println("Yksi kuukausi taaksepäin: $minusOneMonth")
}
```

Kun tämä koodi suoritetaan, tuloste näyttää jotakin seuraavanlaista (riippuen suorituspäivästä):

```
Tänään: 2023-04-12
Kymmenen päivää tästä eteenpäin: 2023-04-22
Yksi kuukausi taaksepäin: 2023-03-12
```

## Deep Dive:
Syväsukellus:

Päivämääriä on laskettu ohjelmallisesti alusta asti. Aikaisemmin oli `java.util.Date`, mutta ongelmat ajovyöhykkeiden ja muotoilun kanssa loivat tarpeen paremmalle API:lle. `java.time` (Joda-Time pohjalta) esiteltiin Java 8:ssa vuonna 2014.

Kotlinissa `java.time` toimii saumattomasti, ja se on suositeltu tapa työskennellä päivämäärien kanssa. Vaihtoehtoisesti voi käyttää vanhempia luokkia tai kolmannen osapuolen kirjastoja, mutta `java.time` on moderni ja yleisesti suosittu.

Päivämäärien laskeminen on yksinkertaista: voit lisätä tai poistaa päiviä, viikkoja, kuukausia tai vuosia. Täytyy kuitenkin olla tietoinen aikavyöhykkeistä ja karkausvuosista, jotka `LocalDate` huomioi automaattisesti.

## See Also:
Katso myös:

- [Kotlinin dokumentaatio](https://kotlinlang.org/docs/home.html)
- [java.time -paketin dokumentaatio](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Joda-Time-kirjasto](https://www.joda.org/joda-time/)

Muista jatkaa oppimista ja kokeilemista - se on paras tapa mestaroida päivämäärien käsittely Kotlinissa!
