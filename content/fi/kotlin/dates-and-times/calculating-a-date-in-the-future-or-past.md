---
date: 2024-01-20 17:31:53.464183-07:00
description: "How to: Miten: Kotlinissa p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen tehd\xE4\
  \xE4n `LocalDate`-luokan avulla. Seuraavilla esimerkeill\xE4 n\xE4ytet\xE4\xE4n,\
  \ kuinka lis\xE4t\xE4 ja v\xE4hent\xE4\xE4\u2026"
lastmod: '2024-04-05T22:38:57.149824-06:00'
model: gpt-4-1106-preview
summary: "Miten: Kotlinissa p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen tehd\xE4\xE4n `LocalDate`-luokan\
  \ avulla. Seuraavilla esimerkeill\xE4 n\xE4ytet\xE4\xE4n, kuinka lis\xE4t\xE4 ja\
  \ v\xE4hent\xE4\xE4 p\xE4iv\xE4m\xE4\xE4ri\xE4."
title: "Tulevan tai menneen p\xE4iv\xE4m\xE4\xE4r\xE4n laskeminen"
weight: 26
---

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
