---
date: 2024-01-20 17:34:59.845226-07:00
description: "Kuinka: Historiallisesti, stringien yhdist\xE4minen oli hitaampaa ja\
  \ tehotonta suurilla stringim\xE4\xE4rill\xE4, koska se luo uusia string-olioita\
  \ muistiin.\u2026"
lastmod: '2024-04-05T21:53:58.089960-06:00'
model: gpt-4-1106-preview
summary: "Historiallisesti, stringien yhdist\xE4minen oli hitaampaa ja tehotonta suurilla\
  \ stringim\xE4\xE4rill\xE4, koska se luo uusia string-olioita muistiin."
title: "Merkkijonojen yhdist\xE4minen"
weight: 3
---

## Kuinka:
```Kotlin
fun main() {
    val tervehdys = "Hei"
    val maailma = "maailma"
    val yhdista = tervehdys + " " + maailma + "!"
    println(yhdista) // Tulostuu: Hei maailma!
    
    // String template -käyttö
    val tervetuloa = "Tervetuloa"
    val kotlin = "Kotlin"
    println("$tervetuloa, $kotlin!") // Tulostuu: Tervetuloa, Kotlin!
    
    // buildString-funktio isommille yhdistelyille
    val kokonaisuus = buildString {
        append(tervehdys)
        append(" ")
        append(maailma)
        append("!")
    }
    println(kokonaisuus) // Tulostuu: Hei maailma!
}
```

## Syväsukellus
Historiallisesti, stringien yhdistäminen oli hitaampaa ja tehotonta suurilla stringimäärillä, koska se luo uusia string-olioita muistiin. Vaihtoehtoina, voi käyttää `StringBuilder`- tai `StringBuffer`-luokkia muistitehokkaampaan konkatenointiin. Kotlinin `buildString` ja templated strings (`$`) ovat käteviä ja tehokkaita työkaluja, jotka piilottavat monimutkaisuutta ja parantavat koodin luettavuutta.

## Lisätietoja
- Kotlinin dokumentaatio stringien yhdistämisestä: [Kotlin Strings](https://kotlinlang.org/docs/basic-types.html#strings)
- Tehokas tekstin käsittely `StringBuilder`-luokalla: [Kotlin StringBuilder](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string-builder/)
- Kotlinin virallinen ohjelmointiopas: [Kotlin Programming Language](https://kotlinlang.org/docs/reference/)
