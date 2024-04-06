---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:01:58.233934-07:00
description: "Hoe te: Kotlin, be\xEFnvloed door andere moderne talen, introduceerde\
  \ stringinterpolatie als een schonere alternatief voor Java's string samenvoeging.\
  \ Het\u2026"
lastmod: '2024-04-05T21:53:50.776220-06:00'
model: gpt-4-0125-preview
summary: "Kotlin, be\xEFnvloed door andere moderne talen, introduceerde stringinterpolatie\
  \ als een schonere alternatief voor Java's string samenvoeging."
title: Een string interpoleren
weight: 8
---

## Hoe te:
```kotlin
fun main() {
    val name = "Alex"
    val age = 29
    // Variabelen in de string interpoleren
    val begroeting = "Hallo, mijn naam is $name en ik ben $age jaar oud."
    println(begroeting) // Uitvoer: Hallo, mijn naam is Alex en ik ben 29 jaar oud.

    // Expressies binnen strings
    de aankondiging = "Volgend jaar zal ik ${age + 1} zijn!"
    println(aankondiging) // Uitvoer: Volgend jaar zal ik 30 zijn!
}
```

## Diepgaande duik
Kotlin, beïnvloed door andere moderne talen, introduceerde stringinterpolatie als een schonere alternatief voor Java's string samenvoeging. Het verbetert de leesbaarheid en vereenvoudigt de code.

Historisch gezien vereiste Java uitgebreide samenvoeging met `+`, wat zowel moeilijk te lezen als minder efficiënt kon zijn, aangezien het meerdere stringobjecten creëerde. Kotlin's aanpak is krachtiger, waardoor niet alleen het inbedden van variabelen mogelijk is, maar ook de evaluatie van expressies binnen strings.

Onder de kap compileert Kotlin deze interpolatie naar `StringBuilder`-bewerkingen of string samenvoeging, afhankelijk van de complexiteit, en neemt zo de last van de ontwikkelaar weg.

Alternatieven voor stringinterpolatie zijn onder meer templating engines voor uitgebreide tekstmanipulatie, maar in code is interpolatie over het algemeen de snelste manier om dynamische inhoud in te sluiten.

## Zie ook
- [Kotlin Documentatie over String Templates](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Kotlin's `String` API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Vergelijken van Java en Kotlin String samenvoegingsprestaties](https://proandroiddev.com/the-cost-of-kotlin-language-features-8f7035e9dcb9)
