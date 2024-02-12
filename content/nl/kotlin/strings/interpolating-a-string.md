---
title:                "Een string interpoleren"
aliases:
- /nl/kotlin/interpolating-a-string/
date:                  2024-01-28T22:01:58.233934-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Stringinterpolatie stelt je in staat om variabelen direct in strings in te sluiten. Het is handig voor het creëren van dynamische, leesbare tekst zonder onhandige samenvoeging.

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
