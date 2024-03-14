---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:38.057890-07:00
description: "Het afronden van getallen betekent dat je ze aanpast naar het dichtstbijzijnde\
  \ hele getal of naar een gespecificeerd niveau van precisie. Programmeurs\u2026"
lastmod: '2024-03-13T22:44:50.762481-06:00'
model: gpt-4-0125-preview
summary: "Het afronden van getallen betekent dat je ze aanpast naar het dichtstbijzijnde\
  \ hele getal of naar een gespecificeerd niveau van precisie. Programmeurs\u2026"
title: Afronden van getallen
---

{{< edit_this_page >}}

## Wat & Waarom?

Het afronden van getallen betekent dat je ze aanpast naar het dichtstbijzijnde hele getal of naar een gespecificeerd niveau van precisie. Programmeurs doen dit om de leesbaarheid te verbeteren, de opslagvereisten te verlagen, of omdat de exacte waarde niet cruciaal is voor latere berekeningen.

## Hoe:

In Kotlin kan afronden worden gedaan met verschillende functies zoals `roundToInt()`, `roundToDouble()`, en het gebruik van `BigDecimal` voor meer controle:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Resultaat: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Resultaat: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Resultaat: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Resultaat: 123.5
}
```

## Diep Duiken

Historisch gezien is het afronden van getallen een fundamenteel concept geweest in zowel wiskunde als informatica, ontworpen om te gaan met beperkingen in numerieke precisie. In de vroege computertechnologie was afronden cruciaal vanwege de hoge kosten van geheugen.

In Kotlin is afronden gebaseerd op de standaard Java-bibliotheken. Opties voor afronden omvatten `Math.round()`, dat afrondt naar het dichtstbijzijnde hele getal, en `BigDecimal` voor aanpasbaar afronden, waar je een schaal en een `RoundingMode` kunt specificeren.

Elke `RoundingMode` heeft verschillende beleidsregels voor het omgaan met gelijke standen (wanneer het cijfer precies in het midden van de opties voor afronden staat). Bijvoorbeeld, `RoundingMode.HALF_UP` rondt af naar de dichtstbijzijnde buur, tenzij beide buren op gelijke afstand zijn, in welk geval het naar boven afrondt.

## Zie Ook

- Kotlin Documentatie over [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracle's Java Documentatie voor [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE Standaard voor Drijvende-komma Rekenkunde (IEEE 754) [IEEE Standaard 754](https://ieeexplore.ieee.org/document/4610935)
