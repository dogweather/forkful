---
date: 2024-01-26 03:45:26.748679-07:00
description: "Avrunding av tall inneb\xE6rer \xE5 justere dem til n\xE6rmeste hele\
  \ tall eller til en spesifisert grad av presisjon. Programmerere gj\xF8r dette for\
  \ \xE5 forbedre\u2026"
lastmod: '2024-03-13T22:44:40.745131-06:00'
model: gpt-4-0125-preview
summary: "Avrunding av tall inneb\xE6rer \xE5 justere dem til n\xE6rmeste hele tall\
  \ eller til en spesifisert grad av presisjon."
title: Avrunding av tall
weight: 13
---

## Hvordan:
I Kotlin kan avrunding gjøres ved hjelp av flere funksjoner som `roundToInt()`, `roundToDouble()`, og ved bruk av `BigDecimal` for mer kontroll:

```kotlin
fun main() {
    val number1 = 3.14159
    println(number1.roundToInt()) // Utskrift: 3

    val number2 = 3.5
    println(number2.roundToInt()) // Utskrift: 4

    val number3 = 123.456
    println("%.2f".format(number3)) // Utskrift: 123.46
    
    val bigDecimal = number3.toBigDecimal().setScale(1, RoundingMode.HALF_EVEN)
    println(bigDecimal) // Utskrift: 123.5
}
```

## Dypdykk
Historisk sett har avrunding av tall vært et grunnleggende konsept i både matematikk og databehandling, designet for å håndtere begrensninger i numerisk presisjon. I tidlig databehandling var avrunding kritisk på grunn av den høye kostnaden for minne.

I Kotlin er avrunding bygget på standard Java-biblioteker. Alternativer for avrunding inkluderer `Math.round()`, som avrunder til det nærmeste hele tallet, og `BigDecimal` for tilpassbar avrunding, der du kan spesifisere en skala og en `RoundingMode`.

Hver `RoundingMode` har ulike policyer for håndtering av uavgjorte situasjoner (når tallet ligger nøyaktig midt mellom alternativene for avrunding). For eksempel, `RoundingMode.HALF_UP` avrunder til nærmeste nabo, med mindre begge naboene er like langt unna, i hvilket tilfelle den avrunder opp.

## Se Også
- Kotlin-dokumentasjon på [`BigDecimal`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/java.math.-big-decimal/index.html)
- Oracles Java-dokumentasjon for [`RoundingMode`](https://docs.oracle.com/javase/8/docs/api/java/math/RoundingMode.html)
- IEEE-standarden for flyttallsaritmetikk (IEEE 754) [IEEE Standard 754](https://ieeexplore.ieee.org/document/4610935)
