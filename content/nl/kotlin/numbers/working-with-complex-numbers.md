---
title:                "Werken met complexe getallen"
aliases: - /nl/kotlin/working-with-complex-numbers.md
date:                  2024-01-28T22:12:10.722410-07:00
model:                 gpt-4-0125-preview
simple_title:         "Werken met complexe getallen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/working-with-complex-numbers.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Complexe getallen breiden ons getallenstelsel uit met de vierkantswortels van negatieve getallen, waarbij de 'imaginaire' eenheid i gelijk staat aan de vierkantswortel van -1. Programmeurs gebruiken ze in domeinen zoals techniek, natuurkunde en signaalverwerking, omdat ze geweldig zijn voor het modelleren van golven, oscillaties en alles dat roteert.

## Hoe te:

Laten we een eenvoudige complexe getallenklasse in Kotlin definiëren:

```kotlin
data class Complex(val real: Double, val imaginary: Double) {
    operator fun plus(other: Complex) = Complex(real + other.real, imaginary + other.imaginary)
    operator fun minus(other: Complex) = Complex(real - other.real, imaginary - other.imaginary)
    operator fun times(other: Complex) = Complex(
        real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real
    )
    
    override fun toString(): String = "($real + ${imaginary}i)"
}

fun main() {
    val a = Complex(1.0, 2.0)
    val b = Complex(3.0, 4.0)
    
    println("a + b = ${a + b}")  // Uitvoer: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Uitvoer: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Uitvoer: a * b = (-5.0 + 10.0i)
}
```

## Diepgaande Duik

Complexe getallen werden voor het eerst genoemd in de 16e eeuw, bij het oplossen van kubieke vergelijkingen die geen reële oplossingen hadden. Techniek en natuurkunde profiteren enorm van complexe getallen voor het analyseren van wisselstroomcircuits en golfvormen. Je zou ook een bibliotheek zoals Kotlin's `koma` of `ejml` kunnen gebruiken voor zwaar werk.

Operaties op complexe getallen spiegelen die van reële getallen, maar met aandacht voor de imaginaire eenheid. Vermenigvuldiging volgt bijvoorbeeld de distributieve eigenschap, met de herinnering dat `i^2 = -1`. Deze imaginaire eenheid stelt ons in staat om multidimensionale getallen te vertegenwoordigen, cruciaal in verschillende wetenschappelijke berekeningen.

## Zie Ook

Kotlin Wiskundige bibliotheken:

- [koma](https://koma.kyonifer.com/): Een wetenschappelijke rekenbibliotheek voor Kotlin.

Verdere lectuur over Complexe Getallen:

- [Wikipedia: Complexe Getallen](https://nl.wikipedia.org/wiki/Complex_getal)
