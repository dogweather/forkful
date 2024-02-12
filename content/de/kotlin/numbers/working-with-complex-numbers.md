---
title:                "Umgang mit komplexen Zahlen"
aliases:
- /de/kotlin/working-with-complex-numbers.md
date:                  2024-01-26T04:42:36.965924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Umgang mit komplexen Zahlen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Komplexe Zahlen erweitern unser Zahlensystem um die Quadratwurzeln negativer Zahlen, wobei die 'imaginäre' Einheit i gleich der Quadratwurzel von -1 ist. Programmierer nutzen sie in Bereichen wie Ingenieurwissenschaften, Physik und Signalverarbeitung, da sie hervorragend sind, um Wellen, Schwingungen und alles, was sich dreht, zu modellieren.

## Wie geht das:

Definieren wir eine grundlegende Klasse für komplexe Zahlen in Kotlin:

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
    
    println("a + b = ${a + b}")  // Ausgabe: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Ausgabe: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Ausgabe: a * b = (-5.0 + 10.0i)
}
```

## Tiefer gehend

Komplexe Zahlen wurden erstmals im 16. Jahrhundert erwähnt, um kubische Gleichungen zu lösen, denen reelle Lösungen fehlten. Ingenieurwissenschaften und Physik profitieren enorm von komplexen Zahlen, um Wechselstromkreise und Wellenformen zu analysieren. Alternativ könnte man für aufwändigere Arbeiten eine Bibliothek wie Kotlin's `koma` oder `ejml` verwenden.

Operationen mit komplexen Zahlen spiegeln die der reellen Zahlen wider, jedoch mit Beachtung der imaginären Einheit. Die Multiplikation beispielsweise folgt dem Distributivgesetz, wobei man sich erinnert, dass `i^2 = -1`. Diese imaginäre Einheit ermöglicht es uns, mehrdimensionale Zahlen darzustellen, was in verschiedenen wissenschaftlichen Berechnungen entscheidend ist.

## Siehe auch

Kotlin-Mathematikbibliotheken:

- [koma](https://koma.kyonifer.com/): Eine wissenschaftliche Rechenbibliothek für Kotlin.

Weiterführende Literatur zu komplexen Zahlen:

- [Wikipedia: Komplexe Zahlen](https://de.wikipedia.org/wiki/Komplexe_Zahl)
