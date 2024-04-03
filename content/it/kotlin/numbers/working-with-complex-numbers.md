---
date: 2024-01-26 04:42:44.039296-07:00
description: "I numeri complessi ampliano il nostro sistema numerico per includere\
  \ le radici quadrate dei numeri negativi, dove l'unit\xE0 'immaginaria' i equivale\
  \ alla\u2026"
lastmod: '2024-03-13T22:44:43.385533-06:00'
model: gpt-4-0125-preview
summary: "I numeri complessi ampliano il nostro sistema numerico per includere le\
  \ radici quadrate dei numeri negativi, dove l'unit\xE0 'immaginaria' i equivale\
  \ alla radice quadrata di -1."
title: Lavorare con i numeri complessi
weight: 14
---

## Cosa e Perché?
I numeri complessi ampliano il nostro sistema numerico per includere le radici quadrate dei numeri negativi, dove l'unità 'immaginaria' i equivale alla radice quadrata di -1. I programmatori li usano in campi come l'ingegneria, la fisica e l'elaborazione dei segnali, perché sono ottimi per modellare onde, oscillazioni e tutto ciò che ruota.

## Come fare:

Definiamo una classe di base per un numero complesso in Kotlin:

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
    
    println("a + b = ${a + b}")  // Output: a + b = (4.0 + 6.0i)
    println("a - b = ${a - b}")  // Output: a - b = (-2.0 - 2.0i)
    println("a * b = ${a * b}")  // Output: a * b = (-5.0 + 10.0i)
}
```

## Approfondimento

I numeri complessi furono menzionati per la prima volta nel XVI secolo, risolvendo equazioni cubiche che mancavano di soluzioni reali. L'ingegneria e la fisica traggono grande beneficio dai numeri complessi per l'analisi dei circuiti in corrente alternata e delle forme d'onda. Alternativamente, potresti usare una libreria come `koma` o `ejml` di Kotlin per lavori più pesanti.

Le operazioni sui numeri complessi rispecchiano i numeri reali, ma prestando attenzione all'unità immaginaria. La moltiplicazione, ad esempio, segue la proprietà distributiva, ricordando che `i^2 = -1`. Questa unità immaginaria ci consente di rappresentare numeri multidimensionali, cruciale in varie computazioni scientifiche.

## Vedi Anche

Librerie di Matematica Kotlin:

- [koma](https://koma.kyonifer.com/): Una libreria per il calcolo scientifico per Kotlin.

Ulteriori letture sui Numeri Complessi:

- [Wikipedia: Numeri Complessi](https://it.wikipedia.org/wiki/Numero_complesso)
