---
title:                "Generazione di numeri casuali"
date:                  2024-01-20T17:49:16.497315-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generazione di numeri casuali"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Generare numeri casuali è un modo per ottenere valori imprevedibili ed è spesso usato in giochi, simulazioni e test di software per creare condizioni variabili e realistiche.

## How to:
```
import kotlin.random.Random

fun main() {
    val numeroCasuale = Random.nextInt(0, 100)  // Genera un numero da 0 a 99
    println(numeroCasuale)
    
    val numeroCasualeDouble = Random.nextDouble(1.0, 10.0)  // Genera un double da 1.0 a 10.0
    println(numeroCasualeDouble)
}
```
Output:
```
42
3.1415926535
```
Ogni volta che esegui, i numeri sono diversi.

## Deep Dive
Prima della versione Kotlin 1.3, si usava la classe `java.util.Random` per generare numeri casuali. Con Kotlin 1.3, è stata introdotta la classe `kotlin.random.Random` per una migliore integrazione con le funzionalità del linguaggio. La generazione di numeri casuali può essere semplice come abbiamo visto, ma può anche basarsi su algoritmi più complessi come "linear congruential generator" (LCG) o "Mersenne Twister". Un'implementazione di base di Kotlin usa LCG, ma può essere sovrascritta con altri algoritmi se necessario.

## See Also
- Kotlin Random Documentation: [kotlinlang.org/api/latest/jvm/stdlib/kotlin.random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/)
- Stack Overflow - Utilizzo di numeri casuali in Kotlin: [stackoverflow.com/questions/tagged/kotlin+random](https://stackoverflow.com/questions/tagged/kotlin+random)