---
title:                "Generare numeri casuali"
html_title:           "Kotlin: Generare numeri casuali"
simple_title:         "Generare numeri casuali"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'operazione comune nelle applicazioni di gioco, nei test di software e in molte altre situazioni in cui è necessario un elemento di casualità. In questo articolo scoprirai come generare numeri casuali utilizzando il linguaggio di programmazione Kotlin.

## Come

Per generare numeri casuali in Kotlin, puoi utilizzare la funzione `Random.nextInt()`. Questa funzione accetta un parametro opzionale che indica il range massimo dei numeri generati, altrimenti verranno generati numeri casuali compresi tra 0 e un valore molto grande. Ecco un esempio di codice che genera un numero casuale tra 1 e 10 e lo stampa a video:
```Kotlin
val randomNumber = Random.nextInt(1, 11)
println(randomNumber)
```
L'output potrebbe essere 5, 8 o qualsiasi altro numero compreso tra 1 e 10.

Se hai bisogno di un numero intero casuale all'interno di un determinato intervallo, puoi utilizzare la funzione `Random.nextInt()` con la seguente formula:
```Kotlin
val randomNumber = Random.nextInt(rangeEnd - rangeStart) + rangeStart
```
In questo caso, il rangeStart specifica il valore minimo dei numeri generati, mentre il rangeEnd indica il valore massimo (escluso).

Per generare numeri casuali di tipo decimale, puoi utilizzare la funzione `Random.nextDouble()`. Questa funzione restituisce un numero a virgola mobile compreso tra 0 e 1. Ecco un esempio di codice:
```Kotlin
val randomDecimal = Random.nextDouble()
println(randomDecimal)
```
L'output potrebbe essere 0.342, 0.901 o qualsiasi altro numero compreso tra 0 e 1.

## Deep Dive

La funzione `Random.nextInt()` utilizza un generatore di numeri casuali basato sull'algoritmo di Lehmer. L'algoritmo si basa sull'idea di utilizzare un numero "seme" iniziale per generare numeri successivi. Questo significa che ogni volta che esegui il tuo programma, verranno generati gli stessi numeri casuali. Per evitare ciò, puoi impostare un seme diverso utilizzando la funzione `Random.setSeed()`, passando un numero diverso come parametro.

Se hai bisogno di numeri casuali con una distribuzione più uniforme, puoi utilizzare la classe `SecureRandom` che utilizza l'algoritmo di generazione di numeri casuali di tipo criptografico. Tieni presente che questa classe è un po' più lenta rispetto alla funzione `Random.nextInt()`, ma fornisce un maggior livello di sicurezza.

## Vedi anche

- [Documentazione ufficiale di Kotlin su Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Tutorial su come generare numeri casuali in Java](https://www.baeldung.com/java-generate-random-long-float-integer-double)
- [Una panoramica degli algoritmi di generazione di numeri casuali](https://www.geeksforgeeks.org/analysis-of-different-sorting-techniques/)