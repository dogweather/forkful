---
title:                "Kotlin: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è una delle funzionalità più utili e divertenti che un programmatore può aggiungere ai propri progetti. Con l'utilizzo di numeri casuali, è possibile creare giochi, applicazioni di simulazione e molto altro ancora.

## Come fare

Per generare numeri casuali in un progetto Kotlin, è necessario importare la classe `Random` dal pacchetto `kotlin.random`. Una volta importata la classe, è possibile utilizzarla per generare numeri casuali di diversi tipi come interi, virgola mobile e booleani.

```Kotlin
// Generazione di un numero intero casuale compreso tra 0 e 10
val randomNumber = Random.nextInt(0, 10)
println(randomNumber)

// Generazione di un numero casuale compreso tra 0 e 1
val randomDouble = Random.nextDouble()
println(randomDouble)

// Generazione di un valore booleano casuale
val randomBool = Random.nextBoolean()
println(randomBool)
```

L'esempio sopra utilizza il metodo `nextInt()`, `nextDouble()` e `nextBoolean()` della classe `Random` per generare rispettivamente un numero intero, un numero con virgola mobile e un valore booleano casuale. Questi metodi possono essere utilizzati all'interno di una funzione o di un ciclo per generare numeri casuali ripetutamente.

## Approfondimento

La generazione di numeri casuali si basa su algoritmi matematici che producono una serie di numeri prevedibili ma apparentemente casuali. Sebbene questi numeri non siano veramente casuali, a meno che non vengano utilizzati dispositivi esterni di generazione di numeri casuali, possono fornire una buona approssimazione della casualità.

È importante ricordare che i numeri casuali generati da un computer sono influenzati da diversi fattori come l'orario di sistema e lo stato della memoria. Ciò significa che, a meno che non si utilizzi un seme di inizializzazione, ogni volta che si esegue il programma verrà generato un diverso insieme di numeri casuali.

## Vedi anche

- [Documentazione di Kotlin sulla classe Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/)
- [Come generare numeri casuali in Java](https://www.baeldung.com/java-generating-random-numbers)
- [Come creare un gioco con numeri casuali in Unity](https://learn.unity.com/tutorial/random-numbers-1)