---
title:    "Kotlin: Generare numeri casuali"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui potresti voler generare numeri casuali in un programma Kotlin. Potresti aver bisogno di scrivere un algoritmo di gioco, creare password randomiche o generare una chiave crittografica. Generare numeri casuali è un'abilità fondamentale per molti programmatori e con Kotlin è facile da fare.

## Come Fare
Per generare numeri casuali in Kotlin, è necessario utilizzare la classe `Random()`. Questa classe ha diversi metodi che ci permettono di ottenere diversi tipi di numeri casuali:
```
// Genera un numero intero tra 0 e 10
val num1 = Random().nextInt(11)

// Genera un numero decimale tra 0 e 1
val num2 = Random().nextDouble()

// Genera un numero lungo
val num3 = Random().nextLong()
```
<br>
Possiamo anche specificare un valore minimo e massimo per limitare l'intervallo dei numeri generati:
```
// Genera un numero intero tra 50 e 100
val num = Random().nextInt(51, 101)
```

## Approfondimento
Sebbene generare numeri casuali possa sembrare una cosa semplice, ci sono alcune cose da considerare quando lo si fa in un programma. Per esempio, quando si genera un numero casuale in un ciclo, è importante fare attenzione a dove viene istanziata la classe `Random()`. Se viene istanziata ogni volta che il ciclo viene eseguito, si potrebbe finire per generare lo stesso numero più volte.

Un'altra cosa importante da considerare è che i numeri casuali non sono veramente casuali, ma sono generati da un algoritmo. I programmatori devono scegliere quale algoritmo utilizzare per garantire una distribuzione casuale dei numeri generati.

Inoltre, esistono anche altre classi in Kotlin che consentono di generare numeri casuali, come `ThreadLocalRandom` che è ottimizzata per l'utilizzo in ambienti multithread.

## Vedi Anche
- [Documentazione sulla classe Random in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Tutorial su come generare numeri casuali in Kotlin](https://www.programiz.com/kotlin-programming/random-numbers)
- [Discussione su Stack Overflow su quale algoritmo utilizzare per generare numeri casuali](https://stackoverflow.com/questions/29666142/java-random-number-vs-math-random)