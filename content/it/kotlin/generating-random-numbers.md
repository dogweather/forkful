---
title:                "Kotlin: Generazione di numeri casuali"
programming_language: "Kotlin"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molti motivi per cui uno potrebbe voler utilizzare numeri casuali nel proprio codice Kotlin. Ad esempio, potrebbe essere utile per generare password o ID univoci, creare simulazioni o giochi, o semplicemente aggiungere casualità in un'applicazione.

## Come Fare
Kotlin ha un'ottima libreria integrata per generare numeri casuali. Per iniziare, dobbiamo importare questa libreria con la seguente riga di codice:

```Kotlin
import kotlin.random.Random
```

Una volta importata, possiamo utilizzare la funzione `nextInt()` per generare un numero intero casuale. Ad esempio, se vogliamo generare un numero da 1 a 10, possiamo utilizzare il seguente codice:

```Kotlin
val numeroCasuale = Random.nextInt(1, 11)
```

Il primo parametro rappresenta il numero minimo incluso nella generazione casuale, mentre il secondo parametro rappresenta il numero massimo escluso. Quindi, nel nostro esempio, il numero casuale sarà compreso tra 1 e 10.

Possiamo anche utilizzare la funzione `nextDouble()` per generare un numero decimale casuale compreso tra 0 e 1. Ad esempio:

```Kotlin
val numeroCasualeDecimale = Random.nextDouble()
```

Se vogliamo generare un numero decimale in un intervallo specifico, possiamo utilizzare la formula `nextDouble() * (max - min) + min`. Ad esempio, se vogliamo generare un numero decimale tra 5 e 10, possiamo utilizzare il seguente codice:

```Kotlin
val numeroCasualeDecimale = Random.nextDouble() * (10 - 5) + 5
```

## Approfondimento
La libreria di numeri casuali di Kotlin utilizza l'algoritmo di Fischer-Yates per generare numeri casuali in modo efficiente. Questo algoritmo garantisce che ogni possibile combinazione di numeri sia ugualemente probabile, rendendo la generazione casuale più equa.

Inoltre, Kotlin offre anche altre funzioni per generare numeri casuali, come `nextBoolean()` per generare un valore booleano casuale, `nextBytes()` per generare un'array di byte casuali e `nextBytesRange()` per generare una sequenza di byte casuali.

Se sei interessato a saperne di più sulla generazione di numeri casuali e gli algoritmi utilizzati, consigliamo di dare un'occhiata alle risorse elencate nella sezione "Vedi anche" in basso.

## Vedi Anche
- [Kotlin Random documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Fischer-Yates shuffle on Wikipedia](https://en.wikipedia.org/wiki/Fisher%E2%80%93Yates_shuffle)
- [Understanding random number generation algorithms](https://www.geeksforgeeks.org/understanding-random-number-generator-algorithms/) (in inglese)