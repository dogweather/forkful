---
date: 2024-01-27 20:34:34.028222-07:00
description: "Generare numeri casuali nella programmazione riguarda la creazione di\
  \ numeri che non presentano alcun schema prevedibile. I programmatori lo fanno per\u2026"
lastmod: '2024-03-13T22:44:43.387378-06:00'
model: gpt-4-0125-preview
summary: Generare numeri casuali nella programmazione riguarda la creazione di numeri
  che non presentano alcun schema prevedibile.
title: Generazione di numeri casuali
weight: 12
---

## Cosa e Perché?

Generare numeri casuali nella programmazione riguarda la creazione di numeri che non presentano alcun schema prevedibile. I programmatori lo fanno per vari motivi, inclusi simulazioni, testing di algoritmi, giochi e applicazioni di sicurezza, dove l'imprevedibilità è la chiave per raggiungere risultati realistici o sicuri.

## Come fare:

Kotlin offre un modo semplice per generare numeri casuali attraverso la sua libreria standard. Ecco come puoi generare diversi tipi di valori casuali:

### Generare un Intero Casuale

Per generare un intero casuale all'interno di un intervallo specifico:

```kotlin
import kotlin.random.Random

fun main() {
    val randomNumber = Random.nextInt(1, 100) // Genera un numero casuale tra 1 e 99
    println(randomNumber)
}
```

### Generare un Double Casuale

Allo stesso modo, per generare un double casuale:

```kotlin
import kotlin.random.Random

fun main() {
    val randomDouble = Random.nextDouble(1.0, 10.0) // Genera un double casuale tra 1.0 e 10.0
    println(randomDouble)
}
```

### Generare un Booleano Casuale

Per generare un valore booleano casuale:

```kotlin
import kotlin.random.Random

fun main() {
    val randomBoolean = Random.nextBoolean() // Genera true o false casualmente
    println(randomBoolean)
}
```

### Seeding per Risultati Riproducibili

Nei casi in cui hai bisogno di sequenze riproducibili di numeri casuali (ad esempio, nei test), puoi impostare il seed del generatore di numeri casuali:

```kotlin
import kotlin.random.Random

fun main() {
    val seed = 12345L
    val random = Random(seed)
    val randomNumber = random.nextInt(1, 100)
    println(randomNumber)
}
```

## Approfondimento

L'approccio della libreria standard di Kotlin alla generazione di numeri casuali sfrutta sotto il cofano `java.util.Random` di Java, garantendo un equilibrio tra facilità d'uso e performance. Tuttavia, è fondamentale notare che questi metodi generano numeri pseudo-casuali, il che significa che i numeri appaiono casuali ma sono generati utilizzando un processo deterministico.

Per la maggior parte delle applicazioni, la casualità fornita dalla classe `Random` di Kotlin è sufficiente. Tuttavia, per applicazioni più sensibili alla sicurezza, come la crittografia, dove la qualità della casualità è di primaria importanza, si dovrebbe considerare l'uso di `java.security.SecureRandom` invece. SecureRandom è specificamente progettato per operazioni crittografiche, fornendo una qualità di casualità superiore, sebbene con un potenziale compromesso sulle performance.

Kotlin non reinventa la ruota ma offre un'API amichevole per Kotlin su meccanismi di generazione di numeri casuali di Java, rendendola più idiomatica e concisa da usare all'interno dei progetti Kotlin. Come sempre, quando si tratta di casualità, i programmatori dovrebbero considerare attentamente il caso d'uso per scegliere lo strumento più appropriato per il lavoro.
