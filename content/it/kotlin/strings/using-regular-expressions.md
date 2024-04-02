---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:17:31.437453-07:00
description: "Le espressioni regolari (regex) sono uno strumento potente per l'elaborazione\
  \ del testo, che permette ai programmatori di cercare, corrispondere e\u2026"
lastmod: '2024-03-13T22:44:43.381887-06:00'
model: gpt-4-0125-preview
summary: "Le espressioni regolari (regex) sono uno strumento potente per l'elaborazione\
  \ del testo, che permette ai programmatori di cercare, corrispondere e\u2026"
title: Utilizzo delle espressioni regolari
weight: 11
---

## Cosa e Perché?

Le espressioni regolari (regex) sono uno strumento potente per l'elaborazione del testo, che permette ai programmatori di cercare, corrispondere e manipolare le stringhe con tecniche avanzate di corrispondenza di modelli. In Kotlin, sfruttare le regex aiuta a eseguire in modo efficiente compiti complessi di elaborazione del testo come la validazione, l'analisi o la trasformazione, rendendolo indispensabile per compiti che vanno dalla semplice manipolazione di stringhe all'analisi di testo complessa.

## Come fare:

### Corrispondenza di base
Per verificare se una stringa corrisponde a un modello specifico in Kotlin, puoi usare il metodo `matches` della classe `Regex`.

```kotlin
val pattern = "kotlin".toRegex()
val input = "I love kotlin"
val result = pattern.containsMatchIn(input)

println(result)  // Output: true
```

### Trovare ed Estratte Parti di Stringa
Se vuoi trovare parti di una stringa che corrispondono a un modello, Kotlin ti permette di iterare su tutte le corrispondenze:

```kotlin
val datePattern = "\\d{2}/\\d{2}/\\d{4}".toRegex()
val input = "La data di oggi è 07/09/2023."
val dates = datePattern.findAll(input)

for (date in dates) {
    println(date.value)
}
// Output: 07/09/2023
```

### Sostituire il Testo
Sostituire parti di una stringa che corrispondono a un modello è semplice con la funzione `replace`:

```kotlin
val input = "Username: user123"
val sanitizedInput = input.replace("\\d+".toRegex(), "XXX")

println(sanitizedInput)  // Output: Username: userXXX
```

### Dividere le Stringhe
Dividi una stringa in un elenco, usando un modello regex come delimitatore:

```kotlin
val input = "1,2,3,4,5"
val numbers = input.split(",".toRegex())

println(numbers)  // Output: [1, 2, 3, 4, 5]
```

### Librerie di terze parti: Kotest
[Kotest](https://github.com/kotest/kotest) è una popolare libreria di testing per Kotlin che estende il supporto regex integrato di Kotlin, particolarmente utile per la validazione nei casi di test.

```kotlin
// Supponendo che Kotest sia aggiunto al tuo progetto
import io.kotest.matchers.string.shouldMatch

val input = "kotlin@test.com"
input shouldMatch "\\S+@\\S+\\.com".toRegex()

// Questo supererà il test se l'input corrisponde al modello di email.
```

Incorporando le espressioni regolari nelle tue applicazioni Kotlin, puoi eseguire l'elaborazione del testo in modo sofisticato ed efficiente. Che tu stia convalidando l'input dell'utente, estraendo dati o trasformando stringhe, i modelli regex offrono una soluzione robusta.
