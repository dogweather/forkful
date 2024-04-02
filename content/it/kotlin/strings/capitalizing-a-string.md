---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:37.640864-07:00
description: "Capitalizzare una stringa nella programmazione comporta la conversione\
  \ del primo carattere della stringa in maiuscolo, se non lo \xE8 gi\xE0, il che\
  \ \xE8 utile per\u2026"
lastmod: '2024-03-13T22:44:43.375396-06:00'
model: gpt-4-0125-preview
summary: "Capitalizzare una stringa nella programmazione comporta la conversione del\
  \ primo carattere della stringa in maiuscolo, se non lo \xE8 gi\xE0, il che \xE8\
  \ utile per\u2026"
title: Capitalizzare una stringa
weight: 2
---

## Cosa & Perché?

Capitalizzare una stringa nella programmazione comporta la conversione del primo carattere della stringa in maiuscolo, se non lo è già, il che è utile per formattare gli input degli utenti o visualizzare testi in un'interfaccia utente in modo più standardizzato o amichevole. I programmatori eseguono questa operazione per garantire la coerenza dei dati o per soddisfare specifiche esigenze di formattazione all'interno delle loro applicazioni software.

## Come fare:

In Kotlin, le stringhe possono essere capitalizzate utilizzando le funzioni della libreria standard senza la necessità di librerie di terze parti. L'approccio di Kotlin alla gestione delle stringhe rende queste operazioni dirette e concise.

### Capitalizzare l'intera stringa:

```kotlin
val message = "ciao, mondo!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Output: CIAO, MONDO!
```

### Capitalizzare solo il primo carattere:

A partire da Kotlin 1.5, la funzione `capitalize()` è deprecata e sostituita con una combinazione di `replaceFirstChar` e una lambda che verifica se si tratta di una lettera minuscola per trasformarla in maiuscolo.

```kotlin
val greeting = "ciao, mondo!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Output: Ciao, mondo!
```

Questo approccio mantiene il resto della frase nella sua forma originale mentre cambia solo la prima lettera in maiuscolo.
