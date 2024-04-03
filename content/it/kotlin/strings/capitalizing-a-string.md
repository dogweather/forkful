---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:37.640864-07:00
description: "Come fare: In Kotlin, le stringhe possono essere capitalizzate utilizzando\
  \ le funzioni della libreria standard senza la necessit\xE0 di librerie di terze\u2026"
lastmod: '2024-03-13T22:44:43.375396-06:00'
model: gpt-4-0125-preview
summary: "In Kotlin, le stringhe possono essere capitalizzate utilizzando le funzioni\
  \ della libreria standard senza la necessit\xE0 di librerie di terze parti."
title: Capitalizzare una stringa
weight: 2
---

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
