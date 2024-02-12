---
title:                "Capitalizzare una stringa"
date:                  2024-02-03T19:05:37.640864-07:00
model:                 gpt-4-0125-preview
simple_title:         "Capitalizzare una stringa"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

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
