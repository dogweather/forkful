---
title:    "Kotlin: Scrivere su standard error"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

##Perché

Scrivere su standard error può sembrare una pratica poco comune, ma in realtà può essere molto utile quando si vuole vedere un output che non è immediatamente visualizzato sullo schermo. Ad esempio, quando si esegue un programma in una console, scrivere su standard error invierà l'output su una console di errore dedicata, separandolo dal resto dei messaggi.

##Come fare

Per scrivere su standard error in Kotlin, è necessario utilizzare la funzione `System.err.println()`, la quale prenderà in input il messaggio che si vuole inviare come errore. Ad esempio:

```Kotlin
System.err.println("Errore: il valore è troppo grande")
```

Questo comando scrive il messaggio "Errore: il valore è troppo grande" sullo standard error.

##Deep Dive

Quando un programma viene eseguito su una console, l'output viene di solito inviato allo standard output, rendendolo visibile all'utente. Tuttavia, ci possono essere situazioni in cui è necessario inviare un output diverso da quello principale, come ad esempio quando si desidera registrare un errore senza interrompere il normale flusso di output.

In questi casi, scrivere su standard error può risultare molto utile. Ad esempio, se si vuole registrare gli errori in un file di log, si potrebbe redirectare lo standard output su quel file e scrivere gli errori direttamente su standard error.

##Vedi anche

- [Documentazione di Kotlin sullo standard error](https://kotlinlang.org/docs/reference/exceptions.html#error-handling)
- [Tutorial su come catturare e gestire gli errori in Kotlin](https://www.baeldung.com/kotlin/exceptions-handling)