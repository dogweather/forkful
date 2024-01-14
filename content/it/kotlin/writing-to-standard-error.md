---
title:                "Kotlin: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché scrivere su standard error

Scrivere su standard error può essere utile in situazioni in cui si vuole visualizzare un messaggio di errore o di debug durante l'esecuzione di un programma Kotlin. A differenza di standard output, il contenuto scritto su standard error appare in rosso e non è influenzato dalle impostazioni di redirezione dei flussi di output. 

## Come scrivere su standard error

Per scrivere su standard error in Kotlin, è necessario utilizzare l'oggetto `System.err`, con il metodo `println()` per stampare il messaggio a schermo. Ad esempio:

```
Kotlin
 fun main() {
    System.err.println("Messaggio di errore!")
 }
```

Output:

```
Messaggio di errore!
```

È anche possibile utilizzare il metodo `print()` per stampare il messaggio senza andare a capo, o il metodo `printStackTrace()` per stampare l'intero stacktrace di un'eccezione.

## Approfondimento su scrivere su standard error

In Kotlin, oltre a utilizzare `System.err`, è possibile utilizzare anche la funzione di estensione `println()` su `System.err` per scrivere su standard error:

```
Kotlin
fun main() {
    System.err.println("Messaggio di errore!".println())
}
```

In questo modo, è possibile agire direttamente sulle stringhe, ad esempio utilizzando il metodo `format()` per formattare il testo da stampare.

## Vedi anche

- Documentazione ufficiale di Kotlin su `System.err`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io...err/
- Tutorial su come utilizzare i flussi di input ed output in Kotlin: https://www.baeldung.com/kotlin-input-output
- Esempio pratico di utilizzo di `System.err` per la gestione degli errori in Kotlin: https://blog.mindorks.com/kotlin-programming-exceptions-and-system-out-system-err