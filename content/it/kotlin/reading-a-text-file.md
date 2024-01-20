---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Leggere un file di testo significa accedere ai dati contenuti in questo. I programmatori lo fanno per gestire e manipolare le informazioni in vari scopi, come l'analisi dei dati, la configurazione, l'elaborazione del testo, e così via.

## Come fare:
Per leggere un file di testo in Kotlin, usiamo la funzione `readText()`. Ecco un esempio con un file di nome `file.txt`.

```Kotlin
import java.io.File

fun main() {
    val data = File("file.txt").readText()
    println(data)
}
```

Questa riga di codice leggerà `file.txt` e stamperà il suo contenuto.

## Approfondimento
Così come in molti altri linguaggi di programmazione, leggere file di testo è una funzione molto antica in Kotlin. Infatti, esistono vari modi per farlo, non solo `readText()`. Alcune alternative includono `readLines()`, che restituisce una lista di stringhe, o `bufferedReader().use {}`, che può essere più efficiente per file di grandi dimensioni.

## Vedi anche
Per ulteriori informazioni sulla manipolazione dei file in Kotlin:
- [Documentazione ufficiale Kotlin - I/O](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/index.html)
- [Post sul blog Jetbrains](https://blog.jetbrains.com/kotlin/2020/03/kotlin-1-4-m3-generating-default-methods-in-interfaces/)
- [Tutorial su Baeldung](https://www.baeldung.com/kotlin-read-file)