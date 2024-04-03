---
date: 2024-01-20 17:52:57.803047-07:00
description: "Stampare output di debug permette di vedere che cosa succede nel codice\
  \ durante l'esecuzione. I programmatori lo fanno per scovare errori o comportamenti\u2026"
lastmod: '2024-03-13T22:44:43.393570-06:00'
model: gpt-4-1106-preview
summary: Stampare output di debug permette di vedere che cosa succede nel codice durante
  l'esecuzione.
title: Stampa dell'output di debug
weight: 33
---

## How to:
Ecco la base: `println()` per mandare in stampa i tuoi messaggi di debug. Semplice.

```Kotlin
fun main() {
    val variabileTest = "Ciao, mondo del debug!"
    println("Output di debug: $variabileTest")
}
```

Output:
```
Output di debug: Ciao, mondo del debug!
```

Ora, aggiungiamo un po' di pepe con `if` per solo quando serve:

```Kotlin
fun main() {
    val debugMode = true
    val status = "tutto ok"

    if (debugMode) {
        println("Debug - Status: $status")
    }
}
```

Output se `debugMode` è true:
```
Debug - Status: tutto ok
```

Niente output se `debugMode` è false.

## Deep Dive
Il father del debug è il breakpoint, ma stampare output è più antico delle colline. Prima i programmatori scrivevano su carta, poi su terminali. Ora su console e log files. 
Alternativa a `println()`? Usare un logger, come `Log4j` o `SLF4J`, che può filtrare per livello di importanza: error, warn, info, debug, trace.
Dettaglio: usare `println()` è comodo ma può creare casino nel codice. Meglio usare un framework di logging che ti dà controlli fini su cosa stampare e dove.

## See Also
- Documentazione Kotlin: [Funzioni di stampa Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- SLF4J: [Simple Logging Facade for Java](http://www.slf4j.org/)
- Log4j 2: [Apache Log4j 2](https://logging.apache.org/log4j/2.x/)
- Guida su come loggare meglio in Kotlin: [Effective Logging in Kotlin](https://www.baeldung.com/kotlin/logging)
