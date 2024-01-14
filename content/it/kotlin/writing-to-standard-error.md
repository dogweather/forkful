---
title:                "Kotlin: Scrivere su errore standard"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Perché
Scrivere su standard error può sembrare un'operazione inutile o addirittura erronea per molti programmatori, ma in realtà può essere molto utile in alcune situazioni. Invece di inviare un messaggio di errore o di debug su standard output, scrivere su standard error consente di distinguere chiaramente i messaggi di errore dai messaggi di output regolari. Inoltre, molti strumenti di sviluppo e applicazioni di monitoraggio mostrano solo i messaggi di errore provenienti da standard error, quindi se vuoi assicurarti di essere a conoscenza di eventuali problemi nel tuo codice, scrivere su standard error è una buona abitudine da adottare.

## Come fare
Per scrivere su standard error in Kotlin, è necessario utilizzare la funzione `println()` e specificare il valore di `System.err` come parametro. Per esempio:

```
Kotlin val message = "Questo è un messaggio di errore."
println(System.err, message)
```

Questo scriverà il messaggio di errore sulla console in rosso, rendendolo facilmente distinguibile dai messaggi di output regolari.

## Approfondimento
Scrivere su standard error può essere particolarmente utile durante il processo di debugging, in cui si cerca di individuare e risolvere eventuali problemi nel codice. Inoltre, è una buona pratica durante lo sviluppo di applicazioni che verranno poi utilizzate in produzione, in quanto i messaggi di errore scritti su standard error saranno registrati nei file di log e potranno essere utilizzati per identificare e risolvere eventuali problemi successivamente.

## Vedi anche
- [Documentazione ufficiale di Kotlin sulla funzione `println()`](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-print-stream/println.html)
- [Come gestire gli errori in Kotlin](https://www.programiz.com/kotlin-programming/error-handling)
- [Come scrivere su un file di log in Kotlin](https://www.baeldung.com/kotlin/print-to-file)