---
title:                "Scrivere su standard error"
html_title:           "Kotlin: Scrivere su standard error"
simple_title:         "Scrivere su standard error"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Che cos'è & perché?

Scrivere su standard error è una pratica comune tra i programmatori che permette di inviare messaggi di errore al terminale. Questo aiuta a identificare e risolvere problemi durante l'esecuzione del programma.

## Come fare:

```
Kotlin System.err.println("Messaggio di errore")
```

Output:
```
Messaggio di errore
```

## Approfondimento:

Scrivere su standard error è una tecnica che risale all'epoca dei primi sistemi operativi. In alcuni casi, viene utilizzato invece di standard output per stampare messaggi di errore, poiché permette di visualizzarli come testo rosso nel terminale, facilitando la loro individuazione.

Un'alternativa alla scrittura di standard error è l'utilizzo di logger, che permettono di gestire i messaggi di errore in modo più dettagliato e personalizzato. Tuttavia, in alcune situazioni la scrittura su standard error è ancora preferita, soprattutto quando si sta lavorando su piccoli script o programmi semplici.

Per scrivere su standard error in Kotlin, è possibile utilizzare il metodo ```println``` della classe ```System.err```. In questo modo, il messaggio verrà stampato correttamente sul terminale.

## Vedi anche:
