---
title:                "Scrittura su errori standard"
html_title:           "Swift: Scrittura su errori standard"
simple_title:         "Scrittura su errori standard"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché? 
Scrivere su standard error non è altro che l'atto di inviare un messaggio di errore ad un canale specifico, invece di stamparlo sullo standard output. I programmatori lo fanno per tenere traccia degli errori in modo più efficiente e per separare i messaggi di errore da quelli di output regolari.

## Come fare:
Per scrivere su standard error in Swift, è necessario utilizzare il metodo `fputs()` insieme alla costante `stderr` che rappresenta lo standard error. Il tuo codice dovrebbe apparire come segue:

```
import Foundation
fputs("Messaggio di errore", stderr)
```

Questo codice invierà il messaggio di errore specificato al canale dello standard error. Puoi anche utilizzare l'operatore `<<` per inviare un messaggio di errore direttamente all'interno di un'istruzione print:

```
import Foundation
print("Messaggio di errore" << stderr)
```

L'output di entrambi i codici sarà simile a questo: 
`Messaggio di errore`

## Approfondimenti:
Lo standard error è stato introdotto nei primi sistemi operativi UNIX nel 1969 e da allora è diventato uno standard nell'industria informatica. Un altro modo per gestire gli errori è utilizzare il meccanismo di eccezioni, ma ciò richiede una maggiore complessità nella gestione e può rallentare l'esecuzione del codice.

## Altre risorse: 
- Documentazione Apple su `Foundation` e `stderr`: https://developer.apple.com/documentation/foundation/1395125-fputs
- Tutorial su come gestire gli errori in Swift: https://www.swiftbysundell.com/posts/throwing-errors-in-swift