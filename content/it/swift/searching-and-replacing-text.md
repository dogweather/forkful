---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cos'è & Perché? 
Cercare e sostituire del testo in una stringa è un'operazione fondamentale nella programmazione. Ci permette di manipolare i dati, fare pulizia del testo, cercare parole chiave o modellare il formato dei dati per esigenze specifiche.

## Come fare:
Here's un esempio su come cercare e sostituire del testo utilizzando Swift.

```swift
let original = "Mi piace la pasta alla carbonara"
let sostituto = original.replacingOccurrences(of: "pasta alla carbonara", with: "pizza margherita")

print(sostituto)
// Stampa: "Mi piace la pizza margherita"
```

In questo codice, abbiamo una stringa originale "Mi piace la pasta alla carbonara". Vogliamo sostituire "pasta alla carbonara" con "pizza margherita". La funzione `replacingOccurrences` fa esattamente questo.

## Deep Dive:
Prima dell'avvento di linguaggi moderni e potenti come Swift, la ricerca e la sostituzione del testo erano più complesse e richiedevano più tempo. Con Swift, queste operazioni sono diventate semplici e dirette.

Un'alternativa a `replacingOccurrences` è l'utilizzo di funzioni di regex (regular expressions). Questo può offrire più flessibilità a costo di una leggera perdita di leggibilità del codice.

`replacingOccurrences` fa parte delle String API in Swift. Utilizza l'algoritmo di Boyer-Moore-Horspool, che è efficiente per il matching di stringhe, specialmente per pattern di lunghezza maggiore.

## See Also:
Per ulteriori approfondimenti, visita i seguenti link:

- Apple Developer Documentation - [`replacingOccurrences(of:with:)`](https://developer.apple.com/documentation/swift/string/2893958-replacingoccurrences)
- Esempi di regex in Swift - [RegexOne](https://regexone.com/lesson/introduction_abcs)

Ricorda, la pratica fa la perfezione. Continua a codificare in Swift, esplora e sperimenta!