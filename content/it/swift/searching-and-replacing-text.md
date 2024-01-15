---
title:                "Ricerca e sostituzione di testo"
html_title:           "Swift: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Sicuramente ti sarà capitato di dover sostituire una parola o una frase all'interno di un testo. Con il linguaggio di programmazione Swift, puoi facilmente automatizzare questa operazione per risparmiare tempo e rendere il tuo codice più efficiente.

## Come Fare

Per effettuare la ricerca e sostituzione di testo in Swift, puoi utilizzare il metodo `replacingOccurrences(of:with:)` sulla stringa di testo su cui vuoi operare.

Ecco un esempio di come sostituire tutte le lettere "a" presenti nella stringa "banana" con la lettera "e":

```Swift
let testo = "banana"
let sostituzione = testo.replacingOccurrences(of: "a", with: "e")
print(sostituzione) // Output: benene
```

Puoi anche specificare che la sostituzione avvenga solo su una determinata porzione di testo, indicando l'intervallo in cui effettuare la sostituzione:

```Swift
let testo = "Sono felice di imparare Swift."
let sostituzione = testo.replacingOccurrences(of: "felice", with: "emozionato", range: testo.range(of: "felice"))
print(sostituzione) // Output: Sono emozionato di imparare Swift.
```

## Approfondimento

Oltre al metodo `replacingOccurrences(of:with:)`, Swift offre anche altri strumenti per effettuare la ricerca e sostituzione di testo.

Un esempio è il metodo `replacingOccurrences(of:with:options:range:locale:)` che permette di specificare delle opzioni di ricerca, come ad esempio l'ignorare la differenza tra maiuscole e minuscole.

Puoi anche utilizzare espressioni regolari con il framework `Foundation` per una maggiore flessibilità nella ricerca e sostituzione di testo.

## Vedi Anche

- Documentazione Apple sul metodo `replacingOccurrences(of:with:)` (https://developer.apple.com/documentation/foundation/nsstring/1413230-replacingoccurrences)
- Documentazione Apple sul framework `Foundation` per l'utilizzo di espressioni regolari (https://developer.apple.com/documentation/foundation/nsregularexpression)