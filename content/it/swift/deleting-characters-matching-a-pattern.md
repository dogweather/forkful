---
title:                "Eliminazione dei caratteri corrispondenti a un modello"
html_title:           "PowerShell: Eliminazione dei caratteri corrispondenti a un modello"
simple_title:         "Eliminazione dei caratteri corrispondenti a un modello"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Cancellare i caratteri corrispondenti a un modello in Swift

## Che cosa e perché?

Cancellare caratteri corrispondenti a un modello significa eliminare specifici caratteri da una stringa basandosi su una regola o un "modello". I programmatori lo fanno per pulire o manipolare i dati.

## Come fare:

Ecco un esempio di come cancellare i caratteri corrispondenti a un modello in Swift:

```Swift
let string = "Ciao Mondo123"
let modello = "[0-9]" // vogliamo eliminare tutti i numeri
let risultato = string.replacingOccurrences(of: modello, with: "", options: .regularExpression)
print(risultato) // Stampa: "Ciao Mondo"
```

In questo caso, il modello sta cercando qualsiasi numero da 0 a 9 nella stringa. Quindi, tutti i numeri vengono eliminati dalla stringa.

## Approfondimento

**Contesto storico**: la possibilità di eliminare caratteri corrispondenti a un modello è una funzione che esiste da molto tempo nei linguaggi di programmazione, derivata dal concetto di espressioni regolari (o regex).

**Alternative**: Swift fornisce altre funzioni per la manipolazione delle stringhe, come `removeAll(where:)` che può essere usato per ottenere un risultato simile se non è necessario un pattern specifico.

**Dettagli implementativi**: La funzione `replacingOccurrences(of:with:options:)` ricerca tutte le occorrenze del modello e le sostituisce con la stringa fornita. Se non si desidera sostituire con nulla, è possibile fornire una stringa vuota "".

## Vedere anche:

- Espressioni regolari in Swift: [NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- Funzioni di manipolazione delle stringhe in Swift: [String](https://developer.apple.com/documentation/swift/string)
- Modelli di caratteri in regex: [Character Classes in Regular Expressions](https://www.regular-expressions.info/charclass.html)