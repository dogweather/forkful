---
title:                "Eliminazione di caratteri corrispondenti a un pattern"
html_title:           "Swift: Eliminazione di caratteri corrispondenti a un pattern"
simple_title:         "Eliminazione di caratteri corrispondenti a un pattern"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
L'eliminazione di caratteri corrispondenti a uno schema è l'azione di rimuovere dal testo qualsiasi carattere che soddisfi una specifica condizione. I programmatori spesso lo fanno per pulire il testo da caratteri indesiderati o per formattarlo in un modo specifico.

## Come fare:
```Swift
let testo = "Questo è un esempio!@@ di testo^ con caratteri non desiderati$$$"
let caratteriIndesiderati = CharacterSet(charactersIn: "!@#$%^&*()_+{}:\"<>?[]',.\\/")

let testoPulito = testo.components(separatedBy: caratteriIndesiderati).joined()
print(testoPulito) // Stampa "Questo è un esempio di testo con caratteri non desiderati"
```

## Approfondimento:
L'eliminazione di caratteri corrispondenti a uno schema è stata una pratica comune fin dai primi tempi della programmazione, in particolare nei linguaggi di basso livello come il C. Esistono anche altri modi per ottenere lo stesso risultato, ad esempio utilizzando espressioni regolari.

## Vedi anche:
- [La documentazione di Swift su CharacterSet](https://developer.apple.com/documentation/foundation/characterset)
- [Un articolo di Medium con altri esempi su come rimuovere caratteri da una stringa in Swift](https://medium.com/@shubhammehrotra/working-with-characters-in-swift-3-3082c1ddf58)