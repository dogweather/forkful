---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

L'estrazione di sottostringhe è un processo per ottenere una stringa più piccola da una stringa più grande. I programmatori lo fanno per ispezionare, manipolare o analizzare dati specifici all'interno di una stringa più ampia.

## Come fare:

Ecco un esempio basilare per estrarre una sottostringa in Swift.

```Swift
let s = "Benvenuti a Swift!"
let fine = s.index(s.startIndex, offsetBy: 8)
let sottostringa = s[..<fine]

print(sottostringa)  // Stampa "Benvenut"
```

Qui, abbiamo deciso di estrarre la sottostringa dai primi 8 caratteri della stringa principale.

## Approfondimento

Sottostringhe in Swift sono un'innovazione rispetto alle generazioni precedenti di linguaggi di programmazione, che richiedevano l'allocazione di nuovi spazi di memoria per ogni sottostringa. In Swift, una sottostringa condivide la stessa memoria della stringa originale, rendendo l'operazione più efficiente.

Tuttavia, esistono altri modi per estrarre sottostringhe, come utilizzare metodi di estensione String. Ad esempio, si potrebbe creare un'estensione che utiliza `NSRange` per estrarre una sottostringa da una posizione specifica. 

Dettagli implementativi specifici possono variare, ma la semplicità e l'efficienza della gestione delle stringhe in Swift rimane un vantaggio fondamentale.

## Vedi anche

Per ulteriori informazioni su come lavorare con stringhe e sottostringhe in Swift, consultare i seguenti collegamenti:

1. Documentazione Apple su [String and Character](https://developer.apple.com/documentation/swift/string_and_characters) (Inglese)
2. Guida rapida a [Substring in Swift](https://learnappmaking.com/substring-swift-how-to/) (Inglese)
3. Tutorial su [How to Split a String into an Array in Swift](https://www.hackingwithswift.com/example-code/strings/how-to-split-a-string-into-an-array-swift-strings) (Inglese)