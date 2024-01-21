---
title:                "Conversione di una stringa in minuscolo"
date:                  2024-01-20T17:39:41.315361-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Convertire una stringa in minuscolo significa semplicemente trasformare tutti i caratteri di una stringa in lettere minuscole. I programmatori lo fanno per uniformare i dati, specialmente per confronti e ricerche non sensibili alla differenza tra maiuscole e minuscole.

## How to:
Ecco come si fa in Swift:

```swift
let testo = "Ciao Mondo!"
let testoMinuscolo = testo.lowercased()

print(testoMinuscolo) // "ciao mondo!"
```

Output:
```
ciao mondo!
```

## Deep Dive
Convertire il testo in minuscolo ha radici nella necessità di normalizzare dati testuali. Nelle prime fasi dell'informatica, la distinzione tra maiuscole e minuscole poteva portare a inconsistenze, soprattutto con limitazioni nelle capacità di memorizzazione e trasmissione.

Alternative includono `localizedLowercase`, che rispetta le specificità locali:

```swift
let saluto = "İstanbul"
let salutoMinuscolo = saluto.lowercased() // "i̇stanbul"
let salutoMinuscoloLocalizzato = saluto.localizedLowercased() // "istanbul"
```

L'implementazione interna usa le mappature Unicode per la conversione dei caratteri. Dettagli importanti soprattutto per lingue con regole di minuscolo/maiuscolo particolari.

## See Also
- [Unicode Standard](https://www.unicode.org/standard/standard.html)
- [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)