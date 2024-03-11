---
date: 2024-01-20 17:46:50.440239-07:00
description: "Estrarre delle sottostringhe significa prelevare una porzione specifica\
  \ di una stringa di testo. I programmatori lo fanno per manipolare, cercare, o\u2026"
lastmod: '2024-03-11T00:14:17.376091-06:00'
model: gpt-4-1106-preview
summary: "Estrarre delle sottostringhe significa prelevare una porzione specifica\
  \ di una stringa di testo. I programmatori lo fanno per manipolare, cercare, o\u2026"
title: Estrazione di sottostringhe
---

{{< edit_this_page >}}

## What & Why?
Estrarre delle sottostringhe significa prelevare una porzione specifica di una stringa di testo. I programmatori lo fanno per manipolare, cercare, o processare solo la parte di dati che interessa.

## How to:
Ecco come estrarre sottostringhe in Swift. Prendi una stringa. Trova l’indice di partenza e quello di arrivo. Preleva la sottostringa.

```Swift
let frase = "Ciao, mondo di Swift!"
let inizio = frase.index(frase.startIndex, offsetBy: 6)
let fine = frase.index(frase.startIndex, offsetBy: 10)
let sottostringa = frase[inizio...fine]

print(sottostringa) // "mondo"
```
In Swift puoi anche usare metodi come `prefix()`, `suffix()`, e gli operatori di slicing per estrarre parti di una stringa.

```Swift
let saluto = frase.prefix(4)
print(saluto) // "Ciao"

let conclusione = frase.suffix(6)
print(conclusione) // "Swift!"

let intervallo = frase[inizio..<fine]
print(intervallo) // "mond"
```

## Deep Dive
Nelle prime versioni di Swift, estrarre sottostringhe era un po' più laborioso. Con le evoluzioni della linguaggio, si è data importanza all'efficienza della gestione della memoria. Quando estrarrai una sottostringa, otterrai un `Substring` invece che una nuova `String`. Questo aiuta a condividere la memoria usata dalla stringa originale, risparmiando risorse. È possibile convertire un `Substring` in una `String` se hai bisogno che la sottostringa viva oltre il contesto attuale. Ricorda che l'uso eccessivo di indici può confondere, quindi considera l'utilizzo di metodi più espressivi come `split(separator:)` quando possibile.

## See Also
Leggi la documentazione ufficiale di Swift sulle stringhe e i caratteri: [Swift Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html). 

Consulta le API di `Substring` per capire come lavorano con le stringhe: [Swift Substring](https://developer.apple.com/documentation/swift/substring).

Esplora `Foundation` per funzioni di alto livello come `NSString` metodi che possono aiutarti: [NSRange](https://developer.apple.com/documentation/foundation/nsrange).
