---
title:                "Capitalizzare una stringa"
html_title:           "Swift: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La capitalizzazione di una stringa è il processo di conversione del primo carattere di ogni parola all'interno della stringa in maiuscolo. I programmatori lo fanno spesso per motivi di presentazione, per migliorare la leggibilità e per seguire le convenzioni della lingua inglese.

## Come fare:

Ecco un esempio su come capitalizzare una stringa in Swift:

```Swift
let lowerCaseString = "un esempio di frase in lowercase"
let capitalizedString = lowerCaseString.capitalized
print(capitalizedString)
```

Questo produrrà l'output:

```Swift
"Un Esempio Di Frase In Lowercase"
```

## Approfondimento

1) Contesto storico: la funzione 'capitalized' esiste in Swift dalla sua prima versione, pubblicata nel 2014. E' stata introdotta per agevolare gli sviluppatori a manipolare le stringhe in modo efficiente.

2) Alternative: oltre alla funzione 'capitalized', esistono altri metodi per alterare le stringhe. Alcuni di questi includono 'uppercased()' per rendere tutte le lettere in maiuscolo e 'lowercased()' per farle tutte in minuscolo.

3) Dettagli di implementazione: la funzione 'capitalized' in Swift utilizza le convenzioni Unicode per determinare quali caratteri sono considerati iniziali di parola. Questo significa che funzionerà correttamente anche con stringhe che contengono caratteri non ascii.

## Guarda anche

Se sei interessato ad approfondire, ecco alcuni link utili:

- [String - Apple Developer Documentation](https://developer.apple.com/documentation/swift/string)
- [Capitalizing Strings in Swift - Blog post](https://nshipster.com/string/)
- [Manipulating Strings in Swift - Swift by Sundell](https://www.swiftbysundell.com/basics/strings/) 

Bene, ora sai come capitalizzare le stringhe in Swift! Ricordati sempre di utilizzare questi strumenti per migliorare la leggibilità del tuo codice. Buona programmazione!