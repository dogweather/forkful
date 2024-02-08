---
title:                "Trovare la lunghezza di una stringa"
aliases:
- it/swift/finding-the-length-of-a-string.md
date:                  2024-01-20T17:48:33.683215-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trovare la lunghezza di una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
In Swift, conoscere la lunghezza di una stringa significa sapere quanti caratteri contiene. I programmatori lo fanno per validare input, manipolare testo, e per gestire la visualizzazione dei dati.

## How to:
Calcolare la lunghezza di una stringa in Swift è semplice. Ecco come:

```Swift
let saluto = "Ciao, mondo!"
let lunghezza = saluto.count

print(lunghezza) // Output: 12
```

Il codice `saluto.count` restituisce `12`, perché ci sono 12 caratteri nella stringa "Ciao, mondo!".

## Deep Dive
Historicamente, trovare la lunghezza di una stringa poteva essere più complicato, considerando che in linguaggi come C si usa un terminatore di stringa nullo ('\0'). In Swift, `count` rende tutto più semplice e diretto.

Bisogna notare che le stringhe in Swift sono basate su Unicode, il che significa che `count` restituisce il numero di `Character` di Swift, che possono essere composti da più `UnicodeScalar`. Quindi, la lunghezza potrebbe non corrispondere al numero di `code points` Unicode o al numero di unità di codifica specifiche, come UTF-16.

Inoltre, ci sono metodi alternativi per ottenere info sulle stringhe. Per esempio, se stai lavorando con un'API che richiede la lunghezza in UTF-16, puoi usare `utf16.count`:

```Swift
let bandiera = "🇮🇹"
print(bandiera.count) // Output: 1
print(bandiera.utf16.count) // Output: 4
```

La bandiera è un singolo `Character` in Swift, ma è composta da più unità UTF-16.

## See Also
- La documentazione ufficiale di Swift sulle stringhe e i caratteri: [Strings and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Il tutorial di Ray Wenderlich su Swift Strings: [Working with Strings](https://www.raywenderlich.com/5539282-working-with-strings-in-swift)
- Unicode Standard per capire come Swift gestisce i diversi caratteri e simboli: [The Unicode Standard](http://www.unicode.org/standard/standard.html)
