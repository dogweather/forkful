---
date: 2024-01-26 03:42:21.611406-07:00
description: "Come fare: Swift ti permette di affrontare il compito di rimozione delle\
  \ virgolette in modo abbastanza pratico. Ecco un esempio rapido utilizzando\u2026"
lastmod: '2024-03-13T22:44:43.758518-06:00'
model: gpt-4-0125-preview
summary: Swift ti permette di affrontare il compito di rimozione delle virgolette
  in modo abbastanza pratico.
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Come fare:
Swift ti permette di affrontare il compito di rimozione delle virgolette in modo abbastanza pratico. Ecco un esempio rapido utilizzando `replacingOccurrences(of:with:)`, che fa esattamente ciò che sembra: scambia pezzi di testo con qualcos'altro, o nulla affatto.

```swift
var quotedString = "\"This is a 'quoted' string.\""
let unquotedString = quotedString.replacingOccurrences(of: "\"", with: "")
print(unquotedString) // This is a 'quoted' string.

// Problemi con le virgolette singole? Basta cambiare il termine di ricerca.
quotedString = "'Here's another example.'"
let singleQuoteRemoved = quotedString.replacingOccurrences(of: "'", with: "")
print(singleQuoteRemoved) // Heres another example.
```

L'output saranno stringhe prive di virgolette, pronte per qualsiasi cosa tu abbia in programma dopo.

## Approfondimento
Abbiamo "pulito" stringhe come queste sin dall'alba della programmazione. Nei primi tempi, si trattava più che altro di conservare la preziosa memoria ed evitare errori di sintassi nell'elaborare gli input. Saltiamo ai giorni nostri ed è questione di buona igiene dei dati, specialmente quando si lavora con JSON o si preparano stringhe per il lavoro con i database. Una virgoletta fuori posto può mandare in tilt le query SQL più velocemente di quanto tu possa dire "errore di sintassi".

Alternative? Beh, se trovi `replacingOccurrences(of:with:)` un po' troppo banale, potresti addentrarti nelle espressioni regolari per schemi più complessi o quando vuoi rimuovere le virgolette solo in certe posizioni. La classe `NSRegularExpression` di Swift è qui per aiutarti. Ma ricorda, le regex possono essere una spada a doppio taglio: potenti ma a volte eccessive.

Dal punto di vista dell'implementazione, `replacingOccurrences(of:with:)` è un metodo fornito da `String` in Swift, che internamente chiama funzioni di manipolazione di stringhe più complesse che gestiscono Unicode e altre intricatezze della moderna elaborazione del testo. È una di quelle situazioni "semplice in superficie, complessa sotto il cofano" che Swift gestisce affinché tu non debba farlo.

## Vedi Anche
Per maggiori informazioni sulle manipolazioni di stringhe in Swift:

- Il Linguaggio di Programmazione Swift (Stringhe e Caratteri): [Documentazione Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- NSRegularExpression: [Documentazione per gli Sviluppatori Apple](https://developer.apple.com/documentation/foundation/nsregularexpression)

E se ora sei curioso sulle espressioni regolari e vuoi testare i tuoi schemi:

- Regex101: [Tester e Debugger di Regex](https://regex101.com)
