---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Haskell: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Il trovare la lunghezza di una stringa è l'operazione di conteggio dei caratteri presenti nella stessa. Questa operazione è molto utile per i programmatori, per esempio, nel validare gli input utente.

## Come fare:

In Swift, possiamo trovare la lunghezza di una stringa usando la proprietà `count`. L'esempio seguente mostra come farlo:

```Swift
let s = "Oggi è bello"
let lunghezza = s.count
print(lunghezza)  // Stampa: 11
```

La stringa "Oggi è bello" contiene 11 caratteri incluse le spaziature, quindi l'output sarà 11.

## Approfondimento:
Anche se potrebbe sembrare una cosa semplice, in realtà la misurazione della lunghezza delle stringhe ha avuto una certa evoluzione nel corso del tempo. Infatti, in passato, i programmatori dovevano fare i conti con i byte invece dei caratteri. Con l'avvento di Unicode e gli standard successivi, la situazione è diventata molto più gestibile. 

Come alternativa alla proprietà `count`, in Swift è possibile accedere ai dati grezzi dei byte utilizzando la proprietà `utf8`. Questo però, non restituisce il conteggio corretto dei caratteri per le stringhe che contengono emoji o altri caratteri multibyte.

In termini di prestazioni, `count` è un'operazione di costo O(1), grazie all'implementazione di Swift delle stringhe. Questo significa che non importa quanto sia lunga la stringa, il tempo per trovare la sua lunghezza è costante.

## Vedere anche:

Per saperne di più su come gestire le stringhe in Swift, consulta i seguenti link:

- La documentazione ufficiale di Swift sulle Stringhe e i Caratteri: [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html).

- Un tutorial dettagliato sulle stringhe in Swift: [Swift Strings](https://www.hackingwithswift.com/read/0/5/string-interpolation).