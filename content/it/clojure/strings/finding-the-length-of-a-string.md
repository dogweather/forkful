---
aliases:
- /it/clojure/finding-the-length-of-a-string/
date: 2024-01-20 17:47:03.099965-07:00
description: "Trovare la lunghezza di una stringa significa contare il numero di caratteri\
  \ che contiene. I programmatori lo fanno per convalidare l'input, troncare il\u2026"
lastmod: 2024-02-18 23:08:55.552596
model: gpt-4-1106-preview
summary: "Trovare la lunghezza di una stringa significa contare il numero di caratteri\
  \ che contiene. I programmatori lo fanno per convalidare l'input, troncare il\u2026"
title: Trovare la lunghezza di una stringa
---

{{< edit_this_page >}}

## What & Why?
Trovare la lunghezza di una stringa significa contare il numero di caratteri che contiene. I programmatori lo fanno per convalidare l'input, troncare il testo, fare confronti e per molteplici altri motivi.

## How to:
Usare la funzione `count` per ottenere la lunghezza di una stringa:

```Clojure
(count "Ciao, mondo!") ; => 12
(count "") ; => 0
```

La funzione `count` è diretta, dà il numero dei caratteri. Facile, no?

## Deep Dive
In Clojure, `count` è l'approccio idiomatico per trovare la lunghezza di una stringa. Risale alle origini funzionali del linguaggio, dove operare su strutture dati come liste e sequenze è centrale.

Alternative? Ne esistono poche, dato che `count` è così integrata nel linguaggio. 

Dettagli? `count` è O(1) per stringhe e molte altre strutture dati perché Clojure le gestisce internamente in modo tale che la lunghezza sia facilmente accessibile senza doverle percorrere.

## See Also
Per approfondire, visita la documentazione ufficiale di `count`: 
- [Clojure - count](https://clojuredocs.org/clojure.core/count)

Interessati all'uso delle stringhe in Clojure? 
- [Clojure - String manipulation](https://clojuredocs.org/clojure.string)
