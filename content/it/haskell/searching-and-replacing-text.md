---
title:                "Haskell: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori si trovano spesso a dover manipolare e modificare grandi quantità di testo. Invece di farlo manualmente, la ricerca e la sostituzione di testo sono strumenti molto potenti e efficienti per automatizzare questo processo.

## Come Fare

In Haskell, la funzione "Prelude.uncurry re" può essere utilizzata per cercare e sostituire porzioni di testo all'interno di una stringa. Ecco un esempio di codice che sostituisce tutte le occorrenze di "cane" con "gatto" in una stringa:
 
```Haskell
Prelude>uncurry re "Il mio cane è adorabile."
"Il mio gatto è adorabile."
```

Inoltre, è anche possibile utilizzare la funzione "substitute" del pacchetto "text" per effettuare la sostituzione in una stringa specifica.

## Approfondimento

La ricerca e la sostituzione di testo in Haskell si basano sull'utilizzo di espressioni regolari. Queste espressioni sono pattern che descrivono un insieme di stringhe che si desidera cercare e sostituire. Per esempio, il pattern "cane" corrisponde a qualsiasi stringa che contiene la parola "cane".

È possibile utilizzare diversi simboli e quantificatori per creare espressioni regolari più complesse e specifiche. Ad esempio, il simbolo "+" indica che il carattere precedente deve essere presente almeno una volta, mentre "*" indica che il carattere deve essere presente zero o più volte.

La ricerca e la sostituzione di testo in Haskell è un processo potente e flessibile, che può essere utilizzato per automatizzare una varietà di attività di manipolazione del testo.

## Vedi Anche

* [Prelude.uncurry re](https://hackage.haskell.org/package/re/docs/Data-Regex.html)
* [Funzioni per la manipolazione del testo nel pacchetto "text"](https://hackage.haskell.org/package/text/docs/Data-Text.html)
* [Introduzione alle espressioni regolari in Haskell](https://wiki.haskell.org/Regular_expressions)