---
title:                "Trova la lunghezza di una stringa"
html_title:           "Gleam: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Cosa è & Perché?

Calcolare la lunghezza di una stringa significa determinare quanti caratteri sono contenuti all'interno di essa. È un'operazione utile per i programmatori in quanto spesso è necessario conoscere le dimensioni di una stringa per poterla elaborare correttamente.

## Come fare:

```Gleam
let stringa = "Ciao, mondo!"
let lunghezza = std.string.length(stringa)
```
Output:
```Gleam
12
```

## Approfondimento:

**Contesto storico:** Il concetto di trovare la lunghezza di una stringa risale ai primi anni della programmazione, quando i computer erano in grado di gestire solo una limitata quantità di caratteri. Oggi, grazie alla potenza di elaborazione dei moderni computer, questi limiti sono stati superati. Tuttavia, trovare la lunghezza di una stringa rimane ancora un'operazione comune e importante nella programmazione.

**Alternative:** In alcune programmazioni, l'operazione di trovare la lunghezza di una stringa può essere sostituita da altre funzioni, come ad esempio counting o indexing. Tuttavia, calcolare esplicitamente la lunghezza rimane una pratica comune e raccomandata per mantenere il codice leggibile e facile da comprendere.

**Dettagli di implementazione:** In Gleam, la funzione `std.string.length` è parte della libreria standard e utilizza un'ottimizzazione per determinare la lunghezza della stringa in modo efficiente. Ciò è possibile grazie alla rappresentazione efficiente delle stringhe in Gleam.

## Vedi anche:

- Documentazione ufficiale di Gleam su `std.string.length`: https://gleam.run/modules/std.string.html#length
- Articolo su stringhe e loro manipolazione: https://stackoverflow.com/a/1045225/15417679
- Tutorial su come usare stringhe in Gleam: https://medium.com/@gleamlang/string-manipulation-in-gleam-952d61d2d4d2