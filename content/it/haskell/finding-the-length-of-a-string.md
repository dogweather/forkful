---
title:                "Trova la lunghezza di una stringa."
html_title:           "Haskell: Trova la lunghezza di una stringa."
simple_title:         "Trova la lunghezza di una stringa."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte applicazioni pratiche per sapere la lunghezza di una stringa, come la gestione dei limiti di caratteri in un campo dati o la formattazione di testi in base alla lunghezza.

## Come Fare

Per trovare la lunghezza di una stringa in Haskell, si può utilizzare la funzione `length` che è inclusa nella libreria standard. Ecco un esempio:

```Haskell
lunghezza :: String -> Int -- Specifica il tipo di input e di output
lunghezza str = length str -- Utilizza la funzione length per trovare la lunghezza di una stringa
```

Esempio di output:

```
lunghezza "Ciao mondo" -- Output: 11 (la lunghezza della stringa è 11)
```

## Approfondimento

La funzione `length` in Haskell è implementata tramite la ricorsione, ovvero si chiama ripetutamente la funzione su stringhe sempre più corte fino ad arrivare alla stringa vuota, che ha una lunghezza di 0. È interessante notare che questa implementazione sfrutta il concetto di *pattern matching* per gestire tutti i casi possibili, come una stringa vuota o una stringa con un solo carattere.

## Vedi Anche

- Documentazione ufficiale della funzione `length`: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:length
- Tutorial su come trovare la lunghezza di una stringa in Haskell: https://www.tutorialspoint.com/haskell/haskell_string_length.htm