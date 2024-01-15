---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Gleam: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capire la lunghezza di una stringa è un'abilità fondamentale nella programmazione. Conoscere il numero di caratteri in una stringa può aiutare a manipolarla e lavorare con essa in modo più efficiente. Inoltre, molte funzioni e librerie richiedono di conoscere la lunghezza di una stringa come argomento, quindi è un'abilità indispensabile per scrivere codici funzionanti.

## Come fare

Per trovare la lunghezza di una stringa in Gleam, possiamo utilizzare la funzione `String.length`, che restituisce il numero di caratteri in una stringa. Vediamo un esempio:

```Gleam
let stringa = "Programmare è divertente!"
let lunghezza = String.length(stringa)

IO.print("La lunghezza della stringa è ", lunghezza)
```

Output:

```
La lunghezza della stringa è 24
```

Come possiamo vedere, la funzione `String.length` ha restituito un valore intero corrispondente alla lunghezza della stringa.

Possiamo anche utilizzare la libreria `Str` per trovare la lunghezza di una stringa usando il metodo `Str.length`:

```Gleam
let stringa = "Buongiorno"
let lunghezza = Str.length(stringa)

IO.print("La lunghezza della stringa è ", lunghezza)
```

Output:

```
La lunghezza della stringa è 10
```

## Approfondimento

Per trovare la lunghezza di una stringa, Gleam itera su ogni carattere della stringa e conta il numero totale di caratteri. Ciò significa che la funzione `String.length` ha una complessità lineare di O(n), dove n è il numero di caratteri nella stringa.

Una considerazione importante da tenere a mente è che la lunghezza di una stringa può essere influenzata dalla codifica utilizzata. Ad esempio, caratteri speciali come accenti o emoji possono essere considerati come un singolo carattere o come una combinazione di più caratteri. Pertanto, è importante avere una buona comprensione della codifica utilizzata per ottenere risultati accurati quando si cerca la lunghezza di una stringa.

## Vedi anche

- Documentazione di Gleam sulle stringhe: https://gleam.run/modules/gleam/str
- Documentazione di Gleam sui caratteri: https://gleam.run/modules/gleam/char