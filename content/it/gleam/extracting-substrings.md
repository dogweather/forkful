---
title:                "Gleam: Estrazione di sottostringhe"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

L'estrazione di sottostringhe è un'operazione utile durante la manipolazione di stringhe in Gleam. Permette di estrarre parti specifiche di una stringa per utilizzarle in altre operazioni o per analizzarle in modo più dettagliato.

## Come Fare

Per estrarre una sottostringa in Gleam, possiamo utilizzare la funzione `String.sub`, specificando la stringa originale, l'indice di inizio e la lunghezza della sottostringa desiderata.

```Gleam
let stringa_originale = "Ciao a tutti!"
let sottostringa = String.sub(stringa_originale, 5, 3)
```

In questo caso, la sottostringa sarebbe "a t".

## Approfondimento

La funzione `String.sub` è molto utile per estrarre parti specifiche di una stringa, ma è importante tenere a mente che l'indice di inizio include anche lo spazio vuoto all'inizio della stringa.

Per estrarre una sottostringa senza lo spazio vuoto, possiamo utilizzare la funzione `String.trim`. Inoltre, possiamo anche specificare un indice di fine anziché la lunghezza della sottostringa desiderata.

```Gleam
let stringa_originale = "Ciao a tutti!"
let sottostringa = stringa_originale |> String.trim |> String.sub(4, 2)
```

In questo caso, la sottostringa sarebbe "a t", senza lo spazio vuoto all'inizio.

## Vedi Anche

- [Documentazione di Gleam sulla gestione delle stringhe](https://gleam.run/documentation/stdlib/string/)
- [Articolo su come utilizzare le funzioni di manipolazione delle stringhe in Gleam](https://www.blogdelsito.com/manipolazione-stringhe-gleam/)