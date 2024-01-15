---
title:                "Trova la lunghezza di una stringa"
html_title:           "Elm: Trova la lunghezza di una stringa"
simple_title:         "Trova la lunghezza di una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

##Perché

Se stai scrivendo un programma o un'applicazione in Elm, potresti aver bisogno di trovare la lunghezza di una stringa. Ciò è utile per molte ragioni, ad esempio quando si tratta di gestire input dell'utente o di manipolare dati.

## Come fare

Per trovare la lunghezza di una stringa, possiamo utilizzare la funzione `String.length`. Ad esempio:

```
Elm String.length "Ciao, mondo!"
```
Questo codice restituirà il valore 13, poiché la stringa "Ciao, mondo!" è composta da 13 caratteri.

Inoltre, è possibile utilizzare lo stesso approccio per ottenere la lunghezza di una lista di stringhe. Basta passare la lista all'interno della funzione `String.length` come argomento.

```
Elm String.length ["Ciao", "mondo"]
```

Questo restituirà il valore 2, poiché la lista contiene due elementi.

## Approfondimento 

La funzione `String.length` in realtà si basa sulla funzione `String.toList`, che converte la stringa in una lista di caratteri e poi restituisce la lunghezza di tale lista. Ciò significa che la funzione `String.length` funziona anche per trovare la lunghezza di una stringa unicode.

Inoltre, se hai bisogno di trovare la lunghezza di una stringa ignorando gli spazi vuoti all'inizio e alla fine, puoi utilizzare la funzione `String.trim` prima di applicare la funzione `String.length`.

## Vedi anche

- Documentazione ufficiale di Elm per la funzione String.length: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Tutorial su come utilizzare le stringhe in Elm: https://guide.elm-lang.org/strings/
- Altri articoli su Elm su [nome del blog]: [link del blog]