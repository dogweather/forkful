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

## Cos'è e perché?
Trovare la lunghezza di una stringa è un'operazione comune in programmazione. Questo ci permette di sapere quanti caratteri ci sono all'interno di una stringa, che può essere utile in molte situazioni.

## Come fare:
Per trovare la lunghezza di una stringa in Elm, possiamo utilizzare la funzione `String.length`. Ad esempio:

```Elm
String.length "Hello!" -- output: 6
```

Possiamo anche utilizzare questa funzione con una variabile che contiene una stringa, come nel seguente esempio:

```Elm
let
  message = "This is a long message!"
in
  String.length message -- output: 24
```

## Approfondimento:
La ricerca della lunghezza di una stringa risale alla nascita dei primi linguaggi di programmazione. In altri linguaggi, come JavaScript, possiamo utilizzare il metodo `length` per ottenere la lunghezza di una stringa.

Esistono anche altre potenziali soluzioni per ottenere la lunghezza di una stringa in Elm, come l'utilizzo della funzione `String.split` e il conteggio degli elementi restituiti.

## Vedi anche:
- Documentazione ufficiale di Elm sulla funzione `String.length`: https://package.elm-lang.org/packages/elm/core/latest/String#length
- Una guida su come trovare la lunghezza di una stringa in altri linguaggi di programmazione: https://www.digitalocean.com/community/tutorials/how-to-find-the-length-of-a-string-in-a-variety-of-programming-languages