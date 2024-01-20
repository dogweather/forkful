---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
La ricerca e la sostituzione del testo è l'operazione di trovare una porzione di testo in una stringa e sostituirla con un altro testo. I programmatori lo fanno per manipolare i dati e personalizzare l'output.

## Come Fare:
In Elm, la funzione `String.replace` aiuta a cercare e sostituire il testo. Ecco un esempio:

```Elm
import String

main =
   let
      testo = "Ciao, Mondo"
      nuovoTesto = String.replace "Mondo" "Elm" testo
  in
   text nuovoTesto
```

Questo produrrà l'output:

```Elm
"Ciao, Elm"
```

## Approfondimento:
(1) Contesto storico: Elm è un linguaggio di programmazione funzionale per il front-end web. Non ha ereditato la ricerca e la sostituzione del testo da un predecessore, come molti linguaggi, bensì l'ha implementata in base alla sua necessità.

(2) Alternative: In Elm, è possibile usare la funzione `String.split` per dividere una stringa in una lista di stringhe e poi combinare la lista con la nuova stringa utilizzando `String.join`.

```Elm
import String exposing (split, join)

main =
   let
     testo = "Ciao, Mondo"
     nuoveParole = split "Mondo" testo
     nuovoTesto = join "Elm" nuoveParole
 in
    text nuovoTesto
```

(3) Dettagli Implementativi: Il modulo `String` di Elm utilizza un algoritmo di matching molto efficiente che può gestire anche stringhe di grandi dimensioni.

## Vedere Anche:
Per ulteriori informazioni sulla ricerca e la sostituzione del testo in Elm, visita:

- [Documentazione Ufficiale Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)