---
title:                "Trovare la lunghezza di una stringa"
html_title:           "Arduino: Trovare la lunghezza di una stringa"
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è & Perché?

Trova la lunghezza di una stringa significa calcolare il numero di caratteri in essa. I programmatori ne hanno bisogno per operazioni come la validazione del input, la manipolazione dei dati e l'implementazione delle logiche dell'interfaccia utente.

## Come fare:

Per trovare la lunghezza di una stringa in Elm, si utilizza la funzione `String.length`. Ecco un esempio:

```Elm
lunghezza : String -> Int
lunghezza stringa = String.length stringa

-- Uso esempio
main =
    let
        testString = "Programma in Elm"
    in
    Html.text (String.fromInt (lunghezza testString))
```

In questo esempio, "Programma in Elm" ha 16 caratteri, quindi l'output sarà "16".

## Approfondimenti

1) Contesto storico: In Elm, la funzione `String.length` è disponibile sin dalla versione 0.18. Essa è essenziale nella manipolazione delle stringhe e è un concetto comune in molti altri linguaggi di programmazione.

2) Alternative: In Elm, non ci sono molte alternative alla funzione `String.length`. Tuttavia, puoi scrivere una funzione ricorsiva per calcolare la lunghezza di una stringa, ma in generale, `String.length` è più efficiente e semplice da usare.

3) Dettagli implementativi: `String.length` in Elm conta i caratteri Unicode nella stringa. Ciò significa che restituisce il numero di codici punti Unicode, non il numero di unità di codice UTF-16.

## Vedere anche:

1) La documentazione ufficiale di Elm sulla stringa e sulle sue funzioni: https://package.elm-lang.org/packages/elm/core/latest/String

2) Per approfondire la manipolazione delle stringhe in Elm: https://elmprogramming.com/strings.html

3) Per informazioni generali sull'UTF-16 e sui codici punti Unicode: https://unicode.org/glossary/