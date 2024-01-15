---
title:                "Cercare e sostituire testo"
html_title:           "Elm: Cercare e sostituire testo"
simple_title:         "Cercare e sostituire testo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore, probabilmente passi gran parte della tua giornata a scrivere e modificare codice. E a volte, devi effettuare modifiche su un gran numero di file o linee di codice. In questi casi, può essere molto utile utilizzare la funzionalità di ricerca e sostituzione del testo.

## Come fare

Per effettuare una ricerca e sostituzione del testo in Elm, è necessario utilizzare la funzione `replace` del modulo `String`. Questa funzione accetta tre parametri: la stringa da cercare, la stringa con cui sostituirla e la stringa in cui effettuare la ricerca. Ad esempio:

```
Elm.replace "gatto" "cane" "Mi piace il mio gatto." -- "Mi piace il mio cane."
```

Se vuoi effettuare la sostituzione solo sulla prima occorrenza, puoi utilizzare la funzione `replaceFirst` al posto di `replace`.

```
Elm.replaceFirst "casa" "barbapapà" "La mia casa è grande, ma la tua è ancora più grande." -- "La mia barbapapà è grande, ma la tua è ancora più grande."
```

Inoltre, è possibile utilizzare espressioni regolari per una ricerca e sostituzione più avanzata utilizzando il modulo `Regex`.

## Approfondimento

Quando utilizzi la funzione `replace` di Elm, è importante ricordare che la stessa stringa di input viene restituita se non viene trovata alcuna corrispondenza. Inoltre, la funzione `replaceFirst` restituisce sempre una stringa diversa dalla stringa di input, anche se non viene trovata alcuna corrispondenza.

## Vedi anche

- Documentazione di Elm sul modulo `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Documentazione di Elm sul modulo `Regex`: https://package.elm-lang.org/packages/elm/regex/latest/Regex
- Articolo su come utilizzare le espressioni regolari in Elm: https://giulioscuro.medium.com/a-beginners-guide-to-regular-expressions-with-elm-d22da77cbd4d