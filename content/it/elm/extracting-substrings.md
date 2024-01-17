---
title:                "Estrarre sottostringhe"
html_title:           "Elm: Estrarre sottostringhe"
simple_title:         "Estrarre sottostringhe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Estrarre le sottostringhe è il processo di selezione di una parte specifica di una stringa più grande. Questo è utile per ottenere informazioni rilevanti da una stringa più grande, come l'URL di un sito web o il nome di un utente. I programmatori spesso utilizzano questa tecnica per manipolare o analizzare dati.

## Come:
```Elm
input = "Ciao, mi chiamo Marco."

// Estrarre le prime tre lettere
substring 0 3 input // Output: "Cia"

// Estrarre il cognome
substring 11 (String.length input - 1) input // Output: "Marco"
```

## Approfondimento:
L'estrattore di sottostringhe è stato introdotto in molti linguaggi di programmazione sin dagli albori. Tuttavia, a seconda del linguaggio, ci possono essere differenze nelle funzioni e nella sintassi utilizzate. Inoltre, esistono anche alternative come i regex o la manipolazione delle stringhe mediante l'utilizzo di array.

## Vedi Anche:
- [Documentazione di Elm su come estrarre sottostringhe](https://package.elm-lang.org/packages/elm-lang/core/latest/String#substring)
- [Tutorial su come utilizzare i regex in Elm](https://elmprogramming.com/regex-tutorial.html)
- [Articolo su come manipolare stringhe con array in Elm](https://www.codewall.co.uk/elm-strings-and-arrays/)