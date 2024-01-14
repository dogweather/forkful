---
title:                "Elm: Ricerca e sostituzione di testo"
programming_language: "Elm"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore o sviluppatore di software, molto probabilmente hai già familiarità con il concetto di "cerca e sostituisci" nel tuo codice. Ma perché dovresti dedicare il tuo tempo a fare questo invece di utilizzare altre funzionalità? La risposta è semplice: la ricerca e sostituzione ti permette di modificare rapidamente e facilmente parti specifiche del tuo codice, risparmiando tempo e fatica.

## Come fare

Per fare una ricerca e sostituzione in Elm, devi utilizzare la funzione `String.replace`. Questa funzione prende tre argomenti: la stringa originale, la parte di stringa che vuoi cercare e la parte di stringa con cui vuoi sostituire quella che hai trovato.

```Elm
String.replace "Ciao mondo!" "mondo" "amico"      -- Output: "Ciao amico!"
```

Puoi anche specificare una stringa di sostituzione vuota per eliminare completamente la parte trovata.

```Elm
String.replace "Ciao mondo!" "mondo" ""           -- Output: "Ciao !"
```

## Approfondimento

Se vuoi approfondire ancora di più la ricerca e sostituzione nel tuo codice Elm, puoi anche utilizzare espressioni regolari. Ciò ti permette di effettuare ricerche più avanzate, come cercare parti di stringa che rispettano uno schema specifico. Per farlo, dovrai utilizzare la funzione `String.regexReplace`.

```Elm
String.regexReplace (Regex.regex "a.*o") "Ciao mondo!" "amico"    -- Output: "Ciao amico!"
```

## Vedi anche

- [Documentazione delle funzioni di ricerca e sostituzione in Elm](https://package.elm-lang.org/packages/elm/core/latest/String#replace)
- [Tutorial di espressioni regolari in Elm](https://medium.com/@lorenzoguaragna/introduction-to-regular-expressions-with-elm-27b21a9aaaf4)