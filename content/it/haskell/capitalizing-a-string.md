---
title:                "Capitalizzare una stringa"
html_title:           "Haskell: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Capitalizzare una stringa significa modificare la prima lettera di ogni parola in maiuscolo, come ad esempio "ciao mondo" diventa "Ciao Mondo". I programmatori fanno questo per rendere più leggibile il codice o per rispettare convenzioni di stile di un progetto.

## Come fare:
Il modo più semplice per capitalizzare una stringa in Haskell è utilizzare la funzione `toUpper` del modulo `Data.Char`. Ecco un esempio di codice:

```haskell
import Data.Char

capitalize :: String -> String
capitalize = unwords . map capitalizeWord . words
    where capitalizeWord (x:xs) = toUpper x : xs
```

Esempio di output:

```haskell
>>> capitalize "ciao mondo"
"Ciao Mondo"
```

## Approfondimento:
La pratica di capitalizzare una stringa deriva dalle regole di grammatica delle lingue in cui la prima lettera di una parola è solitamente maiuscola. Ci sono anche altre funzioni in Haskell che possono essere utilizzate per manipolare le stringhe, come ad esempio `toLower` che converte una stringa in caratteri minuscoli.

## Vedi anche:
- [Documentazione del modulo Data.Char](https://hackage.haskell.org/package/base-4.14.1.0/docs/Data-Char.html)
- [Esempi di manipolazione delle stringhe in Haskell](https://wiki.haskell.org/Manipulating_strings)