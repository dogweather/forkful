---
title:                "Maiuscolizzare una stringa"
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Capitalizzare una stringa in informatica significa convertire la prima lettera di ogni parola in maiuscolo. I programmatori spesso capitalizzano per migliorare la leggibilità dei titoli o per conformarsi a degli standard stilistici.

## How to:

In Haskell, possiamo capitalizzare una stringa usando la funzione `capitalize` che creeremo. Ecco un semplice esempio:

```Haskell
import Data.Char (toUpper)

capitalize :: String -> String
capitalize ""     = ""
capitalize (x:xs) = toUpper x : map toLower xs

main :: IO ()
main = putStrLn $ capitalize "ciao mondo"

-- Output: Ciao mondo
```

## Deep Dive

La capitalizzazione in programmazione ha radici nella tipografia. La convenzione di iniziare i titoli e i nomi con la lettera maiuscola si riflette ora nel codice. In Haskell, capitalizzare non è nativo come in altri linguaggi che hanno metodi integrati, ma è semplice da implementare.

La funzione `toUpper` da `Data.Char` converte un singolo carattere in maiuscolo. Usiamo la comprensione delle liste o `map` per applicarla alla prima lettera e `toLower` per il resto della stringa.

Alternative includono librerie come `text` o `Data.Text` che possono gestire stringhe più efficientemente per applicazioni più grandi.

Dettaglio implementativo importante è assicurarsi di gestire bene stringhe vuote e parola dopo spazi per una capitalizzazione corretta.

## See Also

- Documentazione `Data.Char`: [Hackage Data.Char](https://hackage.haskell.org/package/base-4.16.0.0/docs/Data-Char.html)
- Documentazione Haskell su `text`: [Hackage Data.Text](https://hackage.haskell.org/package/text)
- Approfondimenti sulla tipografia e lettere maiuscole: [Capitalization - Wikipedia](https://en.wikipedia.org/wiki/Capitalization)