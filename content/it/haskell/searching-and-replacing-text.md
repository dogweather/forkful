---
title:                "Ricerca e sostituzione del testo"
date:                  2024-01-20T17:57:50.562265-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
La ricerca e la sostituzione di testo è una manipolazione di stringhe in cui troviamo certo contenuto e lo rimpiazziamo con altro. I programmatori lo fanno per aggiornare i dati, correggere errori, o modificare codice in modo rapido.

## How to:
Ecco un esempio in Haskell utilizzando le funzioni `subRegex` dalla libreria `regex-compat`:

```Haskell
import Text.Regex

searchAndReplace :: String -> String -> String -> String
searchAndReplace target replacement text =
  subRegex (mkRegex target) text replacement

main = putStrLn $ searchAndReplace "gatto" "cane" "Il gatto dorme sul tappeto."
```

Risultato:

```
Il cane dorme sul tappeto.
```

## Deep Dive
Nel mondo di Haskell, ci sono diverse modalità per manipolare testi. `regex-compat` offre un modo compatibile con POSIX per lavorare con espressioni regolari, utile per compiti di ricerca e sostituzione flessibili.

Alternative popolari includono `regex-tdfa` per una compatibilità con le espressioni regolari più moderna stile `Perl`, e `text`, per lavorare con stringhe in grandi quantità in modo efficiente.

I dettagli di implementazione variano in base alla libreria, ma sotto il cofano, la ricerca e la sostituzione di testo si basano su automi e pattern matching, consentendo operazioni complesse in modo efficiente.

## See Also
- Haskell Wiki su regex: https://wiki.haskell.org/Regular_expressions
- Documentazione su `regex-compat`: http://hackage.haskell.org/package/regex-compat
- Documentazione su `regex-tdfa`: http://hackage.haskell.org/package/regex-tdfa
- Documentazione su `text`: http://hackage.haskell.org/package/text
