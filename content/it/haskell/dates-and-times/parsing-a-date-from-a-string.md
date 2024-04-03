---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:14:18.019066-07:00
description: "Come fare: Di base, Haskell offre strumenti semplici per l'analisi delle\
  \ date, ma sfruttare librerie come `time` per la funzionalit\xE0 di base e `date-\u2026"
lastmod: '2024-03-13T22:44:43.484527-06:00'
model: gpt-4-0125-preview
summary: "Di base, Haskell offre strumenti semplici per l'analisi delle date, ma sfruttare\
  \ librerie come `time` per la funzionalit\xE0 di base e `date-parse` o `time-parse`\
  \ per un'analisi pi\xF9 flessibile, pu\xF2 semplificare notevolmente il compito."
title: Analisi di una data da una stringa
weight: 30
---

## Come fare:
Di base, Haskell offre strumenti semplici per l'analisi delle date, ma sfruttare librerie come `time` per la funzionalità di base e `date-parse` o `time-parse` per un'analisi più flessibile, può semplificare notevolmente il compito.

Innanzitutto, assicurati di avere la libreria `time` disponibile; è spesso inclusa con GHC, ma se devi specificarla come dipendenza, aggiungi `time` al file cabal del tuo progetto o usa `cabal install time` per installarla manualmente.

```haskell
import Data.Time.Format
import Data.Time.Clock
import System.Locale (defaultTimeLocale)

-- Usando la libreria time per analizzare una data in un formato standard
parseBasicDate :: String -> Maybe UTCTime
parseBasicDate = parseTimeM True defaultTimeLocale "%Y-%m-%d" 
```

Esempio di utilizzo e output:

```haskell
main :: IO ()
main = print $ parseBasicDate "2023-04-01"

-- Output: Just 2023-03-31 22:00:00 UTC
```

Per scenari più complessi, dove è necessario gestire formati o località multipli, librerie di terze parti come `date-parse` possono essere più convenienti:

Supponendo che tu abbia aggiunto `date-parse` alle tue dipendenze e lo abbia installato, ecco come potresti usarlo:

```haskell
import Data.Time.Calendar
import Text.Date.Parse (parseDate)

-- Analizzando una stringa di data con la libreria date-parse supporta formati multipli
parseFlexibleDate :: String -> Maybe Day
parseFlexibleDate = parseDate
```

Esempio di utilizzo con `date-parse`:

```haskell
main :: IO ()
main = print $ parseFlexibleDate "April 1, 2023"

-- Output: Just 2023-04-01
```

Ogni esempio dimostra l'approccio fondamentale per trasformare una stringa in un oggetto data utilizzabile in Haskell. La scelta tra l'uso delle funzioni integrate della libreria `time` e l'opzione per una soluzione di terze parti come `date-parse` dipende dalle esigenze specifiche della tua applicazione, come la gamma di formati di input che devi gestire.
