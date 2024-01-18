---
title:                "Analizzando una data da una stringa"
html_title:           "Haskell: Analizzando una data da una stringa"
simple_title:         "Analizzando una data da una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

L'analisi di una data da una stringa è un'operazione molto comune tra i programmatori. Consiste nella conversione di una data espressa in forma di testo in un formato più gestibile per il computer, come un oggetto di tipo data.

Solitamente si fa questo per manipolare e confrontare date in modo più semplice, senza dover gestire numerose stringhe e formati diversi.

## Come Fare:

Per ottenere una data dal formato stringa in Haskell, abbiamo a disposizione la funzione `parseTimeM` del modulo `Text.ParserCombinators.ReadP`. Questa funzione accetta come parametro un formato di data e una stringa contenente la data da analizzare, e produce come output un oggetto di tipo `Maybe` di tipo `a`, dove `a` è il tipo della data che desideriamo ottenere.

```
Haskell
import Data.Time
import Text.ParserCombinators.ReadP

parseDate :: String -> Maybe Day
parseDate str = parseTimeM True defaultTimeLocale "%d/%m/%Y" str :: Maybe Day

* * * 

parseDate "15/10/2021" -- Restituisce Just 2021-10-15
```

## Approfondimento:

Esistono diversi modi per analizzare una data da una stringa in Haskell. In passato, la libreria `Parsec` era ampiamente utilizzata per questo scopo, ma la funzione `parseTimeM` è diventata più popolare per la sua semplicità e flessibilità.

Inoltre, questa funzione può essere estesa per elaborare anche formati di data personalizzati, oltre ai formati predefiniti forniti dalla libreria `Data.Time`.

## Vedi Anche:

Per saperne di più sulla funzione `parseTimeM` e sulla gestione delle date in Haskell, puoi consultare la documentazione ufficiale: [https://hackage.haskell.org/package/time-1.9/docs/Data-Time-Format.html](https://hackage.haskell.org/package/time-1.9/docs/Data-Time-Format.html).