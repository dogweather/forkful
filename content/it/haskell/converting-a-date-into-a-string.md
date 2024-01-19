---
title:                "Convertire una data in una stringa"
html_title:           "Javascript: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
Convertire una data in una stringa significa rappresentarla come testo. Gli sviluppatori lo fanno per facilitare la leggibilità e la manipolazione dei dati.

## Come Fare:
Haskell offre la funzionalità di formattare la data in una stringa usando la libreria `Data.Time`.

```Haskell
import Data.Time

main = do
   currentTime <- getCurrentTime
   print $ formatTime defaultTimeLocale "%d/%m/%Y" currentTime
```
Questo codice stampa la data corrente in un formato leggibile come "dd/mm/yyyy".

## Approfondimento
Prima dell'introduzione della funzione `formatTime` in Haskell, la conversione di una data in una stringa richiedeva più passaggi manuali. Per quanto riguarda le alternative, esistono diverse librerie e metodi per formattare le date, come l'uso di `printf`.

Dettagli di implementazione: la funzione `formatTime` utilizza `defaultTimeLocale` per determinare come formattare il tempo. È possibile personalizzare `TimeLocale` per cambiare i risultati di formattazione.

## Vedi Anche
- Documentazione su `Data.Time` su Hackage: http://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html
- Il tutorial di Haskell Wiki su Date e Orari: https://wiki.haskell.org/Dates_and_times