---
title:                "Convertire una data in una stringa"
html_title:           "Haskell: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai lavorando su un progetto in Haskell che richiede l'utilizzo di date, è probabile che ti troverai a dover convertire una data in una stringa. Può sembrare un'operazione banale, ma in realtà ci sono alcune considerazioni da tenere in mente per ottenere i risultati desiderati. In questo articolo, vedremo come convertire una data in una stringa in modo corretto utilizzando il linguaggio di programmazione Haskell.

## Come fare

In Haskell, esistono diverse librerie che possono essere utilizzate per la gestione delle date, come ad esempio `Data.Time` e `Data.DateTime`. Per convertire una data in una stringa, useremo la funzione `formatTime` della libreria `Data.Time` e passeremo come argomento i formati desiderati per il giorno, il mese, l'anno e la separazione tra di essi.

```Haskell
import Data.Time.Format

dateToString :: UTCTime -> String
dateToString date = formatTime defaultTimeLocale "%d/%m/%Y" date
```

Nell'esempio sopra, stiamo importando il modulo `Data.Time.Format` e definendo una funzione `dateToString` che prende in ingresso una data di tipo `UTCTime` (universal time coordinate) e restituisce una stringa nel formato "giorno/mese/anno" (ad esempio "31/12/2021"). Nota che stiamo utilizzando la funzione `defaultTimeLocale` per impostare il formato di data e ora predefinito.

Una volta definita la funzione, possiamo chiamarla passando una data e ottenere la nostra stringa formattata:

```Haskell
let date = fromGregorian 2021 12 31
dateToString date -- output: "31/12/2021"
```

## Approfondimento

Un'ulteriore considerazione da tenere in mente quando si convertire una data in una stringa è la localizzazione. Haskell fornisce un'ampia gamma di funzioni e tipi dati per gestire le differenze tra le lingue e le culture, ma per ottenere una conversione accurata delle date, è importante specificare la localizzazione corretta.

Inoltre, è possibile personalizzare ulteriormente il formato della data passando come argomenti alla funzione `formatTime` diversi formati per le singole parti della data (giorno, mese, anno, ecc.) e aggiungendo anche altri elementi, come ad esempio il nome del giorno della settimana.

## Vedi anche

- [Documentazione ufficiale di Haskell sulla gestione delle date](https://www.haskell.org/documentation/#time)
- [Altro esempio di conversione di una data in una stringa in Haskell](https://wiki.haskell.org/Time_and_Date#Formatting_times_.5B%5D)