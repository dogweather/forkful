---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Haskell: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why?
Calcolare una data nel futuro o nel passato è un'operazione comune per i programmatori, poiché spesso è necessario lavorare con date in modo dinamico. Ad esempio, potrebbe essere richiesto di calcolare una data di scadenza per una prenotazione, o di sapere quando è il prossimo compleanno di un utente. In questi casi, è essenziale sapere come manipolare e calcolare le date in modo efficace.

## How to:
Per calcolare una data in Haskell, è possibile utilizzare la funzione `addDays` della libreria Data.Time. Questa funzione prende come argomenti una data e il numero di giorni che si desidera aggiungere o sottrarre e restituisce una nuova data calcolata. Ecco un esempio di codice:

```Haskell
import Data.Time

-- Calcolo di una data nel futuro
calcolaDataFutura :: Day -> Int -> Day
calcolaDataFutura dataAttuale giorniAggiunti = addDays giorniAggiunti dataAttuale

-- Calcolo di una data nel passato
calcolaDataPassata :: Day -> Int -> Day
calcolaDataPassata dataAttuale giorniSottratti = addDays (-giorniSottratti) dataAttuale

-- Esempio di utilizzo delle funzioni sopra definite
main :: IO ()
main = do
  let dataOggi = fromGregorian 2020 10 20
  let dataFutura = calcolaDataFutura dataOggi 30
  let dataPassata = calcolaDataPassata dataOggi 15
  print dataFutura -- Output: 2020-11-19
  print dataPassata -- Output: 2020-10-05
```

## Deep Dive:
In passato, prima dell'introduzione della libreria Data.Time di Haskell, i programmatori dovevano gestire manualmente la manipolazione delle date utilizzando operazioni matematiche su timestamp. Questo rendeva il codice soggetto a errori e poco flessibile. La libreria Data.Time invece offre un'implementazione robusta e semplice da utilizzare per la gestione delle date.

È importante notare che la libreria Data.Time gestisce le date nel formato `YYYY-MM-DD`, con il mese rappresentato da un numero da 1 a 12, e il giorno da 1 a 31.

Un'alternativa alla libreria Data.Time è il pacchetto `time` di Haskell, che offre funzioni e tipi di dati simili per la gestione delle date.

## See Also:
- [Documentazione della libreria Data.Time di Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Pacchetto time di Haskell](https://hackage.haskell.org/package/time)