---
title:                "Haskell: Confrontare due date"
simple_title:         "Confrontare due date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché confrontare due date in Haskell

Confrontare due date è un'operazione comune nel mondo della programmazione, in particolare quando si lavora con applicazioni che coinvolgono il tempo, come calendari o sistemi di prenotazione. In Haskell, questo processo è reso ancora più semplice grazie alla sua forte tipizzazione e alla sua sintassi funzionale.

## Come fare

Per confrontare due date in Haskell, abbiamo bisogno di importare il modulo `Data.Time` e utilizzare le funzioni `UTCTime` e `compare`. Vediamo un esempio pratico:

```Haskell
import Data.Time

-- Definiamo due date
data1 = UTCTime (fromGregorian 2020 12 10) 0
data2 = UTCTime (fromGregorian 2020 12 15) 0

-- Confrontiamo le due date
compare data1 data2
```

L'output sarà `LT`, che sta per "Less Than" (minore di), indicando che la prima data è antecedente alla seconda. Altri possibili output sono `GT` (Greater Than) se la prima data è successiva alla seconda, e `EQ` (Equal) se le due date sono uguali.

Inoltre, è possibile eseguire anche confronti tra date di diversi tipi, grazie allo smart constructor `fromGregorian` che ci consente di creare una data a partire da un anno, un mese e un giorno.

```Haskell
data3 = UTCTime (fromGregorian 2021 1 1) 0
compare data1 data3
```

L'output sarà `LT`, poiché la data1 è ancora antecedente alla data3.

## Un'analisi approfondita

Quando confrontiamo due date in Haskell, fondamentalmente stiamo confrontando due oggetti di tipo `UTCTime`, che rappresentano un istante preciso nel tempo. Questi oggetti sono composti da due parti: la data e l'orario. Per questo motivo, quando confrontiamo due date, stiamo confrontando sia la data che l'orario.

Vediamo un esempio di come si comporta la funzione `compare` con date che hanno lo stesso valore di data ma orari diversi:

```Haskell
data4 = UTCTime (fromGregorian 2020 12 10) 10
data5 = UTCTime (fromGregorian 2020 12 10) 20
compare data4 data5
```

L'output sarà ancora `LT`, nonostante i valori degli orari siano diversi, poiché per la funzione di confronto contano solo i valori della data.

## Vedi anche

- [Documentazione ufficiale di Haskell sulle date](https://www.haskell.org/hoogle/?q=Data.Time)
- [Tutorial su come utilizzare il modulo Data.Time](https://wiki.haskell.org/How_to_work_on_Dates_and_Time)
- [Esempi pratici di confronti tra date](https://rosettacode.org/wiki/Date_comparison)