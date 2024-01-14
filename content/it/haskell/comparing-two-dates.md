---
title:                "Haskell: Confrontare due date"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Haskell, sicuramente hai incontrato situazioni in cui hai dovuto confrontare due date. Potrebbe essere necessario determinare l'ordine cronologico tra due eventi o calcolare la differenza di tempo tra di essi. In questo articolo esploreremo come confrontare due date usando Haskell.

## Come Fare

Il modo più semplice per confrontare due date in Haskell è utilizzare il modulo `Data.Time`. Per cominciare, importiamo il modulo nel nostro file di progetto:

```Haskell
import Data.Time
```

Per confrontare due date, dobbiamo prima definirle come oggetti `UTCTime` usando la funzione `parseTimeM` del modulo `Data.Time.Format`. Ad esempio, se vogliamo confrontare il 1 ° gennaio 2021 alle 00:00 e il 1 ° febbraio 2021 alle 12:00, possiamo definirle come segue:

```Haskell
let primaData = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2021-01-01 00:00:00" :: Maybe UTCTime
let secondaData = parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" "2021-02-01 12:00:00" :: Maybe UTCTime
```

Una volta definite le date come oggetti `UTCTime`, possiamo utilizzare gli operatori `>` (maggiore), `<` (minore) e `==` (uguale) per confrontarle. Ad esempio:

```Haskell
primaData > secondaData
-- Output: False
primaData < secondaData
-- Output: True
primaData == secondaData
-- Output: False
```

Possiamo anche utilizzare la funzione `diffUTCTime` per calcolare la differenza di tempo tra due date. Questa funzione restituisce un valore di tipo `NominalDiffTime`, che rappresenta la differenza in secondi tra le due date. Ad esempio:

```Haskell
diffUTCTime secondaData primaData
-- Output: 2678400.0 (corrispondente a 31 giorni)
```

## Approfondimento

È importante notare che le date sono sensibili al fuso orario. Quindi, quando si confrontano due date, è necessario assicurarsi che entrambe siano nella stessa zona oraria. Inoltre, la funzione `parseTimeM` restituirà un valore `Maybe` in caso di errore nella conversione. Quindi, è necessario gestire correttamente i possibili valori `Nothing` prima di confrontare le date.

## Vedi Anche

- [HSpec: Testing in Haskell (in Italiano)](https://www.valentinog.com/blog/haskell-hspec/)
- [A Gentle Introduction to Haskell (in Italiano)](https://www.spazio-lab.it/articoli/44-programmazione/45-a-gentle-introduction-to-haskell)