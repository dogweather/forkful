---
title:                "Haskell: Calcolo di una data nel futuro o nel passato"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché
Calcolare una data nel futuro o nel passato può essere utile in diverse situazioni, come ad esempio nella gestione di calendari o nella pianificazione di eventi.

## Come Fare
Per calcolare una data nel futuro o nel passato in Haskell, possiamo utilizzare la libreria `Data.Time`, che offre molte funzioni utili per la manipolazione del tempo e delle date.

Per prima cosa, dobbiamo importare il modulo `Data.Time` nel nostro programma:

```Haskell
import Data.Time
```

Per calcolare una data nel futuro, possiamo utilizzare la funzione `addUTCTime` che accetta come parametri una quantità di secondi e una data di riferimento, e restituisce una nuova data corrispondente alla somma dei secondi specificati alla data di riferimento. Ad esempio:

```Haskell
addUTCTime (60*60*24) (UTCTime (fromGregorian 2021 1 1) 0)
```
Questa funzione calcolerà la data corrispondente a 24 ore dopo il primo gennaio 2021 (2021-01-02).

Invece, se vogliamo calcolare una data nel passato, possiamo utilizzare la funzione `addUTCTimeNeg` che funziona allo stesso modo della funzione `addUTCTime`, ma sottrae la quantità di secondi specificata alla data di riferimento.

Ad esempio, per ottenere la data corrispondente a un giorno prima del primo gennaio 2021, possiamo scrivere:

```Haskell
addUTCTimeNeg (60*60*24) (UTCTime (fromGregorian 2021 1 1) 0)
```

## Approfondimento
Oltre alle funzioni `addUTCTime` e `addUTCTimeNeg`, la libreria `Data.Time` offre anche molte altre funzioni utili per il calcolo delle date nel futuro o nel passato. Possiamo ad esempio utilizzare la funzione `addDays` per aggiungere o sottrarre un determinato numero di giorni da una data, oppure la funzione `addGregorianMonthsClip` per aggiungere o sottrarre un determinato numero di mesi, facendo attenzione a considerare anche gli anni bisestili.

Inoltre, possiamo utilizzare la funzione `diffUTCTime` per calcolare la differenza in secondi tra due date, e così via.

Per ulteriori informazioni sulle funzioni disponibili nella libreria `Data.Time`, si può consultare la documentazione ufficiale su Hackage: [https://hackage.haskell.org/package/time](https://hackage.haskell.org/package/time).

## Vedi Anche
- [https://www.haskell.org/tutorial/io.html](https://www.haskell.org/tutorial/io.html)
- [http://book.realworldhaskell.org/read/io.html](http://book.realworldhaskell.org/read/io.html)
- [https://www.tutorialspoint.com/haskell/haskell_date_time.htm](https://www.tutorialspoint.com/haskell/haskell_date_time.htm)