---
title:                "Haskell: Calcolare una data nel futuro o nel passato."
simple_title:         "Calcolare una data nel futuro o nel passato."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in molte situazioni, come ad esempio nei sistemi di prenotazione o nel calcolo di scadenze. In Haskell, è possibile farlo in modo semplice ed elegante.

## Come

Per prima cosa, dobbiamo importare il modulo `Data.Time` che ci fornisce le funzioni necessarie per lavorare con date e orari. Utilizzeremo anche il modulo `Data.Time.Calendar` per accedere al tipo di dato `Day`, che rappresenta una data.

```Haskell
import Data.Time
import Data.Time.Calendar
```

Una volta importati i moduli necessari, possiamo iniziare a utilizzare la funzione `addDays` per aggiungere o sottrarre un numero di giorni da una data specifica. Ad esempio, per calcolare la data di oggi più 5 giorni nel futuro:

```Haskell
let oggi = utctDay getCurrentTime
let dataFutura = addDays 5 oggi
```

Per convertire una data nel formato `Day` in una stringa più leggibile, possiamo utilizzare la funzione `show`:

```Haskell
let dataLeggibile = show dataFutura
```

Infine, per calcolare una data nel passato, basta utilizzare un numero di giorni negativo.

```Haskell
let dataPassata = addDays (-10) oggi
```

## Deep Dive

In Haskell, le date sono gestite utilizzando il tipo di dato `Day` che rappresenta il numero di giorni trascorsi dal 1 gennaio 4713 a.C. secondo il calendario giuliano. Questo calendario è stato introdotto da Giulio Cesare nel 45 a.C. e indica l'anno corrente con un numero intero positivo, senza considerare eventi come l'era cristiana.

Per rendere le operazioni con le date più semplici e intuitive, il modulo `Data.Time.Calendar` fornisce anche funzioni come `fromGregorian`, `toGregorian` e `gregorianMonthLength` per gestire la conversione tra il formato giuliano e il formato comune del calendario gregoriano.

## Vedi anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation)
- [Esercizi pratici di calcolo delle date in Haskell](https://exercism.io/tracks/haskell/exercises/date)
- [Approfondimento sulle differenze tra i calendari giuliano e gregoriano](https://www.timeanddate.com/calendar/julian-gregorian-switch.html)