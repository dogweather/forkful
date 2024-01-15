---
title:                "Confronto tra due date"
html_title:           "Haskell: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

Sei stanco di scrivere righe intere di codice solo per confrontare due date? L'Haskell offre una sintassi semplice ed elegante per confrontare date, risparmiandoti tempo e fatica.

## Come fare

Per confrontare due date in Haskell, puoi utilizzare l'operatore `compare` che restituisce uno dei tre valori: `GT` (greater than), `LT` (less than) o `EQ` (equal). Ecco un esempio pratico:

``` Haskell
-- Confronto due date
compareDates :: String -> String -> Ordering
compareDates date1 date2 = compare date1 date2
```
Il codice sopra confronta due stringhe di data e restituisce il valore di ordinamento corrispondente. Ad esempio, se `date1` è successivo a `date2`, il risultato sarà `GT`.

Per semplificare ulteriormente il confronto di date, puoi utilizzare la funzione `on` del modulo `Data.Function`, che applica una funzione a due valori e poi li confronta. Ecco un esempio:

``` Haskell
-- Confronto due date utilizzando la funzione "on"
compareDates :: String -> String -> Ordering
compareDates = compare `on` dateFunction
  where dateFunction date = read date :: Day
```

## Approfondimento

Il motivo per cui l'operatore `compare` funziona così bene per il confronto di date è che le date possono essere considerate come tipi di dati ordinabili. Ciò significa che, oltre all'operatore `compare`, è possibile utilizzare anche le funzioni `min` e `max` per trovare rispettivamente la data più piccola e quella più grande tra due date.

E se hai bisogno di eseguire operazioni matematiche sui giorni tra le due date? Haskell offre il tipo di dati `DiffDays` per rappresentare la differenza tra due date in termini di giorni. Ad esempio, per ottenere il numero di giorni tra due date, puoi utilizzare la funzione `diffDays`:

``` Haskell
-- Ottenere il numero di giorni tra due date
getDays :: String -> String -> Integer
getDays date1 date2 = diffDays (read date2 :: Day) (read date1 :: Day)
```
Noterai che è necessario convertire le stringhe di data in tipi di dati `Day` utilizzando la funzione `read`. Questo perché `diffDays` e altre funzioni di manipolazione delle date operano sui tipi di dati interni di Haskell e non sulle stringhe.

## Vedi anche

- [Haskell Date and Time Library - Documentazione ufficiale](https://hackage.haskell.org/package/datetime) 
- [Haskell Date Types and Conversions - Esempio pratico](https://www.tutorialspoint.com/haskell/haskell_date_types_and_conversions.htm)