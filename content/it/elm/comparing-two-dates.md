---
title:    "Elm: Confrontare due date"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Comparare due date è una pratica comune nella programmazione e può essere utile per una varietà di scopi, come ad esempio ordinare eventi cronologicamente o calcolare durate tra due date.

## Come fare

Per comparare due date in Elm, possiamo utilizzare la funzione `compare` del modulo `Date`. Questa funzione accetta due date come argomenti e restituisce un `Ordering`, che può essere `LT` (minore di), `EQ` (uguale a) o `GT` (maggiore di), a seconda di quale data sia considerata precedente. Di seguito un esempio pratico:

```Elm
import Date exposing (Date, compare)

primaData : Date
primaData = Date.fromCalendarDate 2021 1 1

secondaData : Date
secondaData = Date.fromCalendarDate 2021 1 15

comparazione : Ordering
comparazione = compare primaData secondaData

-- Output: comparazione = LT
```

## Approfondimento

Per una comparazione più precisa, possiamo sfruttare la funzione `difference` del modulo `Date`. Questa funzione restituisce un `Time` che rappresenta la differenza tra le due date in millisecondi. Ecco un esempio di come potremmo utilizzarla per calcolare la durata tra due date:

```Elm
import Date exposing (Date, difference)

primaData : Date
primaData = Date.fromCalendarDate 2021 1 1

secondaData : Date
secondaData = Date.fromCalendarDate 2021 1 15

durata : Time
durata = difference primaData secondaData

-- Output: durata = 1209600000 ms (14 giorni)
```

## Vedi anche

- Documentazione su `Date.compare` -> https://package.elm-lang.org/packages/elm/time/latest/Date#compare
- Documentazione su `Date.difference` -> https://package.elm-lang.org/packages/elm/time/latest/Date#difference