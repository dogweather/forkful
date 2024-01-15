---
title:                "Calcolare una data nel futuro o nel passato"
html_title:           "Elm: Calcolare una data nel futuro o nel passato"
simple_title:         "Calcolare una data nel futuro o nel passato"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si scrive un programma, si ha bisogno di calcolare la data in futuro o nel passato. Ad esempio, quando si crea un calendario o un'app per la pianificazione delle attività, è importante essere in grado di calcolare le date in modo preciso e affidabile. In questo articolo, impareremo come fare questo utilizzando il linguaggio di programmazione Elm.

## Come Fare

Per calcolare una data in futuro o nel passato usando Elm, dobbiamo innanzitutto capire come Elm gestisce le date. In Elm, le date sono rappresentate come record con i seguenti campi: `year`, `month`, `day`, `hour`, `minute`, `second`, `millisecond`, e `zone`. Per iniziare, creiamo una funzione che crea una data a partire da questi campi:

```Elm
createDate : Int -> Int -> Int -> Date
createDate year month day =
  Date.DateZone.fromCalendarDate
    year month day 0 0 0 0 DateZone.utc
```

Utilizzando questa funzione, possiamo creare una data specificando l'anno, il mese e il giorno desiderati. Ad esempio:

```Elm
myDate : Date
myDate =
  createDate 2021 10 31
```

Una volta che abbiamo una data di riferimento, possiamo utilizzare alcune funzioni predefinite di Elm per calcolare una data in futuro o nel passato.

Per calcolare una data in futuro, possiamo utilizzare la funzione `add` e specificare quanti giorni, mesi, o anni vogliamo aggiungere alla data di riferimento. Ad esempio, per aggiungere 1 mese alla data `myDate`:

```Elm
Date.add Date.Month 1 myDate
-- { year = 2021, month = 11, day = 30, hour = 0, minute = 0, second = 0, millisecond = 0, zone = DateZone.utc }
```

Per calcolare una data nel passato, possiamo utilizzare la funzione `subtract` e specificare quanti giorni, mesi, o anni vogliamo sottrarre alla data di riferimento. Ad esempio, per sottrarre 1 mese dalla data `myDate`:

```Elm
Date.subtract Date.Month 1 myDate
-- { year = 2021, month = 9, day = 1, hour = 0, minute = 0, second = 0, millisecond = 0, zone = DateZone.utc }
```

## Deep Dive

Se vuoi andare più in profondità nel calcolo delle date in Elm, puoi utilizzare il modulo `Date` e la sua documentazione ufficiale per scoprire tutte le funzioni disponibili per manipolare le date. Puoi anche utilizzare il modulo `Date.Extra` che fornisce funzioni aggiuntive, come il controllo sulle date di inizio e fine dei mesi, la creazione di date casuali, e molto altro ancora.

Inoltre, è importante notare che Elm utilizza la libreria `elm/time` per gestire le date e i tempi. Assicurati di leggere anche la documentazione di questa libreria per saperne di più sulle funzioni disponibili.

## Vedi anche

- Documentazione ufficiale di Elm sulle date: https://package.elm-lang.org/packages/elm/time/latest/
- Modulo `Date` nella documentazione di Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
- Modulo `Date.Extra` nella documentazione di Elm: https://package.elm-lang.org/packages/elm-community/date-extra/latest/Date-Extra