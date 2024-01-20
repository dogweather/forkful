---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Confrontare due date significa determinare quale data è prima o se sono uguali. Questo è fondamentale nella programmazione per gestire gli eventi che si verificano in un certo ordine temporale.

## Come fare:
Qui sotto trovi un esempio di come confrontare due date usando Elm.

```Elm
import Time exposing (..)

date1 = toTime { year = 2021, month = Dec, day = 1, hour = 8, minute = 30, second = 0, millisecond = 0 } |> Result.toMaybe |> Maybe.withDefault (millisToPosix 0)
date2 = toTime { year = 2022, month = Jan, day = 1, hour = 8, minute = 30, second = 0, millisecond = 0 } |> Result.toMaybe |> Maybe.withDefault (millisToPosix 0)

if date1 < date2 then
    "La data1 è precedente alla data2"
else if date1 > date2 then
    "La data1 è successiva alla data2"
else
    "Le date sono uguali"
```

Output:
> "La data1 è precedente alla data2"

## Approfondimento
Le date hanno sempre avuto un ruolo chiave nella programmazione data la loro ubiquità in svariate applicazioni. Elm gestisce le date tramite il modulo `Time`, fornendo funzioni di base per manipolare e confrontare le date. Un'alternativa potrebbe essere l'utilizzo di librerie esterne se necessiti di funzionalità più avanzate non presenti nel modulo standard.
Tieni a mente che confrontare due date significa confrontare i millisecondi trascorsi dal 1 gennaio 1970 (noto come Unix Epoch). Quindi, data1 < data2 se data1 è avvenuto prima di data2.

## Vedere Anche
Per saperne di più sulle funzioni del modulo `Time`, controlla la documentazione ufficiale di Elm [qui](https://package.elm-lang.org/packages/elm/time/latest/). Per uno sguardo più approfondito su come manipolare e confrontare le date, consulta i tutorial specifici disponibili su piattaforme come [Learn Elm](https://www.learn-elm.org/).