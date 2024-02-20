---
date: 2024-01-20 15:14:19.510193-07:00
description: "Ottenere la data attuale significa catturare il momento presente in\
  \ termini di giorno, mese e anno. I programmatori lo fanno per tracciare eventi,\
  \ gestire\u2026"
lastmod: 2024-02-19 22:05:02.422773
summary: "Ottenere la data attuale significa catturare il momento presente in termini\
  \ di giorno, mese e anno. I programmatori lo fanno per tracciare eventi, gestire\u2026"
title: Ottenere la data corrente
---

{{< edit_this_page >}}

## What & Why?
Ottenere la data attuale significa catturare il momento presente in termini di giorno, mese e anno. I programmatori lo fanno per tracciare eventi, gestire le funzionalità legate al tempo e per tenere traccia di quando le cose accadono.

## How to:
In Elm, per ottenere la data attuale usiamo il modulo `Time`. Ecco come:

```Elm
import Time
import Browser

-- Inizializza un'applicazione Elm che ottiene il tempo attuale.
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

-- Modello per mantenere il tempo attuale (in millisecondi).
type alias Model = Time.Posix

-- Inizializza il modello con il tempo attuale.
init : () -> (Model, Cmd Msg)
init _ =
    (Time.millisToPosix 0, Time.now |> Task.perform NewTime)

-- Vista che mostra la data attuale.
view : Model -> Html.Html Msg
view model =
    Html.text (String.fromInt (Time.posixToMillis model))

-- Aggiornamenti basati sui messaggi ricevuti.
type Msg = NewTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update (NewTime newTime) _ =
    (newTime, Cmd.none)

-- Iscrizioni per aggiornamenti del tempo.
subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 (NewTime << Time.millisToPosix)
```

Il codice qui sopra configura un'applicazione che aggiorna e visualizza la data attuale ogni secondo.

## Deep Dive
Elm gestisce date e ore tramite il modulo `Time`. Punto di riferimento è l'Unix Epoch (1 gennaio 1970). Alternative a `Time` in Elm includono librerie esterne come `elm-time` o `elm-datepicker` per funzionalità specifiche legate alle date.

Elm gestisce il tempo in modo funzionale e con un forte tipo di sicurezza, quindi lavorare con le date può essere meno soggetto a errori rispetto ad altri linguaggi. Tuttavia, Elm non offre una libreria di manipolazione della data altrettanto ricca come per esempio `moment.js` in JavaScript, quindi per alcune operazioni potrebbe essere necessario più codice.

## See Also
- Elm Time documentation: [official Time module](https://package.elm-lang.org/packages/elm/time/latest/)
- Extra libraries for handling date and time in Elm:
    - [elm-time](https://package.elm-lang.org/packages/justinmimbs/date/latest/)
    - [elm-datepicker](https://package.elm-lang.org/packages/CurrySoftware/elm-datepicker/latest/)
