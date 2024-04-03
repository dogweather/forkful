---
changelog:
- 2024-02-27, dogweather, edited and tested
- 2024-02-27, gpt-4-0125-preview, translated from English
date: 2024-02-27 22:50:15.127248-07:00
description: "Generare numeri casuali in Elm comporta l'utilizzo del modulo `Random`\
  \ per produrre numeri pseudo-casuali, i quali sono utili per una variet\xE0 di compiti\u2026"
lastmod: '2024-03-13T22:44:43.346048-06:00'
model: gpt-4-0125-preview
summary: "Generare numeri casuali in Elm comporta l'utilizzo del modulo `Random` per\
  \ produrre numeri pseudo-casuali, i quali sono utili per una variet\xE0 di compiti\
  \ come giochi, simulazioni e perfino come parte di algoritmi che richiedono processi\
  \ stocastici."
title: Generare numeri casuali
weight: 12
---

## Come fare:
La natura puramente funzionale di Elm significa che non è possibile generare numeri casuali direttamente come si potrebbe fare nei linguaggi imperativi. Invece, si utilizza il modulo `Random` in congiunzione con i comandi. Ecco un esempio base che genera un numero intero casuale tra 1 e 100.

Prima di tutto, installa il modulo `Random` con `elm install elm/random`. Poi importalo nel tuo file Elm, insieme ai necessari moduli HTML ed eventi, così:

`src/Main.elm`

```elm
module Main exposing (..)

import Browser
import Html exposing (Html, button, text, div)
import Html.Events exposing (onClick)
import Random
```

Per rendere quest'esempio autocontenuto, puoi aggiungere questo codice standard:
```elm
main =
  Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }

init : () -> (Model, Cmd Msg)
init _ =
  (Model 0, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
```

Successivamente, definisci un **comando** per generare un numero casuale. Ciò implica la configurazione di un tipo `Msg` per gestire il numero casuale una volta generato, un `Model` per memorizzarlo e una funzione di aggiornamento per unire il tutto.
```elm
type Msg
    = Generate
    | NewRandom Int

type alias Model = { randomNumber : Int }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Generate ->
            ( model, Random.generate NewRandom (Random.int 1 100) )

        NewRandom number ->
            ( { model | randomNumber = number }, Cmd.none )
```

Per innescare la generazione di un numero, si invierebbe un messaggio `Generate`, ad esempio, tramite un pulsante nella tua vista:
```elm
view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Numero Casuale: " ++ String.fromInt model.randomNumber) ]
        , button [ onClick Generate ] [ text "Genera" ]
        ]
```

Quando clicchi il pulsante "Genera", verrà visualizzato un numero casuale tra 1 e 100.

Questo approccio semplicistico può essere adattato ed espanso, sfruttando altre funzioni nel modulo `Random` per produrre numeri casuali in virgola, liste o anche strutture dati complesse basate su tipi personalizzati, fornendo un vasto campo di gioco per aggiungere imprevedibilità alle tue applicazioni Elm.

La Guida di Elm entra molto più nel dettaglio. Ha anche [un esempio su come lanciare un dado a sei facce](https://guide.elm-lang.org/effects/random).
