---
title:    "Elm: Lettura degli argomenti della riga di comando"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore che utilizza Elm, è probabile che tu abbia già avuto a che fare con i tuoi script che richiedono input dall'utente, ma non sai come gestire i comandi da riga di comando. In questo post, ti illustrerò come leggere e gestire i parametri di input da riga di comando nel linguaggio di programmazione Elm.

## Come fare

Per leggere i parametri da riga di comando in Elm, è necessario importare il modulo "Platform" e utilizzare la funzione `worker`. Qui di seguito troverai un semplice esempio di come gestire un singolo argomento di input.

```Elm
module Main exposing (main)

import Platform

main : Program Never String
main = 
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }

type alias Model = 
    { arg : String 
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model "default", Cmd.none )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = 
    case msg of 
        SetArgument arg ->
            ( { model | arg = arg }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

type Msg = 
    SetArgument String

```

In questo esempio, il parametro inserito dall'utente verrà memorizzato nella variabile `arg` del modello. Per utilizzare più di un parametro, è necessario utilizzare una lista o una tupla come tipo del modello e gestire gli argomenti singolarmente o utilizzare la funzione `Platform.worker` per gestire tutti i parametri in un unico evento.

## Approfondimento

Oltre alla funzione `worker` utilizzata nell'esempio, ci sono anche altri modi per gestire i comandi da riga di comando in Elm. Ad esempio, è possibile utilizzare il modulo `Task` per creare una task che legga i parametri da riga di comando. Inoltre, esistono anche librerie di terze parti come `elm-argv` che semplificano ulteriormente la gestione di input da riga di comando in Elm.

## Vedi anche

- Documentazione ufficiale di Elm sul modulo Platform: https://package.elm-lang.org/packages/elm/core/latest/Platform
- Esempi pratici di gestione di input da riga di comando in Elm: https://gist.github.com/zwilias/06e9dc120552c5d70deb
- Libreria di terze parti per la gestione di input da riga di comando in Elm: https://package.elm-lang.org/packages/elm-community/elm-argv/latest/