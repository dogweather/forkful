---
title:                "Lettura degli argomenti dalla riga di comando"
html_title:           "Elm: Lettura degli argomenti dalla riga di comando"
simple_title:         "Lettura degli argomenti dalla riga di comando"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Hai mai avuto bisogno di leggere gli argomenti passati dalla riga di comando in un programma Elm? Forse stai sviluppando un'applicazione per la riga di comando o vuoi semplicemente gestire alcuni input dall'utente. In ogni caso, saper come leggere gli argomenti della riga di comando può essere estremamente utile.

## Come Fare

Per leggere gli argomenti della riga di comando in Elm, possiamo utilizzare la funzione built-in `Platform.Cmd.getArgs`. Questa funzione restituirà gli argomenti come una lista di stringhe, che possiamo poi elaborare e utilizzare nel nostro programma.

```Elm
import Platform.Cmd as Cmd

arguments : Cmd.Cmd (List String)
arguments =
    Cmd.getArgs

-- Esempio di output: ["Hello", "world"]
```

Possiamo anche isolarli direttamente nella nostra funzione `main` utilizzando la funzione `map` per elaborare la lista degli argomenti:

```Elm
import Platform.Cmd as Cmd

main : Program flags
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : flags -> ( Model, Cmd Cmd.Cmd (List String))
init _ =
    ( Model "", arguments )

update : msg -> Model -> ( Model, Cmd Cmd.Cmd (List String) )
update _ model =
    ( model, Cmd.none )

view : Model -> Html msg
view model =
    text (String.join " " model.arguments)

subscriptions : Model -> Sub.Sub (Msg)
subscriptions model =
    Sub.none

-- Esempio di output: Hello world
```

## Approfondimento

È importante notare che quando utilizziamo la funzione `Platform.Cmd.getArgs`, stiamo aspettando che il programma Elm riceva gli argomenti dalla riga di comando, quindi è necessario eseguire il codice tramite `elm reactor` o `elm make` per vedere gli argomenti in azione.

Inoltre, è necessario assicurarsi di gestire correttamente gli argomenti e i loro tipi all'interno del nostro programma per evitare errori e bug.

## Vedi Anche

Se vuoi saperne di più su come utilizzare la riga di comando in Elm, puoi consultare la documentazione ufficiale di Elm su questo argomento o guardare alcune risorse online come:

- [Documentazione ufficiale di Elm - Modulo Cmd](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd)
- [Canale Youtube di Elm - Comandi e Subscriptions](https://www.youtube.com/watch?v=0Qyt2pGO9qg)
- [Blog di Elm - Usare i comandi per comunicare con JavaScript](https://elm-lang.org/news/compiling-to-javascript-completely-revised)