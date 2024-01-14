---
title:    "Elm: Leggere gli argomenti della riga di comando"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
Alcune volte, quando si scrive un programma, può essere utile avere la possibilità di leggere gli argomenti inseriti dalla riga di comando. Questo può essere particolarmente utile quando si vuole rendere il programma più flessibile e configurabile.

## Come fare
Per leggere gli argomenti dalla riga di comando in Elm, è necessario utilizzare il modulo `Platform.Cmd`, che fornisce una funzione `Cmd.args` che restituisce una lista di stringhe contenti gli argomenti inseriti dalla riga di comando.

```Elm
import Platform.Cmd

main : Program flags
main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : flags -> ( Model, Cmd Msg )
init _ =
  ( [], Cmd.none )

type Msg = GetCommandLineArgs (List String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    GetCommandLineArgs args ->
      ( model ++ args, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
  Platform.Cmd.args GetCommandLineArgs
```

Nell'esempio sopra, abbiamo utilizzato la funzione `Cmd.none` per indicare che non vogliamo eseguire alcun codice quando viene ricevuto il messaggio. Invece, aggiungiamo gli argomenti alla nostra struttura di dati Model.

## Analisi approfondita
In realtà, la funzione `Cmd.args` non legge direttamente gli argomenti dalla riga di comando. Piuttosto, utilizza la funzione `Platform.worker` per creare un'interfaccia con il sistema operativo che gestisce l'esecuzione del codice. Ciò significa che non dobbiamo preoccuparci dei dettagli di basso livello del sistema operativo, ma possiamo semplicemente utilizzare i valori restituiti dalla funzione `Cmd.args` nel nostro codice.

## Vedi anche
- Documentazione ufficiale di Elm: https://elm-lang.org/docs
- Tutorial su come leggere gli argomenti dalla riga di comando in Elm: https://guide.elm-lang.org/interop/cmd.html
- Esempi di progetti Elm che utilizzano la lettura degli argomenti dalla riga di comando: https://github.com/topics/elm-command-line-args