---
title:                "Elm: Lettura degli argomenti della riga di comando."
simple_title:         "Lettura degli argomenti della riga di comando."
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, nei nostri progetti di programmazione, abbiamo bisogno di leggere i dati forniti dall'utente all'avvio del programma. Questa è una pratica comune anche in Elm, ma come possiamo implementarla? In questo post, scopriremo come leggere gli argomenti da linea di comando in Elm.

## Come Fare

Per iniziare, dobbiamo importare il modulo `Platform` in cui è definita la funzione `worker`, che ci permetterà di interagire con gli argomenti della linea di comando:

```Elm
import Platform exposing (worker)
```

Ora dobbiamo definire il nostro model, che conterrà i dati letti dall'utente. In questo caso, creeremo un modello semplice con un campo `name`:

```Elm
type alias Model =
    { name : String
    }
```

Successivamente, definiremo un'azione `ReceiveArgs` che verrà attivata quando i dati verranno letti dalla linea di comando:

```Elm
type Action
    = ReceiveArgs (List String)
```

Infine, dobbiamo scrivere la nostra funzione di aggiornamento che gestirà l'azione `ReceiveArgs` e modificherà il modello con i dati ricevuti:

```Elm
update : Action -> Model -> (Model, Cmd Action)
update action model =
    case action of
        ReceiveArgs args ->
            ( { model | name = List.head args }, Cmd.none )
```

A questo punto, dobbiamo creare la nostra funzione `main` che utilizzerà il modulo `Platform` per leggere gli argomenti dalla linea di comando e attivare l'azione `ReceiveArgs`:

```Elm
main : Program () Model Action
main =
    worker
        { init = ( Model "", Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        }
```

Infine, dobbiamo compilare il nostro programma con `elm make` e avviarlo fornendo gli argomenti desiderati:

```bash
elm make Main.elm
node Main.js nomeUtente
```

Se tutto è andato a buon fine, vedrai la tua stringa inserita come valore del campo `name` nel tuo modello!

## Deep Dive

La funzione `worker` del modulo `Platform` è essenziale per leggere gli argomenti da linea di comando in Elm. Ci consente di creare un `Program` con un `init`, una funzione di `update` e una funzione di `subscriptions`. Inoltre, la funzione `worker` accetta anche una funzione `view`, ma poiché non è necessaria per leggere gli argomenti dalla linea di comando, possiamo lasciarla vuota come nel nostro esempio sopra.

Inoltre, è importante notare che `Platform` non è il solo modo per leggere gli argomenti dalla linea di comando in Elm. Alcuni pacchetti di terze parti, come `elm-argv` e `elm-command-line`, offrono alternative a `Platform`, quindi è sempre una buona idea esplorare diverse opzioni per trovare quella più adatta alle tue esigenze.

## Vedi Anche

- Documentazione ufficiale su `Platform`: https://package.elm-lang.org/packages/elm/core/latest/Platform
- Pacchetto di terze parti `elm-argv`: https://package.elm-lang.org/packages/dillonkearns/elm-argv/latest/
- Pacchetto di terze parti `elm-command-line`: https://package.elm-lang.org/packages/ohanhi/elm-command-line/latest/