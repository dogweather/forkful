---
title:                "Elm: Lettura degli argomenti da riga di comando"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Se stai iniziando ad imparare Elm, potresti chiederti perché dovresti leggere gli argomenti della riga di comando. In realtà, la lettura degli argomenti della riga di comando è molto utile quando si sviluppano applicazioni Elm più complesse, in cui si vuole poter passare informazioni o opzioni all'apertura dell'applicazione.

## Come Fare

Per leggere gli argomenti della riga di comando in Elm, possibilità utilizzare la funzione `Platform.worker` combinata con la funzione `Platform.workerParser`. Innanzitutto, aggiungi la seguente importazione all'inizio del tuo file Elm:

```Elm
import Platform exposing (worker)
import Platform.Cmd exposing (dump)
```

Poi, puoi utilizzare la funzione `workerParser` per elaborare gli argomenti della riga di comando:

```Elm
main =
  Program.worker
    { init = init,
      update = update,
      subscriptions = subscriptions,
      view = view
    }

init : ( Model, Cmd Msg )
init =
  ( Model [],
    Platform.workerParser dump
  )
```

In questo esempio, stiamo utilizzando la funzione `dump` per salvare gli argomenti della riga di comando nella nostra applicazione. Puoi anche utilizzare una funzione personalizzata per elaborare gli argomenti in base alle tue esigenze.

## Approfondimento

Quando si leggono gli argomenti della riga di comando, è importante comprendere come vengono passati e gestiti. In Elm, gli argomenti sono passati come stringhe nella forma `--nome=valore` o `--nome valore`. Queste stringhe possono poi essere elaborate e utilizzate per inizializzare o modificare il modello della tua applicazione.

Se vuoi leggere ulteriori informazioni sui comandi della riga di comando in Elm, ti consiglio di dare un'occhiata alla documentazione ufficiale su [Platform.worker](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker) e [Platform.workerParser](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#workerParser).

## Vedi Anche

- [Documentazione su Platform.worker](https://package.elm-lang.org/packages/elm/core/latest/Platform#worker)
- [Documentazione su Platform.workerParser](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#workerParser)