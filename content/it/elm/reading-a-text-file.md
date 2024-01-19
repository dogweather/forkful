---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Leggere un file di testo significa accedere e interpretare i dati memorizzati al suo interno. Lo facciamo principalmente per operare su tali dati tramite il codice.

## Come fare:

In Elm, non possiamo leggere direttamente i file di testo a causa del suo ambiente sicuro. Tuttavia, con HTML5 FileReader API, possiamo ottenere i dati. Ecco un esempio pratico:

```Elm
type alias Model =
    { file : Maybe File
    , content : String
    }

type Msg
    = GotFile (Maybe File)
    | ReadContent (Result FileReader.Error String)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        GotFile maybeFile ->
            case maybeFile of
                Nothing ->
                    ( { model | file = Nothing }, Cmd.none )

                Just file ->
                    ( { model | file = Just file }
                    , File.readAsText file
                        |> Task.attempt ReadContent
                    )

        ReadContent (Ok content) ->
            ( { model | content = content }, Cmd.none )

        ReadContent (Err _) ->
            ( { model | content = "" }, Cmd.none )
```

Nota: il sopracitato esempio si assume l'uso di elm/file per gestire i files.

## Approfondimento

Elm è un linguaggio di programmazione funzionale per il web front-end. Pone un'importante enfasi sulla sicurezza, ed è per questo motivo che l'interazione diretta con i file di testo non è possibile in Elm. Tuttavia, utilizzando l'API FileReader di HTML5, siamo in grado di leggere file di testo nel browser.

Alcune alternative a Elm per la lettura di file di testo includono JavaScript, Python e Ruby. Ognuna di queste lingue ha le sue particolari implementazioni per la lettura dei file.

Nell'implementazione della lettura dei file in Elm, ci stiamo basando nel moderno modello di promesse JavaScript, rispecchiandolo in Elm con l'uso dei Task.

## Vedi anche

1. Documentazione ufficiale di Elm: https://guide.elm-lang.org/
2. API FileReader di HTML5: https://developer.mozilla.org/it/docs/Web/API/FileReader  
3. Libreria elm/file: https://package.elm-lang.org/packages/elm/file/latest/