---
title:                "Elm: Erstellung einer temporären Datei"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum

Das Erstellen von temporären Dateien ist ein nützliches Werkzeug in der Programmierung, um Daten temporär zu speichern und zu verwalten. Dies kann hilfreich sein, wenn man zum Beispiel an Dateien arbeitet, die man später wieder löschen möchte, oder wenn man spezifische Informationen für eine bestimmte Aufgabe benötigt.

## Wie geht das?

In Elm gibt es verschiedene Möglichkeiten, um temporäre Dateien zu erstellen. Eine Möglichkeit ist die Verwendung der `File` Bibliothek. Diese Bibliothek ermöglicht es, Dateien zu erstellen, zu lesen, zu schreiben und zu löschen. Hier ist ein Beispiel, wie man eine temporäre Datei erstellen kann:

```Elm
import File
import Random

type alias TempFile =
    { name : String
    , path : String
    , content : String
    }

createTempFile : Cmd Msg
createTempFile =
    Random.generate TempGenerated (Random.int 0 100)
        |> Task.toMaybe
        |> Task.andThen
            (\randomNumber ->
                let
                    fileName =
                        "temp_file" ++ (toString randomNumber) ++ ".txt"
                in
                    File.write fileName "Hello, World!"
                        |> Task.map (TempCreated fileName)
            )
        |> Task.perform TaskErr

type Msg
    = TempGenerated Int
    | TempCreated String File.Task
    | TaskErr File.Step

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TempGenerated num ->
            ( model
            , createTempFile
            )

        TempCreated fileName task ->
            ( model
            , Task.attempt TaskErr task
            )

        TaskErr err ->
            ( model, Cmd.none )

main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }

main.wrapped (File.batch []) "elm-output"
```

Dieses Beispiel erstellt eine temporäre Datei mit einem zufällig generierten Namen und fügt den Text "Hello, World!" hinzu.

## Tiefer Einblick

Die `File` Bibliothek ermöglicht auch das Erstellen von Verzeichnissen und das Ausführen von Operationen auf Dateien, wie zum Beispiel das Umbenennen oder Verschieben. Es ist auch wichtig zu beachten, dass die erstellten temporären Dateien nicht permanent gespeichert werden und nach der Verwendung gelöscht werden sollten.

## Siehe auch

- [Elm File Library Dokumentation](https://package.elm-lang.org/packages/elm/file/latest)
- [Elm Random Library Dokumentation](https://package.elm-lang.org/packages/elm/random/latest)
- [Temporäre Dateien in der Programmierung](https://en.wikipedia.org/wiki/Temporary_file)