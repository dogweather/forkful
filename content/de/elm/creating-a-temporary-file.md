---
title:                "Elm: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Warum
Ever wonder how to create a temporary file in Elm? This blog post will guide you through the process and show you why it can be useful.

## Wie
Um eine temporäre Datei in Elm zu erstellen, müssen wir zuerst das Built-In Modul `Platform` importieren. Dann können wir die Funktion `File.Temporary.temp` aufrufen, um eine temporäre Datei zu erstellen und deren Pfad als String zu erhalten. Hier ist ein Beispielcode:

```Elm
import Platform
import File.Temporary
import File

main =
  Platform.worker
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

type Msg = CreateTempFile (Result File.Error String)

init : () -> (Model, Cmd Msg)
init _ =
  (Model, Cmd.batch
    [ File.Temporary.temp CreateTempFile
    ])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    CreateTempFile (Result.Ok path) ->
      (model, File.write path "Hello World!")
    CreateTempFile (Result.Err err) ->
      (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none
```

Der obige Code erstellt eine temporäre Datei und schreibt den String "Hello World!" in diese Datei. Wenn die Datei erfolgreich erstellt wird, sollte das Ergebnis `Result.Ok` sein und der Pfad zu dem temporären Ordner wird in der `CreateTempFile` Nachricht zurückgegeben.

## Deep Dive
Die Funktion `File.Temporary.temp` nimmt einen optionalen Präfix für die temporäre Datei als Argument. Wenn kein Präfix angegeben wird, wird standardmäßig "tmp" verwendet. Darüber hinaus gibt es auch die Möglichkeit, den temporären Ordner zu ändern. Wir können dies erreichen, indem wir den Argument `Just "custom/temp/path"` an die Funktion übergeben.

Die Funktion `File.Temporary.temp` nutzt das Dateisystem des Betriebssystems, um temporäre Dateien zu erstellen. Dies hat den Vorteil, dass die Dateien automatisch gelöscht werden, sobald das Programm beendet wird. Wir sollten diese Funktion daher verwenden, um temporäre Dateien zu erstellen, anstatt manuell Dateien zu erstellen und später zu löschen.

## Siehe auch
- [Offizielle Elm Dokumentation zu temporären Dateien](https://package.elm-lang.org/packages/elm/file/latest/File-Temporary#temp)
- [Blog-Beitrag über das Arbeiten mit Dateien in Elm](https://dev.to/criesbeck/working-with-files-in-elm-3a7g)