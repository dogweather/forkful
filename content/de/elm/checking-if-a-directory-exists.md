---
title:    "Elm: Überprüfen, ob ein Verzeichnis existiert"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Warum
Wenn Sie jemals in einem Projekt mit Dateien gearbeitet haben, sind Sie vielleicht auf die Notwendigkeit gestoßen, zu überprüfen, ob ein bestimmter Ordner existiert. Dies kann nützlich sein, um zu vermeiden, dass unerwünschte Fehler auftreten, wenn Sie versuchen, auf einen nicht vorhandenen Ordner zuzugreifen. In diesem Blog-Beitrag zeige ich Ihnen, wie Sie mithilfe von Elm ganz einfach überprüfen können, ob ein Ordner vorhanden ist.

## Wie geht das?
Um zu überprüfen, ob ein Ordner existiert, müssen wir zunächst die Bibliothek `elm/file` importieren. Diese Bibliothek enthält Funktionen, um mit Dateien und Ordnern zu interagieren. Dann können wir die Funktion `File.isDirectory` verwenden, um zu testen, ob ein bestimmter Pfad zu einem Ordner zeigt oder nicht.

```Elm
import File exposing (isDirectory)

checkDirectory: String -> Cmd msg
checkDirectory path =
  isDirectory path
    |> task
```

Die Funktion `isDirectory` gibt eine `Task` zurück, die wir dann in ein `Cmd msg` konvertieren. Auf diese Weise können wir die Überprüfung in unserer `update` Funktion ausführen und eine Nachricht senden, basierend auf dem Ergebnis.

```Elm
-- Message type

type Msg
  = DirectoryExists
  | DirectoryDoesNotExist

-- Update function

update msg model =
  case msg of
    DirectoryExists ->
      -- handle result when the directory exists

    DirectoryDoesNotExist ->
      -- handle result when the directory does not exist
```

## Tiefer Einblick
In Elm gibt es auch die Möglichkeit, mithilfe von `elm/http` auf Dateien und Ordner auf einem Server zuzugreifen. Dies ermöglicht es uns, nicht nur lokale Ordner, sondern auch entfernte Server-Ordner zu überprüfen. Auch hierfür können wir die Funktion `File.isDirectory` verwenden und den Pfad zu unserem Server-Ordner angeben.

```Elm
import File exposing (isDirectory)
import Http
import Task

checkServerDirectory: String -> Cmd msg
checkServerDirectory url =
  Http.get
    { url = url
    , expect = Http.expectString (\_ -> Task.succeed DirectoryExists)
    , timeout = Nothing
    , headers = []
    , body = Http.emptyBody
    }
    |> Task.mapError
        (\error ->
            case error of
                Http.BadUrl url ->
                    "Invalid URL: " ++ url

                Http.Timeout ->
                    "Request timed out"

                Http.NetworkError ->
                    "Network error"

                Http.BadStatus code ->
                    "Unsuccessful response code: " ++ (toString code)

                Http.BadHeader header ->
                    "Unsupported header: " ++ header
        )
    |> Task.map DirectoryDoesNotExist
```

Hier verwenden wir die Funktion `Http.get`, um eine GET-Anfrage an den Server-Ordner zu senden und einen String als Antwort zu erwarten. Wenn die Anfrage erfolgreich ist, wird die Funktion `expectString` ausgeführt und gibt eine `Task` zurück, die entweder `DirectoryExists` zurückgibt, wenn der Ordner existiert, oder `DirectoryDoesNotExist`, wenn der Ordner nicht vorhanden ist.

# Siehe auch
- Offizielle Dokumentation zu `elm/file`: https://package.elm-lang.org/packages/elm/file/latest/
- Blog-Beitrag über Dateibearbeitung mit Elm: https://elmprogramming.com/file-interactions-in-elm.html
- Beispielprojekt auf GitHub zur Dateiverwaltung mit Elm: https://github.com/dillonkearns/elm-filemanager-example