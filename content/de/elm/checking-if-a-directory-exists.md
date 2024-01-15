---
title:                "Prüfen, ob ein Verzeichnis existiert"
html_title:           "Elm: Prüfen, ob ein Verzeichnis existiert"
simple_title:         "Prüfen, ob ein Verzeichnis existiert"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Warum sollte jemand überprüfen wollen, ob ein Verzeichnis existiert? Ganz einfach: Es ist ein wichtiger Schritt beim Umgang mit Dateien in der Programmierung. Wenn man sicherstellen möchte, dass ein bestimmtes Verzeichnis vorhanden ist, bevor man Dateien darin erstellt oder liest, kann man mit dieser Methode unnötige Fehler vermeiden.

## Wie
Um in Elm zu überprüfen, ob ein Verzeichnis existiert, kann man die Funktion `directoryExists` aus dem Modul `File` verwenden. Diese Funktion nimmt als Argument ein `String` mit dem Pfad zum Verzeichnis und gibt ein `Task Bool` zurück. Innerhalb des `Task Bool` kann man dann überprüfen, ob das Verzeichnis existiert oder nicht. Hier ein Beispiel:

```Elm
import File exposing (directoryExists)

directoryExists "Pfad/zum/Verzeichnis"
    |> Task.perform handleResult

handleResult : Result x Bool -> _
handleResult result =
    case result of
        Ok exists ->
            if exists then
                -- tue etwas, da das Verzeichnis existiert
            else
                -- tue etwas anderes, falls es nicht existiert
        
        Err error ->
            -- handle Fehlerfall
```

In diesem Beispiel nutzen wir die `Task.perform` Funktion, um das Ergebnis der `directoryExists` Funktion zu verarbeiten. Wenn das Verzeichnis existiert, wird `exists` den Wert `True` haben, ansonsten `False`. In unserem `handleResult`-Funktion können wir dann je nach Bedarf entsprechend handeln.

## Deep Dive
Es ist wichtig zu beachten, dass die `directoryExists` Funktion nur überprüft, ob das Verzeichnis zum Zeitpunkt des Aufrufs existiert. Wenn man sicherstellen möchte, dass das Verzeichnis während der Ausführung des Programms existiert, sollte man stattdessen die Funktion `directoryExistsSync` aus dem Modul `File` verwenden. Diese Funktion gibt einfach einen `Bool` Wert zurück, je nachdem ob das Verzeichnis existiert oder nicht. Allerdings sollte man bedenken, dass dies eine synchrone Funktion ist und somit die Ausführung des Programms blockiert, während sie ausgeführt wird.

## Siehe auch
- Offizielle Dokumentation zu `File.directoryExists`: https://package.elm-lang.org/packages/elm/file/latest/File#directoryExists
- Weitere Informationen zu Dateioperationen in Elm: https://guide.elm-lang.org/effects/file.html