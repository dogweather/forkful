---
title:                "Elm: Überprüfen, ob ein Verzeichnis vorhanden ist"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum
Warum ist es wichtig zu überprüfen, ob ein Verzeichnis existiert? Als Programmierer:in ist es wichtig, Fehler zu vermeiden und sicherzustellen, dass unser Code reibungslos funktioniert. Durch die Überprüfung eines Verzeichnisses können wir sicherstellen, dass unser Programm nicht auf Fehler stößt, wenn ein bestimmtes Verzeichnis fehlt.

## Wie geht das?
Bevor wir uns anschauen, wie man in Elm überprüft, ob ein Verzeichnis existiert, müssen wir sicherstellen, dass wir die `elm/file` Bibliothek importiert haben. Diese Bibliothek bietet uns Funktionen zum Lesen, Schreiben und Überprüfen von Dateien und Verzeichnissen.

Um nun ein Verzeichnis zu überprüfen, können wir die Funktion `doesDirectoryExist` verwenden. Diese Funktion erfordert eine Pfadangabe als Argument und gibt ein `Task Bool` zurück. Wir können dann die Funktion `Task.perform` verwenden, um das Ergebnis der Aufgabe abzurufen.

Lass uns das Ganze in Aktion sehen:

```Elm
import File exposing (doesDirectoryExist)
import Task exposing (Task, perform)

main =
    Task.perform checkDirectoryExists (doesDirectoryExist "path/to/directory")

checkDirectoryExists result =
    case result of
        Ok exists ->
            if exists then
                "Das Verzeichnis existiert."
            else
                "Das Verzeichnis existiert nicht."
        Err error ->
            "Fehler beim Überprüfen des Verzeichnisses aufgetreten."
```

Das obige Beispiel zeigt, wie wir die Funktion `doesDirectorytExist` verwenden und das Ergebnis dann in der Funktion `checkDirectoryExists` verarbeiten, um eine entsprechende Ausgabe zu erhalten.

## Tiefere Einblicke
Obwohl die Überprüfung, ob ein Verzeichnis existiert, eine relativ einfache Funktionalität ist, können wir trotzdem einige tiefere Einblicke in den Prozess gewinnen. Zum Beispiel können wir zusätzliche Funktionen wie `listDirectory` verwenden, um eine Liste aller Dateien und Verzeichnisse in einem bestimmten Verzeichnis zu erhalten.

Wir können auch die Funktion `directorySize` verwenden, um die Größe eines Verzeichnisses in Byte zu berechnen. Diese Informationen können uns helfen, den Speicherbedarf unserer Anwendung besser zu verstehen und zu optimieren.

## Siehe auch
- [Die offizielle Dokumentation der `elm/file` Bibliothek](https://package.elm-lang.org/packages/elm/file/latest/)
- [Ein vollständiges Beispiel einer Elm-Anwendung, die Verzeichnisse überprüft] (https://github.com/elm/file/blob/1.0.1/examples/Directories.elm)