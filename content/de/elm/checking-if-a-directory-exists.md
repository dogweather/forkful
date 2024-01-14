---
title:                "Elm: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Schritt in der Programmierung in Elm. Es ermöglicht die Kontrolle, ob eine bestimmte Datei oder ein Ordner vorhanden ist, bevor weitere Operationen durchgeführt werden.

## Wie geht man vor

Um ein Verzeichnis in Elm zu überprüfen, können Sie die `directoryExists` Funktion aus dem `Elm.File` Modul verwenden. Diese Funktion akzeptiert einen Pfad als Eingabe und gibt `True` zurück, wenn das angegebene Verzeichnis existiert, andernfalls gibt es `False` zurück.

````Elm
import File exposing (directoryExists)

-- Überprüfung, ob das Verzeichnis "beispiel/ordner" vorhanden ist
directoryExists "beispiel/ordner"
--> True
````

## Tiefer Einblick

Bevor Sie eine Datei aus einem Verzeichnis öffnen oder schreiben, ist es wichtig, sicherzustellen, dass das Verzeichnis auch tatsächlich vorhanden ist. Wenn Sie versuchen, auf ein nicht existierendes Verzeichnis zuzugreifen, wird es zu einem Fehler kommen. Durch die Verwendung der `directoryExists` Funktion können Sie diesen Fehler vermeiden und sicherstellen, dass Ihr Programm reibungslos funktioniert.

## Weitere Informationen

Weitere nützliche Funktionen rund um die Dateiverwaltung in Elm finden Sie in der offiziellen Dokumentation:

[File Module auf der Elm Dokumentationsseite](https://package.elm-lang.org/packages/elm/file/latest/File)

[FileSystem Module auf der Elm Dokumentationsseite](https://package.elm-lang.org/packages/elm/file/latest/FileSystem)