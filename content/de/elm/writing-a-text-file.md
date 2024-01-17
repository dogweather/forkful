---
title:                "Das Schreiben einer Textdatei"
html_title:           "Elm: Das Schreiben einer Textdatei"
simple_title:         "Das Schreiben einer Textdatei"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elm/writing-a-text-file.md"
---

{{< edit_this_page >}}

# Schreiben einer Textdatei in Elm

## Was & Warum?

Das Schreiben einer Textdatei ist ein wichtiger Bestandteil der Programmierung in Elm. Es ermöglicht das Speichern von Daten auf dauerhafte Weise, um sie später wieder verwenden oder teilen zu können. Programmiererinnen und Programmierer verwenden das Schreiben von Textdateien, um beispielsweise Benutzerinformationen, Konfigurationsdateien oder Protokolle zu speichern.

## Wie geht's?

Die Funktion `writeFile` ermöglicht das Schreiben von Textdateien in Elm. Sie akzeptiert zwei Argumente: den Dateipfad und den zu schreibenden Inhalt. Zum Beispiel:

```Elm
writeFile "nutzerinformationen.txt" "Max Mustermann"
```
Der obige Code erstellt eine Datei mit dem Namen "nutzerinformationen.txt" und schreibt den Text "Max Mustermann" in die Datei. Falls die Datei bereits existiert, wird ihr Inhalt überschrieben.

Um Text zu einer bereits bestehenden Datei hinzuzufügen, kann die Funktion `appendFile` verwendet werden. Sie funktioniert ähnlich wie `writeFile`, akzeptiert jedoch drei Argumente: den Dateipfad, den zu schreibenden Inhalt und eine Option, ob bereits bestehender Inhalt überschrieben werden soll. Zum Beispiel:

```Elm
appendFile "protokoll.txt" "Benutzer angemeldet" { overwrite = False }
```
Dieser Code fügt den Text "Benutzer angemeldet" der Datei "protokoll.txt" hinzu, ohne ihren vorhandenen Inhalt zu überschreiben.

## Tiefergehende Infos

Das Schreiben von Textdateien hat in der Programmierung eine lange Geschichte. Früher war es üblich, dass Programme Daten in Dateien speichern, anstatt sie in Datenbanken abzulegen. Heutzutage gibt es auch Alternativen wie das Schreiben von Daten in eine Datenbank oder das Verwenden von Cloud-Speicherlösungen.

Bei der Implementierung von `writeFile` und `appendFile` nutzt Elm die Standardbibliotheken von JavaScript, um Dateien zu erstellen und zu schreiben. Dies ist möglich, da Elm in JavaScript kompiliert wird.

## Siehe auch

- Offizielle Elm-Dokumentation zu `writeFile` und `appendFile`: https://package.elm-lang.org/packages/elm/file/latest/
- Ein Tutorial zum Schreiben von Dateien in Elm: https://dev.to/kristianpedersen/how-to-write-files-in-elm-6j7