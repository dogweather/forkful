---
title:    "Elm: Erstellen einer temporären Datei"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Warum
Das Erstellen von temporären Dateien ist ein häufiges Szenario beim Programmieren. Es ermöglicht die Speicherung von temporären Daten oder die Verwaltung von Dateien, die nur für einen begrenzten Zeitraum benötigt werden.

##Wie
Um eine temporäre Datei in Elm zu erstellen, können wir die `createFile`-Funktion aus dem `FileSystem`-Modul verwenden. Hier ist ein Beispielcode, der eine temporäre Datei mit dem Namen "temp.txt" erstellt:

```Elm
  import FileSystem

  tempFile : FileSystem.File
  tempFile =
    FileSystem.createFile "temp.txt"

  FileSystem.write "Hello, world!" tempFile
```

Die `createFile`-Funktion gibt ein `FileSystem.File`-Objekt zurück, das dann verwendet werden kann, um Daten in die Datei zu schreiben. In diesem Beispiel verwenden wir die `write`-Funktion, um den String "Hello, world!" in die temporäre Datei zu schreiben.

Um eine Liste aller temporären Dateien auf dem System abzurufen, können wir die `tempFiles`-Funktion verwenden, die eine Liste von `FileSystem.File`-Objekten zurückgibt:

```Elm
  import FileSystem

  tempFiles : List FileSystem.File
  tempFiles =
    FileSystem.tempFiles
```

## Vertiefung
Beim Erstellen von temporären Dateien ist es wichtig, darauf zu achten, dass diese nach der Verwendung auch wieder gelöscht werden. In der `FileSystem`-Bibliothek gibt es dafür die `delete`-Funktion, die es ermöglicht, eine Datei aus dem System zu entfernen. Hier ist ein Beispielcode, der eine temporäre Datei erstellt, Daten in die Datei schreibt und sie dann wieder löscht:

```Elm
  import FileSystem

  tempFile : FileSystem.File
  tempFile =
    FileSystem.createFile "temp.txt"

  FileSystem.write "Hello, world!" tempFile

  FileSystem.delete tempFile
```

Es ist auch möglich, ein spezifisches Verzeichnis für die temporären Dateien anzugeben, indem man den `tempFiles`-Funktion eine Pfadangabe als Argument übergibt. Zum Beispiel können wir eine temporäre Datei im Ordner "myTempFolder" erstellen und sie dann löschen:

```Elm
  import FileSystem

  tempFile : FileSystem.File
  tempFile =
    FileSystem.createFile "myTempFolder/temp.txt"

  FileSystem.write "Hello, world!" tempFile

  FileSystem.delete tempFile
```

## Siehe auch
- [Die offizielle Elm Dokumentation](https://elm-lang.org/docs)
- [Ein Tutorial zum Erstellen von temporären Dateien in Elm](https://www.tutorialspoint.com/elm/elm_file_system.htm)
- [Eine vollständige Referenz für das `FileSystem`-Modul](http://packages.elm-lang.org/packages/elm-lang/core/latest/FileSystem)