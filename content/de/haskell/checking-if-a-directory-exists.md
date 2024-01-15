---
title:                "Überprüfen, ob ein Verzeichnis existiert."
html_title:           "Haskell: Überprüfen, ob ein Verzeichnis existiert."
simple_title:         "Überprüfen, ob ein Verzeichnis existiert."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Gründe, warum es wichtig sein kann, zu überprüfen, ob ein Verzeichnis existiert. Möglicherweise möchten Sie sicherstellen, dass ein bestimmtes Verzeichnis oder Pfad auf Ihrem System vorhanden ist, bevor Sie damit interagieren oder Dateien in dieses Verzeichnis kopieren.

## Wie geht's

Um zu überprüfen, ob ein Verzeichnis existiert, müssen Sie die `doesDirectoryExist` Funktion aus dem `System.Directory` Modul importieren. Diese Funktion gibt einen Bool-Wert zurück, der angibt, ob das angegebene Verzeichnis existiert oder nicht. Hier ist ein Beispielcode:

```Haskell
import System.Directory

main :: IO ()
main = do
    let directory = "/home/user/Desktop"
    exists <- doesDirectoryExist directory
    if exists
        then putStrLn "Das Verzeichnis existiert."
        else putStrLn "Das Verzeichnis existiert nicht."
```

Ausgabe für das obige Beispiel:

```
Das Verzeichnis existiert.
```

Hier kann das Verzeichnis `/home/user/Desktop` durch einen anderen Pfad ersetzt werden, um zu überprüfen, ob ein anderes Verzeichnis existiert oder nicht.

## Tiefer Einblick

Neben der `doesDirectoryExist` Funktion gibt es noch andere Funktionen für die Arbeit mit Verzeichnissen, die im `System.Directory` Modul vorhanden sind. Zum Beispiel:

- `createDirectory` zum Erstellen eines neuen Verzeichnisses
- `removeDirectory` zum Löschen eines Verzeichnisses
- `copyDirectory` zum Kopieren eines Verzeichnisses
- `getDirectoryContents` zum Abrufen einer Liste von Dateien und Unterordnern in einem Verzeichnis

Dies sind nur einige Beispiele für die Verwendung von Verzeichnisfunktionen in Haskell. Es ist wichtig zu beachten, dass die Bearbeitung von Verzeichnissen einige Sicherheitsaspekte haben kann und Sie immer sicherstellen sollten, dass Sie über die benötigten Berechtigungen verfügen, bevor Sie Verzeichnisoperationen durchführen.

## Siehe auch

- [Haskell Dokumentation für das `System.Directory` Modul](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Eine Einführung in Haskell für Anfänger](https://www.fprogram.xyz/haskell-tutorial-deutsch/)
- [Weitere Beispiele für für das Arbeiten mit Verzeichnissen in Haskell](https://stackoverflow.com/questions/30265319/haskell-check-if-a-directory-exists)