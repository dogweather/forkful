---
title:                "Haskell: Überprüfung, ob ein Verzeichnis vorhanden ist"
simple_title:         "Überprüfung, ob ein Verzeichnis vorhanden ist"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist eine wichtige Aufgabe in der Programmierung. Es ermöglicht uns zu überprüfen, ob ein bestimmter Pfad oder Ordner vorhanden ist, bevor wir versuchen, auf ihn zuzugreifen. Dies kann verhindern, dass unser Programm abstürzt oder unerwünschte Fehlermeldungen ausgibt.

## How To

Die Überprüfung eines Verzeichnisses in Haskell ist relativ einfach und erfordert nur wenige Zeilen Code.

```Haskell
import System.Directory -- Importieren des benötigten Moduls

main = do
    dirExists <- doesDirectoryExist "Pfad/des/Verzeichnisses" -- Überprüfung des Verzeichnisses, gibt einen Bool-Wert zurück
    if dirExists -- Boolscher Ausdruck zur Überprüfung des Wertes
        then putStrLn "Das Verzeichnis existiert." -- Ausgabe, wenn das Verzeichnis existiert
        else putStrLn "Das Verzeichnis existiert nicht." -- Ausgabe, wenn das Verzeichnis nicht existiert
```
Der von unserem Programm ausgegebene Text hängt davon ab, ob das überprüfte Verzeichnis existiert oder nicht.

## Deep Dive

Die Funktion `doesDirectoryExist` ist Teil des `System.Directory`-Moduls und gibt einen Bool-Wert zurück, der angibt, ob das übergebene Verzeichnis existiert oder nicht. Diese Funktion ist hilfreich, um Fehler zu vermeiden, wenn wir versuchen, auf ein Verzeichnis zuzugreifen, das möglicherweise nicht vorhanden ist.

Wenn wir jedoch noch weiter in die Tiefe gehen wollen, bietet das `System.Directory`-Modul noch viele weitere Funktionen für die Arbeit mit Verzeichnissen. Einige davon sind:

- `getCurrentDirectory` - gibt den aktuellen Arbeitsverzeichnis-Pfad zurück
- `setCurrentDirectory` - ändert das aktuelle Arbeitsverzeichnis
- `getDirectoryContents` - listet die Inhalte eines Verzeichnisses auf
- `createDirectory` - erstellt ein neues Verzeichnis

Diese und viele weitere Funktionen können uns helfen, effizient mit Verzeichnissen in Haskell zu arbeiten.

## Siehe auch

- [System.Directory Dokumentation](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Haskell Tutorials auf dev.to](https://dev.to/t/haskell)
- [Haskell Programmieren für Anfänger](https://code.tutsplus.com/de/tutorials/learning-haskell-programming-for-beginners--cms-30330)