---
title:                "Überprüfen, ob ein Verzeichnis existiert"
html_title:           "Haskell: Überprüfen, ob ein Verzeichnis existiert"
simple_title:         "Überprüfen, ob ein Verzeichnis existiert"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Was & Warum?
Beim Programmieren geht es darum, Probleme zu lösen und Aufgaben zu automatisieren. Oft müssen wir wissen, ob ein bestimmtes Verzeichnis auf unserem Computer existiert, bevor wir damit arbeiten können. Das Überprüfen der Existenz eines Verzeichnisses ist daher eine wichtige Aufgabe für Programmierer.

# Wie geht man vor?
Zum Glück ist das Überprüfen der Existenz eines Verzeichnisses in Haskell eine einfache Aufgabe. Mit der `doesDirectoryExist` Funktion aus dem `System.Directory` Modul können wir überprüfen, ob ein Verzeichnis mit dem angegebenen Pfad existiert. Hier ist ein Beispiel:

```Haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
  let path = "/Users/Benutzer/Documents/"
  dirExists <- doesDirectoryExist path
  if dirExists
    then putStrLn "Das Verzeichnis existiert!"
    else putStrLn "Das Verzeichnis existiert nicht."
```

Der `doesDirectoryExist` Funktion gibt einen `bool` zurück, der angibt, ob das Verzeichnis existiert oder nicht. Wir können diesen Wert verwenden, um entsprechend zu handeln.

# Tiefer Einblick
In der Vergangenheit mussten Programmierer manuell überprüfen, ob ein Verzeichnis existiert, indem sie die Berechtigungen des Dateisystems überprüften. Mit der Einführung von Haskell wurde diese Aufgabe jedoch durch die Einführung von System.Directory Module wesentlich vereinfacht.

Es gibt auch alternative Möglichkeiten, um die Existenz eines Verzeichnisses zu überprüfen, wie z.B. die Verwendung des `findPred` aus dem `System.Directory.Tree` Modul oder die Verwendung des `getFileStatus` aus dem `System.Posix.Files` Modul.

Die `doesDirectoryExist` Funktion verwendet intern die `doesPathExist` Funktion, die wiederum die `stat` Systemaufruf benutzt, um Informationen über das Dateisystemelement zu erhalten.

# Sieh auch
- [Haskell Dokumentation](https://www.haskell.org/documentation/)
- [System.Directory Modul Dokumentation](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [System.Directory.Tree Modul Dokumentation](https://hackage.haskell.org/package/directory-tree/docs/System-Directory-Tree.html)
- [System.Posix.Files Modul Dokumentation](https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html)