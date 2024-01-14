---
title:    "Haskell: Überprüfen, ob ein Verzeichnis vorhanden ist."
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Warum

Das Überprüfen, ob ein Verzeichnis existiert, ist ein wichtiger Bestandteil des Programmierens in Haskell. Es ermöglicht uns, sicherzustellen, dass unser Programm auf die benötigten Dateien und Ordner zugreifen kann, bevor es versucht, diese zu verwenden.

## Wie man es macht

Das Überprüfen eines Verzeichnisses in Haskell ist relativ einfach. Wir können die Funktion `doesDirectoryExist` aus dem Modul `System.Directory` verwenden, um zu überprüfen, ob ein bestimmtes Verzeichnis vorhanden ist. Hier ist ein Beispielcode:

```Haskell
import System.Directory

main = do
  dirExists <- doesDirectoryExist "Pfad/Zum/Verzeichnis"
  if dirExists
    then putStrLn "Das Verzeichnis existiert!"
    else putStrLn "Das Verzeichnis ist nicht vorhanden."
```

Das `doesDirectoryExist`-Funktion gibt einen booleschen Wert zurück, der angibt, ob das Verzeichnis existiert oder nicht. In unserem Beispiel verwenden wir diese Funktion innerhalb einer `if`-Anweisung, um entsprechend zu reagieren.

## Tiefergehende Untersuchung

Bevor wir die `doesDirectoryExist`-Funktion verwenden können, müssen wir das Modul `System.Directory` importieren. Dieses Modul enthält Funktionen zum Arbeiten mit Dateien und Verzeichnissen auf dem Dateisystem.

Es ist auch wichtig zu beachten, dass die `doesDirectoryExist`-Funktion nur überprüfen kann, ob ein Verzeichnis vorhanden ist. Wenn wir überprüfen möchten, ob eine Datei vorhanden ist, müssen wir stattdessen die Funktion `doesFileExist` verwenden.

Eine weitere Möglichkeit, das Vorhandensein von Verzeichnissen zu überprüfen, besteht darin, die `getDirectoryContents`-Funktion zu verwenden, um eine Liste aller Dateien und Verzeichnisse in einem bestimmten Verzeichnis zu erhalten. Wir können dann überprüfen, ob das gesuchte Verzeichnis in dieser Liste vorhanden ist.

## Siehe auch

- [Offizielle Dokumentation zu System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [Ein kurzes Tutorial zu System.Directory](https://www.codewars.com/kata/58f8a3a27f725e632300012e)
- [Eine andere Möglichkeit, das Vorhandensein von Dateien und Verzeichnissen in Haskell zu überprüfen](https://stackoverflow.com/questions/8900435/haskell-checking-if-a-file-exists/8900510#8900510)