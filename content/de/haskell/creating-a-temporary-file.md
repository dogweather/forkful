---
title:    "Haskell: Erstellen einer temporären Datei"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Das Erstellen von temporären Dateien ist ein wichtiger Aspekt der Programmierung, der in vielen Fällen benötigt wird. Es ermöglicht uns, Daten vorübergehend zu speichern, um sie später zu verarbeiten oder zu löschen. Dies kann besonders nützlich sein, wenn wir mit großen Datenmengen arbeiten oder wenn wir Programme schreiben, die auf externen Datenquellen zugreifen.

# Wie man temporäre Dateien erstellt

Um eine temporäre Datei in Haskell zu erstellen, können wir die `withSystemTempFile` Funktion aus dem `System.IO.Temp` Modul verwenden. Diese Funktion führt eine Aktion aus, bei der eine temporäre Datei erstellt und der Dateipfad sowie ein Handle auf die Datei an die übergebene Aktion übergeben werden. Hier ist ein Beispielcode:

```Haskell
import System.IO.Temp (withSystemTempFile)

main :: IO ()
main = withSystemTempFile "tempFile.txt" $ \path handle -> do
  hPutStrLn handle "Hello world!"
  putStrLn $ "Datei erstellt unter " ++ path
```
In diesem Beispiel erstellen wir eine temporäre Datei mit dem Namen "tempFile.txt" und schreiben dann den Text "Hello world!" in die Datei. Der Pfad zur erstellten Datei wird dann auf der Konsole ausgegeben.

# Tiefere Einblicke

Wenn wir tiefer in die Erstellung von temporären Dateien in Haskell eintauchen, werden wir feststellen, dass es viele weitere Möglichkeiten gibt, dies zu tun. Wir können z.B. `withTempFile` verwenden, um eine temporäre Datei zu erstellen, ohne ein Handle zurückzugeben, oder `withTempDirectory`, um eine temporäre Verzeichnisstruktur zu erstellen und darauf zuzugreifen.

Außerdem können wir auch die `createTempFile` und `createTempDirectory` Funktionen aus dem `System.IO` Modul verwenden, um eine temporäre Datei bzw. Verzeichnisstruktur zu erstellen. Diese Funktionen geben den Pfad zur erstellten Datei bzw. Verzeichnis zurück, so dass wir sie später zum Speichern oder Löschen verwenden können.

# Siehe auch

- [Dokumentation für withSystemTempFile](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO-Temp.html#v:withSystemTempFile)
- [Haskell-Wiki-Artikel über temporäre Dateien](https://wiki.haskell.org/How_to_work_on_files#Temporary_files)
- [Offizielle Dokumentation für System.IO](https://hackage.haskell.org/package/base-4.14.1.0/docs/System-IO.html)