---
title:                "Haskell: Erstellen einer temporären Datei"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Warum

Temporäre Dateien sind ein nützliches Konzept in der Programmierung, besonders wenn es darum geht, Daten vorübergehend zu speichern oder zu teilen. Durch das Erstellen einer temporären Datei können wir sicherstellen, dass unsere Daten nicht dauerhaft gespeichert werden und somit den Speicherplatz nicht unnötig belegt.

# Wie geht man vor

Um eine temporäre Datei mit Haskell zu erstellen, können wir die `System.IO` Bibliothek verwenden. Zuerst müssen wir die Bibliothek in unserem Code importieren:

```Haskell
import System.IO
```

Dann können wir die Funktion `withSystemTempFile` verwenden, um eine temporäre Datei zu erstellen und zu bearbeiten. Diese Funktion erfordert zwei Parameter: ein Namenspräfix und eine Funktion, die die erstellte Datei als Argument erhält.

```Haskell
withSystemTempFile "meine_tempdatei" $ \datei -> do
    hPutStrLn datei "Dies ist eine temporäre Datei!"
    hGetContents datei >>= putStrLn
```

In diesem Beispiel verwenden wir `hPutStrLn`, um einen String in die Datei zu schreiben und `hGetContents` zusammen mit `putStrLn` um den Inhalt der Datei auszugeben. Die `withSystemTempFile` Funktion sorgt dafür, dass die erstellte Datei automatisch gelöscht wird, sobald die Funktion beendet ist.

Die Ausgabe unseres Beispiels sieht folgendermaßen aus:

```
Dies ist eine temporäre Datei!
```

# Tiefere Einblicke

Der Namenpräfix, den wir der `withSystemTempFile` Funktion übergeben, wird verwendet, um den temporären Dateinamen zu generieren. In diesem Fall wird die Datei mit dem Präfix "meine_tempdatei" erstellt und mit einer zufälligen Zeichenfolge ergänzt, um sicherzustellen, dass der Dateiname eindeutig ist.

Außerdem können wir durch die Verwendung der Funktion `openTempFile` auch manuell eine temporäre Datei erstellen und den Dateinamen selbst bestimmen. Diese Funktion gibt einen Tupel mit dem Dateinamen und dem Dateihandle zurück, das verwendet werden kann, um auf die Datei zuzugreifen und sie zu bearbeiten.

```Haskell
(dateiname, handle) <- openTempFile "." "meine_tempdatei.txt"
hPutStrLn handle "Eine andere temporäre Datei!"
hClose handle -- Dateihandle schließen
```

# Siehe auch

- [Die offizielle Dokumentation zur `System.IO` Bibliothek](https://www.haskell.org/onlinereport/standard-prelude.html#module-system-io)
- [Ein Tutorial zur Arbeit mit Dateien in Haskell](http://learnyouahaskell.com/input-and-output#files-and-streams)