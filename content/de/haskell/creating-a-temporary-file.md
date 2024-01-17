---
title:                "Erstellen einer temporären Datei"
html_title:           "Haskell: Erstellen einer temporären Datei"
simple_title:         "Erstellen einer temporären Datei"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

**Was & Warum**
Bei der Erstellung einer temporären Datei handelt es sich um eine kurze Datei, die von Programmierern erstellt wird, um vorübergehende Daten oder Ergebnisse zu speichern. Dies kann nützlich sein, wenn die endgültigen Daten noch nicht verfügbar sind oder wenn eine Zwischenspeicherung für die Ausführung eines Programms erforderlich ist.

**Wie geht's:**
Um eine temporäre Datei in Haskell zu erstellen, können Sie die `withTempFile` Funktion aus dem `System.IO.Temp` Modul verwenden. Hier ist ein Beispielcode, der eine temporäre Datei mit dem Inhalt "Hello World" erstellt und dann deren Inhalt auf der Konsole ausgibt:

```Haskell
import System.IO.Temp (withTempFile)
import System.IO (hPrint, hGetContents, hClose)

main :: IO ()
main = withTempFile "temp" ( \path handle -> do
    hPrint handle "Hello World"
    contents <- hGetContents handle
    putStrLn contents
    )
```

Die Ausgabe dieses Codes wird "Hello World" sein.

**Tiefer tauchen:**
Das Erstellen von temporären Dateien ist eine nützliche Funktion, die in vielen Programmiersprachen verfügbar ist. Es wird häufig verwendet, um temporäre Daten für die Verwendung in einem Programm zu speichern, ohne die endgültigen Daten zu verändern. Darüber hinaus können in Haskell auch andere Methoden zur Arbeit mit temporären Dateien verwendet werden, wie z.B. die `withSystemTempFile` Funktion.

Einige alternative Methoden zum Erstellen von temporären Dateien in Haskell sind:

- Verwendung des `createTempDirectory` Befehls aus dem `System.Directory` Modul, um eine temporäre Datei herzustellen.
- Verwendung des `mkstemp` Befehls aus dem `System.Posix.Temp` Modul, um eine temporäre Datei mit dem Betriebssystem-Standard für temporäre Dateien zu erstellen.

Die `withTempFile` Funktion in Haskell hat eine ähnliche Syntax wie die `withTempFile` Funktion in der Sprache Python, was es einfacher macht, Code zwischen den beiden Sprachen zu übertragen. 

**Siehe auch:**
Weitere Informationen und Beispiele zum Erstellen von temporären Dateien in Haskell finden Sie in der offiziellen Dokumentation des `System.IO.Temp` Moduls: https://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO-Temp.html