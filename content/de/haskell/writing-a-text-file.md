---
title:    "Haskell: Eine Textdatei schreiben"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Warum

Das Verfassen von Textdateien ist ein grundlegender Bestandteil der Programmierung. Es ermöglicht uns, Informationen auf einfache und strukturierte Weise zu speichern und zu verarbeiten.

## Wie man eine Textdatei schreibt

Das Erstellen einer Textdatei in Haskell ist einfach. Wir verwenden die Funktion "writeFile", um eine neue Textdatei zu erstellen oder eine bestehende Textdatei zu überschreiben.

```
Haskell
writeFile :: FilePath -> String -> IO ()
writeFile "mein_dateiname.txt" "Dies ist ein Beispieltext"
```

Dieser Code erstellt eine Datei mit dem Namen "mein_dateiname.txt" und schreibt den Text "Dies ist ein Beispieltext" in die Datei. Wenn die Datei bereits besteht, wird sie überschrieben.

## Tiefergehende Infos über das Schreiben von Textdateien

In Haskell können wir auch weitere Funktionen wie "appendFile" verwenden, um Text an eine bereits bestehende Datei anzuhängen. Wir können auch "readFile" verwenden, um den Inhalt einer Textdatei auszulesen und in einer Haskell-Funktion zu verwenden.

Es ist auch wichtig zu beachten, dass die Dateipfade in Haskell mit "/" statt "\" definiert werden müssen, um Fehler zu vermeiden.

## Siehe auch

- Offizielle Haskell Dokumentation: https://www.haskell.org/documentation
- Tutorial: Textdateien in Haskell schreiben: https://www.tutorialspoint.com/haskell/haskell_files_io.htm
- Thomas auch Haskell Blog: https://thomas.io/textfiles-in-haskell