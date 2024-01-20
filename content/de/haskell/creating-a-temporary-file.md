---
title:                "Eine temporäre Datei erstellen"
html_title:           "Java: Eine temporäre Datei erstellen"
simple_title:         "Eine temporäre Datei erstellen"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Was & Warum?

Einen temporären Datei zu erstellen bedeutet, eine Datei zu erstellen, die nur für die Dauer einer Sitzung oder eines Prozesses existiert. Programmierer tun dies zu häufig für Zwischenspeicherungs-Anforderungen, um Datenflüsse zu verwalten oder Fehler zu verhindern.

## So Geht's:

In Haskell können wir das System.IO.Temp verwenden, um temporäre Dateien anzulegen. Hier ist ein einfaches Beispiel:

```Haskell
import System.IO.Temp
import System.IO

main = withSystemTempFile "temp.txt" $ \tempFile hFile -> do
    hPutStrLn hFile "Ein bisschen Text..."
    hFlush hFile
    readFile tempFile >>= putStrLn
```

Wenn wir diesen Code ausführen, wird eine temporäre Datei mit dem Namen "temp.txt" erstellt, einige Texte hinzugefügt und dann ausgelesen.

## Tief Tauchen:

Ersteinmal sollten wir etwas historischer Kontext durchgehen. In älteren Versionen von Haskell, konnten temporäre Dateien nicht so einfach erstellt werden. Die Funktionen zur Handhabung von temporären Dateien wurden erst in der Bibliothek `System.IO.Temp` eingeführt, die in Haskell Platform 2010 und später eingebaut ist.

Eine alternative Methode wäre die Nutzungen des `System.Directory` Pakets, welches auch Methoden wie `getTemporaryDirectory` enthält. Im Allgemeinen sind die Werkzeug in `System.IO.Temp` aber umfassender und flexibler.

Wenn eine temporäre Datei erstellt wird, wird sie in dem durch das Betriebssystem definierte temporäre Verzeichnis erstellt. In den meisten Fällen ist dies `/tmp` auf Unix-Systemen und `C:\Windows\Temp` auf Windows. Die erstellte Datei wird nach Beendigung des Prozesses automatisch gelöscht.

## Siehe Auch:

Weiterführende Informationen und detaillierte Anleitungen gibt es in der Haskell-Dokumentation unter [Haskell System.IO.Temp](https://hackage.haskell.org/package/temporary-0.2.2.3/docs/System-IO-Temp.html) und im [Haskell Wikibook](https://en.wikibooks.org/wiki/Haskell/Understanding_monads/IO). Sehr empfehlenswert, um tieferes Wissen zu entwickeln!